#
# Dataset documentation:
#   https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku
#   https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf
#


library(shiny)
library(httr)
library(keyring)
library(tidyverse)
library(jsonlite)
library(usmap)
library(scales)
library(ggrepel)

data(statepop)


#' Get the CDC Socrata API JSON endpoint for a given data set ID
#'
#' @param dataSetID the Socrata API data set ID (of the form xxxx-xxxx)
#'
#' @return a string of the URL (scheme, host, and path, no query params)
#'
#' @examples getDataSetURL('vbim-akqf')
getDataSetURL <- function(dataSetID) {
  return(str_c('https://data.cdc.gov/resource/', dataSetID, '.json'))
}

#' A wrapper around `httr::GET()` to handle authentication and errors
#'
#' @param dataSetID the Socrata API data set ID (of the form xxxx-xxxx)
#' @param query a list of named string elements to be used as query parameters
#'
#' @return a response object from the enclosed `GET()` call
#'
#' @examples doGET('9bhg-hcku', list('$select' = 'max(:updated_at)'))
doGET <- function(dataSetID, query, returnResponse = FALSE) {
  response <- GET(getDataSetURL(dataSetID),
                  authenticate(key_get('CDC_DATA'),
                               key_get('CDC_DATA_SECRET')),
                  query = query)
  results <- content(response)

  if (!is.null(results$error) && results$error == TRUE ||
      status_code(response) != 200) {
    stop(results$message)
  }

  if (returnResponse) {
    return(response)
  }

  return(results)
}

#' Get the number of rows in a dataset
#'
#' @param dataSetID the Socrata API data set ID (of the form xxxx-xxxx)
#' @param keyColumn the name of a column that exists for all rows
#'
#' @return a string representing the number of rows in the data set
#'
#' @examples getNumRows('9bhg-hcku', 'data_as_of')
getNumRows <- function(dataSetID, keyColumn) {
  results <- doGET(dataSetID,
                   list('$select' = str_c('count(', keyColumn, ')')))
  results[[1]][[1]] %>%
    return()
}

#' Download a full data set
#'
#' @param dataSetID the Socrata API data set ID (of the form xxxx-xxxx)
#' @param keyColumn the name of a column that exists for all rows
#'
#' @return a data frame containing the data set
#'
#' @examples loadData('vbim-akqf', 'current_status')
loadData <- function(dataSetID, keyColumn) {
  numRows <- getNumRows(dataSetID, keyColumn)

  doGET(dataSetID,
        list('$limit' = numRows),
        returnResponse = TRUE) %>%
    content(as = 'text') %>%
    fromJSON() %>%
    return()
}






fullData <- loadData('9bhg-hcku', 'data_as_of')
fullData <- fullData %>%
  mutate(start_date = str_sub(start_date, 1, 10),
         end_date = str_sub(end_date, 1, 10),
         footnote = NULL,
         data_as_of = NULL) %>%
  mutate_at(vars(matches("dat")), lubridate::ymd) %>%
  mutate_at(vars(group:age_group, year:month), as_factor) %>%
  mutate_at(vars(matches("deaths|covid")), as.numeric) %>%
  filter(state != "United States")

AGE_OPTIONS <- unique(fullData$age_group)
DEATH_COLUMN_OPTIONS <- c(
  'COVID-19 Deaths' = 'covid_19_deaths',
  'Influenza Deaths' = 'influenza_deaths',
  'Pneumonia Deaths' = 'pneumonia_deaths',
  'Pneumonia & COVID-19 Deaths' = 'pneumonia_and_covid_19_deaths',
  'Pneumonia, Influenza, or COVID-19 Deaths' = 'pneumonia_influenza_or_covid',
  'Total Deaths' = 'total_deaths'
)
GROUP_OPTIONS <- unique(fullData$group)
SEX_OPTIONS <- unique(fullData$sex)
STATE_OPTIONS <-
  c('All States',
    unique(as.character(fullData$state[fullData$state != 'Puerto Rico'])),
    recursive = TRUE)

# Pre conditions data

pre_conditions_data <- read_csv("../data/conditions.csv") %>%
  mutate_at(vars(data_as_of:end_date), lubridate::ymd) %>%
  mutate_at(vars(group:age_group), as_factor) %>%
  mutate_at(vars(covid_19_deaths, number_of_mentions), as.numeric) %>%
  filter(group == "By Month",
         !age_group == "Not stated") %>%
  select(-group)

ui <- fluidPage(
  titlePanel("ACCJ COVID-19 Shiny App"),
  tabsetPanel(
    tabPanel(
      "Explore",  # Exploratory data analysis
      fluidRow(
        column(5,
               sidebarPanel(
                 selectInput(inputId = "var1", label = "Variable (Univariate)", choices = names(fullData), selected = "covid_19_deaths"),
                 checkboxInput(inputId = "log1", label = "Log_Transform?", value = FALSE, width = NULL),
                 sliderInput(inputId = "bins1", label = "Bins", min = 1, max = 100, value = 50),
               )
        ),
        column(7,
               mainPanel(
                 plotOutput("plot1"),
               )
        )
      ),
      fluidRow(
        column(5,
               sidebarPanel(
                 selectInput("var2",label = "X Variable (Bivariate)",
                             choices = names(fullData),
                             selected = "age_group"),
                 checkboxInput("log2", "Log_Transform?", value = FALSE, width = NULL),
                 selectInput("var3",label = "Y Variable (Bivariate)",
                             choices = names(fullData),
                             selected = "covid_19_deaths"),
                 checkboxInput("log3", "Log_Transform?", value = FALSE, width = NULL),
                 checkboxInput("ols1", "Trendline", value = FALSE, width = NULL),
               )
        ),
        column(7,
               mainPanel(
                 plotOutput("plot2"),
               )
        )
      )
    ),
    tabPanel(
      "Compare",  # Bivariate data analysis and statistical modeling
      sidebarLayout(
        sidebarPanel(
          varSelectInput("option1", "X Variable:", data = pre_conditions_data %>% select_if(is.numeric), selected = "covid_19_deaths"),
          varSelectInput("option2", "Y Variable:", data = pre_conditions_data %>% select_if(is.factor), selected = "conditions"),
          selectInput("var4",label = "X OLS",
                                  choices = names(pre_conditions_data),
                                  selected = "conditions"),
          checkboxInput("log4", "Log_Transform?", value = FALSE, width = NULL),
          selectInput("var5",label = "Y OLS",
                      choices = names(pre_conditions_data),
                      selected = "covid_19_deaths"),
          checkboxInput("log5", "Log_Transform?", value = FALSE, width = NULL),
          checkboxInput("ols2", "Fit OLS?", value = FALSE, width = NULL)
          # Still need to input the code in the server section for summary output.
          # varSelectInput("option3", "AGE GRP:", data = pre_conditions_data %>% select_if(is.factor), selected = "age_group"),
          # varSelectInput("option4", "STATE:", data = pre_conditions_data %>% select_if(is.factor), selected = "state"),
          # selectizeInput('option2', 'Select variable 1', choices = c("choose" = "", levels(pre_conditions_data$condition))),
          # selectizeInput('option3', 'Select variable 2', choices = c("choose" = "", levels(pre_conditions_data$age_group))),
          # selectizeInput('option4', 'Select variable 3', choices = c("choose" = "", levels(pre_conditions_data$state)))
        ),
        mainPanel(
          plotOutput("plot"),
        )
      )
    ),
    tabPanel(
      'Explore by State',
      fluidRow(
        column(
          4,
          selectInput('deaths', 'Cause(s) of Death', DEATH_COLUMN_OPTIONS,
                      selected = 'COVID-19 Deaths'),
          selectInput('sex', 'Sex', SEX_OPTIONS, selected = 'All Sexes'),
          selectInput('age_group', 'Age Group', AGE_OPTIONS,
                      selected = 'All Ages'),
          selectInput('state', 'State', STATE_OPTIONS,
                      selected = 'All States'),
          selectInput('group', 'Grouping', GROUP_OPTIONS,
                      selected = 'By Total')
        ),
        column(
          8,
          plotOutput('map')
        )
      ),
      fluidRow(
        column(
          12,
          plotOutput('mapPlot')
        )
      )
    ),
    tabPanel(
      "Raw Data",
      tabsetPanel(
        tabPanel(
          "Death Counts",
          dataTableOutput("spreadsheet1")
        ),
        tabPanel(
          "Preconditions",
          dataTableOutput("spreadsheet2")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  mapData <- reactive({
    fullData %>%
      filter(state != 'Puerto Rico' &
             group == input$group &
             sex == input$sex &
             age_group == input$age_group) %>%
      mutate(deaths = .data[[input$deaths]]) ->
      result

    if (input$state != 'All States') {
      result <- filter(result, state == input$state)
    }

    result %>%
      inner_join(statepop, by = c('state' = 'full')) %>%
      return()
  })
  mapDeathsTitle <- reactive({
    names(DEATH_COLUMN_OPTIONS[DEATH_COLUMN_OPTIONS == input$deaths])[[1]]
  })

  output$map <- renderPlot({
    stateData <- mapData()

    if (input$group == 'By Total') {
      fillLabel <- 'Deaths'
    } else {
      fillLabel <- 'Cumulative\nDeaths'

      stateData %>%
        group_by(fips) %>%
        summarize(deaths = sum(deaths, na.rm = TRUE)) ->
        stateData
    }

    if (input$state != 'All States') {
      showLegend <- FALSE
    } else {
      showLegend <- TRUE
    }

    plot_usmap(regions = 'states',
               data = select(stateData, fips, deaths),
               values = 'deaths',
               show.legend = showLegend) +
      labs(fill = fillLabel) +
      scale_fill_viridis_c(labels = label_comma()) %>%
      return()
  })

  output$mapPlot <- renderPlot({
    deathsTitle <- mapDeathsTitle()
    plotData <- mapData()

    if (input$group == 'By Year') {
      plotData %>%
        ggplot(aes(year, deaths, fill = year)) +
        geom_col(show.legend = FALSE) +
        labs(title = str_c(deathsTitle, ' by Year'),
             x = 'Year',
             y = 'Number of Deaths') +
        scale_y_continuous(labels = label_comma()) +
        scale_fill_viridis_d(option = 'B') ->
        plot
    } else if (input$group == 'By Month') {
      plotData %>%
        mutate(monthYear = str_c(year, '-',
                                 str_pad(month, 2, 'left', '0'))) %>%
        ggplot(aes(monthYear, deaths, fill = year)) +
        geom_col() +
        labs(title = str_c(deathsTitle, ' by Month'),
             x = 'Month',
             y = 'Number of Deaths') +
        scale_y_continuous(labels = label_comma()) +
        scale_fill_viridis_d(option = 'B') ->
        plot
    } else {
      if (input$state != 'All States') {
        plotData %>%
          mutate(deaths = case_when(is.na(deaths) ~ -1,
                                    TRUE ~ deaths),
                 deathsText = case_when(deaths == -1 ~ 'NA',
                                        TRUE ~ label_comma()(deaths))) %>%
          ggplot(aes(deaths, group)) +
          geom_text(aes(label = deathsText), show.legend = FALSE,
                    size = 48, vjust = 'bottom') +
          geom_text(aes(label = deathsTitle),
                    size = 12, vjust = 'bottom', nudge_y = -0.333) +
          labs(title = NULL, x = NULL, y = NULL) +
          scale_x_continuous(breaks = NULL, labels = NULL) +
          scale_y_discrete(breaks = NULL, labels = NULL) +
          scale_fill_viridis_c() ->
          plot
      } else {
        plotData %>%
          mutate(plotData, state = fct_reorder(state, deaths)) %>%
          ggplot(aes(deaths, group, label = state)) +
          geom_col(aes(fill = deaths), show.legend = FALSE) +
          geom_label_repel(data = filter(plotData, deaths > 0),
                           position = position_stack(vjust = 0.5),
                           segment.color = 'white',
                           max.overlaps = 51,
                           direction = 'y') +
          labs(title = str_c('Combined ', deathsTitle),
               x = 'Number of Deaths',
               y = NULL) +
          scale_x_continuous(labels = label_comma()) +
          scale_y_discrete(breaks = NULL, labels = NULL) +
          scale_fill_viridis_c() ->
          plot
      }
    }

    plot +
      theme_bw() %>%
      return()
  })

  output$plot1 <- renderPlot({

    if (is.numeric(fullData[,input$var1])) {

      if(!input$log1)
      {
        ggplot(fullData, aes(x = .data[[input$var1]])) +
          geom_histogram(bins = input$bins1)
      }
      else
      {
        ggplot(fullData, aes(x = .data[[input$var1]]))+
          geom_histogram(bins = input$bins1) +
          scale_x_log10()
      }
    }

    else if (input$var1 == "state" || input$var1 == "age_group") {
      ggplot(fullData, aes(x = .data[[input$var1]])) +
        geom_bar() +
        coord_flip()
    }
    else {
      ggplot(fullData, aes(x = .data[[input$var1]])) +
        geom_bar()
    }
  })

  output$plot2 <- renderPlot({

    if (is.numeric(fullData[,input$var2])&&is.numeric(fullData[,input$var3])) {
      p2 <- ggplot(fullData, aes(x = .data[[input$var2]], y = .data[[input$var3]])) +
        geom_point()
      if(!input$log2 && !input$log3)
      {
        p2
      }
      else if(input$log2 && !input$log3)
      {
        p2<- p2 + scale_x_log10()
        p2
      }
      else if(!input$log2 && input$log3)
      {
        p2<- p2 + scale_y_log10()
        p2
      }
      else
      {
        p2<- p2 + scale_x_log10() + scale_y_log10()
        p2
      }

      if(input$ols1)
      {
        p2 <- p2 +geom_smooth(method='loess', formula = y~x, se=FALSE)
        p2
      }
      else
      {
        p2
      }

    }
    else if (input$var2 == "state" || input$var2 == "age_group") {
      if (!is.numeric(fullData[,input$var2])&&is.numeric(fullData[,input$var3])) {
        if (!input$log2 && !input$log3) {
          # Create p3 <- ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]]))
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + coord_flip()
        }
        else if (!input$log2 && input$log3) {
          # Create p3 <- ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]]))
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + scale_y_log10() + coord_flip()
        }
        else if (input$log2 && !input$log3) {
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          # Create p3 <- ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]]))
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + coord_flip()
        }
        else if (input$log2 && input$log3) {
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          # Create p3 <- ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]]))
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + scale_y_log10() + coord_flip()
        }
      }
      else if (!is.numeric(fullData[,input$var2])&&!is.numeric(fullData[,input$var3])) {
        ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
          geom_jitter() + coord_flip()
        # Issue: when age_group and state are on the x-axis, illegible.
        # Consider validate() to warn against plotting age_group vs state.
        # Add code for log transformations and add validate(): can't transform non-numerics, please select a numeric variable.
      }
    }
    else if (input$var2 != "state" || input$var2 != "age_group"){
      if (!is.numeric(fullData[,input$var2])&&is.numeric(fullData[,input$var3])) {
        if (!input$log2 && input$log3) {
          # Create p3 <- ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]]))
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + scale_y_log10()
        }
        else if (input$log2 && !input$log3){
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot()
        }
        else if (input$log2 && input$log3){
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + scale_y_log10()
        }
        else if (!input$log2 && !input$log3){
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot()
        }
      }
      else if (!is.numeric(fullData[,input$var2])&&is.numeric(fullData[,input$var3])) {
        ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
          geom_boxplot()
      }
      else if (!is.numeric(fullData[,input$var2])&&!is.numeric(fullData[,input$var3])) {
        ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
          geom_jitter()
      }
      else if (is.numeric(fullData[,input$var2])&&!is.numeric(fullData[,input$var3])) {
        if (input$log2 && !input$log3){
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + scale_x_log10() + ggstance::geom_boxploth()
        }
        else if (!input$log2 && input$log3){
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + ggstance::geom_boxploth()
        }
        else if (input$log2 && input$log3){
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + scale_x_log10() + ggstance::geom_boxploth()
        }
        else if (!input$log2 && !input$log3){
          ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot() + ggstance::geom_boxploth()
        }
      }
      else if (is.numeric(fullData[,input$var2])&&!is.numeric(fullData[,input$var3])) {
        ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
          geom_boxplot() + ggstance::geom_boxploth()
      }
    }
  })

  # varSelectInput("option1", "X Variable:", data = pre_conditions_data %>% select_if(is.numeric), selected = "covid_19_deaths"),
  # varSelectInput("option2", "Y Variable:", data = pre_conditions_data %>% select_if(is.factor), selected = "conditions")
  output$plot <- renderPlot({
    # generate plots based on variables selected above

    if (!!input$option2 == "age_group") {
      pre_conditions_data %>%
        ggplot(aes(x = !!input$option1, y = !!input$option2, fill = age_group)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        scale_x_log10() +
        scale_fill_brewer(palette = "Blues") +
        theme_gray()
    } else if (!!input$option2 == "state") {
      pre_conditions_data %>%
      group_by(state) %>%
        summarise(covid19_deaths = sum(covid_19_deaths)) %>%
        mutate(state = fct_reorder(state, covid19_deaths)) %>%
        ggplot(aes(covid19_deaths, state, fill = state)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        # scale_fill_brewer(palette = "Blues") +
        theme_gray()
    } else if (!!input$option2 == "condition") {
      pre_conditions_data %>%
      group_by(condition) %>%
        summarise(covid19_deaths = sum(covid_19_deaths)) %>%
        mutate(condition = fct_reorder(condition, covid19_deaths)) %>%
        ggplot(aes(covid19_deaths, condition, fill = condition)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        # scale_fill_brewer(palette = "Blues") +
        theme_gray()
    } else if (!!input$option2 == "condition_group") {
      pre_conditions_data %>%
      group_by(condition_group) %>%
        summarise(covid19_deaths = sum(covid_19_deaths)) %>%
        mutate(condition_group = fct_reorder(condition_group, covid19_deaths)) %>%
        ggplot(aes(covid19_deaths, condition_group, fill = condition_group)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        # scale_fill_brewer(palette = "Blues") +
        theme_gray()
    }


  })

  output$plot <- renderPlot({

    if (is.numeric(pre_conditions_data[,input$var4])&&is.numeric(pre_conditions_data[,input$var5])) {
      p4 <- ggplot(pre_conditions_data, aes(x = .data[[input$var4]], y = .data[[input$var5]])) +
        geom_point()
      if(!input$log4 && !input$log5)
      {
        p4
      }
      else if(input$log4 && !input$log5)
      {
        p4<- p4 + scale_x_log10()
        p4
      }
      else if(!input$log4 && input$log5)
      {
        p4<- p4 + scale_y_log10()
        p4
      }
      else
      {
        p4<- p4 + scale_x_log10() + scale_y_log10()
        p4
      }

      if(input$ols2)
      {
        p4 <- p4 +geom_smooth(method='lm', formula= y~x, se=FALSE)
        p4
      }
      else
      {
        p4
      }

    }
    else if (!is.numeric(pre_conditions_data[,input$var4])&&is.numeric(pre_conditions_data[,input$var5])&&!input$log4 && input$log5) {
      ggplot(pre_conditions_data, aes(x=.data[[input$var4]], y=.data[[input$var5]])) +
        geom_boxplot() + scale_y_log10()
    }
    else if (is.numeric(pre_conditions_data[,input$var4])&&!is.numeric(pre_conditions_data[,input$var5])&&input$log4 && !input$log5) {
      ggplot(pre_conditions_data, aes(x=.data[[input$var4]], y=.data[[input$var5]])) +
        geom_boxplot() + scale_x_log10() + ggstance::geom_boxploth()
    }
    else if (!is.numeric(pre_conditions_data[,input$var4])&&is.numeric(pre_conditions_data[,input$var5])) {
      ggplot(pre_conditions_data, aes(x=.data[[input$var4]], y=.data[[input$var5]])) +
        geom_boxplot()
    }
    else if (is.numeric(pre_conditions_data[,input$var4])&&!is.numeric(pre_conditions_data[,input$var5])) {
      ggplot(pre_conditions_data, aes(x=.data[[input$var4]], y=.data[[input$var5]])) +
        geom_boxplot() + ggstance::geom_boxploth()
    }

    else if (!is.numeric(pre_conditions_data[,input$var4])&&!is.numeric(pre_conditions_data[,input$var5])) {
      ggplot(pre_conditions_data, aes(x=.data[[input$var4]], y=.data[[input$var5]])) +
        geom_jitter()
    }
  })

  output$lm_results <- renderPrint({
    if(is.numeric(pre_conditions_data[,input$var4])&&is.numeric(pre_conditions_data[,input$var5])&&input$ols2) {
      lmout <- lm(formula = pre_conditions_data[[input$var5]]~pre_conditions_data[[input$var4]], data = pre_conditions_data)
      print(summary(lmout))
      if (!input$log4 && !input$log5) {
        lmout <- lm(formula = pre_conditions_data[[input$var5]]~pre_conditions_data[[input$var4]], data = pre_conditions_data)
        print(summary(lmout))
      }
    }
    else if(is.numeric(pre_conditions_data[,input$var4])&&is.numeric(pre_conditions_data[,input$var5])&&input$log4 && !input$log5) {
      lmout <- lm(formula = pre_conditions_data[[input$var5]]~log(pre_conditions_data[[input$var4]]), data = pre_conditions_data)
      print(summary(lmout))
    }
    else if(is.numeric(pre_conditions_data[,input$var4])&&is.numeric(pre_conditions_data[,input$var5])&&!input$log4 && input$log5) {
      lmout <- lm(formula = log(pre_conditions_data[[input$var5]])~pre_conditions_data[[input$var4]], data = pre_conditions_data)
      print(summary(lmout))
    }
    else if (is.numeric(pre_conditions_data[,input$var4])&&is.numeric(pre_conditions_data[,input$var5])&&input$log4 && input$log5) {
      lmout <- lm(formula = log(pre_conditions_data[[input$var5]])~log(pre_conditions_data[[input$var4]]), data = pre_conditions_data)
      print(summary(lmout))
    }

  })

  output$spreadsheet1 <- renderDataTable({
    fullData
  })
  output$spreadsheet2 <- renderDataTable({
    pre_conditions_data
  })
}

shinyApp(ui, server)
