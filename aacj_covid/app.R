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

fullData_EDA <- fullData %>%
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>%
  filter(group == "By Month",
         !is.na(month),
         !is.na(year),
         sex != "All Sexes",
         age_group != "All Ages",
         state != "United States") %>%
  mutate(month = as_factor(month),
         year = as_factor(year)) %>%
  select(-c(group, start_date, end_date))

fullData_color <- fullData %>% select(year, month, sex, age_group)

fullData_EDA$year <- recode_factor(fullData_EDA$year, `1`="2020", `2`="2021")
fullData_EDA$month <- recode_factor(fullData_EDA$month, `1`="Jan", `2`="Feb", `3`="Mar", `4`="Apr", `5`="May", `6`="Jun", `7`="Jul", `8`="Aug", `9`="Sep", `10`="Oct", `11`="Nov", `12`="Dec")

fullData_EDA_1 <- fullData_EDA %>%
  select(-c(state,sex,age_group))

fullData_EDA_1$year <- recode_factor(fullData_EDA$year, `1`="2020", `2`="2021")

# fullData_EDA_2 <- fullData_EDA %>%
#   group_by(state, sex, age_group) %>%
#   summarise(total_deaths=sum(total_deaths),
#             covid_19_deaths = sum(covid_19_deaths),
#             pneumonia_deaths = sum(pneumonia_deaths),
#             pneumonia_and_covid_19_deaths = sum(pneumonia_and_covid_19_deaths),
#             influenza_deaths = sum(influenza_deaths),
#             pneumonia_influenza_or_covid = sum(pneumonia_influenza_or_covid),
#             .groups = "keep")

# ggplot(fullData_EDA_2, aes(x=sex, y=total_deaths)) +
#     geom_col(aes(fill = sex)) + coord_flip()

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

# Surveillance data
covid_surveillance_data <- read_csv("../data/covid_surveillance_df.rds") %>%
  as_tibble() %>%
  mutate_at(vars(current_status:medcond_yn), as_factor)

# Create recipe to up sample imbalanced variables
covid_surveillance <- recipes::recipe(~., data = covid_surveillance_data) %>%
  themis::step_upsample(death_yn) %>%
  themis::step_upsample(hosp_yn) %>%
  themis::step_upsample(icu_yn) %>%
  themis::step_upsample(medcond_yn) %>%
  themis::step_upsample(current_status)

# Prep model df
covid_surveillance %>%
  recipes::prep() %>%
  recipes::juice() -> covid_surveillance

library(rsample)
set.seed(123)
# Create a split object
modeldf_split <- rsample::initial_split(covid_surveillance_data, prop = 0.70)
# Build training data set
model_training <- modeldf_split %>%
  training()
# Build testing data set
model_test <- modeldf_split %>%
  testing()

# training models
train_death_model <- glm(death_yn ~.,family=binomial(link='logit'), data = model_training %>% select(-hosp_yn, -icu_yn))
train_hosp_model <- glm(hosp_yn ~.,family=binomial(link='logit'), data = model_training %>% select(-death_yn, -icu_yn))
train_icu_model <- glm(icu_yn ~.,family=binomial(link='logit'), data = model_training %>% select(-hosp_yn, -death_yn))

# Performance Metrics
# Pseudo R-squared
death_model_pr2 = pscl::pR2(train_death_model)["McFadden"]
hosp_model_pr2 = pscl::pR2(train_hosp_model)["McFadden"]
icu_model_pr2 = pscl::pR2(train_icu_model)["McFadden"]

#testing models
test_death_pred <- predict(train_death_model, model_test, type = "response")
test_hosp_pred <- predict(train_hosp_model, model_test, type = "response")
test_icu_pred <- predict(train_icu_model, model_test, type = "response")

# Performance Metrics
# Confusion Matrix
death_conMat = table(model_test$death_yn, test_death_pred > 0.5) %>% prop.table() %>% round(3)
hosp_conMat = table(model_test$hosp_yn, test_hosp_pred > 0.5) %>% prop.table() %>% round(3)
icu_conMat = table(model_test$icu_yn, test_icu_pred > 0.5) %>% prop.table() %>% round(3)


# Prediction on generated df
# death_pred <- predict(train_death_model, new.df, type = "response")
# hosp_pred <- predict(train_hosp_model, new.df, type = "response")
# icu_pred <- predict(train_icu_model, new.df, type = "response")


ui <- fluidPage(
  titlePanel("ACCJ COVID-19 Shiny App"),
  tabsetPanel(
    tabPanel(
      "Explore",  # Exploratory data analysis
      fluidRow(
        column(5,
               sidebarPanel(
                 selectInput(inputId = "var1", label = "Variable (Univariate)", choices = names(fullData_EDA_1), selected = "covid_19_deaths"),
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
                             choices = names(fullData_EDA),
                             selected = "pneumonia_deaths"),
                 checkboxInput("log2", "Log_Transform?", value = FALSE, width = NULL),
                 selectInput("var3",label = "Y Variable (Bivariate)",
                             choices = names(fullData_EDA),
                             selected = "covid_19_deaths"),
                 checkboxInput("log3", "Log_Transform?", value = FALSE, width = NULL),
                 varSelectInput(inputId = "color1",label = "color", data = fullData_color,selected = "age_group"),
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
      fluidRow(
        column(5,
               sidebarPanel(
                 varSelectInput("option1", "X Variable:", data = pre_conditions_data %>% select_if(is.numeric), selected = "covid_19_deaths"),
                 varSelectInput("option2", "Y Variable:", data = pre_conditions_data %>% select_if(is.factor), selected = "conditions"),
               )
        ),
        column(7,
               mainPanel(
                 plotOutput("plot"),
               )
        )
      ),
      br(),
      br(),
      fluidRow(
        column(5,
               sidebarPanel(
                 selectizeInput('option1', 'Gender', choices = levels(covid_surveillance_data$sex)),
                 selectizeInput('option2', 'Age Group', choices = levels(covid_surveillance_data$age_group)),
                 selectizeInput('option3', 'Race/Ethnicity', choices = levels(covid_surveillance_data$race_ethnicity_combined)),
                 selectizeInput('option4', 'Status', choices = levels(covid_surveillance_data$current_status)),
                 selectizeInput('option5', 'Medical Condition', choices = levels(covid_surveillance_data$medcond_yn)),
                 radioButtons('option6', 'Risk Models', choices = c("Death", "Hospital", "ICU"), selected = "Death"),
                 actionButton('option7', 'Risk', icon = icon("bullseye"), class = "btn-success"),
                 p("Click here to run risk model"),
                 br(),
                 br(),
                 p("McFadden's Log Likelihood"),
                 textOutput("pseudo_r2"),
                 br(),
                 br(),
                 p("Probable Risk with Exposure"),
                 textOutput("Chances"),
                 br(),
                 br(),
                 p("Confusion Matrix"),
                 verbatimTextOutput("Conf_Mat")

               )
        ),
        column(7,
               mainPanel(
                 plotOutput("rocPlot"),

               )
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
        ),
        tabPanel(
          "Surveillance",
          dataTableOutput("spreadsheet3")
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

    if (is.numeric(fullData_EDA_1[,input$var1])) {

      if(!input$log1)
      {
        ggplot(fullData_EDA_1, aes(x = .data[[input$var1]])) +
          geom_histogram(bins = input$bins1,aes(fill=..count..), show.legend = FALSE) +
          scale_fill_gradient("Count", low="green", high="red")
      }
      else
      {
        ggplot(fullData_EDA_1, aes(x = .data[[input$var1]]))+
          geom_histogram(bins = input$bins1,aes(fill=..count..), show.legend = FALSE) +
          scale_x_log10() +
          scale_fill_gradient("Count", low="green", high="red")
      }
    }

    else if (input$var1 == "state" || input$var1 == "age_group") {
      ggplot(fullData_EDA_1, aes(x = .data[[input$var1]])) +
        geom_bar(aes(fill=..count..), show.legend = FALSE) +
        coord_flip() +
        scale_fill_gradient("Count", low="darkgreen", high="darkred")
    }
    else {
      ggplot(fullData_EDA_1, aes(x = .data[[input$var1]])) +
        geom_bar(aes(fill=..count..), show.legend = FALSE) +
        scale_fill_gradient("Count", low="darkgreen", high="darkred")
    }
  })

  output$plot2 <- renderPlot({

    if (is.numeric(fullData_EDA[,input$var2])&&is.numeric(fullData_EDA[,input$var3])) {
      p2 <- ggplot(fullData_EDA, aes(x = .data[[input$var2]], y = .data[[input$var3]])) +
        geom_point(aes(color = !!input$color1))
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
      p3 <- ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
        geom_boxplot(aes(color = .data[[input$var2]]), show.legend = FALSE)
      if (!is.numeric(fullData_EDA[,input$var2])&&is.numeric(fullData_EDA[,input$var3])) {
        if (!input$log2 && !input$log3) {
          p3 + coord_flip()
        }
        else if (!input$log2 && input$log3) {
          p3 + scale_y_log10() + coord_flip()
        }
        else if (input$log2 && !input$log3) {
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          p3 + coord_flip()
        }
        else if (input$log2 && input$log3) {
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          p3 + scale_y_log10() + coord_flip()
        }
      }
      else if (!is.numeric(fullData_EDA[,input$var2])&&!is.numeric(fullData_EDA[,input$var3])) {
        ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
          geom_jitter() + coord_flip()
        # Issue: when age_group and state are on the x-axis, illegible.
        # Consider validate() to warn against plotting age_group vs state.
        # Add code for log transformations and add validate(): can't transform non-numerics, please select a numeric variable.
      }
    }
    else if (input$var2 != "state" || input$var2 != "age_group"){
      p3 <- ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
        geom_boxplot(aes(color = .data[[input$var2]]), show.legend = FALSE)
      if (!is.numeric(fullData_EDA[,input$var2])&&is.numeric(fullData_EDA[,input$var3])) {
        if (!input$log2 && input$log3) {
          p3 + scale_y_log10()
        }
        else if (input$log2 && !input$log3){
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          p3
        }
        else if (input$log2 && input$log3){
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          p3 + scale_y_log10()
        }
        else if (!input$log2 && !input$log3){
          p3
        }
      }
      else if (!is.numeric(fullData_EDA[,input$var2])&&is.numeric(fullData_EDA[,input$var3])) {
        ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
          geom_boxplot()
      }
      else if (!is.numeric(fullData_EDA[,input$var2])&&!is.numeric(fullData_EDA[,input$var3])) {
        ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
          geom_jitter()
      }
      else if (is.numeric(fullData_EDA[,input$var2])&&!is.numeric(fullData_EDA[,input$var3])) {
        p3 <- ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
          geom_boxplot(aes(color = .data[[input$var2]]), show.legend = FALSE)
        if (input$log2 && !input$log3){
          p3 + scale_x_log10() + ggstance::geom_boxploth()
        }
        else if (!input$log2 && input$log3){
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          p3 + ggstance::geom_boxploth()
        }
        else if (input$log2 && input$log3){
          # validate(): cannot log a non-numeric variable, please select a numeric variable.
          p3 + scale_x_log10() + ggstance::geom_boxploth()
        }
        else if (!input$log2 && !input$log3){
          p3 + ggstance::geom_boxploth()
        }
      }
      else if (is.numeric(fullData_EDA[,input$var2])&&!is.numeric(fullData_EDA[,input$var3])) {
        ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
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

  # Build new df

  tib <- reactive({
    new.df <- tibble(!!input$option1, !!input$option2, !!input$option3, !!input$option4, !!input$option5)
    new.df
  })

  # Activate action button
  risk <- eventReactive(input$option7, {

    if (input$option6 == "Death") {
      death_pred <- predict(train_death_model, new.df, type = "response")
      death_pred
    } else if (input$option6 == "Hospital") {
      hosp_pred <- predict(train_hosp_model, new.df, type = "response")
      hosp_pred
    } else if (input$option6 == "ICU") {
      icu_pred <- predict(train_icu_model, new.df, type = "response")
      icu_pred
    }
  })


  # Select model
  output$Chances <- renderText({

    risk()
  })

  #   # Generate Performance Metrics - Pseudo-r2 - McFadden
  output$pseudo_r2 <- renderText({
    if (input$option6 == "Death") {
      death_model_pr2
    } else if (input$option6 == "Hospital") {
      hosp_model_pr2
    } else if (input$option6 == "ICU") {
      icu_model_pr2
    }

  })
  #
  #   # Generate Confusion Matrix
  output$Conf_Mat <- renderPrint({
    if (input$option6 == "Death") {
      death_conMat
    } else if (input$option6 == "Hospital") {
      hosp_conMat
    } else if (input$option6 == "ICU") {
      icu_conMat
    }

  })

  #   # Generate AUC plot
  output$rocPlot <- renderPlot({
    if (input$option6 == "Death") {
      ROCR::prediction(test_death_pred, model_test$death_yn) %>%
        ROCR::performance(measure = "tpr", x.measure = "fpr") %>%
        plot()
    } else if (input$option6 == "Hospital") {
      ROCR::prediction(test_hosp_pred, model_test$hosp_yn) %>%
        ROCR::performance(measure = "tpr", x.measure = "fpr") %>%
        plot()
    } else if (input$option6 == "ICU") {
      ROCR::prediction(test_icu_pred, model_test$icu_yn) %>%
        ROCR::performance(measure = "tpr", x.measure = "fpr") %>%
        plot()
    }

  })

  output$spreadsheet1 <- renderDataTable({
    fullData
  })
  output$spreadsheet2 <- renderDataTable({
    pre_conditions_data
  })
  output$spreadsheet3 <- renderDataTable({
    covid_surveillance_data
  })
}


shinyApp(ui, server)
