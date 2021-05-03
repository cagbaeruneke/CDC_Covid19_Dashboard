library(shiny)
library(httr)
library(keyring)
library(tidyverse)
library(jsonlite)
library(usmap)
library(scales)
library(ggrepel)
library(rsample)

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

set.seed(123)
# Create a split object
modeldf_split <- rsample::initial_split(covid_surveillance, prop = 0.70)
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

# New df 
new_data <- covid_surveillance_data %>%
  select(-death_yn, -hosp_yn, -icu_yn) 

ui <- fluidPage(
  titlePanel("ACCJ COVID-19 Shiny App"),
  tabsetPanel(
    tabPanel(
      "Explore",
      fluidRow(
        column(
          5,
          selectInput(inputId = "dataset",label = "Choose a dataset:", choices = c("Death Counts", "Preconditions", "Surveillance"), selected = "Death Counts"),
          conditionalPanel(
            'input.dataset == "Death Counts"',
            selectInput(inputId = "var1", label = "Variable (Univariate)", choices = names(fullData_EDA), selected = "covid_19_deaths"),
            checkboxInput(inputId = "log1", label = "Log_Transform?", value = FALSE, width = NULL),
            sliderInput(inputId = "bins1", label = "Bins", min = 1, max = 100, value = 50)
          ),
          conditionalPanel(
            'input.dataset == "Preconditions"',
            selectInput(inputId = "bvar1", label = "Variable (Univariate)", choices = names(pre_conditions_data)),
            checkboxInput(inputId = "blog1", label = "Log_Transform?", value = FALSE, width = NULL),
            sliderInput(inputId = "bbins1", label = "Bins", min = 1, max = 100, value = 50)
          ),
          conditionalPanel(
            'input.dataset == "Surveillance"',
            selectInput(inputId = "cvar1", label = "Variable (Univariate)", choices = names(covid_surveillance_data)),
            checkboxInput(inputId = "clog1", label = "Log_Transform?", value = FALSE, width = NULL),
            sliderInput(inputId = "cbins1", label = "Bins", min = 1, max = 100, value = 50)
          )
        ),
        column(
          7,
          h4("Univariate Graph"),
          plotOutput("plot1")
        )
      ),
      fluidRow(
        column(
          5,
          conditionalPanel(
            'input.dataset == "Death Counts"',
            selectInput("var2",label = "X Variable (Multivariate)",
                        choices = names(fullData_EDA),
                        selected = "pneumonia_deaths"),
            checkboxInput("log2", "Log_Transform?", value = FALSE, width = NULL),
            selectInput("var3",label = "Y Variable (Multivariate)",
                        choices = names(fullData_EDA),
                        selected = "covid_19_deaths"),
            checkboxInput("log3", "Log_Transform?", value = FALSE, width = NULL),
            varSelectInput(inputId = "color1",label = "color", data = fullData_color,selected = "age_group"),
            checkboxInput("trend1", "Trendline", value = FALSE, width = NULL)
          ),
          conditionalPanel(
            'input.dataset == "Preconditions"',
            selectInput("bvar2",label = "X Variable (Multivariate)",
                        choices = names(pre_conditions_data)),
            checkboxInput("blog2", "Log_Transform?", value = FALSE, width = NULL),
            selectInput("bvar3",label = "Y Variable (Multivariate)",
                        choices = names(pre_conditions_data)),
            checkboxInput("blog3", "Log_Transform?", value = FALSE, width = NULL),
            varSelectInput("bcolor1",label = "color", data = pre_conditions_data),
            checkboxInput("btrend1", "Trendline", value = FALSE, width = NULL)
          ),
          conditionalPanel(
            'input.dataset == "Surveillance"',
            selectInput("cvar2",label = "X Variable (Multivariate)",
                        choices = names(covid_surveillance_data)),
            checkboxInput("clog2", "Log_Transform?", value = FALSE, width = NULL),
            selectInput("cvar3",label = "Y Variable (Multivariate)",
                        choices = names(covid_surveillance_data)),
            checkboxInput("clog3", "Log_Transform?", value = FALSE, width = NULL),
            varSelectInput("ccolor1",label = "color", data = covid_surveillance_data),
            checkboxInput("ctrend1", "Trendline", value = FALSE, width = NULL)
          ),
          helpText("Note: cannot log a non-numeric variable,", 
                   "please select a numeric variable,",
                   "if available.")
        ),
        column(
          7,
          h4("Multivariate Graph"),
          plotOutput("plot2")
        )
      )
    ),
    tabPanel(
      "Compare",
      fluidRow(
        column(
          5,
          varSelectInput("option1", "X Variable:", data = pre_conditions_data %>% select_if(is.numeric), selected = "covid_19_deaths"),
          varSelectInput("option2", "Y Variable:", data = pre_conditions_data %>% select_if(is.factor), selected = "conditions")
        ),
        column(
          7,
          plotOutput("plot")
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          5,
          selectizeInput('option3', 'Status', choices = levels(covid_surveillance$current_status)),
          selectizeInput('option4', 'Gender', choices = levels(covid_surveillance$sex)),
          selectizeInput('option5', 'Age Group', choices = levels(covid_surveillance$age_group)),
          selectizeInput('option6', 'Race/Ethnicity', choices = levels(covid_surveillance$race_ethnicity_combined)),
          selectizeInput('option7', 'Medical Condition', choices = levels(covid_surveillance$medcond_yn)),
          radioButtons('option8', 'Risk Models', choices = c("Death", "Hospital", "ICU"), selected = "Death"),
          actionButton('option9', 'Risk', icon = icon("bullseye"), class = "btn-success"),
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
        ),
        column(
          7,
          tableOutput("view"),
          plotOutput("rocPlot")
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
    
    if (input$dataset == "Death Counts"){
      
      if (is.numeric(fullData_EDA[,input$var1])) {
        
        if(!input$log1)
        {
          ggplot(fullData_EDA, aes(x = .data[[input$var1]])) +
            geom_histogram(bins = input$bins1,aes(fill=..count..), show.legend = TRUE) +
            scale_fill_gradient("Count", low="green", high="red") +
            theme_minimal()
        }
        else
        {
          ggplot(fullData_EDA, aes(x = .data[[input$var1]]))+
            geom_histogram(bins = input$bins1,aes(fill=..count..), show.legend = TRUE) +
            scale_x_log10() +
            scale_fill_gradient("Count", low="green", high="red") +
            theme_minimal()
        }
      }
      
      else if (input$var1 == "state" || input$var1 == "age_group") {
        ggplot(fullData_EDA, aes(x = .data[[input$var1]])) +
          geom_bar(aes(fill=..count..), show.legend = TRUE) +
          scale_fill_gradient("Count", low="darkgreen", high="darkred") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45))  
      }
      else {
        ggplot(fullData_EDA, aes(x = .data[[input$var1]])) +
          geom_bar(aes(fill=..count..), show.legend = TRUE) +
          scale_fill_gradient("Count", low="darkgreen", high="darkred") +
          theme_minimal()
      }
    }
    else if (input$dataset == "Preconditions"){
      
      if (is.numeric(pre_conditions_data[,input$bvar1])) {
        
        if(!input$blog1)
        {
          ggplot(pre_conditions_data, aes(x = .data[[input$bvar1]])) +
            geom_histogram(bins = input$bbins1,aes(fill=..count..), show.legend = TRUE) +
            scale_fill_gradient("Count", low="green", high="red") +
            theme_minimal()
        }
        else
        {
          ggplot(pre_conditions_data, aes(x = .data[[input$bvar1]]))+
            geom_histogram(bins = input$bbins1,aes(fill=..count..), show.legend = TRUE) +
            scale_x_log10() +
            scale_fill_gradient("Count", low="green", high="red") +
            theme_minimal()
        }
      }
      
      else if (input$bvar1 == "state" || input$bvar1 == "age_group" || input$bvar1 == "condition" || input$bvar1 == "condition_group") {
        ggplot(pre_conditions_data, aes(x = .data[[input$bvar1]])) +
          geom_bar(aes(fill=..count..), show.legend = TRUE) +
          scale_fill_gradient("Count", low="darkgreen", high="darkred") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45))  
      }
      else {
        ggplot(pre_conditions_data, aes(x = .data[[input$bvar1]])) +
          geom_bar(aes(fill=..count..), show.legend = TRUE) +
          scale_fill_gradient("Count", low="darkgreen", high="darkred") +
          theme_minimal()
      }
    }
    else if (input$dataset == "Surveillance"){
      
      if (is.numeric(covid_surveillance_data[,input$cvar1])) {
        
        if(!input$clog1)
        {
          ggplot(covid_surveillance_data, aes(x = .data[[input$cvar1]])) +
            geom_histogram(bins = input$cbins1,aes(fill=..count..), show.legend = TRUE) +
            scale_fill_gradient("Count", low="green", high="red") +
            theme_minimal()
        }
        else
        {
          ggplot(covid_surveillance_data, aes(x = .data[[input$cvar1]]))+
            geom_histogram(bins = input$cbins1,aes(fill=..count..), show.legend = TRUE) +
            scale_x_log10() +
            scale_fill_gradient("Count", low="green", high="red") +
            theme_minimal()
        }
      }
      
      else if (input$cvar1 == "state" || input$cvar1 == "age_group" || input$cvar1 == "race_ethnicity") {
        ggplot(covid_surveillance_data, aes(x = .data[[input$cvar1]])) +
          geom_bar(aes(fill=..count..), show.legend = TRUE) +
          scale_fill_gradient("Count", low="darkgreen", high="darkred") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45))  
      }
      else {
        ggplot(covid_surveillance_data, aes(x = .data[[input$cvar1]])) +
          geom_bar(aes(fill=..count..), show.legend = TRUE) +
          scale_fill_gradient("Count", low="darkgreen", high="darkred") +
          theme_minimal()
      }
    }
  })

  output$plot2 <- renderPlot({
    
    if (input$dataset == "Death Counts"){
      
      if (is.numeric(fullData_EDA[,input$var2])&&is.numeric(fullData_EDA[,input$var3])) {
        p2 <- ggplot(fullData_EDA, aes(x = .data[[input$var2]], y = .data[[input$var3]])) +
          geom_point(aes(color = !!input$color1), show.legend = TRUE) +
          theme_minimal()
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
        
        if(input$trend1)
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
        p3 <- ggplot(fullData_EDA, 
                     aes(x=.data[[input$var2]], 
                         y=.data[[input$var3]])) +
          geom_boxplot(aes(color = .data[[input$var2]]), 
                       show.legend = TRUE) +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45))
        if (!is.numeric(fullData_EDA[,input$var2])&&is.numeric(fullData_EDA[,input$var3])) {
          if (!input$log2 && !input$log3) {
            p3 
          }
          else if (!input$log2 && input$log3) {
            p3 + scale_y_log10() 
          }
          else if (input$log2 && !input$log3) {
            p3
          }
          else if (input$log2 && input$log3) {
            
            p3 + scale_y_log10()
          }
        }
        else if (!is.numeric(fullData_EDA[,input$var2])&&!is.numeric(fullData_EDA[,input$var3])) {
          ggplot(fullData_EDA, 
                 aes(x=.data[[input$var2]], 
                     y=.data[[input$var3]])) +
            geom_jitter(aes(color = !!input$color1), 
                        show.legend = TRUE) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) 
          
        }
      }
      else if (input$var2 != "state" || input$var2 != "age_group"){
        p3 <- ggplot(fullData_EDA, 
                     aes(x=.data[[input$var2]], 
                         y=.data[[input$var3]])) +
          geom_boxplot(aes(color = .data[[input$var2]]), 
                       show.legend = TRUE) + 
          theme_minimal()
        if (!is.numeric(fullData_EDA[,input$var2])&&is.numeric(fullData_EDA[,input$var3])) {
          if (!input$log2 && input$log3) {
            p3 + scale_y_log10()
          }
          else if (input$log2 && !input$log3){
            
            p3
          }
          else if (input$log2 && input$log3){
            
            p3 + scale_y_log10()
          }
          else if (!input$log2 && !input$log3){
            p3
          }
        }
        else if (!is.numeric(fullData_EDA[,input$var2])&&is.numeric(fullData_EDA[,input$var3])) {
          ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_boxplot(aes(color = .data[[input$var2]]), show.legend = TRUE) + theme_minimal()
        }
        else if (!is.numeric(fullData_EDA[,input$var2])&&!is.numeric(fullData_EDA[,input$var3])) {
          ggplot(fullData_EDA, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
            geom_jitter(aes(color = !!input$color1), show.legend = TRUE) + 
            theme_minimal()
        }
        else if (is.numeric(fullData_EDA[,input$var2])&&!is.numeric(fullData_EDA[,input$var3])) {
          p3 <- ggplot(fullData_EDA, 
                       aes(x=.data[[input$var2]], 
                           y=.data[[input$var3]])) + 
            ggstance::geom_boxploth(aes(color = .data[[input$var3]]), 
                                    show.legend = TRUE) + 
            theme_minimal()
          
          if (input$log2 && !input$log3){
            p3 + scale_x_log10() 
          }
          else if (!input$log2 && input$log3){
            
            p3 
          }
          else if (input$log2 && input$log3){
            
            p3 + scale_x_log10() 
          }
          else if (!input$log2 && !input$log3){
            p3 
          }
        }
        else if (is.numeric(fullData_EDA[,input$var2])&&!is.numeric(fullData_EDA[,input$var3])) {
          ggplot(fullData_EDA, 
                 aes(x=.data[[input$var2]], 
                     y=.data[[input$var3]]))  + 
            ggstance::geom_boxploth(aes(color = .data[[input$var3]]), 
                                    show.legend = TRUE) + 
            theme_minimal()
        }
      }
    }
    else if (input$dataset == "Preconditions"){
      
      if (is.numeric(pre_conditions_data[,input$bvar2])&&is.numeric(pre_conditions_data[,input$bvar3])) {
        p4 <- ggplot(pre_conditions_data, aes(x = .data[[input$bvar2]], y = .data[[input$bvar3]])) +
          geom_point(aes(color = !!input$bcolor1), show.legend = TRUE) +
          theme_minimal()
        if(!input$blog2 && !input$blog3)
        {
          p4
        }
        else if(input$blog2 && !input$blog3)
        {
          p4<- p4 + scale_x_log10()
          p4
        }
        else if(!input$blog2 && input$blog3)
        {
          p4<- p4 + scale_y_log10()
          p4
        }
        else
        {
          p4<- p4 + scale_x_log10() + scale_y_log10()
          p4
        }
        
        if(input$btrend1)
        {
          p4 <- p4 +geom_smooth(method='loess', formula = y~x, se=FALSE)
          p4
        }
        else
        {
          p4
        }
        
      }
      else if (input$bvar2 == "state" || input$bvar2 == "age_group" || input$bvar2 == "condition" || input$bvar2 == "condition_group") {
        p5 <- ggplot(pre_conditions_data, 
                     aes(x=.data[[input$bvar2]], 
                         y=.data[[input$bvar3]])) +
          geom_boxplot(aes(color = .data[[input$bvar2]]), 
                       show.legend = TRUE) +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45))
        if (!is.numeric(pre_conditions_data[,input$bvar2])&&is.numeric(pre_conditions_data[,input$bvar3])) {
          if (!input$blog2 && !input$blog3) {
            p5 
          }
          else if (!input$blog2 && input$blog3) {
            p5 + scale_y_log10() 
          }
          else if (input$blog2 && !input$blog3) {
            p5
          }
          else if (input$blog2 && input$blog3) {
            
            p5 + scale_y_log10()
          }
        }
        else if (!is.numeric(pre_conditions_data[,input$bvar2])&&!is.numeric(pre_conditions_data[,input$bvar3])) {
          ggplot(pre_conditions_data, 
                 aes(x=.data[[input$bvar2]], 
                     y=.data[[input$bvar3]])) +
            geom_jitter(aes(color = !!input$bcolor1), 
                        show.legend = TRUE) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) 
          
        }
      }
      else if (input$bvar2 != "state" || input$bvar2 != "age_group" || input$bvar2 != "condition" || input$bvar2 != "condition_group"){
        p5 <- ggplot(pre_conditions_data, 
                     aes(x=.data[[input$bvar2]], 
                         y=.data[[input$bvar3]])) +
          geom_boxplot(aes(color = .data[[input$bvar2]]), 
                       show.legend = TRUE) + 
          theme_minimal()
        if (!is.numeric(pre_conditions_data[,input$bvar2])&&is.numeric(pre_conditions_data[,input$bvar3])) {
          if (!input$blog2 && input$blog3) {
            p5 + scale_y_log10()
          }
          else if (input$blog2 && !input$blog3){
            
            p5
          }
          else if (input$blog2 && input$blog3){
            
            p5 + scale_y_log10()
          }
          else if (!input$blog2 && !input$blog3){
            p5
          }
        }
        else if (!is.numeric(pre_conditions_data[,input$bvar2])&&is.numeric(pre_conditions_data[,input$bvar3])) {
          ggplot(pre_conditions_data, aes(x=.data[[input$bvar2]], y=.data[[input$bvar3]])) +
            geom_boxplot(aes(color = .data[[input$bvar2]]), show.legend = TRUE) + theme_minimal()
        }
        else if (!is.numeric(pre_conditions_data[,input$bvar2])&&!is.numeric(pre_conditions_data[,input$bvar3])) {
          ggplot(pre_conditions_data, aes(x=.data[[input$bvar2]], y=.data[[input$bvar3]])) +
            geom_jitter(aes(color = !!input$bcolor1), show.legend = TRUE) + 
            theme_minimal()
        }
        else if (is.numeric(pre_conditions_data[,input$bvar2])&&!is.numeric(pre_conditions_data[,input$bvar3])) {
          p5 <- ggplot(pre_conditions_data, 
                       aes(x=.data[[input$bvar2]], 
                           y=.data[[input$bvar3]])) + 
            ggstance::geom_boxploth(aes(color = .data[[input$bvar3]]), 
                                    show.legend = TRUE) + 
            theme_minimal()
          
          if (input$blog2 && !input$blog3){
            p5 + scale_x_log10() 
          }
          else if (!input$blog2 && input$blog3){
            
            p5 
          }
          else if (input$blog2 && input$blog3){
            
            p5 + scale_x_log10() 
          }
          else if (!input$blog2 && !input$blog3){
            p5 
          }
        }
        else if (is.numeric(pre_conditions_data[,input$bvar2])&&!is.numeric(pre_conditions_data[,input$bvar3])) {
          ggplot(pre_conditions_data, 
                 aes(x=.data[[input$bvar2]], 
                     y=.data[[input$bvar3]]))  + 
            ggstance::geom_boxploth(aes(color = .data[[input$bvar3]]), 
                                    show.legend = TRUE) + 
            theme_minimal()
        }
      }
    }
    else if (input$dataset == "Surveillance"){
      
      if (is.numeric(covid_surveillance_data[,input$cvar2])&&is.numeric(covid_surveillance_data[,input$cvar3])) {
        p6 <- ggplot(covid_surveillance_data, aes(x = .data[[input$cvar2]], y = .data[[input$cvar3]])) +
          geom_point(aes(color = !!input$ccolor1), show.legend = TRUE) +
          theme_minimal()
        if(!input$clog2 && !input$clog3)
        {
          p6
        }
        else if(input$clog2 && !input$clog3)
        {
          p6<- p6 + scale_x_log10()
          p6
        }
        else if(!input$clog2 && input$clog3)
        {
          p6<- p6 + scale_y_log10()
          p6
        }
        else
        {
          p6<- p6 + scale_x_log10() + scale_y_log10()
          p6
        }
        
        if(input$ctrend1)
        {
          p6 <- p6 +geom_smooth(method='loess', formula = y~x, se=FALSE)
          p6
        }
        else
        {
          p6
        }
        
      }
      else if (input$cvar2 == "state" || input$cvar2 == "age_group" || input$cvar2 == "race_ethnicity") {
        p7 <- ggplot(covid_surveillance_data, 
                     aes(x=.data[[input$cvar2]], 
                         y=.data[[input$cvar3]])) +
          geom_boxplot(aes(color = .data[[input$cvar2]]), 
                       show.legend = TRUE) +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45))
        if (!is.numeric(covid_surveillance_data[,input$cvar2])&&is.numeric(covid_surveillance_data[,input$cvar3])) {
          if (!input$clog2 && !input$clog3) {
            p7 
          }
          else if (!input$clog2 && input$clog3) {
            p7 + scale_y_log10() 
          }
          else if (input$clog2 && !input$clog3) {
            p7
          }
          else if (input$clog2 && input$clog3) {
            
            p7 + scale_y_log10()
          }
        }
        else if (!is.numeric(covid_surveillance_data[,input$cvar2])&&!is.numeric(covid_surveillance_data[,input$cvar3])) {
          ggplot(covid_surveillance_data, 
                 aes(x=.data[[input$cvar2]], 
                     y=.data[[input$cvar3]])) +
            geom_jitter(aes(color = !!input$ccolor1), 
                        show.legend = TRUE) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) 
          
        }
      }
      else if (input$cvar2 != "state" || input$cvar2 != "age_group" || input$cvar2 != "race_ethnicity"){
        p7 <- ggplot(covid_surveillance_data, 
                     aes(x=.data[[input$cvar2]], 
                         y=.data[[input$cvar3]])) +
          geom_boxplot(aes(color = .data[[input$cvar2]]), 
                       show.legend = TRUE) + 
          theme_minimal()
        if (!is.numeric(covid_surveillance_data[,input$cvar2])&&is.numeric(covid_surveillance_data[,input$cvar3])) {
          if (!input$clog2 && input$clog3) {
            p7 + scale_y_log10()
          }
          else if (input$clog2 && !input$clog3){
            
            p7
          }
          else if (input$clog2 && input$clog3){
            
            p7 + scale_y_log10()
          }
          else if (!input$clog2 && !input$clog3){
            p7
          }
        }
        else if (!is.numeric(covid_surveillance_data[,input$cvar2])&&is.numeric(covid_surveillance_data[,input$cvar3])) {
          ggplot(covid_surveillance_data, aes(x=.data[[input$cvar2]], y=.data[[input$cvar3]])) +
            geom_boxplot(aes(color = .data[[input$cvar2]]), show.legend = TRUE) + theme_minimal()
        }
        else if (!is.numeric(covid_surveillance_data[,input$cvar2])&&!is.numeric(covid_surveillance_data[,input$cvar3])) {
          ggplot(covid_surveillance_data, aes(x=.data[[input$cvar2]], y=.data[[input$cvar3]])) +
            geom_jitter(aes(color = !!input$ccolor1), show.legend = TRUE) + 
            theme_minimal()
        }
        else if (is.numeric(covid_surveillance_data[,input$cvar2])&&!is.numeric(covid_surveillance_data[,input$cvar3])) {
          p7 <- ggplot(covid_surveillance_data, 
                       aes(x=.data[[input$cvar2]], 
                           y=.data[[input$cvar3]])) + 
            ggstance::geom_boxploth(aes(color = .data[[input$cvar3]]), 
                                    show.legend = TRUE) + 
            theme_minimal()
          
          if (input$clog2 && !input$clog3){
            p7 + scale_x_log10() 
          }
          else if (!input$clog2 && input$clog3){
            
            p7 
          }
          else if (input$clog2 && input$clog3){
            
            p7 + scale_x_log10() 
          }
          else if (!input$clog2 && !input$clog3){
            p7 
          }
        }
        else if (is.numeric(covid_surveillance_data[,input$cvar2])&&!is.numeric(covid_surveillance_data[,input$cvar3])) {
          ggplot(covid_surveillance_data, 
                 aes(x=.data[[input$cvar2]], 
                     y=.data[[input$cvar3]]))  + 
            ggstance::geom_boxploth(aes(color = .data[[input$cvar3]]), 
                                    show.legend = TRUE) + 
            theme_minimal()
        }
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
        ggplot(aes(covid19_deaths, state, fill = covid19_deaths)) +
        geom_bar(stat = "identity", show.legend = FALSE) 
       
    } else if (!!input$option2 == "condition") {
      pre_conditions_data %>%
        group_by(condition) %>%
        summarise(covid19_deaths = sum(covid_19_deaths)) %>%
        mutate(condition = fct_reorder(condition, covid19_deaths)) %>%
        ggplot(aes(covid19_deaths, condition, fill = covid19_deaths)) +
        geom_bar(stat = "identity", show.legend = FALSE) 
        
    } else if (!!input$option2 == "condition_group") {
      pre_conditions_data %>%
        group_by(condition_group) %>%
        summarise(covid19_deaths = sum(covid_19_deaths)) %>%
        mutate(condition_group = fct_reorder(condition_group, covid19_deaths)) %>%
        ggplot(aes(covid19_deaths, condition_group, fill = covid19_deaths)) +
        geom_bar(stat = "identity", show.legend = FALSE) 
        
    }
  })
  
  # Create a new df from the test data
  output$view <- renderTable({
    new.df() %>%
      sample_n(1, replace = TRUE)
    
  })
  
  new.df <- reactive({
    new_data %>%
      filter(current_status == input$option3 &
               sex == input$option4 &
               age_group == input$option5 &
               race_ethnicity_combined == input$option6 &
               medcond_yn == input$option7) 
  }) 
  
  
  
  # Activate action button
  
  risk <- eventReactive(input$option9, {
    
    if (input$option8 == "Death") {
      death_pred <- predict(train_death_model, new.df(), type = "response")
      death_pred
      
    } else if (input$option8 == "Hospital") {
      hosp_pred <- predict(train_hosp_model, new.df(), type = "response")
      hosp_pred
    } else if (input$option8 == "ICU") {
      icu_pred <- predict(train_icu_model, new.df(), type = "response")
      icu_pred
    }
    
  })
  
  # Select model
  output$Chances <- renderText({
    
    risk()[[1]]
  })
  
  loglik <- eventReactive(input$option9, { 
    
    # Generate Performance Metrics - Pseudo-r2 - McFadden
    # output$pseudo_r2 <- renderText({
    if (input$option8 == "Death") {
      death_model_pr2
    } else if (input$option8 == "Hospital") {
      hosp_model_pr2
    } else if (input$option8 == "ICU") {
      icu_model_pr2
    }
  })  
  # }) 
  
  # See performance
  output$pseudo_r2 <- renderText({
    
    loglik()
  })
  
  #   # Generate Confusion Matrix
  conMatrx <- eventReactive(input$option9, {
    
    # output$Conf_Mat <- renderPrint({
    if (input$option8 == "Death") {
      death_conMat
    } else if (input$option8 == "Hospital") {
      hosp_conMat
    } else if (input$option8 == "ICU") {
      icu_conMat
    }

  })
  
  # See matrix
  output$Conf_Mat <- renderPrint({
    
    conMatrx()
  })
  
  #   # Generate AUC plot
  rocCurve <- eventReactive(input$option9, {
    
    # output$rocPlot <- renderPlot({
    if (input$option8 == "Death") {
      ROCR::prediction(test_death_pred, model_test$death_yn) %>%
        ROCR::performance(measure = "tpr", x.measure = "fpr") %>%
        plot()
    } else if (input$option8 == "Hospital") {
      ROCR::prediction(test_hosp_pred, model_test$hosp_yn) %>%
        ROCR::performance(measure = "tpr", x.measure = "fpr") %>%
        plot()
    } else if (input$option8 == "ICU") {
      ROCR::prediction(test_icu_pred, model_test$icu_yn) %>%
        ROCR::performance(measure = "tpr", x.measure = "fpr") %>%
        plot()
    }

  })
  
  # See ROC Curve
  output$rocPlot <- renderPlot({
    
    rocCurve()
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
