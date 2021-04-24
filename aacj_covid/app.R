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






# FIXME: 22 million rows is too big to download every time and on the fly;
#        we should probably use the CSV version instead?
# fullData <- loadData('vbim-akqf', 'current_status')

# TODO: use the smaller provisional data set for now
fullData <- loadData('9bhg-hcku', 'data_as_of')
fullData <- fullData %>%
  mutate(data_as_of = str_sub(data_as_of, 1, 10),
         start_date = str_sub(start_date, 1, 10),
         end_date = str_sub(end_date, 1, 10),
         footnote = NULL,
         data_as_of = NULL) %>%
  mutate_at(vars(matches("dat")), lubridate::ymd) %>%
  mutate_at(vars(group:age_group, year:month), as_factor) %>%
  mutate_at(vars(matches("deaths|covid")), as.numeric) %>%
  filter(state != "United States")

DEATH_COLUMN_OPTIONS <- c(
  'COVID-19 Deaths' = 'covid_19_deaths',
  'Total Deaths' = 'total_deaths',
  'Pneumonia Deaths' = 'pneumonia_deaths',
  'Pneumonia & COVID-19 Deaths' = 'pneumonia_and_covid_19_deaths',
  'Influenza Deaths' = 'influenza_deaths',
  'Pneumonia, Influenza, or COVID-19 Deaths' = 'pneumonia_influenza_or_covid'
)
STATE_OPTIONS <- c(
  'All States',
  unique(as.character(fullData$state[fullData$state != 'Puerto Rico'])),
  recursive = TRUE)


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
                 checkboxInput("ols1", "Fit OLS?", value = FALSE, width = NULL),
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
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(
      "Locate",  # Maps
      fluidRow(
        column(
          4,
          selectInput('deaths', 'Cause(s) of Death', DEATH_COLUMN_OPTIONS,
                      selected = 'COVID-19 Deaths'),
          selectInput('state', 'State', STATE_OPTIONS,
                      selected = 'All States'),
          selectInput('group', 'Group', unique(fullData$group),
                      selected = 'By Total'),
          selectInput('sex', 'Sex', unique(fullData$sex),
                      selected = 'All Sexes'),
          selectInput('age_group', 'Age Group', unique(fullData$age_group),
                      selected = 'All Ages')
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
      "Spreadsheet",  # Datasets
      dataTableOutput('spreadsheet')
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
      inner_join(statepop, by = c('state' = 'full')) ->
      result

    if (input$state != 'All States') {
      result %>%
        filter(state == input$state) ->
        result
    }

    return(result)
  })
  mapDeathsTitle <- reactive({
    names(DEATH_COLUMN_OPTIONS[DEATH_COLUMN_OPTIONS == input$deaths])[[1]]
  })

  output$map <- renderPlot({
    deathsTitle <- mapDeathsTitle()
    stateData <- mapData()

    if (input$group == 'By Total') {
      stateData <- select(stateData, 'fips', input$deaths)

      plot_usmap(regions = 'states',
                 data = stateData,
                 values = input$deaths) +
        labs(fill = 'Deaths') +
        scale_fill_continuous(labels = comma) +
        scale_fill_viridis_c()
    } else {
      stateData %>%
        group_by(state) %>%
        mutate(sumDeaths = sum(!!as.symbol(input$deaths), na.rm = TRUE)) %>%
        ungroup() %>%
        select(fips, sumDeaths) ->
        stateData

      plot_usmap(regions = 'states',
                 data = stateData,
                 values = 'sumDeaths') +
        labs(fill = 'Cumulative Deaths') +
        scale_fill_continuous(labels = comma) +
        scale_fill_viridis_c()
    }
  })

  output$mapPlot <- renderPlot({
    plotData <- mapData()
    deathsTitle <- mapDeathsTitle()

    if (input$group == 'By Year') {
      plotData %>%
        ggplot(aes(year, plotData[[input$deaths]], fill = year)) +
        geom_line() +
        geom_col(show.legend = FALSE) +
        labs(x = 'Year',
             y = str_c('Number of Deaths'),
             title = str_c(deathsTitle, ' by Year')) +
        scale_y_continuous(labels = comma) +
        scale_fill_viridis_d() +
        theme_bw()
    } else if (input$group == 'By Month') {
      plotData %>%
        group_by(year, month) %>%
        mutate(monthYear = str_c(year, '-', str_pad(month, 2, 'left', '0'))) %>%
        ggplot(aes(monthYear, plotData[[input$deaths]], fill = year)) +
        geom_line() +
        geom_col() +
        labs(x = 'Month',
             y = str_c('Number of Deaths'),
             title = str_c(deathsTitle, ' by Month')) +
        scale_y_continuous(labels = comma) +
        scale_fill_viridis_d() +
        theme_bw()
    }
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
        p2 <- p2 +geom_smooth(method='lm', formula= y~x, se=FALSE)
        p2
      }
      else
      {
        p2
      }

    }
    else if (!is.numeric(fullData[,input$var2])&&is.numeric(fullData[,input$var3])&&!input$log2 && input$log3) {
      ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
        geom_boxplot() + scale_y_log10()
    }
    else if (is.numeric(fullData[,input$var2])&&!is.numeric(fullData[,input$var3])&&input$log2 && !input$log3) {
      ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
        geom_boxplot() + scale_x_log10() + ggstance::geom_boxploth()
    }
    else if (!is.numeric(fullData[,input$var2])&&is.numeric(fullData[,input$var3])) {
      ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
        geom_boxplot()
    }
    else if (is.numeric(fullData[,input$var2])&&!is.numeric(fullData[,input$var3])) {
      ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
        geom_boxplot() + ggstance::geom_boxploth()
    }

    else if (!is.numeric(fullData[,input$var2])&&!is.numeric(fullData[,input$var3])) {
      ggplot(fullData, aes(x=.data[[input$var2]], y=.data[[input$var3]])) +
        geom_jitter()
    }
  })

  output$spreadsheet <- renderDataTable({
    fullData
  })
}

shinyApp(ui, server)
