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

DEATH_COLUMN_OPTIONS <- c(
  'COVID-19 Deaths' = 'covid_19_deaths',
  'Total Deaths' = 'total_deaths',
  'Pneumonia Deaths' = 'pneumonia_deaths',
  'Pneumonia & COVID-19 Deaths' = 'pneumonia_and_covid_19_deaths',
  'Influenza Deaths' = 'influenza_deaths',
  'Pneumonia, Influenza, or COVID-19 Deaths' = 'pneumonia_influenza_or_covid'
)

ui <- fluidPage(
  titlePanel("ACCJ COVID-19 Shiny App"),
  tabsetPanel(
    tabPanel(
      "Explore",  # Exploratory data analysis
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
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
      sidebarLayout(
        sidebarPanel(
          selectInput('deaths', 'Cause(s) of Death', DEATH_COLUMN_OPTIONS,
                      selected = 'COVID-19 Deaths'),
          selectInput('state', 'State', unique(fullData$state),
                      selected = 'United States'),  # TODO: change to "All States", remove PR
          selectInput('group', 'Group', unique(fullData$group),
                      selected = 'By Total'),
          selectInput('sex', 'Sex', unique(fullData$sex),
                      selected = 'All Sexes'),
          selectInput('age_group', 'Age Group', unique(fullData$age_group),
                      selected = 'All Ages')
        ),
        mainPanel(
          plotOutput('map')
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
  output$map <- renderPlot({
    fullData %>%
      filter(!(state %in% c('United States', 'Puerto Rico'))) %>%
      filter(group == input$group &
             sex == input$sex &
             age_group == input$age_group) %>%
      inner_join(statepop, by = c('state' = 'full')) ->
      stateData

    if (input$state != 'United States') {  # TODO: change to "All States"
      stateData %>%
        filter(state == input$state) ->
        stateData
    }

    stateData %>%
      select('fips', input$deaths) ->
      stateData

    stateData[[input$deaths]] <- as.integer(stateData[[input$deaths]])

    plot_usmap(regions = 'states',
               data = stateData,
               values = input$deaths)
  })

  output$spreadsheet <- renderDataTable({
    fullData
  })
}

shinyApp(ui, server)
