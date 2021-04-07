#
# Developer documentation:
#   https://dev.socrata.com/foundry/data.cdc.gov/9bhg-hcku

# Dataset documentation:
#   https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku


library(shiny)
library(httr)
library(keyring)
library(tidyverse)


#' getNumRows
#'
#' @param dataSetID the Socrata API data set ID (of the form xxxx-xxxx)
#' @param keyColumn the name of a column that exists for all rows
#'
#' @return an integer representing the number of rows in the data set
#'
#' @examples getNumRows('9bhg-hcku', 'data_as_of')
getNumRows <- function (dataSetID, keyColumn) {
    dataSetURL <- str_c('https://data.cdc.gov/resource/', dataSetID, '.json')
    selectValue <- str_c('count(', keyColumn, ')')

    response <- GET(dataSetURL,
                    authenticate(key_get('CDC_DATA'),
                                 key_get('CDC_DATA_SECRET')),
                    query = list('$select' = selectValue))
    results <- content(response)

    if (!is.null(results$error) && results$error == TRUE ||
            status_code(response) != 200) {
        # TODO: display results$message
    }

    return(as.integer(results[[1]][[1]]))
}

numRows <- getNumRows('9bhg-hcku', 'data_as_of')


ui <- fluidPage(titlePanel("ACCJ COVID-19 Shiny App"),
                tabsetPanel( tabPanel(title = "Explore",value = 1), # exploratory data analysis
                             tabPanel(title = "Compare",value = 2), # Bivariate data analysis and statistical modelling
                             tabPanel(title = "Locate", value = 3), # Maps
                             tabPanel(title = "Spreadsheet", value = 4), # Datasets.
                             id="tab"),
                conditionalPanel(condition = "input.tab==1",
                                 sidebarPanel(),
                                 mainPanel()),
                conditionalPanel(condition = "input.tab==2",
                                 sidebarPanel(),
                                 mainPanel()),
                conditionalPanel(condition = "input.tab==3",
                                 sidebarPanel(),
                                 mainPanel()),
                conditionalPanel(condition = "input.tab==4",
                                 sidebarPanel(),
                                 mainPanel())
           

)

server <- function(input, output, session) {

}

shinyApp(ui, server)
