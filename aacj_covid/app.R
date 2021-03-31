library(shiny)

ui <- fluidPage(titlePanel("ACCJ COVID-19 Shiny App"),
                tabsetPanel( tabPanel(title = "Adim",value = 1),
                             tabPanel(title = "Chidi",value = 2),  
                             tabPanel(title = "Corey",value = 3),
                             tabPanel(title = "Jerome", value = 4),
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
                                 mainPanel()),
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)