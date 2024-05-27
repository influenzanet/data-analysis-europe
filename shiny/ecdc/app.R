#
source("conf.R")

init.path('ecdc/indicator')

library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)
library(bslib)
library(htmlwidgets)
library(htmltools)
library(markdown)

datasets = list(
  "externals"=list(file="externals.rds", title="Data from external sources (email), aggregated data are provided by each platform"),
  "bundles"=list(file="bundles.rds", title="Data prepared for public website (external + shared db)"),
  "all"=list(file="datasets.rds")
)



# Define UI for application that draws a histogram
ui <- page_sidebar(

    # Sidebar with a slider input for number of bins 
    sidebar = sidebar(
            selectInput("dataset", "Dataset",  c('Externals'='externals', 'Public data'='bundles', "All"='all')),
            checkboxGroupInput("countries","Platform codes", choices=NULL),
            checkboxGroupInput("methods", "Computation Methods", choices = NULL)
        ),

        # Show a plot of the generated distribution
        card(
          fluidRow(
            card(
              textOutput("dataset_title")
            ),
            tabsetPanel(
              tabPanel("Active participants", 
                fluidRow(
                  radioButtons("active_seasonal", "By season", choices=c("Yes"=TRUE, "No"=FALSE), inline = TRUE, selected = FALSE),
                  plotOutput("activePlot")
                )
              ),
              tabPanel("Incidence", 
                fluidRow(
                  radioButtons("incidence_seasonal", "By season", choices=c("Yes"=TRUE, "No"=FALSE), inline = TRUE, selected = FALSE),
                  plotOutput("incidencePlot")
                )
              ),
              tabPanel("help",
                  htmlOutput("helpOutput")
              )
              
            )
          )
    )
)



makeChoices = function(v) {
  setNames(v,v)
}

extract_data_field = function(d, field) {
  v = c()
  if(hasName(d, "active")) {
    v = d$active[[field]]
  }
  if(hasName(d, "incidence")) {
    v = d$incidence[[field]]
  }
  unique(v)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    data = reactive({
      dataset = datasets[[input$dataset]]
      if(is.null(dataset)) {
        return(list())
      }
      file = dataset$file
      d = as.list(readRDS(my.path(file)))
      if(hasName(d, "active")) {
        d$active$method = forcats::fct_recode(d$active$method, unknown="unknow")
      }
      if(hasName(d, "incidence")) {
        d$incidence$method = forcats::fct_recode(d$incidence$method, unknown="unknow")
      }
      d
    })
    
    available_countries = reactive({
      d = data()
      extract_data_field(d, "country")
    })
    
    available_methods = reactive({
      d = data()
      extract_data_field(d, "method")
    })
    
    observe({
      cc = available_countries()
      updateCheckboxGroupInput(session, "countries", choices=makeChoices(cc), selected = cc)
      mm = available_methods()
      updateCheckboxGroupInput(session, "methods", choices=makeChoices(mm), selected=mm)
    })
    
    output$dataset_title <- renderText({
      dataset = datasets[[input$dataset]]
      if(is.null(dataset)) {
        return(paste("Unknow dataset", sQuote(input$dataset)))
      }
      dataset$title
    })
    
    
    output$activePlot <- renderPlot({
      dd = data()
      if(!"active" %in% names(dd)) {
        return(NULL)
      }
      dd = dd$active
      countries = input$countries
      methods = input$methods
      d = dd %>% filter(country %in% countries)
      d = d %>% filter(method %in% methods)
      scales = "free_y"
      if(input$active_seasonal) {
        cols = vars(season)
        scales="free"
      } else {
        cols = NULL
      }
      ggplot(d, aes(x=monday_of_week(yw), y=active, color=method)) +
        geom_line() +
        facet_grid(rows=vars(country), cols=cols, scales=scales)
    })
    
    output$incidencePlot <- renderPlot({
      dd = data()
      if(!"active" %in% names(dd)) {
        return(NULL)
      }
      dd = dd$active
      countries = input$countries
      methods = input$methods
      d = dd %>% filter(country %in% countries)
      d = d %>% filter(method %in% methods)
      scales = "free_y"
      if(input$incidence_seasonal) {
        cols = vars(season)
        scales="free"
      } else {
        cols = NULL
      }
      ggplot(d, aes(x=monday_of_week(yw), y=active, color=method)) +
        geom_line() +
        facet_grid(rows=vars(country), cols=cols, scales=scales)
    })
    
    output$helpOutput <- renderUI({
      htmltools::includeMarkdown("help.md")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
