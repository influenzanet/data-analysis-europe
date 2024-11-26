#
source("conf.R")

init.path('ecdc/indicator')

library(shiny)
library(dplyr)
library(cowplot)
library(ggplot2)
library(forcats)
library(bslib)
library(htmlwidgets)
library(htmltools)
library(markdown)
library(shinyWidgets)
datasets = list(
  "bundles"=list(file="bundles.rds", title="Data prepared for public website (external + shared db)"),
  "externals"=list(file="externals.rds", title="Data from external sources (email), aggregated data are provided by each platform"),
  "all"=list(file="datasets.rds")
)

# Define UI for application that draws a histogram
ui <- page_sidebar(

    # Sidebar with a slider input for number of bins 
    sidebar = sidebar(
            selectInput("dataset", "Dataset",  c('Public data'='bundles', "All"='all', 'Externals'='externals')),
            pickerInput("countries","Platform codes", choices=NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),
            pickerInput("methods", "Computation Methods", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),
            pickerInput("seasons", "Seasons", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
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
                  plotOutput("activePlot")
                )
              ),
              tabPanel("Incidence", 
                fluidRow(
                  checkboxGroupInput("syndromes", "Syndromes", choices = NULL),
                  plotOutput("incidencePlot"),
                  plotOutput("countPlot")
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
    
    available_seasons = reactive({
      d = data()
      extract_data_field(d, "season")
    })
    
    available_syndromes = reactive({
      d = data()
      extract_data_field(d, "syndrome")
    })
    
    observe({
      
      cc = available_countries()
      
      updatePickerInput(session, "countries", choices=makeChoices(cc), selected = cc)
      mm = available_methods()
      
      updatePickerInput(session, "methods", choices=makeChoices(mm), selected=mm)
      ss = available_seasons()
      
      updatePickerInput(session, "seasons", choices=makeChoices(ss), selected=ss)

      sd = available_syndromes()
      updateCheckboxGroupInput(session, "syndromes", choices=makeChoices(sd), selected=sd)
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
      seasons = input$seasons
      d = dd %>% filter(country %in% countries, method %in% methods, season %in% seasons)
      ggplot(d, aes(x=monday_of_week(yw), y=active, color=method)) +
        geom_line() +
        facet_grid(rows=vars(country), cols=vars(season), scales="free")
    })
    
    incidence_data = reactive({
      dd = data()
      if(!"incidence" %in% names(dd)) {
        return(NULL)
      }
      dd = dd$incidence
      countries = input$countries
      methods = input$methods
      seasons = input$seasons
      syndromes = input$syndromes
      
      dd %>% filter(country %in% countries, method %in% methods, season %in% seasons, syndrome %in% syndromes)
      
    })
    
    
    output$incidencePlot <- renderPlot({
      
      dd = incidence_data()
      
      ggplot(dd, aes(x=monday_of_week(yw), y=incidence, color=syndrome, linetype=type)) +
        geom_line() +
        facet_grid(cols=vars(season), rows=vars(country), scales="free")
      
    })
    
    output$countPlot <- renderPlot({
      
      dd = incidence_data()
      
      dd = dd %>% filter(type == "adj") # Only count once (same value regardless adjustment)
      
      ggplot(dd, aes(x=monday_of_week(yw), y=count, color=syndrome)) +
        geom_line() +
        facet_grid(cols=vars(season), rows=vars(country), scales="free")
    })
    
    output$helpOutput <- renderUI({
      htmltools::includeMarkdown("help.md")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
