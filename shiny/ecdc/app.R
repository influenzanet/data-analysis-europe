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

incidence_types = c("Raw"="raw", "Adjusted"="adj")

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
                  checkboxGroupInput("types", "Types", choices = incidence_types, selected = incidence_types),
                  checkboxInput("confint", "Show Confidence interval", value = TRUE),
                  plotOutput("incidencePlot", fill = FALSE),
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

plot_country_incidence = function(d, country.labeller=identity, plot.confint=FALSE) {
  
  if(plot.confint) {
    g_ic = geom_ribbon(aes(ymin=lower, ymax=upper, fill=syndrome), color=NA, alpha=.40)
  } else {
    g_ic = NULL
  }
  
  country = country.labeller(as.character(d$country[1]))
  
  g1 = ggplot(d, aes(x=monday_of_week(yw), y=incidence, color=syndrome, linetype=type)) +
    geom_line() +
    g_ic +
    facet_grid(cols=vars(season), scales="free") +
    labs(x="", title=paste("Country ", country), y="Incidence rate") + 
    theme(
      axis.text.x = element_blank(), 
      plot.margin=margin(b=0), 
      axis.title = element_blank(),
      title=element_text(size=rel(1.3))
    ) 
  
  g2 =  ggplot(d, aes(x=monday_of_week(yw), y=count, color=syndrome, linetype=type)) +
    geom_step() +
    facet_grid(cols=vars(season), scales="free", margins = F) +
    theme(strip.text=element_blank(), title = element_blank()) +
    labs(x="Week", y="Count of participants")
  cowplot::plot_grid(g1, g2, ncol = 1, align = "v")
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
      
      updatePickerInput(session, "seasons", choices=makeChoices(ss), selected=tail(sort(ss), n=2))

      sd = available_syndromes()
      updateCheckboxGroupInput(session, "syndromes", choices=makeChoices(sd), selected=sd)
    })
    
    incidence_plot_height = reactive({
      cc = available_countries()
      100 * length(cc)
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
      
      confint = input$confint
      
      dd = dd %>% filter(type %in% input$types)
      
      gg = by(dd, dd$country, plot_country_incidence, plot.confint=confint)
      gg = Filter(function(x) !is.null(x), gg)
      do.call(grid.arrange, list(grobs=gg, ncol=1))
    }, height = incidence_plot_height)
    
    output$helpOutput <- renderUI({
      htmltools::includeMarkdown("help.md")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
