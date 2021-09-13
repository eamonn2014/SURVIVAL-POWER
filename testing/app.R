library(shiny)
library(shinydashboard)

dataPlotUI <- function(id) {
  ns <- NS(id) # create namespace for entered ID
  fluidRow(
    box(plotOutput(ns("plot.1"), height = 400)),
    box(
      selectInput(
        ns("color.choice"), "Color:",
        c("darkcyan", "darkolivegreen", "deeppink", "lightsalmon2", "slateblue2", "springgreen3")
      ),
      sliderInput(ns("range"), label = "Range", min = 10, max = 100, value = 50)
    ) # end box
  )
}

# Module for Server
serverModule <- function(input, output, session, site) {
  ns <- session$ns
  
  output$plot.1 <- renderPlot({
    x <- seq(1, input$range, 1) # use slider to set max of x
    y <- x + rnorm(length(x), 0, 3)
    
    par(mai = c(.6, .6, .1, .1), las = 1, bty = "l")
    plot(y ~ x, pch = 20, col = input$color.choice)
  })
  
  observeEvent(session$userData$settings$chosenColour, {
    if (!is.na(session$userData$settings$chosenColour)) 
      updateSelectInput(session, "color.choice", selected=session$userData$settings$chosenColour)
  })
  
  rv <- reactive({input$color.choice})
  return(rv)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI
ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      # Icons can be found: https://fontawesome.com/icons?d=gallery&m=free
      menuItem("Tab Page 1", tabName = "tabA"),
      menuItem("Tab Page 2", tabName = "tabB"),
      menuItem("Tab Page 3", tabName = "tabC")
    )
  ), # End Dashboard Sidebar
  dashboardBody(
    # Start with overall tabItems
    tabItems(
      tabItem(
        tabName = "tabA",
        dataPlotUI("tab_one")
      ),
      #
      tabItem(
        tabName = "tabB",
        dataPlotUI("tab_two")
      ),
      
      tabItem(
        tabName = "tabC",
        dataPlotUI("tab_three")
      )
    )
  ) # end dashboard body
)

# Server
server <- function(input, output, session) {
  session$userData$settings <- reactiveValues(chosenColour=NA)
  tab1 <- callModule(serverModule, "tab_one")
  tab2 <- callModule(serverModule, "tab_two")
  tab3 <- callModule(serverModule, "tab_three")
  # Module observers
  observeEvent(tab1(), {session$userData$settings$chosenColour <- tab1()})
  observeEvent(tab2(), {session$userData$settings$chosenColour <- tab2()})
  observeEvent(tab3(), {session$userData$settings$chosenColour <- tab3()})
}

shinyApp(ui = ui, server = server)