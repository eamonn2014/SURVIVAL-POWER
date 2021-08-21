# Module UI ------------------------------------------------------

#' @title mod_sidebar_ui mod_sidebar_server
#' @decription a shiny module.
#' 
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' 
#' @rdname mod_sidebar
#' 
#' @keywords internal
#' @export
#' @importFrom shiny NS


mod_survplot1_ui <- function(id){
  
  ns <- NS(id)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~
tabItem("survplot1",
        ##~~~~~~~~~~~~~~
        column(width=3,
               
               numericInput(ns("CSurvProp_input"), label = h5("Control survival probability"), value = 0.40),
               numericInput(ns("Time_input"), label = h5("Time at which we assess survival probability"), value = 0.40),
               numericInput(ns("surv.perc.change.improvement_input"), label = h5("Postulated percentage change in survival probability"), value = -15),
               
               selectInput(
               inputId=ns("PBox"),
               label="treatment xxx:",
               choices=c("A","B"),
               selected="A"),
              
         #~~~~~~~~~~~~~~~~~~~
         conditionalPanel(
        
               condition="inputPBox == 'A'",
              
               ns=ns,
              
          column(width=12),
          
               radioButtons(
                 inputId = ns("Example"),
                 label="Dateset:",
                 choices=c("X","Y"),
                 selected="X")),
              
        #~~~~~~~~~~~~~~~~~~~
         # conditionalPanel(
         #        
         #        condition="inputPBox == 'B'",
         #        
         #        ns=ns,
         #        
         #        column(width=12),
         #        radioButtons(
         #          inputId = ns("Example"),
         #          label="Dateset:",
         #          choices=c("X","Y"),
         #          selected="X")),
         #~~~~~~~~~~~~~~~~~~~
        ),
        ##~~~~~~~~~~~~~


  #~~~~~~~~~~~~~~~~~      
  mainPanel(
    
    conditionalPanel(condition="input.PBox == 'A'",
                     ns=ns, column(width=12,
                                   plotOutput(ns("survplot1")))),
  ),
  
  #~~~~~~~~~~~~~~~~
  
  tags$head(tags$style(HTML('content-wrapper { overflow: auto; }')))
)
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Module Server ------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @keywords internal
#' 
mod_survplot1_server <- function(input, output, session){
  
  ns <- session$ns
  
  output$survplot1 <- renderPlot({

    survplot1(  CSurvProp=input$CSurvProp_input, 
                time1=10, 
                surv.perc.change.improvement=-15 ) # %15 worse, red low
  
  
    })

}

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   
        
         