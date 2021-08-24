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


mod_survplot6_ui <- function(id){
  
  ns <- NS(id)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~
tabItem("survplot6",
        ##~~~~~~~~~~~~~~
        column(width=3,
               tagList(
               # changed labels from h5("Postulated percentage change in survival probability")
               numericInput(inputId=ns("CSurvProp_input"),                    label = c("Control survival probability"),                         value = 0.40, min=0.05,max=.95, step=.01),
               numericInput(inputId=ns("Time_input"),                         label = c("Time at which we assess control survival probability"),         value = 10, step=1),
               numericInput(inputId=ns("HR_input"), label = c("Postulated HR"), value = 2, min=.1,max=20, step=.1),
               )
        ),
        ##~~~~~~~~~~~~~
 
  mainPanel(
   
     plotOutput(ns("survplot6"))
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
mod_survplot6_server <- function(input, output, session){
  
  ns <- session$ns
  
  output$survplot6 <- renderPlot({

    # this function id in the global.R file
    survplot6(  CSurvProp=input$CSurvProp_input, 
                time1=input$Time_input, 
                HR=input$HR_input )  
  
  
    }, height=700, width=1000)

   
}

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   
        
         