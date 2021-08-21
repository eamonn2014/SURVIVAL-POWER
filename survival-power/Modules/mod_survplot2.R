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


mod_survplot2_ui <- function(id){
  
  ns <- NS(id)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~
tabItem("survplot2",
        ##~~~~~~~~~~~~~~
        column(width=3,
               
               # changed lables from h5("Postulated percentage change in survival probability")
               numericInput(inputId=ns("CSurvProp_input"),                    label = c("Control survival probability"),                         value = 0.40, min=0.05,max=.95, step=.01),
               numericInput(inputId=ns("Time_input"),                         label = c("Time at which we fix control survival probability"),         value = 10, step=1),
               numericInput(inputId=ns("surv.perc.change.improvement_input"), label = c("Postulated percentage change in survival probability"), value = -15, min=-200,max=200, step=1),
       
        ),
        ##~~~~~~~~~~~~~


  #~~~~~~~~~~~~~~~~~      
  mainPanel(
    
    conditionalPanel(condition="input.PBox == 'A'",
                     ns=ns, column(width=12,
                                   plotOutput(ns("survplot2")))),
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
mod_survplot2_server <- function(input, output, session){
  
  ns <- session$ns
  
  output$survplot2 <- renderPlot({

    # this function id in the global.R file
    survplot2(  CSurvProp=input$CSurvProp_input, 
                time1=input$Time_input, 
                surv.perc.change.improvement=input$surv.perc.change.improvement_input )  
  
  
    }, height=700, width=1000)

   
}

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   
        
         