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


mod_survplot7_ui <- function(id){
  
  ns <- NS(id)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~
tabItem("survplot7",
        ##~~~~~~~~~~~~~~
        
        # inputs
        # n     <- 100                             # no of patients per group
        # ms1   <- 4                               # median survival null
        # ms2   <- 7                               # median survival alternative
        # p     <- 0.50                            # time for survival probability
        # fact. <- (ms2-ms1)/ms1                   # % increase in time for survival of this means
        # h     <- lambda   <-  log(2)/ms1         # baseline hazard (reference)
        # h2    <- lambda2  <-  lambda/(1+fact.)   # alternate hazard
        # hr    <- lambda/lambda2                  # hr compares to alternate/longer survival
        
        
        column(width=3,
               tagList(
               numericInput(inputId=ns("nsim_input"),                    label = c("no of simulated studies"),                         value = 30, min=1,  max=1000, step=5),
               numericInput(inputId=ns("npergrp_input"),                 label = c("no of patients per group"),                        value = 100, min=10,  max=1000, step=1),
               numericInput(inputId=ns("SurvProp_input"),                label = c("Survival probability"),                            value = 0.5, min=0.01,max=.95,  step=.01),
               numericInput(inputId=ns("tSurvNULL_input"),               label = c("Control survival time (green)"),                           value = 4,   min=1,   max=100,  step=1),
               numericInput(inputId=ns("tSurvALT_input"),                label = c("Intervention survival time (blue)"),                      value = 7,   min=1,   max=100,  step=1),
               br(),
               actionButton("goButton", "Go!")
                          )
        ),
        ##~~~~~~~~~~~~~
 
  mainPanel(
   
     plotOutput(ns("survplot7"))
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
mod_survplot7_server <- function(input, output, session){
  
  ns <- session$ns
  
  # values <- reactiveValues(
  #   nsim_input = 31,
  #   npergrp_input = 100,
  #   SurvProp_input = .5,
  #   tSurvNULL_input =4,
  #   tSurvALT_input =7
 # )
  
  # observeEvent(input$goButton, {
    
    # values$nsim_input      <- input$nsim_input
    # values$npergrp_input   <- input$npergrp_input
    # values$tSurvNULL_input <- input$tSurvNULL_input
    # values$tSurvALT_input  <- input$tSurvALT_input
    # values$SurvProp_input  <- input$SurvProp_input

# })
  
# go button not working
  
  
  
  observeEvent(
    list(input$goButton,
         input$nsim_input,input$npergrp_input, input$tSurvNULL_input,input$tSurvALT_input,input$SurvProp_input),{
  
           
           
           
  output$survplot7 <- renderPlot(
    
    survplot7(  nsim     =input$nsim_input, 
                npergroup=input$npergrp_input,
                tSurvNULL=input$tSurvNULL_input,
                tSurvALT =input$tSurvALT_input,
                SurvProp =input$SurvProp_input )  
    
   , height=700, width=1000)
 
  
  })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   
}
         