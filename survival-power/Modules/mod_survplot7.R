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
     #   useShinyjs(),
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
                 h4(paste("This tab shows what can be exepected if a study is run over and over again. This is a very simple scenario. 
                          The inputs below define the number of simulations, sample size (1:1 randomisation) and population parameters:")),
                 br(),
               numericInput(inputId=ns("nsim_input"),                    label = c("No. of simulated studies"),                         value = 30, min=1,  max=1000, step=5),
               numericInput(inputId=ns("npergrp_input"),                 label = c("No. of patients per group"),                        value = 100, min=10,  max=1000, step=1),
               numericInput(inputId=ns("SurvProp_input"),                label = c("Survival probability"),                            value = 0.5, min=0.01,max=.95,  step=.01),
               numericInput(inputId=ns("tSurvNULL_input"),               label = c("Control survival time (green)"),                           value = 4,   min=1,   max=100,  step=1),
               numericInput(inputId=ns("tSurvALT_input"),                label = c("Intervention survival time (blue)"),                      value = 7,   min=1,   max=100,  step=1),
               br(),
               h4(paste("The button below refreshes and repeats the simulation. There is no practical need for this but is
                        just an exercise in programming whereby the plot is updated by either the inputs or the button. 
                        See the module mod_survplot7.R code which has some references that were helpful.")),
               br(),
               actionButton(ns("resample"),label=" Hit to run another simulation", icon = icon("th"),  width =300  )
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
# https://community.rstudio.com/t/run-code-just-when-actionbutton-is-pressed/53183/2
# https://www.rdataguy.com/2019/11/lesson-9-random-number-generator-part-2.html
# https://stackoverflow.com/questions/31548092/shiny-generate-random-values-every-time-press-button
# https://www.rdataguy.com/2019/11/lesson-9-random-number-generator-part-2.html
# https://stackoverflow.com/questions/61324097/how-to-apply-the-actionbutton-to-update-my-ggplot-in-shiny-in-r
 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 

#https://stackoverflow.com/questions/52898292/r-shiny-refreshing-plot-when-entering-input-or-pressing-an-action-button

mod_survplot7_server <- function(input, output, session){
  ns <- session$ns
  
  plotSettings <- reactiveValues()
  
  observeEvent(c(input$resample, 
                 input$nsim_input, input$npergrp_input, 
                 input$tSurvNULL_input, input$tSurvALT_input,
                 input$SurvProp_input  ), {
                   
                   plotSettings$A <- input$nsim_input
                   plotSettings$B <- input$npergrp_input
                   plotSettings$C <- input$tSurvNULL_input
                   plotSettings$D <- input$tSurvALT_input
                   plotSettings$E <- input$SurvProp_input 
                   
                 }, ignoreNULL = FALSE)
  
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # https://stackoverflow.com/questions/29496921/how-to-make-shiny-button-re-call-function-for-plot-with-no-input?rq=1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observe({   
    
    if(input$resample > -1) {   # if 0 plot only appears after first button hit! 
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      survplot7(  nsim     =plotSettings$A, 
                  npergroup=plotSettings$B,
                  tSurvNULL=plotSettings$C,
                  tSurvALT =plotSettings$D,
                  SurvProp =plotSettings$E) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      
      output$survplot7 <- renderPlot({
        
        
        survplot7(  nsim     =plotSettings$A, 
                    npergroup=plotSettings$B,
                    tSurvNULL=plotSettings$C,
                    tSurvALT =plotSettings$D,
                    SurvProp =plotSettings$E) 
   
      }, height=700, width=1000)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      
    }
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
}


 

