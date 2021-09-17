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


mod_testing_ui <- function(id){
  
  ns <- NS(id)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # the values are created here
  setupInput<-function(id){
    ns<-NS(id)
    tagList(
      sliderInput(ns("bins"), "Number of bins:",
                  min = 1,  max = 50, value = 30),
      # numericInput(inputId=ns("nsim_input"),                    label = c("No. of simulated studies"),                         value = 30, min=1,  max=1000, step=5),
      # numericInput(inputId=ns("npergrp_input"),                 label = c("No. of patients per group"),                        value = 100, min=10,  max=1000, step=1),
      # numericInput(inputId=ns("SurvProp_input"),                label = c("Survival probability"),                            value = 0.5, min=0.01,max=.95,  step=.01),
      # numericInput(inputId=ns("tSurvNULL_input"),               label = c("Control survival time (green)"),                           value = 4,   min=1,   max=100,  step=1),
      # numericInput(inputId=ns("tSurvALT_input"),                label = c("Intervention survival time (blue)"),                      value = 7,   min=1,   max=100,  step=1),      
      # 
      
    )                                                         
  }
  
 
  # end new
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tabItem("testing",
          ##~~~~~~~~~~~~~~
          column(width=3,
                 tagList(
                   h4(paste("xxxxxxxxxxxxxxxxxxxxxxxxxxx")),
                   br(),
                   numericInput(inputId=ns("nsim_input"),                    label = c("No. of simulated studies"),                         value = 30, min=1,  max=1000, step=5),
                   numericInput(inputId=ns("npergrp_input"),                 label = c("No. of patients per group"),                        value = 100, min=10,  max=1000, step=1),
                   numericInput(inputId=ns("SurvProp_input"),                label = c("Survival probability"),                            value = 0.5, min=0.01,max=.95,  step=.01),
                   numericInput(inputId=ns("tSurvNULL_input"),               label = c("Control survival time (green)"),                           value = 4,   min=1,   max=100,  step=1),
                   numericInput(inputId=ns("tSurvALT_input"),                label = c("Intervention survival time (blue)"),                      value = 7,   min=1,   max=100,  step=1),
                   br(),
                   h4(paste("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.")),
                   br(),
                   
                   #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                   setupInput("basic"),
                   #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                   
                   actionButton(ns("resample"),"Hit to run another simulation",icon=icon("bell"), width =300 ,
                                class = "btn action-button",
                                style = "color: white;
                           background-color: blue")
                 )
                 
                 
                 
          ),
          ##~~~~~~~~~~~~~
        #
        
        sidebarLayout(sidebarPanel(
          
        ),
        
          mainPanel(
            plotOutput(ns("survplot7")),
           
            
            #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
            chartUI("first"),     #  new
            chartUI("second")     #  new
            #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
            
          )),
          #~~~~~~~~~~~~~~~~
          
          tags$head(tags$style(HTML('content-wrapper { overflow: auto; }')))
  )
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Module Server ------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @keywords internal


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@NEW

  setup<-function(input,output,session){       # hmm
    # How to display all input values in a table
    output$inputs<-renderTable({
      reactiveValuesToList(input)
     })
     return(input)
  }
   
  chartUI <- function(id) ({   # this allows multiple plots?
    ns <- NS(id)
    plotOutput(ns("distPlot"))
  })
  
  chart <- function(input, output, session, setup) {  # construct plot takes input from 'setup'
    output$distPlot <- renderPlot({
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = setup$bins + 1)
      hist(x,
           breaks = bins,
           col = 'darkgray',
           border = 'white')
    })
  }
#https://stackoverflow.com/questions/52898292/r-shiny-refreshing-plot-when-entering-input-or-pressing-an-action-button
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@END NEW


mod_testing_server <- function(input, output, session){
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
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  #new
 
   
 
    
    
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
}



