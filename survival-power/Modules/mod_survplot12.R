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
#' #https://stackoverflow.com/questions/52898292/r-shiny-refreshing-plot-when-entering-input-or-pressing-an-action-button



mod_survplot12_ui <- function(id){
  
  ns <- NS(id)
  
#-----------------------------------------------------------------------------------------------------------------------      
  # the values are created here
  setupInput<-function(id){
    ns<-NS(id)
    tagList(

#-----------------------      
      numericInput(inputId=ns("tref"),  label = c("Reference time*"),                         value = 6,  min=0.5,max=200, step=.5),
      numericInput(inputId=ns("hr"), label = c("Hazard Ratio*"),                              value = 2/3, min=0.01,max=10, step=.01),
      numericInput(inputId=ns("mc"), label = c("Mortality control arm at reference time*"),   value = .5, min=0,max=1, step=.05),
      numericInput(inputId=ns("n"),     label = c("Total sample size (1:1 randomisation)"),   value = 352, step=1),
      numericInput(inputId=ns("tmin"), label = c("Minimum follow up time"),                  value = 39/4, min=0,max=100, step=1),
      numericInput(inputId=ns("accrual"), label = c("Length of accrual"),                    value = 74/4, min=0,max=100, step=1),
      numericInput(inputId=ns("noncomp.c"), label = c("Non compliance control"),             value = 0, min=0,max=1, step=.1),
      numericInput(inputId=ns("noncomp.i"), label = c("Non compliance intervention"),        value = 0, min=0,max=1, step=.1),
      numericInput(inputId=ns("alpha"), label = c("alpha"),                                  value = 0.05, min=0.01,max=.5, step=.01),
      br(),
     )                                                         
  }
  
  #-----------------------------------------------------------------------------------------------------------------------      
  
  
  #-----------------------------------------------------------------------------------------------------------------------      
  tabItem("survplot12",
          ##~~~~~~~~~~~~~~
          column(width=3,
                 tagList(
                   setupInput("basic"),
         
                 )
               
          ),
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          # sidebarLayout(sidebarPanel(
          #   
          # ),
          
          mainPanel(
            h4(paste("Frank Harrell's Hmisc::cpower")),
            chartUI2("first"),  
            chartUI("first"),      
          #)
          ),
          #~~~~~~~~~~~~~~~~
          
          tags$head(tags$style(HTML('content-wrapper { overflow: auto; }')))
  )
  #-----------------------------------------------------------------------------------------------------------------------      
}

#-----------------------------------------------------------------------------------------------------------------------      
#-----------------------------------------------------------------------------------------------------------------------      
#-----------------------------------------------------------------------------------------------------------------------      


# Module Server ------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @keywords internal

# --------------------------------------------------------------------------------------------

  setup<-function(input,output,session){       # hmm
    # How to display all input values in a table
    output$inputs<-renderTable({
      reactiveValuesToList(input)
    })
    return(input)
  }

  # ------------------------------ 
  
  chartUI <- function(id) ({   # this allows multiple plots
    ns <- NS(id)
    plotOutput(ns("distPlot"))
  })

# ------------------------------ 

  chartUIx <- function(id) ({   # this allows multiple plots
    ns <- NS(id)
    plotOutput(ns("distPlotx"))
  })

# --------------------------------------------------------------------------------------------

  chart <- function(input, output, session, setup) {  # construct plot takes input from 'setup'
    output$distPlot <- renderPlot({
      
      FF <- setup$tref  # Reference time -------------------------------------
      G <- setup$n      # Total sample size (1:1 randomisation)
      H <- setup$mc     # Mortality control arm at reference time --------------
      I <- setup$r      # % reduction in mortality
      J <- setup$hr     # Hazard Ratio -----------------------------------------
      K <- setup$tmin      # Minimum follow up time
      L <- setup$accrual   # Length of accrual
      M <- setup$noncomp.c # Non compliance control
      N <- setup$noncomp.i # Non compliance intervention
      O <- setup$alpha #
     
     #  lambdaC <- -log(1-H)/FF   # control hazard  .5   6
       #lambdaT <- J*lambdaC   
      #
        # survP( Ihaz= lambdaT, Chaz=lambdaC, CSurvProp=1-H ) 
      
      survplot3 ( CSurvProp=1-H, time1=FF, HR=J) 
            
    } ,  height=400, width=1000)
  }

# --------------------------------------------------------------------------------------------
  chartx <- function(input, output, session, setup) {  # construct plot takes input from 'setup'
    output$distPlotx <- renderPlot({
      
      FF <- setup$tref  # Reference time -------------------------------------
      G <- setup$n      # Total sample size (1:1 randomisation)
      H <- setup$mc     # Mortality control arm at reference time --------------
      I <- setup$r      # % reduction in mortality
      J <- setup$hr     # Hazard Ratio -----------------------------------------
      K <- setup$tmin      # Minimum follow up time
      L <- setup$accrual   # Length of accrual
      M <- setup$noncomp.c # Non compliance control
      N <- setup$noncomp.i # Non compliance intervention
      O <- setup$alpha #
   
      survplot6( CSurvProp=1-H, time1=FF, HR=J )
      
    } ,  height=400, width=1000)
  }
# --------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------

  chartUI2 <- function(id) ({   # this allows multiple plots?
    ns <- NS(id)
    verbatimTextOutput(ns("survplot8"))
  })

# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------

  chart2 <- function(input, output, session, setup){  # re use setup
    
    ns <- session$ns
  
  output$survplot8 <- renderPrint({
     
    FF <- setup$tref  # Reference time -------------------------------------
    G <- setup$n      # Total sample size (1:1 randomisation)
    H <- setup$mc     # Mortality control arm at reference time --------------
    I <- setup$r      # % reduction in mortality
    J <- setup$hr     # Hazard Ratio -----------------------------------------
    K <- setup$tmin      # Minimum follow up time
    L <- setup$accrual   # Length of accrual
    M <- setup$noncomp.c # Non compliance control
    N <- setup$noncomp.i # Non compliance intervention
    O <- setup$alpha #
  
    # get % reduction in mortality given hr and time
    d1 <- morti(d0=H, hr=J, time=FF)  # see function in global.r
    d0 <- H
    
    # this function is from HMisc package
    cpower(tref=FF, n=G, mc=H, r= 100*(d0 - d1)/d0, tmin=K,  #mc is  tref-year mortality, control
           accrual=L, alpha=O, pr=TRUE,
           noncomp.c = M, noncomp.i = N)
    
    # --------------------------------------------------------------------------------------------
   
  } )
  
  }
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
  