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

# cpower(tref=6, n=352, mc=d0, r= 100*(d0 - d1)/d0, tmin=39/4,  #mc is  tref-year mortality, control
#        accrual=74/4, alpha=.05, pr=TRUE,
#        noncomp.c = 0, noncomp.i = 0)


mod_survplot11_ui <- function(id){
  
  ns <- NS(id)
  
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tabItem("survplot11",
          
          
          ##~~~~~~~~~~~~~~
          column(width=3,
                 tagList(
                   # changed labels from h5("Postulated percentage change in survival probability")
                   numericInput(inputId=ns("tref"),  label = c("Hazard"),                         value = 0.03,  min=0.01 ,max=200, step=.01),
                   numericInput(inputId=ns("n"),     label = c("Total sample size (1:1 randomisation)"),   value = 380, step=1),
                   numericInput(inputId=ns("mc"), label = c("Mortality control arm at reference time"),   value = .5, min=0,max=1, step=.05),
                   #numericInput(inputId=ns("r"), label = c("% reduction in mortality"), value = .5, min=0,max=1, step=.05),
                   numericInput(inputId=ns("hr"), label = c("Hazard Ratio"),                              value = .6, min=0.01,max=10, step=.01),
                   numericInput(inputId=ns("tmin"), label = c("Minimum follow up time"),                  value = 15, min=0,max=100, step=1),
                   numericInput(inputId=ns("accrual"), label = c("Length of accrual"),                    value = 20, min=0,max=100, step=1),
                   numericInput(inputId=ns("noncomp.c"), label = c("Non compliance control"),             value = 0, min=0,max=1, step=.1),
                   numericInput(inputId=ns("noncomp.i"), label = c("Non compliance intervention"),        value = 0, min=0,max=1, step=.1),
                   numericInput(inputId=ns("alpha"), label = c("alpha"),                                  value = 0.05, min=0.01,max=.5, step=.01),
                   br(),
                   numericInput(inputId=ns("sims"), label = c("Simulations (not needed in cpower function)"), value = 100, min=10,max=100000, step=1),
                   
                   
                   br(),
                   #   actionButton(ns("resample"),label=" Hit to run another simulation", icon = icon("th"),  width =300  )
                   #  ,
                   
                   
                   #  https://stackoverflow.com/questions/47512205/change-color-of-action-button
                   actionButton(ns("resample"),"Hit to run another simulation",icon=icon("bell"), width =300 ,
                                class = "btn action-button",
                                style = "color: white;
                           background-color: blue")
                   
                   
                   
                 )
          ),
          ##~~~~~~~~~~~~~
          
          mainPanel(
            
            
            h4(paste("Frank Harrell's Hmisc::cpower")),
            verbatimTextOutput(ns("survplot11")),
            h4(paste("My simulation to approximate cpower (although does not accommodate non compliance).")),
            verbatimTextOutput(ns("survplot11a")),
            
            
            h4(paste("Reproduce the example on the bottom of page 353 of An Introduction to Survival Analysis
Using Stata Third Edition")),
            
            h4(paste("Power is estimated based on total sample size, control hazard,
 probability of mortality for control (from which refernce time can be determined) hr, follow up and accrual time.")),
            
            br(),
            h4(paste("STATA code")),
            h4(paste("stpower exponential 0.03, hratio(0.6) power(0.9) aperiod(20) fperiod(15)")),
             
          ),
          #~~~~~~~~~~~~~~~~
          
          # tags$head(tags$style(HTML('content-wrapper { overflow: auto; }')))  # is this needed?
  )
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Module Server ------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @keywords internal
#' 
mod_survplot11_server <- function(input, output, session){
  
  ns <- session$ns
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Hmisc::cpower
  output$survplot11 <- renderPrint({
    
    h <- input$tref
    time <- 1/h*(-log(input$mc))^(1)
    
    # get % reduction in mortality given hr and time
    d1 <- morti(d0=input$mc, hr=input$hr, time=time)  # see function in global.r
    d0 <- input$mc
    # this function is from HMisc package
    cpower(tref=time, n=input$n, mc=input$mc, r= 100*(d0 - d1)/d0, tmin=input$tmin,  #mc is  tref-year mortality, control
           accrual=input$accrual, alpha=input$alpha, pr=TRUE,
           noncomp.c = input$noncomp.c, noncomp.i = input$noncomp.i)
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # do the same approach as module 7 add a button to re simulate if input or button is pressed
  
  plotSettings <- reactiveValues()
  
  observeEvent(c(input$hr, 
                 input$n, input$accrual, 
                 input$tmin, input$mc,
                 input$tref, input$alpha  ), {
                   
                   plotSettings$A <- input$hr
                   plotSettings$B <- input$n/2
                   plotSettings$C <- input$accrual
                   plotSettings$D <-  input$tref ############# hazard
                   plotSettings$E <- input$tmin
                   plotSettings$FF <- input$alpha
                   
                 }, ignoreNULL = FALSE)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observe({   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(input$resample > -1) {   # if 0 plot only appears after first button hit! 
      
      simres<- NULL
      # p=1 is to stipulate shape =1 so exponential
      
      simres <- plyr::raply(input$sims ,simfunfx(p=1, 
                                                 hr= plotSettings$A,
                                                 n=  plotSettings$B, 
                                                 acc=plotSettings$C,
                                                 fup=plotSettings$E,
                                                 lambdaC= plotSettings$D ,
                                                 alpha=plotSettings$FF ))     
      simres <- as.data.frame(simres)
      names(simres) <- c("hazard rate ctrl","hazard rate trt","No of events",
                         "Proportion of events","Mean HR", "HR Upper 90%CI","Mean P-value",
                         "SD log HR","Power")
      
      r <- apply(simres,2, mean)
      #print(r, digits=6)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # my simulation
      output$survplot11a <- renderPrint({
        
        simres<- NULL
        
        simres <- plyr::raply(input$sims ,simfunfx(p=1, 
                                                   hr= plotSettings$A,
                                                   n=  plotSettings$B, 
                                                   acc=plotSettings$C,
                                                   fup=plotSettings$E,
                                                   lambdaC= plotSettings$D , 
                                                   alpha=plotSettings$FF  ))    # check lambdaC??
        simres <- as.data.frame(simres)
        names(simres) <- c("hazard rate ctrl","hazard rate trt","No of events",
                           "Proportion of events","Mean HR", "HR Upper 90%CI","Mean P-value",
                           "SD log HR","Power")
        
        r <- apply(simres,2, mean)
        print(r, digits=6)
        
      })
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   

