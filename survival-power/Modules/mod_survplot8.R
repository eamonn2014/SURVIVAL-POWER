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


mod_survplot8_ui <- function(id){
  
  ns <- NS(id)
  
  #tabItems(
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tabItem("survplot8", 
          
      
          
          ##~~~~~~~~~~~~~~
          column(width=3,
                 tagList(
                   # changed labels from h5("Postulated percentage change in survival probability")
                   numericInput(inputId=ns("tref"),  label = c("Reference time"),                         value = 6,  min=0.5,max=200, step=.5),
                   numericInput(inputId=ns("n"),     label = c("Total sample size (1:1 randomisation)"),   value = 352, step=1),
                   numericInput(inputId=ns("mc"), label = c("Mortality control arm at reference time"),   value = .5, min=0,max=1, step=.05),
                   #numericInput(inputId=ns("r"), label = c("% reduction in mortality"), value = .5, min=0,max=1, step=.05),
                   numericInput(inputId=ns("hr"), label = c("Hazard Ratio"),                              value = 2/3, min=0.01,max=10, step=.01),
                   numericInput(inputId=ns("tmin"), label = c("Minimum follow up time"),                  value = 39/4, min=0,max=100, step=1),
                   numericInput(inputId=ns("accrual"), label = c("Length of accrual"),                    value = 74/4, min=0,max=100, step=1),
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
                   
                   
                   
                 )#,
                 #("tab1.2")
         # )
          ),
          ##---------------
          # 
          # module1 <- function(input, output, session) {
          #   dataone <- reactive({
          #     req(!is.null(input$cars))
          #     return(input$cars)
          #   })
          #   
          #   # return dataone so that it is visible in the app
          #   return(dataone)
          # }
          # 
     
         
          ##~~~~~~~~~~~~~
          
          mainPanel(
             
         #   plotOutput(ns("survplot8b")), #new
            h4(paste("Frank Harrell's Hmisc::cpower")),
            verbatimTextOutput(ns("survplot8")),
            h4(paste("My simulation to approximate cpower (although does not accommodate non compliance).")),
            verbatimTextOutput(ns("survplot8a")),
            
            
            h4(paste("The default inputs duplicate Nquery example 'Using an unstratified log-rank test at the one-sided 2.5% significance level, a total of 282 events would
    allow 92.6% power to demonstrate a 33% risk reduction (hazard ratio for RAD/placebo of about 0.67, as calculated from an anticipated
    50% increase in median PFS, from 6 months in placebo arm to 9 months in the RAD001 arm). With a uniform accrual of approximately 23 
    patients per month over 74 weeks and a minimum follow up of 39 weeks, a total of 352 patients would be required to obtain 282 PFS events, 
    assuming an exponential progression-free survival distribution with a median of 6 months in the Placebo arm and of 9 months in RAD001 arm. 
    With an estimated 10% lost to follow up patients, a total sample size of 392 patients should be randomized'. This example is linked to in the references. 
    As censoring is assumed uninformative we simply calculate the desired sample size as 352/0.9 ~ 392.")),
            
            
            h4(paste("Power is estimated based on total sample size, mortality for control at reference time, HR, 
            follow up and accrual time. The problem statement mentions one sided 2.5%, this is equivalent to two sided 5%.")),
            
            br(),
            h4(paste("STATA code for Nquery example@")),
            h4(paste("stpower exponential 0.11552453 0.0770,  n(352) aperiod(18.5) fperiod(9.75)  loghazard detail")),
            h4(paste("stpower exponential 0.11552453, hratio(.6666667) n(352) aperiod(18.5) fperiod(9.75)  loghazard detail")),
          ),
          #~~~~~~~~~~~~~~~~
          
         tags$head(tags$style(HTML('content-wrapper { overflow: auto; }')))  # is this needed?
         
     
  ) 
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Module Server ------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @keywords internal
#' 
mod_survplot8_server <- function(input, output, session){
  
  ns <- session$ns
  
  # dataone <- reactive({
  #   # req(!is.null(input$cars))
  #   return(
  #     list(
  #   hr=  input$hr,
  #   tref=  input$tref,
  #   n==  input$n,
  #   mc=   input$mc,
  #   tmin=  input$tmin,
  #   accrual=  input$accrual ,
  #   alpha=  input$alpha,
  #   noncomp.c=  input$noncomp.c,
  #   noncomp.i=  input$noncomp.i
  #     )
  #   )
  # })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Hmisc::cpower
  output$survplot8 <- renderPrint({
    
    # get % reduction in mortality given hr and time
    d1 <- morti(d0=input$mc, hr=input$hr, time=input$tref)  # see function in global.r
    d0 <- input$mc
    # this function is from HMisc package
    cpower(tref=input$tref, n=input$n, mc=input$mc, r= 100*(d0 - d1)/d0, tmin=input$tmin,  #mc is  tref-year mortality, control
           accrual=input$accrual, alpha=input$alpha, pr=TRUE,
           noncomp.c = input$noncomp.c, noncomp.i = input$noncomp.i)
    
    # starting to apply this
    # https://stackoverflow.com/questions/46167693/how-to-call-a-shiny-module-in-another-shiny-module-reactive-function-from-one-m
    # dataone <- reactive({
    #   # req(!is.null(input$cars))
    #   return(
    #     
    #     input$hr,
    #     input$tref, 
    #     input$n, 
    #     input$mc, 
    #     input$tmin,  
    #     input$accrual ,
    #     input$alpha,
    #     input$noncomp.c,
    #     input$noncomp.i
    #     
    #   )
    #   return(dataone)# end new
    #   
    # })
    
    # return dataone so that it is visible in the app
    
    
    
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
                   plotSettings$D <- -log(1-input$mc )/input$tref
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
                                                 alpha=plotSettings$FF ))    # check lambdaC??
      simres <- as.data.frame(simres)
      names(simres) <- c("hazard rate ctrl","hazard rate trt","No of events",
                         "Proportion of events","Mean HR", "HR Upper 90%CI","Mean P-value",
                         "SD log HR","Power")
      
      r <- apply(simres,2, mean)
      #print(r, digits=6)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # my simulation
      output$survplot8a <- renderPrint({
        
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
    
    ##new
    
    # output$survplot8b <- renderPlot({
    #   
    #   # this function id in the global.R file
    #   survP( 0.07701635,0.11552453,0.5)
    #   
    #   
    # }, height=700, width=1000)
    #end new
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
   
     
  
  
  
  
  
  
  
  
  
  
  
  
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   

