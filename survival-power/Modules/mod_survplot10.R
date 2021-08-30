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
 
mod_survplot10_ui <- function(id){
  
  ns <- NS(id)
  
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tabItem("survplot10",
          
          
          ##~~~~~~~~~~~~~~
          column(width=3,
                 tagList(
                   # changed labels from h5("Postulated percentage change in survival probability")
                   numericInput(inputId=ns("tref"),  label = c("Reference time"),                         value = 5,  min=0.5,max=200, step=.5),
                   numericInput(inputId=ns("n"),     label = c("Total sample size (1:1 randomisation)"),  value = 400, step=1),
                   numericInput(inputId=ns("mc"), label = c("Mortality control arm at reference time"),   value = .6, min=0,max=1, step=.01),
                   #numericInput(inputId=ns("r"), label = c("% reduction in mortality"), value = .5, min=0,max=1, step=.05),
                   numericInput(inputId=ns("d1"), label = c("Mortality intervention arm at reference time"),  value = .41, min=0,max=1, step=.01),
                   numericInput(inputId=ns("tmin"), label = c("Minimum follow up time"),                  value = 2, min=0,max=100, step=1),
                   numericInput(inputId=ns("accrual"), label = c("Length of accrual"),                    value = 1.5, min=0,max=100, step=1),
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
            verbatimTextOutput(ns("survplot10")),
            h4(paste("My simulation to approximate cpower (although does not accommodate non compliance).")),
            verbatimTextOutput(ns("survplot10a")),
            
            tags$a(href = "https://books.google.ie/books?id=Okf7CAAAQBAJ&printsec=frontcover&dq=David+Collett+Modelling+survival+data+in+medical+research-CRC+Press+(2015)&hl=en&sa=X&redir_esc=y#v=onepage&q=sample%20size&f=false",
                   tags$span(style="color:blue", "Modelling Survival Data in Medical Research David Collett (2015)"),),   
            div(p(" ")),
            
            h4(paste("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'")),
            
            br(),
            h4(paste("STATA code for example@")),
            h4(paste("stpower exponential xxxxxxxxxxxxx  loghazard detail")),
            h4(paste("stpower exponential xxxxxxxxxxxxx  loghazard detail")),
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
mod_survplot10_server <- function(input, output, session){
  
  ns <- session$ns
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Hmisc::cpower
  output$survplot10 <- renderPrint({
    
    # get % reduction in mortality given hr and time
   # d1 <- log(input$mc)/log(input$d1) # see function in global.r
    d1 <- input$d1
    d0 <- input$mc
    # this function is from HMisc package
    cpower(tref=input$tref, n=input$n, mc=input$mc, r= 100*(d0 - d1)/d0, tmin=input$tmin,  #mc is  tref-year mortality, control
           accrual=input$accrual, alpha=input$alpha, pr=TRUE,
           noncomp.c = input$noncomp.c, noncomp.i = input$noncomp.i)
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # do the same approach as module 7 add a button to re simulate if input or button is pressed
  
  plotSettings <- reactiveValues()
  
  observeEvent(c(input$d1, 
                 input$n, input$accrual, 
                 input$tmin, input$mc,
                 input$tref, input$alpha  ), {
                   
                   plotSettings$A <- log(1-input$d1)/log(1-input$mc) #hr
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
      output$survplot10a <- renderPrint({
        
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

