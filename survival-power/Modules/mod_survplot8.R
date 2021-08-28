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

###~~~~~~~~~~~~~~~~~~~~~~~~~~~
tabItem("survplot8",
        
       
        ##~~~~~~~~~~~~~~
        column(width=3,
               tagList(
               # changed labels from h5("Postulated percentage change in survival probability")
               numericInput(inputId=ns("tref"),  label = c("Reference time"),                         value = 6,  min=0.5,max=200, step=.5),
               numericInput(inputId=ns("n"),     label = c("Sample size"),                            value = 352, step=1),
               numericInput(inputId=ns("mc"), label = c("mortality control arm at reference time"),   value = .5, min=0,max=1, step=.05),
               #numericInput(inputId=ns("r"), label = c("% reduction in mortality"), value = .5, min=0,max=1, step=.05),
               numericInput(inputId=ns("hr"), label = c("Hazard Ratio"),                              value = 2/3, min=0.01,max=10, step=.01),
               numericInput(inputId=ns("tmin"), label = c("Minimum follow up time"),                  value = 39/4, min=0,max=100, step=1),
               numericInput(inputId=ns("accrual"), label = c("Length of accrual"),                    value = 74/4, min=0,max=100, step=1),
               numericInput(inputId=ns("noncomp.c"), label = c("Non compliance control"),             value = 0, min=0,max=1, step=.1),
               numericInput(inputId=ns("noncomp.i"), label = c("Non compliance intervention"),        value = 0, min=0,max=1, step=.1),
               )
        ),
        ##~~~~~~~~~~~~~
 
  mainPanel(
   
    verbatimTextOutput(ns("survplot8"))
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
mod_survplot8_server <- function(input, output, session){
  
  ns <- session$ns
  
  output$survplot8 <- renderPrint({

    # get % reduction in mortality given hr and time
    d1 <- morti(d0=input$mc, hr=input$hr, time=input$tref)  # see function in global.r
    d0 <- input$mc
    # this function is from HMisc package
    cpower(tref=input$tref, n=input$n, mc=input$mc, r= 100*(d0 - d1)/d0, tmin=input$tmin,  #mc is  tref-year mortality, control
           accrual=input$accrual, alpha=.05, pr=TRUE,
           noncomp.c = input$noncomp.c, noncomp.i = input$noncomp.i)
  
  
    })
   
}

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   
        
         