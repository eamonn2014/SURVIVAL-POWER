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


mod_exp2_ui <- function(id){
  
  ns <- NS(id)
  
  #plot1vars <- callModule(mod_survplot8_server, "plot1_vars")
 # plot2vars <- callModule(mod_survplot8_server, "plot2_vars")
  
  
  #tabItems(
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tabItem("exp2",
          
          
          ##~~~~~~~~~~~~~~
         #  column(width=3,
         #         tagList(
         #          # changed labels from h5("Postulated percentage change in survival probability")
         #           # numericInput(inputId=ns("tref"),  label = c("Reference time"),                         value = 6,  min=0.5,max=200, step=.5),
         #           # numericInput(inputId=ns("n"),     label = c("Total sample size (1:1 randomisation)"),   value = 352, step=1),
         #           # numericInput(inputId=ns("mc"), label = c("Mortality control arm at reference time"),   value = .5, min=0,max=1, step=.05),
         #           # #numericInput(inputId=ns("r"), label = c("% reduction in mortality"), value = .5, min=0,max=1, step=.05),
         #           # numericInput(inputId=ns("hr"), label = c("Hazard Ratio"),                              value = 2/3, min=0.01,max=10, step=.01),
         #           # numericInput(inputId=ns("tmin"), label = c("Minimum follow up time"),                  value = 39/4, min=0,max=100, step=1),
         #           # numericInput(inputId=ns("accrual"), label = c("Length of accrual"),                    value = 74/4, min=0,max=100, step=1),
         #           # numericInput(inputId=ns("noncomp.c"), label = c("Non compliance control"),             value = 0, min=0,max=1, step=.1),
         #           # numericInput(inputId=ns("noncomp.i"), label = c("Non compliance intervention"),        value = 0, min=0,max=1, step=.1),
         #           # numericInput(inputId=ns("alpha"), label = c("alpha"),                                  value = 0.05, min=0.01,max=.5, step=.01),
         #           # br(),
         #           # numericInput(inputId=ns("sims"), label = c("Simulations (not needed in cpower function)"), value = 100, min=10,max=100000, step=1),
         #           # 
         # 
         #           br(),
         #            actionButton(ns("resample"),label=" Hit to run another simulation", icon = icon("th"),  width =300  )
         #             ,
         #           # 
         #           # 
         #           # #  https://stackoverflow.com/questions/47512205/change-color-of-action-button
         #            actionButton(ns("resample"),"Hit to run another simulation",icon=icon("bell"), width =300 ,
         #                         class = "btn action-button",
         #                         style = "color: white;
         #                    background-color: blue")
         # 
         #           
         #           
         #         )#,
         #         #("tab1.2")
         # # )
         #  ),
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
          
        #  mainPanel(
             
         #   plotOutput(ns("survplot12b")), #new
          #  h4(paste("Frank Harrell's Hmisc::cpower")),
           # verbatimTextOutput(ns("survplot12")),
           # h4(paste("My simulation to approximate cpower (although does not accommodate non compliance).")),
            #verbatimTextOutput(ns("survplot12a")),
            
            
    #         h4(paste("The default inputs duplicate Nquery example 'Using an unstratified log-rank test at the one-sided 2.5% significance level, a total of 282 events would
    # allow 92.6% power to demonstrate a 33% risk reduction (hazard ratio for RAD/placebo of about 0.67, as calculated from an anticipated
    # 50% increase in median PFS, from 6 months in placebo arm to 9 months in the RAD001 arm). With a uniform accrual of approximately 23 
    # patients per month over 74 weeks and a minimum follow up of 39 weeks, a total of 352 patients would be required to obtain 282 PFS events, 
    # assuming an exponential progression-free survival distribution with a median of 6 months in the Placebo arm and of 9 months in RAD001 arm. 
    # With an estimated 10% lost to follow up patients, a total sample size of 392 patients should be randomized'. This example is linked to in the references. 
    # As censoring is assumed uninformative we simply calculate the desired sample size as 352/0.9 ~ 392.")),
            
            # 
            # h4(paste("Power is estimated based on total sample size, mortality for control at reference time, HR, 
            # follow up and accrual time. The problem statement mentions one sided 2.5%, this is equivalent to two sided 5%.")),
            # 
            # br(),
            # h4(paste("STATA code for Nquery example@")),
            # h4(paste("stpower exponential 0.11552453 0.0770,  n(352) aperiod(18.5) fperiod(9.75)  loghazard detail")),
            # h4(paste("stpower exponential 0.11552453, hratio(.6666667) n(352) aperiod(18.5) fperiod(9.75)  loghazard detail")),
         # ),
          #~~~~~~~~~~~~~~~~
         
         # tabPanel("Trafficker",
         #          h2("Trafficker tab")
         # ),
         #  
  
         mainPanel(
           
           plotOutput(ns("survplot12b"))
         ),
         
          # tags$head(tags$style(HTML('content-wrapper { overflow: auto; }')))  # is this needed?
  )
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Module Server ------------------------------------------------------------------
# https://stackoverflow.com/questions/61074543/communication-between-shiny-modules  <- idea from here

#' @rdname mod_sidebar
#' @export
#' @keywords internal
#' 
mod_exp2_server <- function(input, output, session, A, B , C  # note A B C, also see app.R
                                   ) {
  
  ns <- session$ns
  
  # 
  # Module_Server <- function(input, output, session,
  #                            xlim) {
    
    output$survplot12b <- renderPlot({
   
      # actually A B C are these see
      # A         = input$mc 
      # B         = input$hr
      # C         = input$tref
      
      lambdaC <- -log(1-A)/C   # control hazard
      lambdaT <- B*lambdaC     # intervention hazard
      
      survP( Ihaz=lambdaT[1], Chaz=lambdaC[1], CSurvProp=.5 )  # Ihaz= runif(1,1,1.5), Chaz=runif(1,2,2.5), CSurvProp=runif(1) ) {
     # survP(  )lambdaC
    }, height=900, width=1450)
    
  }
  
 
# Module_Server <- function(input, output, session,
#                           DataPack, DataSetName, xlim) {
#   
#   output$Plot <- renderPlot({
#     message(paste("Plot", DataSetName))
#     plot(DataPack()[[DataSetName]], # note the parentheses
#          xlim = c(xlim[1], xlim[2])) })
#   
# }

  
  
  
  
  # execute plot variable selection modules
 
    #  output$survplot12b <- renderPlot({
    # 
    #   # this function id in the global.R file
    #   survP( 0.07701635,0.11552453,0.5)
    # 
    # 
    # }, height=900, width=1450)
    # #end new#
    # 
  #} #)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
   
     
  
  
  
  
  
  
  
  
  
  
  
  
#}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   

