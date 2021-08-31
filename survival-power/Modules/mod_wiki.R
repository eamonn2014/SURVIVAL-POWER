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

  mod_wiki_ui <- function(id) {
    ns <- NS(id)

    tabItem("wiki",
            
     # we can type directly here!   
     h4(paste("This app helps understand and plan time to event studies assuming exponentially distributed event times.")),  
     #span(textOutput(ns("warning")), style="color:black;font-size:18px"),   # changes both font and colour
    
     h4(paste("The exponential distribution is the simplest parametric distribution for describing survival distributions. 
              There is a simple relationship between survival probability, time and hazard with the exponential distribution. Knowing 2 of the 3 parameters completely 
              determines the remaining parameter. See the equation below, lambda represents the hazard rate and S(t) represents the survival proportion at time t.")),
     br(),
     uiOutput(ns("text3")),   # equation
     br(),
     h4(paste("Using the exponential distribution we present survival curves and 
              hypothesised changes due to an intervention. Much like the survival converter linked to in the references we show how to convert
              hazard rates to any survival probability at time t, or alternatively, convert survival probability at time t to a hazard rate. 
              Here though we also plot the survival distibutions.")),     
     h4(paste("We look at the effect of an intervention from two perspectives: A) Changing survival probability, and B) Changing survival time.")),      
     h4(paste("Clicking on the menu option starting 'A' there are 3 options each holding the desired survival time constant. 
              The first option allows the user to specify a relative change in terms of a percentage change in survival/mortality.        
     The second option allows the user to specify an absolute change in terms of a percentage point change in survival/mortality. 
        The third option allows the user to specify a hazard ratio and seeing the effect on survival probability.       
              
              ")),
     h4(paste("Clicking on the menu option starting 'B' there are 3 options each holding the desired survival probability constant. 
              The first option allows the user to specify a relative change in terms of a percentage change in survival/mortality time.        
     The second option allows the user to specify an absolute change in terms of a percentage point change in survival/mortality time. 
        The third option allows the user to specify a hazard ratio in survival/mortlaity and seeing the effect on the survival/mortality time.
              
              ")),
     
     h4(paste("The menu option starting 'C' shows what a large number of studies look like. You can get an idea of whether a real difference
     in survival (or whatever outcome measure is of interest) between the two groups is detectable. ")),
     
     h4(paste("The next menu D reproduces a time to event power calculation by Nquery assuming exponential distribution event times
              using the Hmisc::cpower function and also a simulation to approximate cpower.We use the inputs: ")),
     
    # tags$hr(),
     
    # h1(strong("Title"), style = "color:blue;font-size:50px"),  # bold 
  
  
     
   # textOutput("text4"),
   # p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", 
   #   style = "font-family: 'times'; font-si16pt; font-size:50px; colour:blue"),
   #   
   

    # uiOutput(ns("text1")),
     
    
     
     #uiOutput(ns("text4")),
     br(),
     
     # div(h4("References:")),  
     # tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/15724232/", tags$span(style="color:blue", h5("[1] Generating survival times to simulate Cox proportional hazards models - key paper for simulation")),),   
     # div(p(" ")),
     # tags$a(href = "https://github.com/eamonn2014/Survival-power/tree/master/survival-power", tags$span(style="color:blue", h5("[2] Here's the Shiny code")),),   
     # div(p(" ")),
     # tags$a(href = "https://stattools.crab.org/R/Survival_Converter.html", tags$span(style="color:blue", h5("[3] Survival converter")),),   
     # div(p(" ")),
     # tags$a(href = "https://stats.stackexchange.com/questions/105881/how-to-simulate-survival-times-using-true-base-line-hazard-function", tags$span(style="color:blue", h5("[4] How to simulate survival times using true base line hazard function")),),   
     # div(p(" ")),
     # tags$a(href = "https://www.youtube.com/watch?v=inMjG32nzcw&ab_channel=Statsols%28ProviderofnQuery%29", tags$span(style="color:blue", h5("[5] Sample Size For Survival Analysis - A guide to planning successful clinical trials - we duplicate this example")),),   
     # div(p(" ")),
     # tags$a(href = "https://sas-and-r.blogspot.com/2010/03/example-730-simulate-censored-survival.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+SASandR+%28SAS+and+R%29", tags$span(style="color:blue", h5("[6] Simulate censored survival data - useful starting point")),),   
     # div(p(" ")),
     
      )
    
    

  }

#' # Module Server ------------------------------------------------------------------
#'
#' @rdname mod_sidebar
#' @export
#' @keywords internal

  
  mod_wiki_server <-  function(input, output, session) {
    ns <- session$ns

      #@@@@@@@@@@@@@@@@ 1
      output$warning <- renderText({
       paste("The exponential distribution is the simplest parametric distribution for describing survival distributions. 
              There is a simple relationship between survival probability, time and hazard with the exponential distribution. Knowing 2 of the 3 parameters completely 
              determines the remaining parameter. See equation 1 below, lambda represents the hazard rate and S(t) represents the survival proportion at time t.")
      })

      #@@@@@@@@@@@@@@@@ 2
      output$text1 <- renderUI({
        HTML(paste0(br(),
                    "where lambda represents hazard rate and S(t) represents survival proportion at time t.",
                    br(),
                    br(),
                    "blah" ))
      })
        
      
      output$text4 <- renderText({  ("hello input is" ) })
      
      #@@@@@@@@@@@@@@@@ 3
      # output$text2 <- renderUI({ 
      # 
      #   withMathJax(
      #     helpText(
      #       tags$span(style="color:black", 
      #                 ' $$ {  {\\it{s}^2} =  \\frac{     {(\\sigma^2)}   {(\\chi^2}_{(n - 1), (1-\\alpha)}) }  {(n-1)}          } \\qquad  \\qquad \\qquad  \\qquad \\left[ 1 \\right]    \\!$$')))
      #  })  
      #@@@@@@@@@@@@@@@@ 
      output$text2 <- renderUI({ 
        
        withMathJax(
          helpText(
            tags$span(style="color:black", 
                      ' $$ {  {\\it{s}^2} =  \\frac{     {(\\sigma^2)}   {(\\chi^2}_{(n - 1), (1-\\alpha)}) }  {(n-1)}          }    \\!$$')))
      })  
      #@@@@@@@@@@@@@@@@ 
      
      
      
      
      
      
      # output$text3 <- renderUI({ 
      #   
      #   withMathJax(
      #     helpText(
      #       tags$span(style="color:black", h3(
      #                 ' $$ {  {\\lambda} =  \\frac{        {-\\ln} {(S(t) }) }  {t}          } \\qquad  \\qquad \\qquad  \\qquad \\left[ 1 \\right]    \\!$$'))) )
      # })  
 
      
    
      #@@@@@@@@@@@@@@@@
      
      
      output$text3 <- renderUI({ 
        
        withMathJax(
          helpText(
            tags$span(style="color:black", h3(
              ' $$ {  {\\lambda} =  \\frac{        {-\\ln} {(S(t) }) }  {t}          }     \\!$$'))) )
      })  
      
      
      
  }


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   