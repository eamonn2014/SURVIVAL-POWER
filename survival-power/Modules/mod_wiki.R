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
     h4(paste("In this app we show how to use the exponential distribution to help understand survival and 
              hypothesised changes due to an intervention. Much like the survival converter linked to below we show how to convert
              hazard rates to any survival probability at time t, or alternatively, convert survival probability at time t to a hazard rate. 
              Though here we also plot the survival distibutions.")),       
            
     # div(h4("References:")),  
     # tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/15724232/", tags$span(style="color:blue", h4("[1] Generating survival times to simulate Cox proportional hazards models")),),   
     # div(p(" ")),
     # tags$a(href = "https://github.com/eamonn2014/Survival-power/tree/master/survival-power", tags$span(style="color:blue", h4("[2] Shiny code")),),   
     # div(p(" ")),
     # tags$a(href = "https://stattools.crab.org/R/Survival_Converter.html", tags$span(style="color:blue", h4("[3] Survival converter")),),   
     # div(p(" ")),
     # tags$a(href = "https://stats.stackexchange.com/questions/105881/how-to-simulate-survival-times-using-true-base-line-hazard-function", tags$span(style="color:blue", h4("[4] How to simulate survival times using true base line hazard function")),),   
     # div(p(" ")),
     # tags$a(href = "https://www.youtube.com/watch?v=inMjG32nzcw&ab_channel=Statsols%28ProviderofnQuery%29", tags$span(style="color:blue", h4("[5] Sample Size For Survival Analysis - A guide to planning successful clinical trials")),),   
     # div(p(" ")),
     # tags$a(href = "https://sas-and-r.blogspot.com/2010/03/example-730-simulate-censored-survival.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+SASandR+%28SAS+and+R%29", tags$span(style="color:blue", h4("[6] Simulate censored survival data")),),   
     # div(p(" ")),
     # 
     
     
     
     
     tags$hr(),
     
     
     
     
     # or create objects in the server and output here:        
     span(textOutput(ns("warning")), style="color:blue"),
     br(),

     uiOutput(ns("text1")),
     
    # uiOutput(ns("text2")),
     
     uiOutput(ns("text3")),
     br(),
     
     div(h4("References:")),  
     tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/15724232/", tags$span(style="color:blue", h5("[1] Generating survival times to simulate Cox proportional hazards models - key paper for simulation")),),   
     div(p(" ")),
     tags$a(href = "https://github.com/eamonn2014/Survival-power/tree/master/survival-power", tags$span(style="color:blue", h5("[2] Here's the Shiny code")),),   
     div(p(" ")),
     tags$a(href = "https://stattools.crab.org/R/Survival_Converter.html", tags$span(style="color:blue", h5("[3] Survival converter")),),   
     div(p(" ")),
     tags$a(href = "https://stats.stackexchange.com/questions/105881/how-to-simulate-survival-times-using-true-base-line-hazard-function", tags$span(style="color:blue", h5("[4] How to simulate survival times using true base line hazard function")),),   
     div(p(" ")),
     tags$a(href = "https://www.youtube.com/watch?v=inMjG32nzcw&ab_channel=Statsols%28ProviderofnQuery%29", tags$span(style="color:blue", h5("[5] Sample Size For Survival Analysis - A guide to planning successful clinical trials - we duplicate this example")),),   
     div(p(" ")),
     tags$a(href = "https://sas-and-r.blogspot.com/2010/03/example-730-simulate-censored-survival.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+SASandR+%28SAS+and+R%29", tags$span(style="color:blue", h5("[6] Simulate censored survival data - useful starting point")),),   
     div(p(" ")),
     
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
              There is a simple relationship between survival probability, time and hazard with the exponential distribution. Knowing 2 of the 3 parameter completely 
              determines the remaining parameter.")
      })

      #@@@@@@@@@@@@@@@@ 2
      output$text1 <- renderUI({
        HTML(paste0(br(),
                    "where lambda represents hazard rate and S(t) represents survival proportion at time t.",
                    br(),
                    br(),
                    "blah" ))
      })
        
      #@@@@@@@@@@@@@@@@ 3
      output$text2 <- renderUI({ 
 
        withMathJax(
          helpText(
            tags$span(style="color:black",
                      ' $$ {  {\\it{s}^2} =  \\frac{     {(\\sigma^2)}   {(\\chi^2}_{(n - 1), (1-\\alpha)}) }  {(n-1)}          } \\qquad  \\qquad \\qquad  \\qquad \\left[ 1 \\right]    \\!$$')))
       })  
      #@@@@@@@@@@@@@@@@ 
      output$text3 <- renderUI({ 
        
        withMathJax(
          helpText(
            tags$span(style="color:black",
                      ' $$ {  {\\lambda} =  \\frac{        {-\\ln} {(S(t) }) }  {t}          } \\qquad  \\qquad \\qquad  \\qquad \\left[ 1 \\right]    \\!$$')))
      })  
 
      
      #[ \lambda = \frac{-ln(S(t))}{t} \]
      
      # output$text3 <- renderUI({ 
      # 
      # h4("Reference:") 
      # tags$a(href = "https://en.wikipedia.org/wiki/Chi-squared_distribution#Gamma,_exponential,_and_related_distributions", tags$span(style="color:blue", "[3] Chi-squared distribution"),)  
      # p(" ")
      # 
      # tags$hr()
      
      #})  
      #@@@@@@@@@@@@@@@@
      
      
      
  }


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   