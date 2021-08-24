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
     h4(paste("Plugging in alpha, the population variance and the number of replicates (n) into equation [1] we calculate the specification.")),       
            
     # or create objects in the server and output here:        
     span(textOutput(ns("warning")), style="color:blue"),
     br(),

     uiOutput(ns("text1")),
     
     uiOutput(ns("text2")),
     
     br(),
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
       (paste("The exponential distribution is the simplest parametric distribution for describing survival distributions. There is a simpler relationship between survival probability, time and hazard with the exponential distribution. We show this relationship"))
      })

      #@@@@@@@@@@@@@@@@
      output$text1 <- renderUI({
        HTML(paste0(br(),
                    "blah",
                    br(),
                    br(),
                    "blah" ))
      })
        
      #@@@@@@@@@@@@@@@@ 2
      output$text2 <- renderUI({ 
 
        withMathJax(
          helpText(
            tags$span(style="color:black",
                      ' $$ {  {\\it{s}^2} =  \\frac{     {(\\sigma^2)}   {(\\chi^2}_{(n - 1), (1-\\alpha)}) }  {(n-1)}          } \\qquad  \\qquad \\qquad  \\qquad \\left[ 1 \\right]    \\!$$')))
       })  
      #@@@@@@@@@@@@@@@@ 3


  }


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   