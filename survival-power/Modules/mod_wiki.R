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
     span(textOutput(ns("warning")), style="color:blue"),
     br(),

     uiOutput(ns("text1"))
      )

  }

#' # Module Server ------------------------------------------------------------------
#'
#' @rdname mod_sidebar
#' @export
#' @keywords internal

  
  mod_wiki_server <-  function(input, output, session) {
    ns <- session$ns

      output$warning <- renderText({
        paste("The exponential distribution is the simplest parametric distribution used to describe survival distributions. There is a simpler relationship between survival probability, time and hazard with the exponential distribution. We show this relationship")
      })

      output$text1 <- renderUI({
        HTML(paste0(br(),
                    "blah",
                    br(),
                    br(),
                    "blah" ))

      })


  }


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # homepage <- function() {
  #   renderUI({
  #     tagList(
  #       fluidRow(
  #         box(title = h2("Welcome to ReviewR", style='text-align: center;'),
  #             width = 12,
  #             status = 'primary', 
  #             solidHeader = F
  #         )
  #       ),
  #       fluidRow(
  #         box(width = 12,
  #             status = 'primary', 
  #             solidHeader = F, 
  #             HTML("ReviewR is a portable tool to help you explore data across different data models.  Within ReviewR, you can browse patient data stored in either the OMOP or MIMIC-III data model.<br>"),
  #             br(),
  #             HTML("In addition to viewing patient data, you may also connect to a REDCap project to perform a chart review<br>"),
  #             br(),
  #             HTML("To get started, please complete the 'Setup' step (found in the left navigation menu)<br>")
  #         )
  #       )
  #     )
  #   })
  # }