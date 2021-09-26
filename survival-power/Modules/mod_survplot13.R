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


mod_survplot13_ui <- function(id){
  
  ns <- NS(id)
 
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tabItem("survplot13",
         
          
          #sidebarLayout(sidebarPanel(
            
         # ),
          
          mainPanel(
            
            #--------------------------------------
             chartUI("second")  ,   #  new
             br(),
             chartUIx("first")      #  new
            #--------------------------------------
            
        #  )
          ),
          #~~~~~~~~~~~~~~~~
          
          tags$head(tags$style(HTML('content-wrapper { overflow: auto; }')))  # not certain of purpose of this
  )
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Module Server ------------------------------------------------------------------
 



