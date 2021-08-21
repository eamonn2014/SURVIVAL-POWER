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


mod_boxplot_ui <- function(id){
  ns <- NS(id)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~
tabItem("Boxplot",
        ##~~~~~~~~~~~~~~
        column(width=3,
               selectInput(
               inputId=ns("PBox"),
               label="treatment xxx:",
               choices=c("A","B"),
               selected="A"),
              
         
         #~~~~~~~~~~~~~~~~~~~
         conditionalPanel(
        
               condition="inputPBox == 'A'",
              
               ns=ns,
              
          column(width=12),
               radioButtons(
                 inputId = ns("Example"),
                 label="Dateset:",
                 choices=c("X","Y"),
                 selected="X")),
              
        #~~~~~~~~~~~~~~~~~~~
         conditionalPanel(
                
                condition="inputPBox == 'B'",
                
                ns=ns,
                
                column(width=12),
                radioButtons(
                  inputId = ns("Example"),
                  label="Dateset:",
                  choices=c("X","Y"),
                  selected="X")),
         #~~~~~~~~~~~~~~~~~~~
        ),
        ##~~~~~~~~~~~~~


  #~~~~~~~~~~~~~~~~~      
  mainPanel(
    
    conditionalPanel(condition="input.PBox == 'A'",
                     ns=ns, column(width=12,
                                   plotlyOutput(ns("Plot1")))),
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
mod_boxplot_server <- function(input, output, session){
  
  ns <- session$ns
  
  output$Plot1 <- renderPlotly({

      pp <- ggplot(xx, aes(x = percentP , y = density, fill = status)) +
        geom_boxplot() +
        facet_wrap(~percentP, scales = "free_x") 
      
     ggplotly(pp)
  
  
    })

}

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   
        
         