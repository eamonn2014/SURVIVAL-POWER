# Module UI ------------------------------------------------------

#' @title xxxxxxxxxxxxxxxxxxx
#' @decription a shiny module
#' 
#' 
#' 
#' 


mod_survival_ui <- function(id){
  ns <- NS(id)
}


tabItem("Survival",
        column(width=3,
               selectInput(
               inputId=ns("PSurv"),
               label="treatment xxx:",
               choices=c("A","B"),
               selected="A"),
               
               
               ) )