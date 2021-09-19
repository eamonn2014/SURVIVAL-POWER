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


mod_testing2_ui <- function(id){
  
  ns <- NS(id)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # the values are created here
  # setupInput2<-function(id){
  #   ns<-NS(id)
  #   tagList(
  #     sliderInput(ns("bins"), "Number of bins:",
  #                 min = 1,  max = 50, value = 30),
  #     # numericInput(inputId=ns("nsim_input"),                    label = c("No. of simulated studies"),                         value = 30, min=1,  max=1000, step=5),
  #     # numericInput(inputId=ns("npergrp_input"),                 label = c("No. of patients per group"),                        value = 100, min=10,  max=1000, step=1),
  #     # numericInput(inputId=ns("SurvProp_input"),                label = c("Survival probability"),                            value = 0.5, min=0.01,max=.95,  step=.01),
  #     # numericInput(inputId=ns("tSurvNULL_input"),               label = c("Control survival time (green)"),                           value = 4,   min=1,   max=100,  step=1),
  #     # numericInput(inputId=ns("tSurvALT_input"),                label = c("Intervention survival time (blue)"),                      value = 7,   min=1,   max=100,  step=1),      
  #     # 
  #     
  #   )                                                         
  # }
  
 
  # end new
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tabItem("testing2",
          ##~~~~~~~~~~~~~~
          column(width=3,
                 tagList(
                   h4(paste("xxxxxxxxxxxxxxxxxxxxxxxxxxx")),
                   br(),
                   # numericInput(inputId=ns("nsim_input"),                    label = c("No. of simulated studies"),                         value = 30, min=1,  max=1000, step=5),
                   # numericInput(inputId=ns("npergrp_input"),                 label = c("No. of patients per group"),                        value = 100, min=10,  max=1000, step=1),
                   # numericInput(inputId=ns("SurvProp_input"),                label = c("Survival probability"),                            value = 0.5, min=0.01,max=.95,  step=.01),
                   # numericInput(inputId=ns("tSurvNULL_input"),               label = c("Control survival time (green)"),                           value = 4,   min=1,   max=100,  step=1),
                   # numericInput(inputId=ns("tSurvALT_input"),                label = c("Intervention survival time (blue)"),                      value = 7,   min=1,   max=100,  step=1),
                   br(),
                   h4(paste("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.")),
                   br(),
                   
                   #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                   #setupInput("basic"),
                   #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                   
                   # actionButton(ns("resample"),"Hit to run another simulation",icon=icon("bell"), width =300 ,
                   #              class = "btn action-button",
                   #              style = "color: white;
                   #         background-color: blue")
                 )
                 
                 
                 
          ),
          ##~~~~~~~~~~~~~
        #
        
        sidebarLayout(sidebarPanel(
          
        ),
        
          mainPanel(
           # plotOutput(ns("survplot7")),
           
            
            #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
            #chartUI("first"),     #  new
            chartUI("second")     #  new
            #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
            
          )),
          #~~~~~~~~~~~~~~~~
          
          tags$head(tags$style(HTML('content-wrapper { overflow: auto; }')))
  )
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Module Server ------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @keywords internal


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@NEW
 
#https://stackoverflow.com/questions/52898292/r-shiny-refreshing-plot-when-entering-input-or-pressing-an-action-button
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@END NEW


mod_testing2_server <- function(input, output, session){
  ns <- session$ns
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
}



