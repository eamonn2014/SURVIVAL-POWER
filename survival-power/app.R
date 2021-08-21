
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
# This is a top level file
# load packages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
    source("~/Survival-power/survival-power/Global/libraries.R")

# run global file, mostly R code~~~~~~~~~~~~~~~~~~~

    source("~/Survival-power/survival-power/Global/global.R")  # generate data here
 
# set header~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    header <- source("~/Survival-power/survival-power/Global/header.R")$value

# set sidebar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    sidebar <- source("~/Survival-power/survival-power/Global/sidebar.R")$value

# import modules~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ a list here of sources here:

    source("~/Survival-power/survival-power/Modules/mod_boxplot.R")  # do something here


#set body~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    body <- dashboardBody(
        
        tabItems(
            
        mod_boxplot_ui("boxplot_1")
        
        )    
    )

#IU~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ui <- dashboardPage(header, sidebar, body)

# define server logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    server <- function(input, output, session) {
        
        callModule(mod_boxplot_server, "boxplot_1")
        
    }
 
# Run the application~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    shinyApp(ui = ui, server = server)
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~