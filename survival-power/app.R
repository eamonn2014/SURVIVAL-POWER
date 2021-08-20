# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
# This is a top level file
# load packages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("    libraries.R")

# run global file , mostly R code~~~~~~~~~~~~~~~~~~~

source("    GlobalR.R")
 
# set header~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

header <- source("   header.R")$value

# set sidebar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sidebar <- source("   sidebar.R")$value

# import modules~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("    mod_barplot.R")

#set body~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
body <- dashboardBody(
    
    tabItems(
        
    mod_barplot_ui("barplot_1"),
    
    )    
)

#IU~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- dashboardPage(header, sidebar, body)

# define server logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output, session) {
    callModule(mod_barplot_server, "barplot_1")
}
 
# Run the application~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(ui = ui, server = server)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~