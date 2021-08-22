
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
# This is a top level file

# steps to building app
# 1 copy survplotx R code function to global.R
# 2 update app.R modules/body/server
# 3 create a new mod_????.R
# 4 amend the new mod_????.R  function names, output names
# 5 update sidebar.R 


# load packages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
    source("global/libraries.R")  # /Survival-power/survival-power/
    
# run global file, mostly R code~~~~~~~~~~~~~~~~~~~

    source("global/global.R")  # generate data here
 
# set header~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    header <- source("global/header.R")$value

# set sidebar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    sidebar <- source("global/sidebar.R")$value

# import modules~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ a list here of sources here:

    source("modules/mod_boxplot.R")    # do something here
    source("modules/mod_survplot1.R")  # do something here
    source("modules/mod_survplot2.R")  # do something here
    source("modules/mod_survplot3.R")  # do something here
   # source("~/Survival-power/survival-power/Modules/mod_wiki.R")  # do something here
#set body~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    body <- dashboardBody(
        
        tabItems(
            
        mod_boxplot_ui("boxplot_1"),     # commas required here
        mod_survplot1_ui("survplot_1"),
        mod_survplot2_ui("survplot_2"),
        mod_survplot3_ui("survplot_3")#,
      #  mod_wiki_ui("wiki")
        
        )    
    )

#IU~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ui <- dashboardPage(header, sidebar, body)

# define server logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    server <- function(input, output, session) {
        
        callModule(mod_boxplot_server, "boxplot_1")   # no commas required here
        callModule(mod_survplot1_server, "survplot_1")
        callModule(mod_survplot2_server, "survplot_2")
        callModule(mod_survplot3_server, "survplot_3")
        #callModule(mod_wiki_server, "wiki")
    }
 
# Run the application~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    shinyApp(ui = ui, server = server)
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~