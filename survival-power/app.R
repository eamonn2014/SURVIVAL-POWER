
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
# This is a top level file
# deployment is case sensitive, so not .r but R
# use library explicitly for loading packages

# steps to building app
# 1 copy function survplot?.R to global.R
# 2 update app.R modules/body/server
# 3 create a new mod_????.R
# 4 amend the new mod_????.R  function names, output names
# 5 update sidebar.R 


# git hub authentication, see accepted answer!
# https://stackoverflow.com/questions/66065099/how-to-update-github-authentification-token-on-rstudio-to-match-the-new-policy

# load packages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
 
    source("global/libraries.R")  # /Survival-power/survival-power/
    
# run global file, mostly R code~~~~~~~~~~~~~~~~~~~

    source("global/global.R")  # generate data here
 
# set header~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    header <- source("global/header.R")$value

# set sidebar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    sidebar <- source("global/sidebar.R")$value

# import modules~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ a list here of sources here:

    source("modules/mod_survplot1.R")  # do something here
    source("modules/mod_survplot2.R")  # do something here
    source("modules/mod_survplot3.R")  # do something here
    source("modules/mod_survplot4.R")  # do something here
    source("modules/mod_survplot5.R")  # do something here
    source("modules/mod_survplot6.R")  # do something here
    source("modules/mod_survplot7.R")  # do something here
    source("modules/mod_survplot8.R")  # do something here
    source("modules/mod_wiki.R")       # do something here
    
#set body~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    body <- dashboardBody(
        
        tabItems(
            
          mod_wiki_ui("wiki_1"),  
        mod_survplot1_ui("survplot_1"),
        mod_survplot2_ui("survplot_2"),
        mod_survplot3_ui("survplot_3"),
        mod_survplot4_ui("survplot_4"),
        mod_survplot5_ui("survplot_5"),
        mod_survplot6_ui("survplot_6"),
        mod_survplot7_ui("survplot_7"),
        mod_survplot8_ui("survplot_8")
        )    
    )

#IU~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ui <- dashboardPage(header, sidebar, body)

# define server logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    server <- function(input, output, session) {
        
        callModule(mod_wiki_server, "wiki_1")
        callModule(mod_survplot1_server, "survplot_1")
        callModule(mod_survplot2_server, "survplot_2")
        callModule(mod_survplot3_server, "survplot_3")
        callModule(mod_survplot4_server, "survplot_4")
        callModule(mod_survplot5_server, "survplot_5")
        callModule(mod_survplot6_server, "survplot_6")
        callModule(mod_survplot7_server, "survplot_7")
        callModule(mod_survplot8_server, "survplot_8")
    }
 
# Run the application~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    shinyApp(ui = ui, server = server)
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~