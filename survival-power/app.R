5
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
    source("modules/mod_survplot7.R")  # simulate many studies
    source("modules/mod_survplot8.R")  # nquery power
    source("modules/mod_survplot9.R")  # pub paper power
    source("modules/mod_survplot10.R")  # pub paper power  <- david collett drop this
    source("modules/mod_survplot11.R")  # 
#    source("modules/mod_survplot12.R")  # test
    source("modules/mod_wiki.R")       # introduction and landing page
 #    source("modules/mod_testing.R")  # test
 #   source("modules/mod_testing2.R")  # test
 # source("modules/mod_exp1.R")  # test
 # source("modules/mod_exp2.R")  # test
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
        mod_survplot8_ui("survplot_8"),
        mod_survplot9_ui("survplot_9"),
        mod_survplot10_ui("survplot_10"),
        mod_survplot11_ui("survplot_11")
      #  mod_survplot12_ui("survplot_12")
     #    mod_testing_ui("testing"),
     #  mod_testing2_ui("testing2"),
     # mod_exp1_ui("exp1"),
     #  mod_exp2_ui("exp2")
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
        callModule(mod_survplot9_server, "survplot_9")
        callModule(mod_survplot10_server, "survplot_10")
        callModule(mod_survplot11_server, "survplot_11")
    #   callModule(mod_survplot12_server, "survplot_12")
        
        # callModule(mod_survplot12_server, "survplot_12",  # inputs A B C not working
        #            A         = input$mc ,
        #            B         = input$hr,
        #            C         = input$tref)

       
        # callModule(mod_testing_server, "testing")
        # 
        # bins <- callModule(setup,"basic")  # new
        # callModule(chart, "first", bins)   # new
        # callModule(chart, "second", bins)  # new
        # 
        # 
        # 
        # callModule(mod_exp1_server, "exp1")
        # callModule(mod_exp2_server, "exp2",  # inputs A B C not working
        #            A         = input$mc ,
        #            B         = input$hr,
        #            C         = input$tref)
     }
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # source("modules/mod_exp11.R")  # test
    # plot1vars <- callModule(varselect_mod_server, "plot1_vars")
    # plot2vars <- callModule(varselect_mod_server, "plot2_vars")
    # 
    # # execute scatterplot module
    # res <- callModule(scatterplot_mod_server,
    #                   "plots",
    #                   dataset = ames,
    #                   plot1vars = plot1vars,
    #                   plot2vars = plot2vars)
  
# Run the application~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    shinyApp(ui = ui, server = server)
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~