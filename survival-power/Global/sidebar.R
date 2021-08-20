dashboardSidebar(
  
  width=250,
  
  sidebarMenu(
    
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id="tabs",
    
    menuItem("Wiki", tabname ="wiki"),
    
    menuItem("Summary Statistics", tabname ="sumstats"),
    
    menuItem("test",
             menuSubItem("xxx", tabName="xxx")),
    
    menuItem("test2",
             menuSubItem("yyy", tabName="yyy"))
    
  )
)