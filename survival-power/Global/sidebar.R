dashboardSidebar(
  
  width=350,  #250
  
  sidebarMenu(
    
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id="tabs",
    
    menuItem("Wiki", tabname ="wiki"),
    
    menuItem("Summary Statistics", tabname ="sumstats"),
    
    menuItem("test",
             menuSubItem("xxx", tabName="xxx")),
    
    menuItem("charts",
             menuSubItem("Boxplot", tabName="Boxplot")),  # must match
    
    # wrap the text
    menuItem("Survival plots",
             menuSubItem(h4(HTML("A percentage change in survival probability<br/>at a fixed time is postulated")), tabName="survplot1")),  # must match
    
    
    menuItem("test2",
             menuSubItem("yyy", tabName="yyy")) 
    
  )
)

 