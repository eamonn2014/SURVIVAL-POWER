dashboardSidebar(
  
  width=400,  #250
  
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
             menuSubItem(h4(HTML("A percentage change in survival probability<br/>at a fixed time is postulated")), tabName="survplot1"),
             menuSubItem(h4(HTML("An absolute percentage point change in survival<br/>probability at a fixed time is postulated")), tabName="survplot2")
             
             ),  # must match
             #menuSubItem(h4(HTML("xxxxxxxxxxxxxxxxxxxxxx<br/>xxxxxxxxxxxxxxxxxxxxxxxx")), tabName="survplot2")),  # must match
  
    
    menuItem("test2",
             menuSubItem("yyy", tabName="yyy")) 
    
  )
)
 