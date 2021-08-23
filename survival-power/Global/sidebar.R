dashboardSidebar(
  
  width=400,  #250
  
  sidebarMenu(
    
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id="tabs",
    
    menuItem(text="Wiki", 
             menuSubItem(h4(HTML("xxxxxxxxxxx")), tabName="wiki")), 
              
    
  #  menuItem("Summary Statistics", tabname ="sumstats"),
    
  #  menuItem("test",
  #           menuSubItem("xxx", tabName="xxx")),
    
   # menuItem("charts",
   #          menuSubItem("Boxplot", tabName="Boxplot")),  # must match
    
    # wrap the text
    menuItem("Exponential survival plots, fixed survival time",
             menuSubItem(h4(HTML("A percentage change in survival probability<br/>at a fixed time is postulated")), tabName="survplot1"),
             menuSubItem(h4(HTML("An absolute percentage point change in survival<br/>probability at a fixed time is postulated")), tabName="survplot2"),
             menuSubItem(h4(HTML("A hazard ratio is postulated")), tabName="survplot3")
             
             ), #, 
  menuItem("Exponential survival plots, fixed survival probability",
           menuSubItem(h4(HTML("A percentage change in survival time<br/>at a fixed survival probability is postulated")), tabName="survplot4")
           
           
  ) #, 
    
   # menuItem("test2",
          #   menuSubItem("yyy", tabName="yyy")) 
    
  )
)
 

# menuItem(tabName = 'homepage', text = 'Homepage',icon = icon('home')),
# menuItem(tabName = 'setup', text = 'Setup', icon = icon('cog')),
# menuItem(tabName = 'patient_search', text = 'Patient Search', icon = icon('users')),
# menuItem(tabName = 'chart_review', text ='Chart Review', icon = icon('table'))
# )