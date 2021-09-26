dashboardSidebar(
  
  width=400,  #250
  
  sidebarMenu(
    
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id="tabs",
    
    menuItem(text="Wiki", tabName="wiki" ,  icon = icon("home") ),
 
    # wrap the text
  menuItem("A. Exponential survival plots, fixed survival time", icon = icon("cog", class="fa-spin"),
             menuSubItem(h5(HTML("1. Relative risk change: A percentage change in survival <br/>probability at a fixed time is postulated")), tabName="survplot1", icon = icon("bar-chart-o",lib='glyphicon')),
             menuSubItem(h5(HTML("2. Absolute risk change: An absolute percentage point change<br/>in survivalprobability at a fixed time is postulated")), tabName="survplot2", icon = icon("bar-chart-o",lib='glyphicon')),
             menuSubItem(h5(HTML("3. A hazard ratio is postulated")), tabName="survplot3", icon = icon("bar-chart-o",lib='glyphicon'))
             
             ), 
  menuItem("B. Exponential survival plots, fixed survival probability", icon = icon("cog", class="fa-spin"),
           menuSubItem(h5(HTML("4. A (relative) percentage change in survival time<br/>at a fixed survival probability is postulated")), tabName="survplot4", icon = icon("bar-chart-o",lib='glyphicon')),
           menuSubItem(h5(HTML("5. An absolute change in survival time<br/>at a fixed survival probability is postulated")), tabName="survplot5", icon = icon("bar-chart-o",lib='glyphicon')),
           menuSubItem(h5(HTML("6. A hazard ratio is postulated")), tabName="survplot6", icon = icon("bar-chart-o",lib='glyphicon'))
  ), 
    
  menuItem("C. Plot simulations of simple study", tabName="survplot7",  icon = icon("cog", class="fa-spin")),
  
  menuItem("D. Example study power calculation Nquery example",tabName="survplot8",  icon = icon("cog", class="fa-spin")),
 
  menuItem("E. Example power calculation published paper", tabName="survplot9",  icon = icon("cog", class="fa-spin")),
 
  menuItem("F. Example power calculation Stata", tabName="survplot11",  icon = icon("cog", class="fa-spin")),
  
  menuItem("G. Example study power calculation David Collett", tabName="survplot10",  icon = icon("cog", class="fa-spin")),
  
  menuItem("H: Example power calculation + plots",   icon = icon("cog", class="fa-spin"),   
          menuSubItem(h5(HTML("Hmisc::cpower + plot")), tabName="survplot12", icon = icon("bar-chart-o",lib='glyphicon') ),    # tab name must match! ...tabItem("testing",...
          menuSubItem(h5(HTML("Plots from two perspectives!")), tabName="survplot13", icon = icon("bar-chart-o",lib='glyphicon') )
 ),

 
  menuItem("References", icon = icon("users"),
           
           menuSubItem(h5(HTML( "Generating survival times to simulate Cox proportional<br/>hazards models - key paper for simulation")),  
                       icon = icon("send",lib='glyphicon'), 
                       href = "https://pubmed.ncbi.nlm.nih.gov/15724232/"),
           
           menuSubItem(h5(HTML( "Here's the Shiny code")),
                       icon = icon("send",lib='glyphicon'), 
                       href = "https://github.com/eamonn2014/Survival-power/tree/master/survival-power") ,
           
           
           menuSubItem(h5(HTML( "Here's some R code")),
                       icon = icon("send",lib='glyphicon'), 
                       href = "https://raw.githubusercontent.com/eamonn2014/Survival-power/master/survival-power-exploration.R") ,
           
            
           menuSubItem( h5(HTML("Survival converter")),  
                        icon = icon("send",lib='glyphicon'), 
                        href = "https://stattools.crab.org/R/Survival_Converter.html"),
           
           menuSubItem( h5(HTML("How to simulate survival times using true base line<br/>hazard function")),  
                        icon = icon("send",lib='glyphicon'), 
                        href = "https://stats.stackexchange.com/questions/105881/how-to-simulate-survival-times-using-true-base-line-hazard-function"),
           
           menuSubItem( h5(HTML("Sample Size For Survival Analysis - A guide to planning<br/>successful clinical trials - we duplicate this example")),  
                        icon = icon("send",lib='glyphicon'), 
                        href = "https://www.youtube.com/watch?v=inMjG32nzcw&ab_channel=Statsols%28ProviderofnQuery%29"),
           
           menuSubItem( h5(HTML("Simulate censored survival data - useful starting point")),  
                        icon = icon("send",lib='glyphicon'), 
                        href = "https://sas-and-r.blogspot.com/2010/03/example-730-simulate-censored-survival.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+SASandR+%28SAS+and+R%29") 
           
            
           
           
           #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
  
  
)
  ##
  
  )
)
 

# menuItem(tabName = 'homepage', text = 'Homepage',icon = icon('home')),
# menuItem(tabName = 'setup', text = 'Setup', icon = icon('cog')),
# menuItem(tabName = 'patient_search', text = 'Patient Search', icon = icon('users')),
# menuItem(tabName = 'chart_review', text ='Chart Review', icon = icon('table'))
# )