  
  # library(shiny)
  # library(tidyverse)
  # library(plotly)
  # library(ggplot2)
  # library(shinydashboard)
  # library(DT)
  # library(survival) 
  
  my_packages <- c("shiny","tidyverse","plotly","ggplot2","shinydashboard","DT","survival")
  
  lapply(my_packages, library, character.only = TRUE)    # Load multiple packages