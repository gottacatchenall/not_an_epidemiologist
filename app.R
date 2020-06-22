library(shiny)
library(tidyverse)
library(zoo)
library(ggthemr)
ggthemr('fresh', spacing=2 ,layout="scientific")


source('./src/shiny_app/ui.R')
source('./src/shiny_app/server.R')

shinyApp(ui = ui, server = server)
