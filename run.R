library(shiny)
install.packages("shinyBS")
install.packages("rvest")
install.packages("plyr")
install.packages("dplyr")
install.packages("shinycustomloader")
install.packages("DT")
install.packages("shinycssloaders")
install.packages("profvis")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("colorfindr")
install.packages("shinyalert")
library(rvest)

install.packages("shinythemes")
library(shinyBS)
library(shinythemes)
port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
