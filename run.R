library(shiny)
install.packages("shinyBS")
install.packages("shinythemes")
library(shinyBS)
library(shinythemes)
port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
