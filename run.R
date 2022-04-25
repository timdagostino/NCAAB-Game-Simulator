library(shiny)
install.packages("shinyBS")
library(shinyBS)
port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
