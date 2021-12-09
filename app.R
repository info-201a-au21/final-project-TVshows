## load library
library(shiny)
library(rsconnect)

# source
source("app_server.R")
source("app_ui.R")

# loaded `ui` and `server` variables
shinyApp(ui = ui, server = server)
