## load library
library(shiny)
library(rsconnect)

# source
source("app_ui.R")
source("app_server.R")

# loaded `ui` and `server` variables
shinyApp(ui = ui, server = server)
