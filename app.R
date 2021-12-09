## load library
library(shiny)

# source
source("app_ui.R")
source("app_server.R")

# loaded `ui` and `server` variables
shinyApp(ui = ui, server = server)
