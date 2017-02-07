##
## Goaltracker
##
## Shiny app to manage and track goals
##

library(shiny)
library(RSQLite)

createDB <- function() {
  db <- dbConnect(SQLite(), dbname='goals.sqlite')
  dbSendQuery(db,'CREATE TABLE mainGoals (name TEXT)')
  dbSendQuery(db,'CREATE TABLE subGoals (name TEXT, start TEXT, end TEXT, percentComplete INTEGER)')
  dbDisconnect(db)
}

if (!file.exists('goals.sqlite')) createDB()

server <- function(input, output) {
  
}

ui <- fluidPage(
  
)

shinyApp(ui = ui, server = server)

