##
## Goaltracker
##
## Shiny app to manage and track goals
##

library(shiny)
library(shinyExtra)
library(RSQLite)

createDB <- function() {
  db <- dbConnect(SQLite(), dbname='goals.sqlite')
  dbSendQuery(db,'CREATE TABLE mainGoals (name TEXT)')
  dbSendQuery(db,'CREATE TABLE subGoals (name TEXT, start TEXT, end TEXT, percentComplete INTEGER)')
  dbDisconnect(db)
}

## If database does not exist, create it
if (!file.exists('goals.sqlite')) createDB()

## Connect to database and read in tables
db <- dbConnect(SQLite(), dbname='goals.sqlite')
df.main <- dbReadTable(db, 'mainGoals')
df.sub <- dbReadTable(db, 'subGoals')

server <- function(input, output) {
  
  ## main and subgoal data frames are reactive
  goals <- reactiveValues(main = df.main, sub = df.sub)
  
  observeEvent(input$butAddMain, {
    showModal(modalDialog(
      title = "Add Main Goal",
      div(style='display:inline-block; vertical-align:middle;', textInput('txtMainName', 'Name')),
      div(style='display:inline-block; vertical-align:middle;', actionButton('butMainConfirm', 'Add', class = 'btn action-button btn-success'))
    ))
  })
  
  observeEvent(input$butMainConfirm, {
    goals$main[nrow(goals$main) + 1, ] <- input$txtMainName
    removeModal()
  })
  
}

ui <- fluidPage(
  
  SXPanel('panMainGoals', heading = 'Main Goals', text_size = 'large', styleclass = 'success',
          actionButton('butAddMain', 'Add', class = 'btn action-button btn-success')
          )
  
)

shinyApp(ui = ui, server = server)

