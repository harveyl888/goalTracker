##
## Goaltracker
##
## Shiny app to manage and track goals
##

library(shiny)
library(shinyExtra)
library(RSQLite)
library(dplyr)
library(DT)

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

  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }

  ## main and subgoal data frames are reactive
  goals <- reactiveValues(main = df.main, sub = df.sub)
  
  observeEvent(input$butAddMain, {
    showModal(modalDialog(
      title = "Add Main Goal",
      div(style='display:inline-block; vertical-align:middle;', textInput('txtMainName', 'Name')),
      div(style='display:inline-block; vertical-align:middle;', actionButton('butMainConfirm', 'Add', class = 'btn action-button btn-success'))
    ))
  })
  
  ## main goal added - update table
  observeEvent(input$butMainConfirm, {
    goals$main[nrow(goals$main) + 1, ] <- input$txtMainName
    removeModal()
  })
  
  ## table of main goals
  output$tabMainGoals <- DT::renderDataTable({
    req(nrow(goals$main) > 0)
    df.out <- data.frame(Delete = shinyInput(actionButton, nrow(goals$main), 'delbut_', label = 'Delete', onclick = 'Shiny.onInputChange(\"delete_button\", this.id)'),
                         goals$main,
                         stringsAsFactors = FALSE)
    DT::datatable(df.out, escape = FALSE, selection = 'single'
    )
  }, server = FALSE)
  
  ## main goal deleted
  observeEvent(input$delete_button, {
    selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][2])
    goals$main <- goals$main %>% 
      slice(-selectedRow)
  })
  
}

ui <- fluidPage(
  
  SXPanel('panMainGoals', heading = 'Main Goals', text_size = 'large', styleclass = 'success',
          actionButton('butAddMain', 'Add', class = 'btn action-button btn-success'),
          DT::dataTableOutput('tabMainGoals')
          )
  
)

shinyApp(ui = ui, server = server)

