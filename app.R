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
  dbSendQuery(db,'CREATE TABLE mainGoals (refMain INTEGER, name TEXT)')
  dbSendQuery(db,'CREATE TABLE subGoals (refSub INTEGER, refMain INTEGER, name TEXT, start TEXT, end TEXT, percentComplete INTEGER)')
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
  goals <- reactiveValues(main = df.main, sub = df.sub, subFiltered = NULL)
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }

  ## Determine next main goal reference number
  nextMainRef <- function() {
    if (nrow(goals$main) == 0) {
      return(1)
    } else {
      return(max(goals$main$refMain) + 1)
    }
  }
  
  ## Determine next sub goal reference number
  nextSubRef <- function() {
    if (nrow(goals$sub) == 0) {
      return(1)
    } else {
      return(max(goals$sub$refSub) + 1)
    }
  }

  ## Add main goal modal window
  observeEvent(input$butAddMain, {
    showModal(modalDialog(
      title = "Add Main Goal",
      div(style='display:inline-block; vertical-align:middle;', textInput('txtMainName', 'Name')),
      div(style='display:inline-block; vertical-align:middle;', actionButton('butMainConfirm', 'Add', class = 'btn action-button btn-success'))
    ))
  })
  
  ## main goal added - update table
  observeEvent(input$butMainConfirm, {
    goals$main <- rbind(goals$main, data.frame(refMain = nextMainRef(),
                                               name = input$txtMainName,
                                               stringsAsFactors = FALSE))
    removeModal()
  })
  
  ## table of main goals
  output$tabMainGoals <- DT::renderDataTable({
    req(nrow(goals$main) > 0)
    df.out <- data.frame(Delete = shinyInput(actionButton, nrow(goals$main), 'main_delbut_', label = 'Delete', onclick = 'Shiny.onInputChange(\"main_delete_button\", this.id)'),
                         goals$main,
                         stringsAsFactors = FALSE)
    DT::datatable(df.out, escape = FALSE, selection = 'single'
    )
  }, server = FALSE)
  
  ## main goal deleted
  observeEvent(input$main_delete_button, {
    selectedRow <- as.numeric(strsplit(input$main_delete_button, "_")[[1]][3])
    mainGoalRef <- goals$main[selectedRow, 'refMain']
    subGoalCount <- nrow(goals$sub %>% filter(refMain == mainGoalRef))
    if (subGoalCount > 0) {  ## warn on deletion
      showModal(modalDialog(
        title = "Delete Main Goal",
        h4(paste0('Deleting this goal also deletes ', subGoalCount, ' sub-goal', ifelse(subGoalCount == 1, '', 's'))),
        actionButton('butMainDeleteOK', 'OK', class = 'btn action-button btn-success'),
        actionButton('butMainDeleteCancel', 'Cancel', class = 'btn action-button btn-danger')
      ))
    } else {
      goals$main <- goals$main %>% 
        slice(-selectedRow)
    }
  })
  
  ## confirm main goal deletion
  observeEvent(input$butMainDeleteOK, {
    removeModal()
    selectedRow <- as.numeric(strsplit(input$main_delete_button, "_")[[1]][3])
    mainGoalRef <- goals$main[selectedRow, 'refMain']
    goals$main <- goals$main %>% 
      slice(-selectedRow)
    goals$sub <- goals$sub %>%
      filter(refMain != mainGoalRef)
  })
  
  ## cancel main goal deletion
  observeEvent(input$butMainDeleteCancel, {
    removeModal()
  })
  
  ## Add sub goal modal window
  observeEvent(input$butAddSub, {
    showModal(modalDialog(
      title = "Add Sub Goal",
      div(style='display:inline-block; vertical-align:middle;', textInput('txtSubName', 'Name')),
      div(style='display:inline-block; vertical-align:middle;', actionButton('butSubConfirm', 'Add', class = 'btn action-button btn-success')),
      checkboxInput('chkTimebound', 'Time Bound?'),
      conditionalPanel('input.chkTimebound == true',
                       wellPanel(
                         dateInput('dateStart', 'Start Date'),
                         dateInput('dateEnd', 'End Date')
                       )
                       )
    ))
  })
  
  ## sub goal added - update table
  observeEvent(input$butSubConfirm, {
    mainGoalRef <- goals$main[input$tabMainGoals_rows_selected, 'refMain']
    if(input$chkTimebound == TRUE) {
      goalDates <- c(as.character(input$dateStart), as.character(input$dateEnd))
    } else {
      goalDates <- c(NA, NA)
    }
    goals$sub <- rbind(goals$sub, data.frame(refSub = nextSubRef(),
                                             refMain = mainGoalRef,
                                             name = input$txtSubName,
                                             start = goalDates[1],
                                             end = goalDates[2],
                                             percentComplete = 0,
                                             stringsAsFactors = FALSE))
    removeModal()
  })
  
  ## filtered sub goal table
  observe({
    mainGoalRef <- goals$main[input$tabMainGoals_rows_selected, 'refMain']
    goals$subFiltered <- goals$sub[goals$sub$refMain == mainGoalRef, ]
  })
  
  ## table of sub goals
  output$tabSubGoals <- DT::renderDataTable({
    req(nrow(goals$subFiltered) > 0)
    df.out <- data.frame(Delete = shinyInput(actionButton, nrow(goals$subFiltered), 'sub_delbut_', label = 'Delete', onclick = 'Shiny.onInputChange(\"sub_delete_button\", this.id)'),
                         goals$subFiltered,
                         stringsAsFactors = FALSE)
    DT::datatable(df.out, escape = FALSE, selection = 'single'
    )
  }, server = FALSE)
  
  ## sub goal deleted
  observeEvent(input$sub_delete_button, {
    selectedRow <- as.numeric(strsplit(input$sub_delete_button, "_")[[1]][3])
    selectedRef <- goals$subFiltered[selectedRow, 'refSub']
    goals$sub <- goals$sub %>% 
      filter(refSub != selectedRef)
  })
  
  output$t1 <- renderTable(goals$main)
  output$t2 <- renderTable(goals$sub)
}

ui <- fluidPage(
  
  SXPanel('panMainGoals', heading = 'Main Goals', text_size = 'large', styleclass = 'success',
          actionButton('butAddMain', 'Add', class = 'btn action-button btn-success'),
          DT::dataTableOutput('tabMainGoals')
          ),
  SXPanel('panSubGoals', heading = 'Sub Goals', text_size = 'large', styleclass = 'info',
          actionButton('butAddSub', 'Add', class = 'btn action-button btn-success'),
          DT::dataTableOutput('tabSubGoals')
  ),
  fluidRow(
    column(6, tableOutput('t1')),
    column(6, tableOutput('t2'))
  )
)

shinyApp(ui = ui, server = server)

