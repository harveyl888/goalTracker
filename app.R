##
## Goaltracker
##
## Shiny app to manage and track goals
##

library(shiny)
library(shinyExtra)
library(DBI)
library(pool)
library(dplyr)
library(DT)

pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = "goals.sqlite"
)

## Check tables exist in database
addMainTable <- !'mainGoals' %in% dbListTables(pool)
addSubTable <- !'subGoals' %in% dbListTables(pool)
if (any(addMainTable, addSubTable)) {
  conn <- poolCheckout(pool)
  if(addMainTable) dbSendQuery(conn, 'CREATE TABLE mainGoals (refMain INTEGER, name TEXT)')
  if(addSubTable) dbSendQuery(conn, 'CREATE TABLE subGoals (refSub INTEGER, refMain INTEGER, name TEXT, start TEXT, end TEXT, percentComplete INTEGER)')
  poolReturn(conn)
  
}

## Connect to database and read in tables
df.main <- dbReadTable(pool, 'mainGoals')
df.sub <- dbReadTable(pool, 'subGoals')

server <- function(input, output, session) {

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
    df.out <- data.frame(Delete = shinyInput(actionButton, nrow(goals$main), 'main_delbut_', label = 'Delete', onclick = 'Shiny.onInputChange(\"main_delete_button\", [this.id, Math.random()])'),
                         Edit = shinyInput(actionButton, nrow(goals$main), 'main_editbut_', label = 'Edit', onclick = 'Shiny.onInputChange(\"main_edit_button\", [this.id, Math.random()])'),
                         goals$main,
                         stringsAsFactors = FALSE)
    DT::datatable(df.out, escape = FALSE, selection = 'single', options = list(dom = 'tp'),
                                                                               callback = JS('
                                                                                  table.on("click.dt", "tbody td button", function(e) {
                                                                                    e.stopPropagation();
                                                                                  });
                                                                               ')
                  )
  }, server = FALSE)
  
  ## main goal deleted
  observeEvent(input$main_delete_button, {
    selectedRow <- as.numeric(strsplit(input$main_delete_button[1], "_")[[1]][3])
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
  
  ## main goal edit
  observeEvent(input$main_edit_button, {
    selectedRow <- as.numeric(strsplit(input$main_edit_button[1], "_")[[1]][3])
    showModal(modalDialog(
      title = "Edit Main Goal",
      div(style='display:inline-block; vertical-align:middle;', textInput('txtMainNameEdit', 'Name', value = goals$main[selectedRow, 'name'])),
      div(style='display:inline-block; vertical-align:middle;', actionButton('butMainConfirmEdit', 'OK', class = 'btn action-button btn-success'))
    ))
  })
  
  ## confirm main goal edit
  observeEvent(input$butMainConfirmEdit, {
    selectedRow <- as.numeric(strsplit(input$main_edit_button[1], "_")[[1]][3])
    goals$main[selectedRow, 'name'] <- input$txtMainNameEdit
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
    df.out <- data.frame(Delete = shinyInput(actionButton, nrow(goals$subFiltered), 'sub_delbut_', label = 'Delete', onclick = 'Shiny.onInputChange(\"sub_delete_button\", [this.id, Math.random()])'),
                         Edit = shinyInput(actionButton, nrow(goals$subFiltered), 'sub_editbut_', label = 'Edit', onclick = 'Shiny.onInputChange(\"sub_edit_button\", [this.id, Math.random()])'),
                         goals$subFiltered,
                         stringsAsFactors = FALSE)
    DT::datatable(df.out, escape = FALSE, selection = 'single', options = list(dom = 'tp'),
                                                                               callback = JS('
                                                                                  table.on("click.dt", "tbody td button", function(e) {
                                                                                    e.stopPropagation();
                                                                                  });
                                                                                ')
                  )
  }, server = FALSE)
  
  ## sub goal deleted
  observeEvent(input$sub_delete_button, {
    selectedRow <- as.numeric(strsplit(input$sub_delete_button[1], "_")[[1]][3])
    selectedRef <- goals$subFiltered[selectedRow, 'refSub']
    goals$sub <- goals$sub %>% 
      filter(refSub != selectedRef)
  })
  
  ## sub goal edit
  observeEvent(input$sub_edit_button, {
    selectedRow <- as.numeric(strsplit(input$sub_edit_button[1], "_")[[1]][3])
    selectedRef <- goals$subFiltered[selectedRow, 'refSub']
    subRow <- which(goals$sub$refSub == selectedRef)
    showModal(modalDialog(
      title = "Edit Sub Goal",
      div(style='display:inline-block; vertical-align:middle;', textInput('txtSubNameEdit', 'Name', value = goals$sub[subRow, 'name'])),
      div(style='display:inline-block; vertical-align:middle;', actionButton('butSubConfirmEdit', 'OK', class = 'btn action-button btn-success')),
      checkboxInput('chkTimeboundEdit', 'Time Bound?'),
      conditionalPanel('input.chkTimeboundEdit == true',
                       wellPanel(
                         dateInput('dateStartEdit', 'Start Date', value = goals$sub[subRow, 'start']),
                         dateInput('dateEndEdit', 'End Date', value = goals$sub[subRow, 'end'])
                       )
      )
    ))
  })
  
  ## confirm sub goal edit
  observeEvent(input$butSubConfirmEdit, {
    selectedRow <- as.numeric(strsplit(input$sub_edit_button[1], "_")[[1]][3])
    selectedRef <- goals$subFiltered[selectedRow, 'refSub']
    subRow <- which(goals$sub$refSub == selectedRef)
    if(input$chkTimeboundEdit == TRUE) {
      goalDates <- c(as.character(input$dateStartEdit), as.character(input$dateEndEdit))
    } else {
      goalDates <- c(NA, NA)
    }
    goals$sub[subRow, c(3, 4, 5)] <- c(input$txtSubNameEdit, goalDates[1], goalDates[2])
    removeModal()
  })
  
  
  ## Close database upon exit
  session$onSessionEnded(function() {
    observe({
      isolate({
        dbWriteTable(pool, 'mainGoals', goals$main, overwrite = TRUE)
        dbWriteTable(pool, 'subGoals', goals$sub, overwrite = TRUE)
        poolClose(pool)
      })
    })
    stopApp()
  })
  
  output$t1 <- renderTable(goals$main)
  output$t2 <- renderTable(goals$sub)
}

ui <- fluidPage(
  br(),
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

