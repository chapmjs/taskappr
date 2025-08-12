# Simple Task App in R Shiny with MySQL Backend
# File: app.R

library(shiny)
library(shinydashboard)
library(DT)
library(RMySQL)
library(DBI)
library(shinyWidgets)
library(lubridate)

# Database configuration
DB_HOST <- "mexico.bbfarm.org"
DB_NAME <- "chapmjs_taskappdb"
DB_USER <- "your_username"  # Replace with your MySQL username
DB_PASS <- "your_password"  # Replace with your MySQL password

# Category mapping
CATEGORIES <- c(
  "1 - Relationship with God" = 1,
  "2 - Spouse" = 2,
  "3 - Family" = 3,
  "4 - Church" = 4,
  "5 - Work-Education" = 5,
  "6 - Community-Friends" = 6,
  "7 - Hobbies-Interest" = 7
)

STATUS_OPTIONS <- c("Idea", "Open", "Closed")

# Database connection function
get_db_connection <- function() {
  tryCatch({
    dbConnect(MySQL(),
              host = DB_HOST,
              dbname = DB_NAME,
              user = DB_USER,
              password = DB_PASS)
  }, error = function(e) {
    stop("Database connection failed: ", e$message)
  })
}

# CRUD Functions
get_tasks <- function() {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- "
    SELECT t.id, t.subject, t.category, t.status, t.created_at, t.updated_at,
           GROUP_CONCAT(n.note SEPARATOR ' | ') as notes
    FROM tasks t
    LEFT JOIN notes n ON t.id = n.task_id
    GROUP BY t.id, t.subject, t.category, t.status, t.created_at, t.updated_at
    ORDER BY t.created_at DESC
  "
  
  result <- dbGetQuery(conn, query)
  return(result)
}

insert_task <- function(subject, category, status, note = NULL) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Insert task
  task_query <- "INSERT INTO tasks (subject, category, status) VALUES (?, ?, ?)"
  dbExecute(conn, task_query, params = list(subject, category, status))
  
  # Get the inserted task ID
  task_id <- dbGetQuery(conn, "SELECT LAST_INSERT_ID() as id")$id
  
  # Insert note if provided
  if (!is.null(note) && nchar(trimws(note)) > 0) {
    note_query <- "INSERT INTO notes (task_id, note) VALUES (?, ?)"
    dbExecute(conn, note_query, params = list(task_id, note))
  }
  
  return(task_id)
}

update_task <- function(id, subject, category, status) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- "UPDATE tasks SET subject = ?, category = ?, status = ? WHERE id = ?"
  dbExecute(conn, query, params = list(subject, category, status, id))
}

delete_task <- function(id) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Delete notes first (foreign key constraint)
  dbExecute(conn, "DELETE FROM notes WHERE task_id = ?", params = list(id))
  
  # Delete task
  dbExecute(conn, "DELETE FROM tasks WHERE id = ?", params = list(id))
}

get_task_notes <- function(task_id) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- "SELECT id, note, created_at FROM notes WHERE task_id = ? ORDER BY created_at DESC"
  result <- dbGetQuery(conn, query, params = list(task_id))
  return(result)
}

add_note <- function(task_id, note) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- "INSERT INTO notes (task_id, note) VALUES (?, ?)"
  dbExecute(conn, query, params = list(task_id, note))
}

delete_note <- function(note_id) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- "DELETE FROM notes WHERE id = ?"
  dbExecute(conn, query, params = list(note_id))
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Simple Task Manager"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tasks", tabName = "tasks", icon = icon("tasks")),
      menuItem("Add Task", tabName = "add_task", icon = icon("plus"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Tasks tab
      tabItem(tabName = "tasks",
        fluidRow(
          box(
            title = "All Tasks", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("tasks_table")
          )
        ),
        
        # Modal for editing tasks
        bsModal("edit_modal", "Edit Task", "edit_btn", size = "large",
          fluidRow(
            column(6,
              textInput("edit_subject", "Task Subject:", value = "")
            ),
            column(3,
              selectInput("edit_category", "Category:", 
                         choices = CATEGORIES, selected = 1)
            ),
            column(3,
              selectInput("edit_status", "Status:", 
                         choices = STATUS_OPTIONS, selected = "Idea")
            )
          ),
          
          h4("Notes:"),
          DT::dataTableOutput("task_notes_table"),
          br(),
          
          fluidRow(
            column(10,
              textAreaInput("new_note", "Add New Note:", value = "", 
                           placeholder = "Enter a new note...", rows = 3)
            ),
            column(2,
              br(),
              actionButton("add_note_btn", "Add Note", 
                          class = "btn-success", style = "margin-top: 5px;")
            )
          ),
          
          br(),
          div(style = "text-align: right;",
            actionButton("save_task_btn", "Save Changes", class = "btn-primary"),
            actionButton("delete_task_btn", "Delete Task", class = "btn-danger")
          )
        )
      ),
      
      # Add Task tab
      tabItem(tabName = "add_task",
        fluidRow(
          box(
            title = "Add New Task", status = "primary", solidHeader = TRUE, width = 8,
            
            textInput("subject", "Task Subject:", 
                     placeholder = "Enter task subject..."),
            
            selectInput("category", "Category:", 
                       choices = CATEGORIES, selected = 1),
            
            selectInput("status", "Status:", 
                       choices = STATUS_OPTIONS, selected = "Idea"),
            
            textAreaInput("note", "Initial Note (Optional):", 
                         placeholder = "Enter an optional note...", 
                         rows = 4),
            
            br(),
            actionButton("submit_btn", "Add Task", 
                        class = "btn-primary btn-lg")
          ),
          
          box(
            title = "Instructions", status = "info", solidHeader = TRUE, width = 4,
            p("Fill out the form to add a new task."),
            p("• Subject is required"),
            p("• Choose appropriate category"),
            p("• Set initial status"),
            p("• Add an optional note"),
            br(),
            p("You can add more notes and edit tasks from the Tasks tab.")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  tasks_data <- reactiveVal(data.frame())
  current_task_id <- reactiveVal(NULL)
  task_notes_data <- reactiveVal(data.frame())
  
  # Load tasks on startup
  observe({
    tryCatch({
      tasks_data(get_tasks())
    }, error = function(e) {
      showNotification(paste("Error loading tasks:", e$message), type = "error")
    })
  })
  
  # Tasks table
  output$tasks_table <- DT::renderDataTable({
    df <- tasks_data()
    if (nrow(df) > 0) {
      # Format the data for display
      df$category_name <- names(CATEGORIES)[match(df$category, CATEGORIES)]
      df$created_at <- format(as.POSIXct(df$created_at), "%Y-%m-%d %H:%M")
      
      display_df <- df[, c("id", "subject", "category_name", "status", "created_at", "notes")]
      names(display_df) <- c("ID", "Subject", "Category", "Status", "Created", "Notes")
      
      DT::datatable(display_df, 
                    selection = "single",
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    } else {
      DT::datatable(data.frame(Message = "No tasks found"), 
                    options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  # Handle row selection for editing
  observeEvent(input$tasks_table_rows_selected, {
    if (length(input$tasks_table_rows_selected) > 0) {
      selected_row <- input$tasks_table_rows_selected
      task_data <- tasks_data()[selected_row, ]
      
      current_task_id(task_data$id)
      
      # Populate edit form
      updateTextInput(session, "edit_subject", value = task_data$subject)
      updateSelectInput(session, "edit_category", selected = task_data$category)
      updateSelectInput(session, "edit_status", selected = task_data$status)
      
      # Load notes for this task
      tryCatch({
        notes <- get_task_notes(task_data$id)
        task_notes_data(notes)
      }, error = function(e) {
        showNotification(paste("Error loading notes:", e$message), type = "error")
      })
      
      # Show modal
      toggleModal(session, "edit_modal", toggle = "open")
    }
  })
  
  # Task notes table
  output$task_notes_table <- DT::renderDataTable({
    notes_df <- task_notes_data()
    if (nrow(notes_df) > 0) {
      notes_df$created_at <- format(as.POSIXct(notes_df$created_at), "%Y-%m-%d %H:%M")
      display_notes <- notes_df[, c("note", "created_at")]
      names(display_notes) <- c("Note", "Created")
      
      DT::datatable(display_notes,
                    selection = "single",
                    options = list(pageLength = 5, dom = 'tp'),
                    rownames = FALSE)
    } else {
      DT::datatable(data.frame(Message = "No notes for this task"),
                    options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  # Add new task
  observeEvent(input$submit_btn, {
    if (nchar(trimws(input$subject)) == 0) {
      showNotification("Task subject is required!", type = "error")
      return()
    }
    
    tryCatch({
      insert_task(input$subject, input$category, input$status, input$note)
      
      # Reset form
      updateTextInput(session, "subject", value = "")
      updateTextAreaInput(session, "note", value = "")
      updateSelectInput(session, "status", selected = "Idea")
      
      # Refresh tasks
      tasks_data(get_tasks())
      
      showNotification("Task added successfully!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error adding task:", e$message), type = "error")
    })
  })
  
  # Add note to existing task
  observeEvent(input$add_note_btn, {
    if (is.null(current_task_id()) || nchar(trimws(input$new_note)) == 0) {
      showNotification("Note cannot be empty!", type = "error")
      return()
    }
    
    tryCatch({
      add_note(current_task_id(), input$new_note)
      updateTextAreaInput(session, "new_note", value = "")
      
      # Refresh notes
      notes <- get_task_notes(current_task_id())
      task_notes_data(notes)
      
      showNotification("Note added successfully!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error adding note:", e$message), type = "error")
    })
  })
  
  # Save task changes
  observeEvent(input$save_task_btn, {
    if (is.null(current_task_id()) || nchar(trimws(input$edit_subject)) == 0) {
      showNotification("Task subject is required!", type = "error")
      return()
    }
    
    tryCatch({
      update_task(current_task_id(), input$edit_subject, 
                  input$edit_category, input$edit_status)
      
      # Refresh tasks
      tasks_data(get_tasks())
      
      toggleModal(session, "edit_modal", toggle = "close")
      showNotification("Task updated successfully!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error updating task:", e$message), type = "error")
    })
  })
  
  # Delete task
  observeEvent(input$delete_task_btn, {
    if (is.null(current_task_id())) return()
    
    showModal(modalDialog(
      title = "Confirm Delete",
      "Are you sure you want to delete this task and all its notes?",
      footer = tagList(
        actionButton("confirm_delete", "Delete", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirm_delete, {
    tryCatch({
      delete_task(current_task_id())
      
      # Refresh tasks
      tasks_data(get_tasks())
      
      removeModal()
      toggleModal(session, "edit_modal", toggle = "close")
      showNotification("Task deleted successfully!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error deleting task:", e$message), type = "error")
    })
  })
  
  # Delete note (double-click to delete)
  observeEvent(input$task_notes_table_rows_selected, {
    if (length(input$task_notes_table_rows_selected) > 0) {
      selected_note_row <- input$task_notes_table_rows_selected
      notes_df <- task_notes_data()
      
      if (nrow(notes_df) > 0) {
        note_id <- notes_df[selected_note_row, "id"]
        
        showModal(modalDialog(
          title = "Delete Note",
          "Are you sure you want to delete this note?",
          footer = tagList(
            actionButton("confirm_delete_note", "Delete", class = "btn-danger"),
            modalButton("Cancel")
          )
        ))
        
        # Store note ID for deletion
        current_note_id <- note_id
        
        observeEvent(input$confirm_delete_note, {
          tryCatch({
            delete_note(current_note_id)
            
            # Refresh notes
            notes <- get_task_notes(current_task_id())
            task_notes_data(notes)
            
            removeModal()
            showNotification("Note deleted successfully!", type = "success")
          }, error = function(e) {
            showNotification(paste("Error deleting note:", e$message), type = "error")
          })
        }, once = TRUE)
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
