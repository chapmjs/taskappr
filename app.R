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
# Get environment variables with fallback logic
DB_HOST <- Sys.getenv("DB_HOST")
if (DB_HOST == "") DB_HOST <- "your_database_host.com"

DB_NAME <- Sys.getenv("DB_NAME")
if (DB_NAME == "") DB_NAME <- "your_database_name"

DB_USER <- Sys.getenv("DB_USER")
if (DB_USER == "") DB_USER <- "your_username"  # Replace with your MySQL username

DB_PASS <- Sys.getenv("DB_PASS")
if (DB_PASS == "") DB_PASS <- "your_password"  # Replace with your MySQL password

# Check if credentials are still default values
if (DB_HOST == "your_database_host.com" || DB_NAME == "your_database_name" || 
    DB_USER == "your_username" || DB_PASS == "your_password") {
  warning("Database configuration not properly set. Please configure environment variables or update the code directly.")
}

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
    # Debug info
    cat("Attempting to connect to database:\n")
    cat("Host:", DB_HOST, "\n")
    cat("Database:", DB_NAME, "\n")
    cat("User:", DB_USER, "\n")
    
    conn <- dbConnect(MySQL(),
                      host = DB_HOST,
                      dbname = DB_NAME,
                      user = DB_USER,
                      password = DB_PASS)
    
    # Set connection options to handle character encoding and data types
    dbExecute(conn, "SET NAMES utf8mb4")
    dbExecute(conn, "SET CHARACTER SET utf8mb4")
    
    cat("Database connection successful!\n")
    return(conn)
  }, error = function(e) {
    cat("Database connection error:", e$message, "\n")
    stop("Database connection failed: ", e$message)
  })
}

# CRUD Functions
get_tasks <- function(status_filter = "Open") {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Build the WHERE clause based on filter
  where_clause <- if (status_filter == "All") {
    ""
  } else {
    paste0("WHERE t.status = '", status_filter, "'")
  }
  
  query <- paste0("
    SELECT t.id, t.subject, t.category, t.status, 
           DATE_FORMAT(t.created_at, '%Y-%m-%d %H:%i:%s') as created_at,
           DATE_FORMAT(t.updated_at, '%Y-%m-%d %H:%i:%s') as updated_at,
           GROUP_CONCAT(n.note SEPARATOR ' | ') as notes
    FROM tasks t
    LEFT JOIN task_notes n ON t.id = n.task_id
    ", where_clause, "
    GROUP BY t.id, t.subject, t.category, t.status, t.created_at, t.updated_at
    ORDER BY t.category ASC, t.created_at DESC
  ")
  
  result <- dbGetQuery(conn, query)
  
  # Convert character dates back to POSIXct if needed
  if (nrow(result) > 0) {
    result$created_at <- as.POSIXct(result$created_at, format = "%Y-%m-%d %H:%M:%S")
    result$updated_at <- as.POSIXct(result$updated_at, format = "%Y-%m-%d %H:%M:%S")
  }
  
  return(result)
}

insert_task <- function(subject, category, status, note = NULL) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Escape single quotes in the subject
  escaped_subject <- gsub("'", "''", subject)
  
  # Insert task
  task_query <- paste0("INSERT INTO tasks (subject, category, status) VALUES ('", 
                       escaped_subject, "', ", category, ", '", status, "')")
  dbExecute(conn, task_query)
  
  # Get the inserted task ID
  task_id <- dbGetQuery(conn, "SELECT LAST_INSERT_ID() as id")$id
  
  # Insert note if provided
  if (!is.null(note) && nchar(trimws(note)) > 0) {
    escaped_note <- gsub("'", "''", note)
    note_query <- paste0("INSERT INTO task_notes (task_id, note) VALUES (", 
                         task_id, ", '", escaped_note, "')")
    dbExecute(conn, note_query)
  }
  
  return(task_id)
}

update_task <- function(id, subject, category, status) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Escape single quotes in the subject
  escaped_subject <- gsub("'", "''", subject)
  
  query <- paste0("UPDATE tasks SET subject = '", escaped_subject, 
                  "', category = ", category, ", status = '", status, 
                  "' WHERE id = ", id)
  dbExecute(conn, query)
}

delete_task <- function(id) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Delete notes first (foreign key constraint)
  dbExecute(conn, paste0("DELETE FROM task_notes WHERE task_id = ", id))
  
  # Delete task
  dbExecute(conn, paste0("DELETE FROM tasks WHERE id = ", id))
}

get_task_notes <- function(task_id) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- paste0("SELECT id, note, DATE_FORMAT(created_at, '%Y-%m-%d %H:%i:%s') as created_at FROM task_notes WHERE task_id = ", task_id, " ORDER BY created_at DESC")
  result <- dbGetQuery(conn, query)
  
  # Convert character dates back to POSIXct if needed
  if (nrow(result) > 0) {
    result$created_at <- as.POSIXct(result$created_at, format = "%Y-%m-%d %H:%M:%S")
  }
  
  return(result)
}

add_note <- function(task_id, note) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Escape single quotes in the note
  escaped_note <- gsub("'", "''", note)
  query <- paste0("INSERT INTO task_notes (task_id, note) VALUES (", task_id, ", '", escaped_note, "')")
  dbExecute(conn, query)
}

delete_note <- function(note_id) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- paste0("DELETE FROM task_notes WHERE id = ", note_id)
  dbExecute(conn, query)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Task App"),
  
  dashboardSidebar(
    collapsed = TRUE,  # Start with sidebar collapsed
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
            
            # Status filter control
            fluidRow(
              column(3,
                selectInput("status_filter", "Show Tasks:", 
                           choices = c("Open" = "Open", 
                                     "Idea" = "Idea", 
                                     "Closed" = "Closed", 
                                     "All" = "All"), 
                           selected = "Open")
              ),
              column(9,
                # Edit task button
                br(),
                actionButton("edit_task_btn", "Edit Selected Task", 
                            class = "btn-info", style = "margin-bottom: 10px;")
              )
            ),
            
            DT::dataTableOutput("tasks_table")
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
  
  # Create DataTable proxy for programmatic control
  tasks_table_proxy <- dataTableProxy("tasks_table")
  
  # Load tasks on startup
  observe({
    tryCatch({
      tasks_data(get_tasks("Open"))  # Default to Open tasks
    }, error = function(e) {
      showNotification(paste("Error loading tasks:", e$message), type = "error")
    })
  })
  
  # Handle status filter changes
  observeEvent(input$status_filter, {
    tryCatch({
      tasks_data(get_tasks(input$status_filter))
    }, error = function(e) {
      showNotification(paste("Error filtering tasks:", e$message), type = "error")
    })
  })
  
  # Tasks table
  output$tasks_table <- DT::renderDataTable({
    df <- tasks_data()
    if (nrow(df) > 0) {
      # Format the data for display
      df$category_name <- names(CATEGORIES)[match(df$category, CATEGORIES)]
      df$created_at <- format(as.POSIXct(df$created_at), "%Y-%m-%d %H:%M")
      
      # Truncate text fields to 35 characters
      df$subject_display <- ifelse(nchar(df$subject) > 35, 
                                   paste0(substr(df$subject, 1, 35), "..."), 
                                   df$subject)
      
      df$notes_display <- ifelse(!is.na(df$notes) & nchar(df$notes) > 35, 
                                 paste0(substr(df$notes, 1, 35), "..."), 
                                 ifelse(is.na(df$notes), "", df$notes))
      
      display_df <- df[, c("id", "subject_display", "category_name", "status", "created_at", "notes_display")]
      names(display_df) <- c("ID", "Subject", "Category", "Status", "Created", "Notes")
      
      DT::datatable(display_df, 
                    selection = "single",
                    filter = "top",
                    options = list(
                      pageLength = 25, 
                      lengthMenu = list(c(5, 10, 25, 50, 100, -1), 
                                       c('5', '10', '25', '50', '100', 'All')),
                      scrollX = TRUE,
                      search = list(regex = FALSE, caseInsensitive = TRUE),
                      searchHighlight = TRUE,
                      dom = 'Blfrtip',
                      order = list(list(2, 'asc'), list(4, 'desc')), # Category ASC, Created DESC
                      language = list(
                        search = "Search tasks:",
                        searchPlaceholder = "Enter search term...",
                        lengthMenu = "Show _MENU_ tasks per page"
                      ),
                      columnDefs = list(
                        list(width = "50px", targets = 0),    # ID column - narrow
                        list(width = "200px", targets = 1),   # Subject column - wider
                        list(width = "120px", targets = 2),   # Category column
                        list(width = "80px", targets = 3),    # Status column
                        list(width = "120px", targets = 4),   # Created column
                        list(width = "150px", targets = 5)    # Notes column
                      )
                    ),
                    rownames = FALSE)
    } else {
      DT::datatable(data.frame(Message = "No tasks found"), 
                    options = list(
                      dom = 't',
                      search = list(regex = FALSE, caseInsensitive = TRUE)
                    ), 
                    rownames = FALSE)
    }
  }, server = FALSE)
  
  # Handle row selection for editing (single click)
  observeEvent(input$edit_task_btn, {
    if (length(input$tasks_table_rows_selected) == 0) {
      showNotification("Please select a task to edit!", type = "warning")
      return()
    }
    
    show_edit_modal()
  })
  
  # Handle double-click for editing
  observeEvent(input$tasks_table_cell_clicked, {
    if (!is.null(input$tasks_table_cell_clicked$row)) {
      # Update the selected row
      selectRows(proxy = tasks_table_proxy, 
                 selected = input$tasks_table_cell_clicked$row)
      
      # Show edit modal
      show_edit_modal()
    }
  })
  
  # Function to show edit modal
  show_edit_modal <- function() {
    if (length(input$tasks_table_rows_selected) == 0) {
      showNotification("Please select a task to edit!", type = "warning")
      return()
    }
    
    selected_row <- input$tasks_table_rows_selected
    task_data <- tasks_data()[selected_row, ]
    
    current_task_id(task_data$id)
    
    # Load notes for this task
    tryCatch({
      notes <- get_task_notes(task_data$id)
      task_notes_data(notes)
    }, error = function(e) {
      showNotification(paste("Error loading notes:", e$message), type = "error")
    })
    
    # Show modal with task data
    showModal(modalDialog(
      title = "Edit Task",
      size = "l",
      
      fluidRow(
        column(6,
          textInput("edit_subject", "Task Subject:", value = task_data$subject)
        ),
        column(3,
          selectInput("edit_category", "Category:", 
                     choices = CATEGORIES, selected = task_data$category)
        ),
        column(3,
          selectInput("edit_status", "Status:", 
                     choices = STATUS_OPTIONS, selected = task_data$status)
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
      
      footer = tagList(
        actionButton("save_task_btn", "Save Changes", class = "btn-primary"),
        actionButton("delete_task_btn", "Delete Task", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  }
  
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
      tasks_data(get_tasks(input$status_filter))
      
      showNotification("Task added successfully!", type = "message")
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
      
      showNotification("Note added successfully!", type = "message")
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
      tasks_data(get_tasks(input$status_filter))
      
      removeModal()
      showNotification("Task updated successfully!", type = "message")
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
      tasks_data(get_tasks(input$status_filter))
      
      removeModal()
      showNotification("Task deleted successfully!", type = "message")
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
            showNotification("Note deleted successfully!", type = "message")
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
