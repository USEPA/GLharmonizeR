
filePathInput <- function(id){
  shiny::fluidRow(
    shiny::fileInput(
    id,
    paste("Filepath to", id),
    accept = c(".csv", ".Rds", ".rds", ".accdb", ".xls", ".xlsx"),
    buttonLabel = "Browse...",
    placeholder = "No file selected"
    ))
}

