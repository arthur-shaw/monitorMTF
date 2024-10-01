#' 4_validate_0_set_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_0_set_status_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectizeInput(
      inputId = ns("statuses_to_validate"),
      label = "Select the Survey Solutions status(es) to validate",
      choices = c(
        "Completed" = 100,
        "ApprovedBySupervisor" = 120,
        "ApprovedByHeadquarters" = 130
      ),
      multiple = TRUE,
      # select the most common cases
      selected = c(100, 120)
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_validate_0_set_status Server Functions
#'
#' @noRd 
mod_4_validate_0_set_status_server <- function(id, parent, info){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    gargoyle::on("load_project", {

      # set previous status selections
      if (!is.null(info$statuses_to_validate_provided)) {

        shiny::updateSelectizeInput(
          inputId = "statuses_to_validate",
          selected = info$statuses_to_validate
        )

      }

    })

    # ==========================================================================
    # react to saving
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture input in R6
      info$statuses_to_validate_provided <- TRUE
      info$statuses_to_validate <- input$statuses_to_validate

      # write R6 to disk
      info$write()

      # send signal that info saved
      gargoyle::trigger("saved_statuses_to_validate")

    })

  })
}
    
## To be copied in the UI
# mod_4_validate_0_set_status_ui("4_validate_0_set_status_1")
    
## To be copied in the server
# mod_4_validate_0_set_status_server("4_validate_0_set_status_1")
