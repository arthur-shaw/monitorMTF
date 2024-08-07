#' 4_validate_3_reject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_3_reject_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("run"),
      label = "Run"
    )

  )
}
    
#' 4_validate_3_reject Server Functions
#'
#' @noRd 
mod_4_validate_3_reject_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # create a reactive container for these files attributes so that they can:
    # change during a single session
    # and be accessible to different scopes
    decisions_file <- shiny::reactiveValues(
      path = NA_character_,
      exists = FALSE
    )

    gargoyle::on("load_project", {

      decisions_file$path <- fs::path(
        info$proj_dir, "02_decisions", "03_decisions", "01_hhold",
        "to_reject_api.dta"
      )

      decisions_file$exists <- fs::file_exists(decisions_file$path)

      if (decisions_file$exists == FALSE) {

        shiny::updateActionButton(
          inputId = "run",
          disabled = TRUE
        )

      }

    })

    gargoyle::on("done_edit", {

      decisions_file$path <- fs::path(
        info$proj_dir, "02_decisions", "03_decisions", "01_hhold",
        "to_reject_api.dta"
      )

      decisions_file$exists <- fs::file_exists(decisions_file$path)

      if (decisions_file$exists == TRUE) {

        shiny::updateActionButton(
          inputId = "run",
          disabled = FALSE
        )

      }

    })


    # ==========================================================================
    # react to run
    # ==========================================================================

    shiny::observeEvent(input$run, {

      # load rejections from disk
      to_reject <- haven::read_dta(file = decisions_file$path)

      # execute rejection for these cases
      purrr::pwalk(
        .l = to_reject,
        .f = ~ susoreview::reject_interview(
          interview__id = ..1,
          interview__status = ..3,
          reject_comment = ..2,
          statuses_to_reject = c(
            100,  # Completed
            120   # ApprovedBySupervisor
          ),
          server = info$server,
          workspace = info$workspace,
          user = info$user,
          password = info$password
        )
      )

      # send signal that rejection has been done
      gargoyle::trigger("done_reject")

    })

  })
}
    
## To be copied in the UI
# mod_4_validate_3_reject_ui("4_validate_3_reject_1")
    
## To be copied in the server
# mod_4_validate_3_reject_server("4_validate_3_reject_1")
