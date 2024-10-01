#' 4_validate_1_validate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_1_validate_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("run"),
      label = "Run"
    )

  )
}
    
#' 4_validate_1_validate Server Functions
#'
#' @noRd 
mod_4_validate_1_validate_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$run, {

      # ------------------------------------------------------------------------
      # Prepare file system
      # ------------------------------------------------------------------------

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # compose directories to hold data
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      dirs <- create_validation_dirs(proj_dir = info$proj_dir)

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # delete stale files from previous session
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      delete_stale_data(dir = dirs$data_hh)
      delete_stale_data(dir = dirs$recommend_hh)
      delete_stale_data(dir = dirs$decide_hh)
      delete_stale_data(dir = dirs$report_hh)

      # ------------------------------------------------------------------------
      # Execute decision-making workflow
      # ------------------------------------------------------------------------

      cases <- identify_cases_to_review(
        proj_dir = info$proj_dir,
        hhold_name = info$qnr_var_hhold
      )

      attributes <- create_attributes(
        proj_dir = info$proj_dir,
        hhold_name = info$qnr_var_hhold,
        cases_df = cases
      )

      issues <- create_issues(attributes = attributes)

      decisions <- create_decisions(
        proj_dir = info$proj_dir,
        cases_to_review = cases,
        issues = issues
      )

      # ------------------------------------------------------------------------
      # Save outputs to disk
      # ------------------------------------------------------------------------

      write_df_to_disk(df = cases, dir = dirs$recommend_hh)
      write_df_to_disk(df = attributes, dir = dirs$recommend_hh)
      write_df_to_disk(df = issues, dir = dirs$recommend_hh)

      write_df_list_to_disk(df_list = decisions, dir = dirs$recommend_hh)

      # ------------------------------------------------------------------------
      # send signal that validation is complete
      # ------------------------------------------------------------------------

      gargoyle::trigger("done_validate")

    })


  })
}
    
## To be copied in the UI
# mod_4_validate_1_validate_ui("4_validate_1_validate_1")
    
## To be copied in the server
# mod_4_validate_1_validate_server("4_validate_1_validate_1")
