#' 5_report_2_quality UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_5_report_2_quality_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("create"),
      "Create"
    ),
    # render download button UI here when conditions in server satisfied
    shiny::uiOutput(outputId = ns("dl_button"))

  )
}
    
#' 5_report_2_quality Server Functions
#'
#' @noRd 
mod_5_report_2_quality_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # initialize page
    # ==========================================================================

    # create a reactive container for these files attributes so that they can:
    # change during a single session
    # and be accessible to different scopes
    report <- shiny::reactiveValues(
      data_dir = NA_character_,
      data_exist = FALSE,
      output_path = NA_character_
    )

    gargoyle::on("load_project", {

      # require project directory and other inputs
      shiny::req(info$proj_dir, info$qnr_var_hhold)

      report$data_dir <- fs::path(
        info$proj_dir, "01_data", "01_hhold", "02_combined"
      )

      report$output_path <- fs::path(
        info$proj_dir, "03_reports", "02_quality", "report_quality.html"
      )

      main_file_path <- fs::path(
        report$data_dir, paste0(info$qnr_var_hhold, ".dta")
      )

      report$data_exist <- fs::file_exists(main_file_path)

      if (report$data_exist == FALSE) {

        shiny::updateActionButton(
          inputId = "create",
          disabled = TRUE
        )

      }

    })

    gargoyle::on("downloaded_data", {

      report$data_exist <- TRUE

      if (report$data_exist == TRUE) {

        shiny::updateActionButton(
          inputId = "create",
          disabled = FALSE
        )

      }

    })

    # ==========================================================================
    # react to create button
    # ==========================================================================

    # create a waitress that overlays the create button to communicate progress
    report_waitress <- waiter::Waitress$new(
      selector = '.accordion-item[data-value="quality_panel"] button.action-button',
      theme = "overlay-percent",
      infinite = TRUE
    )

    shiny::observeEvent(input$create, {

      # require inputs
      shiny::req(
        # project directory
        info$proj_dir,
        # report parameters
        info$qnr_var_hhold
      )

      # start the progress overlay
      report_waitress$start()

      # ------------------------------------------------------------------------
      # set reactive values
      # ------------------------------------------------------------------------

      report$data_dir <- fs::path(
        info$proj_dir, "01_data", "01_hhold", "02_combined"
      )

      report$output_path <- fs::path(
        info$proj_dir, "03_reports", "02_quality", "report_quality.html"
      )

      main_file_path <- fs::path(
        report$data_dir, paste0(info$qnr_var_hhold, ".dta")
      )

      # ------------------------------------------------------------------------
      # prepare file system
      # ------------------------------------------------------------------------

      dirs <- create_report_dirs(proj_dir = info$proj_dir)

      # ------------------------------------------------------------------------
      # check that Quarto is installed/visible on PATH
      # ------------------------------------------------------------------------

      # check
      quarto_installed <- !is.null(quarto::quarto_path())

      # alert user if there are problems
      # TODO

      # ------------------------------------------------------------------------
      # render document
      # ------------------------------------------------------------------------

      # compose parameter list
      doc_params <- list(
        proj_dir = info$proj_dir,
        data_dir = report$data_dir,
        hhold_name = info$qnr_var_hhold
      )

      # render document
      render_report(
        report_type = "quality",
        proj_dir = info$proj_dir,
        params = doc_params
      )

      # ------------------------------------------------------------------------
      # show report download button
      # ------------------------------------------------------------------------

      output$dl_button <- shiny::renderUI(

        shiny::downloadButton(
          outputId = ns("download"),
          label = "Download"
        )

      )

      # close the progress overlay
      report_waitress$close()

    })

    # ==========================================================================
    # react to download button
    # ==========================================================================

    output$download <- shiny::downloadHandler(
      filename = "report_quality.html",
      content = function(file) {
        fs::file_copy(path = report$output_path, new_path = file)
      }
    )

  })
}
    
## To be copied in the UI
# mod_5_report_2_quality_ui("5_report_2_quality_1")
    
## To be copied in the server
# mod_5_report_2_quality_server("5_report_2_quality_1")
