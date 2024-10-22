#' 4_validate_2_edit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_2_edit_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    ),
    rhandsontable::rHandsontableOutput(outputId = ns("to_reject"))

  )
}
    
#' 4_validate_2_edit Server Functions
#'
#' @noRd 
mod_4_validate_2_edit_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # create a reactive container for these files attributes so that they can:
    # change during a single session
    # and be accessible to different scopes
    to_reject_file <- shiny::reactiveValues(
      path = NA_character_,
      exists = FALSE,
    )

    gargoyle::on("load_project", {

      # set the path once project is loaded
      to_reject_file$path <- fs::path(
        info$proj_dir, "02_decisions", "02_recommendations", "01_hhold",
        "to_reject_api.dta"
      )

      # determine whether the file exists in the project
      to_reject_file$exists <- fs::file_exists(to_reject_file$path)

    })

    gargoyle::on("done_validate", {

      to_reject_file$path <- fs::path(
        info$proj_dir, "02_decisions", "02_recommendations", "01_hhold",
        "to_reject_api.dta"
      )

      to_reject_file$exists <- fs::file_exists(to_reject_file$path)

    })

    output$to_reject <- rhandsontable::renderRHandsontable({

      if (to_reject_file$exists == FALSE) {
        NULL
      } else {

        to_reject_df <- haven::read_dta(file = to_reject_file$path) |>
          haven::zap_label() |>
          haven::zap_labels() |>
          haven::zap_widths()

        n_to_reject <- nrow(to_reject_df)

        if (n_to_reject == 0) {

        } else if (n_to_reject > 0) {

          # compose interactive display table
          rhandsontable::rhandsontable(data = to_reject_df) |>
            # dictate read and write access of columns
            # read only: values of selected variables
            rhandsontable::hot_col(
              col = c(1, 3),
              readOnly = TRUE
            ) |>
            # highlight current row in focus
            rhandsontable::hot_table(highlightRow = TRUE) |>
            # set column widths
            rhandsontable::hot_cols(
              # note rhandsontable only supports pixels
              # unclear whether can set values as a function of table's area
              # and have it adapt dynamically
              # see: https://forum.handsontable.com/t/expect-column-width-in-percentage-in-handson-table/6908
              # note also: `wordWrap` isn't supported, but text in the cells
              # still appear to wrap
              colWidths = c(
                250,  # interview__id
                600,  # reject_comment
                150   # interview__status
              )
            )

        }

      }


    })

    shiny::observeEvent(input$save, {

      # require inputs in the table
      shiny::req(input$to_reject)

      # extract data frame from table
      to_reject_edited <- rhandsontable::hot_to_r(input$to_reject)

      # write it to disk
      haven::write_dta(
        data = to_reject_edited,
        path = fs::path(
          info$proj_dir, "02_decisions", "03_decisions", "01_hhold",
          "to_reject_api.dta"
        )
      )

      # send signal that editing is done
      gargoyle::trigger("done_edit")

    })

  })
}
    
## To be copied in the UI
# mod_4_validate_2_edit_ui("4_validate_2_edit_1")
    
## To be copied in the server
# mod_4_validate_2_edit_server("4_validate_2_edit_1")
