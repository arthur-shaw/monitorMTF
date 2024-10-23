#' 4_validate_2_review_1_reject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_2_review_1_reject_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    ),
    rhandsontable::rHandsontableOutput(outputId = ns("to_reject"))

  )
}
    
#' 4_validate_2_review_1_reject Server Functions
#'
#' @noRd 
mod_4_validate_2_review_1_reject_server <- function(id, parent, info){
  moduleServer(id, function(input, output, session){
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

      # require inputs
      shiny::req(info$proj_dir)

      # set the path once project is loaded
      to_reject_file$path <- fs::path(
        info$proj_dir, "02_decisions", "02_recommendations", "01_hhold",
        "to_reject_api.dta"
      )

      # determine whether the file exists in the project
      to_reject_file$exists <- fs::file_exists(to_reject_file$path)

    })

    gargoyle::on("done_validate", {

      # require inputs
      shiny::req(info$proj_dir)

      # set path to file
      to_reject_file$path <- fs::path(
        info$proj_dir, "02_decisions", "02_recommendations", "01_hhold",
        "to_reject_api.dta"
      )

      # determine whether the file exists in the project
      to_reject_file$exists <- fs::file_exists(to_reject_file$path)

    })

    output$to_reject <- rhandsontable::renderRHandsontable({

      shiny::req(to_reject_file$path)

      if (to_reject_file$exists == FALSE) {
        NULL
      } else {

        to_reject_df <- haven::read_dta(file = to_reject_file$path) |>
          haven::zap_label() |>
          haven::zap_labels() |>
          haven::zap_widths() |>
          # construct a URL for each interview
          # perform row-wise since httr's functions aren't vectorized
          dplyr::rowwise() |>
          dplyr::mutate(
            interview_url = httr::modify_url(
              url = info$server,
              path = fs::path(
                info$workspace, "Interview", "Review", interview__id
              )
            ),
            # compose the URL with glue
            # recast to character in order to drop the additional glue class
            # that poses problems for rhandsontable's conversion back to R
            interview_url = glue::glue('<a href="{interview_url}" target="_blank">{interview__id}</a>'),
            interview_url = as.character(interview_url)

          ) |>
          dplyr::ungroup() |>
          # move columns
          dplyr::relocate(interview_url, .before = 1)	|>
          dplyr::select(-interview__id)
	
# browser()
        n_to_reject <- nrow(to_reject_df)

        if (n_to_reject == 0) {

        } else if (n_to_reject > 0) {

          shiny::req(to_reject_df)

          # compose interactive disp lay table
          rhandsontable::rhandsontable(
            data = to_reject_df,
            colHeaders = c(
              "Interview", "Rejection reason(s)", "Interview status"
            )
          ) |>
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
                300,  # interview_url
                600,  # reject_comment
                150   # interview__status
              )
            ) |>
            # show url as a link that opens in a new window
            rhandsontable::hot_col(
              col = 1,
              renderer = htmlwidgets::JS("safeHtmlRenderer")
            )

        }

      }


    })

    # ==========================================================================
    # react to save button
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # extract data frame from table
      # reconstruct the interview ID from the URL
      to_reject_edited <- rhandsontable::hot_to_r(input$to_reject) |>
        dplyr::mutate(
          interview__id = stringr::str_extract(
            string = interview_url,
            pattern = "(?<=>).+?(?=</)"
          ),
          .before = 1
        ) |>
        dplyr::select(interview__id, reject_comment, interview__status)

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
# mod_4_validate_2_review_1_reject_ui("4_validate_2_review_1_reject_1")
    
## To be copied in the server
# mod_4_validate_2_review_1_reject_server("4_validate_2_review_1_reject_1")
