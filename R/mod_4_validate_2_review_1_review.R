#' 4_validate_2_review_1_review UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_2_review_1_review_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::downloadButton(
      outputId = ns("download"),
      label = "Download"
    ),
    reactable::reactableOutput(outputId = ns("to_review"))

  )
}
    
#' 4_validate_2_review_1_review Server Functions
#'
#' @noRd 
mod_4_validate_2_review_1_review_server <- function(id, parent, info){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # create a reactive container for these files attributes so that they can:
    # change during a single session
    # and be accessible to different scopes
    to_review_file <- shiny::reactiveValues(
      path = NA_character_,
      exists = FALSE,
    )

    gargoyle::on("load_project", {

      # require inputs
      shiny::req(info$proj_dir)

      # set the path once project is loaded
      to_review_file$path <- fs::path(
        info$proj_dir, "02_decisions", "02_recommendations", "01_hhold",
        "to_review_api.dta"
      )

      # determine whether the file exists in the project
      to_review_file$exists <- fs::file_exists(to_review_file$path)

    })

    gargoyle::on("done_validate", {

      # require inputs
      shiny::req(info$proj_dir)

      # set the path once project is loaded
      to_review_file$path <- fs::path(
        info$proj_dir, "02_decisions", "02_recommendations", "01_hhold",
        "to_review_api.dta"
      )

      # determine whether the file exists in the project
      to_review_file$exists <- fs::file_exists(to_review_file$path)

    })

    output$to_review <- reactable::renderReactable({

      shiny::req(to_review_file$path)

      if (to_review_file$exists == FALSE) {
        NULL
      } else {

        # TODO: make this reactive
        to_review_df <- haven::read_dta(file = to_review_file$path) |>
          # remove variable label and width attributes, keeping labels
          # perform row-wise since httr's functions aren't vectorized
          haven::zap_label() |>
          haven::zap_widths() |>
          # construct metadata for each interview
          dplyr::rowwise() |>
          dplyr::mutate(
            # construct a URL for each interview
            interview_url = httr::modify_url(
              url = info$server,
              path = fs::path(
                info$workspace, "Interview", "Review", interview__id
              )
            ),
            # make SuSo interview status into a factor
            interview__status = haven::as_factor(
              interview__status, levels = "both"
            )
          ) |>
          dplyr::ungroup()

        reactable::reactable(
          data = to_review_df,
          columns = list(
            interview__id = reactable::colDef(
              name = "Interview",
              html = TRUE,
              # construct anchor tag
              # href is the URL
              # text is the interview ID
              cell = function(value, index) {
                sprintf(
                  fmt = '<a href="%s" target="_blank">%s</a>',
                  to_review_df$interview_url[index],
                  value
                )
              }
            ),
            reject_comment = reactable::colDef(
              name = "Reason(s) for rejection"
            ),
            interview__status = reactable::colDef(
              name = "SuSo status"
            ),
            # hide column needed
            interview_url = reactable::colDef(show = FALSE)
          )

        )

      }

    })

    # ==========================================================================
    # react to download button
    # ==========================================================================

    output$download <- shiny::downloadHandler(
      filename = "to_review_details.dta",
      content = function(file) {

        # serve up file on disk from which display table is derived
        fs::file_copy(
          path = to_review_file$path,
          new_path = file
        )

      }
    )

  })
}

## To be copied in the UI
# mod_4_validate_2_review_1_review_ui("4_validate_2_review_1_review_1")

## To be copied in the server
# mod_4_validate_2_review_1_review_server("4_validate_2_review_1_review_1")
