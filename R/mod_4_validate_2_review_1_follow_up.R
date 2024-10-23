#' 4_validate_2_review_1_follow_up UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_2_review_1_follow_up_ui <- function(id) {
  ns <- NS(id)
  tagList(

    reactable::reactableOutput(outputId = ns("to_follow_up"))

  )
}
    
#' 4_validate_2_review_1_follow_up Server Functions
#'
#' @noRd 
mod_4_validate_2_review_1_follow_up_server <- function(id, parent, info){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # initialize page
    # ==========================================================================

    # create a reactive container for these files attributes so that they can:
    # change during a single session
    # and be accessible to different scopes
    to_follow_up_file <- shiny::reactiveValues(
      path = NA_character_,
      exists = FALSE,
    )

    gargoyle::on("load_project", {

      # require inputs
      shiny::req(info$proj_dir)

      # set the path once project is loaded
      to_follow_up_file$path <- fs::path(
        info$proj_dir, "02_decisions", "02_recommendations", "01_hhold",
        "to_follow_up_api.dta"
      )

      # determine whether the file exists in the project
      to_follow_up_file$exists <- fs::file_exists(to_follow_up_file$path)

    })

    gargoyle::on("done_validate", {

      # require inputs
      shiny::req(info$proj_dir)

      # set the path once project is loaded
      to_follow_up_file$path <- fs::path(
        info$proj_dir, "02_decisions", "02_recommendations", "01_hhold",
        "to_follow_up_api.dta"
      )

      # determine whether the file exists in the project
      to_follow_up_file$exists <- fs::file_exists(to_follow_up_file$path)

    })

    output$to_follow_up <- reactable::renderReactable({

      shiny::req(to_follow_up_file$path)

      if (to_follow_up_file$exists == FALSE) {
        NULL
      } else {

        to_follow_up_df <- haven::read_dta(file = to_follow_up_file$path) |>
          # remove variable label and width attributes, keeping labels
          haven::zap_label() |>
          haven::zap_widths() |>
          dplyr::mutate(
            # construct a URL for each interview
            interview_url = httr::modify_url(
              url = info$server,
              path = fs::path(
                info$workspace, "Interview", "Review", interview__id
              )
            )
            # TODO: uncomment once update susoreview to return
            # `interview__status` rather than `date`
            # ,
            # make SuSo interview status into a factor
            # interview__status = haven::as_factor(
            #   interview__status, levels = "both"
            # )
          )

        reactable::reactable(
          data = to_follow_up_df,
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
                  to_follow_up_df$interview_url[index],
                  value
                )
              }
            ),
            reject_comment = reactable::colDef(
              name = "Reason(s) for rejection"
            ),
            # TODO: uncomment once update `susoreview`
            # interview__status = reactable::colDef(
            #   name = "SuSo status"
            # ),
            # hide column needed
            interview_url = reactable::colDef(show = FALSE)
          )

        )

      }

    })

  })
}
    
## To be copied in the UI
# mod_4_validate_2_review_1_follow_up_ui("4_validate_2_review_1_follow_up_1")
    
## To be copied in the server
# mod_4_validate_2_review_1_follow_up_server("4_validate_2_review_1_follow_up_1")
