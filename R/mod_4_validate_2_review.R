#' 4_validate_2_review UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_2_review_ui <- function(id) {
  ns <- NS(id)
  tagList(

# TODO: put summary info here
# card?

    shiny::uiOutput(outputId = ns("overview_card")),
    bslib::accordion(
      id = ns("review_steps"),
      bslib::accordion_panel(
        title = "Reject",
        value = "1_reject",
        mod_4_validate_2_review_1_reject_ui(
          ns("4_validate_2_review_1_reject_1")
        )
      ),
      bslib::accordion_panel(
        title = "Review",
        value = "2_review",
        mod_4_validate_2_review_1_review_ui(
          ns("4_validate_2_review_1_review_1")
        )
      ),
      bslib::accordion_panel(
        title = "Follow up",
        value = "3_follow_up",
        mod_4_validate_2_review_1_follow_up_ui(
          ns("4_validate_2_review_1_follow_up_1")
        )
      )
    )

  )
}
    
#' 4_validate_2_review Server Functions
#'
#' @noRd 
mod_4_validate_2_review_server <- function(id, parent, info){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    gargoyle::on("load_project", {

      shiny::req(
        info$proj_dir,
        info$n_to_reject, info$n_to_review, info$n_to_follow_up
      )

      output$overview_card <- shiny::renderUI(

        bslib::card(
          bslib::card_header(
            "Overview of validation results"
          ),
          bslib::card_body(
            htmltools::p(
              bsicons::bs_icon(name = "clipboard-x"),
              glue::glue("To reject : {info$n_to_reject}")
            ),
            htmltools::p(
              bsicons::bs_icon(name = "zoom-in"),
              glue::glue("To review : {info$n_to_review}")
            ),
            htmltools::p(
              bsicons::bs_icon(name = "chat-left-dots"),
              glue::glue("To follow up : {info$n_to_follow_up}")
            )
          )
        )

      )

    })

    # ==========================================================================
    # load server logic definitions of child modules
    # ==========================================================================

    mod_4_validate_2_review_1_reject_server(
      id = "4_validate_2_review_1_reject_1",
      parent = session,
      info = info
    )
    mod_4_validate_2_review_1_review_server(
      id = "4_validate_2_review_1_review_1",
      parent = session,
      info = info
    )
    mod_4_validate_2_review_1_follow_up_server(
      id = "4_validate_2_review_1_follow_up_1",
      parent = session,
      info = info
    )

    # ==========================================================================
    # manage opening and closing of accordion panels
    # ==========================================================================

    # TODO: simply close panel in which save button pressed?

    # ==========================================================================
    # update content of the overview card
    # ==========================================================================

    gargoyle::on("done_validate", {

      shiny::req(
        info$proj_dir,
        info$n_to_reject, info$n_to_review, info$n_to_follow_up
      )

      output$overview_card <- shiny::renderUI(

        bslib::card(
          bslib::card_header(
            "Overview of validation results"
          ),
          bslib::card_body(
            htmltools::p(glue::glue("To reject ❌: {info$n_to_reject}")),
            htmltools::p(glue::glue("To review 🔍: {info$n_to_review}")),
            htmltools::p(glue::glue("To follow up 😕: {info$n_to_follow_up}"))
          )
        )

      )

    })

  })
}
    
## To be copied in the UI
# mod_4_validate_2_review_ui("4_validate_2_review_1")
    
## To be copied in the server
# mod_4_validate_2_review_server("4_validate_2_review_1")
