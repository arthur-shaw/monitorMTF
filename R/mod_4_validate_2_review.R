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

    # bslib::card(


    # ),
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

  })
}
    
## To be copied in the UI
# mod_4_validate_2_review_ui("4_validate_2_review_1")
    
## To be copied in the server
# mod_4_validate_2_review_server("4_validate_2_review_1")
