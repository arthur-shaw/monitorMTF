#' 2_setup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_setup_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion(
      id = ns("setup"),
      bslib::accordion_panel(
        title = "Server details",
        value = "server_details_panel",
        mod_2_setup_1_suso_creds_ui(ns("2_setup_1_suso_creds_1"))
      ),
      bslib::accordion_panel(
        title = "Questionnaires",
        value = "qnrs_panel",
        mod_2_setup_2_qnrs_ui(ns("2_setup_2_qnrs_1"))
      )

    )


  )
}
    
#' 2_setup Server Functions
#'
#' @noRd 
mod_2_setup_server <- function(id, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # load definition of child modules
    # ==========================================================================

    mod_2_setup_1_suso_creds_server(
      id = "2_setup_1_suso_creds_1",
      parent = session,
      info = info
    )

    mod_2_setup_2_qnrs_server(
      id = "2_setup_2_qnrs_1",
      parent = session,
      info = info
    )

    # ==========================================================================
    # manage opening and closing of accordions
    # ==========================================================================

    # from server details to questionnaires
    gargoyle::on("save_creds", {

      # close server details
      bslib::accordion_panel_close(
        id = "setup",
        values = "server_details_panel"
      )

      # open questionnaires
      bslib::accordion_panel_open(
        id = "setup",
        values = "qnrs_panel"
      )

    })

    # from questionnaires to get data tab
    gargoyle::on("qnr_saved_health", {

      # close questionnaires
      bslib::accordion_panel_close(
        id = "setup",
        values = "qnrs_panel"
      )

      # move to get data tab
      # NOTE: this is handled in app_server.R

    })

  })
}
    
## To be copied in the UI
# mod_2_setup_ui("2_setup_1")
    
## To be copied in the server
# mod_2_setup_server("2_setup_1")
