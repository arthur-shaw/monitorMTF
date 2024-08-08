#' 5_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_5_report_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion(
      id = ns("reports"),
      bslib::accordion_panel(
        title = "Completeness",
        value = "completeness_panel",
        mod_5_report_1_completeness_ui(ns("5_report_1_completeness_1"))
      ),
      bslib::accordion_panel(
        title = "Quality",
        value = "quality_panel",
        mod_5_report_2_quality_ui(ns("5_report_2_quality_1"))
      )

    )
 
  )
}
    
#' 5_report Server Functions
#'
#' @noRd 
mod_5_report_server <- function(id, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # load server logic definitions of child modules
    # ==========================================================================

    mod_5_report_2_quality_server(
      id = "5_report_2_quality_1",
      parent = session,
      info = info
    )
    mod_5_report_1_completeness_server(
      id = "5_report_1_completeness_1",
      parent = session,
      info = info
    )

  })
}
    
## To be copied in the UI
# mod_5_report_ui("5_report_1")
    
## To be copied in the server
# mod_5_report_server("5_report_1")
