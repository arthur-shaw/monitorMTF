#' 4_validate_4_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_4_report_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("generate"),
      label = "Generate"
    ),
    shiny::downloadButton(
      outputId = "dl_report",
      label = "Download"
    )

  )
}
    
#' 4_validate_4_report Server Functions
#'
#' @noRd 
mod_4_validate_4_report_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
    
## To be copied in the UI
# mod_4_validate_4_report_ui("4_validate_4_report_1")
    
## To be copied in the server
# mod_4_validate_4_report_server("4_validate_4_report_1")
