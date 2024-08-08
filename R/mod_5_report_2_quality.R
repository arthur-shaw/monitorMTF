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
    shiny::downloadButton(
      outputId = ns("download"),
      label = "Download"
    )

  )
}
    
#' 5_report_2_quality Server Functions
#'
#' @noRd 
mod_5_report_2_quality_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}
    
## To be copied in the UI
# mod_5_report_2_quality_ui("5_report_2_quality_1")
    
## To be copied in the server
# mod_5_report_2_quality_server("5_report_2_quality_1")
