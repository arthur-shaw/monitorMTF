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
 
  )
}
    
#' 5_report Server Functions
#'
#' @noRd 
mod_5_report_server <- function(id, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_5_report_ui("5_report_1")
    
## To be copied in the server
# mod_5_report_server("5_report_1")
