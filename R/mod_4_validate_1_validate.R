#' 4_validate_1_validate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_1_validate_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("run"),
      label = "Run"
    )

  )
}
    
#' 4_validate_1_validate Server Functions
#'
#' @noRd 
mod_4_validate_1_validate_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$run, {

      # send signal that validation is complete
      gargoyle::trigger("done_validate")

    })


  })
}
    
## To be copied in the UI
# mod_4_validate_1_validate_ui("4_validate_1_validate_1")
    
## To be copied in the server
# mod_4_validate_1_validate_server("4_validate_1_validate_1")
