#' 4_validate_3_reject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_3_reject_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("run"),
      label = "Run"
    )

  )
}
    
#' 4_validate_3_reject Server Functions
#'
#' @noRd 
mod_4_validate_3_reject_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$run, {

      # send signal that rejection has been done
      gargoyle::trigger("done_reject")

    })

  })
}
    
## To be copied in the UI
# mod_4_validate_3_reject_ui("4_validate_3_reject_1")
    
## To be copied in the server
# mod_4_validate_3_reject_server("4_validate_3_reject_1")
