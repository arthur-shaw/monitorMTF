#' 4_validate_2_edit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_2_edit_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_validate_2_edit Server Functions
#'
#' @noRd 
mod_4_validate_2_edit_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$save, {

      # send signal that editing is done
      gargoyle::trigger("done_edit")

    })

  })
}
    
## To be copied in the UI
# mod_4_validate_2_edit_ui("4_validate_2_edit_1")
    
## To be copied in the server
# mod_4_validate_2_edit_server("4_validate_2_edit_1")
