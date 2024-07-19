#' 2_setup_2_qnrs_hhold UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_setup_2_qnrs_hhold_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::textInput(
      inputId = ns("qnr_txt"),
      label = bslib::popover(
        # where popover is invoked
        trigger = list(
          "Provide a string that identifies the questionnaire(s) of interest",
          bsicons::bs_icon("info-circle")
        ),
        # content of popover message
        shiny::p(
          'To do so, provide either a substring (e.g., "Household")',
          ' or a ', 
          htmltools::a(
            'regular expression',
            href = "https://regexlearn.com/"
          ),
          ' (e.g., "[Hh]ousehold")'
        )
      )
    ),
    shiny::actionButton(
      inputId = ns("search"),
      label = "Search"
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 2_setup_2_qnrs_hhold Server Functions
#'
#' @noRd 
mod_2_setup_2_qnrs_hhold_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture input in R6
      # info[[paste0("qnr_txt", qnr_name)]] <- input$qnr_text
      # info[[paste0("qnr_txt_provided", qnr_name)]] <- TRUE

      # write R6 to disk
      # info$write()

    })
  })
}
    
## To be copied in the UI
# mod_2_setup_2_qnrs_hhold_ui("2_setup_2_qnrs_hhold_1")
    
## To be copied in the server
# mod_2_setup_2_qnrs_hhold_server("2_setup_2_qnrs_hhold_1")
