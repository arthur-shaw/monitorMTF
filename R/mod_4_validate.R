#' 4_validate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_validate_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion(
      id = ns("validate_steps"),
      bslib::accordion_panel(
        title = "1. Validate",
        value = "1_validate",
        mod_4_validate_1_validate_ui(ns("4_validate_1_validate_1"))
      ),
      bslib::accordion_panel(
        title = "2. Edit (optional)",
        value = "2_edit",
        mod_4_validate_2_edit_ui(ns("4_validate_2_edit_1"))
      ),
      bslib::accordion_panel(
        title = "3. Reject",
        value = "3_reject",
        mod_4_validate_3_reject_ui(ns("4_validate_3_reject_1"))
      ),
      bslib::accordion_panel(
        title = "4. Report",
        value = "4_report",
        mod_4_validate_4_report_ui(ns("4_validate_4_report_1"))
      ),
    )

  )
}

#' 4_validate Server Functions
#'
#' @noRd 
mod_4_validate_server <- function(id, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # load server logic definitions of child modules
    # ==========================================================================

    mod_4_validate_1_validate_server(
      id = "4_validate_1_validate_1",
      parent = session,
      info = info
    )
    mod_4_validate_2_edit_server(
      id = "4_validate_2_edit_1",
      parent = session,
      info = info
    )
    mod_4_validate_3_reject_server(
      id = "4_validate_3_reject_1",
      parent = session,
      info = info
    )
    mod_4_validate_4_report_server(
      id = "4_validate_4_report_1",
      parent = session,
      info = info
    )

    # ==========================================================================
    # manage opening and closing of accordion panels
    # ==========================================================================

    # from validate to edit
    gargoyle::on("done_validate", {

      # close validate
      bslib::accordion_panel_close(
        id = "validate_steps",
        value = "1_validate"
      )

      # open edit
      bslib::accordion_panel_open(
        id = "validate_steps",
        value = "2_edit"
      )

    })

    # from edit to reject
    gargoyle::on("done_edit", {

      # close edit
      bslib::accordion_panel_close(
        id = "validate_steps",
        value = "2_edit"
      )

      # open reject
      bslib::accordion_panel_open(
        id = "validate_steps",
        value = "3_reject"
      )

    })

    # from reject to report
    gargoyle::on("done_reject", {

      # close reject
      bslib::accordion_panel_close(
        id = "validate_steps",
        value = "3_reject"
      )

      # open report
      bslib::accordion_panel_open(
        id = "validate_steps",
        value = "4_report"
      )


    })

    # from reject to report tab ?
    gargoyle::on("done_report", {

      # close report
      bslib::accordion_panel_close(
        id = "validate_steps",
        value = "4_report"
      )

      # move focus to report tab
      # NOTE: done in app_server.R

    })

  })
}
    
## To be copied in the UI
# mod_4_validate_ui("4_validate_1")
    
## To be copied in the server
# mod_4_validate_server("4_validate_1")
