#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "monitorMTF",
      id = "navbar",
      underline = TRUE,
      theme = bslib::bs_theme(version = 5),
      bslib::nav_panel(
        title = "Choose",
        value = "choose",
        mod_1_choose_ui("1_choose_1")
      ),
      bslib::nav_panel(
        title = "Setup",
        value = "setup",
        mod_2_setup_ui("2_setup_1")
      ),
      bslib::nav_panel(
        title = "Get",
        value = "get",
        mod_3_get_ui("3_get_1")
      ),
      bslib::nav_panel(
        title = "Validate",
        value = "validate",
        mod_4_validate_ui("4_validate_1")
      ),
      bslib::nav_panel(
        title = "Report",
        value = "report",
        mod_5_report_ui("5_report_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "monitorMTF"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(),
    waiter::use_waitress()
  )
}
