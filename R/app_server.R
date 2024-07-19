#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # ============================================================================
  # initialize R6 objects
  # ============================================================================

  # create application directory
  app_dir <- create_user_app_dir()

  # instantiate objects
  list <- list$new()
  info <- info$new()

  # set path for user's app directory
  list$app_dir <- create_user_app_dir()

  # handle R6 object as a function of (past) app state
  # case 1: app never used => ; past R6 not found as RDS in local storage
  # write new R6 to local storage
  settings_file_exists <- fs::file_exists(fs::path(app_dir, "projects.rds"))
  if (!settings_file_exists) {
    list$write()
  # case 2: app used => RDS exists in app's user data folder
  # restore r6 from past session by reading its values from RDS to r6
  } else {
    list$read()
  }

  # ============================================================================
  # initialize gargoyle listeners
  # ============================================================================

  gargoyle::init("load_project")
  gargoyle::init("save_creds")
  gargoyle::init("qnr_saved_hhold")
  gargoyle::init("qnr_saved_comm")
  gargoyle::init("qnr_saved_educ")
  gargoyle::init("qnr_saved_health")
  gargoyle::init("downloaded_data")
  gargoyle::init("done_validate")
  gargoyle::init("done_edit")
  gargoyle::init("done_reject")
  gargoyle::init("done_report")


  # ============================================================================
  # load module server logic
  # ============================================================================

  mod_1_choose_server(
    id = "1_choose_1",
    list = list,
    info = info
  )
  mod_2_setup_server(
    id = "2_setup_1",
    info = info
  )
  mod_3_get_server(
    id = "3_get_1",
    info = info
  )
  mod_4_validate_server(
    id = "4_validate_1",
    info = info
  )
  mod_5_report_server(
    id = "5_report_1",
    info = info
  )

  # ============================================================================
  # initialize UI
  # ============================================================================

  disable_navbar_element(id = "setup")
  disable_navbar_element(id = "get")
  disable_navbar_element(id = "validate")
  disable_navbar_element(id = "report")


  # ============================================================================
  # manage tab state and selection
  # ============================================================================

  gargoyle::on("load_project", {

    enable_navbar_element(id = "setup")
    enable_navbar_element(id = "get")
    enable_navbar_element(id = "validate")
    enable_navbar_element(id = "report")

  })

  gargoyle::on("qnr_saved_health", {

    bslib::nav_select(id = "navbar", selected = "get")

  })

}
