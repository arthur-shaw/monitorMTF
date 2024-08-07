#' Construct user app data directory path
#'
#' @description
#' Consists of three pieces of information:
#'
#' - System location
#' - App name
#' - App version
#'
#' @importFrom rappdirs user_data_dir
#' @importFrom desc desc_get
#'
#' @return Character. Path to user's app data directory
#'
#' @noRd
construct_user_data_path <- function() {

  # given that app_sys() will behave differently based on
  # wether or not we are in dev mode, we can use `{desc}`
  # and find the correct DESCRIPTION here, using the
  # internal app_sys()
  app_description_path <- app_sys("DESCRIPTION")

  dir <- rappdirs::user_data_dir(
    appname = desc::desc_get(
      "Package",
      file = app_description_path
    ),
    appauthor = NULL,
    version = desc::desc_get(
      "Version",
      file = app_description_path
    )
  )

  return(dir)
}

#' Create user application directory for persistent storage
#'
#' @return Character. Path where app directory was created/exists
#'
#' @importFrom fs dir_exists dir_create
#' @importFrom glue glue
#'
#' @noRd
create_user_app_dir <- function() {

  # create user app data directory
  # determine where it should be
  app_dir <- construct_user_data_path()

  # create directory if it doesn't exist
  if (!fs::dir_exists(app_dir)) {
    fs::dir_create(app_dir)
  }

  # confirm that user app data directory exists
  app_dir_exists <- fs::dir_exists(app_dir)
  if (app_dir_exists == TRUE) {
    cat(glue::glue("App directory created at {app_dir}\n"))
    return(app_dir)
  } else {
    stop("App data directory could not be created")
  }
}

#' Enable navbar element
#' 
#' @param id Character. Shiny id given to navbar element.
#' 
#' @return Side-effect of enabling target navbar element.
#' 
#' @importFrom shinyjs enable
#' @importFrom glue glue
#'
#' @noRd
enable_navbar_element <- function(id) {

  shinyjs::enable(selector = glue::glue("a[data-value='{id}']"))

}

#' Disable navbar element
#' 
#' @param id Character. Shiny id given to navbar element.
#' 
#' @return Side-effect of disabling target navbar element.
#' 
#' @importFrom shinyjs enable
#' @importFrom glue glue
#'
#' @noRd
disable_navbar_element <- function(id) {

  shinyjs::disable(selector = glue::glue("a[data-value='{id}']"))

}
