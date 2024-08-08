#' Create directories for the report module's storage
#'
#' @param proj_dir Character. Path to the report module's root.
#'
#' @return List containing named paths.
#'
#' @importFrom fs path dir_create
create_report_dirs  <- function(proj_dir) {

  root_dir <- fs::path(proj_dir, "03_reports")

  # completeness
  completeness_dir <- fs::path(root_dir, "01_completeness")
  fs::dir_create(completeness_dir, recurse = TRUE)
  fs::dir_create(fs::path(completeness_dir, "resources"))

  # quality
  quality_dir <- fs::path(root_dir, "02_quality")
  fs::dir_create(quality_dir, recurse = TRUE)
  fs::dir_create(fs::path(quality_dir, "resources"))

  # collect paths to created directories
  dirs <- list(
    # report root
    report = root_dir,
    # completeness
    completeness = completeness_dir,
    # quality
    quality = quality_dir
  )

  return(dirs)

}

#' Perform Quarto report rendering workflow
#'
#' @description
#' Workflow:
#' - Determine which report template to use
#' - Copy that template, ana associated resources, from the package to the app
#' - Render the report where the template and resources have been copied
#' - Move the rendered report to a user-facing folder
#'
#' @importFrom glue glue
#' @importFrom dplyr case_when
#' @importFrom fs path path_package file_copy file_move
#' @importFrom quarto quarto_render
#'
#' @return Side-effect of producing a rendered report in a certain directory
render_report <- function(
  report_type,
  proj_dir,
  params
) {

  # ============================================================================
  # set paths as a function of project path and report type
  # ============================================================================

  # ----------------------------------------------------------------------------
  # app paths
  # ----------------------------------------------------------------------------

  report_name <- glue::glue("report_{report_type}.qmd")

  report_dir <- dplyr::case_when(
    report_type == "completeness" ~ "01_completeness",
    report_type == "quality" ~ "02_quality",
    TRUE ~ ""
  )

  # top-level report-specific directory
  report_app_dir <- fs::path(
    proj_dir, "03_reports", report_dir
  )

  # where template should be copied
  template_app_path <- fs::path(
    report_app_dir, "resources", report_name
  )

  # ----------------------------------------------------------------------------
  # package paths
  # ----------------------------------------------------------------------------

  pkg_path <- fs::path_package(
    package = "monitorMTF",
    "quarto", "templates"
  )

  template_pkg_path <- fs::path(
    pkg_path, glue::glue("{report_type}"), report_name
  )

  # ============================================================================
  # copy resources from package to app
  # ============================================================================

  fs::file_copy(
    path = template_pkg_path,
    new_path = template_app_path,
    overwrite = TRUE
  )

  # TODO: add files/revise approach as needed

  # ============================================================================
  # render report in situ
  # ============================================================================

  quarto::quarto_render(
    input = template_app_path,
    execute_params = params
  )

  # ============================================================================
  # move report to user-facing report dir
  # ============================================================================

  fs::file_move(
    path = fs::path(
      report_app_dir, "resources", glue::glue("report_{report_type}.html")
    ),
    new_path = fs::path(report_app_dir, glue::glue("report_{report_type}.html"))
  )

}
