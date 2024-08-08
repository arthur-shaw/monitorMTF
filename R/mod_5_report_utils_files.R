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
