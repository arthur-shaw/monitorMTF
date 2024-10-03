#' Identify cases to review
#'
#' @param proj_dir Character. Path to the project directory.
#' @param hhold_name Character. Name of the main household hhold data file.
#' Without extension. Expected to come from `info$qnr_var_hhold`.
#' 
#' @return Data frame. Cases to review, with the following columns:
#'
#' - `interview__id`
#' - `interview__key`
#' - `interview_complete`
#' 
#' @importFrom fs path
#' @importFrom haven read_dta write_dta
#' @importFrom dplyr filter mutate select
identify_cases_to_review <- function(
  proj_dir,
  hhold_name,
  statuses_to_validate
) {

  file_path <- fs::path(
      proj_dir, "01_data", "01_hhold", "02_combined",
      paste0(hhold_name, ".dta")
    )

  cases <- file_path |>
    haven::read_dta() |>
    # filter to complete interviews
    # by questionnaire content
    dplyr::filter(
      # found household
      SEC_Cov_Q01 == 1 &
      # have eligible, adult repsondent
      SEC_Cov_Q03 == 1 &
      # have consent
      SEC_Cov_Q05 == 1
    ) |>
    # by statuses to validate
    dplyr::filter(interview__status %in% statuses_to_validate) |>
    dplyr::mutate(interview_complete = 1) |>
    dplyr::select(
      interview__id, interview__key,
      interview_complete, interview__status
    )

  haven::write_dta(
    data = cases,
    path = fs::path(
      proj_dir, "02_decisions", "01_data", "01_hhold", "cases_to_review.dta"
    )
  )

  return(cases)

}

#' Create directories for the validation module's storage
#'
#' @param proj_dir Character. Path to the validaiton module's root.
#'
#' @return List containing named paths.
#'
#' @importFrom fs path dir_create
create_validation_dirs  <- function(proj_dir) {

  root_dir <- fs::path(proj_dir, "02_decisions")

  # data
  data_dir <- fs::path(root_dir, "01_data", "01_hhold")
  fs::dir_create(path = data_dir)

  # recommendations
  recommend_dir <- fs::path(root_dir, "02_recommendations", "01_hhold")
  fs::dir_create(path = recommend_dir)

  # decisions
  decide_dir <- fs::path(root_dir, "03_decisions", "01_hhold")
  fs::dir_create(path = decide_dir)

  # reports
  report_dir <- fs::path(root_dir, "04_reports", "01_hhold")
  fs::dir_create(path = report_dir)

  # collect paths to created directories
  dirs <- list(
    # data
    data_hh = data_dir,
    # recommend
    recommend_hh = recommend_dir,
    # decide
    decide_hh = decide_dir,
    # report
    report_hh = decide_dir
  )

  return(dirs)

}

write_df_to_disk <- function(
  df,
  dir
) {

  df_name <- base::substitute(df, env = rlang::current_env()) |>
    base::deparse()

  haven::write_dta(
    data = df,
    path = fs::path(dir, paste0(df_name, ".dta"))
  )

}

#' Write data frame list element to disk
#' 
#' @param df_list List of data frames
#' @param df_name Character. Name of entry in list containing the target df.
#' @param dir Character. Directory where data should be written.
#'
#' @importFrom haven write_data
write_list_el_to_disk <- function(df_list, df_name, dir) {

  df <- df_list[[df_name]]

  haven::write_dta(
    data= df,
    path = fs::path(
      dir, paste0(df_name, ".dta")
    )
  )

}

#' Write all elements of data fram list to disk
#' 
#' @inheritParams write_list_el_to_disk
#'
#' @param purrr walk
write_df_list_to_disk <- function(df_list, dir) {

  # capture the names of data frame entries in list
  list_names  <- names(df_list)

  # iternatively save entries to disk
  purrr::walk(
    .x = list_names,
    .f = ~ write_list_el_to_disk(
      df_list = df_list,
      df_name = .x,
      dir = dir
    )

  )
}