
#' Construct paths to data directory and sub-directories
#'
#' @param dir Character. Path to root for a questionnaire.
#'
#' @return Character vector. Paths directories :
#' - Root for a questionnaire
#' - Downloaded data
#' - Combined data
#'
#' @importFrom fs path
#' 
#' @noRd
construct_data_paths <- function(dir) {

  dirs <- c(
    dir,
    fs::path(dir, "01_downloaded"),
    fs::path(dir, "02_combined")
  )

  return(dirs)

}


#' Create data directories on disk
#'
#' Creates required data directories for questionnaires:
#' - Household
#' - Community
#' - Education facility
#' - Health facility
#'
#' @param proj_dir Character. Path for project's root directory.
#'
#' @importFrom fs path dir_create
#'
#' @noRd
create_data_dirs <- function(proj_dir) {

  root_data_dir <- fs::path(proj_dir, "01_data")

  # metadata
  meta_dir <- fs::path(root_data_dir, "00_meta")
  teams_dir <- fs::path(meta_dir, "01_team_composition")
  fs::dir_create(path = c(meta_dir, teams_dir))

  # household
  hhold_root_dir <- fs::path(root_data_dir, "01_hhold")
  hhold_dirs <- construct_data_paths(dir = hhold_root_dir)
  fs::dir_create(path = hhold_dirs)

  # community
  comm_root_dir <- fs::path(root_data_dir, "02_comm")
  comm_dirs <- construct_data_paths(dir = comm_root_dir)
  fs::dir_create(path = comm_dirs)

  # education facility
  educ_root_dir <- fs::path(root_data_dir, "03_educ")
  educ_dirs <- construct_data_paths(dir = educ_root_dir)
  fs::dir_create(path = educ_dirs)

  # health facility
  health_root_dir <- fs::path(root_data_dir, "04_health")
  health_dirs <- construct_data_paths(dir = health_root_dir)
  fs::dir_create(path = health_dirs)

  # collect paths to created directories
  dirs <- list(
    # meta
    teams = teams_dir,
    # hhold
    hhold_r = hhold_dirs[1],
    hhold_dl = hhold_dirs[2],
    hhold_c = hhold_dirs[3],
    # comm
    comm_r = comm_dirs[1],
    comm_dl = comm_dirs[2],
    comm_c = comm_dirs[3],
    # educ
    educ_r = educ_dirs[1],
    educ_dl = educ_dirs[2],
    educ_c = educ_dirs[3],
    # health
    health_r = health_dirs[1],
    health_dl = health_dirs[2],
    health_c = health_dirs[3]
  )

  return(dirs)

}

#' Delete all stale data files inside target directory
#'
#' Delete files, directories, and files in those directories.
#'
#' @param dir Character. Path to target directory
#' 
#' @return Side-effect of deleting stale files.
#'
#' @importFrom fs dir_exists dir_ls dir_delete file_delete
#'
#' @noRd
delete_stale_data <- function(dir) {

  # if folder exists, purge its contents
  if (fs::dir_exists(dir)) {

    # list directories
    directories <- fs::dir_ls(
      path = dir,
      type = "directory",
      recurse = FALSE
    )

    # remove directories, if they exist
    fs::dir_delete(directories)

    # remove all files
    files <- fs::dir_ls(
      path = dir,
      type = "file",
      all = TRUE
    )
    fs::file_delete(files)

  } else {
    print(
      paste0(
        "Diectory not deleted.",
        paste0(dir, " does not exist"),
        collapse = "\n"
      )
    )
  }

}

#' Unpack single zip file to a folder bearing its name
#'
#' Rather than unpack a file to the directory in which the file sits,
#' create a folder with the file's name (minus extension) and
#' unpack its contents there.
#'
#' @param zipfile Character. Full file path of the zip file.
#'
#' @return Side-effect of creating a folder and unpacking zip contents there.
#'
#' @importFrom fs path_dir path_file path_ext_remove
#' @importFrom zip unzip
#'
#' @noRd
unpack_zip_to_dir <- function(zipfile) {

  parent_dir <- fs::path_dir(zipfile)
  file_name <- fs::path_file(zipfile)
  unpack_name <- fs::path_ext_remove(file_name)
  unpack_dir <- fs::path(parent_dir, unpack_name)

  zip::unzip(
    zipfile = zipfile,
    exdir = unpack_dir
  )

}

#' Unpack all zip files found in directory to same-named sub-directory
#'
#' Applies `unpack_zip_to_dir()` to all zip files in directory
#'
#' @param dir Character. Directory where zip files can be found
#'
#' @return Side-effect of creating a folder for each zip and
#' unpacking its contents there.
#'
#' @importFrom fs dir_ls
#' @importFrom purrr walk
#'
#' @noRd
unpack_all_zip_to_dir <- function(dir) {

  # obtain list of zip files
  files <- fs::dir_ls(
    path = dir,
    type = "file",
    regexp = "\\.zip$",
    recurse = FALSE
  )

  # unpack all identified zip files
  purrr::walk(
    .x = files,
    .f = ~ unpack_zip_to_dir(.x)
  )

}

#' Combine and save Stata data files
#'
#' @param file_info_df Data frame. Return value of `fs::file_info()` that
#' contains an additioal column `file_name`.
#' @param name Character. Name of the file (with extension) to ingest from
#' all folders where it is found.
#' @param dir Character. Directory where combined data will be saved.
#'
#' @return Side-effect of creating data frame objects in the global environment
#' with the name `name`.
#'
#' @importFrom rlang `.data`
#' @importFrom dplyr filter pull
#' @importFrom purrr map_dfr
#' @importFrom haven read_dta
#' @importFrom fs path_ext_remove path
#'
#' @noRd
combine_and_save_dta <- function(
  file_info_df,
  name,
  dir
) {

  # file paths
  # so that can locate data files to combine
  file_paths <- file_info_df |>
    dplyr::filter(.data$file_name == name) |>
    dplyr::pull(.data$path)

  # data frame
  # so that can assign this value to a name
  df <- purrr::map_dfr(
    .x = file_paths,
    .f = ~ haven::read_dta(file = .x)
  )

  # save to destination directory
  haven::write_dta(data = df, path = fs::path(dir, name))

}

#' Combine and save all downloaded `.dta` files
#'
#' Compile list of `dta` files. Combine all files of same name
#'
#' @param dir_in Character. Root directory whose sub-directories
#' contain `.dta` files
#' @param dir_out Chracter. Directory where
#'
#' @return Side-effect of combining each
#'
#' @importFrom fs dir_ls path_file
#' @importFrom purrr map_dfr walk
#' @importFrom dplyr mutate distinct pull
#' @importFrom rlang `.data`
#'
#' @noRd
combine_and_save_all_dta <- function(
  dir_in,
  dir_out
) {

  # obtain list of all directories of unpacked zip files
  dirs <- fs::dir_ls(
    path = dir_in,
    type = "directory",
    recurse = FALSE
  )

  # compile list of all Stata files in all directories
  files_df <- dirs |>
    purrr::map_dfr(
      .f = ~ fs::dir_info(
        path = .x,
        recurse = FALSE,
        type = "file",
        regexp = "\\.dta$"
      )
    ) |>
    dplyr::mutate(file_name = fs::path_file(.data$path))

  # extract a list of all unique files found in the directories
  file_names <- files_df |>
    dplyr::distinct(.data$file_name) |>
    dplyr::pull(.data$file_name)

  # combine and save all same-named Stata files
  purrr::walk(
    .x = file_names,
    .f = ~ combine_and_save_dta(
      file_info_df = files_df,
      name = .x,
      dir = dir_out
    )
  )

}

#' Provision data
#'
#' Provisioning involves:
#' - Downloading
#' - Unpacking
#' - Combining
#' - Informing the user throughout the process
#'
#' @param qnr_type Character. Name of questionnaire to be shown to user.
#' @param qnr_name Character. Questionnaire name for identifying attributes
#' in R6.
#' @param qnr_dirs List. Directories for questionnaire type: root,
#' downloaded, combined.
#'
#' @importFrom waiter waiter_show spin_ring
#' @importFrom shiny tagList h4
#' @importFrom dplyr mutate pull
#' @importFrom glue glue
#'
#' @noRd
provision_data <- function(
  r6_obj,
  qnr_type,
  qnr_name,
  qnr_dirs
) {

  # inform the user that microdata are being fetched
  waiter::waiter_show(
    html = shiny::tagList(
      waiter::spin_ring(),
      shiny::h4(glue::glue("Fetching {qnr_type} data"))
    )
  )

  # extract ID and title for questionnaires matching user-provided pattern
  qnr_ids <- r6_obj[[paste0("qnrs_", qnr_name)]] |>
    # compose questionnaire ID expected by `susoflows::download_data()`
    # format: {GUID}${version}
    dplyr::mutate(id = paste0(.data$questionnaireId, "$", .data$version)) |>
    dplyr::pull(.data$id)
  qnr_titles <- r6_obj[[paste0("qnrs_", qnr_name)]] |>
    dplyr::pull(.data$title)

  # prepare to loop over questionnaire IDs
  qnr_tot <- length(qnr_ids)
  qnr_n <- 1

  # download by iterating through each matching questionnaire
  for(qnr_id in qnr_ids) {

    # extract title of current questionnaire
    qnr_title <- qnr_titles[qnr_n]

    # update user on which data is being downloaded
    waiter::waiter_show(html = shiny::tagList(
      waiter::spin_ring(),
      shiny::h4(
        glue::glue("
        Downloading data for questionnaire {qnr_n} of {qnr_tot} :
        {qnr_title}"
        )
      )
    ))

    # download data
    susoflows::download_data(
      qnr_id = qnr_id,
      export_type = "STATA",
      path = qnr_dirs[[paste0(qnr_name, "_dl")]],
      server = r6_obj$server,
      workspace = r6_obj$workspace,
      user = r6_obj$user,
      password = r6_obj$password
    )

    # increment questionnaire counter
    qnr_n <- qnr_n + 1

  }
  # unpack downloaded data
  waiter::waiter_show(html = shiny::tagList(
    waiter::spin_ring(),
    shiny::h4(glue::glue("Unzipping {qnr_type} data"))
  ))
  unpack_all_zip_to_dir(qnr_dirs[[paste0(qnr_name, "_dl")]])

  # combine downloaded data
  waiter::waiter_show(html = shiny::tagList(
    waiter::spin_ring(),
    shiny::h4(glue::glue("Combining {qnr_type} data"))
  ))
  combine_and_save_all_dta(
    dir_in = qnr_dirs[[paste0(qnr_name, "_dl")]],
    dir_out = qnr_dirs[[paste0(qnr_name, "_c")]]
  )

}

#' Get team composition
#'
#' First, fetch composition. Then, write to labelled Stata file.
#'
#' @param dir Character. Directory where data should be stored.
#' @inheritParams susoflows::download_matching
#'
#' @importFrom susoapi get_interviewers
#' @importFrom labelled var_label
#' @importFrom haven write_dta
#' @importFrom fs path
#'
#' @noRd
get_team_composition <- function(
  dir,
  server,
  workspace,
  user,
  password
) {

  # construct team composition
  team_composition <- susoapi::get_interviewers(
    server = server,
    workspace = workspace,
    user = user,
    password = password
  )

  # label columns for easier comprehension
  labelled::var_label(team_composition) <- list(
    UserId = "Interviewer GUID",
    UserName = "Interviewer user name",
    SupervisorId = "Supervisor user GUID",
    SupervisorName = "Supervisor user name",
    Role = "Role: Interviewer, Supervisor"
  )

  # write to disk
  haven::write_dta(
    data = team_composition,
    path = fs::path(dir, "team_composition.dta")
  )

}
