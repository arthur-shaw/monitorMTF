#' Persistent storage of each project's information for app usage
#'
#' @field proj_dir Character. Full path to the project's directory.
#' @field file_name Character. Name of file where project details are stored.
#'
#' @importFrom R6 R6Class
#'
#' @noRd
info <- R6::R6Class(
  classname = "info",
  public = list(
    # project metadata
    proj_name = NULL,
    proj_dir = NULL,
    file_name = "project_info.rds",
    # suso server details
    server = NULL,
    workspace = NULL,
    user = NULL,
    password = NULL,
    suso_creds_provided = NULL,
    # qnrs
    # household
    qnr_use_hhold = NULL,
    qnr_txt_hhold = NULL,
    qnr_txt_provided_hhold = NULL,
    qnrs_hhold = NULL,
    qnr_var_hhold = NULL,
    # community
    qnr_use_comm = NULL,
    qnr_txt_comm = NULL,
    qnr_txt_provided_comm = NULL,
    qnrs_comm = NULL,
    qnr_var_comm = NULL,
    # education facility
    qnr_use_educ = NULL,
    qnr_txt_educ = NULL,
    qnr_txt_provided_educ = NULL,
    qnrs_educ = NULL,
    qnr_var_educ = NULL,
    # health facility
    qnr_use_health = NULL,
    qnr_txt_health = NULL,
    qnr_txt_provided_health = NULL,
    qnrs_health = NULL,
    qnr_var_health = NULL,
    # data downloded
    downloaded_data = NULL,
    # data paths
    data_dir_teams = NULL,
    data_dir_hhold_c = NULL,
    data_dir_comm_c = NULL,
    data_dir_educ_c = NULL,
    data_dir_health_c = NULL,

    #' @description
    #' Read past R6 values from disk
    #'
    #' Perform the following tasks:
    #'
    #' - Read RDS on disk
    #' - Populate R6 fields with values from RDS
    #'
    #' @param path Character. Path to the RDS file containing R6 values.
    read = function(
      path = fs::path(self[["proj_dir"]], self[["file_name"]])
    ) {

      # read setup file from disk
      input_file <- readRDS(path)

      # collect names of fields in setup file
      fields <- names(input_file)

      # populate the R6 object with the corresponding setup file value
      # data frame fields need to be extracted from a list
      # "scalar" fields can be extracted directly
      for (field in fields) {
        field_type <- typeof(input_file[[field]])
        if (field_type == "list") {
          self[[field]] <- input_file[[field]][[1]]
        } else {
          self[[field]] <- input_file[[field]]
        }
      }

    },
    #' @description
    #' Write R6 container to disk as RDS file
    #'
    #' Write all R6 fields to a single RDS file, from which they can be
    #' "restored" with the `read()` method above
    #'
    #' @param path Character. Path where RDS files should be written
    write = function(
      path = fs::path(self[["proj_dir"]], self[["file_name"]])
    ) {

      # collect names of the fields to write

      # data frame fields
      df_fields <- c(
        "qnrs_hhold",
        "qnrs_comm",
        "qnrs_educ",
        "qnrs_health"
      )

      # vector fields
      vctr_fields <- c(
      )

      # "scalar" fields
      # introspect to obtain vector fields and methods
      fields <-  names(self)

      # remove system components and methods
      fields <- fields[
        ! fields %in% c(
          # system components
          ".__enclos_env__", "clone",
          # methods
          "write", "read",
          # omitting data frames and vectors
          df_fields, vctr_fields
        )
      ]

      # put fields in parameter data frame
      # create empty 1-row data frame
      df <- tibble::tibble(.rows = 1)

      # iternatively populate it with the value of all non-df fields
      for (field in fields) {
        df[[field]] <- self[[field]]
      }

      # change the order since the data is being saved in reverse order
      # from the latest field saved to the earliest

      order <- seq(ncol(df), 1, -1)
      df <- df[, order]

      # put data frames and vectors in parameter data frame
      non_atomic_fields <- c(df_fields, vctr_fields)

      for (non_atomic_field in non_atomic_fields) {
        df[[non_atomic_field]] <- list(self[[non_atomic_field]])
      }

      # write data frame to disk
      saveRDS(
        object = df,
        file = path
      )

    }

  )
)
