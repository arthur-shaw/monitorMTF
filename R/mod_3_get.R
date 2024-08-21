#' 3_get UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_get_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::actionButton(
        inputId = ns("fetch_data"),
        label = "Fetch data"
      ),
      bslib::accordion(
        id = ns("data_downloads"),
        bslib::accordion_panel(
          title = "Download data",
          value = "data_panel",
          # TODO: add text providing date of last download
          # consider storing in R6 object to avoid inspecting files for date
          # TODO: adopt regular flow layout where all icons are in a column
          # or find grid alternative with bslib's labout functions
          shiny::downloadButton(
            outputId = ns("hhold"),
            label = "Household"
          ),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(
            outputId = ns("comm"),
            label = "Community"
          ),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(
            outputId = ns("educ"),
            label = "Education facility"
          ),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(
            outputId = ns("health"),
            label = "Health facility"
          ),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(
            outputId = ns("teams"),
            label = "Team composition"
          )
        )
      )

    )

  )
}
    
#' 3_get Server Functions
#'
#' @noRd 
mod_3_get_server <- function(id, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    gargoyle::on("load_project", {

      # if data never downloaded, disable buttons
      if (is.null(info$downloaded_data)) {

        # disable download buttons
        shinyjs::disable(id = "hhold")
        shinyjs::disable(id = "comm")
        shinyjs::disable(id = "educ")
        shinyjs::disable(id = "health")
        shinyjs::disable(id = "teams")

        # close accordion panel
        bslib::accordion_panel_close(
          id = "data_downloads",
          value = "data_panel"
        )


      } else if (!is.null(info$downloaded_data)) {

        # enable download buttons
        shinyjs::enable(id = "hhold")
        shinyjs::enable(id = "comm")
        shinyjs::enable(id = "educ")
        shinyjs::enable(id = "health")
        shinyjs::enable(id = "teams")

        # close accordion panel
        bslib::accordion_panel_close(
          id = "data_downloads",
          values = "data_panel"
        )

      }

    })

    # ==========================================================================
    # react to fetch download button
    # ==========================================================================

    shiny::observeEvent(input$fetch_data, {

      # ------------------------------------------------------------------------
      # Show waiter
      # ------------------------------------------------------------------------

      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Starting data download process")
        )
      )
      Sys.sleep(1)

      # ------------------------------------------------------------------------
      # Prepare file system
      # ------------------------------------------------------------------------

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # create directories to hold data
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      dirs <- create_data_dirs(proj_dir = info$proj_dir)

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # delete stale files from previous session
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      # meta
      delete_stale_data(dir = dirs$teams)

      # household
      delete_stale_data(dir = dirs$hhold_dl)
      delete_stale_data(dir = dirs$hhold_c)

      # community
      delete_stale_data(dir = dirs$comm_dl)
      delete_stale_data(dir = dirs$comm_c)

      # education facility
      delete_stale_data(dir = dirs$educ_dl)
      delete_stale_data(dir = dirs$educ_c)

      # health facility
      delete_stale_data(dir = dirs$health_dl)
      delete_stale_data(dir = dirs$health_c)

      # ------------------------------------------------------------------------
      # Provision data
      # ------------------------------------------------------------------------

      # inform the user that microdata are being fetched
      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Fetching data")
        )
      )

      # household
      provision_data(
        r6_obj = info,
        qnr_type = "household",
        qnr_name = "hhold",
        qnr_dirs = dirs
      )

      # community
      provision_data(
        r6_obj = info,
        qnr_type = "community",
        qnr_name = "comm",
        qnr_dirs = dirs
      )

      # education facility
      provision_data(
        r6_obj = info,
        qnr_type = "education facility",
        qnr_name = "educ",
        qnr_dirs = dirs
      )

      # health facility
      provision_data(
        r6_obj = info,
        qnr_type = "health facility",
        qnr_name = "health",
        qnr_dirs = dirs
      )

      # team composition
      waiter::waiter_show(html = shiny::tagList(
        waiter::spin_ring(),
        shiny::h4("Downloading team composition data")
      ))
      get_team_composition(
        dir = dirs$teams,
        server = info$server,
        workspace = info$workspace,
        user = info$user,
        password = info$password
      )

      # ------------------------------------------------------------------------
      # signal that process is complete
      # ------------------------------------------------------------------------

      # signal that download process is complete
      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Data download complete")
        )
      )

      # change flag about whether data downloaded
      info$downloaded_data <- TRUE

      # set paths to data directories
      info$data_dir_teams <- dirs$teams
      info$data_dir_hhold_c <- dirs$hhold_c
      info$data_dir_comm_c <- dirs$comm_c
      info$data_dir_educ_c <- dirs$educ_c
      info$data_dir_health_c <- dirs$health_c

      # write updated info to disk
      info$write()

      # hide the waiter
      waiter::waiter_hide()

      # ========================================================================
      # update UI elements
      # ========================================================================

      # in a parent module
      # indirectly, by signal that data downloaded
      gargoyle::trigger("downloaded_data")

      # in this module
      # open accordion panel of data to download
      shinyjs::enable(id = "hhold")
      shinyjs::enable(id = "comm")
      shinyjs::enable(id = "educ")
      shinyjs::enable(id = "health")
      shinyjs::enable(id = "teams")

      bslib::accordion_panel_open(
        id = "data_downloads",
        values = "data_panel"
      )


    })
  
  # ============================================================================
  # react to download buttons
  # ============================================================================

  output$hhold <- shiny::downloadHandler(
    filename = "hhold.zip",
    content = function(file) {

      # zip file path
      zip_file_path <- fs::path(info$data_dir_hhold_c, "hhold.zip")

      # files to zip
      files_to_zip <- fs::dir_ls(
        path = info$data_dir_hhold_c,
        type = "file",
        regexp = "\\.dta"
      )

      # zip data files
      zip::zipr(
        zipfile = zip_file_path,
        files = files_to_zip
      )

      # serve them up
      fs::file_copy(
        path = zip_file_path,
        new_path = file
      )

    }

  )

  output$comm <- shiny::downloadHandler(
    filename = "community.zip",
    content = function(file) {

      # zip file path
      zip_file_path <- fs::path(info$data_dir_comm_c, "community.zip")

      # files to zip
      files_to_zip <- fs::dir_ls(
        path = info$data_dir_comm_c,
        type = "file",
        regexp = "\\.dta"
      )

      # prepare zip data file
      zip::zipr(
        zipfile = zip_file_path,
        files = files_to_zip
      )

      # serve it up
      fs::file_copy(
        path = zip_file_path,
        new_path = file
      )

    }

  )


  output$educ <- shiny::downloadHandler(
    filename = "education.zip",
    content = function(file) {

      # zip file path
      zip_file_path <- fs::path(info$data_dir_educ_c, "education.zip")

      # files to zip
      files_to_zip <- fs::dir_ls(
        path = data_dirs$educ_c,
        type = "file",
        regexp = "\\.dta"
      )

      # prepare zip data file
      zip::zipr(
        zipfile = zip_file_path,
        files = files_to_zip
      )

      # serve them up
      fs::file_copy(
        path = zip_file_path,
        new_path = file
      )

    }

  )

  output$educ <- shiny::downloadHandler(
    filename = "health.zip",
    content = function(file) {

      # zip file path
      zip_file_path <- fs::path(info$data_dir_health_c, "health.zip")

      # files to zip
      files_to_zip <- fs::dir_ls(
        path = info$data_dir_health_c,
        type = "file",
        regexp = "\\.dta"
      )

      # prepare zip data file
      zip::zipr(
        zipfile = zip_file_path,
        files = files_to_zip
      )

      # serve them up
      fs::file_copy(
        path = zip_file_path,
        new_path = file
      )

    }

  )

  output$teams <- shiny::downloadHandler(
    filename = "hhold.zip",
    content = function(file) {

      file_path <- fs::path(info$data_dir_teams, "team_composition.dta")

      # serve up single file 
      fs::file_copy(
        path = file_path,
        new_path = file
      )

    }

  )

  })
}
    
## To be copied in the UI
# mod_3_get_ui("3_get_1")
    
## To be copied in the server
# mod_3_get_server("3_get_1")
