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

      bslib::accordion_panel_open(
        id = "data_downloads",
        values = "data_panel"
      )

    })

  })
}
    
## To be copied in the UI
# mod_3_get_ui("3_get_1")
    
## To be copied in the server
# mod_3_get_server("3_get_1")
