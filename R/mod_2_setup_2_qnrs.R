#' 2_setup_1_qnrs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_setup_2_qnrs_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion(
      id = ns("qnr_details"),
      bslib::accordion_panel(
        title = "Household",
        value = "household",
        icon = fontawesome::fa(name = "people-roof"),
        mod_2_setup_2_qnrs_details_ui(
          id = ns("2_setup_2_qnrs_details_hhold"),
          qnr_txt = "household"
        )
      ),
      bslib::accordion_panel(
        title = "Community",
        value = "community",
        icon = bsicons::bs_icon("houses"),
        mod_2_setup_2_qnrs_details_ui(
          id = ns("2_setup_2_qnrs_details_comm"),
          qnr_txt = "community"
        )
      ),
      bslib::accordion_panel(
        title = "Education facility",
        value = "education_facility",
        icon = fontawesome::fa(name = "school"),
        mod_2_setup_2_qnrs_details_ui(
          id = ns("2_setup_2_qnrs_details_educ"),
          qnr_txt = "education facility"
        )
      ),
      bslib::accordion_panel(
        title = "Health facility",
        value = "health_facility",
        icon = bsicons::bs_icon("hospital"),
        mod_2_setup_2_qnrs_details_ui(
          id = ns("2_setup_2_qnrs_details_health"),
          qnr_txt = "health facility"
        )
      ),
      bslib::accordion_panel(
        title = "Enterprise",
        value = "enterprise",
        icon = bsicons::bs_icon("briefcase"),
        mod_2_setup_2_qnrs_details_ui(
          id = ns("2_setup_2_qnrs_details_biz"),
          qnr_txt = "enterprise"
        )
      )
    )
  )
}

#' 2_setup_1_qnrs Server Functions
#'
#' @noRd
mod_2_setup_2_qnrs_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # load definition of child modules
    # ==========================================================================

    mod_2_setup_2_qnrs_details_server(
      id = "2_setup_2_qnrs_details_hhold",
      parent = session,
      info = info,
      qnr_name = "hhold"
    )

    mod_2_setup_2_qnrs_details_server(
      id = "2_setup_2_qnrs_details_comm",
      parent = session,
      info = info,
      qnr_name = "comm"
    )

    mod_2_setup_2_qnrs_details_server(
      id = "2_setup_2_qnrs_details_educ",
      parent = session,
      info = info,
      qnr_name = "educ"
    )

    mod_2_setup_2_qnrs_details_server(
      id = "2_setup_2_qnrs_details_health",
      parent = session,
      info = info,
      qnr_name = "health"
    )

    mod_2_setup_2_qnrs_details_server(
      id = "2_setup_2_qnrs_details_biz",
      parent = session,
      info = info,
      qnr_name = "biz"
    )

    # ==========================================================================
    # manage opening and closing of qnr accordions
    # ==========================================================================

    # from household to community
    gargoyle::on("qnr_saved_hhold", {

      # close household
      bslib::accordion_panel_close(
        id = "qnr_details",
        value = "household"
      )

      # open community
      bslib::accordion_panel_open(
        id = "qnr_details",
        value = "community"
      )

    })

    # from community to education facility
    gargoyle::on("qnr_saved_comm", {

      # close community
      bslib::accordion_panel_close(
        id = "qnr_details",
        value = "community"
      )

      # open education facility
      bslib::accordion_panel_open(
        id = "qnr_details",
        value = "education_facility"
      )

    })

    # from education facility to health facility
    gargoyle::on("qnr_saved_educ", {

      # close education facility
      bslib::accordion_panel_close(
        id = "qnr_details",
        value = "education_facility"
      )

      # open health facility
      bslib::accordion_panel_open(
        id = "qnr_details",
        value = "health_facility"
      )

    })

    # from health facility to enterprise
    gargoyle::on("qnr_saved_health", {

      # close health facility
      bslib::accordion_panel_close(
        id = "qnr_details",
        value = "health_facility"
      )

      # open enterprise
      bslib::accordion_panel_open(
        id = "qnr_details",
        value = "enterprise"
      )

    })

    # from enterprise to get data
    gargoyle::on("qnr_saved_biz", {

      # close enterprise
      bslib::accordion_panel_close(
        id = "qnr_details",
        value = "enterprise"
      )

      # NOTE: get data is handled in app_server.R

    })


  })
}
    
## To be copied in the UI
# mod_2_setup_2_qnrs_ui("2_setup_2_qnrs_1")
    
## To be copied in the server
# mod_2_setup_2_qnrs_server("2_setup_2_qnrs_1")
