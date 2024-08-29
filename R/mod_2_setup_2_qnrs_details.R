#' 2_setup_2_qnrs_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_setup_2_qnrs_details_ui <- function(id, qnr_txt){
  ns <- NS(id)
  shiny::tagList(

    bslib::input_switch(
      id = ns("use_qnr"),
      label = glue::glue("Uses {qnr_txt} questionnaire"),
      value = FALSE
    ),
    shiny::actionButton(
      inputId = ns("na_next"),
      label = "Next"
    ),
    shiny::textInput(
      inputId = ns("qnr_txt"),
      label = bslib::popover(
        # where popover is invoked
        trigger = list(
          "Provide a string that identifies the questionnaire(s) of interest",
          bsicons::bs_icon("info-circle")
        ),
        # content of popover message
        shiny::p(
          'To do so, provide either a substring (e.g., "Household")',
          ' or a ', 
          htmltools::a(
            'regular expression',
            href = "https://regexlearn.com/"
          ),
          ' (e.g., "[Hh]ousehold")'
        )
      )
    ),
    shiny::actionButton(
      inputId = ns("search"),
      label = "Search"
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    ),
    reactable::reactableOutput(
      outputId = ns("qnrs")
    )

  )
}
    
#' 2_setup_2_qnrs_details Server Functions
#'
#' @noRd 
mod_2_setup_2_qnrs_details_server <- function(id, parent, info, qnr_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    gargoyle::on("load_project", {

      # if parameter not provided (i.e., not present in R6)
      if (is.null(info[[paste0("qnr_txt_provided_", qnr_name)]])) {

        bslib::update_switch(
          id = "use_qnr",
          value = FALSE
        )

        shinyjs::show(id = "na_next")

        shinyjs::hide(id = "qnr_txt")
        shinyjs::hide(id = "search")
        shinyjs::hide(id = "save")

      # otherwise, load from disk
      } else if (!is.null(info[[paste0("qnr_txt_provided_", qnr_name)]])) {

        bslib::update_switch(
          id = "use_qnr",
          value = info[[paste0("qnr_use_", qnr_name)]]
        )

        shiny::updateTextInput(
          inputId = "qnr_txt",
          value = info[[paste0("qnr_txt_", qnr_name)]]
        )

        output$qnrs <- reactable::renderReactable({
          reactable::reactable(
            data = qnrs,
            columns = list(
              questionnaireId = reactable::colDef(show = FALSE)
            )
          )
        })

        # note: questionnaire table is loaded below
        # since updating UI element constitutes an event for the listener below,
        # need to manage content of the table in the event listener

      }

    })

    # ==========================================================================
    # react to using the questionnaire
    # ==========================================================================

    shiny::observeEvent(input$use_qnr, {

      if (input$use_qnr == TRUE) {

        # manage display of UI elements
        shinyjs::hide(id = "na_next")

        shinyjs::show(id = "qnr_txt")
        shinyjs::show(id = "search")
        shinyjs::show(id = "save")

        shinyjs::show(id = "qnrs")

        # populate the questionnaire table
        # if no questionnires not previously set, load all questionnaires
        # otherwise, load past questionnaires
        if (is.null(info[[paste0("qnr_txt_provided_", qnr_name)]])) {

          # fetch all questionnaires
          qnrs <- susoapi::get_questionnaires(
            server = info$server,
            workspace = info$workspace,
            user = info$user,
            password = info$password
          ) |>
            dplyr::select(
              .data$title, .data$version, .data$variable,
              .data$questionnaireId
            )

          # show them
          output$qnrs <- reactable::renderReactable({
            reactable::reactable(
              data = qnrs,
              columns = list(
                questionnaireId = reactable::colDef(show = FALSE)
              )
            )
          })

        } else if (!is.null(info[[paste0("qnr_txt_provided_", qnr_name)]])) {

          output$qnrs <- reactable::renderReactable({
            reactable::reactable(
              data = info[[paste0("qnrs_", qnr_name)]],
              columns = list(
                questionnaireId = reactable::colDef(show = FALSE)
              )
            )
          })

        }

      } else if (input$use_qnr == FALSE) {

        shinyjs::show(id = "na_next")

        shinyjs::hide(id = "qnr_txt")
        shinyjs::hide(id = "search")
        shinyjs::hide(id = "save")

        shinyjs::hide(id = "qnrs")

      }

    },
    ignoreInit = TRUE)

    # ==========================================================================
    # react to search
    # ==========================================================================

    # create a reactive container for the table
    # so that its values are accessible outside of the observeEvent scope
    matching_qnrs <- shiny::reactiveValues(
      df = info[[paste0("qnrs_", qnr_name)]],
      qnr_var = info[[paste0("qnr_var_", qnr_name)]]
    )

    shiny::observeEvent(input$search, {

      matching_qnrs$df <- susoapi::get_questionnaires(
        server = info$server,
        workspace = info$workspace,
        user = info$user,
        password = info$password
      ) |>
        dplyr::filter(
          grepl(
            x = .data$title,
            pattern = input$qnr_txt
          )
        ) |>
        dplyr::select(
          .data$title, .data$version, .data$variable,
          .data$questionnaireId
        )

      matching_qnrs$qnr_var <- matching_qnrs$df |>
        # take the first row, where variable qnr variable is assumed the same
        dplyr::filter(dplyr::row_number() == 1) |>
        # extract the variable column
        dplyr::pull(variable)

      output$qnrs <- reactable::renderReactable({
        reactable::reactable(
          data = matching_qnrs$df,
          columns = list(
            questionnaireId = reactable::colDef(show = FALSE)
          )
        )

      })

    })

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture input in R6
      info[[paste0("qnr_use_", qnr_name)]] <- TRUE
      info[[paste0("qnr_txt_", qnr_name)]] <- input$qnr_txt
      info[[paste0("qnr_txt_provided_", qnr_name)]] <- TRUE
      info[[paste0("qnrs_", qnr_name)]] <- matching_qnrs$df
      info[[paste0("qnr_var_", qnr_name)]] <- matching_qnrs$qnr_var

      # write R6 to disk
      info$write()

      # signal that info saved
      gargoyle::trigger(paste0("qnr_saved_", qnr_name))

    })

    # ==========================================================================
    # react to next
    # ==========================================================================

    shiny::observeEvent(input$na_next, {

      # capture input in R6
      info[[paste0("qnr_use_", qnr_name)]] <- FALSE

      # write R6 to disk
      info$write()

      # signal that info saved
      gargoyle::trigger(paste0("qnr_saved_", qnr_name))

    })


  })
}
    
## To be copied in the UI
# mod_2_setup_2_qnrs_details_ui("2_setup_2_qnrs_details_1")
    
## To be copied in the server
# mod_2_setup_2_qnrs_details_server("2_setup_2_qnrs_details_1")
