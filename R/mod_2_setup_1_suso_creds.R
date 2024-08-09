#' 2_setup_1_suso_creds UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_setup_1_suso_creds_ui <- function(id){
  ns <- NS(id)
  tagList(

     shiny::textInput(
      inputId = ns("server"),
      label = shiny::tags$p(
        "Server URL",
        bsicons::bs_icon("browser-chrome")
      )
    ),
    shiny::textInput(
      inputId = ns("workspace"),
      label = shiny::tags$p(
        "Workspace",
        bsicons::bs_icon("diagram-3-fill")
      ),
    ),
    shiny::textInput(
      inputId = ns("user"),
      label = shiny::tags$p(
        "API user name",
        fontawesome::fa(name = "user-shield")
      )
    ),
    shiny::passwordInput(
      inputId = ns("password"),
      label = shiny::tags$p(
        "API user's password",
        bsicons::bs_icon("unlock-fill")
      )
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )
  )
}
    
#' 2_setup_1_suso_creds Server Functions
#'
#' @noRd 
mod_2_setup_1_suso_creds_server <- function(id, parent, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    gargoyle::on("load_project", {

      if (!is.null(info$suso_creds_provided)) {

        if (info$suso_creds_provided == TRUE) {

          shiny::updateTextInput(
            inputId = "server",
            value = info$server
          )
          shiny::updateTextInput(
            inputId = "workspace",
            value = info$workspace
          )
          shiny::updateTextInput(
            inputId = "user",
            value = info$user
          )
          shiny::updateTextInput(
            inputId = "password",
            value = info$password
          )

        }

      }

    })

    # ==========================================================================
    # react to save button
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # check all inputs provided
      all_creds_provided <- shiny::reactive({
        !all(
          input$server    == "",
          input$workspace == "",
          input$user      == "",
          input$password  == ""
        )
      })

      if (all_creds_provided() == FALSE) {

        shinyFeedback::showToast(
          type = "error",
          title = "Missing input",
          message = "One or more component of the server credentials is missing"
        )

      } else if (all_creds_provided() == TRUE) {

        # set credentials
        susoapi::set_credentials(
          server = input$server,
          workspace = input$workspace,
          user = input$user,
          password = input$password
        )

        # check credentials are valid
        creds_ok <- shiny::reactive({
          susoapi::check_credentials(
            # server = input$server,
            # workspace = input$workspace,
            # user = input$user,
            # password = input$password,
            verbose = TRUE
          )
        })

        # react to validity of credentials
        if (creds_ok()) {

          # write credentials to info
          info$server               <- input$server
          info$workspace            <- input$workspace
          info$user                 <- input$user
          info$password             <- input$password
          info$suso_creds_provided  <- TRUE

          # save credentials to local storage
          info$write()

          # send signal that credentials saved
          gargoyle::trigger("save_creds")

        } else {

          # inform the user credentials invalid
          shinyFeedback::showToast(
            type = "error",
            message = "Server credentials invalid"
          )

        }

      }

    })
  })
}
    
## To be copied in the UI
# mod_2_setup_1_suso_creds_ui("2_setup_1_suso_creds_1")
    
## To be copied in the server
# mod_2_setup_1_suso_creds_server("2_setup_1_suso_creds_1")
