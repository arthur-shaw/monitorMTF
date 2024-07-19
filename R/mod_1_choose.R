#' 1_choose UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_choose_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion(
      id = ns("choose_action"),
      # option 1: create a new project
      bslib::accordion_panel(
        title = "Create a new project",
        value = "create_new",
        shiny::textInput(
          inputId = ns("name"),
          label = "Provide a short name for your project"
        ),
        shiny::actionButton(
          inputId = ns("create"),
          label = "Create"
        )
      ),
      # option 2: load existing project
      bslib::accordion_panel(
        title = "Select an existing project",
        value = "select_existing",
        shiny::selectInput(
          inputId = ns("project"),
          label = "Select the project",
          choices = NULL
        ),
        shiny::actionButton(
          inputId = ns("load"),
          label = "Load project"
        )
      )
    )

  )
}
    
#' 1_choose Server Functions
#'
#' @noRd 
mod_1_choose_server <- function(id, list, info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize UI
    # ==========================================================================

    if (is.null(list$projects)) {

      # open the create new project accordion
      bslib::accordion_panel_open(
        id = "choose_action",
        values = "create_new"
      )

      # disable UI elements that depend on a non-empty project list
      shinyjs::disable(id = "project")
      shinyjs::disable(id = "load")

    } else if (!is.null(list$projects)) {

      # load the project list
      shiny::updateSelectInput(
        inputId = "project",
        choices = list$projects
      )

      # close the create new accordion
      bslib::accordion_panel_close(
        id = "choose_action",
        values = "create_new"
      )
      # open the select project accordion
      bslib::accordion_panel_open(
        id = "choose_action",
        values = "select_existing"
      )

    }

    # ==========================================================================
    # react to create
    # ==========================================================================

    shiny::observeEvent(input$create, {

      # ------------------------------------------------------------------------
      # validate user inputs
      # ------------------------------------------------------------------------

      # TODO: validations
      # - name not already present
      # - name can be built into a valid path (plus other name rules)

      # ------------------------------------------------------------------------
      # update/save project list and project info R6 objects
      # ------------------------------------------------------------------------

      # capture input in R6
      # append value to project to the list
      list$projects <- c(list$projects, input$name)

      # write R6 to disk
      list$write()

      # create rds file for new project
      new_proj_dir <- fs::path(list$app_dir, input$name)
      fs::dir_create(path = new_proj_dir)
      info$proj_name <- input$name
      info$proj_dir <- new_proj_dir
      info$write()

      # ------------------------------------------------------------------------
      # update UI
      # ------------------------------------------------------------------------

      # enable elements that might have been disabled
      shinyjs::enable(id = "project")
      shinyjs::enable(id = "load")

      # update choices
      shiny::updateSelectInput(
        inputId = "project",
        choices = list$projects
      )

      # ------------------------------------------------------------------------
      # load newly created project
      # ------------------------------------------------------------------------

      # load from disk
      # NOTE: not sure this is necessary, since info is instantiated above
      # info$read(
      #   path = fs::path(list$app_dir, input$name, "project_info.rds")
      # )

      # send signal that project loaded
      gargoyle::trigger("load_project")

    })

    # ==========================================================================
    # react to load
    # ==========================================================================

    shiny::observeEvent(input$load, {

      shiny::req(input$project)

      # read project info from disk
      info$read(
        path = fs::path(list$app_dir, input$project, "project_info.rds")
      )

      # signal that a project has been read from disk
      gargoyle::trigger("load_project")

    })


  })
}
    
## To be copied in the UI
# mod_1_choose_ui("1_choose_1")
    
## To be copied in the server
# mod_1_choose_server("1_choose_1")
