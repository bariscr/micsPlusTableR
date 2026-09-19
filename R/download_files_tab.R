# Internal Shiny module for downloading the public survey materials.
download_files_ui <- function(id, project_dir) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 370,
      shiny::actionButton(ns("refresh"), "Load / refresh surveys", class = "btn-outline-primary w-100"),
      shiny::radioButtons(ns("scope"), "Which survey folders?",
                          choices = c("Selected surveys" = "selected", "All surveys" = "all")),
      shiny::conditionalPanel(
        condition = "input.scope === 'selected'", ns = ns,
        shiny::checkboxGroupInput(ns("surveys"), "Choose one or more surveys", choices = character())
      ),
      shiny::textInput(ns("destination"), "Download folder", value = project_dir),
      shiny::tags$div(
        class = "d-flex flex-wrap gap-2 mb-2",
        shinyFiles::shinyDirButton(ns("browse"), "Browse folders...", "Choose the download folder"),
        shiny::actionButton(ns("use_project"), "Use current project")
      ),
      shiny::helpText("Each survey gets its own subfolder in this location."),
      shiny::checkboxInput(ns("overwrite"), "Replace existing files", value = FALSE),
      shiny::actionButton(ns("download"), "Download files", class = "btn-primary w-100")
    ),
    bslib::layout_columns(
      col_widths = 12,
      bslib::card(
        bslib::card_header("Download tabulation plans and preparation files"),
        shiny::tags$p("Load the available surveys, choose one or more folders (or all surveys), then choose where to save them."),
        shiny::tags$p(
          "Household and household-member data are obtained separately from the ",
          shiny::tags$a("MICS Plus website", href = "https://mics.unicef.org/surveys",
                        target = "_blank", rel = "noopener"),
          " with the required permission. They are not included in these downloads."
        ),
        shiny::tags$p(class = "mb-0", "Current project: ",
                      shiny::tags$code(class = "text-break", project_dir))
      ),
      bslib::card(
        bslib::card_header("Selected survey folders"),
        shiny::textOutput(ns("selection_summary")),
        shiny::tableOutput(ns("preview")),
        shiny::uiOutput(ns("status"))
      ),
      bslib::card(
        bslib::card_header("After downloading"),
        shiny::tags$p("Open Data Preparation and select the downloaded tabulation plan (Excel) and complete preparation folder, together with your separately obtained household and household-member data."),
        shiny::tags$a("Alternative: download all plans and preparation files as a ZIP",
                      href = "https://github.com/bariscr/micsPlusTableR-Files/archive/refs/heads/main.zip",
                      target = "_blank", rel = "noopener")
      )
    )
  )
}

download_files_server <- function(id, project_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    catalogue <- shiny::reactiveVal(NULL)
    notice <- shiny::reactiveVal(list(kind = "info", message = "Click Load / refresh surveys to see the published materials."))
    roots <- c(Project = project_dir, Home = path.expand("~"))
    if (.Platform$OS.type == "windows") roots <- c(roots, shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "browse", roots = roots, session = session,
                               defaultRoot = "Project", allowDirCreate = TRUE)

    shiny::observeEvent(input$browse, {
      path <- shinyFiles::parseDirPath(roots, input$browse)
      if (length(path) == 1L && nzchar(path)) {
        shiny::updateTextInput(session, "destination", value = path)
      }
    })
    shiny::observeEvent(input$use_project, {
      shiny::updateTextInput(session, "destination", value = project_dir)
    })

    shiny::observeEvent(input$refresh, {
      tryCatch({
        available <- shiny::withProgress(message = "Loading available surveys", value = 0.5, {
          list_survey_files()
        })
        if (!nrow(available)) stop("No plans or preparation files are published yet.", call. = FALSE)
        catalogue(available)
        choices <- unique(available$survey)
        shiny::updateCheckboxGroupInput(
          session, "surveys",
          choices = stats::setNames(choices, survey_files_labels(choices)),
          selected = intersect(input$surveys, choices)
        )
        notice(list(kind = "info", message = paste("Loaded", length(choices), "survey folders. Choose your selection and download folder.")))
      }, error = function(e) {
        catalogue(NULL)
        shiny::updateCheckboxGroupInput(session, "surveys", choices = character())
        notice(list(kind = "danger", message = conditionMessage(e)))
      })
    })

    selected_files <- shiny::reactive({
      available <- catalogue()
      if (is.null(available)) return(NULL)
      if (identical(input$scope, "all")) return(available)
      available[available$survey %in% input$surveys, , drop = FALSE]
    })
    output$selection_summary <- shiny::renderText({
      selected <- selected_files()
      if (is.null(selected)) return("No survey list loaded yet.")
      if (!nrow(selected)) return("Choose at least one survey, or select All surveys.")
      sprintf("%d survey folders, %d files (%.2f MB).", length(unique(selected$survey)),
              nrow(selected), sum(selected$size) / 1024^2)
    })
    output$preview <- shiny::renderTable({
      selected <- selected_files()
      if (is.null(selected) || !nrow(selected)) return(NULL)
      ids <- unique(selected$survey)
      data.frame(
        Survey = survey_files_labels(ids),
        Files = vapply(ids, function(id) sum(selected$survey == id), integer(1)),
        `Size (MB)` = vapply(ids, function(id) sum(selected$size[selected$survey == id]) / 1024^2, numeric(1)),
        check.names = FALSE
      )
    }, digits = 2, striped = TRUE, bordered = FALSE, spacing = "s")
    output$status <- shiny::renderUI({
      value <- notice()
      shiny::tags$div(class = paste("alert", paste0("alert-", value$kind), "mt-3 mb-0 text-break"),
                      role = "status", `aria-live` = "polite", value$message)
    })

    shiny::observeEvent(input$download, {
      tryCatch({
        selected <- selected_files()
        if (is.null(selected)) stop("Load the available surveys first.", call. = FALSE)
        if (!nrow(selected)) stop("Choose at least one survey, or select All surveys.", call. = FALSE)
        destination <- resolve_survey_download_dir(input$destination, project_dir)
        # Use the displayed catalogue's commit so the selection cannot change
        # between previewing the files and downloading them.
        commit <- attr(catalogue(), "commit")
        downloaded <- shiny::withProgress(message = "Downloading survey files", value = 0.5, {
          download_survey_files(
            surveys = unique(selected$survey), dest_dir = destination,
            overwrite = isTRUE(input$overwrite), quiet = TRUE, ref = commit
          )
        })
        notice(list(kind = "success", message = paste0(
          "Downloaded ", nrow(downloaded), " files for ",
          paste(survey_files_labels(unique(downloaded$survey)), collapse = ", "), " to ", destination,
          ". Open Data Preparation to select your files."
        )))
      }, error = function(e) {
        notice(list(kind = "danger", message = conditionMessage(e)))
      })
    })
  })
}

resolve_survey_download_dir <- function(path, project_dir) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    stop("Choose a download folder or click Use current project.", call. = FALSE)
  }
  path <- path.expand(trimws(path))
  absolute <- startsWith(path, "/") || startsWith(path, "\\\\") ||
    grepl("^[A-Za-z]:[/\\\\]", path)
  if (!absolute) path <- file.path(project_dir, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}
