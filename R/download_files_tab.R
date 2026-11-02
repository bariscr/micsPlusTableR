# Internal Shiny module for downloading the public survey materials.
download_files_ui <- function(id, project_dir) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 370,
      shiny::actionButton(ns("refresh"), "Refresh surveys", class = "btn-outline-primary w-100 mics-survey-refresh"),
      shiny::selectizeInput(ns("surveys"), "Select survey folder(s)",
        choices = character(), multiple = TRUE,
        options = list(placeholder = "Choose one or more surveys...")),
      shiny::tags$div(
        class = "d-flex gap-2 mb-3",
        shiny::actionButton(ns("select_all"), "Select all", class = "btn-secondary w-100"),
        shiny::actionButton(ns("deselect_all"), "Deselect all", class = "btn-secondary w-100")
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
        shiny::tags$p("The survey list loads automatically when you open this tab. Choose survey folders, or use Select all, then choose where to save them."),
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
        shiny::uiOutput(ns("status")),
        shiny::tags$script(shiny::HTML("
          if (!window.micsSurveyRefreshReady) {
            window.micsSurveyRefreshReady = true;
            function surveyRefreshState(x) {
              var button = document.getElementById(x.id);
              if (!button) return;
              button.disabled = x.busy;
              button.textContent = x.busy ? 'Loading…' : 'Refresh surveys';
              button.setAttribute('aria-busy', String(x.busy));
            }
            Shiny.addCustomMessageHandler('micsSurveyRefreshState', surveyRefreshState);
            $(document).on('shiny:inputchanged.micsSurveyRefresh', function(event) {
              var button = document.getElementById(event.name);
              if (event.value > 0 && button && button.classList.contains('mics-survey-refresh')) {
                surveyRefreshState({id: event.name, busy: true});
              }
            });
          }
        "))
      ),
      bslib::card(
        bslib::card_header("After downloading"),
        shiny::tags$p("Open Data Preparation and select the downloaded tabulation plan (Excel), which includes its Preparation File sheet, together with your separately obtained household and household-member data. Select a preparation folder only when FIES helpers are needed."),
        shiny::tags$a("Alternative: download all plans and preparation files as a ZIP",
                      href = "https://github.com/bariscr/micsPlusTableR-Files/archive/refs/heads/main.zip",
                      target = "_blank", rel = "noopener")
      )
    )
  )
}

download_files_server <- function(id, project_dir, active = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    catalogue <- shiny::reactiveVal(NULL)
    notice <- shiny::reactiveVal(list(kind = "info", message = "The survey list loads when you open Download Files."))
    loading <- shiny::reactiveVal(FALSE)
    diagnostics <- shiny::reactiveVal(NULL)
    cancel_listing <- function() NULL
    closed <- FALSE
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

    apply_catalogue <- function(available) {
      catalogue(available)
      choices <- unique(available$survey)
      shiny::updateSelectizeInput(session, "surveys",
        choices = stats::setNames(choices, survey_files_labels(choices)),
        selected = intersect(input$surveys, choices))
    }
    set_busy <- function(value) {
      loading(value)
      session$sendCustomMessage("micsSurveyRefreshState", list(id = session$ns("refresh"), busy = value))
    }
    domain <- function(code) shiny::withReactiveDomain(session, shiny::isolate(code))
    start_listing <- function(force = FALSE) {
      if (closed || loading()) return(invisible(NULL))
      cached <- survey_catalogue_cached()
      if (!is.null(cached)) {
        apply_catalogue(cached$data)
        if (!force && as.numeric(difftime(Sys.time(), cached$time, units = "secs")) < 300) {
          notice(list(kind = "info", message = paste("Loaded", length(unique(cached$data$survey)),
            "survey folders. Choose your selection and download folder.")))
          return(invisible(NULL))
        }
      }
      set_busy(TRUE)
      diagnostics(NULL)
      notice(list(kind = "info", message = "Loading available surveys…"))
      cancel_listing <<- survey_catalogue_load(
        on_progress = function(message) domain({
          if (!closed) notice(list(kind = "info", message = message))
        }),
        on_success = function(available) domain({
          if (closed) return(invisible(NULL))
          survey_catalogue_remember(available)
          apply_catalogue(available)
          diagnostics(attr(available, "catalogue_diagnostics"))
          notice(list(kind = "info", message = paste("Loaded", length(unique(available$survey)),
            "survey folders. Choose your selection and download folder.")))
          set_busy(FALSE)
        }),
        on_error = function(message, records) domain({
          if (closed) return(invisible(NULL))
          diagnostics(records)
          retained <- !is.null(catalogue())
          notice(list(kind = if (retained) "warning" else "danger",
            message = paste(message, if (retained) "The previously loaded list is still available." else ""),
            details = survey_catalogue_diagnostics(records)))
          set_busy(FALSE)
        }))
      invisible(NULL)
    }
    shiny::observeEvent(active(), {
      if (isTRUE(active())) start_listing()
    }, ignoreInit = FALSE)
    shiny::observeEvent(input$refresh, start_listing(force = TRUE))
    session$onSessionEnded(function() {
      closed <<- TRUE
      cancel_listing()
    })
    shiny::observeEvent(input$select_all, {
      available <- catalogue()
      if (is.null(available)) {
        shiny::showNotification(if (loading()) "The survey list is still loading." else
          "Click Refresh surveys to load the available folders.", type = "message")
        return()
      }
      shiny::updateSelectizeInput(session, "surveys", selected = unique(available$survey))
    })
    shiny::observeEvent(input$deselect_all, {
      shiny::updateSelectizeInput(session, "surveys", selected = character())
    })

    selected_files <- shiny::reactive({
      available <- catalogue()
      if (is.null(available)) return(NULL)
      available[available$survey %in% input$surveys, , drop = FALSE]
    })
    output$selection_summary <- shiny::renderText({
      selected <- selected_files()
      if (is.null(selected)) return("No survey list loaded yet.")
      if (!nrow(selected)) return("Choose at least one survey, or use Select all.")
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
        role = "status", `aria-live` = "polite", `aria-busy` = tolower(as.character(loading())),
        if (loading()) shiny::tags$span(class = "spinner-border spinner-border-sm me-2", `aria-hidden` = "true"),
        value$message,
        if (!is.null(value$details)) shiny::tags$details(
          shiny::tags$summary("Technical details"), shiny::tags$pre(value$details)))
    })

    shiny::observeEvent(input$download, {
      tryCatch({
        selected <- selected_files()
        if (is.null(selected)) stop("Wait for the survey list to load, or click Refresh surveys to retry.", call. = FALSE)
        if (!nrow(selected)) stop("Choose at least one survey, or use Select all.", call. = FALSE)
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
