# app.R

# Shiny sources app.R in its own environment. Copy the package namespace's
# imported symbols into that environment without attaching packages globally.
micsPlusTableR:::bind_namespace_symbols(environment())

survey_choices <- micsPlusTableR:::read_survey_choices()
project_dir <- normalizePath(
  getOption("micsPlusTableR.project_dir", here::here()),
  winslash = "/", mustWork = TRUE
)

# Serve the HTML guide and printable PDF from their canonical package location.
guide_resource_prefix <- "micsPlusTableR-guide"
guide_directory <- system.file("doc", package = "micsPlusTableR")
if (!dir.exists(guide_directory)) {
  stop("The packaged user's guide directory is missing.", call. = FALSE)
}
registered_paths <- shiny::resourcePaths()
registered_guide <- if (guide_resource_prefix %in% names(registered_paths)) {
  registered_paths[[guide_resource_prefix]]
} else {
  NULL
}
if (is.null(registered_guide)) {
  shiny::addResourcePath(guide_resource_prefix, guide_directory)
} else if (!identical(
  normalizePath(registered_guide, winslash = "/", mustWork = FALSE),
  normalizePath(guide_directory, winslash = "/", mustWork = TRUE)
)) {
  shiny::removeResourcePath(guide_resource_prefix)
  shiny::addResourcePath(guide_resource_prefix, guide_directory)
}

# For setting the number of digits  ------------------------------------------------
options(digits = 17)

# Resolve this once, before the UI and server are created, so the app can show
# the exact destination it will use.
excel_root <- normalizePath(
  getOption("micsPlusTableR.output_dir", micsPlusTableR:::default_output_dir()),
  winslash = "/",
  mustWork = FALSE
)
dir.create(excel_root, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(excel_root)) {
  stop("The Excel output directory could not be created: ", excel_root,
       call. = FALSE)
}

# For dropdown selection of variables in the data exploration and data view tabs ------
# A css is written in addition to the selectInput function to define the ellipsis-select class.
selectInput_ellipsis <- function(inputId, label, choices = NULL, multiple = TRUE,
                                 selectize = FALSE, width = "100%", ...) {
  tagAppendAttributes(
    selectInput(
      inputId = inputId,
      label = label,
      choices = choices,
      multiple = multiple,
      selectize = selectize,
      width = width,
      ...
    ),
    class = "ellipsis-select"
  )
}


# helpers ---------------------------------------------------------------------------
labelled_to_value_label <- function(x) {
  if (!haven::is.labelled(x)) {
    return(x)
  }

  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs) || length(labs) == 0) {
    # labelled but no value labels available -> just return values as character
    vals <- as.character(haven::zap_labels(x))
    vals[is.na(x)] <- NA_character_
    return(vals)
  }

  # labs is a named vector: names(labs) are LABELS, values(labs) are CODES
  code_to_label <- setNames(names(labs), as.character(unname(labs)))

  # underlying values (keep codes)
  vals <- as.character(haven::zap_labels(x))

  # map value -> label
  lbls <- unname(code_to_label[vals])

  # if a value has no label, keep just the value (no " - (NA)")
  out <- ifelse(!is.na(vals) & !is.na(lbls),
    paste0(vals, " - (", lbls, ")"),
    vals
  )

  # keep true missings as NA
  out[is.na(x)] <- NA_character_
  out
}

# use x if not null or empty, otherwise use y
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.character(x) && all(!nzchar(x))) {
    return(y)
  }
  x
}

# variable label helpers ----------------------------------------------------------------
var_label_safe <- function(x) {
  lab <- attr(x, "label", exact = TRUE)
  if (is.null(lab)) "" else as.character(lab)
}

make_var_choices <- function(ds) {
  nms <- names(ds)
  labs <- vapply(nms, function(nm) var_label_safe(ds[[nm]]), character(1))

  shown <- ifelse(nzchar(labs),
    paste0(nms, " - (", labs, ")"),
    nms
  )

  # Shiny selectInput: names are shown, values are returned
  setNames(nms, shown)
}

make_value_label_df <- function(df) {
  out <- df
  nms <- names(out)

  for (nm in nms) {
    x <- out[[nm]]
    if (haven::is.labelled(x)) {
      out[[nm]] <- labelled_to_value_label(x)
    }
  }

  out
}


var_display_map <- function(ds) {
  ch <- make_var_choices(ds) # named: display -> real
  setNames(names(ch), unname(ch)) # real -> display
}

# flextable helper to allow duplicate columns ------------------------------------------
make_flextable_allow_dup_cols <- function(df,
                                          blank_label = "(blank)",
                                          sep = "__dup__") {
  stopifnot(is.data.frame(df))

  hdr <- names(df)

  # 1) Replace NA / empty / whitespace-only with a visible label
  hdr_clean <- hdr
  hdr_clean[is.na(hdr_clean)] <- blank_label
  hdr_clean <- trimws(hdr_clean)
  hdr_clean[hdr_clean == ""] <- blank_label

  # 2) Make unique keys for flextable
  keys <- make.unique(hdr_clean, sep = sep)

  # 3) Rename df to unique keys
  names(df) <- keys

  # 4) Build flextable with unique keys but show original header labels
  ft <- flextable::flextable(df, col_keys = keys)

  # show "blank_label" for the missing headers (and keep duplicates as-is)
  ft <- flextable::set_header_labels(ft, values = stats::setNames(hdr_clean, keys))

  ft
}

# Use Shiny's standard file input markup, but ask the browser for a directory.
# The browser therefore shows the same native picker as the other uploads while
# retaining each uploaded file's path relative to the selected folder.
prep_folder_input <- htmltools::tagQuery(
  fileInput(
    "prep_folder",
    "Select preparation folder",
    multiple = TRUE,
    buttonLabel = "Browse...",
    placeholder = "No folder selected"
  )
)$find("#prep_folder")$addAttrs(
  webkitdirectory = "webkitdirectory",
  directory = "directory"
)$allTags()

# Keep upload staging local to the app. This prevents a running R session with
# an older package namespace from failing when the installed app files have
# already been updated on disk.
stage_preparation_upload_app <- function(uploaded_files,
                                         relative_paths,
                                         stage_dir = tempfile("mics-prep-upload-")) {
  required_columns <- c("name", "datapath")
  if (!is.data.frame(uploaded_files) || !nrow(uploaded_files) ||
      !all(required_columns %in% names(uploaded_files))) {
    stop("Select a preparation folder.", call. = FALSE)
  }

  relative_paths <- as.character(unlist(relative_paths, use.names = FALSE))
  if (length(relative_paths) != nrow(uploaded_files)) {
    stop("The browser did not provide the selected folder structure.",
         call. = FALSE)
  }

  relative_paths <- gsub("\\\\", "/", relative_paths)
  relative_paths <- sub("^\\./", "", relative_paths)
  invalid <- !nzchar(relative_paths) |
    grepl("^/|^[A-Za-z]:/", relative_paths) |
    grepl("(^|/)\\.\\.(/|$)", relative_paths)
  if (any(invalid)) {
    stop("The selected folder contains an unsafe relative path.", call. = FALSE)
  }

  parts <- strsplit(relative_paths, "/", fixed = TRUE)
  has_folder_root <- all(lengths(parts) >= 2L) &&
    length(unique(vapply(parts, `[[`, character(1), 1L))) == 1L
  folder_name <- if (has_folder_root) parts[[1L]][[1L]] else "Preparation folder"
  inside_paths <- if (has_folder_root) {
    vapply(parts, function(x) paste(x[-1L], collapse = "/"), character(1))
  } else {
    relative_paths
  }

  top_level_r <- which(
    dirname(inside_paths) == "." &
      tolower(tools::file_ext(inside_paths)) == "r"
  )
  if (!length(top_level_r)) {
    stop("The selected folder must contain a top-level preparation R file.",
         call. = FALSE)
  }

  prep_like <- top_level_r[
    grepl("prep", basename(inside_paths[top_level_r]), ignore.case = TRUE)
  ]
  main_index <- if (length(top_level_r) == 1L) {
    top_level_r
  } else if (length(prep_like) == 1L) {
    prep_like
  } else {
    candidates <- if (length(prep_like)) prep_like else top_level_r
    stop(
      "The selected folder has multiple possible preparation R files: ",
      paste(basename(inside_paths[candidates]), collapse = ", "),
      ". Keep one main prep-like R file at the folder's top level.",
      call. = FALSE
    )
  }

  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(stage_dir)) {
    stop("The uploaded preparation folder could not be staged.", call. = FALSE)
  }

  for (i in seq_len(nrow(uploaded_files))) {
    destination <- file.path(stage_dir, inside_paths[[i]])
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    copied <- file.copy(uploaded_files$datapath[[i]], destination, overwrite = TRUE)
    if (!isTRUE(copied)) {
      stop("Could not stage uploaded file: ", inside_paths[[i]], call. = FALSE)
    }
  }

  prep_dir <- normalizePath(stage_dir, winslash = "/", mustWork = TRUE)
  prep_script <- normalizePath(
    file.path(stage_dir, inside_paths[[main_index]]),
    winslash = "/",
    mustWork = TRUE
  )
  child_dirs <- list.dirs(prep_dir, full.names = TRUE, recursive = FALSE)
  fies_dirs <- child_dirs[
    tolower(basename(child_dirs)) == tolower("FIES-inputs")
  ]
  fies_inputs_dir <- if (length(fies_dirs)) {
    normalizePath(fies_dirs[[1L]], winslash = "/", mustWork = TRUE)
  } else {
    NULL
  }

  list(
    prep_dir = prep_dir,
    prep_script = prep_script,
    fies_inputs_dir = fies_inputs_dir,
    folder_name = folder_name,
    uploaded_file_count = nrow(uploaded_files)
  )
}

ui <- tagList(
  # tags$head style and scripts ------------------------------------------------------
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/xlsx@0.18.5/dist/xlsx.full.min.js"),
    tags$style(HTML("
    #xlsx_tbl table { width: 100%; border-collapse: collapse; font-size: 12px; }
    #xlsx_tbl th, #xlsx_tbl td { border: 1px solid #ddd; padding: 4px; vertical-align: top; }
    #xlsx_tbl tr:nth-child(even) { background: #fafafa; }
  ")),

# CSS for the ellipsis-select class ---------------------------------------------------
    tags$style(HTML("
/* Selectize dropdown items: single line + ellipsis */
.ellipsis-select .selectize-dropdown .option,
.ellipsis-select .selectize-dropdown .optgroup-header {
  white-space: nowrap !important;
  overflow: hidden !important;
  text-overflow: ellipsis !important;
}

/* Ensure the option area behaves like a single-line block */
.ellipsis-select .selectize-dropdown .option {
  display: block;
}

")),
    tags$script(HTML("
(function() {
  $(document).on('change', '#prep_folder', function() {
    var files = Array.prototype.slice.call(this.files || []);
    var relativePaths = files.map(function(file) {
      return file.webkitRelativePath || file.name;
    });
    var folderName = '';

    if (relativePaths.length && relativePaths[0].indexOf('/') >= 0) {
      folderName = relativePaths[0].split('/')[0];
    }

    Shiny.setInputValue(
      'prep_folder_relative_paths',
      relativePaths,
      { priority: 'event' }
    );

    if (folderName) {
      $(this).closest('.input-group').find('input[type=text]').val(folderName);
    }
  });
})();
")),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('render_xlsx', async function(msg) {
      const container = document.getElementById('xlsx_container');
      if (!container) return;

      container.innerHTML = '<div style=\"padding:8px\">Loading preview<U+2026></div>';

      try {
        const res = await fetch(msg.url, { cache: 'no-store' });
        if (!res.ok) throw new Error('HTTP ' + res.status);

        const buf = await res.arrayBuffer();
        const wb = XLSX.read(buf, { type: 'array' });

        // use requested sheet if it exists; else fall back to first
        const sheetName = (msg.sheet && wb.SheetNames.includes(msg.sheet))
          ? msg.sheet
          : wb.SheetNames[0];

        const ws = wb.Sheets[sheetName];
        const html = XLSX.utils.sheet_to_html(ws, { id: 'xlsx_tbl' });

        container.innerHTML = html;
      } catch (err) {
        container.innerHTML = '<div style=\"padding:8px; color:#b00\">Preview failed: ' + err + '</div>';
      }
    });
  "))
  ),
  tags$style(HTML("
      /* =========================
         Compact Table Name Cards
         ========================= */
      .table-name-card {
        max-height: 140px !important;
        overflow: hidden !important;
      }
      .table-name-card .card-body {
        padding-top: .5rem !important;
        padding-bottom: .5rem !important;
      }
      .table-name-card pre {
        margin: 0 !important;
        max-height: 2.8em;       /* ~2 lines */
        overflow: auto !important;
        white-space: pre-wrap;   /* wrap long names */
      }


      /* =========================
         Path card block + buttons
         ========================= */
      .path-block {
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas,
                     \"Liberation Mono\", \"Courier New\", monospace;
        white-space: pre-wrap;
        margin: 0;
        max-height: 2.8em;  /* ~2 lines */
        overflow: auto;
      }
      .path-sub {
        font-size: 0.85rem;
        margin-top: .25rem;
      }

      /* survey consistency card */
      .survey-card {
        max-height: 220px !important;
        overflow: visible !important;
      }
      .survey-card pre {
        max-height: 180px !important;
        overflow: auto !important;
        white-space: pre-wrap !important;
      }

      /* =========================
         Consistency check tab colors
         ========================= */
      .nav-tabs .nav-link.check-tab-green {
        background-color: #00B050 !important;
        color: #ffffff !important;
        border-color: #00B050 !important;
      }
      .nav-tabs .nav-link.check-tab-green.active {
        background-color: #1a4d38ff !important;
        color: #ffffff !important;
        border-color: #1a4d38ff !important;
      }

      .nav-tabs .nav-link.check-tab-red {
        background-color: #FF0000 !important;
        color: #ffffff !important;
        border-color: #FF0000 !important;
      }
      .nav-tabs .nav-link.check-tab-red.active {
        background-color: #570505ff !important;
        color: #ffffff !important;
        border-color: #570505ff !important;
      }
    ")),
  tags$script(HTML("
      Shiny.addCustomMessageHandler('setCheckTabStatus', function(msg) {
        var sel = '.nav-tabs .nav-link[data-value=\"' + msg.value + '\"]';
        var els = document.querySelectorAll(sel);
        if (!els || !els.length) return;

        els.forEach(function(el) {
          el.classList.remove('check-tab-green');
          el.classList.remove('check-tab-red');

          if (msg.status === 'green') el.classList.add('check-tab-green');
          if (msg.status === 'red')   el.classList.add('check-tab-red');
        });
      });

      // =========================================================
      // Auto-scroll Run-and-write reactable to bottom
      // =========================================================
      Shiny.addCustomMessageHandler('scrollRunAllTable', function(msg) {
        var root = document.getElementById('run_all_results_tbl');
        if (!root) return;

        // reactable scroll container
        var scrollDiv = root.querySelector('.rt-table');
        if (!scrollDiv) scrollDiv = root.querySelector('.reactable');
        if (!scrollDiv) return;

        scrollDiv.scrollTop = scrollDiv.scrollHeight;
      });
    ")),
  tags$script(HTML("
(function() {

  function applyTitlesSelectize() {
    document.querySelectorAll('.ellipsis-select .selectize-dropdown .option').forEach(function(opt) {
      opt.title = opt.textContent.trim();
    });
  }

  // When selectize dropdown opens, options get created/updated
  $(document).on('mouseenter', '.ellipsis-select .selectize-dropdown', applyTitlesSelectize);
  $(document).on('click', '.ellipsis-select .selectize-dropdown', applyTitlesSelectize);
  $(document).on('keyup', '.ellipsis-select .selectize-control input', applyTitlesSelectize);

})();

")),
  page_navbar(
    title = NULL,
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    selected = "Data Preparation",

    
    # TAB 0: USER'S GUIDE -------------------------------------------------------------
    nav_panel(
      "User's Guide",
      card(
        full_screen = TRUE,
        card_header(
          tags$div(
            class = "d-flex flex-wrap align-items-center gap-3",
            tags$strong("Application user guide"),
            tags$a(
              "Open full guide",
              href = paste0(guide_resource_prefix, "/user-guide.html"),
              target = "_blank", rel = "noopener"
            ),
            tags$a(
              "Download PDF",
              href = paste0(guide_resource_prefix, "/user-guide.pdf"),
              download = "micsPlusTableR-user-guide.pdf"
            )
          )
        ),
        tags$iframe(
          src = paste0(guide_resource_prefix, "/user-guide.html"),
          title = "MICS Plus Tabulation application user guide",
          style = "width: 100%; height: calc(100vh - 210px); min-height: 650px; display: block; border: none;"
        )
      )
    ),


    nav_panel(
      "Download Files",
      micsPlusTableR:::download_files_ui("survey_download", project_dir)
    ),

    # TAB 0: DATA PREPARATION --------------------------------------------------------
    nav_panel(
      "Data Preparation",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          selectInput("select_country", "Select country and period",
            choices = unique(survey_choices$label)
          ),
          selectInput("select_wave", "Select wave", choices = survey_choices$wave[
            survey_choices$label == survey_choices$label[1L]
          ]),
          fileInput("hh_file", "Select household file", accept = c(".sav")),
          fileInput("hl_file", "Select household members file", accept = c(".sav")),
          fileInput("tab_file", "Select tabulation file", accept = c(".xls", ".xlsx")),
          prep_folder_input,
          tags$div(
            class = "small text-muted mt-1 mb-2 text-break",
            textOutput("prep_folder_info", inline = TRUE)
          ),
          actionButton("run_settings", "Set", class = "btn-primary w-100"),
          div(
            class = "alert alert-info mt-3 mb-0 small",
            tags$strong("Excel output folder"),
            tags$p(
              class = "mb-0 mt-1",
              "Generated Output and Formatted workbooks are written to:"
            ),
            tags$code(class = "d-block text-break", excel_root)
          )
        ),
        layout_columns(
          col_widths = c(12),
          card(
            class = "survey-card",
            full_screen = FALSE,
            card_header("Survey info consistency"),
            verbatimTextOutput("survey_info_consistency")
          ),

          # Tabulation Plan card ----
          card(
            full_screen = TRUE,
            card_header("Tabulation Plan"),

            # Sheet selector
            selectInput("tabplan_sheet", "Sheet", choices = character(0)),

            # Scrollable preview area
            tags$div(
              style = "max-height: 7000px; overflow: auto;",
              tags$div(id = "xlsx_container")
            )
          )
        )
      )
    ),

    # TAB 1: TABULATOR ---------------------------------------------------------------
    nav_panel(
      "Tabulator",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          selectInput("sheet_id", "Select a sheet", choices = character(0)),
          radioButtons(
            "format_type",
            "Format type",
            choices = c("Unformatted" = "unformatted", "Suppressed" = "formatted", "Formatted" = "view"),
            selected = "view"
          ),
          br(),
          radioButtons(
            "pivot_type",
            "Pivot type",
            choices = c("Index" = "index", "Header" = "header", "Logic" = "logic"),
            selected = "header"
          ),
          actionButton("run_tab", "Run", class = "btn-primary w-100")
        ),
        layout_columns(
          col_widths = c(12),
          card(
            class = "table-name-card",
            full_screen = FALSE,
            card_header("Table name"),
            verbatimTextOutput("table_name_tab1")
          ),
          card(full_screen = TRUE, card_header("Pivot table preview"), uiOutput("pivot_preview"))
        )
      )
    ),

    # TAB 2: CONSISTENCY CHECKS -------------------------------------------------------
    nav_panel(
      "Consistency Checks",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          checkboxInput("show_only_inconsistent", "Show only inconsistent cells", value = FALSE),
          actionButton("run_checks", "Run Checks", class = "btn-warning w-100")
        ),
        tagList(
          card(
            class = "table-name-card",
            full_screen = FALSE,
            card_header("Table name"),
            verbatimTextOutput("table_name_tab2")
          ),
          card(
            full_screen = TRUE,
            navset_tab(
              id = "checks_tabs",
              nav_panel("Row Group (N)", value = "row_group_n", reactableOutput("tbl_row_group_n", height = "650px")),
              nav_panel("Row Group (Perc)", value = "row_group_p", reactableOutput("tbl_row_group_p", height = "650px")),
              nav_panel("Row Indent Group (N)", value = "row_indent_n", reactableOutput("tbl_row_indent_n", height = "650px")),
              nav_panel("Row Indent Group (Perc)", value = "row_indent_p", reactableOutput("tbl_row_indent_p", height = "650px")),
              nav_panel("Col Group (N)", value = "col_group_n", reactableOutput("tbl_col_group_n", height = "650px")),
              nav_panel("Col Group (Perc)", value = "col_group_p", reactableOutput("tbl_col_group_p", height = "650px"))
            )
          )
        )
      )
    ),

    # TAB 3: WRITE TO EXCEL ----------------------------------------------------------
    nav_panel(
      "Write to Excel",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          radioButtons(
            "excel_target",
            "Create",
            choices = c("Output" = "output", "Formatted" = "formatted", "Both" = "both"),
            selected = "both",
            inline = FALSE
          ),
          actionButton("create_excel_files", "Create Excel File(s)", class = "btn-outline-primary w-100"),
          br(), br(),
          shinySaveButton(
            "dest_file",
            # (where to write should be in another line), \n doesn't work
            "Choose existing output file (where to write)",
            title = "Save output workbook as",
            filetype = list(xlsx = "xlsx")
          ),
          actionButton("write_output_table", "Write to Output Table", class = "btn-outline-success w-100"),
          br(), br(),
          shinySaveButton(
            "dest__f_file",
            # (where to write should be in another line)
            "Choose existing formatted file (where to write)",
            title = "Save formatted workbook as",
            filetype = list(xlsx = "xlsx")
          ),
          actionButton("write_formatted_table", "Write to Formatted Table", class = "btn-outline-success w-100")
        ),
        layout_columns(
          col_widths = c(12),
          card(
            class = "table-name-card",
            full_screen = FALSE,
            card_header("Output file path"),
            uiOutput("output_file_path")
          ),
          card(
            class = "table-name-card",
            full_screen = FALSE,
            card_header("Formatted file path"),
            uiOutput("formatted_file_path")
          )
        )
      )
    ),

    # TAB 4: RUN AND WRITE ------------------------------------------------------------
    nav_panel(
      "Multi-Sheet Tabulator",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          selectizeInput(
            "sheet_ids_multi",
            "Select sheet(s)",
            choices = character(0),
            multiple = TRUE,
            options = list(placeholder = "Choose one or more sheets...")
          ),
          div(
            class = "d-flex gap-2",
            # Select all and Deselect all buttons should be have darker background color
            actionButton("select_all_sheets", "Select all", class = "btn-secondary w-100"),
            actionButton("deselect_all_sheets", "Deselect all", class = "btn-secondary w-100")
          ),
          hr(),
          radioButtons(
            "write_target_all",
            "Write to",
            choices = c("Output table" = "output", "Formatted table" = "formatted", "Both" = "both"),
            selected = "both"
          ),
          actionButton("run_all_btn", "Run and write selected", class = "btn-danger w-100")
        ),
        layout_columns(
          col_widths = c(12),
          card(full_screen = TRUE, card_header("Run-and-write results"), reactableOutput("run_all_results_tbl"))
        )
      )
    ),

    # TAB 5: DATA EXPLORATION ---------------------------------------------------------
    nav_panel(
      "Data Exploration",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          markdown("
            ## Data Exploration
          "),
          radioButtons(
            "select_data",
            "Select data set",
            choices = c("hh" = "hh", "hl" = "hl"),
            selected = "hh"
          ),
          # An area to define the filters
          textInput("filter_area1", "Filter 1", value = ""),
          textInput("filter_area2", "Filter 2", value = ""),
          radioButtons("select_weight_type", "Select weighting type", choices = c("Unweighted" = "unweighted_type", "Weighted" = "weighted_type"), selected = "unweighted_type"),
          # This should be disabled if the weight type is unweighted
          conditionalPanel(
            condition = "input.select_weight_type == 'weighted_type'",
            textInput(
              "select_weight",
              "Type weight variable",
              value = "",
              placeholder = "No weight variable selected"
            )
          ),
          radioButtons("analysis_type", "Select analysis type", choices = c("Frequency" = "frequency", "Cross table" = "cross-table"), selected = "frequency"),
          div(
            class = "ellipsis-select",
            selectizeInput(
              "select_variable",
              "Select variable(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "No variables are selected"
              )
            )
          ),
          actionButton("run_data_analysis", "Run Data Explorer", class = "btn-primary w-100")
        ),
        layout_columns(
          col_widths = c(12),
          card(full_screen = TRUE, card_header("Output Table"), reactableOutput("freq_table_df"))
        )
      )
    ),

    # TAB 6: DATA VIEW UI -------------------------------------------------------------
    nav_panel(
      "Data View",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          markdown("
            ## Data View
          "),
          radioButtons(
            "select_data_view",
            "Select data to view",
            choices = c("hh" = "hh", "hl" = "hl"),
            selected = "hh"
          ),
          div(
            class = "ellipsis-select",
            selectizeInput(
              "select_variable2",
              "Select variable(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "All variables are selected"
              )
            )
          ),
          actionButton("run_data_view", "Run Data View", class = "btn-primary w-100")
        ),
        layout_columns(
          col_widths = c(12),
          card(full_screen = TRUE, card_header("Output Table"), reactableOutput("data_view_table"))
        )
      )
    ),

    # TAB 7: Long Format Data ---------------------------------------------------------
    nav_panel(
      "Long Format Data",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          markdown("
            ## Long Format Data
          "),
          actionButton("run_long_format_data", "Run all tables and prepare...", class = "btn-primary w-100"),
          conditionalPanel(
            condition = "output.long_format_download_ready === 'true'",
            tags$hr(),
            tags$h5("Download output"),
            tags$p(class = "text-muted small", "Download all rows and columns of the prepared data."),
            tags$div(
              class = "d-grid gap-2",
              downloadButton("download_long_format_excel", "Excel (.xlsx)"),
              downloadButton("download_long_format_csv", "CSV (.csv)"),
              downloadButton("download_long_format_rds", "RDS (.rds)")
            )
          ),
          conditionalPanel(
            condition = "output.long_format_download_ready !== 'true'",
            tags$p(class = "text-muted small mt-3", "Prepare the data to enable downloads.")
          )
        ),
        layout_columns(
          col_widths = c(12),
          card(full_screen = TRUE, card_header("Output Table"), reactableOutput("data_long_format_table"))
        )
      )
    ),
    nav_spacer(),
    nav_item(tags$div(class = "ms-auto navbar-text fw-semibold", "MICS Plus Tabulation"))
  )
)

server <- function(input, output, session) {
  micsPlusTableR:::download_files_server("survey_download", project_dir)
  engine_env <- micsPlusTableR::mics_session()
  micsPlusTableR:::bind_engine_functions(engine_env, environment())

  # GLOBAL CONSISTENCY THRESHOLD (everywhere) ---------------------------------------
  TOL <- 1e-6

  # Reactive values ------------------------------------------------------------------
  table_name_rv <- reactiveVal(NULL)
  tb_rv <- reactiveVal(NULL)

  sheet_rv <- reactiveVal(NULL)
  cell_results_rv <- reactiveVal(NULL)


ensure_current_workbook <- function() {
  req(tab_path_rv())

  p <- tab_path_rv()
  req(is.character(p), length(p) == 1, file.exists(p))

  wb_now <- openxlsx2::wb_load(p)
  sheets_now <- openxlsx2::wb_get_sheet_names(wb_now)

  tab_wb_rv(wb_now)
  tab_sheets_rv(sheets_now)

  invisible(sheets_now)
}
  # col_index -> table_column map (named vector)
  # names: "col_index" (as character)
  # values: table_column (integer 1..K)
  column_map_rv <- reactive({
    cr <- cell_results_rv()
    req(cr)

    cm <- cr |>
      dplyr::distinct(col_index) |>
      dplyr::arrange(col_index) |>
      dplyr::mutate(table_column = dplyr::row_number())

    stats::setNames(cm$table_column, as.character(cm$col_index))
  })

  # destinations (REAL filesystem paths)
  dest_rv <- reactiveVal(NULL)
  dest_f_rv <- reactiveVal(NULL)

  run_all_results_rv <- reactiveVal(NULL)

  # store hh/hl from uploaded savs (option B script creates hh/hl)
  hh_rv <- reactiveVal(NULL)
  hl_rv <- reactiveVal(NULL)

  hh_new_rv <- reactiveVal(NULL)
  hl_new_rv <- reactiveVal(NULL)

  # Data view outputs
  data_view_rv <- reactiveVal(NULL)
  # Per-check outputs (6)
  chk_row_group_n_rv <- reactiveVal(NULL)
  chk_row_group_p_rv <- reactiveVal(NULL)
  chk_col_group_n_rv <- reactiveVal(NULL)
  chk_col_group_p_rv <- reactiveVal(NULL)
  chk_row_indent_n_rv <- reactiveVal(NULL)
  chk_row_indent_p_rv <- reactiveVal(NULL)

  # Run-and-write state
  run_all_running_rv <- reactiveVal(FALSE)
  run_all_start_rv <- reactiveVal(NULL)
  run_all_step_rv <- reactiveVal("")
  run_all_pct_rv <- reactiveVal(0)

  # <U+2705> finish popup state
  run_all_done_rv <- reactiveVal(FALSE)
  run_all_done_msg_rv <- reactiveVal("")

  # Keeps the current sheet list in-sync
  sheets_all_rv <- reactiveVal(character(0))

  # For long format data
  cell_results_all_rv <- reactiveVal(NULL)
  long_format_metadata_rv <- reactiveVal(NULL)
  micsPlusTableR:::mics_long_format_downloads(
    output, cell_results_all_rv, long_format_metadata_rv
  )

  observeEvent(input$select_country, {
    waves <- survey_choices$wave[
      survey_choices$label == input$select_country
    ]
    req(length(waves))
    selected <- if (length(input$select_wave) == 1L && input$select_wave %in% waves) {
      input$select_wave
    } else {
      waves[1L]
    }
    updateSelectInput(session, "select_wave", choices = waves, selected = selected)
  })

  survey_meta_rv <- reactive({
    req(input$select_country, input$select_wave)
    selected <- survey_choices[
      survey_choices$label == input$select_country &
        survey_choices$wave == input$select_wave,
      , drop = FALSE
    ]
    req(nrow(selected) == 1L)

    assign("country_abb", selected$country_code, envir = engine_env)

    list(
      country     = selected$country,
      country_abb = selected$country_code,
      period      = selected$period,
      wave        = selected$wave
    )
  })

  # shinyFiles roots -----------------------------------------------------------------
  # Keep workbook defaults in the packaged engine session.
  assign("path_output_tables", excel_root, envir = engine_env)
  assign("path_formatted_tables", excel_root, envir = engine_env)
  assign("excel_root_default", excel_root, envir = engine_env)

  home_path <- function(winslash = "/") {
    p <- if (.Platform$OS.type == "windows") {
      Sys.getenv("USERPROFILE") %||% paste0(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH"))
    } else {
      path.expand("~")
    }
    normalizePath(p, winslash = winslash, mustWork = FALSE)
  }

  roots <- if (.Platform$OS.type == "windows") {
    c(
      ExcelFiles = excel_root,
      Home       = home_path("/"),
      shinyFiles::getVolumes()()
    )
  } else {
    c(
      ExcelFiles = excel_root,
      Home       = home_path("/")
    )
  }

  prep_folder_selection <- reactive({
    req(input$prep_folder, input$prep_folder_relative_paths)
    stage_preparation_upload_app(
      input$prep_folder,
      input$prep_folder_relative_paths
    )
  })

  prep_script_path <- reactive({
    prep_folder_selection()$prep_script
  })

  output$prep_folder_info <- renderText({
    if (is.null(input$prep_folder)) return("")

    tryCatch({
      selection <- prep_folder_selection()
      fies_status <- if (is.null(selection$fies_inputs_dir)) "none" else "FIES-inputs"
      paste0(
        "Main prep: ", basename(selection$prep_script),
        " | FIES helpers: ", fies_status
      )
    }, error = function(e) {
      message <- conditionMessage(e)
      if (!nzchar(message)) return("")
      paste("Preparation folder issue:", message)
    })
  })

  shinyFileSave(
    input, "dest_file",
    roots = roots,
    session = session,
    filetypes = c("xlsx"),
    defaultRoot = "ExcelFiles"
  )

  shinyFileSave(
    input, "dest__f_file",
    roots = roots,
    session = session,
    filetypes = c("xlsx"),
    defaultRoot = "ExcelFiles"
  )

  # Helper: synchronize workbook destinations used by packaged engine writers. --------
  sync_dest_globals <- function() {
    if (!is.null(dest_rv())) assign("dest", dest_rv(), envir = engine_env)
    if (!is.null(dest_f_rv())) assign("dest_f", dest_f_rv(), envir = engine_env)
  }

  paths_key <- reactive({
    meta <- survey_meta_rv()

    tab_id <- if (!is.null(input$tab_file)) {
      paste0(basename(input$tab_file$name), "::", input$tab_file$size)
    } else {
      "no_tab"
    }

    paste(
      meta$country_abb,
      meta$period,
      meta$wave,
      input$excel_target,
      tab_id,
      sep = "|"
    )
  })

  paths_key_last_rv <- reactiveVal(NULL)

  observeEvent(paths_key(),
    {
      if (!identical(paths_key_last_rv(), paths_key())) {
        dest_rv(NULL)
        dest_f_rv(NULL)

        if (exists("dest", envir = engine_env, inherits = FALSE)) {
          rm(list = "dest", envir = engine_env)
        }
        if (exists("dest_f", envir = engine_env, inherits = FALSE)) {
          rm(list = "dest_f", envir = engine_env)
        }

        paths_key_last_rv(paths_key())
      }
    },
    ignoreInit = TRUE
  )


  observeEvent(input$dest_file, {
    sf <- shinyFiles::parseSavePath(roots, input$dest_file)
    req(nrow(sf) == 1)
    p <- as.character(sf$datapath)
    if (!grepl("\\.xlsx$", p, ignore.case = TRUE)) p <- paste0(p, ".xlsx")
    dest_rv(p)

    sync_dest_globals()
    showNotification(paste("Output destination set:", p), type = "message")
  })

  observeEvent(input$dest__f_file, {
    sf <- shinyFiles::parseSavePath(roots, input$dest__f_file)
    req(nrow(sf) == 1)
    p <- as.character(sf$datapath)
    if (!grepl("\\.xlsx$", p, ignore.case = TRUE)) p <- paste0(p, ".xlsx")
    dest_f_rv(p)

    sync_dest_globals()
    showNotification(paste("Formatted destination set:", p), type = "message")
  })

  observeEvent(hh_rv(),
    {
      hh <- hh_rv()
      req(hh)
      hh_new_rv(make_value_label_df(hh))
    },
    ignoreInit = TRUE
  )

  observeEvent(hl_rv(),
    {
      hl <- hl_rv()
      req(hl)
      hl_new_rv(make_value_label_df(hl))
    },
    ignoreInit = TRUE
  )

  observeEvent(list(hh_rv(), hl_rv(), input$select_data),
    {
      req(hh_rv(), hl_rv())

      ds <- if (identical(input$select_data, "hh")) hh_rv() else hl_rv()
      req(ds)

      choices <- make_var_choices(ds)

      current <- isolate(input$select_variable %||% character(0))
      keep <- intersect(current, unname(choices))

      updateSelectInput(
        session,
        "select_variable",
        choices  = choices,
        selected = keep
      )
    },
    ignoreInit = FALSE
  )


  # Open file/folder with OS command ------------------------------------------------
  open_path_os <- function(path) {
    path <- as.character(path)
    sys <- Sys.info()[["sysname"]]

    if (identical(sys, "Darwin")) {
      path <- normalizePath(path, winslash = "/", mustWork = FALSE)
      system2("open", shQuote(path), wait = FALSE)
    } else if (identical(sys, "Windows")) {
      path <- normalizePath(path, winslash = "\\", mustWork = FALSE)

      if (!dir.exists(path) && !file.exists(path)) {
        stop("Path does not exist: ", path)
      }

      # shell.exec works for both files and folders
      shell.exec(path)
    } else {
      path <- normalizePath(path, winslash = "/", mustWork = FALSE)
      system2("xdg-open", shQuote(path), wait = FALSE)
    }
  }




  # UI: SHOW path + buttons ----------------------------------------------------------
  pretty_path <- function(p) {
    if (is.null(p) || !nzchar(p)) {
      return(NULL)
    }
    normalizePath(p, winslash = "/", mustWork = FALSE)
  }

  file_path_ui <- function(p, prefix = "output") {
    p <- pretty_path(p)

    if (is.null(p)) {
      return(
        tags$div(
          tags$pre(
            class = "path-block",
            "Not created yet.\n\nUse: Create Excel File(s)\n(or choose a path with the Save button)."
          ),
          tags$div(
            class = "d-flex gap-2 mt-2",
            actionButton(paste0("open_", prefix, "_file"), "Open file", class = "btn btn-sm btn-outline-secondary w-50"),
            actionButton(paste0("open_", prefix, "_folder"), "Open folder", class = "btn btn-sm btn-outline-secondary w-50")
          )
        )
      )
    }

    tags$div(
      tags$pre(class = "path-block", p),
      tags$div(
        class = "d-flex gap-2 mt-2",
        actionButton(paste0("open_", prefix, "_file"), "Open file", class = "btn btn-sm btn-outline-primary w-50"),
        actionButton(paste0("open_", prefix, "_folder"), "Open folder", class = "btn btn-sm btn-outline-primary w-50")
      ),
      if (!file.exists(p)) {
        tags$div(
          class = "path-sub",
          style = "color:#b30000; font-weight:600;",
          "(file does not exist on disk)"
        )
      }
    )
  }

  output$output_file_path <- renderUI({
    file_path_ui(dest_rv(), prefix = "output")
  })

  output$formatted_file_path <- renderUI({
    file_path_ui(dest_f_rv(), prefix = "formatted")
  })

  observeEvent(input$open_output_file, {
    p <- dest_rv()
    if (is.null(p) || !nzchar(p)) {
      showNotification("Output path is not set yet.", type = "warning")
      return()
    }
    tryCatch(open_path_os(p), error = function(e) {
      showNotification(paste("Could not open file:", e$message), type = "error")
    })
  })

  observeEvent(input$open_output_folder, {
    p <- dest_rv()
    if (is.null(p) || !nzchar(p)) {
      showNotification("Output path is not set yet.", type = "warning")
      return()
    }
    tryCatch(open_path_os(dirname(p)), error = function(e) {
      showNotification(paste("Could not open folder:", e$message), type = "error")
    })
  })

  observeEvent(input$open_formatted_file, {
    p <- dest_f_rv()
    if (is.null(p) || !nzchar(p)) {
      showNotification("Formatted path is not set yet.", type = "warning")
      return()
    }
    tryCatch(open_path_os(p), error = function(e) {
      showNotification(paste("Could not open file:", e$message), type = "error")
    })
  })

  observeEvent(input$open_formatted_folder, {
    p <- dest_f_rv()
    if (is.null(p) || !nzchar(p)) {
      showNotification("Formatted path is not set yet.", type = "warning")
      return()
    }
    tryCatch(open_path_os(dirname(p)), error = function(e) {
      showNotification(paste("Could not open folder:", e$message), type = "error")
    })
  })

  # Helper: set a check sub-tab status ("none" | "green" | "red")
  set_check_tab_status <- function(value, status = c("none", "green", "red")) {
    status <- match.arg(status)
    session$sendCustomMessage("setCheckTabStatus", list(value = value, status = status))
  }

  # Consistency issue counter ----------------------------------------------------------------
  safe_issue_n <- function(cfg, df = NULL, tol = TOL) {
    n1 <- 0L
    if (exists(cfg$issue, envir = engine_env, inherits = FALSE)) {
      raw <- get(cfg$issue, envir = engine_env)

      n1 <- suppressWarnings(as.integer(raw))
      if (length(n1) != 1) n1 <- suppressWarnings(as.integer(sum(as.numeric(raw), na.rm = TRUE)))
      if (is.na(n1) || length(n1) == 0) n1 <- 0L
    }

    n2 <- 0L
    if (is.data.frame(df) && "diff_value" %in% names(df)) {
      dv <- suppressWarnings(as.numeric(df$diff_value))
      n2 <- as.integer(sum(!is.na(dv) & abs(dv) > tol))
    }

    max(n1, n2)
  }

  filter_inconsistent <- function(df, tol = TOL) {
    if (!isTRUE(input$show_only_inconsistent)) {
      return(df)
    }
    if (!is.data.frame(df)) {
      return(df)
    }

    if ("diff_value" %in% names(df)) {
      dv <- suppressWarnings(as.numeric(df$diff_value))
      ok <- !is.na(dv)
      return(df[ok & abs(dv) > tol, , drop = FALSE])
    }

    if (all(c("group_total", "total_value") %in% names(df))) {
      gt <- suppressWarnings(as.numeric(df$group_total))
      tv <- suppressWarnings(as.numeric(df$total_value))
      ok <- !is.na(gt) & !is.na(tv)
      return(df[ok & abs(gt - tv) > tol, , drop = FALSE])
    }

    df
  }

  # DATA PREPARATION (uploads) -------------------------------------------------------
  survey_info_msg_rv <- reactiveVal("No check run yet.")

  output$survey_info_consistency <- renderPrint({
    cat(survey_info_msg_rv())
  })

  clear_checks_outputs <- function() {
    chk_row_group_n_rv(NULL)
    chk_row_group_p_rv(NULL)
    chk_col_group_n_rv(NULL)
    chk_col_group_p_rv(NULL)
    chk_row_indent_n_rv(NULL)
    chk_row_indent_p_rv(NULL)

    set_check_tab_status("row_group_n", "none")
    set_check_tab_status("row_group_p", "none")
    set_check_tab_status("col_group_n", "none")
    set_check_tab_status("col_group_p", "none")
    set_check_tab_status("row_indent_n", "none")
    set_check_tab_status("row_indent_p", "none")
  }

  clear_revision_state <- function() {
    table_name_rv(NULL)
    tb_rv(NULL)
    sheet_rv(NULL)
    cell_results_rv(NULL)
    clear_checks_outputs()

    objs <- c("sheet", "cell_results", "tables_extra", "extra_tables_dict", "any_issues_found")
    for (nm in objs) {
      if (exists(nm, envir = engine_env, inherits = FALSE)) {
        rm(list = nm, envir = engine_env)
      }
    }
  }

  get_current_tab_path <- function() {
    p <- tab_path_rv()
    req(p)
    validate(need(file.exists(p), "Uploaded tabulation file is not available. Please upload it again."))
    p
  }

  build_cell_results_for_sheet <- function(sheet) {
    path_tab_excel <- get_current_tab_path()

    assign("path_tab_excel", path_tab_excel, envir = engine_env)
    assign("sheet", sheet, envir = engine_env)

    clear_previous()
    ensure_current_workbook()
    read_tabulation(path_tab_excel, sheet)
    table_name_rv(get_table_name())

    if (!exists("tables_extra", envir = engine_env, inherits = FALSE) ||
      is.null(get("tables_extra", envir = engine_env, inherits = FALSE))) {
      assign("tables_extra", character(0), envir = engine_env)
    }

    if (!exists("extra_tables_dict", envir = engine_env, inherits = FALSE) ||
      is.null(get("extra_tables_dict", envir = engine_env, inherits = FALSE))) {
      assign("extra_tables_dict", list(), envir = engine_env)
    }

    tables_extra0 <- get("tables_extra", envir = engine_env, inherits = FALSE)
    extra_dict0 <- get("extra_tables_dict", envir = engine_env, inherits = FALSE)

    if (length(tables_extra0) == 0 || !(sheet %in% tables_extra0)) {
      cell_results <- tabulate_mics()
      assign("cell_results", cell_results, envir = engine_env)
      return(cell_results)
    }

    key <- extra_dict0[[sheet]]

    if (is.null(key) || length(key) != 1 || !nzchar(key)) {
      stop(sprintf("extra_tables_dict has no valid entry for sheet '%s'.", sheet))
    }

    if (!exists(key, envir = engine_env, inherits = FALSE)) {
      stop(sprintf("Extra table object '%s' not found in engine_env for sheet '%s'.", key, sheet))
    }

    table_new <- get(key, envir = engine_env, inherits = FALSE)
    cell_results <- tabulate_extra_table(table_new)
    assign("cell_results", cell_results, envir = engine_env)

    cell_results
  }

  # Shared state for uploaded tabulation plan ---------------------------------------
  tab_path_rv   <- reactiveVal(NULL)         # server temp path
tab_wb_rv     <- reactiveVal(NULL)         # current workbook
tab_sheets_rv <- reactiveVal(character(0)) # all sheet names (incl IDX)

observeEvent(input$tab_file, {
  req(input$tab_file)

  clear_revision_state()

  tab_path <- input$tab_file$datapath
  tab_path_rv(tab_path)

  wb <- openxlsx2::wb_load(tab_path)
  tab_wb_rv(wb)

  sheets <- openxlsx2::wb_get_sheet_names(wb)
  tab_sheets_rv(sheets)

  updateSelectInput(
    session, "tabplan_sheet",
    choices = sheets,
    selected = if (length(sheets)) sheets[1] else character(0)
  )

  sheets_local <- setdiff(sheets, "IDX")
  sheets_all_rv(sheets_local)

  updateSelectInput(
    session, "sheet_id",
    choices = sheets_local,
    selected = if (length(sheets_local)) sheets_local[[1]] else character(0)
  )

  updateSelectizeInput(
    session, "sheet_ids_multi",
    choices = sheets_local,
    selected = character(0),
    server = TRUE
  )
})

  # Serve the uploaded file for browser preview
  tabplan_url <- reactive({
    tab_path <- tab_path_rv()
    req(tab_path, input$tab_file)

    dest_dir <- file.path(tempdir(), "tabplan_preview")
    dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)

    dest <- file.path(
      dest_dir,
      paste0("tabplan-", as.integer(Sys.time()), "-", basename(input$tab_file$name))
    )
    file.copy(tab_path, dest, overwrite = TRUE)

    shiny::addResourcePath("tabplan", dest_dir)
    paste0("tabplan/", basename(dest))
  })

  # Re-render preview when either file or selected preview sheet changes
  observeEvent(list(tab_path_rv(), input$tabplan_sheet), {
    req(tab_path_rv(), input$tabplan_sheet)

    session$sendCustomMessage(
      "render_xlsx",
      list(url = tabplan_url(), sheet = input$tabplan_sheet)
    )
  })

  observeEvent(input$run_settings, {
    survey_info_msg_rv("Running survey info consistency check...\n")

    tryCatch(
      {
        req(input$hh_file, input$hl_file, input$tab_file, input$prep_folder)
        tab_path <- tab_path_rv()
        req(tab_path)

        # (optional) ensure global is set (it is already set on upload)
        assign("path_tab_excel", tab_path, envir = engine_env)

        # --- Prepare hh/hl from the selected preparation script ---
        hh_path <- input$hh_file$datapath
        hl_path <- input$hl_file$datapath
        prep_script <- prep_script_path()

        micsPlusTableR::prepare_mics_data(
          engine_env,
          hh_path = hh_path,
          hl_path = hl_path,
          prep_script = prep_script,
          output_dir = excel_root
        )

        hh_obj <- get("hh", envir = engine_env, inherits = FALSE)
        hl_obj <- get("hl", envir = engine_env, inherits = FALSE)

        hh_rv(hh_obj)
        hl_rv(hl_obj)
        assign("hh", hh_obj, envir = engine_env)
        assign("hl", hl_obj, envir = engine_env)

        # Expected from UI ----------------------------------------------------------------
        expected_country <- input$select_country |>
          as.character() |>
          stringr::str_trim()
        expected_wave <- input$select_wave |>
          as.character() |>
          stringr::str_trim() |>
          stringr::str_replace("^Wave\\s*", "")

        # Read IDX from the uploaded file -----------------------------------------------
        df_idx <- readxl::read_excel(tab_path, sheet = "IDX", col_names = FALSE)
        vec <- df_idx |>
          dplyr::select(1) |>
          dplyr::pull() |>
          as.character()

        if (length(vec) < 4) stop("IDX sheet is too short (needs at least 4 rows in column 1).")

        country_e <- vec[1] |> stringr::str_trim()
        period_e <- vec[3] |> stringr::str_trim()
        country_e <- paste0(country_e, " (", period_e, ")")
        wave_e <- vec[4] |>
          stringr::str_trim() |>
          stringr::str_replace("^Wave\\s*", "")

        ok <- identical(country_e, expected_country) && identical(wave_e, expected_wave)

        if (ok) {
survey_info_msg_rv(
  paste0(
    "✅ The information from the Reference file is consistent.\n\n",
    "Reference (IDX):\n",
    "  Country: ", country_e, "\n",
    "  Wave   : ", wave_e
  )
)
        } else {
survey_info_msg_rv(
  paste0(
    "❌ WARNING: The information from the Reference file is NOT consistent.\n\n",
    "Reference (IDX):\n",
    "  Country: ", country_e, "\n",
    "  Wave   : ", wave_e, "\n\n",
    "Expected (UI/settings):\n",
    "  Country: ", expected_country, "\n",
    "  Wave   : ", expected_wave
  )
)
        }

        showNotification("Settings check done (hh/hl prepared; IDX checked).", type = "message")
      },
      error = function(e) {
        survey_info_msg_rv(paste0("❌ Error: ", e$message))
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })


  # CORE: run ALL checks --------------------------------------------------------------
  run_checks_core <- function(x, update_ui = FALSE) {
    vars_to_check <- c(
      "totals_col_perc_df_issue_n",
      "totals_col_df_issue_n",
      "totals_row_perc_df_issue_n",
      "totals_row_df_issue_n",
      "totals_indent_row_df_issue_n",
      "totals_indent_row_perc_df_issue_n"
    )
    assign("vars_to_check", vars_to_check, envir = engine_env)

    check_map <- list(
      "Row Group"             = list(fun = "row_group_total_check", out = "totals_row_df", issue = "totals_row_df_issue_n", store = chk_row_group_n_rv, value = "row_group_n"),
      "Row Group Perc"        = list(fun = "row_group_perc_total_check", out = "totals_row_perc_df", issue = "totals_row_perc_df_issue_n", store = chk_row_group_p_rv, value = "row_group_p"),
      "Col Group"             = list(fun = "col_group_total_check", out = "totals_col_df", issue = "totals_col_df_issue_n", store = chk_col_group_n_rv, value = "col_group_n"),
      "Col Group Perc"        = list(fun = "col_group_perc_total_check", out = "totals_col_perc_df", issue = "totals_col_perc_df_issue_n", store = chk_col_group_p_rv, value = "col_group_p"),
      "Row Indent Group"      = list(fun = "row_indent_group_total_check", out = "totals_indent_row_df", issue = "totals_indent_row_df_issue_n", store = chk_row_indent_n_rv, value = "row_indent_n"),
      "Row Indent Group Perc" = list(fun = "row_indent_group_perc_total_check", out = "totals_indent_row_perc_df", issue = "totals_indent_row_perc_df_issue_n", store = chk_row_indent_p_rv, value = "row_indent_p")
    )

    out_names <- unique(vapply(check_map, `[[`, character(1), "out"))
    for (nm in out_names) assign(nm, data.frame(), envir = engine_env)
    for (nm in vars_to_check) assign(nm, 0L, envir = engine_env)

    if (isTRUE(update_ui)) {
      for (cfg in check_map) set_check_tab_status(cfg$value, "none")
    }

    for (label in names(check_map)) {
      cfg <- check_map[[label]]

      if (!exists(cfg$fun, mode = "function", envir = engine_env)) {
        if (isTRUE(update_ui)) {
          cfg$store(NULL)
          set_check_tab_status(cfg$value, "none")
        }
        next
      }

      f <- get(cfg$fun, envir = engine_env)

      tryCatch(
        f(x, diff = TOL),
        error = function(e) tryCatch(f(x), error = function(e2) NULL)
      )

      if (exists(cfg$out, envir = engine_env, inherits = FALSE)) {
        df <- get(cfg$out, envir = engine_env)

        if (is.data.frame(df)) {
          if (isTRUE(update_ui)) {
            if (nrow(df) > 0) cfg$store(df) else cfg$store(NULL)
          }

          issue_n <- safe_issue_n(cfg, df = df, tol = TOL)

          if (isTRUE(update_ui)) {
            if (isTRUE(issue_n > 0)) {
              set_check_tab_status(cfg$value, "red")
            } else if (nrow(df) > 0) {
              set_check_tab_status(cfg$value, "green")
            } else {
              set_check_tab_status(cfg$value, "none")
            }
          }
        }
      }
    }

    invisible(TRUE)
  }

  # TABULATOR (single sheet) ----------------------------------------------------------------
  observeEvent(input$run_tab, {
    req(input$sheet_id)

    tryCatch(
      {
        clear_checks_outputs()

        sheet <- input$sheet_id
        sheet_rv(sheet)

        cell_results <- build_cell_results_for_sheet(sheet)
        cell_results_rv(cell_results)

        format_type <- switch(input$format_type,
          "view"        = "view",
          "formatted"   = TRUE,
          "unformatted" = FALSE,
          "view"
        )

        tb <- pivot_table(
          table = cell_results,
          formatted = format_type,
          type = input$pivot_type
        )

        tb_rv(tb)

        showNotification("Done.", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # EXCEL: file creation logic -------------------------------------------------------
  replace_time_tag_in_path <- function(p, tag) {
    p <- as.character(p)
    ext <- tools::file_ext(p)

    if (tolower(ext) == "xlsx") {
      base <- sub("\\.xlsx$", "", p, ignore.case = TRUE)
    } else {
      base <- p
    }

    base <- sub("(_\\d{14})+$", "", base)
    paste0(base, "_", tag, ".xlsx")
  }

  observeEvent(input$create_excel_files, {
    tryCatch(
      {
        req(input$excel_target)
        path_tab_excel <- get_current_tab_path()

        need_output <- input$excel_target %in% c("output", "both")
        need_formatted <- input$excel_target %in% c("formatted", "both")

        current_tag <- format(Sys.time(), "%Y%m%d%H%M%S")
        assign("time_tag", current_tag, envir = engine_env)

        if (need_output && !is.null(dest_rv())) {
          new_dest <- replace_time_tag_in_path(dest_rv(), current_tag)

          dir.create(dirname(new_dest), recursive = TRUE, showWarnings = FALSE)
          ok <- file.copy(path_tab_excel, new_dest, overwrite = FALSE)
          if (!isTRUE(ok)) stop("Failed to create output workbook at: ", new_dest)

          dest_rv(new_dest)
        }

        if (need_formatted && !is.null(dest_f_rv())) {
          new_dest_f <- replace_time_tag_in_path(dest_f_rv(), current_tag)

          dir.create(dirname(new_dest_f), recursive = TRUE, showWarnings = FALSE)
          ok <- file.copy(path_tab_excel, new_dest_f, overwrite = FALSE)
          if (!isTRUE(ok)) stop("Failed to create formatted workbook at: ", new_dest_f)

          dest_f_rv(new_dest_f)
        }
        if ((need_output && is.null(dest_rv())) || (need_formatted && is.null(dest_f_rv()))) {
          meta <- survey_meta_rv()
          created <- micsPlusTableR::create_excel_workbooks(
            path_tab_excel = path_tab_excel,
            output_dir = excel_root,
            country_code = meta$country_abb,
            period = meta$period,
            wave = meta$wave,
            target = input$excel_target,
            time_tag = current_tag
          )
          if (need_output && is.null(dest_rv())) dest_rv(created$output)
          if (need_formatted && is.null(dest_f_rv())) dest_f_rv(created$formatted)
        }


        sync_dest_globals()

        showNotification(
          paste0(
            "Excel file(s) created.\n",
            if (need_output) paste0("Output: ", dest_rv(), "\n") else "",
            if (need_formatted) paste0("Formatted: ", dest_f_rv()) else ""
          ),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  observeEvent(input$write_output_table, {
    tryCatch(
      {
        req(dest_rv(), sheet_rv(), cell_results_rv())

        sync_dest_globals()

        write_to_excel(dest = dest_rv(), sheet = sheet_rv(), table = cell_results_rv())
        showNotification(paste("Written to Output:", dest_rv()), type = "message")
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  observeEvent(input$write_formatted_table, {
    tryCatch(
      {
        req(dest_f_rv(), sheet_rv(), cell_results_rv())

        sync_dest_globals()

        write_to_excel(
          dest = dest_f_rv(),
          sheet = sheet_rv(),
          table = cell_results_rv(),
          formatted = TRUE,
          drop_n_unw = TRUE
        )
        showNotification(paste("Written to Formatted:", dest_f_rv()), type = "message")
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # CONSISTENCY CHECKS (UI button) ---------------------------------------------------
  observeEvent(input$run_checks, {
    tryCatch(
      {
        req(cell_results_rv())
        x <- cell_results_rv()

        chk_row_group_n_rv(NULL)
        chk_row_group_p_rv(NULL)
        chk_col_group_n_rv(NULL)
        chk_col_group_p_rv(NULL)
        chk_row_indent_n_rv(NULL)
        chk_row_indent_p_rv(NULL)

        run_checks_core(x, update_ui = TRUE)
        showNotification("Checks completed.", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # RUN AND WRITE: Select all / Deselect all ------------------------------------------
  observeEvent(input$select_all_sheets, {
    all_sheets <- sheets_all_rv()

    if (length(all_sheets) == 0) {
      showNotification("No sheets available to select.", type = "warning")
      return()
    }

    updateSelectizeInput(session, "sheet_ids_multi", selected = all_sheets)
  })

  observeEvent(input$deselect_all_sheets, {
    updateSelectizeInput(session, "sheet_ids_multi", selected = character(0))
  })

  # <U+2705> Scroll after each refresh
  observeEvent(run_all_results_rv(),
    {
      df <- run_all_results_rv()
      if (is.null(df)) {
        return()
      }
      session$sendCustomMessage("scrollRunAllTable", list())
    },
    ignoreInit = TRUE
  )

  # <U+2705> Finish popup
  observeEvent(run_all_done_rv(),
    {
      if (!isTRUE(run_all_done_rv())) {
        return()
      }

      showModal(
modalDialog(
  title = "✅ Run-and-write finished",
  tags$pre(style = "white-space: pre-wrap;", run_all_done_msg_rv()),
  easyClose = TRUE,
  footer = modalButton("OK")
)
      )

      run_all_done_rv(FALSE)
    },
    ignoreInit = TRUE
  )

  # RUN AND WRITE (LIVE UPDATES PER SHEET) -------------------------------------------
  run_all_queue_rv <- reactiveVal(character(0))
  run_all_idx_rv <- reactiveVal(0)
  run_all_target_rv <- reactiveVal("both")

  fmt_elapsed <- function(sec) {
    sec <- as.numeric(sec)
    if (is.na(sec) || sec < 0) {
      return(NA_character_)
    }
    m <- floor(sec / 60)
    s <- floor(sec %% 60)
    sprintf("%02d:%02d", m, s)
  }

  observeEvent(input$run_all_btn, {
    req(input$sheet_ids_multi, input$write_target_all)

    if (isTRUE(run_all_running_rv())) {
      showNotification("Run-and-write is already running.", type = "warning")
      return()
    }

    run_all_running_rv(TRUE)
    run_all_start_rv(Sys.time())
    run_all_step_rv("Starting…")
    run_all_pct_rv(0)

    run_all_queue_rv(input$sheet_ids_multi)
    run_all_idx_rv(0)
    run_all_target_rv(input$write_target_all)

    run_all_results_rv(data.frame(
      sheet = character(0),
      table_name = character(0),
      status = character(0),
      elapsed = character(0), # <U+2705> NEW
      message = character(0),
      consistency_issues = character(0),
      stringsAsFactors = FALSE
    ))

    ensure_destinations <- function() {
      need_output <- run_all_target_rv() %in% c("output", "both")
      need_formatted <- run_all_target_rv() %in% c("formatted", "both")
      path_tab_excel <- get_current_tab_path()

      if ((need_output && is.null(dest_rv())) || (need_formatted && is.null(dest_f_rv()))) {
        meta <- survey_meta_rv()
        created <- micsPlusTableR::create_excel_workbooks(
          path_tab_excel = path_tab_excel,
          output_dir = excel_root,
          country_code = meta$country_abb,
          period = meta$period,
          wave = meta$wave,
          target = run_all_target_rv()
        )
        if (need_output && is.null(dest_rv())) dest_rv(created$output)
        if (need_formatted && is.null(dest_f_rv())) dest_f_rv(created$formatted)
      }

      sync_dest_globals()
    }

    process_next_sheet <- function() {
      isolate({
        if (!isTRUE(run_all_running_rv())) {
          return()
        }

        queue <- run_all_queue_rv()
        idx <- run_all_idx_rv()
        target <- run_all_target_rv()
        n <- length(queue)

        if (n == 0 || idx >= n) {
          run_all_pct_rv(100)
          run_all_step_rv("Finished.")
          run_all_running_rv(FALSE)

          total_sec <- as.numeric(difftime(Sys.time(), run_all_start_rv(), units = "secs"))
          msg <- paste0(
            "All selected sheets are finished.\n\n",
            "Total sheets: ", n, "\n",
            "Total elapsed: ", fmt_elapsed(total_sec), "\n",
            "Target: ", target
          )
          run_all_done_msg_rv(msg)
          run_all_done_rv(TRUE)

          return()
        }

        i <- idx + 1
        sheet <- queue[[i]]

        run_all_step_rv(paste0("Sheet ", i, "/", n, ": ", sheet))
        run_all_pct_rv(round(100 * (i - 1) / max(1, n)))

        dest <- dest_rv()
        destf <- dest_f_rv()

        need_output <- target %in% c("output", "both")
        need_formatted <- target %in% c("formatted", "both")

        if (need_output && is.null(dest)) stop("Output destination is NULL. Create/select output workbook first.")
        if (need_formatted && is.null(destf)) stop("Formatted destination is NULL. Create/select formatted workbook first.")

        sheet_start <- Sys.time()

        row_out <- tryCatch(
          {
            assign("any_issues_found", FALSE, envir = engine_env)

            cell_results <- build_cell_results_for_sheet(sheet)
            tname <- table_name_rv()


            run_checks_core(cell_results, update_ui = FALSE)

            wrote <- character(0)

            if (need_output) {
              sync_dest_globals()
              write_to_excel(dest = dest, sheet = sheet, table = cell_results)
              wrote <- c(wrote, "output")
            }

            if (need_formatted) {
              sync_dest_globals()
              write_to_excel(
                dest = destf,
                sheet = sheet,
                table = cell_results,
                formatted = TRUE,
                drop_n_unw = TRUE
              )
              write_footnotes(df = cell_results, sheet = sheet)
              wrote <- c(wrote, "formatted")
            }

print(tname)
print(sheet)


            consistency_flag <- tryCatch(
              {
                if (exists("any_issues_found", envir = engine_env, inherits = FALSE)) {
                  isTRUE(get("any_issues_found", envir = engine_env))
                } else {
                  FALSE
                }
              },
              error = function(e) FALSE
            )

            consistency_text <- if (isTRUE(consistency_flag)) "Not consistent" else "no consistency issues"

            sheet_sec <- as.numeric(difftime(Sys.time(), sheet_start, units = "secs"))



            data.frame(
              sheet = sheet,
              table_name = as.character(tname),
              status = "OK",
              elapsed = fmt_elapsed(sheet_sec),
              message = paste("Wrote:", paste(wrote, collapse = " + ")),
              consistency_issues = consistency_text,
              stringsAsFactors = FALSE
            )
          },
          error = function(e) {
            sheet_sec <- as.numeric(difftime(Sys.time(), sheet_start, units = "secs"))

            data.frame(
              sheet = sheet,
              table_name = NA_character_,
              status = "ERROR",
              elapsed = fmt_elapsed(sheet_sec),
              message = e$message,
              consistency_issues = NA_character_,
              stringsAsFactors = FALSE
            )
          }
        )

        cur <- run_all_results_rv()
        run_all_results_rv(rbind(cur, row_out))

        run_all_idx_rv(i)

        later::later(process_next_sheet, 0)
      })
    }

    tryCatch(
      {
        ensure_destinations()
        showNotification("Run-and-write started…", type = "message")

        later::later(process_next_sheet, 0)
      },
      error = function(e) {
        run_all_running_rv(FALSE)
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # OUTPUTS -------------------------------------------------------------------------
  output$table_name_tab1 <- renderText({
    if (is.null(table_name_rv())) "" else table_name_rv()
  })
  output$table_name_tab2 <- renderText({
    if (is.null(table_name_rv())) "" else table_name_rv()
  })

  output$pivot_preview <- renderUI({
    tb <- tb_rv()
    req(tb)

    df <- if (is.data.frame(tb)) tb else as.data.frame(tb)

    # We assign n_unw for non-labelled columns
    ft <- make_flextable_allow_dup_cols(df, blank_label = "n_unw")

    ft <- flextable::compose(ft, i = 1, j = 1, part = "header", value = flextable::as_paragraph(""))

    # indices
    n_body <- nrow(df)
    n_cols <- ncol(df)
    num_cols <- if (n_cols >= 2) 2:n_cols else integer(0)
    stripe_i <- if (n_body >= 2) seq(2, n_body, by = 2) else integer(0)

    add_upper_header_from_ranges <- function(
        ft, hdr_ranges,
        column_map,
        stub_n = 1, # number of left stub columns in ft (your table has 1)
        level = 2,
        line_color = "#666666",
        line_width = 2,
        row_height = 0.28,
        row_padding = 6,
        seg_gap = 18) {
      if (is.null(hdr_ranges) || NROW(hdr_ranges) == 0) {
        return(ft)
      }
      if (is.null(column_map) || length(column_map) == 0) {
        return(ft)
      }

      level_bg <- switch(as.character(level),
        "4" = "#e3e7ef",
        "3" = "#eef1f6",
        "2" = "#f6f7fa",
        "#f2f2f2"
      )

      n <- ncol(ft$body$dataset)

      df <- as.data.frame(hdr_ranges, stringsAsFactors = FALSE)
      if (!all(c("label", "col_start", "col_end") %in% names(df))) {
        return(ft)
      }

      flatten1 <- function(x) {
        while (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
        x
      }

      label <- as.character(flatten1(df$label))
      col_start <- as.character(flatten1(df$col_start))
      col_end <- as.character(flatten1(df$col_end))

      # map col_index -> table_column (1..K)
      start_tc <- suppressWarnings(as.integer(unname(column_map[col_start])))
      end_tc <- suppressWarnings(as.integer(unname(column_map[col_end])))

      ok <- !is.na(label) & nzchar(label) & !is.na(start_tc) & !is.na(end_tc)
      if (!any(ok)) {
        return(ft)
      }

      label <- label[ok]
      start_tc <- start_tc[ok]
      end_tc <- end_tc[ok]

      # convert table_column -> flextable column index
      # (shift by stub columns)
      start <- start_tc + stub_n
      end <- end_tc + stub_n

      # clamp to valid ft columns
      start <- pmax(1L, pmin(start, n))
      end <- pmax(1L, pmin(end, n))

      keep <- start <= end
      if (!any(keep)) {
        return(ft)
      }

      start <- start[keep]
      end <- end[keep]
      label <- label[keep]

      # identify blank columns (not covered by any span)
      covered <- rep(FALSE, n)
      for (k in seq_along(start)) covered[start[k]:end[k]] <- TRUE
      blank_cols <- which(!covered)

      # unique placeholders so blanks don't merge
      values <- paste0(".__blank__", seq_len(n))
      for (k in seq_along(label)) values[start[k]:end[k]] <- label[k]

      ft <- ft |>
        flextable::add_header_row(values = values, top = TRUE) |>
        flextable::merge_h(i = 1, part = "header")

      # wipe placeholders only where no header span exists
      if (length(blank_cols) > 0) {
        ft <- flextable::compose(
          ft,
          i = 1, j = blank_cols, part = "header",
          value = flextable::as_paragraph("")
        )
      }

      ft <- ft |>
        flextable::bold(i = 1, part = "header") |>
        flextable::bg(i = 1, part = "header", bg = level_bg) |>
        flextable::align(i = 1, part = "header", align = "center") |>
        flextable::padding(i = 1, part = "header", padding = row_padding) |>
        flextable::height(i = 1, part = "header", height = row_height)

      # remove bottom border everywhere on this row first
      ft <- ft |>
        flextable::border(
          i = 1, j = 1:n, part = "header",
          border.bottom = officer::fp_border(color = "transparent", width = 0)
        )

      # segments ordered
      ord <- order(start, end)
      start2 <- start[ord]
      end2 <- end[ord]
      K <- length(start2)

      if (K > 0) {
        # underline each segment anchor
        for (k in seq_len(K)) {
          ft <- ft |>
            flextable::border(
              i = 1, j = start2[k], part = "header",
              border.bottom = officer::fp_border(color = line_color, width = line_width)
            )
        }

        # gap between segments via thick white right border at boundary
        gap_border <- officer::fp_border(color = "#ffffff", width = line_width + 2)

        for (k in seq_len(K - 1)) {
          boundary_col <- end2[k]
          ft <- ft |>
            flextable::border(
              i = 1, j = boundary_col, part = "header",
              border.right = gap_border
            )
        }
      }

      ft
    }


    add_multi_upper_headers <- function(
        ft,
        header_2 = NULL, header_3 = NULL, header_4 = NULL,
        column_map,
        stub_n = 1) {
      if (!is.null(header_2) && NROW(header_2) > 0) {
        ft <- add_upper_header_from_ranges(
          ft, header_2,
          column_map = column_map,
          stub_n = stub_n,
          level = 2,
          line_color = "#6f6f6f", line_width = 2,
          seg_gap = 12
        )
      }

      if (!is.null(header_3) && NROW(header_3) > 0) {
        ft <- add_upper_header_from_ranges(
          ft, header_3,
          column_map = column_map,
          stub_n = stub_n,
          level = 3,
          line_color = "#5f5f5f", line_width = 2,
          seg_gap = 20
        )
      }

      if (!is.null(header_4) && NROW(header_4) > 0) {
        ft <- add_upper_header_from_ranges(
          ft, header_4,
          column_map = column_map,
          stub_n = stub_n,
          level = 4,
          line_color = "#4a4a4a", line_width = 3,
          seg_gap = 24
        )
      }

      ft
    }


    # use in the pipeline ----------------------------------------------------------------
    ft <- ft |>
      flextable::autofit() |>
      flextable::font(fontname = "Arial", part = "all") |>
      flextable::fontsize(size = 12, part = "all") |>
      flextable::padding(padding = 3, part = "all") |>
      flextable::valign(valign = "top", part = "all") |>
      flextable::border_remove() |>
      # IMPORTANT: do NOT draw global lines on "all"
      flextable::hline_top(part = "body", border = officer::fp_border(width = 1)) |>
      flextable::hline_bottom(part = "body", border = officer::fp_border(width = 1)) |>
      flextable::bold(part = "header") |>
      flextable::bg(part = "header", bg = "#f2f2f2") |>
      flextable::align(j = 1, align = "left", part = "header") |>
      flextable::align(j = num_cols, align = "center", part = "header") |>
      flextable::align(j = num_cols, align = "right", part = "body") |>
      flextable::bg(j = 1, bg = "#f0f0f0", part = "body") |>
      flextable::bg(i = stripe_i, j = num_cols, bg = "#e6e6e6", part = "body") |>
      flextable::hline(part = "body", border = officer::fp_border(color = "#dddddd", width = 0.5))

    cm <- column_map_rv()
    out_glob <- get("out_glob", envir = engine_env, inherits = FALSE)


    ft <- add_multi_upper_headers(
      ft,
      header_2    = out_glob$header_2,
      header_3    = out_glob$header_3,
      header_4    = out_glob$header_4,
      column_map  = cm,
      stub_n      = 1
    )




    flextable::htmltools_value(ft)
  })





  make_tbl <- function(rv) {
    renderReactable({
      df <- rv()
      req(df)
      df2 <- filter_inconsistent(df, tol = TOL)
      reactable(df2,
        searchable = TRUE, striped = TRUE,
        height = 650,
        resizable = TRUE,
        outlined = FALSE,
        highlight = TRUE, bordered = TRUE, defaultPageSize = 1000,
        # Selection options (100, 500, 1000)
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(100, 500, 1000)
      )
    })
  }

  output$tbl_row_group_n <- make_tbl(chk_row_group_n_rv)
  output$tbl_row_group_p <- make_tbl(chk_row_group_p_rv)
  output$tbl_col_group_n <- make_tbl(chk_col_group_n_rv)
  output$tbl_col_group_p <- make_tbl(chk_col_group_p_rv)
  output$tbl_row_indent_n <- make_tbl(chk_row_indent_n_rv)
  output$tbl_row_indent_p <- make_tbl(chk_row_indent_p_rv)

  output$run_all_results_tbl <- renderReactable({
    df <- run_all_results_rv()
    req(df)

    reactable(
      df,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultPageSize = 50,
      columns = list(
        status = colDef(style = function(value) list(fontWeight = "bold")),
        elapsed = colDef(name = "Time elapsed", minWidth = 90),
        message = colDef(minWidth = 300),
        consistency_issues = colDef(
          name = "Consistency issues",
          minWidth = 180,
          style = function(value) {
            if (identical(value, "Not consistent")) {
              list(background = "#FF0000", color = "white", fontWeight = "bold")
            } else {
              list()
            }
          }
        )
      )
    )
  })

  
  # DATA EXPLORATION (reactive-safe) --------------------------------------------------
  
  # DATA EXPLORATION (Frequency / Cross table)
  # Requires UI: radioButtons("analysis_type", choices = c("Frequency"="frequency","Cross table"="crosstab"))


  freq_rv <- reactiveVal(NULL)

  selected_dataset_rv <- reactive({
    req(hh_rv(), hl_rv(), input$select_data)
    if (identical(input$select_data, "hh")) hh_rv() else hl_rv()
  })

  weight_var_rv <- reactive({
    w <- as.character(input$select_weight %||% "")
    w <- stringr::str_trim(w)
    if (!nzchar(w)) {
      return(NULL)
    } # blank => unweighted
    w
  })


  analysis_mode_rv <- reactive({
    req(input$analysis_type)
    x <- tolower(trimws(as.character(input$analysis_type %||% "frequency")))
    if (x == "cross-table") "cross-table" else "frequency"
  })

  safe_filter_apply <- function(ds, txt) {
    txt <- as.character(txt %||% "")
    txt <- stringr::str_trim(txt)
    if (!nzchar(txt)) {
      return(ds)
    }

    expr <- tryCatch(
      rlang::parse_expr(txt),
      error = function(e) stop("Invalid filter expression: '", txt, "'\n", e$message)
    )

    tryCatch(
      dplyr::filter(ds, !!expr),
      error = function(e) stop("Filter failed for: '", txt, "'\n", e$message)
    )
  }
  build_freq_table <- function(ds, vars, weight_var = NULL) {
    vars <- as.character(vars %||% character(0))
    vars <- vars[nzchar(vars)]
    if (length(vars) == 0) {
      return(NULL)
    }

    vmap <- var_display_map(ds)
    ds_pretty <- make_value_label_df(ds) # labelled -> "code - (label)"

    # prepare weights once (if requested) ----------------------------------------------
    if (!is.null(weight_var)) {
      if (!weight_var %in% names(ds)) stop("Weight variable not found: ", weight_var)

      w <- ds[[weight_var]]

      # labelled weights -> numeric values
      if (haven::is.labelled(w)) w <- haven::zap_labels(w)

      # robust numeric coercion (handles characters etc.)
      w <- suppressWarnings(as.numeric(w))
      if (all(is.na(w))) stop("Weight variable '", weight_var, "' is not numeric (or all missing).")

      # NA weights contribute 0
      w[is.na(w)] <- 0
    }

    out <- lapply(vars, function(v) {
      if (!v %in% names(ds)) {
        return(NULL)
      }

      x <- ds_pretty[[v]]

      # make NA explicit like useNA="ifany"
      x_chr <- as.character(x)
      x_chr[is.na(x)] <- "(NA)"

      if (is.null(weight_var)) {
        # unweighted
        dfv <- dplyr::tibble(Value = x_chr) |>
          dplyr::count(Value, name = "Frequency", .drop = FALSE)
      } else {
        # weighted: sum weights within each category
        dfv <- dplyr::tibble(Value = x_chr, w = w) |>
          dplyr::group_by(Value, .drop = FALSE) |>
          dplyr::summarise(Frequency = sum(w, na.rm = TRUE), .groups = "drop")
      }

      # keep ordering similar to table() (optional)
      dfv <- dfv |>
        dplyr::mutate(Variable = vmap[[v]] %||% v) |>
        dplyr::select(Variable, Value, Frequency)

      dfv
    })

    out <- Filter(Negate(is.null), out)
    if (length(out) == 0) {
      return(NULL)
    }

    dplyr::bind_rows(out)
  }


  # build_crosstab --------------------------------------------------------------------
  build_crosstab <- function(ds, v1, v2, weight_var = NULL) {
    # variables exist -----------------------------------------------------------------
    if (!v1 %in% names(ds)) stop("Variable not found: ", v1)
    if (!v2 %in% names(ds)) stop("Variable not found: ", v2)

    vmap <- var_display_map(ds)
    ds_pretty <- make_value_label_df(ds)

    # make NA explicit (like useNA="ifany") --------------------------------------------
    x1 <- as.character(ds_pretty[[v1]])
    x2 <- as.character(ds_pretty[[v2]])
    x1[is.na(ds_pretty[[v1]])] <- "(NA)"
    x2[is.na(ds_pretty[[v2]])] <- "(NA)"

    # weights (optional) ----------------------------------------------------------------
    w <- NULL
    if (!is.null(weight_var)) {
      weight_var <- as.character(weight_var)
      weight_var <- stringr::str_trim(weight_var)

      # treat blank as unweighted
      if (!nzchar(weight_var)) {
        weight_var <- NULL
      } else {
        if (!weight_var %in% names(ds)) stop("Weight variable not found: ", weight_var)

        w <- ds[[weight_var]]
        if (haven::is.labelled(w)) w <- haven::zap_labels(w)

        w <- suppressWarnings(as.numeric(w))
        if (all(is.na(w))) stop("Weight variable '", weight_var, "' is not numeric (or all missing).")

        # NA weights contribute 0
        w[is.na(w)] <- 0
      }
    }

    # build Frequency (unweighted or weighted) -----------------------------------------
    base <- dplyr::tibble(Value1 = x1, Value2 = x2)

    if (is.null(weight_var)) {
      df <- base |>
        dplyr::count(Value1, Value2, name = "Frequency", .drop = FALSE)
    } else {
      df <- dplyr::mutate(base, w = w) |>
        dplyr::group_by(Value1, Value2, .drop = FALSE) |>
        dplyr::summarise(Frequency = sum(w, na.rm = TRUE), .groups = "drop")
    }

    # percents ------------------------------------------------------------------------
    total <- sum(df$Frequency, na.rm = TRUE)
    df$Percent_total <- if (total > 0) 100 * df$Frequency / total else NA_real_

    df <- df |>
      dplyr::group_by(Value1, .drop = FALSE) |>
      dplyr::mutate(
        Percent_row = {
          denom <- sum(Frequency, na.rm = TRUE)
          if (denom > 0) 100 * Frequency / denom else NA_real_
        }
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(Value2, .drop = FALSE) |>
      dplyr::mutate(
        Percent_col = {
          denom <- sum(Frequency, na.rm = TRUE)
          if (denom > 0) 100 * Frequency / denom else NA_real_
        }
      ) |>
      dplyr::ungroup()

    # add pretty variable labels -------------------------------------------------------
    df |>
      dplyr::mutate(
        Variable1 = vmap[[v1]] %||% v1,
        Variable2 = vmap[[v2]] %||% v2
      ) |>
      dplyr::select(
        Variable1, Variable2, Value1, Value2, Frequency,
        Percent_total, Percent_row, Percent_col
      )
  }



  observeEvent(input$run_data_analysis, {
    tryCatch(
      {
        ds <- selected_dataset_rv()
        req(ds)

        ds <- safe_filter_apply(ds, input$filter_area1)
        ds <- safe_filter_apply(ds, input$filter_area2)

        vars <- as.character(input$select_variable %||% character(0))
        vars <- vars[nzchar(vars)]

        mode <- analysis_mode_rv()

        if (identical(mode, "frequency")) {
          ft <- build_freq_table(ds, vars, weight_var = weight_var_rv())


          if (is.null(ft) || nrow(ft) == 0) {
            freq_rv(data.frame(
              Variable = character(0),
              Value = character(0),
              Frequency = integer(0),
              stringsAsFactors = FALSE
            ))
            showNotification("No results (check filters / selected variables).", type = "warning")
          } else {
            freq_rv(ft)
            showNotification("Frequency table created.", type = "message")
          }
        } else if (identical(mode, "cross-table")) { # crosstab

          if (length(vars) != 2) {
            freq_rv(NULL)
            showNotification("Cross table requires exactly TWO variables selected.", type = "warning")
            return()
          }

          ct <- build_crosstab(ds, vars[[1]], vars[[2]], weight_var = weight_var_rv())
          freq_rv(ct)
          showNotification("Cross table created.", type = "message")
        }
      },
      error = function(e) {
        freq_rv(NULL)
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })


  output$freq_table_df <- renderReactable({
    df <- freq_rv()
    req(df)

    # Detect which table we are showing
    is_freq <- all(c("Variable", "Value", "Frequency") %in% names(df))
    is_ct <- all(c("Variable1", "Variable2", "Value1", "Value2", "Frequency") %in% names(df))

    if (is_freq) {
      reactable::reactable(
        df,
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 1000,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(100, 500, 1000),
        columns = list(
          Variable  = reactable::colDef(minWidth = 140),
          Value     = reactable::colDef(minWidth = 180),
          Frequency = reactable::colDef(minWidth = 110)
        )
      )
    } else if (is_ct) {
      # optional: pretty rounding for perc columns if they exist
      for (p in c("Percent_total", "Percent_row", "Percent_col")) {
        if (p %in% names(df)) df[[p]] <- round(as.numeric(df[[p]]), 2)
      }

      reactable::reactable(
        df,
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 1000,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(100, 500, 1000),
        columns = list(
          Variable1      = reactable::colDef(name = "Variable 1", minWidth = 120),
          Variable2      = reactable::colDef(name = "Variable 2", minWidth = 120),
          Value1         = reactable::colDef(name = "Value 1", minWidth = 160),
          Value2         = reactable::colDef(name = "Value 2", minWidth = 160),
          Frequency      = reactable::colDef(minWidth = 100),
          Percent_total  = reactable::colDef(name = "% total", minWidth = 100),
          Percent_row    = reactable::colDef(name = "% row", minWidth = 100),
          Percent_col    = reactable::colDef(name = "% col", minWidth = 100)
        )
      )
    } else {
      # fallback if something unexpected was stored
      reactable::reactable(
        df,
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 1000,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(100, 500, 1000),
      )
    }
  })

  # DATA VIEW (reactive-safe) ---------------------------------------------------------
  # Requires UI: radioButtons("select_data_view", "Select data to view", choices = c("hh" = "hh", "hl" = "hl"), selected = "hh")


  # SELECTED DATASET (hh/hl) ----
  selected_dataset_view_rv <- reactive({
    req(input$select_data_view)

    if (identical(input$select_data_view, "hh")) {
      req(hh_rv(), hh_new_rv())
      hh_new_rv()
    } else {
      req(hl_rv(), hl_new_rv())
      hl_new_rv()
    }
  })


  # UPDATE VARIABLE LIST WHEN DATASET CHANGES ----
  observeEvent(selected_dataset_view_rv(),
    {
      ds <- selected_dataset_view_rv()

      choices <- make_var_choices(ds) # <U+0001F448> show name - (label)

      current <- isolate(input$select_variable2)
      if (is.null(current)) current <- character(0)
      keep <- intersect(as.character(current), unname(choices))

      updateSelectInput(
        session,
        "select_variable2",
        choices  = choices,
        selected = keep
      )
    },
    ignoreInit = FALSE
  )



  observeEvent(input$run_data_view, {
    tryCatch(
      {
        ds <- selected_dataset_view_rv()
        req(ds)

        vars <- input$select_variable2
        if (is.null(vars)) vars <- character(0)
        vars <- as.character(vars)
        vars <- vars[nzchar(vars)]
        vars <- intersect(vars, names(ds))
        if (length(vars) > 0) ds <- ds[, vars, drop = FALSE]


        data_view_rv(ds)
      },
      error = function(e) {
        showNotification(paste("Data View error:", conditionMessage(e)), type = "error")
      }
    )
  })

  observeEvent(selected_dataset_view_rv(),
    {
      if (is.null(data_view_rv())) data_view_rv(selected_dataset_view_rv())
    },
    ignoreInit = FALSE
  )

  observeEvent(input$run_long_format_data, {
    current_sheet <- NULL
    stage <- "initializing"
    tryCatch(
      {
        path_tab_excel <- get_current_tab_path()
        meta <- survey_meta_rv()
        sdmx_config <- new_mics_sdmx_config(
          country = meta$country,
          ref_area = meta$country_abb,
          period = meta$period,
          wave = meta$wave
        )

        sheets_to_run <- sheets_all_rv()
        req(length(sheets_to_run) > 0)

        res_list <- vector("list", length(sheets_to_run))

        for (i in seq_along(sheets_to_run)) {
          sheet <- sheets_to_run[[i]]
          current_sheet <- sheet
          stage <- "reading the tabulation plan"

          clear_previous()
          assign("sheet", sheet, envir = engine_env)

          ensure_current_workbook()
          read_tabulation(path_tab_excel, sheet)

          # normalize extra table defaults safely
          if (is.null(get0("tables_extra", envir = engine_env, inherits = FALSE))) {
            tables_extra <- character(0)
            assign("tables_extra", tables_extra, envir = engine_env)
          }

          if (is.null(get0("extra_tables_dict", envir = engine_env, inherits = FALSE))) {
            extra_tables_dict <- list()
            assign("extra_tables_dict", extra_tables_dict, envir = engine_env)
          }

          tables_extra0 <- get("tables_extra", envir = engine_env, inherits = FALSE)
          extra_dict0 <- get("extra_tables_dict", envir = engine_env, inherits = FALSE)

          stage <- "calculating cells"
          # regular table
          if (length(tables_extra0) == 0 || !(sheet %in% tables_extra0)) {
            cell_results <- tabulate_mics()
          } else {
            # extra table logic
            key <- extra_dict0[[sheet]]

            if (is.null(key) || length(key) != 1 || !nzchar(key)) {
              stop(sprintf("extra_tables_dict has no valid entry for sheet '%s'.", sheet))
            }

            if (!exists(key, envir = engine_env, inherits = FALSE)) {
              stop(sprintf("Extra table object '%s' not found in engine_env for sheet '%s'.", key, sheet))
            }

            table_new <- get(key, envir = engine_env, inherits = FALSE)
            cell_results <- tabulate_extra_table(table_new)
          }

          # Convert the engine's legacy cells to the standardized 35-variable
          # MICS/SDMX contract while this sheet's parsed header context is live.
          stage <- "converting to long format"
          cell_results <- as_sdmx_compatible(
            cell_results,
            config = sdmx_config,
            table_id = sheet,
            table_context = get("out_glob", envir = engine_env, inherits = FALSE)
          )

          res_list[[i]] <- cell_results

          showNotification(
            paste0("Prepared long format table ", i, "/", length(sheets_to_run), ": ", sheet),
            type = "message",
            duration = 2
          )
        }

        current_sheet <- NULL
        stage <- "combining and validating tables"
        cell_results_all <- dplyr::bind_rows(res_list)
        validate_sdmx_compatible(cell_results_all)

        assign("cell_results_all", cell_results_all, envir = engine_env)
        long_format_metadata_rv(sdmx_config)
        cell_results_all_rv(cell_results_all)

        showNotification(
          paste("Long format data completed for", length(sheets_to_run), "sheet(s)."),
          type = "message"
        )
      },
      error = function(e) {
        if (inherits(e, "shiny.silent.error")) return(invisible(NULL))
        table_context <- if (is.null(current_sheet)) "" else {
          paste0(" in table '", current_sheet, "'")
        }
        showNotification(
          paste0("Long format data error", table_context, " while ", stage,
                 ":\n", conditionMessage(e)),
          type = "error", duration = NULL
        )
      }
    )
  })
  # OUTPUT TABLE ---------------------------------------------------------------------
  output$data_view_table <- renderReactable({
    ds <- data_view_rv()
    req(ds)

    # Build "VAR - (Label)" display names, but keep real column names in ds
    choices <- make_var_choices(ds) # named vector: value=real var, name=display
    display_map <- setNames(names(choices), unname(choices))
    # display_map[["HH17"]] = "HH17 - (Some label)"

    reactable::reactable(
      ds,
      searchable = TRUE,
      filterable = TRUE,
      resizable = TRUE,
      wrap = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      showPageSizeOptions = TRUE,
      defaultPageSize = 100,
      pageSizeOptions = c(25, 50, 100),

      # <U+2705> show VAR - (Label) in headers
      columns = lapply(names(ds), function(v) {
        reactable::colDef(name = display_map[[v]] %||% v)
      }) |> rlang::set_names(names(ds)),

      # <U+2705> widen columns (applies to all columns unless overridden)
      defaultColDef = reactable::colDef(
        minWidth = 180,
        width    = 220,
        style    = list(whiteSpace = "normal")
      ),
      style = list(fontSize = "14px")
    )
  })

  output$data_long_format_table <- renderReactable({
    df <- cell_results_all_rv()

    if (is.null(df) || nrow(df) == 0) {
      df <- data.frame(Message = "No long format data prepared yet.")
    }

    reactable::reactable(
      df,
      searchable = TRUE,
      filterable = TRUE,
      resizable = TRUE,
      wrap = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultPageSize = 100,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50, 100, 500, 1000),
      defaultColDef = reactable::colDef(
        minWidth = 140,
        style = list(whiteSpace = "normal")
      ),
      style = list(fontSize = "14px")
    )
  })
}

shinyApp(ui, server)
