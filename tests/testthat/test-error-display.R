row_display_error <- function() {
  s <- small_plan_session()
  s$sheet <- "6.3"
  s$out_glob$tab_r <- tibble::tibble(row_index = 47L, row_lgc = "ch117relresp == 1")
  step <- s$out_glob$filter_row
  step$col_index <- 3L
  step$filter_condition <- NA_character_
  step$calculation <- "mutate(ea = 1) |> mutate(hhc = 1)"
  s$out_glob$filter_row <- dplyr::bind_rows(s$out_glob$filter_row, step)
  tryCatch(tabulate_h(s), error = identity)
}

test_that("row errors summarize the actual failing cell and preserve full details", {
  error <- row_display_error()
  expect_s3_class(error, "mics_user_error")
  original <- conditionMessage(error)
  display <- micsPlusTableR:::mics_error_display(error)
  expect_identical(display$summary, paste0(
    "Worksheet '6.3', cell B47:\nVariable 'ch117relresp' was not found.",
    "\nCondition: ch117relresp == 1"))
  expect_identical(display$details, original)
  expect_match(display$details, "mutate(hhc = 1)", fixed = TRUE)
  expect_false(grepl("horizontal block|calculation:|global filter:", display$summary))
  expect_identical(conditionMessage(error), original)
})

test_that("definition errors retain caller and definition coordinates without the chain", {
  definitions <- data.frame(id = "ea", code = "mutate(ea = absent_input)", row = 2L)
  instruction <- micsPlusTableR:::mics_parse_condition_cell(
    "define(ea)\nTRUE", 5, 3, "6.3", definitions)$calculation
  s <- small_plan_session()
  s$sheet <- "6.3"
  s$out_glob$filter_row$calculation <- instruction
  error <- tryCatch(tabulate_h(s), error = identity)
  display <- micsPlusTableR:::mics_error_display(error)
  expect_match(display$summary, "cell C5 -> Define!B2 (id 'ea')", fixed = TRUE)
  expect_match(display$summary, "absent_input", fixed = TRUE)
  expect_false(grepl("mics_apply_definition|calculation:", display$summary))
  expect_identical(display$details, conditionMessage(error))
})

test_that("unrecognized contexts fall back to the original message", {
  simple <- simpleError("A multiline failure\nwith essential details")
  wrap <- function(context) tryCatch(micsPlusTableR:::mics_abort_context(simple, context), error = identity)
  for (error in list(simple, wrap("Unknown subsystem"),
                    wrap("Worksheet '6.3'; Excel row 47, column 3; row condition: x == 1; column condition: y == 1"))) {
    display <- micsPlusTableR:::mics_error_display(error)
    expect_identical(display$summary, micsPlusTableR:::mics_error_message(error))
    expect_null(display$details)
  }
})

test_that("technical details are collapsed and error text is escaped", {
  error <- row_display_error()
  ui <- micsPlusTableR:::mics_error_notification_ui(error)
  expect_identical(ui$children[[2]]$name, "details")
  expect_null(ui$children[[2]]$attribs$open)
  expect_identical(ui$children[[2]]$children[[1]]$children[[1]], "Technical details")
  expect_identical(ui$children[[2]]$children[[2]]$children[[1]], conditionMessage(error))
  html <- as.character(micsPlusTableR:::mics_error_notification_ui(simpleError("<script>bad()</script>")))
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
})

test_that("the app delivers a short notification with expandable details and stays connected", {
  old <- options(micsPlusTableR.output_dir = tempfile("display-test-"))
  on.exit(options(old), add = TRUE)
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"), local = new.env())$value
  shiny::testServer(app, {
    notices <- list()
    session$sendNotification <- function(type, message) notices[[length(notices) + 1L]] <<- message
    session$setInputs(sheet_id = "6.3")
    # Exercise the Run Table handler and real notification/HTML processing.
    error_to_show <- row_display_error()
    assign("build_cell_results_for_sheet", function(sheet) stop(error_to_show),
      envir = environment(build_cell_results_for_sheet))
    session$setInputs(run_tab = 1)
    expect_false(session$isClosed())
    expect_length(notices, 1)
    expect_identical(notices[[1]]$type, "error")
    expect_null(notices[[1]]$duration)
    summary <- strsplit(notices[[1]]$html, "<details>", fixed = TRUE)[[1]][1]
    expect_match(summary, "cell B47", fixed = TRUE)
    expect_false(grepl("calculation:|mutate\\(", summary))
    expect_match(notices[[1]]$html, "Technical details", fixed = TRUE)
    expect_match(notices[[1]]$html, "mutate(hhc = 1)", fixed = TRUE)
    session$setInputs(run_tab = 2)
    expect_false(session$isClosed())
    expect_length(notices, 2)
  })
})
