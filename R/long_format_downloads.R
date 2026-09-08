# Register downloads against the same completed dataset used by the preview.
mics_long_format_downloads <- function(output, data, metadata) {
  output$long_format_download_ready <- shiny::renderText({
    result <- data()
    if (is.data.frame(result) && nrow(result) > 0L) "true" else "false"
  })
  shiny::outputOptions(output, "long_format_download_ready", suspendWhenHidden = FALSE)

  download_data <- shiny::reactive({
    result <- data()
    shiny::req(is.data.frame(result), nrow(result) > 0L)
    validate_sdmx_compatible(result)
    result
  })
  filename <- function(extension) {
    download_data()
    meta <- metadata()
    shiny::req(meta$ref_area, meta$period, meta$wave)
    # Match the input filenames, preserving the full survey period and wave.
    wave <- gsub("[[:space:]]+", "", trimws(meta$wave))
    stem <- paste(meta$ref_area, "MICSPlus", meta$period, wave)
    stem <- gsub('[[:cntrl:]<>:"/\\\\|?*]', "_", stem)
    paste0(stem, "_LongFormatData_", format(Sys.Date(), "%Y%m%d"), ".", extension)
  }

  output$download_long_format_excel <- shiny::downloadHandler(
    filename = function() filename("xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      openxlsx2::write_xlsx(
        download_data(), file = file, sheet = "Long Format Data",
        row_names = FALSE, first_row = TRUE, na = "", overwrite = TRUE
      )
    }
  )
  output$download_long_format_csv <- shiny::downloadHandler(
    filename = function() filename("csv"),
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      readr::write_excel_csv(download_data(), file = file, na = "")
    }
  )
  output$download_long_format_rds <- shiny::downloadHandler(
    filename = function() filename("rds"),
    contentType = "application/octet-stream",
    content = function(file) {
      saveRDS(download_data(), file = file)
    }
  )
}
