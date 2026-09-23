# Missing medians and NaN estimates share the existing zero-base display marker.
# Keep the underlying estimates unchanged.
mics_missing_value_dash <- function(value, stat_type) {
  is_median <- grepl("^median(?:[_ ]?unw)?\\s*(?:\\(|$)",
                     trimws(stat_type), ignore.case = TRUE, perl = TRUE)
  is.nan(value) | (is_median & is.na(value))
}
