config <- list(
  max_upload_size = 500 * 1024^2,
  test_mode = isTRUE(getOption("anthro.debug"))
)

options(shiny.maxRequestSize = config$max_upload_size)

# check if test_mode is on
if (!config$test_mode) {
  options("lifecycle_verbosity" = "quiet")
}

flextable::set_flextable_defaults(
  split = TRUE,
  table_align = "left",
  table.layout = "autofit",
  big.mark = "",
  font.size = 8
)
