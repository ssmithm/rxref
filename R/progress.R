#' Should this function show a progress bar?
#'
#' @keywords internal
#' @noRd
.rxref_show_progress <- function(show_progress, x, min_n = 5L) {
  isTRUE(show_progress) && length(x) >= min_n
}


#' Map with optional cli progress bar
#'
#' @keywords internal
#' @noRd
.rxref_progress_map_dfr <- function(
    .x,
    .f,
    ...,
    name = "Working",
    show_progress = interactive(),
    min_n = 5L
) {
  use_progress <- .rxref_show_progress(show_progress, .x, min_n = min_n)

  progress_id <- NULL

  if (use_progress) {
    progress_id <- cli::cli_progress_bar(
      name = name,
      total = length(.x),
      type = "iterator"
    )

    on.exit(cli::cli_progress_done(id = progress_id), add = TRUE)
  }

  purrr::map_dfr(.x, function(.val) {
    out <- .f(.val, ...)

    if (use_progress) {
      cli::cli_progress_update(id = progress_id)
    }

    out
  })
}
