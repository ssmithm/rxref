#' rxref: Tidy RxNorm utilities (API-first)
#'
#' Core exported verbs: [resolve()], [get_properties()], [map_to()].
#' Configure behavior with [rxref_conf()] and [set_backend()].
#' @keywords internal
"_PACKAGE"


#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate select rename relocate arrange distinct filter left_join bind_rows bind_cols
#' @importFrom tidyr unnest unnest_wider unnest_longer replace_na
#' @importFrom purrr map map2 map_chr map_int map_lgl imap compact pluck possibly quietly
#' @importFrom stringr str_detect str_trim str_remove_all str_replace_all str_squish
#' @importFrom vctrs vec_recycle vec_cast vec_c vec_rbind
#' @importFrom jsonlite fromJSON
#' @importFrom memoise memoise cache_filesystem
#' @importFrom rlang .data
NULL
