#' Map between NDCs and RxCUIs
#'
#' `map_to()` maps identifiers between National Drug Codes (NDCs) and RxNorm
#' Concept Unique Identifiers (RxCUIs).
#'
#' When mapping from NDC to RxCUI, NDCs may be supplied as 10-digit,
#' 11-digit, or hyphenated values. Input NDCs are normalized to 11-digit
#' format before querying RxNorm.
#'
#' When mapping from RxCUI to NDC, the `history` argument controls whether
#' only currently active NDCs are retrieved or whether historical NDC
#' associations are also included.
#'
#' @param x Character vector of NDCs or RxCUIs.
#' @param to Direction of mapping. One of `"rxcui"` or `"ndc"`.
#'   Use `"rxcui"` to map NDCs to RxCUIs and `"ndc"` to map RxCUIs to NDCs.
#' @param status For `to = "ndc"` only, optional character vector of NDC
#'   statuses to retain, such as `"ACTIVE"`, `"OBSOLETE"`, `"ALIEN"`, or
#'   `"UNKNOWN"`. If `NULL`, no status-based filtering is applied to the
#'   retrieved NDCs. Note that when `history = "active"`, RxNorm's active NDC
#'   endpoint is used, so returned NDCs are expected to be active even when
#'   `status = NULL`.
#' @param history For `to = "ndc"` only, the NDC association history to
#'   retrieve. One of `"active"`, `"direct"`, or `"all"`.
#'
#'   - `"active"` retrieves currently active NDCs associated with the supplied
#'     RxCUI.
#'   - `"direct"` retrieves NDCs ever directly associated with the supplied
#'     RxCUI.
#'   - `"all"` retrieves NDCs ever directly or indirectly associated with the
#'     supplied RxCUI, including associations through remapped or archived
#'     concepts.
#'
#'   If `NULL` and `to = "ndc"`, defaults to `"active"`. Ignored when
#'   `to = "rxcui"`.
#' @param show_progress Logical; if `TRUE`, show a progress bar for vectorized
#'   mapping operations. Defaults to `interactive()`.
#'
#' @return A tibble.
#'
#' For `to = "rxcui"`, returns one row per input NDC/RxCUI mapping with columns:
#' \describe{
#'   \item{input}{Original input NDC.}
#'   \item{ndc11}{Normalized 11-digit NDC.}
#'   \item{rxcui}{Mapped RxCUI, if found.}
#' }
#'
#' For `to = "ndc"` and `history = "active"`, returns one row per RxCUI/NDC
#' mapping with columns:
#' \describe{
#'   \item{rxcui}{Input RxCUI.}
#'   \item{ndc11}{Mapped NDC.}
#'   \item{ndc_status}{NDC status returned by RxNorm, when available.}
#' }
#'
#' For `to = "ndc"` and `history` equal to `"direct"` or `"all"`, the output
#' may also include:
#' \describe{
#'   \item{related_rxcui}{RxCUI associated with the historical NDC record. This
#'     may differ from the input RxCUI when historical or indirect associations
#'     are retrieved.}
#'   \item{ndc_start_date}{Start date of the NDC association, when available.}
#'   \item{ndc_end_date}{End date of the NDC association, when available.}
#' }
#'
#' @details
#' RxNorm distinguishes between currently active NDCs and historical NDC
#' associations. The default behavior, `history = "active"`, uses RxNorm's
#' active NDC endpoint and preserves the earlier behavior of `map_to()`.
#'
#' To retrieve obsolete, discontinued, or otherwise historical NDCs, use
#' `history = "direct"` or `history = "all"`. Setting `status = NULL` does not
#' by itself request historical NDCs; it only means that no status filter is
#' applied after NDCs are retrieved.
#'
#' @examples
#' \dontrun{
#' # Map an NDC to RxCUI
#' map_to("00093-7424-56", to = "rxcui")
#'
#' # Map an RxCUI to currently active NDCs
#' map_to("1049630", to = "ndc")
#'
#' # Map an RxCUI to all directly associated historical NDCs
#' map_to("1049630", to = "ndc", history = "direct")
#'
#' # Map an RxCUI to all historical NDCs and retain obsolete NDCs only
#' map_to("1049630", to = "ndc", history = "all", status = "OBSOLETE")
#' }
#'
#' @export
map_to <- function(
    x,
    to = c("rxcui", "ndc"),
    status = NULL,
    history = NULL,
    show_progress = interactive()
) {
  to <- match.arg(to)

  stopifnot(is.character(x))

  if (to == "ndc") {
    if (is.null(history)) {
      history <- "active"
    }

    history <- match.arg(history, c("active", "direct", "all"))
  } else {
    if (!is.null(history)) {
      cli::cli_warn("{.arg history} is only used when {.code to = \"ndc\"}; it will be ignored.")
    }

    if (!is.null(status)) {
      cli::cli_warn("{.arg status} is only used when {.code to = \"ndc\"}; it will be ignored.")
    }
  }

  progress_name <- if (to == "rxcui") {
    "Mapping NDCs to RxCUIs"
  } else {
    "Mapping RxCUIs to NDCs"
  }

  if (to == "rxcui") {
    .rxref_progress_map_dfr(
      x,
      function(ndc) {
        ndc11 <- ndc_to_11(ndc)

        if (is.na(ndc11) || !nzchar(ndc11)) {
          return(
            tibble::tibble(
              input = ndc,
              ndc11 = NA_character_,
              rxcui = NA_character_
            )
          )
        }

        res <- rx_get_json(
          "/rxcui",
          query = list(idtype = "NDC", id = ndc11)
        )

        rxs <- rx_pluck_chr(res, "idGroup", "rxnormId")

        rxs <- if (length(rxs)) {
          unlist(rxs, use.names = FALSE)
        } else {
          NA_character_
        }

        tibble::tibble(
          input = ndc,
          ndc11 = ndc11,
          rxcui = as.character(rxs)
        )
      },
      name = progress_name,
      show_progress = show_progress
    ) |>
      dplyr::distinct()
  } else {
    .rxref_progress_map_dfr(
      x,
      function(id) {
        if (is.na(id) || !nzchar(id)) {
          return(tibble::tibble(
            rxcui = id,
            ndc11 = NA_character_,
            ndc_status = NA_character_
          ))
        }

        if (history == "active") {
          res <- rx_get_json(paste0("/rxcui/", id, "/ndcs"))
          ndcs <- rx_pluck_chr(res, "ndcGroup", "ndcList", "ndc")

          ndcs <- if (length(ndcs)) {
            unlist(ndcs, use.names = FALSE)
          } else {
            NA_character_
          }

          out <- tibble::tibble(
            rxcui = id,
            ndc11 = ndcs
          )
        } else {
          history_value <- switch(
            history,
            direct = 1,
            all = 2
          )

          res <- rx_get_json(
            paste0("/rxcui/", id, "/allhistoricalndcs"),
            query = list(history = history_value)
          )

          hist <- rx_pluck_list(res, "historicalNdcConcept", "historicalNdcTime")

          if (is.null(hist) || !length(hist)) {
            out <- tibble::tibble(
              rxcui = id,
              related_rxcui = NA_character_,
              ndc11 = NA_character_,
              ndc_start_date = NA_character_,
              ndc_end_date = NA_character_
            )
          } else {
            out <- purrr::map_dfr(hist, function(h) {
              ndc_time <- rx_pluck_list(h, "ndcTime")

              if (is.null(ndc_time) || !length(ndc_time)) {
                return(tibble::tibble(
                  rxcui = id,
                  related_rxcui = rx_scalar_chr(rx_pluck_chr(h, "rxcui")),
                  ndc11 = NA_character_,
                  ndc_start_date = NA_character_,
                  ndc_end_date = NA_character_
                ))
              }

              purrr::map_dfr(ndc_time, function(nt) {
                ndcs <- rx_pluck_chr(nt, "ndc")

                ndcs <- if (length(ndcs)) {
                  unlist(ndcs, use.names = FALSE)
                } else {
                  NA_character_
                }

                tibble::tibble(
                  rxcui = id,
                  related_rxcui = rx_scalar_chr(rx_pluck_chr(h, "rxcui")),
                  ndc11 = as.character(ndcs),
                  ndc_start_date = rx_scalar_chr(rx_pluck_chr(nt, "startDate")),
                  ndc_end_date = rx_scalar_chr(rx_pluck_chr(nt, "endDate"))
                )
              })
            })
          }

          ndcs <- out$ndc11
        }

        stats <- purrr::map_chr(ndcs, function(n) {
          if (is.na(n) || !nzchar(n)) {
            return(NA_character_)
          }

          # hyphenate to 5-4-2 for the status endpoint
          ndc_h <- hyphenate_ndc_5_4_2(n)

          st <- tryCatch(
            rx_get_json("/ndcstatus", query = list(ndc = ndc_h)),
            error = function(e) NULL
          )

          # common JSON shape: ndcStatus$status
          status_value <- rx_scalar_chr(rx_pluck_chr(st, "ndcStatus", "status"))

          if (is.na(status_value) || !nzchar(status_value)) {
            return(NA_character_)
          }

          status_value
        })

        out$ndc_status <- stats

        if (!is.null(status)) {
          keep <- tolower(out$ndc_status) %in% tolower(status)
          out <- out[keep, , drop = FALSE]
        }

        out
      },
      name = progress_name,
      show_progress = show_progress
    ) |>
      dplyr::distinct()
  }
}


#' Map NDCs to RxCUIs
#'
#' Convenience wrapper around [map_to()] for mapping National Drug Codes
#' (NDCs) to RxNorm Concept Unique Identifiers (RxCUIs).
#'
#' NDCs may be supplied as 10-digit, 11-digit, or hyphenated values. Input
#' NDCs are normalized to 11-digit format before querying RxNorm.
#'
#' @param x Character vector of NDCs.
#' @param show_progress Logical; if `TRUE`, show a progress bar for vectorized
#'   mapping operations. Defaults to `interactive()`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{input}{Original input NDC.}
#'   \item{ndc11}{Normalized 11-digit NDC.}
#'   \item{rxcui}{Mapped RxCUI, if found.}
#' }
#'
#' @examples
#' \dontrun{
#' map_ndc_to_rxcui("00093-7424-56")
#' }
#'
#' @export
map_ndc_to_rxcui <- function(x, show_progress = interactive()) {
  map_to(x, to = "rxcui", show_progress = show_progress)
}


#' Map RxCUIs to NDCs
#'
#' Convenience wrapper around [map_to()] for mapping RxNorm Concept Unique
#' Identifiers (RxCUIs) to National Drug Codes (NDCs).
#'
#' By default, this function retrieves currently active NDCs associated with
#' the supplied RxCUIs. Use `history = "direct"` or `history = "all"` to
#' retrieve historical NDC associations.
#'
#' @param x Character vector of RxCUIs.
#' @param status Optional character vector of NDC statuses to retain, such as
#'   `"ACTIVE"`, `"OBSOLETE"`, `"ALIEN"`, or `"UNKNOWN"`. If `NULL`, no
#'   status-based filtering is applied to the retrieved NDCs. Note that
#'   `history = "active"` uses RxNorm's active NDC endpoint, so returned NDCs
#'   are expected to be active even when `status = NULL`.
#' @param history For `to = "ndc"` only, the NDC association history to
#'   retrieve. One of `"active"`, `"direct"`, or `"all"`.
#'
#'   - `"active"` retrieves currently active NDCs associated with the supplied
#'     RxCUI.
#'   - `"direct"` retrieves NDCs ever directly associated with the supplied
#'     RxCUI.
#'   - `"all"` retrieves NDCs ever directly or indirectly associated with the
#'     supplied RxCUI, including associations through remapped or archived
#'     concepts.
#'
#'   If `NULL` and `to = "ndc"`, defaults to `"active"`. Ignored when
#'   `to = "rxcui"`.
#' @param show_progress Logical; if `TRUE`, show a progress bar for vectorized
#'   mapping operations. Defaults to `interactive()`.
#'
#' @return A tibble with one row per RxCUI/NDC mapping.
#'
#' For `history = "active"`, returns columns:
#' \describe{
#'   \item{rxcui}{Input RxCUI.}
#'   \item{ndc11}{Mapped NDC.}
#'   \item{ndc_status}{NDC status returned by RxNorm, when available.}
#' }
#'
#' For `history = "direct"` or `"all"`, the output may also include:
#' \describe{
#'   \item{related_rxcui}{RxCUI associated with the historical NDC record. This
#'     may differ from the input RxCUI when historical or indirect associations
#'     are retrieved.}
#'   \item{ndc_start_date}{Start date of the NDC association, when available.}
#'   \item{ndc_end_date}{End date of the NDC association, when available.}
#' }
#'
#' @details
#' `status = NULL` means that no status filter is applied after NDCs are
#' retrieved. It does not, by itself, request historical NDCs. To retrieve
#' historical NDCs, set `history = "direct"` or `history = "all"`.
#'
#' @examples
#' \dontrun{
#' # Current active NDCs
#' map_rxcui_to_ndc("1049630")
#'
#' # NDCs ever directly associated with this RxCUI
#' map_rxcui_to_ndc("1049630", history = "direct")
#'
#' # All historical NDCs, retaining obsolete NDCs only
#' map_rxcui_to_ndc(
#'   "1049630",
#'   history = "all",
#'   status = "OBSOLETE"
#' )
#' }
#'
#' @export
map_rxcui_to_ndc <- function(
    x,
    status = NULL,
    history = c("active", "direct", "all"),
    show_progress = interactive()
) {
  history <- match.arg(history)

  map_to(
    x,
    to = "ndc",
    status = status,
    history = history,
    show_progress = show_progress
  )
}
