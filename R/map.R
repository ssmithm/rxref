#' Map between NDC and RxCUI in either direction
#'
#' @param x Character vector of NDCs (10/11-digit or hyphenated) or RxCUIs.
#' @param to One of c("rxcui","ndc") indicating desired output id.
#' @param status For `to = "ndc"`, filter by NDC status (e.g., "ACTIVE"). Use NULL for all.
#' @param show_progress Logical. Show a progress bar in interactive sessions.
#'
#' @return tibble with columns depending on direction.
#'
#' @export
map_to <- function(
    x,
    to = c("rxcui", "ndc"),
    status = NULL,
    show_progress = interactive()
) {
  to <- match.arg(to)
  stopifnot(is.character(x))

  x <- unique(x)

  show_progress <- isTRUE(show_progress) && length(x) >= 5. # don't bother with cli progress bar for short jobs

  progress_id <- NULL

  if (show_progress) {
    progress_id <- cli::cli_progress_bar(
      name = if (to == "rxcui") {
        "Mapping NDCs to RxCUIs"
      } else {
        "Mapping RxCUIs to NDCs"
      },
      total = length(x)
    )
  }

  if (to == "rxcui") {
    # input must be NDC-ish
    out <- purrr::map_dfr(x, function(ndc) {
      result <- {
        ndc11 <- ndc_to_11(ndc)

        res <- rx_get_json(
          "/rxcui",
          query = list(idtype = "NDC", id = ndc11)
        )

        rxs <- res$idGroup$rxnormId
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
      }

      if (show_progress) {
        cli::cli_progress_update(id = progress_id)
      }

      result
    }) |>
      dplyr::distinct()
  } else {
    # to == "ndc"; input must be RxCUI(s)
    out <- purrr::map_dfr(x, function(id) {
      result <- {
        if (is.na(id) || !nzchar(id)) {
          tibble::tibble(
            rxcui = id,
            ndc11 = NA_character_,
            ndc_status = NA_character_
          )
        } else {
          res  <- rx_get_json(paste0("/rxcui/", id, "/ndcs"))
          ndcs <- res$ndcGroup$ndcList$ndc
          ndcs <- if (length(ndcs)) {
            unlist(ndcs, use.names = FALSE)
          } else {
            NA_character_
          }

          out <- tibble::tibble(
            rxcui = id,
            ndc11 = ndcs
          )

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
            if (
              is.null(st) ||
              is.null(st$ndcStatus) ||
              is.null(st$ndcStatus$status)
            ) {
              return(NA_character_)
            }

            as.character(st$ndcStatus$status)
          })

          out$ndc_status <- stats

          if (!is.null(status)) {
            keep <- tolower(out$ndc_status) %in% tolower(status)
            out <- out[keep, , drop = FALSE]
          }

          out
        }
      }

      if (show_progress) {
        cli::cli_progress_update(id = progress_id)
      }

      result
    }) |>
      dplyr::distinct()
  }

  if (show_progress) {
    cli::cli_progress_done(id = progress_id)
  }

  out
}


#' Convenience wrappers
#'
#' @rdname map_to
#' @export
map_ndc_to_rxcui <- function(x, show_progress = interactive()) {
  map_to(x, to = "rxcui", show_progress = show_progress)
}


#' @rdname map_to
#' @export
map_rxcui_to_ndc <- function(x, status = NULL, show_progress = interactive()) {
  map_to(x, to = "ndc", status = status, show_progress = show_progress)
}
