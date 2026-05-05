# R/classes.R

#' Get RxClass assertions for RxNorm drugs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `get_classes()` returns RxClass assertions for RxNorm concepts or drug names.
#' These include drug-class relationships from sources such as ATC, FDASPL,
#' DAILYMED, MEDRT, SNOMEDCT, and VA. Depending on the source and relationship,
#' returned assertions may represent pharmacologic classes, chemical structures,
#' mechanisms of action, physiologic effects, indications, contraindications,
#' VA classes, ATC classes, or SNOMED CT dispositions/structures.
#'
#' Because different sources use different classification logic, this function
#' preserves class type, relationship, and relationship source rather than
#' collapsing results into a single class label.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param rela_source Optional RxClass relationship source filter, such as
#'   `"ATC"`, `"ATCPROD"`, `"DAILYMED"`, `"FDASPL"`, `"MEDRT"`, `"SNOMEDCT"`,
#'   or `"VA"`. May be a character vector; values are queried separately.
#' @param relas Optional relationship filter. Examples include `"has_EPC"`,
#'   `"has_MoA"`, `"has_PE"`, `"may_treat"`, `"ci_with"`,
#'   `"has_chemical_structure"`, and others supported by RxClass.
#' @param class_types Optional filter on returned class types, such as `"EPC"`,
#'   `"MOA"`, `"PE"`, `"DISEASE"`, `"CHEM"`, `"VA"`, or `"ATC1-4"`.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble with one row per RxClass assertion.
#' @export
get_classes <- function(x,
                        by = c("rxcui", "name"),
                        rela_source = NULL,
                        relas = NULL,
                        class_types = NULL,
                        keep_input = TRUE) {
  by <- match.arg(by)

  lifecycle::signal_stage("experimental", "get_classes()")

  stopifnot(is.character(x))

  if (is.null(rela_source)) {
    rela_source <- NA_character_
  }

  purrr::map_dfr(x, function(.x) {
    purrr::map_dfr(rela_source, function(.rela_source) {
      query <- list()

      if (by == "rxcui") {
        path <- "/class/byRxcui"
        query$rxcui <- .x
      } else {
        path <- "/class/byDrugName"
        query$drugName <- .x
      }

      if (!is.na(.rela_source)) {
        query$relaSource <- .rela_source
      }

      if (!is.null(relas)) {
        query$relas <- .rxclass_collapse_query(relas)
      }

      res <- rxclass_get_json(path, query = query)

      out <- parse_rxclass_drug_classes(res)

      if (!is.null(class_types)) {
        out <- dplyr::filter(out, .data$class_type %in% class_types)
      }

      if (keep_input) {
        out <- dplyr::mutate(out, input = .x, .before = 1)
      }

      out
    })
  })
}


#' Get class-like RxClass assertions for RxNorm drugs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `get_drug_classes()` is an experimental convenience function that returns
#' RxClass assertions that are likely to behave like drug-class labels.
#'
#' It combines selected assertions from ATC, ATCPROD, FDA/SPL EPC, VA, and
#' SNOMED CT disposition relationships. It intentionally excludes relationship
#' types that usually describe contraindications, indications, physiologic
#' effects, chemical structures, or other non-class assertions.
#'
#' For a more compact and ingredient-oriented output, rely on defaults for
#' include_sources. For extended list of product-level sources, consider adding
#' "ATCPROD" and "VA".
#'
#' This function is experimental because "drug class" is not a single native
#' RxClass concept. Different sources use different classification logic, and
#' this helper applies an opinionated filter to return class-like assertions.
#' For source-specific results, use [get_classes()], [get_atc()], [get_epc()],
#' [get_va()], or related functions directly.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param include_sources Character vector of class-like sources to include.
#'   Defaults to `c("ATC", "ATCPROD", "EPC", "VA", "SNOMEDCT")`.
#' @param collapse Logical; if `TRUE`, returns one row per unique class
#'   assertion per input and source, dropping drug-specific columns. If `FALSE`,
#'   returns the full source-specific rows, including matched RxCUI, drug name,
#'   and term type.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of class-like RxClass assertions.
#'
#' @export
get_drug_classes <- function(x,
                             by = c("rxcui", "name"),
                             include_sources = c("ATC", "EPC", "SNOMEDCT"),
                             collapse = TRUE,
                             keep_input = TRUE) {
  by <- match.arg(by)

  lifecycle::signal_stage("experimental", "get_drug_classes()")

  include_sources <- match.arg(
    include_sources,
    choices = c("ATC", "ATCPROD", "EPC", "VA", "SNOMEDCT"),
    several.ok = TRUE
  )

  out <- list()

  if ("ATC" %in% include_sources) {
    out$atc <- get_atc(
      x = x,
      by = by,
      keep_input = keep_input
    )
  }

  if ("ATCPROD" %in% include_sources) {
    out$atcprod <- get_atcprod(
      x = x,
      by = by,
      keep_input = keep_input
    )
  }

  if ("EPC" %in% include_sources) {
    out$epc <- get_epc(
      x = x,
      by = by,
      keep_input = keep_input
    )
  }

  if ("VA" %in% include_sources) {
    out$va <- get_va(
      x = x,
      by = by,
      keep_input = keep_input
    )
  }

  if ("SNOMEDCT" %in% include_sources) {
    out$snomedct <- get_classes(
      x = x,
      by = by,
      rela_source = "SNOMEDCT",
      relas = "isa_disposition",
      keep_input = keep_input
    )
  }

  out <- dplyr::bind_rows(out) |>
    dplyr::distinct()

  if (isTRUE(collapse)) {
    if (isTRUE(keep_input)) {
      out <- out |>
        dplyr::select(
          .data$input,
          .data$class_id,
          .data$class_name,
          .data$class_type,
          .data$class_url,
          .data$rela,
          .data$rela_source
        )
    } else {
      out <- out |>
        dplyr::select(
          .data$class_id,
          .data$class_name,
          .data$class_type,
          .data$class_url,
          .data$rela,
          .data$rela_source
        )
    }

    out <- out |>
      dplyr::distinct()
  }

  out
}


#' Get ATC classes for RxNorm drugs
#'
#' `get_atc()` is a convenience wrapper for `get_classes()` that returns
#' Anatomical Therapeutic Chemical (ATC) class assertions for RxNorm drugs.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of ATC class assertions.
#' @export
get_atc <- function(x, by = c("rxcui", "name"), keep_input = TRUE) {
  by <- match.arg(by)

  get_classes(
    x = x,
    by = by,
    rela_source = "ATC",
    keep_input = keep_input
  )
}


#' Get ATC product-level classes for RxNorm drugs
#'
#' `get_atc()` is a convenience wrapper for `get_classes()` that returns
#' Anatomical Therapeutic Chemical (ATC) product-level class assertions for
#' RxNorm drugs.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of ATCPROD class assertions.
#' @export
get_atcprod <- function(x, by = c("rxcui", "name"), keep_input = TRUE) {
  by <- match.arg(by)

  get_classes(
    x = x,
    by = by,
    rela_source = "ATCPROD",
    keep_input = keep_input
  )
}


#' Get FDA/SPL established pharmacologic classes
#'
#' `get_epc()` is a convenience wrapper for `get_classes()` that returns
#' FDA labeling class assertions for RxNorm drugs.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of established pharmacologic class assertions.
#' @export
get_epc <- function(x, by = c("rxcui", "name"), keep_input = TRUE) {
  by <- match.arg(by)

  get_classes(
    x = x,
    by = by,
    rela_source = c("DAILYMED", "FDASPL"),
    relas = "has_EPC",
    class_types = "EPC",
    keep_input = keep_input
  )
}


#' Get MED-RT assertions for RxNorm drugs
#'
#' `get_medrt()` is a convenience wrapper for `get_classes()` that returns
#' MED-RT assertions for RxNorm drugs. These include, for example, asserted
#' mechanisms of action, contraindications, physiologic effects (including
#' adverse side effects), etc.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param relas Optional MED-RT relationship filter.
#' @param class_types Optional MED-RT class type filter.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of MED-RT assertions.
#' @export
get_medrt <- function(x,
                      by = c("rxcui", "name"),
                      relas = NULL,
                      class_types = NULL,
                      keep_input = TRUE) {
  by <- match.arg(by)

  get_classes(
    x = x,
    by = by,
    rela_source = "MEDRT",
    relas = relas,
    class_types = class_types,
    keep_input = keep_input
  )
}


#' Get MED-RT mechanism-of-action assertions
#'
#' `get_medrt_moa()` is a convenience wrapper for `get_medrt()` that returns
#' MED-RT mechanism-of-action assertions when available. Not all drugs have
#' MED-RT has_moa assertions; for some drugs, mechanism-like or class-like
#' information may instead be available from FDA/SPL EPC via `get_epc()` or as
#' physiologic effects via `get_medrt_pe()`.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of MED-RT mechanism-of-action assertions.
#' @export
get_medrt_moa <- function(x, by = c("rxcui", "name"), keep_input = TRUE) {
  by <- match.arg(by)

  get_medrt(
    x = x,
    by = by,
    relas = "has_MoA",
    class_types = "MOA",
    keep_input = keep_input
  )
}


#' Get MED-RT physiologic-effect assertions
#'
#' `get_medrt_pe()` is a convenience wrapper for `get_medrt()` that returns
#' only physiologic effect MED-RT assertions for RxNorm drugs.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of MED-RT physiologic-effect assertions.
#' @export
get_medrt_pe <- function(x, by = c("rxcui", "name"), keep_input = TRUE) {
  by <- match.arg(by)

  get_medrt(
    x = x,
    by = by,
    relas = "has_PE",
    class_types = "PE",
    keep_input = keep_input
  )
}


#' Get contraindication assertions from MED-RT
#'
#' `get_contraindications()` is a convenience wrapper for `get_medrt()` that
#' returns only contraindication MED-RT assertions for RxNorm drugs.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of MED-RT contraindication assertions.
#' @export
get_contraindications <- function(x,
                                  by = c("rxcui", "name"),
                                  keep_input = TRUE) {
  by <- match.arg(by)

  get_medrt(
    x = x,
    by = by,
    relas = "ci_with",
    class_types = "DISEASE",
    keep_input = keep_input
  )
}


#' Get indication/treatment assertions from MED-RT
#'
#' `get_may_treat()` is a convenience wrapper for `get_medrt()` that returns
#' indications (disease assertions) from MED-RT for RxNorm drugs, when
#' available. Note that absence of a row (or an empty table altogether)
#' should not necessarily be interpreted as absence of an indication.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of MED-RT may-treat assertions.
#' @export
get_may_treat <- function(x,
                          by = c("rxcui", "name"),
                          keep_input = TRUE) {
  by <- match.arg(by)

  get_medrt(
    x = x,
    by = by,
    relas = "may_treat",
    class_types = "DISEASE",
    keep_input = keep_input
  )
}


#' Get chemical-structure assertions
#'
#' `get_chemical_structure()` is a convenience wrapper for `get_classes()` that
#' returns chemical-structure assertions for RxNorm drugs.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param rela_source Relationship source. Defaults to FDASPL, DAILYMED, and MEDRT.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of chemical-structure assertions.
#' @export
get_chemical_structure <- function(x,
                                   by = c("rxcui", "name"),
                                   rela_source = c("FDASPL", "DAILYMED", "MEDRT"),
                                   keep_input = TRUE) {
  by <- match.arg(by)

  get_classes(
    x = x,
    by = by,
    rela_source = rela_source,
    relas = "has_chemical_structure",
    class_types = "CHEM",
    keep_input = keep_input
  )
}


#' Get VA drug classes for RxNorm drugs
#'
#' `get_va()` is a convenience wrapper for `get_classes()` that
#' returns VA drug class assertions for RxNorm drugs.
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param extended Logical; if `TRUE`, includes extended VA class assertions.
#' @param keep_input Logical; if `TRUE`, includes the original input value.
#'
#' @return A tibble of VA class assertions.
#' @export
get_va <- function(x,
                   by = c("rxcui", "name"),
                   extended = TRUE,
                   keep_input = TRUE) {
  by <- match.arg(by)

  relas <- if (isTRUE(extended)) {
    c("has_VAClass", "has_VAClass_extended")
  } else {
    "has_VAClass"
  }

  get_classes(
    x = x,
    by = by,
    rela_source = "VA",
    relas = relas,
    class_types = "VA",
    keep_input = keep_input
  )
}


#' Find RxClass drug classes by class name
#'
#' @param query Character string to search for.
#' @param class_types Optional class type filter, such as `"EPC"`, `"ATC1"`,
#'   `"ATC2"`, `"ATC3"`, `"ATC4"`, `"CHEM"`, `"DISEASE"`, or `"VA"`.
#'
#' @return A tibble of matching class concepts.
#' @export
find_classes <- function(query, class_types = NULL) {
  stopifnot(is.character(query), length(query) == 1)

  q <- list(className = query)

  if (!is.null(class_types)) {
    q$classTypes <- .rxclass_collapse_query(class_types)
  }

  res <- rxclass_get_json(
    "/class/byName",
    query = q
  )

  parse_rxclass_find_classes(res)
}


#' Get RxNorm members of an RxClass class
#'
#' `get_class_members()` queries the RxClass API to identify members of a
#' specific drug class. Typically, this would be used to identify all
#' ingredient RxCUIs in a class, which can then be combined with other search
#' functions to extract related product RxCUIs or NDCs.
#'
#' @param class_id RxClass class identifier.
#' @param rela_source RxClass relationship source, such as `"ATC"`,
#'   `"ATCPROD"`, `"DAILYMED"`, `"FDASPL"`, `"MEDRT"`, `"SNOMEDCT"`, or `"VA"`.
#' @param rela Optional relationship filter.
#' @param include_indirect Logical; if `TRUE`, include direct and indirect
#'   members. If `FALSE`, return direct members only.
#' @param ttys Optional RxNorm term type filter, such as `"IN"`, `"PIN"`,
#'   `"SCD"`, or `"SBD"`.
#'
#' @return A tibble of RxNorm drug members.
#' @export
get_class_members <- function(class_id,
                              rela_source,
                              rela = NULL,
                              include_indirect = TRUE,
                              ttys = NULL) {
  stopifnot(is.character(class_id), length(class_id) == 1)
  stopifnot(is.character(rela_source), length(rela_source) == 1)

  q <- list(
    classId = class_id,
    relaSource = rela_source,
    trans = if (isTRUE(include_indirect)) "0" else "1"
  )

  if (!is.null(rela)) {
    q$rela <- rela
  }

  if (!is.null(ttys)) {
    q$ttys <- .rxclass_collapse_query(ttys)
  }

  res <- rxclass_get_json(
    "/classMembers",
    query = q
  )

  parse_rxclass_members(
    res = res,
    class_id = class_id,
    rela_source = rela_source,
    rela = rela
  )
}

#' Summarize RxClass relationship types for drugs
#'
#' `rxclass_relationships()` is a diagnostic helper that evaluates the type(s)
#' of relationship (rela) asserted in RxNorm MED-RT data and the number within
#' each relationship asserted. In some cases, `get_medrt()` or its various
#' convenience wrappers may return empty or shorter-than-expected tibbles
#' because some relationships expected by the user are not asserted in RxNorm
#' (for example, an MOA may not be asserted by MED-RT).
#'
#' @param x Character vector of RxCUIs or drug names.
#' @param by One of `"rxcui"` or `"name"`.
#' @param rela_source Optional RxClass relationship source filter.
#'
#' @return A tibble summarizing available relationship/class-type combinations.
#' @export
rxclass_relationships <- function(x,
                                  by = c("rxcui", "name"),
                                  rela_source = NULL) {
  by <- match.arg(by)

  get_classes(
    x = x,
    by = by,
    rela_source = rela_source,
    keep_input = TRUE
  ) |>
    dplyr::count(
      .data$input,
      .data$rela_source,
      .data$rela,
      .data$class_type,
      name = "n"
    ) |>
    dplyr::arrange(
      .data$input,
      .data$rela_source,
      .data$rela,
      .data$class_type
    )
}

#' Get RxClass relationship types
#'
#' `rxclass_relas()` is a helper function that queries the RxClass API for
#' generally valid relationships within the data. Note that just because a
#' relationship is generally valid within a rela_source, not every drug or
#' RxCUI will have a corresponding relationship asserted.
#'
#' @param rela_source RxClass relationship source, such as `"ATC"`,
#'   `"DAILYMED"`, `"FDASPL"`, `"MEDRT"`, `"SNOMEDCT"`, or `"VA"`.
#'
#' @return A tibble of relationship names for the selected source.
#' @export
rxclass_relas <- function(rela_source) {
  stopifnot(is.character(rela_source), length(rela_source) == 1)

  res <- rxclass_get_json(
    "/relas",
    query = list(relaSource = rela_source)
  )

  relas <- res$relaList$relaName

  if (is.null(relas) || length(relas) == 0) {
    return(tibble::tibble(
      rela_source = character(),
      rela = character()
    ))
  }

  tibble::tibble(
    rela_source = rela_source,
    rela = as.character(relas)
  )
}


#' Get RxClass relationship sources
#'
#' @return A tibble of RxClass relationship sources.
#' @export
rxclass_rela_sources <- function() {
  res <- rxclass_get_json("/relaSources")

  sources <- res$relaSourceList$relaSourceName

  if (is.null(sources) || length(sources) == 0) {
    return(tibble::tibble(rela_source = character()))
  }

  tibble::tibble(rela_source = as.character(sources))
}



#' @keywords internal
#' @noRd
empty_rxclass_drug_classes <- function() {
  tibble::tibble(
    rxcui = character(),
    drug_name = character(),
    drug_tty = character(),
    class_id = character(),
    class_name = character(),
    class_type = character(),
    class_url = character(),
    rela = character(),
    rela_source = character()
  )
}


#' @keywords internal
#' @noRd
parse_rxclass_drug_classes <- function(res) {
  classes <- res$rxclassDrugInfoList$rxclassDrugInfo

  if (is.null(classes) || length(classes) == 0) {
    return(empty_rxclass_drug_classes())
  }

  purrr::map_dfr(classes, function(z) {
    concept <- z$minConcept
    cls <- z$rxclassMinConceptItem

    tibble::tibble(
      rxcui = null2chr(concept$rxcui),
      drug_name = null2chr(concept$name),
      drug_tty = null2chr(concept$tty),
      class_id = null2chr(cls$classId),
      class_name = null2chr(cls$className),
      class_type = null2chr(cls$classType),
      class_url = null2chr(cls$classUrl),
      rela = null2chr(z$rela),
      rela_source = null2chr(z$relaSource)
    )
  })
}


#' @keywords internal
#' @noRd
parse_rxclass_find_classes <- function(res) {
  classes <- res$rxclassMinConceptList$rxclassMinConcept

  if (is.null(classes) || length(classes) == 0) {
    return(tibble::tibble(
      class_id = character(),
      class_name = character(),
      class_type = character(),
      class_url = character()
    ))
  }

  purrr::map_dfr(classes, function(z) {
    tibble::tibble(
      class_id = null2chr(z$classId),
      class_name = null2chr(z$className),
      class_type = null2chr(z$classType),
      class_url = null2chr(z$classUrl)
    )
  })
}


#' @keywords internal
#' @noRd
empty_rxclass_members <- function() {
  tibble::tibble(
    rxcui = character(),
    name = character(),
    tty = character(),
    class_id = character(),
    rela_source = character(),
    rela = character(),
    source_id = character(),
    source_name = character(),
    source_url = character(),
    relation = character()
  )
}


#' @keywords internal
#' @noRd
parse_rxclass_members <- function(res,
                                  class_id = NA_character_,
                                  rela_source = NA_character_,
                                  rela = NA_character_) {
  members <- res$drugMemberGroup$drugMember

  if (is.null(members) || length(members) == 0) {
    return(empty_rxclass_members())
  }

  purrr::map_dfr(members, function(z) {
    concept <- z$minConcept
    attrs <- parse_rxclass_node_attrs(z$nodeAttr)

    tibble::tibble(
      rxcui = null2chr(concept$rxcui),
      name = null2chr(concept$name),
      tty = null2chr(concept$tty),
      class_id = class_id,
      rela_source = rela_source,
      rela = if (is.null(rela)) NA_character_ else rela,
      source_id = null2chr(attrs$SourceId),
      source_name = null2chr(attrs$SourceName),
      source_url = null2chr(attrs$SourceUrl),
      relation = null2chr(attrs$Relation)
    )
  })
}


#' @keywords internal
#' @noRd
parse_rxclass_node_attrs <- function(node_attr) {
  out <- list(
    SourceId = NA_character_,
    SourceName = NA_character_,
    SourceUrl = NA_character_,
    Relation = NA_character_
  )

  if (is.null(node_attr) || length(node_attr) == 0) {
    return(out)
  }

  for (a in node_attr) {
    nm <- null2chr(a$attrName)
    val <- null2chr(a$attrValue)

    if (!is.na(nm) && nm %in% names(out)) {
      out[[nm]] <- val
    }
  }

  out
}
