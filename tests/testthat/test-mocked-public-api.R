test_that("resolve() resolves a drug name using mocked approximateTerm and properties", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/spellingsuggestions") {
      return(list())
    }

    if (path == "/approximateTerm") {
      expect_equal(query$term, "lisinopril")
      expect_equal(query$maxEntries, 1)

      return(list(
        approximateGroup = list(
          candidate = list(
            list(
              rxcui = "29046",
              name = "lisinopril",
              score = "100"
            )
          )
        )
      ))
    }

    if (path == "/rxcui/29046/properties") {
      return(list(
        properties = list(
          rxcui = "29046",
          name = "lisinopril",
          tty = "IN"
        )
      ))
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- resolve("lisinopril", type = "name", max_entries = 1)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$input, "lisinopril")
  expect_equal(out$type, "name")
  expect_equal(out$rxcui, "29046")
  expect_equal(out$name, "lisinopril")
  expect_equal(out$tty, "IN")
  expect_equal(out$score, 100)
  expect_equal(out$matched_term, "lisinopril")
})


test_that("resolve() resolves an NDC using mocked /rxcui and properties", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/rxcui") {
      expect_equal(query$idtype, "NDC")
      expect_equal(query$id, "00002080001")

      return(list(
        idGroup = list(
          rxnormId = list("860975")
        )
      ))
    }

    if (path == "/rxcui/860975/properties") {
      return(list(
        properties = list(
          rxcui = "860975",
          name = "metformin hydrochloride 500 MG Oral Tablet",
          tty = "SCD"
        )
      ))
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- resolve("0002-0800-01", type = "ndc")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$input, "0002-0800-01")
  expect_equal(out$type, "ndc")
  expect_equal(out$ndc11, "00002080001")
  expect_equal(out$rxcui, "860975")
  expect_equal(out$tty, "SCD")
})


test_that("resolve() resolves an RxCUI using mocked properties", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/rxcui/29046/properties") {
      return(list(
        properties = list(
          rxcui = "29046",
          name = "lisinopril",
          tty = "IN"
        )
      ))
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- resolve("29046", type = "rxcui")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$input, "29046")
  expect_equal(out$type, "rxcui")
  expect_equal(out$rxcui, "29046")
  expect_equal(out$name, "lisinopril")
  expect_equal(out$tty, "IN")
})


test_that("find_ingredients() keeps IN and PIN concepts by default", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/approximateTerm") {
      expect_equal(query$term, "lisinopril")
      expect_equal(query$maxEntries, 10)

      return(list(
        approximateGroup = list(
          candidate = list(
            list(rxcui = "29046", name = "lisinopril", score = "100"),
            list(rxcui = "12345", name = "lisinopril sodium", score = "90"),
            list(rxcui = "99999", name = "not an ingredient", score = "80")
          )
        )
      ))
    }

    if (path == "/rxcui/29046/properties") {
      return(list(
        properties = list(
          rxcui = "29046",
          name = "lisinopril",
          tty = "IN"
        )
      ))
    }

    if (path == "/rxcui/12345/properties") {
      return(list(
        properties = list(
          rxcui = "12345",
          name = "lisinopril sodium",
          tty = "PIN"
        )
      ))
    }

    if (path == "/rxcui/99999/properties") {
      return(list(
        properties = list(
          rxcui = "99999",
          name = "lisinopril 10 MG Oral Tablet",
          tty = "SCD"
        )
      ))
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- find_ingredients("lisinopril")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_equal(out$input, c("lisinopril", "lisinopril"))
  expect_equal(out$rxcui, c("29046", "12345"))
  expect_equal(out$tty, c("IN", "PIN"))
  expect_equal(out$score, c(100, 90))
})


test_that("find_ingredients() excludes PIN concepts when include_pin = FALSE", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/approximateTerm") {
      return(list(
        approximateGroup = list(
          candidate = list(
            list(rxcui = "29046", name = "lisinopril", score = "100"),
            list(rxcui = "12345", name = "lisinopril sodium", score = "90")
          )
        )
      ))
    }

    if (path == "/rxcui/29046/properties") {
      return(list(
        properties = list(
          rxcui = "29046",
          name = "lisinopril",
          tty = "IN"
        )
      ))
    }

    if (path == "/rxcui/12345/properties") {
      return(list(
        properties = list(
          rxcui = "12345",
          name = "lisinopril sodium",
          tty = "PIN"
        )
      ))
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- find_ingredients("lisinopril", include_pin = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$rxcui, "29046")
  expect_equal(out$tty, "IN")
})


test_that("find_ingredients() returns NA row when no candidates are found", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/approximateTerm") {
      return(list(
        approximateGroup = list(
          candidate = NULL
        )
      ))
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- find_ingredients("definitely not a drug")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$input, "definitely not a drug")
  expect_true(is.na(out$rxcui))
  expect_true(is.na(out$name))
  expect_true(is.na(out$tty))
  expect_true(is.na(out$score))
})


test_that("map_ndc_to_rxcui() maps NDC to RxCUI", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/rxcui") {
      expect_equal(query$idtype, "NDC")
      expect_equal(query$id, "00002080001")

      return(list(
        idGroup = list(
          rxnormId = list("860975")
        )
      ))
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- map_ndc_to_rxcui("0002-0800-01", show_progress = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$input, "0002-0800-01")
  expect_equal(out$ndc11, "00002080001")
  expect_equal(out$rxcui, "860975")
})


test_that("map_ndc_to_rxcui() returns NA RxCUI when no mapping exists", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/rxcui") {
      return(list(
        idGroup = list(
          rxnormId = NULL
        )
      ))
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- map_ndc_to_rxcui("0002-0800-01", show_progress = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$input, "0002-0800-01")
  expect_equal(out$ndc11, "00002080001")
  expect_true(is.na(out$rxcui))
})


test_that("map_rxcui_to_ndc() maps RxCUI to NDCs and retrieves NDC status", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/rxcui/860975/ndcs") {
      return(list(
        ndcGroup = list(
          ndcList = list(
            ndc = list("00002080001", "00002080002")
          )
        )
      ))
    }

    if (path == "/ndcstatus") {
      if (query$ndc == "00002-0800-01") {
        return(list(
          ndcStatus = list(
            status = "ACTIVE"
          )
        ))
      }

      if (query$ndc == "00002-0800-02") {
        return(list(
          ndcStatus = list(
            status = "OBSOLETE"
          )
        ))
      }
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- map_rxcui_to_ndc("860975", show_progress = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_equal(out$rxcui, c("860975", "860975"))
  expect_equal(out$ndc11, c("00002080001", "00002080002"))
  expect_equal(out$ndc_status, c("ACTIVE", "OBSOLETE"))
})


test_that("map_rxcui_to_ndc() filters by requested NDC status", {
  fake_rx_get_json <- function(path, query = list()) {
    if (path == "/rxcui/860975/ndcs") {
      return(list(
        ndcGroup = list(
          ndcList = list(
            ndc = list("00002080001", "00002080002")
          )
        )
      ))
    }

    if (path == "/ndcstatus") {
      if (query$ndc == "00002-0800-01") {
        return(list(
          ndcStatus = list(
            status = "ACTIVE"
          )
        ))
      }

      if (query$ndc == "00002-0800-02") {
        return(list(
          ndcStatus = list(
            status = "OBSOLETE"
          )
        ))
      }
    }

    stop("Unexpected path: ", path)
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- map_rxcui_to_ndc("860975", status = "ACTIVE", show_progress = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$rxcui, "860975")
  expect_equal(out$ndc11, "00002080001")
  expect_equal(out$ndc_status, "ACTIVE")
})


test_that("get_classes() queries RxClass by RxCUI and parses class assertions", {
  fake_rxclass_get_json <- function(path, query = list()) {
    expect_equal(path, "/class/byRxcui")
    expect_equal(query$rxcui, "29046")
    expect_equal(query$relaSource, "ATC")

    return(list(
      rxclassDrugInfoList = list(
        rxclassDrugInfo = list(
          list(
            minConcept = list(
              rxcui = "29046",
              name = "lisinopril",
              tty = "IN"
            ),
            rxclassMinConceptItem = list(
              classId = "C09AA",
              className = "ACE inhibitors, plain",
              classType = "ATC4",
              classUrl = "https://example.org/C09AA"
            ),
            rela = "has_ATC",
            relaSource = "ATC"
          )
        )
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  withr::local_options(lifecycle_verbosity = "quiet")

  out <- get_classes(
    "29046",
    by = "rxcui",
    rela_source = "ATC",
    keep_input = TRUE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$input, "29046")
  expect_equal(out$rxcui, "29046")
  expect_equal(out$drug_name, "lisinopril")
  expect_equal(out$drug_tty, "IN")
  expect_equal(out$class_id, "C09AA")
  expect_equal(out$class_name, "ACE inhibitors, plain")
  expect_equal(out$class_type, "ATC4")
  expect_equal(out$rela, "has_ATC")
  expect_equal(out$rela_source, "ATC")
})


test_that("get_classes() queries RxClass by drug name", {
  fake_rxclass_get_json <- function(path, query = list()) {
    expect_equal(path, "/class/byDrugName")
    expect_equal(query$drugName, "lisinopril")
    expect_equal(query$relaSource, "ATC")

    return(list(
      rxclassDrugInfoList = list(
        rxclassDrugInfo = list(
          list(
            minConcept = list(
              rxcui = "29046",
              name = "lisinopril",
              tty = "IN"
            ),
            rxclassMinConceptItem = list(
              classId = "C09AA",
              className = "ACE inhibitors, plain",
              classType = "ATC4",
              classUrl = "https://example.org/C09AA"
            ),
            rela = "has_ATC",
            relaSource = "ATC"
          )
        )
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  withr::local_options(lifecycle_verbosity = "quiet")

  out <- get_classes(
    "lisinopril",
    by = "name",
    rela_source = "ATC",
    keep_input = TRUE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$input, "lisinopril")
  expect_equal(out$rxcui, "29046")
  expect_equal(out$class_id, "C09AA")
})


test_that("get_classes() can filter class types", {
  fake_rxclass_get_json <- function(path, query = list()) {
    return(list(
      rxclassDrugInfoList = list(
        rxclassDrugInfo = list(
          list(
            minConcept = list(
              rxcui = "29046",
              name = "lisinopril",
              tty = "IN"
            ),
            rxclassMinConceptItem = list(
              classId = "C09AA",
              className = "ACE inhibitors, plain",
              classType = "ATC4",
              classUrl = "https://example.org/C09AA"
            ),
            rela = "has_ATC",
            relaSource = "ATC"
          ),
          list(
            minConcept = list(
              rxcui = "29046",
              name = "lisinopril",
              tty = "IN"
            ),
            rxclassMinConceptItem = list(
              classId = "C000000",
              className = "Some non-ATC class",
              classType = "EPC",
              classUrl = "https://example.org/EPC"
            ),
            rela = "has_EPC",
            relaSource = "FDASPL"
          )
        )
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  withr::local_options(lifecycle_verbosity = "quiet")

  out <- get_classes(
    "29046",
    by = "rxcui",
    class_types = "ATC4",
    keep_input = FALSE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$class_id, "C09AA")
  expect_equal(out$class_type, "ATC4")
  expect_false("input" %in% names(out))
})


test_that("find_classes() searches class concepts by name", {
  fake_rxclass_get_json <- function(path, query = list()) {
    expect_equal(path, "/class/byName")
    expect_equal(query$className, "ACE inhibitors")

    return(list(
      rxclassMinConceptList = list(
        rxclassMinConcept = list(
          list(
            classId = "C09A",
            className = "ACE inhibitors",
            classType = "ATC3",
            classUrl = "https://example.org/C09A"
          ),
          list(
            classId = "C09AA",
            className = "ACE inhibitors, plain",
            classType = "ATC4",
            classUrl = "https://example.org/C09AA"
          )
        )
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  out <- find_classes("ACE inhibitors")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_equal(out$class_id, c("C09A", "C09AA"))
  expect_equal(out$class_name, c("ACE inhibitors", "ACE inhibitors, plain"))
  expect_equal(out$class_type, c("ATC3", "ATC4"))
})


test_that("find_classes() passes class_types filter to RxClass", {
  fake_rxclass_get_json <- function(path, query = list()) {
    expect_equal(path, "/class/byName")
    expect_equal(query$className, "ACE inhibitors")
    expect_equal(query$classTypes, "ATC4 EPC")

    return(list(
      rxclassMinConceptList = list(
        rxclassMinConcept = list(
          list(
            classId = "C09AA",
            className = "ACE inhibitors, plain",
            classType = "ATC4",
            classUrl = "https://example.org/C09AA"
          )
        )
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  out <- find_classes("ACE inhibitors", class_types = c("ATC4", "EPC"))

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$class_id, "C09AA")
  expect_equal(out$class_type, "ATC4")
})


test_that("find_classes() returns empty tibble when no class concepts are found", {
  fake_rxclass_get_json <- function(path, query = list()) {
    return(list(
      rxclassMinConceptList = list(
        rxclassMinConcept = NULL
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  out <- find_classes("not a real class")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_named(out, c("class_id", "class_name", "class_type", "class_url"))
})


test_that("get_class_members() queries RxClass members and parses node attributes", {
  fake_rxclass_get_json <- function(path, query = list()) {
    expect_equal(path, "/classMembers")
    expect_equal(query$classId, "C09AA")
    expect_equal(query$relaSource, "ATC")
    expect_equal(query$trans, "0")
    expect_equal(query$ttys, "IN PIN")

    return(list(
      drugMemberGroup = list(
        drugMember = list(
          list(
            minConcept = list(
              rxcui = "29046",
              name = "lisinopril",
              tty = "IN"
            ),
            nodeAttr = list(
              list(attrName = "SourceId", attrValue = "C09AA03"),
              list(attrName = "SourceName", attrValue = "lisinopril"),
              list(attrName = "SourceUrl", attrValue = "https://example.org/C09AA03"),
              list(attrName = "Relation", attrValue = "direct")
            )
          )
        )
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  out <- get_class_members(
    class_id = "C09AA",
    rela_source = "ATC",
    include_indirect = TRUE,
    ttys = c("IN", "PIN")
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$rxcui, "29046")
  expect_equal(out$name, "lisinopril")
  expect_equal(out$tty, "IN")
  expect_equal(out$class_id, "C09AA")
  expect_equal(out$rela_source, "ATC")
  expect_true(is.na(out$rela))
  expect_equal(out$source_id, "C09AA03")
  expect_equal(out$source_name, "lisinopril")
  expect_equal(out$relation, "direct")
})


test_that("get_class_members() passes rela and direct-only options", {
  fake_rxclass_get_json <- function(path, query = list()) {
    expect_equal(path, "/classMembers")
    expect_equal(query$classId, "C09AA")
    expect_equal(query$relaSource, "ATC")
    expect_equal(query$rela, "has_ATC")
    expect_equal(query$trans, "1")

    return(list(
      drugMemberGroup = list(
        drugMember = list(
          list(
            minConcept = list(
              rxcui = "29046",
              name = "lisinopril",
              tty = "IN"
            ),
            nodeAttr = list()
          )
        )
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  out <- get_class_members(
    class_id = "C09AA",
    rela_source = "ATC",
    rela = "has_ATC",
    include_indirect = FALSE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$rxcui, "29046")
  expect_equal(out$rela, "has_ATC")
  expect_equal(out$class_id, "C09AA")
})


test_that("get_class_members() returns empty tibble when no members are found", {
  fake_rxclass_get_json <- function(path, query = list()) {
    return(list(
      drugMemberGroup = list(
        drugMember = NULL
      )
    ))
  }

  testthat::local_mocked_bindings(
    rxclass_get_json = fake_rxclass_get_json,
    .package = "rxref"
  )

  out <- get_class_members(
    class_id = "C09AA",
    rela_source = "ATC"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_named(out, c(
    "rxcui",
    "name",
    "tty",
    "class_id",
    "rela_source",
    "rela",
    "source_id",
    "source_name",
    "source_url",
    "relation"
  ))
})


test_that("ingredients_for_rxcui() wraps ingredient helper and renames related_rxcui to rxcui", {
  fake_get_ingredients <- function(x, include_pin = TRUE, include_min = FALSE) {
    expect_equal(x, "860975")
    expect_true(include_pin)
    expect_false(include_min)

    tibble::tibble(
      related_rxcui = x,
      ingredient_rxcui = c("6809", "12345"),
      ingredient_name = c("metformin", "metformin hydrochloride"),
      ingredient_tty = c("IN", "PIN")
    )
  }

  testthat::local_mocked_bindings(
    .rxref_get_ingredients_for_rxcui = fake_get_ingredients,
    .package = "rxref"
  )

  out <- ingredients_for_rxcui("860975", show_progress = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_equal(out$rxcui, c("860975", "860975"))
  expect_equal(out$ingredient_rxcui, c("6809", "12345"))
  expect_equal(out$ingredient_name, c("metformin", "metformin hydrochloride"))
  expect_equal(out$ingredient_tty, c("IN", "PIN"))
  expect_false("related_rxcui" %in% names(out))
})


test_that("ingredients_for_rxcui() de-duplicates input RxCUIs", {
  calls <- 0L

  fake_get_ingredients <- function(x, include_pin = TRUE, include_min = FALSE) {
    calls <<- calls + 1L

    tibble::tibble(
      related_rxcui = x,
      ingredient_rxcui = "6809",
      ingredient_name = "metformin",
      ingredient_tty = "IN"
    )
  }

  testthat::local_mocked_bindings(
    .rxref_get_ingredients_for_rxcui = fake_get_ingredients,
    .package = "rxref"
  )

  out <- ingredients_for_rxcui(c("860975", "860975"), show_progress = FALSE)

  expect_equal(calls, 1L)
  expect_equal(nrow(out), 1)
  expect_equal(out$rxcui, "860975")
  expect_equal(out$ingredient_rxcui, "6809")
})
