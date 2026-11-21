# tests/testthat/test-class-members-relas.R

# Small simulated RxClass response containing one drug member
mock_rxclass_member <- function(rxcui = "29046") {
  list(
    drugMemberGroup = list(
      drugMember = list(
        list(
          minConcept = list(
            rxcui = rxcui,
            name = "example drug",
            tty = "IN"
          ),
          nodeAttr = list()
        )
      )
    )
  )
}


test_that("get_class_members supports multiple relationships", {
  calls <- character()

  testthat::local_mocked_bindings(
    rxclass_get_json = function(path, query = list()) {
      expect_equal(path, "/classMembers")
      expect_equal(query$classId, "D006973")
      expect_equal(query$relaSource, "MEDRT")
      expect_equal(query$trans, "0")

      calls <<- c(calls, query$rela)

      mock_rxclass_member()
    },
    .package = "rxref"
  )

  out <- get_class_members(
    class_id = "D006973",
    rela_source = "MEDRT",
    rela = c("may_treat", "CI_with", "may_treat")
  )

  # Only two distinct API requests should be made
  expect_equal(calls, c("may_treat", "CI_with"))

  # Preserve distinct relationships for the same RxCUI
  expect_equal(nrow(out), 2L)
  expect_equal(out$rxcui, c("29046", "29046"))
  expect_equal(out$rela, c("may_treat", "CI_with"))
  expect_equal(out$rela_source, rep("MEDRT", 2))
  expect_equal(out$class_id, rep("D006973", 2))
})


test_that("get_class_members preserves single-rela behavior", {
  testthat::local_mocked_bindings(
    rxclass_get_json = function(path, query = list()) {
      expect_equal(path, "/classMembers")
      expect_equal(query$classId, "CV400")
      expect_equal(query$relaSource, "VA")
      expect_equal(query$rela, "has_VAClass")
      expect_equal(query$trans, "1")
      expect_equal(query$ttys, "IN PIN")

      mock_rxclass_member()
    },
    .package = "rxref"
  )

  out <- get_class_members(
    class_id = "CV400",
    rela_source = "VA",
    rela = "has_VAClass",
    include_indirect = FALSE,
    ttys = c("IN", "PIN")
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$rela, "has_VAClass")
  expect_equal(out$rela_source, "VA")
})


test_that("get_class_members preserves ATC behavior without rela", {
  testthat::local_mocked_bindings(
    rxclass_get_json = function(path, query = list()) {
      expect_equal(path, "/classMembers")
      expect_false("rela" %in% names(query))

      if (query$classId == "C02") {
        return(mock_rxclass_member())
      }

      list(drugMemberGroup = list(drugMember = NULL))
    },
    .package = "rxref"
  )

  # Successful ATC query without rela
  expect_silent(
    out <- get_class_members("C02", "ATCPROD")
  )

  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$rela))

  # Empty ATC query should not trigger a warning
  expect_silent(
    empty <- get_class_members("C99", "ATC")
  )

  expect_equal(nrow(empty), 0L)
})


test_that("get_class_members warns on empty non-ATC queries without rela", {
  testthat::local_mocked_bindings(
    rxclass_get_json = function(path, query = list()) {
      expect_equal(path, "/classMembers")
      list(drugMemberGroup = list(drugMember = NULL))
    },
    .package = "rxref"
  )

  expect_warning(
    out <- get_class_members("D006973", "MEDRT"),
    "No class members returned"
  )

  expect_equal(nrow(out), 0L)

  # Explicit rela: empty results should not trigger this warning
  expect_silent(
    out_explicit <- get_class_members(
      "D006973", "MEDRT", rela = "may_treat"
    )
  )

  expect_equal(nrow(out_explicit), 0L)
})


test_that("get_class_members does not warn on successful unfiltered queries", {
  testthat::local_mocked_bindings(
    rxclass_get_json = function(path, query = list()) {
      expect_false("rela" %in% names(query))
      mock_rxclass_member()
    },
    .package = "rxref"
  )

  expect_silent(
    out <- get_class_members("D006973", "MEDRT")
  )

  expect_equal(nrow(out), 1L)
})


test_that("get_class_members validates rela arguments", {
  expect_error(
    get_class_members("D006973", "MEDRT", rela = character()),
    "rela"
  )

  expect_error(
    get_class_members("D006973", "MEDRT", rela = NA_character_),
    "rela"
  )

  expect_error(
    get_class_members("D006973", "MEDRT", rela = ""),
    "rela"
  )

  expect_error(
    get_class_members("D006973", "MEDRT", rela = 123),
    "rela"
  )
})


test_that("rxclass_relas parses the relaList response", {
  testthat::local_mocked_bindings(
    rxclass_get_json = function(path, query = list()) {
      expect_equal(path, "/relas")
      expect_equal(query$relaSource, "MEDRT")

      list(
        relaList = list(
          rela = list("may_treat", "CI_with")
        )
      )
    },
    .package = "rxref"
  )

  out <- rxclass_relas("MEDRT")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)
  expect_equal(out$rela_source, c("MEDRT", "MEDRT"))
  expect_equal(out$rela, c("may_treat", "CI_with"))
})


test_that("rxclass_relas handles empty responses", {
  testthat::local_mocked_bindings(
    rxclass_get_json = function(path, query = list()) {
      list(relaList = list(rela = NULL))
    },
    .package = "rxref"
  )

  out <- rxclass_relas("VA")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("rela_source", "rela"))
})
