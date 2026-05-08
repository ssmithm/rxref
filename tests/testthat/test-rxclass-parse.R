test_that("parse_rxclass_drug_classes handles empty response", {
  res <- list(rxclassDrugInfoList = list(rxclassDrugInfo = NULL))
  out <- rxref:::parse_rxclass_drug_classes(res)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_named(out, c(
    "rxcui", "drug_name", "drug_tty",
    "class_id", "class_name", "class_type",
    "class_url", "rela", "rela_source"
  ))
})

test_that("parse_rxclass_drug_classes parses one class assertion", {
  res <- list(
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
            classUrl = "https://example.com"
          ),
          rela = "has_ATC",
          relaSource = "ATC"
        )
      )
    )
  )

  out <- rxref:::parse_rxclass_drug_classes(res)

  expect_equal(nrow(out), 1)
  expect_equal(out$rxcui, "29046")
  expect_equal(out$class_id, "C09AA")
  expect_equal(out$class_type, "ATC4")
  expect_equal(out$rela_source, "ATC")
})
