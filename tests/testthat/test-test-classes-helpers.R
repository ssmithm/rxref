test_that(".rxclass_collapse_query collapses vectors with spaces", {
  expect_equal(rxref:::.rxclass_collapse_query(c("ATC", "EPC")), "ATC EPC")
  expect_null(rxref:::.rxclass_collapse_query(NULL))
  expect_null(rxref:::.rxclass_collapse_query(character()))
})
