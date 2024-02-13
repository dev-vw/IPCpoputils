test_that("Pivoted colnames are sorted and correct", {
  out_colnames <- c("ADM_LEVEL", "ADM0", "ADM1", "ADM2", "YR", "SEX", "AGE_CAT", "POP_TOTAL")
  expect_contains(colnames(clean_idb(country = "kenya",
                                     datadir = "data-raw/",
                                     skip = 1)),
                  out_colnames)
})

