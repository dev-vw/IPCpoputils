test_that("geodata validation successful", {
  # testing corrupt sf import
  png_corrupt_sf <- st_read("data-raw/shapefiles/png-broken_sf/broken_llg.shp",
                            quiet = TRUE)
  expect_error(validate_geodata("GEO_MATCH", png_corrupt_sf))

  # testing existence of match column
  expect_true(validate_geodata("GEO_MATCH", kenya_adm2))
  expect_error(validate_geodata("foo", kenya_adm2))

  # testing uniqueness of geo matches
  expect_true(validate_geodata("GEO_MATCH", kenya_adm2))

  # not unique geo matches
  kenya_adm2_redundant <- rbind(kenya_adm2, kenya_adm2[1, ])
  expect_error(validate_geodata("GEO_MATCH", kenya_adm2_redundant))
})

