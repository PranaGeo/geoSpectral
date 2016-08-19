library(geoSpectral)
context("Constructor Function for Spectra class")

sp <- spc.example_spectra()

test_that("Output of Spectra() is of type 'Spectra' ", {
  expect_is(sp,"Spectra")
  expect_equal(nrow(sp), 26)
})

test_that("Conversion to/from data.frame", {
  expect_is(as(sp, "data.frame"),"data.frame")
  expect_is(as(as(sp, "data.frame"), "Spectra"), "Spectra")
})

test_that("Dimension is integer", {
  expect_is(dim(sp),"integer")
})