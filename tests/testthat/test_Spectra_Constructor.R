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
dim(sp)
c=dim(sp)
a=spc.getwavelengths(sp)
test_that("Dimension is integer or null or equal to numbers of row and column", {
  expect_is(dim(sp),"integer")
  expect_is(sp@header, "SpcHeader" )
  expect_equal(c[1]+c[2], ncol(sp)+nrow(sp))
  expect_equal(length(a),ncol(sp))
  })
nc=dim(sp)
test_that("nuber of row and column are equal to output of ncol and nrow", {
  expect_equal(ncol(sp),nc[2] )
  expect_equal(nrow(sp),nc[1] )
  expect_is(ncol(sp),"integer")
  expect_is(nrow(sp),"integer")
})

test_that("Test for names() ", {
  
  expect_is(names(sp),"character")
  expect_equal(length(names(sp)),512)
})

hd=head(sp,7)
test_that("Tests for head()", {
  expect_equal(length(hd[,1]),7)
  expect_equal(dim(hd)[2],ncol(hd))
  expect_equal(dim(sp)[2],ncol(hd))
  expect_equal(dim(hd)[2],ncol(sp))          
  expect_is(hd,"matrix")
})

test_that("test for spc.colnames()", {
  expect_is(spc.colnames(sp),"character")
  expect_equal(length(spc.colnames(sp)),ncol(sp))
  a = "anap_300" %in% spc.colnames(sp)
  expect_equal(a,TRUE)
})

test_that("rbind test for Spectral object" ,{
  expect_equal(length(spc.rbind(sp,sp)),length(sp)*2)
})

test_that("Show Spectra",{
  expect_output(show(sp),"501 spectral channels in columns and 26 observations in rows")
  
  })


