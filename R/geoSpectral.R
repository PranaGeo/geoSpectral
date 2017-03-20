#' @title Classes and methods for working with spectral (electromagnetic) data in R
#'
#' @description 
#' \pkg{geoSpectral} is an R package providing a new data type for R that stores
#'  spectral, temporal, and spatial attributes of measurement data as well as methods for 
#' accessing and manipulating the spectral (and non-spectral) data
#' with code.
#'
#' @details 
#' This package provides the following S4 classes: 
#' \itemize{
#'   \item \code{Spectra} (stores spatial/temporal/spectral aspects of data)
#'   \item \code{SpcHeader} (stores metadata in an R list object)
#'   \item \code{SpcList} (makes a collection of Spectra objects in an R list)
#' }
#' as well as basic data access and manipulation methods for importing, acessing and
#' subsetting, converting into R objects, analyzing, plotting, and exporting to other scientific
#' data formats. Have a look at the constructor function by typing \code{?Spectra} to get started.
#'
#' @author Servet Ahmet Cizmeli <ahmet@pranageo.com>
#' 
#' @references
#' There is a tutorial for \pkg{geoSpectral} at the package's GitHub page: \url{https://github.com/PranaGeo/geoSpectral/wiki/RPackage_geoSpectral_Tutorial}.
#'
#' @concept Spectral Wavelength
#' @seealso See also the packages \pkg{spacetime}, \pkg{rgdal},
#'\pkg{sp}, \pkg{xts}

#' @docType package
#' @name geoSpectral
#' @import maps methods rgdal spacetime xts
#' @importFrom dplyr select everything
#' @importFrom rbokeh figure ly_map ly_points
#' @importFrom plotly plot_ly add_trace layout toRGB subplot
NULL
