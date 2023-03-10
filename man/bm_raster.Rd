% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blackmarbler.R
\name{bm_raster}
\alias{bm_raster}
\title{Make Black Marble Raster}
\usage{
bm_raster(roi_sf, product_id, date, bearer, variable = NULL)
}
\arguments{
\item{roi_sf}{Region of interest; sf polygon. Must be in the \href{https://epsg.io/4326}{WGS 84 (epsg:4326)} coordinate reference system.}

\item{product_id}{One of the following:
\itemize{
\item \code{"VNP46A1"}: Daily (raw)
\item \code{"VNP46A2"}: Daily (corrected)
\item \code{"VNP46A3"}: Monthly
\item \code{"VNP46A4"}: Annual
}}

\item{date}{Date of raster data. Entering one date will produce a raster. Entering multiple dates will produce a raster stack.
\itemize{
\item For \code{product_id}s \code{"VNP46A1"} and \code{"VNP46A2"}, a date (eg, \code{"2021-10-03"}).
\item For \code{product_id} \code{"VNP46A3"}, a date or year-month (e.g., \code{"2021-10-01"}, where the day will be ignored, or \code{"2021-10"}).
\item For \code{product_id} \code{"VNP46A4"}, year or date  (e.g., \code{"2021-10-01"}, where the month and day will be ignored, or \code{2021}).
}}

\item{bearer}{NASA bearer token. For instructions on how to create a token, see \href{https://github.com/ramarty/blackmarbler#bearer-token-}{here}.}

\item{variable}{Variable to used to create raster (default: \code{NULL}). If \code{NULL}, uses the following default variables:
\itemize{
\item For \code{product_id} \verb{:VNP46A1"}, uses \code{DNB_At_Sensor_Radiance_500m}.
\item For \code{product_id} \code{"VNP46A2"}, uses \code{Gap_Filled_DNB_BRDF-Corrected_NTL}.
\item For \code{product_id}s \code{"VNP46A3"} and \code{"VNP46A4"}, uses \code{NearNadir_Composite_Snow_Free}.
For information on other variable choices, see \href{https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document\%20Archive/Science\%20Data\%20Product\%20Documentation/VIIRS_Black_Marble_UG_v1.2_April_2021.pdf}{here}; for \code{VNP46A1}, see Table 3; for \code{VNP46A2} see Table 6; for \code{VNP46A3} and \code{VNP46A4}, see Table 9.
}}
}
\value{
Raster
}
\description{
Make a raster of nighttime lights from \href{https://blackmarble.gsfc.nasa.gov/}{NASA Black Marble data}
}
\examples{
\dontrun{
# Define bearer token
bearer <- "BEARER-TOKEN-HERE"

# sf polygon of Ghana
library(geodata)
roi_sf <- gadm(country = "GHA", level=0, path = tempdir()) \%>\% st_as_sf()

# Daily data: raster for October 3, 2021
ken_20210205_r <- bm_raster(roi_sf = roi_sf,
                            product_id = "VNP46A2",
                            date = "2021-10-03",
                            bearer = bearer)

# Monthly data: raster for March 2021
ken_202103_r <- bm_raster(roi_sf = roi_sf,
                          product_id = "VNP46A3",
                          date = "2021-03-01",
                          bearer = bearer)

# Annual data: raster for 2021
ken_2021_r <- bm_raster(roi_sf = roi_sf,
                        product_id = "VNP46A4",
                        date = 2021,
                        bearer = bearer)
}

}
