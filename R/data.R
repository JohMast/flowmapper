#' CH_migration_data
#'
#' Internal migrations between Cantons of Switzerland, 2011-2016.
#'
#' @format ## `CH_migration_data`
#' A data frame with 325 rows and 8 columns:
#' \describe{
#'   \item{id_a, id_b}{Names of Cantons A and B}
#'   \item{flow_ab}{Number of migrations from A to B}
#'   \item{flow_ba}{Number of migrations from B to A}
#'   \item{xa,ya}{Longitude and latitude of the centroid of Canton A. Web-Mercator projection (EPSG: 3857)}
#'   \item{xb,yb}{Longitude and latitude of the centroid of Canton B. Web-Mercator projection (EPSG: 3857)}
#' }
#' @source Federal Statistical Office of Switzerland, under OPEN-BY-ASK terms of use: <https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/migration-integration/binnenwanderung.assetdetail.3222163.html>
"CH_migration_data"


#' cantons
#'
#' Geometries of Cantons of Switzerland. CRS is unassigned, but should be EPSG:3857.
#'
#' @format ## `cantons`
#' A sf object with 26 rows and 2 columns:
#' \describe{
#'   \item{NAME_1}{Name of Canton}
#'   \item{geometry}{polygon coordinates}
#' }
#' @source GADM database <https://gadm.org/>
"cantons"
