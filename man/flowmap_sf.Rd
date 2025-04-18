% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flowmap_sf.R
\name{flowmap_sf}
\alias{flowmap_sf}
\title{Export flow edges and nodes as simple features.}
\usage{
flowmap_sf(
  flowdat = NULL,
  od = NULL,
  nodes = NULL,
  k_nodes = NULL,
  node_buffer_factor = 1.2,
  node_fill_factor = NULL,
  node_radius_factor = 1,
  edge_offset_factor = 1,
  edge_width_factor = 1.2,
  arrow_point_angle = 45,
  crs = 4326
)
}
\arguments{
\item{flowdat}{Input dataframe. See details below.}

\item{od}{As an alternative to \code{flowdat}, dataframe with the origin-destination pairs and the flow between them.  Must contain the columns o, d, value. \code{nodes} must be provided as well. See details below.}

\item{nodes}{As an alternative to \code{flowdat}, a dataframe with the nodes of the network. Must contain the columns name, x, y. See details below.}

\item{k_nodes}{Number of clusters to group nodes into. If defined, nodes will be clustered hierarchically based on spatial proximity. By default, no clustering will be applied.}

\item{node_buffer_factor}{Controls the distance between the nodes and the edges ( in multiple of the nodes' radii).}

\item{node_fill_factor}{Controls the downscaling of the fill of the nodes ( as to not outshine the edges ).}

\item{node_radius_factor}{Controls the size of the nodes.}

\item{edge_offset_factor}{Controls the distance between the parallel arrows.}

\item{edge_width_factor}{Controls the width of the edges.}

\item{arrow_point_angle}{Controls the pointiness of the edges.}

\item{crs}{The EPSG code for the coordinate reference system of the input data. Default is 4326 (WGS84).}
}
\value{
A list with two elements: edges and nodes. Each element is a sf object.

For the edges, contains the following columns:
\itemize{
\item \strong{orig:} The unique id of the origin node.
\item \strong{dest:} The unique id of the destination node.
\item \strong{flow:} The flow from a to b.
}

And for the nodes:
\itemize{
\item \strong{name:} The unique id of the node.
\item \strong{flowsum:} The sum of all flows involving that node.
}
}
\description{
Export flow edges and nodes as simple features.
}
\examples{
testdata <-
data.frame(
 id_a = c("X1","X2","X3","X3","X1"),
 id_b = c("X8","X7","X1","X8","X7"),
 xa = c(2,14,10,10,2),
 ya = c(6,10,9,9,6),
 xb = c(10,4,2,10,4),
 yb = c(4,10,6,4,10),
 flow_ab = c(2,1,1,1,1),
 flow_ba = c(5,1,1,1,2))
sf_objects <- flowmap_sf(flowdat = testdata,crs=4326)
sf_edges <- sf_objects$edges
sf_nodes <- sf_objects$nodes
}
