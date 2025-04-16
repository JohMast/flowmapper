#' Export flow edges and nodes as simple features.
#'
#' @param flowdat Input dataframe. See details below.
#' @param od As an alternative to \code{flowdat}, dataframe with the origin-destination pairs and the flow between them.  Must contain the columns o, d, value. \code{nodes} must be provided as well. See details below.
#' @param nodes As an alternative to \code{flowdat}, a dataframe with the nodes of the network. Must contain the columns name, x, y. See details below.
#' @param k_nodes Number of clusters to group nodes into. If defined, nodes will be clustered hierarchically based on spatial proximity. By default, no clustering will be applied.
#' @param node_buffer_factor Controls the distance between the nodes and the edges ( in multiple of the nodes' radii).
#' @param node_radius_factor Controls the size of the nodes.
#' @param node_fill_factor Controls the downscaling of the fill of the nodes ( as to not outshine the edges ).
#' @param edge_offset_factor Controls the distance between the parallel arrows.
#' @param edge_width_factor Controls the width of the edges.
#' @param arrow_point_angle Controls the pointiness of the edges.
#' @param crs The EPSG code for the coordinate reference system of the input data. Default is 4326 (WGS84).
#' @return A list with two elements: edges and nodes. Each element is a sf object.
#'
#' For the edges, contains the following columns:
#' \itemize{
#'  \item \strong{orig:} The unique id of the origin node.
#'  \item \strong{dest:} The unique id of the destination node.
#'  \item \strong{flow:} The flow from a to b.
#' }
#'
#' And for the nodes:
#' \itemize{
#'  \item \strong{name:} The unique id of the node.
#'  \item \strong{flowsum:} The sum of all flows involving that node.
#' }
#' @export
#' @importFrom sfheaders sf_polygon
#' @importFrom sf st_crs
#' @importFrom dplyr select
#'
#' @examples
#' testdata <-
#' data.frame(
#'  id_a = c("X1","X2","X3","X3","X1"),
#'  id_b = c("X8","X7","X1","X8","X7"),
#'  xa = c(2,14,10,10,2),
#'  ya = c(6,10,9,9,6),
#'  xb = c(10,4,2,10,4),
#'  yb = c(4,10,6,4,10),
#'  flow_ab = c(2,1,1,1,1),
#'  flow_ba = c(5,1,1,1,2))
#' sf_objects <- flowmap_sf(flowdat = testdata,crs=4326)
#' sf_edges <- sf_objects$edges
#' sf_nodes <- sf_objects$nodes

flowmap_sf <- function(flowdat=NULL,od=NULL,nodes=NULL,k_nodes=NULL,node_buffer_factor = 1.2, node_fill_factor = NULL, node_radius_factor = 1, edge_offset_factor = 1, edge_width_factor = 1.2, arrow_point_angle = 45,crs=4326){

  prepared_flowmap <- prep_flowmap(
    flowdat = flowdat
    ,od = od
    ,nodes = nodes
    ,k_nodes = k_nodes
    ,node_buffer_factor = node_buffer_factor
    ,node_radius_factor = node_radius_factor
    ,edge_offset_factor = edge_offset_factor
    ,node_fill_factor = node_fill_factor
    ,edge_width_factor = edge_width_factor
    ,arrow_point_angle = arrow_point_angle
  )

  x <- prepared_flowmap[["edges"]]
  y <- prepared_flowmap[["nodes"]]
  sf_edges <- sfheaders::sf_polygon(
    obj = x
    ,x = "x"
    ,y = "y",
    keep=TRUE,
    polygon_id = "group"
  )
  sf::st_crs( sf_edges ) <- crs
  sf_edges <- sf_edges |> dplyr::select(orig=aux1,dest=aux2,flow)

  sf_nodes <- sfheaders::sf_polygon(
    obj = y
    ,x = "x"
    ,y = "y",
    keep=TRUE,
    polygon_id = "group"
  )
  sf::st_crs( sf_nodes ) <- crs
  sf_nodes <- sf_nodes |> dplyr::select(name=group,flowsum)
  return(list(edges=sf_edges,nodes=sf_nodes))
}
