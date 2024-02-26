utils::globalVariables(c("a_b","a_m","adj_radius","adj_radius_a","adj_radius_b",
                         "aes","aux1","aux2","e","flow","flow_ab","flow_ba",
                         "flowsum","group","id","id_a","id_b","label","m_c",
                         "name","off_x","off_y","point","radius","value",
                         "width","x","xa","xb","xe","xf","xg","xm","xz","y",
                         "ya","yb","ye","yf","yg","ym","yz"))

#' Add a flow map to a ggplot
#'
#' @param p The plot to which the flowmap should be added.
#' @param flowdat Input dataframe. See details below.
#' @param node_buffer_factor Controls the distance between the nodes and the edges ( in multiple of the nodes' radii)
#' @param node_radius_factor Controls the size of the nodes
#' @param node_fill_factor Controls the downscaling of the fill of the nodes ( as to not outshine the edges )
#' @param edge_offset_factor Controls the distance between the parallel arrows
#' @param edge_width_factor Controls the width of the edges
#' @param outline_linewidth The linewidth of the outline of the arrows
#' @param alpha Opacity of the edges.
#' @param outline_col Color of the outline of the edges.
#' @param arrow_point_angle Controls the pointiness of the edges.
#'
#' @details
#' The function requires as inputs a dataframe \code{flowdat} which contains for every combination of two nodes a and b the coordinates of these nodes as well as the intensity of flow between those nodes in both directions (a to b, b to a). The dataframe should have the following columns:
#' \itemize{
#'  \item \strong{id_a:} The unique id of node a
#'  \item \strong{id_b:} The unique id of node b
#'  \item \strong{xa:} The x coordinate of node a
#'  \item \strong{ya:} The y coordinate of node a
#'  \item \strong{xb:} The x coordinate of node b
#'  \item \strong{yb:} The y coordinate of node b
#'  \item \strong{flow_ab:} The intensity of flow from node a to node b
#'  \item \strong{flow_ba:} The intensity of flow from node b to node a
#' }
#'
#' The function will impose coord_equal() on the ggplot.
#'
#' Inspired by\href{https://flowmap.gl/}{flowmap.gl}.
#'
#' @importFrom dplyr mutate select left_join summarize group_by ungroup bind_rows n arrange group_split
#' @importFrom tidyr pivot_longer separate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_polygon
#' @return The ggplot with an additional polygon layer for the flow arrows and an additional polygon layer for the nodes
#' @author Johannes Mast
#' @export
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
#'  flow_ba = c(5,1,1,1,2)
#')
#' library(ggplot2)
#' plot <- ggplot()
#' plot |> add_flowmap(testdata)
add_flowmap <- function(p,flowdat,outline_linewidth=0.01,alpha=0.8,outline_col="black",node_buffer_factor = 1.2, node_radius_factor = 1, edge_offset_factor = 1, node_fill_factor = 0.25, edge_width_factor = 1.2, arrow_point_angle = 45){

  # Some checks
  if(arrow_point_angle>75){arrow_point_angle <- 75; cat("Warning. arrow_point_angle cannot exceed 75 degrees")}
  if(arrow_point_angle<20){arrow_point_angle <- 20; cat("Warning. arrow_point_angle cannot be lower than 20 degrees")}


  nodes <-
    bind_rows(
      flowdat |>
        mutate(flow=flow_ab+flow_ba) |>
        select(id=id_a,x=xa,y=ya,flow),
      flowdat |>
        mutate(flow=flow_ab+flow_ba) |>
        select(id=id_b,x=xb,y=yb,flow)
    ) |>
    group_by(id,x,y) |>
    summarize(
      flowsum=sum(flow),
      radius=sqrt(flowsum/pi),
      n_edges=n(),
    ) |>
    ungroup()




  xrange=max(nodes$x)-min(nodes$x)
  yrange=max(nodes$y)-min(nodes$y)
  # width of the widest arrow
  maxwidth <- 0.05*max(xrange,yrange)*edge_width_factor
  # circle size
  max_circle_size <- 0.02*max(xrange,yrange)*node_radius_factor
  # value by which the two arrows are separated
  off <- 0.005*max(xrange,yrange)*edge_offset_factor

  # create group id
  flowdat <-
    flowdat |>
    mutate(group=paste0(id_a, " - ", id_b))

  # calculate the adjusted radius for the nodes
  nodes <-
    nodes |>
    mutate(
      adj_radius=max_circle_size*radius/max(radius))

  # compute global scaling
  max_flow <- max(max(flowdat$flow_ab),max(flowdat$flow_ba))

  ###########
  flowdat_ab <-
    flowdat |>
    # add the information about the radius of the nodes
    left_join(
      nodes |> select(id,adj_radius_b=adj_radius),
      by=c("id_a"="id")
    )|>
    left_join(
      nodes |> select(id,adj_radius_a=adj_radius),
      by=c("id_b"="id")
    ) |>
    mutate(width=maxwidth*flow_ab/max_flow,
           e = atan2(xb-xa,yb-ya),
           #apply shortening
           xa = xa+sin(e)*adj_radius_b*node_buffer_factor,
           ya = ya+cos(e)*adj_radius_b*node_buffer_factor,
           xb = xb-sin(e)*adj_radius_a*node_buffer_factor,
           yb = yb-cos(e)*adj_radius_a*node_buffer_factor,

           a_b = sqrt((xb-xa)^2+(yb-ya)^2),
           m_c = width*tan((90-arrow_point_angle)*(pi/180)),
           a_m = a_b - m_c,
           xm = xa+(xb-xa)*(a_m/a_b), # coordinates of B
           ym = ya+(yb-ya)*(a_m/a_b), # coordinates of B

           xe=cos(e)*width+xm,
           ye=sin(e)*-width+ym,
           xf=cos(e)*width/2+xm,
           yf=sin(e)*-width/2+ym,
           xg=cos(e)*width/2+xa,
           yg=sin(e)*-width/2+ya,
           # calculate offsets
           off_x=cos(e)*off/2,
           off_y=sin(e)*-off/2,
           # apply offsets
           xa=xa+off_x,xb=xb+off_x,xe=xe+off_x,xm=xm+off_x,xf=xf+off_x,xg=xg+off_x,
           ya=ya+off_y,yb=yb+off_y,ye=ye+off_y,ym=ym+off_y,yf=yf+off_y,yg=yg+off_y,
    )

  flowdat_ba <-
    flowdat  |>
    # add the information about the radius of the nodes
    left_join(
      nodes |> select(id,adj_radius_b=adj_radius),
      by=c("id_a"="id")
    )|>
    left_join(
      nodes |> select(id,adj_radius_a=adj_radius),
      by=c("id_b"="id")
    ) |>
    mutate(width=maxwidth*flow_ba/max_flow,
           # swap o and d coords, keep all else the same
           xz=xa,xa=xb,xb=xz,xz=NULL,
           yz=ya,ya=yb,yb=yz,yz=NULL,
           # end swap

           e = atan2(xb-xa,yb-ya),

           #apply shortening
           xa = xa+sin(e)*adj_radius_a*node_buffer_factor,
           ya = ya+cos(e)*adj_radius_a*node_buffer_factor,
           xb = xb-sin(e)*adj_radius_b*node_buffer_factor,
           yb = yb-cos(e)*adj_radius_b*node_buffer_factor,


           a_b = sqrt((xb-xa)^2+(yb-ya)^2),
           m_c = width*tan((90-arrow_point_angle)*(pi/180)),
           a_m = a_b - m_c,
           xm = xa+(xb-xa)*(a_m/a_b), # coordinates of m
           ym = ya+(yb-ya)*(a_m/a_b), # coordinates of m

           xe=cos(e)*width+xm,
           ye=sin(e)*-width+ym,
           xf=cos(e)*width/2+xm,
           yf=sin(e)*-width/2+ym,
           xg=cos(e)*width/2+xa,
           yg=sin(e)*-width/2+ya,

           # calculate offsets
           off_x=cos(e)*off/2,
           off_y=sin(e)*-off/2,
           # apply offsets
           xa=xa+off_x,xb=xb+off_x,xe=xe+off_x,xm=xm+off_x,xf=xf+off_x,xg=xg+off_x,
           ya=ya+off_y,yb=yb+off_y,ye=ye+off_y,ym=ym+off_y,yf=yf+off_y,yg=yg+off_y,
    )

  #calculate sum of flows for each route (for ordering the arrows drawing order)
  flowsums <-
    flowdat_ab |>
    select(group,flow_ab) |>
    left_join(flowdat_ba|>
                select(group,flow_ba),by="group") |>
    mutate(flowsum=flow_ab+flow_ba) |>
    select(group,flowsum)

  flows <-
    bind_rows(
      flowdat |>
        mutate(group = paste0(group,"_ab")) |>
        select(group,flow=flow_ab),
      flowdat |>
        mutate(group = paste0(group,"_ba")) |>
        select(group,flow=flow_ba)
    )

  flows <-
    bind_rows(
      flowdat |>
        mutate(group = group) |>
        select(group,flow=flow_ab),
      flowdat |>
        separate(group,into=c("aux1","aux2"),sep = " - ") |>
        mutate(group = paste0(aux2," - ",aux1)) |>
        select(group,flow=flow_ba)
    )

  plot_df <-
    bind_rows(
      left_join(
        flowdat_ab |>
          select(group,ya,ym,yb,ye,yf,yg) |>
          tidyr::pivot_longer(-group) |>
          mutate(point=sub('.', '', name)) |>
          select(group,point,y=value),
        flowdat_ab |>
          select(group,xa,xm,xb,xe,xf,xg) |>
          tidyr::pivot_longer(-group) |>
          mutate(point=sub('.', '', name)) |>
          select(group,point,x=value),
        by=c("group","point")
      ) |>
        mutate(dir="ab"),
      left_join(
        flowdat_ba |>
          select(group,ya,ym,yb,ye,yf,yg) |>
          tidyr::pivot_longer(-group) |>
          mutate(point=sub('.', '', name)) |>
          select(group,point,y=value),
        flowdat_ba |>
          select(group,xa,xm,xb,xe,xf,xg) |>
          tidyr::pivot_longer(-group) |>
          mutate(point=sub('.', '', name)) |>
          select(group,point,x=value),
        by=c("group","point")
      ) |>
        mutate(dir="ba")
    ) |>
    # order by sum of flows to have bigger flows on top
    left_join(flowsums,by="group") |>
    arrange(flowsum) |>
    #
    # mutate(group=paste0(group,"_",dir))|>
    # create unique group name for each direction
    separate(group,into=c("aux1","aux2"),sep = " - ") |>
    mutate(group=ifelse(
      dir=="ab",
      paste0(aux1," - ", aux2),
      paste0(aux2," - ", aux1)
    ))|>
    # add the flows for each arrow (for fill)
    left_join(flows,by="group") |>
    mutate(
      label=paste0("Route: ",group,"\nFlow: ",flow),
      group=forcats::fct_reorder(group,flowsum)
    )

  # for the nodes, calculate the flow to be visualized (as fill) from the node_fill_factor and their flow sum
  nodes <-
    nodes |>
    mutate(flow=flowsum*node_fill_factor)

  nodes_poly <-
    nodes |>
    group_by(id) |>
    group_split() |>
    lapply(function(x){
      coords <- get_circle_coords(c(x$x,x$y),x$adj_radius)
      coords$group=x$id
      coords$flowsum=x$flowsum
      coords$flow=x$flow
      return(coords)
    }) |>
    bind_rows()|>
    mutate(label = paste0("Node: ",group,"\nTotal Flow: ",flowsum))


  # add the nodes and edges to the base plot
  # ( suppressing warnings for the dummy variable text which is only used for plotly tooltips)
  withCallingHandlers({
    p <-
      p +
      geom_polygon(data=plot_df,aes(x,y,group=group,fill=flow,text=label),alpha=alpha,col=outline_col,linewidth=outline_linewidth)+
      geom_polygon(data=nodes_poly,aes(x=x,y=y,group=group,fill=flow,text=label),col=outline_col,linewidth=outline_linewidth)
  }, warning = function(w) {
    if (grepl("text",w,fixed=T)){invokeRestart("muffleWarning")}
  })

  return(p)
}


#' Credit to https://stackoverflow.com/a/6863490
#' Helper function to create coordinates for circles of nodes
#'
#' @param center center y and y coordinates
#' @param r radius
#' @param npoints number of points
#'
#' @return a dataframe with x and y coordinates of the circle
get_circle_coords <-
  function(center = c(0,0),r = 1, npoints = 25){
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
