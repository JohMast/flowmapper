#' prep_flowmap
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
#' @return A list with two dataframes: edges and nodes. The edges dataframe contains the coordinates of the edges, and the nodes dataframe contains the coordinates of the nodes. Additional list elements contain the maximum and minimum width of the arrows.
#' @importFrom dplyr mutate select left_join summarize group_by ungroup bind_rows n arrange group_split n_distinct across where filter_all coalesce all_vars
#' @importFrom tidyr pivot_longer separate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_polygon annotate
#' @importFrom purrr map
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
#' flowmapper:::prep_flowmap(testdata)
prep_flowmap <- function(flowdat=NULL,od=NULL,nodes=NULL,k_nodes=NULL,node_buffer_factor = 1.2, node_radius_factor = 1, edge_offset_factor = 1, node_fill_factor = NULL, edge_width_factor = 1.2, arrow_point_angle = 45){

  if(!is.null(od) & is.null(nodes)){
    stop("When providing data in od format, nodes (a dataframe with the nodes coordiantes) must be defined as well.")
  }

  if(!is.null(od) & is.null(nodes)){
    stop("When providing nodes, od must be defined as well.")
  }

  if(!is.null(flowdat) & !is.null(od)){
    stop("Only one of flowdat or od can be defined.")
  }

  if(is.null(flowdat) & is.null(od)){
    stop("Either flowdat or od must be defined.")
  }

  if(is.null(flowdat) & !is.null(od) & !is.null(nodes)){
    #force convert columns o and d to characters
    od <- od |> mutate(o=as.character(o),d=as.character(d))
    # force convert column name to character
    nodes <- nodes |> mutate(name=as.character(name))
    flowdat <- util_data_flow_to_flowdat(nodes,od)
  }

  if(inherits(flowdat,"grouped_df")){
    flowdat <- flowdat |> dplyr::ungroup()
  }
  # Some checks
  if(arrow_point_angle>75){arrow_point_angle <- 75; warning("Warning. arrow_point_angle cannot exceed 75 degrees")}
  if(arrow_point_angle<20){arrow_point_angle <- 20; warning("Warning. arrow_point_angle cannot be lower than 20 degrees")}

  if(!is.null(k_nodes)){
    flowdat <- hca_flowdat(flowdat,k_nodes)
  }else{
    if(n_distinct(flowdat$id_a)>50){
      warning("Number of flows very high. Consider setting k_nodes to cluster nodes.")
    }
  }

  flowdat <- flowdat |> dplyr::filter(id_a!=id_b)

  #force convert factor columns to character
  flowdat <- flowdat |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character))

  # remove any row with a missing value in any column
  flowdat <- flowdat |> dplyr::filter_all(dplyr::all_vars(!is.na(.)))

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


  if(is.null(node_fill_factor)){
    node_fill_factor <- max(c(flowdat$flow_ab,flowdat$flow_ba))/max(nodes$flowsum)
  }

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

  plot_df <-
    plot_df |> group_by(label) |> filter(sum(flow)>0) |> ungroup()

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

  maxwidth <- max(max(flowdat_ba$width),max(flowdat_ab$width))/2
  minwidth <- min(min(flowdat_ba$width),min(flowdat_ab$width))/2

  return(list(edges=plot_df,nodes=nodes_poly,maxwidth=maxwidth,minwidth=minwidth))
}
