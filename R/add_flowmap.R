utils::globalVariables(c("a_b","a_m","adj_radius","adj_radius_a","adj_radius_b",
                         "aes","aux1","aux2","e","flow","flow_ab","flow_ba","flows",
                         "flowsum","group","id","id_a","id_b","label","m_c",
                         "name","off_x","off_y","point","radius","value",
                         "width","x","xa","xb","xe","xf","xg","xm","xz","y",
                         "ya","yb","ye","yf","yg","ym","yz","o","d","."))

#' Add a flow map to a ggplot
#'
#' @param p The plot to which the flowmap should be added.
#' @param flowdat Input dataframe. See details below.
#' @param od As an alternative to \code{flowdat}, dataframe with the origin-destination pairs and the flow between them.  Must contain the columns o, d, value. \code{nodes} must be provided as well. See details below.
#' @param nodes As an alternative to \code{flowdat}, a dataframe with the nodes of the network. Must contain the columns name, x, y. See details below.
#' @param k_nodes Number of clusters to group nodes into. If defined, nodes will be clustered hierarchically based on spatial proximity. By default, no clustering will be applied.
#' @param node_buffer_factor Controls the distance between the nodes and the edges ( in multiple of the nodes' radii).
#' @param node_radius_factor Controls the size of the nodes.
#' @param node_fill_factor Controls the downscaling of the fill of the nodes ( as to not outshine the edges ).
#' @param edge_offset_factor Controls the distance between the parallel arrows.
#' @param edge_width_factor Controls the width of the edges.
#' @param outline_linewidth The linewidth of the outline of the arrows.
#' @param alpha Opacity of the edges.
#' @param nodes_alpha Opacity of the nodes.
#' @param outline_col Color of the outline of the edges.
#' @param arrow_point_angle Controls the pointiness of the edges.
#' @param add_legend Add a legend for width to the plot? Must be one of "none","bottom","top","left", or "right". (Experimental)
#' @param legend_col If \code{add_legend}, controls the color of the legend. Default is grey.
#' @param legend_nudge_x Adjusts the horizontal position of the legend in map units.
#' @param legend_nudge_y Adjusts the vertical position of the legend in map units.
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
#' Alternatively, the function can take as input a dataframe \code{od} which contains the origin-destination pairs and the flow between them. The dataframe should have the following columns:
#' \itemize{
#' \item \strong{o:} The unique id of the origin node
#' \item \strong{d:} The unique id of the destination node
#' \item \strong{value:} The intensity of flow between the origin and destination
#' }
#' In this case, the function also requires a dataframe \code{nodes} which contains the coordinates of the nodes. The dataframe should have the following columns:
#' \itemize{
#' \item \strong{name:} The unique id of the node
#' \item \strong{x:} The x coordinate of the node
#' \item \strong{y:} The y coordinate of the node
#' }
#'
#' The function will impose coord_equal() on the ggplot.
#'
#' Inspired by \href{https://flowmap.gl/}{flowmap.gl}.
#'
#' @importFrom dplyr mutate select left_join summarize group_by ungroup bind_rows n arrange group_split n_distinct across where filter_all coalesce all_vars
#' @importFrom tidyr pivot_longer separate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_polygon annotate
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
add_flowmap <- function(p,flowdat=NULL,od=NULL,nodes=NULL,outline_linewidth=0.01,alpha=0.8,nodes_alpha=0.8,outline_col="black",k_nodes=NULL,node_buffer_factor = 1.2, node_radius_factor = 1, edge_offset_factor = 1, node_fill_factor = NULL, edge_width_factor = 1.2, arrow_point_angle = 45,add_legend="none",legend_nudge_x=0,legend_nudge_y=0,legend_col="gray"){

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
  if(!add_legend %in% c("top","bottom","none","right","left")){
    warning("add_legend must be either 'top', 'bottom','right','left', or 'none'. Defaulting to 'none'.")
    add_legend <- "none"}

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


  # add the nodes and edges to the base plot
  # ( suppressing warnings for the dummy variable text which is only used for plotly tooltips)
  withCallingHandlers({
    p <-
      p +
      geom_polygon(data=plot_df,aes(x,y,group=group,fill=flow,text=label),alpha=alpha,col=outline_col,linewidth=outline_linewidth)+
      geom_polygon(data=nodes_poly,aes(x=x,y=y,group=group,fill=flow,text=label),alpha=nodes_alpha,col=outline_col,linewidth=outline_linewidth)
  }, warning = function(w) {
    if (grepl("text",w,fixed=T)){invokeRestart("muffleWarning")}
  })


  if(add_legend %in% c("top","bottom")){
    pa_l <- min(nodes_poly$x)
    pa_r <- max(nodes_poly$x)
    pa_d <- min(nodes_poly$y)
    pa_u <- max(nodes_poly$y)
    hrange <- pa_r - pa_l
    vrange <- pa_u - pa_d

    maxwidth <- max(max(flowdat_ba$width),max(flowdat_ab$width))/2
    minwidth <- min(min(flowdat_ba$width),min(flowdat_ab$width))/2

    minflow <- min(c(flowdat_ab$flow_ab,flowdat_ba$flow_ba))
    maxflow <- max(c(flowdat_ab$flow_ab,flowdat_ba$flow_ba))
    meanflow <- mean(c(minflow,maxflow))

    l1_y <- pa_d
    l1_x <- pa_l
    l2_y <- pa_d
    l2_x <- pa_r
    l3_y <- pa_d - maxwidth
    l3_x <- pa_r
    l4_y <- pa_d - maxwidth
    l4_x <- pa_r - hrange*0.1
    l5_y <- pa_d - minwidth
    l5_x <- pa_l + hrange*0.1
    l6_y <- pa_d - minwidth
    l6_x <- pa_l

    legend_df <-
      data.frame(x=c(l1_x,l2_x,l3_x,l4_x,l5_x,l6_x),
           y=c(l1_y,l2_y,l3_y,l4_y,l5_y,l6_y),
           flow=maxflow)



    #nudge in x direction
    nudge_x <- - 0.0* hrange
    legend_df$x <- legend_df$x + nudge_x + legend_nudge_x

    #nudge in y direction depends on whether the legend is top or bottom
    if(add_legend == "top"){
      #nudge in y direction
      nudge_y <- + 1.2* vrange
      legend_df$y <- legend_df$y + nudge_y + legend_nudge_y
    }

    if(add_legend == "bottom"){
      #nudge in y direction
      nudge_y <- - 0.2* vrange
      legend_df$y <- legend_df$y + nudge_y + legend_nudge_y
    }


    p <-
      p+
    geom_polygon(data=legend_df,aes(x=x,y=y),fill=legend_col,alpha=alpha,col=outline_col,linewidth=outline_linewidth)+
    # min annotation
    annotate(geom = "text",
             x = legend_df$x[5],
             y = legend_df$y[1]+vrange*0.05,
             label=short_scale(minflow),
             color=legend_col)+
    annotate("segment",
             x = legend_df$x[5],
             y = legend_df$y[1],
             xend = legend_df$x[5],
             yend = legend_df$y[1]-minwidth,col="white")+

    # mean annotation
    annotate(geom = "text",
             x = mean(c(legend_df$x[4],legend_df$x[5])),
             y = legend_df$y[1]+vrange*0.05,
             label=short_scale(meanflow),
             color=legend_col)+
    annotate("segment",
             x = mean(c(legend_df$x[4],legend_df$x[5])),
             y = legend_df$y[1],
             xend = mean(c(legend_df$x[4],legend_df$x[5])),
             yend = legend_df$y[1]-mean(c(minwidth,maxwidth)),col="white")+
    # max annotation
    annotate(geom = "text",
             x = legend_df$x[4],
             y = legend_df$y[1]+vrange*0.05,
             label=short_scale(maxflow),
             color=legend_col)+
    annotate("segment",
             x = legend_df$x[4],
             y = legend_df$y[1],
             xend = legend_df$x[4],
             yend = legend_df$y[1]-maxwidth,col="white")
  }





  if(add_legend %in% c("left","right")){
    pa_l <- min(nodes_poly$x)
    pa_r <- max(nodes_poly$x)
    pa_d <- min(nodes_poly$y)
    pa_u <- max(nodes_poly$y)
    hrange <- pa_r - pa_l
    vrange <- pa_u - pa_d

    maxwidth <- max(max(flowdat_ba$width),max(flowdat_ab$width))/2
    minwidth <- min(min(flowdat_ba$width),min(flowdat_ab$width))/2

    #flip the legend if on left
    if(add_legend=="left"){
      minwidth <- -1*minwidth
      maxwidth <- -1*maxwidth
    }


    minflow <- min(c(flowdat_ab$flow_ab,flowdat_ba$flow_ba))
    maxflow <- max(c(flowdat_ab$flow_ab,flowdat_ba$flow_ba))
    meanflow <- mean(c(minflow,maxflow))

    l1_y <- pa_d
    l1_x <- pa_r
    l2_y <- pa_u
    l2_x <- pa_r
    l3_y <- pa_u
    l3_x <- pa_r + maxwidth
    l4_y <- pa_u - vrange*0.1
    l4_x <- pa_r + maxwidth
    l5_y <- pa_d + vrange*0.1
    l5_x <- pa_r + minwidth
    l6_y <- pa_d
    l6_x <- pa_r + minwidth

    legend_df <-
      data.frame(x=c(l1_x,l2_x,l3_x,l4_x,l5_x,l6_x),
             y=c(l1_y,l2_y,l3_y,l4_y,l5_y,l6_y),
             flow=maxflow)



    #nudge in y direction
    nudge_y <- - 0.0* vrange
    legend_df$y <- legend_df$y + nudge_y + legend_nudge_y

    #nudge in y direction depends on whether the legend is top or bottom
    if(add_legend == "left"){
      #nudge in x direction
      nudge_x <- - 1.1* hrange
      legend_df$x <- legend_df$x + nudge_x + legend_nudge_x
    }

    if(add_legend == "right"){
      #nudge in y direction
      nudge_x <- + 0.2* hrange
      legend_df$x <- legend_df$x + nudge_x + legend_nudge_x
    }

    p <- p+
      geom_polygon(data=legend_df,aes(x=x,y=y),fill=legend_col,alpha=alpha,col=outline_col,linewidth=outline_linewidth)+
      annotate("segment",
               x = legend_df$x[5],
               y = legend_df$y[1],
               xend = legend_df$x[5]+minwidth,
               yend = legend_df$y[1],col="white")+
      annotate("segment",
               y = mean(c(legend_df$y[4],legend_df$y[5])),
               x = legend_df$x[1],
               yend = mean(c(legend_df$y[4],legend_df$y[5])),
               xend = legend_df$x[1]+mean(c(minwidth,maxwidth)),col="white")+

      annotate("segment",
               x = legend_df$x[1],
               y = legend_df$y[4],
               xend = legend_df$x[1]+maxwidth,
               yend = legend_df$y[4],col="white"
               )
    if(add_legend=="left"){
      p <-
        p +
        annotate(geom = "text",
                 x = legend_df$x[5],
                 y = legend_df$y[1],
                 label=paste0(" ",short_scale(minflow)),
                 color=legend_col,
                 hjust = 0)+
        annotate(geom = "text",
                 y = mean(c(legend_df$y[4],legend_df$y[5])),
                 x = legend_df$x[1],
                 label=paste0(" ",short_scale(meanflow)),
                 color=legend_col,
                 hjust = 0)+
        annotate(geom = "text",
                 x = legend_df$x[1],
                 y = legend_df$y[4],
                 label=paste0(" ",short_scale(maxflow)),
                 color=legend_col,
                 hjust = 0)
    }
    if(add_legend=="right"){
      p <-
        p +
        annotate(geom = "text",
                 x = legend_df$x[5],
                 y = legend_df$y[1],
                 label=paste0(short_scale(minflow)," "),
                 color=legend_col,
                 hjust = 1)+
        annotate(geom = "text",
                 y = mean(c(legend_df$y[4],legend_df$y[5])),
                 x = legend_df$x[1],
                 label=paste0(short_scale(meanflow)," "),
                 color=legend_col,
                 hjust = 1)+
        annotate(geom = "text",
                 x = legend_df$x[1],
                 y = legend_df$y[4],
                 label=paste0(short_scale(maxflow)," "),
                 color=legend_col,
                 hjust = 1)
    }

  }

  return(p)
}


#' Create short scale format for numbers in the legend
#'
#' @param x The number
#' @param digits Significant digits
#'
#' @author Johannes Mast, credit: https://stackoverflow.com/a/59086755
#' @importFrom scales scientific
#' @importFrom dplyr case_when
short_scale = function(x, digits=3) {
  compress = function(x, n) {
    signif(x * 10^(-n), digits)
  }
  case_when(
    x >= 1e12   ~ paste0(compress(x, 12), "T"),
    x >= 1e9   ~ paste0(compress(x, 9), "G"),
    x >= 1e6   ~ paste0(compress(x, 6), "M"),
    x >= 1000  ~ paste0(compress(x, 3), "k"),
    x >= 1     ~ as.character(compress(x, 0)),
    x >= 0.001 ~ paste0(scales::scientific(x)),
    x >= 1e-6  ~ paste0(scales::scientific(x))
  )
}

#' Helper function to merge od data in long data and nodes to flowdat format
#' @title util_data_flow_to_flowdat
#' @description This function takes a flow data frame in long format and a data frame with the nodes coordinates and returns a flowdat data frame
#' @param nodes A data frame with the nodes of the network
#' @param flows A data frame with the flow data
#' @return A data frame with the flow data in flowdat format
#' @author Johannes Mast,
#' @importFrom dplyr full_join select rename mutate coalesce filter left_join
#' @examples
#' #nodes <- data.frame(name=c("a","b","c"),x=c(0,1,2),y=c(0,1,2))
#' #flow <- data.frame(o=c("a","b"),d=c("b","c"),value=c(1,2))
#' #util_data_flow_to_flowdat(nodes,flow)
util_data_flow_to_flowdat <- function(nodes,flows){

  missing_nodes_o = unique(flows$o) %in% nodes$name
  missing_nodes_d = unique(flows$d) %in% nodes$name
  if(sum(!missing_nodes_o)>0) message(sum(!missing_nodes_o), " flow origins with no match in nodes names")
  if(sum(!missing_nodes_d)>0) message(sum(!missing_nodes_d), " flow destinations with no match in nodes names")

  f <-
    flows |>
    dplyr::full_join(flows |> dplyr::select(d=o,o=d,flow_ba=value),by=c("o", "d")) |>
    dplyr::rename(flow_ab=value) |>
    dplyr::mutate(flow_ab=dplyr::coalesce(flow_ab,0L),
                  flow_ba=dplyr::coalesce(flow_ba,0L))|>
    dplyr::rename(id_a=o,id_b=d) |>
    dplyr::filter(id_a>=id_b)
  flowdat <-
    f |>
    dplyr::left_join(nodes |> dplyr::select(name,xa=x,ya=y),by=c("id_a"="name"))|>
    dplyr::left_join(nodes |> dplyr::select(name,xb=x,yb=y),by=c("id_b"="name"))
  return(flowdat)
}

#'
#' Helper function to create coordinates for circles of nodes
#' @author Johannes Mast, Credit to https://stackoverflow.com/a/6863490
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
