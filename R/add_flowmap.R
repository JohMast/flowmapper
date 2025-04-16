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
#' @param legend_gradient If TRUE, the legend color will be a gradient from min to max flow. If FALSE, the legend will be a single color.
#' @param outline_col Color of the outline of the edges.
#' @param arrow_point_angle Controls the pointiness of the edges.
#' @param add_legend Add a legend for width to the plot? Must be one of "none","bottom","top","left", or "right". (Experimental)
#' @param legend_col If \code{add_legend}, sets a monotone color for the legend. By default is "gray".
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
#' @importFrom purrr map
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
add_flowmap <- function(p,flowdat=NULL,od=NULL,nodes=NULL,outline_linewidth=0.01,alpha=0.8,nodes_alpha=0.8,outline_col="black",k_nodes=NULL,node_buffer_factor = 1.2, node_radius_factor = 1, edge_offset_factor = 1, node_fill_factor = NULL, edge_width_factor = 1.2, arrow_point_angle = 45,add_legend="none",legend_nudge_x=0,legend_nudge_y=0,legend_col="gray",legend_gradient=FALSE){

  if(!add_legend %in% c("top","bottom","none","right","left")){
    warning("add_legend must be either 'top', 'bottom','right','left', or 'none'. Defaulting to 'none'.")
    add_legend <- "none"}

  prepared_flowmap <- prep_flowmap(
    flowdat = flowdat,
    od = od,
    nodes = nodes,
    k_nodes = k_nodes,
    node_buffer_factor = node_buffer_factor,
    node_radius_factor = node_radius_factor,
    edge_offset_factor = edge_offset_factor,
    node_fill_factor = node_fill_factor,
    edge_width_factor = edge_width_factor,
    arrow_point_angle = arrow_point_angle
  )

  plot_df <- prepared_flowmap[[1]]
  nodes_poly <- prepared_flowmap[[2]]
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

    maxwidth <- prepared_flowmap$maxwidth
    minwidth <- prepared_flowmap$minwidth

    # minflow <- min(c(flowdat_ab$flow_ab,flowdat_ba$flow_ba))
    # maxflow <- max(c(flowdat_ab$flow_ab,flowdat_ba$flow_ba))
    minflow <- min(plot_df$flow)
    maxflow <- max(plot_df$flow)
    meanflow <- mean(c(minflow,maxflow))

    l1_y <- pa_d
    l1_x <- pa_l
    l2_y <- pa_d
    l2_x <- pa_l + hrange*0.1
    l3_y <- pa_d
    l3_x <- pa_r - hrange*0.1
    l4_y <- pa_d
    l4_x <- pa_r
    l5_y <- pa_d - maxwidth
    l5_x <- pa_r
    l6_y <- pa_d - maxwidth
    l6_x <- pa_r - hrange*0.1
    l7_y <- pa_d - minwidth
    l7_x <- pa_l + hrange*0.1
    l8_y <- pa_d - minwidth
    l8_x <- pa_l
    # end segments
    legend_outline <-
      data.frame(x=c(l1_x,l2_x,l3_x,l4_x,l5_x,l6_x,l7_x,l8_x),
                 y=c(l1_y,l2_y,l3_y,l4_y,l5_y,l6_y,l7_y,l8_y),
                 flow=meanflow,
                 group="outline")
    legend_tail <-
      data.frame(x=c(l1_x,l2_x,l7_x,l8_x),
                 y=c(l1_y,l2_y,l7_y,l8_y),
                 flow=minflow,
                 group="tail")
    legend_head <-
      data.frame(x=c(l3_x,l4_x,l5_x,l6_x),
                 y=c(l3_y,l4_y,l5_y,l6_y),
                 flow=maxflow,
                 group="head")
    # intermediate segments
    fracs <- seq(from=0,to=1,length.out=50)
    intermediate_segment <- function(i){
      frac <- fracs[i]
      prev_frac <- fracs[i-1]
      y01 = l2_y
      x01 = l2_x + prev_frac*(l3_x-l2_x)
      y02 = l2_y
      x02 = l2_x + frac*(l3_x-l2_x)
      y03 = l7_y + frac*(l6_y-l7_y)
      x03 = l7_x + frac*(l3_x-l2_x)
      y04 = l7_y + prev_frac*(l6_y-l7_y)
      x04 = l7_x + prev_frac*(l3_x-l2_x)
      data.frame(x=c(x01,x02,x03,x04),
                 y=c(y01,y02,y03,y04),
                 flow=minflow + frac*(maxflow-minflow),
                 group=paste("intermediate",round(frac,3),sep="_"))
    }
    #combined segment df
    legend_df <-
      bind_rows(legend_tail,
                2:length(fracs) |> purrr::map(intermediate_segment) |> bind_rows(),
                legend_head)



    #nudge in x direction
    nudge_x <- - 0.0* hrange
    legend_df$x <- legend_df$x + nudge_x + legend_nudge_x
    legend_outline$x <- legend_outline$x + nudge_x + legend_nudge_x


    #nudge in y direction depends on whether the legend is top or bottom
    if(add_legend == "top"){
      #nudge in y direction
      nudge_y <- + 1.2* vrange
      legend_df$y <- legend_df$y + nudge_y + legend_nudge_y
      legend_outline$y <- legend_outline$y + nudge_y + legend_nudge_y
    }

    if(add_legend == "bottom"){
      #nudge in y direction
      nudge_y <- - 0.2* vrange
      legend_df$y <- legend_df$y + nudge_y + legend_nudge_y
      legend_outline$y <- legend_outline$y + nudge_y + legend_nudge_y
    }


    if(!legend_gradient){
      p <- p +
        geom_polygon(data=legend_df,aes(x=x,y=y,group=group),fill=legend_col,alpha=alpha,col=NA,linewidth=outline_linewidth)
    }else{
      p <- p+ geom_polygon(data=legend_df,aes(x=x,y=y,fill=flow,group=group),alpha=alpha,col=NA,linewidth=outline_linewidth)
    }

    p <- p+
    # min annotation
    annotate(geom = "text",
             x = legend_tail$x[2],
             y = legend_df$y[1]+vrange*0.05,
             label=short_scale(minflow),
             color=legend_col)+
    annotate("segment",
             x = legend_tail$x[2],
             y = legend_df$y[1],
             xend = legend_tail$x[2],
             yend = legend_df$y[1]-minwidth,col="white")+

    # mean annotation
    annotate(geom = "text",
             x = mean(c(legend_head$x[3],legend_tail$x[3])),
             y = legend_df$y[1]+vrange*0.05,
             label=short_scale(meanflow),
             color=legend_col)+
    annotate("segment",
             x = mean(c(legend_head$x[3],legend_tail$x[3])),
             y = legend_df$y[1],
             xend = mean(c(legend_head$x[3],legend_tail$x[3])),
             yend = legend_df$y[1]-mean(c(minwidth,maxwidth)),col="white")+
    # max annotation
    annotate(geom = "text",
             x = legend_head$x[1],
             y = legend_df$y[1]+vrange*0.05,
             label=short_scale(maxflow),
             color=legend_col)+
    annotate("segment",
             x = legend_head$x[1],
             y = legend_df$y[1],
             xend = legend_head$x[1],
             yend = legend_df$y[1]-maxwidth,col="white")

    p <- p+
      geom_polygon(data=legend_outline,aes(x=x,y=y),fill=NA,alpha=alpha,col=outline_col,linewidth=outline_linewidth)

  }





  if(add_legend %in% c("left","right")){
    pa_l <- min(nodes_poly$x)
    pa_r <- max(nodes_poly$x)
    pa_d <- min(nodes_poly$y)
    pa_u <- max(nodes_poly$y)
    hrange <- pa_r - pa_l
    vrange <- pa_u - pa_d

    # maxwidth <- max(max(flowdat_ba$width),max(flowdat_ab$width))/2
    # minwidth <- min(min(flowdat_ba$width),min(flowdat_ab$width))/2
    maxwidth <- prepared_flowmap$maxwidth
    minwidth <- prepared_flowmap$minwidth

    #flip the legend if on left
    if(add_legend=="left"){
      minwidth <- -1*minwidth
      maxwidth <- -1*maxwidth
    }


    # minflow <- min(c(flowdat_ab$flow_ab,flowdat_ba$flow_ba))
    # maxflow <- max(c(flowdat_ab$flow_ab,flowdat_ba$flow_ba))
    minflow <- min(plot_df$flow)
    maxflow <- max(plot_df$flow)
    meanflow <- mean(c(minflow,maxflow))

    l1_y <- pa_d
    l1_x <- pa_r
    l2_y <- pa_d + vrange*0.1
    l2_x <- pa_r
    l3_y <- pa_u - vrange*0.1
    l3_x <- pa_r
    l4_y <- pa_u
    l4_x <- pa_r
    l5_y <- pa_u
    l5_x <- pa_r + maxwidth
    l6_y <- pa_u - vrange*0.1
    l6_x <- pa_r + maxwidth
    l7_y <- pa_d + vrange*0.1
    l7_x <- pa_r + minwidth
    l8_y <- pa_d
    l8_x <- pa_r + minwidth
    # end segments
    legend_outline <-
      data.frame(x=c(l1_x,l2_x,l3_x,l4_x,l5_x,l6_x,l7_x,l8_x),
                 y=c(l1_y,l2_y,l3_y,l4_y,l5_y,l6_y,l7_y,l8_y),
                 flow=meanflow,
                 group="outline")

    legend_tail <-
      data.frame(x=c(l1_x,l2_x,l7_x,l8_x),
                 y=c(l1_y,l2_y,l7_y,l8_y),
                 flow=minflow,
                 group="tail")
    legend_head <-
      data.frame(x=c(l3_x,l4_x,l5_x,l6_x),
                 y=c(l3_y,l4_y,l5_y,l6_y),
                 flow=maxflow,
                 group="head")
    # intermediate segments
    fracs <- seq(from=0,to=1,length.out=50)
    intermediate_segment <- function(i){
      frac <- fracs[i]
      prev_frac <- fracs[i-1]
      y01 = l2_y + prev_frac*(l3_y-l2_y)
      x01 = l2_x
      y02 = l7_y + frac*(l3_y-l2_y)
      x02 = l2_x #+ frac*(l3_x-l2_x)
      y03 = l7_y + frac*(l6_y-l7_y)
      x03 = l7_x + frac*(l6_x-l7_x)
      y04 = l7_y + prev_frac*(l3_y-l2_y)
      x04 = l7_x + prev_frac*(l6_x-l7_x)
      data.frame(x=c(x01,x02,x03,x04),
                 y=c(y01,y02,y03,y04),
                 flow=minflow + frac*(maxflow-minflow),
                 group=paste("intermediate",round(frac,3),sep="_"))
    }
    #combined segment df
    legend_df <-
      bind_rows(legend_tail,
                2:length(fracs) |> purrr::map(intermediate_segment) |> bind_rows(),
                legend_head
      )

    #nudge in y direction
    nudge_y <- - 0.0* vrange
    legend_df$y <- legend_df$y + nudge_y + legend_nudge_y

    #nudge in y direction depends on whether the legend is top or bottom
    if(add_legend == "left"){
      #nudge in x direction
      nudge_x <- - 1.1* hrange
      legend_df$x <- legend_df$x + nudge_x + legend_nudge_x
      legend_tail$x <- legend_tail$x + nudge_x + legend_nudge_x
      legend_head$x <- legend_head$x + nudge_x + legend_nudge_x
      legend_outline$x <- legend_outline$x + nudge_x + legend_nudge_x


      }

    if(add_legend == "right"){
      #nudge in y direction
      nudge_x <- + 0.2* hrange
      legend_df$x <- legend_df$x + nudge_x + legend_nudge_x
      legend_tail$x <- legend_tail$x + nudge_x + legend_nudge_x
      legend_head$x <- legend_head$x + nudge_x + legend_nudge_x
      legend_outline$x <- legend_outline$x + nudge_x + legend_nudge_x
    }

    if(!legend_gradient){
      p <- p+ geom_polygon(data=legend_df,aes(x=x,y=y,group=group),fill=legend_col,alpha=alpha,col=NA,linewidth=outline_linewidth)
    }else{
      p <- p+ geom_polygon(data=legend_df,aes(x=x,y=y,fill=flow,group=group),alpha=alpha,col=NA,linewidth=outline_linewidth)
    }
    p <- p+
      annotate("segment",
               x = legend_tail$x[2],
               y = legend_tail$y[2],
               xend = legend_tail$x[3],
               yend = legend_tail$y[3],col="white")+
      annotate("segment",
               y = mean(c(legend_tail$y[2],legend_head$y[1])),
               x = legend_df$x[1],
               yend = mean(c(legend_tail$y[2],legend_head$y[1])),
               xend = legend_df$x[1]+mean(c(minwidth,maxwidth)),col="white")+

      annotate("segment",
               x = legend_head$x[1],
               y = legend_head$y[1],
               xend = legend_head$x[4],
               yend = legend_head$y[4],col="white"
               )
    if(add_legend=="left"){
      p <-
        p +
        annotate(geom = "text",
                 x = legend_head$x[1],
                 y = legend_tail$y[2],
                 label=paste0(" ",short_scale(minflow)),
                 color=legend_col,
                 hjust = 0)+
        annotate(geom = "text",
                 y = mean(c(legend_head$y[1],legend_tail$y[2])),
                 x = legend_df$x[1],
                 label=paste0(" ",short_scale(meanflow)),
                 color=legend_col,
                 hjust = 0)+
        annotate(geom = "text",
                 x = legend_head$x[1],
                 y = legend_head$y[4],
                 label=paste0(" ",short_scale(maxflow)),
                 color=legend_col,
                 hjust = 0)
    }
    if(add_legend=="right"){
      p <-
        p +
        annotate(geom = "text",
                 x = legend_head$x[1],
                 y = legend_tail$y[2],
                 label=paste0(short_scale(minflow)," "),
                 color=legend_col,
                 hjust = 1)+
        annotate(geom = "text",
                 y = mean(c(legend_head$y[1],legend_tail$y[2])),
                 x = legend_head$x[1],
                 label=paste0(short_scale(meanflow)," "),
                 color=legend_col,
                 hjust = 1)+
        annotate(geom = "text",
                 x = legend_head$x[1],
                 y = legend_head$y[4],
                 label=paste0(short_scale(maxflow)," "),
                 color=legend_col,
                 hjust = 1)
    }
    p <- p+
      geom_polygon(data=legend_outline,aes(x=x,y=y),fill=NA,alpha=alpha,col=outline_col,linewidth=outline_linewidth)

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
