utils::globalVariables(c("a_b","a_m","adj_radius","adj_radius_a","adj_radius_b",
                         "aes","aux1","aux2","e","flow","flow_ab","flow_ba","flows",
                         "flowsum","group","id","id_a","id_b","label","m_c",
                         "name","off_x","off_y","point","radius","value",
                         "width","x","xa","xb","xe","xf","xg","xm","xz","y",
                         "ya","yb","ye","yf","yg","ym","yz","o","d","."))

#' Add a list of flow maps to a ggplot, creating a list of plots
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param p The plot to which the flowmap should be added.
#' @param flowdat A list of input dataframes. See details below.
#' @param od As an alternative to \code{flowdat}, a list of dataframes with the origin-destination pairs and the flow between them.  Must contain the columns o, d, value. \code{nodes} must be provided as well. See details below.
#' @param nodes As an alternative to \code{flowdat}, a list of dataframes with the nodes of the network. Must contain the columns name, x, y. See details below.
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
#'
#' This function creates a list of ggplot objects, each containing a flow map based on the provided data, which match in scales and are thus comparable. It is designed to work with a list of dataframes, where each dataframe represents for example a different day. The function prepares the data for plotting and then adds the flow map to the ggplot object.
#' The function requires as inputs a list of input dataframes \code{flowdat} which contain for every combination of two nodes a and b the coordinates of these nodes as well as the intensity of flow between those nodes in both directions (a to b, b to a). The dataframe should have the following columns:
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
#' Alternatively, the function can take as input a list of dataframes \code{od} which contain the origin-destination pairs and the flow between them. The dataframe should have the following columns:
#' \itemize{
#' \item \strong{o:} The unique id of the origin node
#' \item \strong{d:} The unique id of the destination node
#' \item \strong{value:} The intensity of flow between the origin and destination
#' }
#' In this case, the function also requires a list of dataframes \code{nodes} which contain the coordinates of the nodes. The dataframe should have the following columns:
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
#' @return A list of ggplots, each corresponding to the input ggplot with an additional polygon layer for the flow arrows and an additional polygon layer for the nodes. One output is generated for each input element (in \code{flowdat}, \code{od}, or \code{nodes}).
#' @author Johannes Mast
#' @export
#'
#' @examples
#'
#' flowdatA <-
#' data.frame(
#'   id_a = c("X1","X2","X3","X3","X1"),
#'   id_b = c("X8","X7","X1","X8","X7"),
#'   xa = c(2,14,10,10,2),
#'   ya = c(6,10,9,9,6),
#'   xb = c(10,4,2,10,4),
#'   yb = c(4,10,6,4,10),
#'   flow_ab = c(2,1,1,1,1),
#'   flow_ba = c(8,1,1,1,2))
#'
#' flowdatB <-
#'   data.frame(
#'     id_a = c("X1","X2","X3","X3","X1"),
#'     id_b = c("X8","X7","X1","X8","X7"),
#'     xa = c(2,14,10,10,2),
#'     ya = c(6,10,9,9,6),
#'     xb = c(10,4,2,10,4),
#'     yb = c(4,10,6,4,10),
#'     flow_ab = c(2,3,2,0.2,1),
#'     flow_ba = c(3,3,2,1,5))
#'
#'
#' flowdatC <-
#'   data.frame(
#'     id_a = c("X1","X2","X3","X3","X1"),
#'     id_b = c("X8","X7","X1","X8","X7"),
#'     xa = c(2,14,10,10,2),
#'     ya = c(6,10,9,9,6),
#'     xb = c(10,4,2,10,4),
#'     yb = c(4,10,6,4,10),
#'     flow_ab = c(1,1,2,1,1)/2,
#'     flow_ba = c(3,3,2,1,5)/3)
#'
#'
#' list_of_flowdats <- list(flowdatA, flowdatB, flowdatC)
#'
#' library(ggplot2)
#' base_plot <-
#'   ggplot()+
#'   theme_bw()+
#'   coord_equal()
#'
#' flowmap_plots <-
#'   base_plot |>
#'   add_flowmap_list(list_of_flowdats,
#'                    legend_gradient = TRUE,
#'                    add_legend = "bottom",
#'                    k_nodes = 3)
#'
#'
#'
#'

add_flowmap_list <- function(p,flowdat=NULL,od=NULL,nodes=NULL,outline_linewidth=0.01,alpha=0.8,nodes_alpha=0.8,outline_col="black",k_nodes=NULL,node_buffer_factor = 1.2, node_radius_factor = 1, edge_offset_factor = 1, node_fill_factor = NULL, edge_width_factor = 1.2, arrow_point_angle = 45,add_legend="none",legend_nudge_x=0,legend_nudge_y=0,legend_col="gray",legend_gradient=FALSE){

  if(!add_legend %in% c("top","bottom","none","right","left")){
    warning("add_legend must be either 'top', 'bottom','right','left', or 'none'. Defaulting to 'none'.")
    add_legend <- "none"}


  # check that either a list of flowdat or a list of od and nodes is provided. They must be lists.
  if(is.null(flowdat) && (is.null(od) || is.null(nodes))){
    stop("Either flowdat or both od and nodes must be provided.")
  }
  # check that no mix of flowdat and od is provided
  if(!is.null(flowdat) && (!is.null(od) || !is.null(nodes))){
    stop("Either flowdat or both od and nodes must be provided, not a mix of both.")
  }
  # check that flowdat, if provided, is a list of data frames
  if(!is.null(flowdat) && !is.list(flowdat)){
    stop("flowdat must be a list.")
  }
  # check that od, if provided, is a list
  if(!is.null(od) && !is.list(od)){
    stop("od must be a list.")
  }
  # check that nodes, if provided, is a list
  if(!is.null(nodes) && !is.list(nodes)){
    stop("nodes must be a list.")
  }
  # check that od and nodes, if provided, are of the same length
  if(!is.null(od) && !is.null(nodes) && length(od) != length(nodes)){
    stop("od and nodes must be of the same length.")
  }

  # if flowdat is NULL, but od and nodes are provided, create a list of NULL with the same length as od
  if(is.null(flowdat) && !is.null(od) && !is.null(nodes)){
    flowdat <- rep(list(NULL), length(od))
  }
  # if flowdat is not NULL, but od and nodes are not provided, create a list of NULL with the same length as flowdat
  if(is.null(od) || is.null(nodes)){
    od <- rep(list(NULL), length(flowdat))
    nodes <- rep(list(NULL), length(flowdat))
  }

  # for every element in flowdat, apply prep_flowmap
  # repeat the preparation of flowmaps with prep_flowmap, but apply an edge_width_factor multiplied by the respective edges_normalisation_factor
  prepared_flowmaps <- purrr::map(seq_along(flowdat), function(f_i) {
    prep_flowmap(
      flowdat = flowdat[[f_i]],
      od = od[[f_i]],
      nodes = nodes[[f_i]],
      k_nodes = k_nodes,
      node_buffer_factor = node_buffer_factor,
      node_radius_factor = node_radius_factor,
      edge_offset_factor = edge_offset_factor,
      node_fill_factor = node_fill_factor,
      edge_width_factor = edge_width_factor,
      arrow_point_angle = arrow_point_angle
    )
  })

  #get the overall maximum of max(prepared_flowmaps[[n]]$edges$flowsum) for all n list elements
  if(!is.null(prepared_flowmaps)){
    max_flowsum_edges <- max(purrr::map_dbl(prepared_flowmaps, function(x) max(x$edges$flowsum)))
    min_flowsum_edges <- min(purrr::map_dbl(prepared_flowmaps, function(x) min(x$edges$flowsum)))
    edges_normalisation_factors <- purrr::map_dbl(prepared_flowmaps, function(x) max(x$edges$flowsum)/max_flowsum_edges)
    # same for the nodes
    max_flowsum_nodes <- max(purrr::map_dbl(prepared_flowmaps, function(x) max(x$nodes$flowsum)))
    min_flowsum_nodes <- min(purrr::map_dbl(prepared_flowmaps, function(x) min(x$nodes$flowsum)))
    nodes_normalisation_factors <- purrr::map_dbl(prepared_flowmaps, function(x) max(x$nodes$flowsum)/max_flowsum_nodes)
  }

  # repeat the preparation of flowmaps with prep_flowmap, but apply an edge_width_factor multiplied by the respective edges_normalisation_factor
  prepared_flowmaps <- purrr::map(seq_along(flowdat), function(f_i) {
    prep_flowmap(
      flowdat = flowdat[[f_i]],
      od = od[[f_i]],
      nodes = nodes[[f_i]],
      k_nodes = k_nodes,
      node_buffer_factor = node_buffer_factor,
      node_radius_factor = node_radius_factor * nodes_normalisation_factors[[f_i]],
      edge_offset_factor = edge_offset_factor,
      node_fill_factor = node_fill_factor,
      edge_width_factor = edge_width_factor * edges_normalisation_factors[[f_i]],
      arrow_point_angle = arrow_point_angle
    )
  })


  plot_flowmap <- function(p, plot_df, alpha, outline_col,
                           outline_linewidth, nodes_poly, nodes_alpha, add_legend, maxwidth,minwidth,
                           edges_normalisation_factor,nodes_normalisation_factor,
                           legend_nudge_x, legend_nudge_y, legend_gradient, legend_col) {

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

      # maxwidth <- prepared_flowmap$maxwidth
      # minwidth <- prepared_flowmap$minwidth

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
      # maxwidth <- prepared_flowmap$maxwidth
      # minwidth <- prepared_flowmap$minwidth

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

  # apply plot_flowmap to every element in prepared_flowmaps and store the result in a list

  plots <- list()
  if(!is.null(prepared_flowmaps)){
    for(i in seq_along(prepared_flowmaps)){
      plots[[i]] <- plot_flowmap(
        p = p,
        plot_df = prepared_flowmaps[[i]]$edges,
        alpha = alpha,
        outline_col = outline_col,
        outline_linewidth = outline_linewidth,
        nodes_poly = prepared_flowmaps[[i]]$nodes,
        nodes_alpha = nodes_alpha,
        add_legend = add_legend,
        maxwidth = prepared_flowmaps[[i]]$maxwidth,
        minwidth = prepared_flowmaps[[i]]$minwidth,
        edges_normalisation_factor = edges_normalisation_factors[i],
        nodes_normalisation_factor = nodes_normalisation_factors[i],
        legend_nudge_x = legend_nudge_x,
        legend_nudge_y = legend_nudge_y,
        legend_gradient = legend_gradient,
        legend_col = legend_col
      )
    }
  }

  # 2DO: finally, force all plots to have the same x and y limits (keep the current settings for expand)
  if(length(plots)>0){
    # get the current expand for all plots
    # x_expand <- purrr::map(plots, function(x) ggplot2::layer_scales(x)$x$expand)
    # y_expand <- purrr::map(plots, function(x) ggplot2::layer_scales(x)$y$expand)
    # xlims <- range(unlist(purrr::map(plots, function(x) ggplot2::layer_scales(x)$x$range$range)))
    # ylims <- range(unlist(purrr::map(plots, function(x) ggplot2::layer_scales(x)$y$range$range)))
    # plots <- purrr::map(plots, function(x) x + ggplot2::xlim(xlims) + ggplot2::ylim(ylims))
  }
  # also force all plots to have the same fill limits of min_flowsum_edges and max_flowsum_edges
  if(length(plots)>0){
    # suppressing warnings
    withCallingHandlers({
      plots <- purrr::map(plots, function(x) x + ggplot2::scale_fill_gradient(limits = c(min_flowsum_edges, max_flowsum_edges)))
    }, warning = function(w) {
      if (grepl("fill",w,fixed=T) || grepl("exists",w,fixed=T)){
        invokeRestart("muffleWarning")
      }
    })



  }

  # return the list of plots
  return(plots)

}
