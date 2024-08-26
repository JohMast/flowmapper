utils::globalVariables(c("cutree","hclust","dist","clust","weighted.mean",
                         "id_a_old","id_b_old","clust_a","clust_b","id_merged","f"))
#' Use hierarchical clustering to merge nodes based on proximity
#'
#' @param flowdat The data containing flows from a to b, b to a, and the coordinates of a and b
#' @param k The number of nodes to keep.
#' @importFrom dplyr filter mutate group_by first select bind_rows left_join summarize ungroup slice_head
#' @return a dataframe of the same format as flowdat, but with some nodes (and their flows) merged. Note that this will in most cases contain some circular flows (a to a) even if the input flowdat did not.
hca_flowdat <- function(flowdat,k=20) {
  nodes <-
    bind_rows(
      flowdat |>
        mutate(f = flow_ab + flow_ba)  |>
        select(id = id_a, x = xa, y = ya, f),
      flowdat |>
        mutate(f = flow_ab + flow_ba) |>
        select(id = id_b, x = xb, y = yb, f)
    ) |>
    group_by(id) |>
    summarize(
      id = first(id),
      x = first(x),
      y = first(y),
      f = sum(f)
    )

  nodes$clust <-
    cutree(hclust(dist(nodes[, 2:3], method = "euclidean")), k = k)

  nodes_new <-
    nodes |>
    group_by(clust) |>
    mutate(
      id_merged = first(id),
      id_merged = paste0(first(id)," and ", n(), " others."),  #for pasting others
      x = weighted.mean(x, w = f),
      y = weighted.mean(y, w = f)
    ) |>
    ungroup()

  flow_long <- bind_rows(
    flowdat |>
      select(id_a_old=id_a,id_b_old=id_b,flow_ab) ,
    flowdat |>
      select(id_a_old=id_b,id_b_old=id_a,flow_ab=flow_ba)
  ) |>
    left_join(nodes_new |> select(id,clust_a=clust),by=c("id_a_old"="id")) |>
    left_join(nodes_new |> select(id,clust_b=clust),by=c("id_b_old"="id")) |>
    select(-id_a_old,-id_b_old)

  flow_long_grouped <-
    flow_long |>
    group_by(clust_a,clust_b) |>
    summarize(flow_ab=sum(flow_ab)) |>
    ungroup()

  clust_new <-
    nodes_new |>
    group_by(clust) |>
    slice_head(n=1)

  flowdat_new <-
    flow_long_grouped |>
    left_join(flow_long_grouped |> select(clust_a=clust_b,clust_b=clust_a,flow_ba=flow_ab),by=c("clust_a","clust_b"))|>
    filter(clust_a>=clust_b) |>
    ungroup()|>
    left_join(clust_new |> select(id_a=id_merged,clust_a=clust,xa=x,ya=y),by="clust_a")|>
    left_join(clust_new |> select(id_b=id_merged,clust_b=clust,xb=x,yb=y),by="clust_b") |>
    select(id_a,id_b,flow_ab,xa,ya,xb,yb,flow_ba)

  return(flowdat_new)
}
