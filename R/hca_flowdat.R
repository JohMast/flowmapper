#' Use hierarchical clustering to merge nodes based on proximity
#'
#' @param flowdat The data containing flows from a to b, b to a, and the coordinates of a and b
#' @param k The number of nodes to keep.
#'
#' @return a dataframe of the same format as flowdat, but with some nodes (and their flows) merged. Note that this will in most cases contain some circular flows (a to a) even if the input flowdat did not.
hca_flowdat <- function(flowdat,k=20) {
  nodes <-
    bind_rows(
      flowdat %>%
        mutate(f = flow_ab + flow_ba) %>%
        select(id = id_a, x = xa, y = ya, f),
      flowdat %>%
        mutate(f = flow_ab + flow_ba) %>%
        select(id = id_b, x = xb, y = yb, f)
    ) %>%
    group_by(id) %>%
    summarize(
      id = first(id),
      x = first(x),
      y = first(y),
      f = sum(f)
    )

  nodes$clust <-
    cutree(hclust(dist(nodes[, 2:3], method = "euclidean")), k = k)

  nodes_new <-
    nodes %>%
    group_by(clust) %>%
    mutate(
      id_merged = first(id),
      #id_merged = paste0(id, collapse = " ; "),  #for pasting others
      x = weighted.mean(x, w = f),
      y = weighted.mean(y, w = f)
    ) %>%
    ungroup()

  flowdat_new <-
    flowdat %>%
    select(
      id_a_old = id_a,
      id_b_old = id_b,
      flow_ab_old = flow_ab,
      flow_ba_old = flow_ba
    ) %>%
    left_join(nodes_new %>%
                select(
                  id_a_old = id,
                  xa = x,
                  ya = y,
                  id_a = id_merged
                ),
              by = "id_a_old") %>%
    left_join(nodes_new %>%
                select(
                  id_b_old = id,
                  xb = x,
                  yb = y,
                  id_b = id_merged
                ),
              by = "id_b_old") %>%
    group_by(id_a, id_b) %>%
    summarize(
      flow_ab = sum(flow_ab_old),
      flow_ba = sum(flow_ba_old),
      xa = first(xa),
      xb = first(xb),
      ya = first(ya),
      yb = first(yb)
    ) |>
    ungroup()
  return(flowdat_new)
}
