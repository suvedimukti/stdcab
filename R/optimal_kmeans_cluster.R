#-------------- funs 08- optimal cluster ---------------------------------------#
#' Optimal K-means Clustering
#'
#' @description This function performs k-means clustering on spatial or
#' non-spatial data and determines the optimal number of clusters based on
#' either the within-cluster sum of squares or the average silhouette width.
#'
#' @param data The input data for clustering. It can be an 'sf' object,
#' a 'SpatialPoints' object, or a data frame with coordinates.
#' @param spatial Logical indicating whether the input data is spatial
#' (default is TRUE). If set to FALSE, the data is assumed to be non-spatial
#' and the clustering is performed on the provided coordinates.
#' @param coords The column names of the coordinates in the data
#' (required if spatial is set to FALSE).
#' @param max_cluster The maximum number of clusters to consider.
#' @param method The method to determine the optimal number of clusters.
#' It can be "wss" (within-cluster sum of squares) or "silhouette" (average silhouette width).
#'
#' @return A list containing the clustering results (data frame) and
#' the plot of the selected method (ggplot object).
#' @importFrom stats kmeans
#' @examples
#' \dontrun{
#' data("landcover")
#' okc <- optimal_kmeans_cluster(data = landcover,spatial = TRUE,coords = NULL,
#'                               max_cluster = 15, method =  "wss")
#' # look at data
#' okc$data
#' }
#'
optimal_kmeans_cluster <- function(data, spatial = TRUE, coords = NULL,
                                   max_cluster = 2L, method = c("wss", "silhouette")) {
  if ((!is.null(coords) && is.null(spatial)) || (!is.null(coords) && spatial == FALSE)) {
    coords <- data[, coords, drop = FALSE]
  }
  #------------
  if (is.null(coords) && is.null(spatial)) {
    stop("Either `coords` should be present as variables in `data`, or data should be in `sf` or `spatial` object")
  }
  if (!spatial == FALSE || !methods::missingArg(spatial) || !is.null(spatial)) {
    if (methods::is(data, "SpatialPoints")) {
      data <- sf::st_as_sf(data)
      xy <- data.frame(sf::st_coordinates(data))
      sf::st_geometry(data) <- NULL
    } else if (methods::is(data, "sf")) {
      data <- data
      xy <- data.frame(sf::st_coordinates(data))
      sf::st_geometry(data) <- NULL
    } else if (!is.null(coords)) {
      data <- data
      xy <- data[, coords]
    } else {
      stop("Input data should be a `sf` or `SpatialPoints` object when coords are not provided")
    }
  }

  if (max_cluster < 2) {
    stop("max_cluster must be greater than or equal to 2 (max_cluster >= 2)")
  }

  if (!method %in% c("wss", "silhouette")) {
    method <- "wss"
    warning("The supplied method is not one of (wss, silhouette). The method is defaulting to 'wss'.")
  }

  if (method == "wss") {
    set.seed(1318)
    clust_info <- list()
    for (i in 1:max_cluster) {
      wss <- kmeans(xy, i)
      with_ss <- wss$tot.withinss
      var_exp <- round(wss$betweenss / wss$totss, 3)

      df_vars <- data.frame(
        clust_num = i,
        with_ss = with_ss,
        var_exp = var_exp
      )

      clust_info[[i]] <- df_vars
    }

    wss_df <- do.call(rbind, clust_info)
  } else if (method == "silhouette") {
    set.seed(1318)
    clust_info <- list()
    for (i in 2:max_cluster) {
      sil <- kmeans(xy, i)
      avg_sil_width <- extract_ave_sil_width(stats::dist(xy), cluster = sil$cluster)
      var_exp <- round(sil$betweenss / sil$totss, 3)
      df_vars <- data.frame(
        clust_num = i,
        with_ss = avg_sil_width,
        var_exp = var_exp
      )

      clust_info[[i]] <- df_vars
    }
    wss_dfs <- do.call(rbind, clust_info)

    wss_df <- rbind(
      data.frame(
        clust_num = 1,
        with_ss = 0,
        var_exp = 0
      ),
      wss_dfs
    )
  }

  y_lab <- ifelse(method == "silhouette", "Average silhouette width", "Total sums of squares within clusters")

  gp <- ggplot2::ggplot(wss_df, ggplot2::aes(x = clust_num, y = with_ss), shape = 21, size = 6) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab("Number of clusters") +
    ggplot2::ylab(y_lab) +
    ggplot2::ggtitle(paste0("Clustering Method ", ": ", toupper(method))) +
    ggplot2::scale_x_continuous(breaks = c(1:max_cluster)) +
    ggplot2::theme_bw(12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(size = 11),
      axis.text.y.left = ggplot2::element_text(face = "bold", color = "black", size = 11),
      axis.text.x.bottom = ggplot2::element_text(face = "bold", color = "black", size = 11)
    )

  if (method == "silhouette") {
    gp <- gp + ggplot2::geom_vline(xintercept = which.max(wss_df$with_ss), linetype = 2, color = "blue")
  }

  print(gp)

  ret_list <- list(
    data = wss_df,
    plot = gp
  )

  return(ret_list)
}

# -------------------------- extract average silhouette width ----------------#
extract_ave_sil_width <- function(dist, cluster) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("to use silhouette information you have to have cluster package \ninstalled on your machine, please install 'cluster' package")
  }
  sil_info <- cluster::silhouette(cluster, dist)
  mean(sil_info[, 3])
}
