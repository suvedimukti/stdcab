#------------------- fun 06- spatial_cluster_sample ----------------------------#
#'
#' Spatial Clustering of Points Data for Tidy-modeling
#'
#' @description This is a wrapper function around spatial_clustering_cv from spatialsample.
#'  Spatial cluster sampling splits a data into V groups groups using
#'  partitioning (kmeans)/ hierarchical(hclust) clustering of some variables,
#'  typically spatial coordinates. A resample of a analysis data consists
#'  of V-1 of the folds/clusters while the assessment set contains the final fold/cluster. In
#'  basic spatial cross-validation (i.e. no repeats), the number of resamples is equal to V.
#'
#' @details
#'  The variables in the `coords` argument, if input data is data.frame or
#'  extracted from sp, or sf data are used for  clustering of the data into
#'  disjointed sets. These clusters are used as the folds for cross-validation.
#'  Depending on how the data are distributed spatially.
#'
#' Since this function heavily relies on "spatialsample" all the attributes class
#' and attributes were not modified from. In fact, these information holds true in case of
#' \code{\link{repeated_spatial_cluster_sample}}.
#'
#' @param data input data set one of sp, sf or data.frame with X and Y as variables
#' @param coords (vector) pair of coordinates if data type is aspatial
#' @param v number of partitions of the data set or number of clusters
#' @param spatial (logical) if data set is spatial (when sf or sp) or aspatial (data.frame)
#' @param clust_method one of partitioning (default = kmeans) or one of
#' hierarchical methods(\code{\link[stats]{hclust}})
#' @param dist_clust the agglomeration method to be used.
#' This should be one of “ward.D”, “ward.D2”, “single”, “complete”,
#' “average” (= UPGMA), “mcquitty” (= WPGMA), “median” (= WPGMC) or
#' “centroid” (= UPGMC).
#' the dist_clust in the function is method in stats::hclust
#'
#' @param ... Extra arguments passed on to [stats::kmeans()] or [stats::hclust()]
#'
#' @importFrom rlang .data
#'
#' @return
#' A tibble with classes `spatial_cv`, `rset`, `tbl_df`, `tbl`, and
#'  `data.frame`. The results include a column for the data split objects and
#'  an identification variable `id`.
#'
#'
#' @references
#' A. Brenning, "Spatial cross-validation and bootstrap for the assessment of
#' prediction rules in remote sensing: The R package sperrorest," 2012 IEEE
#' International Geoscience and Remote Sensing Symposium, Munich, 2012,
#' pp. 5372-5375, doi: 10.1109/IGARSS.2012.6352393.
#'
#' Julia Silge (2021). spatialsample: Spatial Resampling Infrastructure. https://github.com/tidymodels/spatialsample,
#' https://spatialsample.tidymodels.org.
#'
#' Julia Silge, Fanny Chow, Max Kuhn and Hadley Wickham (2021). rsample: General Resampling Infrastructure. R package version 0.1.1.
#' https://CRAN.R-project.org/package=rsample
#'
#' @export
#'
#' @examples
#'
#' # spatial point clustering
#'
#' # read data
#'
#' data("landcover")
#'
#' # setting seeds
#'
#' set.seed(1318)
#'
#' spc_fold<- spatial_cluster_sample(data = landcover,coords = NULL,v = 10,spatial = TRUE,
#'                                  clust_method = "kmeans")
#' class(spc_fold)
#'
#  using meuse data from sp
#'
#' \dontrun{
#' data("landcover")
#'
#' scv<- spatial_cluster_sample(data = landcover,coords = NULL, v = 10, spatial = TRUE,
#'            clust_method = "kmeans",dist_clust = NULL,)
#' scv
#' }
#'

spatial_cluster_sample <- function(data = data, coords = NULL, v = 10, spatial = TRUE,
                                   clust_method = "kmeans", dist_clust = NULL, ...) {
  if (spatial) {
    # Check for availability of packages
    pkg <- c("sf", "purrr", "rsample")
    pkgna <- names(which(sapply(sapply(pkg, find.package, quiet = TRUE), length) == 0))

    if (length(pkgna) > 0) {
      message("This function requires ", paste0(pkg, collapse = ", "), " package(s) for spatial clustering.\nWould you like to install it now?\n1: yes\n2: no")
      user <- readline(prompt = "Selection: ")

      if (tolower(user) %in% c("1", "yes", "y")) {
        utils::install.packages(pkgna, lib = NULL)
      } else {
        stop("Please install the required packages or set spatial = FALSE.")
      }
    }

    invisible(lapply(pkg, library, character.only = TRUE))
  }

  if (is.null(coords) && spatial) {
    if (methods::is(data, "SpatialPoints") || methods::is(data, "SpatialPointsDataFrame") ||
        methods::is(data, "SpatVector")) {
      data <- sf::st_as_sf(data)
    } else if (!methods::is(data, "sf")) {
      stop("The 'data' object should be a spatial or sf object when spatial = TRUE.")
    }
  }

  if (spatial) {
    if (is.null(coords) && methods::is(data, "SpatialPoints")) {
      data <- sf::st_as_sf(data)
      cdat <- data.frame(sf::st_coordinates(data))
      data <- cbind(data, cdat)
      sf::st_geometry(data) <- NULL
      coords <- colnames(cdat)
    } else if (is.null(coords)) {
      cdat <- data.frame(sf::st_coordinates(data))
      data <- cbind(data, cdat)
      sf::st_geometry(data) <- NULL
      coords <- colnames(cdat)
    }
  }

  split_objs <- spatial_clustering_splits_cv(
    data = data, coords = coords, v = v,
    clust_method = clust_method, dist_clust = dist_clust, ...
  )

  split_objs$splits <- purrr::map(split_objs$splits, rm_out)

  cv_att <- list(v = v, repeats = 2)

  rsample::new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("spatial_clustering_cv", "rset")
  )
}

spatial_clustering_splits_cv <- function(data, coords, clust_method = clust_method,
                                         v = v, dist_clust = dist_clust, ...) {
  if (!is.numeric(v) || length(v) != 1) {
    rlang::abort("`v` must be a single integer.")
  }

  if (is.null(clust_method)) {
    dist_clust <- NULL
    warning("clust_method is NULL, therefore, the default 'kmeans' will be used by setting dist_clust to NULL.")
  }

  if (is.null(dist_clust) || !clust_method == "kmeans") {
    clust_method <- "kmeans"
  }

  sclust <- list()

  if (clust_method == "kmeans" || is.null(clust_method)) {
    n <- nrow(data)
    clusters <- stats::kmeans(data[coords], centers = v, ...)
    folds <- clusters$cluster
    idx <- seq_len(n)
    indices <- split_unnamed(idx, folds)
    indices <- lapply(indices, default_complement, n = n)
    split_objs <- purrr::map(indices, rsample::make_splits,
                             data = data,
                             class = "spatial_clustering_split"
    )

    tibble::tibble(
      splits = split_objs,
      id = names0(length(split_objs), "Fold")
    )
  } else if (clust_method == "hclust") {
    n <- nrow(data)
    coorddata <- data[coords]

    if (!methods::is(coorddata, "data.frame")) {
      coorddata <- data.frame(coorddata)
    }

    d <- dist(coorddata, method = "euclidean")
    hc <- stats::hclust(d, method = dist_clust)
    folds <- stats::cutree(hc, k = v)
    idx <- seq_len(n)
    indices <- split_unnamed(idx, folds)
    indices <- lapply(indices, default_complement, n = n)
    split_objs <- purrr::map(indices, rsample::make_splits,
                             data = data,
                             class = "spatial_clustering_split"
    )

    tibble::tibble(
      splits = split_objs,
      id = names0(length(split_objs), "Fold")
    )
  } else {
    stop("Only 'kmeans' and 'hclust' methods are supported.")
  }
}
