
#' Spatial cluster  cross-validation
#'
#' @description This is a wrapper functin around \code{\link[spatialsample]{spatial_clustering_cv}}.
#'  Spatial cluster sampling splits the data into V groups groups using partitioning (kmeans)/ hierarchical(hclust) clustering of some variables, typically
#'  spatial coordinates. A resample of the analysis data consists of V-1 of the folds/clusters while the assessment set contains the final fold/cluster. In
#'  basic spatial cross-validation (i.e. no repeats), the number of resamples.
#'  is equal to V.
#'
#' @details
#'  The variables in the `coords` argument, if input data is data.frame or extracted from sp, or sf data are used for  clustering of
#'  the data into disjointed sets. These clusters are used as the folds for cross-validation. Depending on how the
#'  data are distributed spatially.
#'
#'
#' @param data input data set one of sp, sf or data.frame with X and Y as variables
#' @param coords (vector) pair of coordinates if data type is aspatial
#' @param v number of partitions of the dataset or number of clusters
#' @param spatial (logical) if data set is spatial (when sf or sp) or aspatial (data.frame)
#' @param clust_method one of partitioning (default = kmeans) or one of hierarchical methods(\code{\link[stats]{hclust}})
#' @param dist_clust the agglomeration method to be used. This should be one of “ward.D”, “ward.D2”, “single”, “complete”,
#'  “average” (= UPGMA), “mcquitty” (= WPGMA), “median” (= WPGMC) or “centroid” (= UPGMC).
#'  the dist_clust in the funtion is method in stats::hclust
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
#'\href{https://spatialsample.tidymodels.org/}{spatialsample}: provide functions and classes for spatial resampling to use with rsample
#'
#' @export
#'
#' @examples
spatial_cluster_sample <- function(data = data, coords = NULL, v = 10, spatial = TRUE,
                                  clust_method = "kmeans",dist_clust = NULL, ...){

  if(spatial==TRUE){
    # check for availability of packages
    pkg<-  c("sf", "sp", "purrr", "rsample")
    pkgna <- names(which(sapply(sapply(pkg, find.package, quiet = TRUE), length)==0))
    if (length(pkgna) >0){
      message(" this function requires ", paste0(pkg,collapse = ", "),  " package(s) for spatial clustering", "\n would you like to install it now?\n1: yes\n2: no")
      user <- readline(prompt = paste0("Selection: "))
      if(tolower(user) %in% c("1", "yes", "y")){
        utils::install.packages(pkgna,lib = NULL)
      } else{
        stop("please install required packages or set Spatial  = FALSE. ")
      }
    }
    invisible(lapply(pkg, library, character.only = TRUE))
  }

  if(is.null(coords) && spatial == TRUE ){
    if(methods::is(data, "SpatialPoints") || methods::is(data, "SpatialPointsDataFrame")){
      data <- sf::st_as_sf(data)
    } else if(!methods::is(data, "sf")){
      stop("spatial, should be a spatial or sf object")
    }
  }

  ## check if coords variables are present in the data
  # if(!is.null(coords)){
  #   if(coords %in% colnames(data) == FALSE){
  #     warning("There is no match between the column names in 'data' and 'coords' argument.\n")
  #     coords <- NULL
  #   }
  # }
  if(spatial == TRUE){
    if(is.null(coords)&& methods::is(data, "SpatialPoints")) {
      #if(methods::is(data, "SpatialPoints") || methods::is(data, "SpatialPointsDataFrame")){
      data <- sf::st_as_sf(data)
      cdat<- data.frame(sf::st_coordinates(data))
      data<- cbind(data, cdat)
      sf::st_geometry(data)<- NULL
      coords<- colnames(cdat)
      #   stop("one of the coords or spatial should should not be NULL or False")
    }else
      if(is.null(coords)){
        cdat<- data.frame(sf::st_coordinates(data))
        data<- cbind(data, cdat)
        sf::st_geometry(data)<- NULL
        coords<- colnames(cdat)
        #coords <- tidyselect::eval_select(rlang::enquo(coords), data = data)
      }
    # if(coords %in% colnames(data) == FALSE){
    #   stop("There is no match between the column names in the 'data' object and the specified 'coords' argument.\n")
    # }
    # if(!is.numeric(data[,coords, drop = TRUE])){
    #   stop("The coords values in the specified foldsCol should be integer numbers.")
    # }
  }

  #coords <- tidyselect::eval_select(rlang::enquo(coords), data = data)

  split_objs <- spatial_clustering_splits_cv(data = data, coords = coords, v = v,
                                          clust_method = clust_method, dist_clust = dist_clust,...)

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- purrr::map(split_objs$splits, rm_out)

  ## Save some overall information

  cv_att <- list(v = v, repeats = 2)

  rsample::new_rset(splits = split_objs$splits,
           ids = split_objs[, grepl("^id", names(split_objs))],
           attrib = cv_att,
           subclass = c("spatial_clustering_cv", "rset"))
}

spatial_clustering_splits_cv <- function(data, coords,clust_method = clust_method,
                                      v = v,dist_clust = dist_clust, ...) {

  if (!is.numeric(v) || length(v) != 1)
    rlang::abort("`v` must be a single integer.")

  if(is.null(clust_method)){
    dist_clust<-  NULL
    warning("clust == NULL, therefore, default 'kmeans', will be used \n by setting dist_clust to NULL
            " )
  }

  if(is.null(dist_clust) || !clust_method == "kmeans"){
    clust_method <- "kmeans"
  }
  sclust <- list()
  if(clust_method == "kmeans" || is.null(clust_method)){
    n <- nrow(data)
    clusters <- stats::kmeans(data[coords], centers = v, ...)
    folds <- clusters$cluster
    idx <- seq_len(n)
    indices <- split_unnamed(idx, folds)
    indices <- lapply(indices, default_complement, n = n)
    split_objs <- purrr::map(indices, rsample::make_splits, data = data,
                             class = "spatial_clustering_split") # make_split from
    # rsample

    tibble::tibble(splits = split_objs,
                   id = names0(length(split_objs), "Fold"))
  }
  else if(clust_method == "hclust"){
    n<- nrow(data)
    coorddata<- data[coords]
    if(!methods::is(coorddata, "data.frame")){
      coorddata<- data.frame(coorddata)
    }
    d<- dist(coorddata, method = "euclidean" )
    hc<- stats::hclust(d, method = dist_clust)
    folds<- stats::cutree(hc, k = v)
    idx<- seq_len(n)
    indices<- split_unnamed(idx, folds)
    indices <- lapply(indices, default_complement, n = n)
    split_objs <- purrr::map(indices, rsample::make_splits, data = data,
                             class = "spatial_clustering_split") # make_split from


    tibble::tibble(splits = split_objs,
                   id = names0(length(split_objs), "Fold"))
  }else{
    stop("Only `kmeans`, and `hclust` methods are supported")
  }
}

##' @export
print.spatial_clustering_cv <- function(x, ...) {
  cat("# ", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("spatial_clustering_cv", "rset"))]
  print(x, ...)
}

# ## Keep synced with rsample
#
names0 <- function(num, prefix = "x") {
  if (num == 0L) {
    return(character())
  }
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}
#
## This will remove the assessment indices from an rsplit object
rm_out <- function(x) {
  x$out_id <- NA
  x
}

# ## Get the indices of the analysis set from the assessment set
default_complement <- function(ind, n) {
  list(analysis = setdiff(1:n, ind),
       assessment = unique(ind))
}

## Split, but no names
split_unnamed <- function(x, f) {
  out <- split(x, f)
  unname(out)
}
#
# pretty.spatial_clustering_cv <- function(x, ...) {
#   details <- attributes(x)
#   res <- paste0(details$v, "-fold spatial cross-validation")
#   res
# }
#
# # coord<- data.frame(st_coordinates(data))
# #
# # dat<- cbind(data,coord[,1:2])
# #
# # st_geometry(data)<- NULL
#
#
# #scv<- spatial_clustering_cv(data = dat,coords = c("X" , "Y"),v = 5)
#
# # make splits from rsample
