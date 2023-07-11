#------------------ fun 07- repeated_spatial_cluster_sample--------------------#
#' Repeated Spatial Clustering of Point Data for Tidy Modeling
#'
#'
#' @description Repeated spatial cluster sampling splits the data into V groups
#' using partitioning (`kmeans`)/ hierarchical(`hclust`)
#' clustering of some variables, typically spatial coordinates.
#'
#' A resample of the analysis data works as in \code{\link{spatial_cluster_sample}} but with repeats.
#' The number or resamples is equal to fold * repeats, resample sizes are
#' not equal across folds and repeats.
#'
#' @details
#' The variables in the `coords` argument, if input data is data.frame or
#' extracted from sp, or sf data are used for  clustering of the data into
#' disjointed sets. These clusters are used as the folds for cross-validation.
#' Depending on how the data are distributed spatially.
#' The function is similar to repeated cross validation or v-fold cross
#' validation \code{\link[rsample]{vfold_cv}} but for spatial data with clustering.
#'
#' @param data data input data set one of sp, sf or data.frame with X and Y as variables
#' @param v  number of partitions of the data set or number of clusters
#' @param repeats number of repetitions of partition of data set
#' @param coords (vector) pair of coordinates if data type is aspatial or data.frame
#' @param strata (character) strata variable; default is NULL, as it
#' does not yield good results with stratification based on class/strata
#' @param breaks (integer) A single number giving the number of bins
#' desired to stratify a numeric stratification variable
#' @param pool   (numeric) A proportion of data used to determine if a
#' particular group is too small and should be pooled into another group.
#' Default is 0.1 \code{\link[rsample]{vfold_cv}}
#' @param spatial (logical) if data set is spatial (when sf or sp)
#' or aspatial (data.frame)
#' @param clust_method one of partitioning (default = kmeans) or
#' one of hierarchical methods(\code{\link[stats]{hclust}})
#' @param dist_clust the agglomeration method to be used.
#' This should be one of “ward.D”, “ward.D2”, “single”, “complete”,
#' “average” (= UPGMA), “mcquitty” (= WPGMA), “median” (= WPGMC) or “centroid” (= UPGMC).
#' The dist_clust in the function is method in stats::hclust
#' @param ... currently not used
#'
#' @return
#' A tibble with classes `spatial_cv`, `rset`, `tbl_df`, `tbl`, and `data.frame.` The
#' results include a column for the data split objects and one or more
#' identification variables.
#' For a single repeat, there will be one column called id that has a character
#' string with the fold identifier. For repeats, `id` is the repeat number and an
#' additional column called id2 that contains the fold information (within repeat).
#'
#' @importFrom stats kmeans
#'
#' @references
#'
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
#' \dontrun{
#' data("landcover")
#'
#' rscv<- repeated_spatial_cluster_sample(data = landcover,coords = NULL, v = 10,
#'       repeats = 5, spatial = TRUE, clust_method = "kmeans",
#'       dist_clust = NULL, breaks = 4, pool = 0.1)
#'
#' rscv
#' }
#'
repeated_spatial_cluster_sample <- function(data = data, v = 10, repeats = 1,
                                            coords = c("X", "Y"),
                                            strata = NULL, breaks = 4,
                                            pool = 0.1, spatial = FALSE,
                                            clust_method = "kmeans",
                                            dist_clust = NULL, ...) {
  #--------- if spatial == TRUE check if the required packages are available
  if (spatial == TRUE) {
    # check for availability of packages
    pkg <- c("sf", "sp", "purrr", "rsample")
    pkgna <- names(which(sapply(
      sapply(pkg, find.package, quiet = TRUE),
      length
    ) == 0))
    if (length(pkgna) > 0) {
      message(
        " this function requires ", paste0(pkg, collapse = ", "),
        " package(s) for spatial clustering", "\n would you like to install it now?\n1: yes\n2: no"
      )
      user <- readline(prompt = paste0("Selection: "))
      if (tolower(user) %in% c("1", "yes", "y")) {
        utils::install.packages(pkgna, lib = NULL)
      } else {
        stop("please install required packages or set Spatial  = FALSE. ")
      }
    }
    invisible(lapply(pkg, library, character.only = TRUE))
  }
  #--------------- is coordinates are not applied and spatial data is provided
  # if sp objects convert to sf
  if (is.null(coords) && spatial == TRUE) {
    if (methods::is(data, "SpatialPoints") ||
        methods::is(data, "SpatialPointsDataFrame")) {
      data <- sf::st_as_sf(data)
      cdat <- data.frame(sf::st_coordinates(data))
      data <- cbind(data, cdat)
      sf::st_geometry(data) <- NULL
      coords <- colnames(cdat)
    } else if (methods::is(data, "sf")) {
      cdat <- data.frame(sf::st_coordinates(data))
      data <- cbind(data, cdat)
      sf::st_geometry(data) <- NULL
      coords <- colnames(cdat)
    } else {
      stop("data should be a `spatial` (sp) or `sf` object when coordinates are not provided")
    }
  }

  #----------------------------------------------------------------------------#
  #
  coords <- tidyselect::eval_select(rlang::enquo(coords), data = data)

  # if(is_empty(coords)) {
  #   rlang::abort("`coords` are required and must be variables in `data`.")
  # }
  #
  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!rlang::enquo(strata))
    if (length(strata) == 0) strata <- NULL
  }
  #--------- strata check
  strata_check <- function(strata, data) {
    if (!is.null(strata)) {
      if (!is.character(strata) | length(strata) != 1) {
        rlang::abort("`strata` should be a single character value.")
      }
      if (inherits(data[, strata], "Surv")) {
        rlang::abort("`strata` cannot be a `Surv` object. Use the time or event variable directly.")
      }
      if (!(strata %in% names(data))) {
        rlang::abort(strata, " is not in `data`.")
      }
    }
    invisible(NULL)
  }

  strata_check(strata, data)

  spatial_clustering_splits_rcv <- function(data, v = 10,
                                            strata = NULL, clust_method = "kmeans",
                                            dist_clust = NULL, ...) {
    if (!is.numeric(v) || length(v) != 1) {
      stop("`v` must be a single integer.", call. = FALSE)
    }

    #------- when clust_method is null
    if (is.null(clust_method)) {
      dist_clust <- NULL
      warning("clust == NULL, therefore, default 'kmeans', will be used \n by setting dist_clust to NULL
            ")
    }
    #--------- if clust method is kmeans and dist is not provided
    if (is.null(dist_clust) || !clust_method == "kmeans") {
      clust_method <- "kmeans"
    }

    if (is.null(strata)) {
      if (clust_method == "kmeans" || is.null(clust_method)) {
        n <- nrow(data)
        clusters <- kmeans(data[, coords], centers = v)
        folds <- clusters$cluster
        idx <- seq_len(n)
        indices <- split_unnamed(idx, folds)
      } else if (clust_method == "hclust" && !is.null(dist_clust)) {
        coorddata <- data[, coords]
        if (!methods::is(coorddata, "data.frame")) {
          coorddata <- data.frame(coorddata)
        }
        n <- nrow(data)
        d <- dist(coorddata, method = "euclidean")
        hc <- stats::hclust(d, method = dist_clust)
        folds <- stats::cutree(hc, k = v)
        idx <- seq_len(n)
        indices <- split_unnamed(idx, folds)
      }
    } else {
      n <- nrow(data)
      stratas <- tibble::tibble(
        idx = 1:nrow(data),
        strata = rsample::make_strata(getElement(data, strata),
                                      breaks = breaks,
                                      pool = pool
        )
      )
      stratas <- split_unnamed(stratas, stratas$strata)
      stratas <- purrr::map(stratas, add_vfolds, v = v)
      stratas <- dplyr::bind_rows(stratas)
      indices <- split_unnamed(stratas$idx, stratas$folds)
    }

    indices <- lapply(indices, default_complement, n = n)

    split_objs <- purrr::map(indices, rsample::make_splits,
                             data = data,
                             class = "spatial_clustering_split"
    )
    tibble::tibble(
      splits = split_objs,
      id = names0(length(split_objs), "Fold")
    )
  }

  #------ for repeated analysis -----------------------------------------------#

  if (repeats == 1) {
    split_objs <- spatial_clustering_splits_rcv(data = data, v = v, strata = strata)
  } else {
    for (i in 1:repeats) {
      tmp <- spatial_clustering_splits_rcv(data = data, v = v, strata = strata)
      tmp$id2 <- tmp$id
      tmp$id <- names0(repeats, "Repeat")[i]
      split_objs <- if (i == 1) {
        tmp
      } else {
        rbind(split_objs, tmp)
      }
    }
  }
  split_objs$splits <- purrr::map(split_objs$splits, rm_out)

  ## Save some overall information

  cv_att <- list(v = v, repeats = repeats, strata = !is.null(strata))

  rsample::new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("spatial_clustering_split", "rset")
  )
}
#------------------------------------------------------------------------------
split_unnamed <- function(x, f) {
  out <- split(x, f)
  unname(out)
}
#------------ default
# Get the indices of the analysis set from the assessment set
default_complement <- function(ind, n) {
  list(
    analysis = setdiff(1:n, ind),
    assessment = ind
  )
}
#---------- rm_out
## This will remove the assessment indices from an rsplit object
rm_out <- function(x) {
  x$out_id <- NA
  x
}

add_vfolds <- function(x, v) {
  x$folds <- sample(rep(1:v, length.out = nrow(x)))
  x
}
