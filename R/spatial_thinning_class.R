#' Spatial Thinning of Multi-class Point Data
#'
#' @description The function is a wrapper around thin from spThin package.
#'  It makes spatial thinning easy for multi-class input data set. This function is handy and flexible:
#'  can support xy coordinates, spatialPoints, or simple features. It assumes that data are in projected
#'  planar coordinates system; preferably Universal Transverse Mercator (UTM).
#'  There is no restrictions on number of columns in the data set as long as data is in the required format.
#'
#' @param data input data set one of sp, sf or data.frame with X and Y as variables
#' @param spatial (logical) if variable is spatial or aspatial
#' @param coords (vector) pair of coordinates if data type is aspatial
#' @param distance minimum distance between two points of same class
#' @param reps  (integer) replication
#' @param class multi-class variable or strata variable. For each class
#' thinning will be applied based on specified distance
#'
#' @importFrom sf st_geometry
#' @importFrom sf st_coordinates
#' @importFrom stats dist
#'
#' @return data.frame
#'
#' @export
#'
#' @details
#' Spatial thinning expect the data in the projected coordinate system, the distance
#' value is the threshold which limits the number of observations. All the observations, in a particular class, smaller than the
#' distance thresholds are deleted. If there are more than two classes, minimum distance between observations from each class.
#'
#' @references
#' Aiello-Lammens, M. E., Boria, R. A., Radosavljevic, A. , Vilela, B. and Anderson, R. P. (2015). spThin: an
#' Rpackage for spatial thinning of species occurrence records for use in ecological niche models.
#' Ecography, 38:541-545. URL https://onlinelibrary.wiley.com/doi/10.1111/ecog.01132.
#'
#' @examples
#' ## load data from the package
#' tdat<- landcover
#'
#' # Run spatial thinning (sf format)
#' stc<- spatial_thinning_class(data = tdat,spatial = TRUE,
#' coords = NULL,distance = 10000,reps = 1,class = "Class_name")
#'
#' ### compare the number of samples from original data to the thinned data
#' ## Original data
#' library(dplyr)
#' tdat %>% group_by(Class_name) %>% summarize(n = n())
#'
#' ## Thinned data
#' stc %>% group_by(Class_name) %>% summarize(n = n())
#'
#' ##  spatial thinning using meuse data
#' # load library and data
#' library(sp)
#' data(meuse)
#'
#' # apply thinning
#'
#' mthin<- spatial_thinning_class(data = meuse, spatial = FALSE,
#'          coords = c("x", "y"),
#'          distance = 1000,
#'          reps = 2, class = "ffreq"
#'          )
#'
#' \dontrun{
#' data<- landcover
#' spatial_thinning_class(data = landcover, spatial = TRUE, coords = NULL,
#' distance = 10000, reps = 2, class = "Class_name")
#' }
#'
#'
#'
spatial_thinning_class <- function(data,
                                   spatial = FALSE,
                                   coords = c("X", "Y"),
                                   distance,
                                   reps = 1,
                                   class = "class") {
   if (!is.null(coords)) {
    coords <- tidyselect::eval_select(rlang::enquo(coords), data = data)
    # spatial<- NULL
  }
  if (is.null(coords) && is.null(spatial)) {
    rlang::abort("either `coords` should be present as variables in `data`.\n Or data should be in `sf` or `spatial` object")
  }

  #---------------------------------------------------------------------------
  if (!spatial == FALSE) {
    if (methods::is(data, "SpatialPoints")) {
      data <- sf::st_as_sf(data)
      xy <- data.frame(sf::st_coordinates(data))
      # data<- data
      sf::st_geometry(data) <- NULL
      data <- cbind(data, xy)
      coords <- names(xy)
    } else if (methods::is(data, "sf")) {
      data <- data
      xy <- data.frame(sf::st_coordinates(data))
      # data<- data
      sf::st_geometry(data) <- NULL
      data <- cbind(data, xy)
      coords <- names(xy)
    } else if (!is.null(coords)) {
      data <- data
    } else {
      stop("Input data should be a `sf` or `SpatialPoints` object \n when coords are not provided")
    }
  }

  #----------------------------- when xy = true
  #---------------------------------------------------------------------------
  # coords<- names(xy)

  # #------------- spatial to sf
  # if(methods::is(data, "SpatialPoints")){
  #   df <- sf::st_as_sf(data)
  # } else if(!methods::is(data, "sf")){
  #   stop("Input data should be a `sf` or `SpatialPoints` object \n when coords are not provided")
  # }
  #
  # if(!methods::is(data, "SpatialPoints") || !methods::is(data, "sf")){
  #   if(!is.null(coords)){
  #   df <- data
  #   }
  # }
  #----------------
  # spec.col <- class
  # cname<- invisible(dput(names(data)))
  cname <- colnames(data)
  species <- unique(data[[which(names(data) == class)]])
  # species<- unique(data[,class])
  splength <- length(species)

  # split the data frame to list and applied random thinning to each class

  splt <- split(data, f = data[, class])
  # splt <- split(x, f = x$class)

  List <- list()

  for (seq.class in seq(splt)) {
    # print computation information on console
    print(paste0("running spatial random thinning of class: ", seq.class))

    reduced.rec.dfs <- vector("list", reps)
    ## Calculate square distance matrix AND identify which elements
    ## are less than the thin parameter
    ## ***
    ## Distances calculated using 'dist' function which differs from spThin
    ## package that utilises `fields::rdist.earth` function.

    rec.df.orig <- splt[[seq.class]]

    dist.mat.save <- as.matrix(stats::dist(rec.df.orig[, coords], method = "euclidean")) < distance

    # dist.mat.save <- as.matrix(fields::rdist.earth(x1 = rec.df.orig[, coords],
    #                                               miles = FALSE)) < distance

    ## Set the diagonal of the dist matrix to FALSE values
    diag(dist.mat.save) <- FALSE

    ## Set any NA values in the dist matrix to FALSE
    dist.mat.save[is.na(dist.mat.save)] <- FALSE

    ## Calculate the row sums of the dist.mat.save object
    ## This returns the number of elements that are less than
    ## the distance for each row
    sum.vec.save <- rowSums(dist.mat.save)

    ## Make a vector of TRUE values of length equal to the number
    ## of rows in the dist.mat
    df.keep.save <- rep(TRUE, length(sum.vec.save))

    for (rep in seq_len(reps)) {
      dist.mat <- dist.mat.save
      sum.vec <- sum.vec.save
      df.keep <- df.keep.save

      ## Perform while loop based on two criteria
      ## 1. The minimum distance between two occurrences is less than the
      ##    thinning parameter
      ## 2. The number of rows in the resulting data set is greater than 1
      while (any(dist.mat) && sum(df.keep) > 1) {
        remove.rec <- which(sum.vec == max(sum.vec))
        if (length(remove.rec) > 1) {
          remove.rec <- sample(remove.rec, 1)
        } # close remove.rec if clause
        sum.vec <- sum.vec - dist.mat[, remove.rec]

        ## Set the sum.vec value for the row to be removed equal to 0
        sum.vec[remove.rec] <- 0L

        ## Set the occ to be ignored in the next iteration of the while loop
        dist.mat[remove.rec, ] <- FALSE
        dist.mat[, remove.rec] <- FALSE

        ## Note the occurrence for removal from the thinned data set
        df.keep[remove.rec] <- FALSE
      } # close while loop
      ## Make the new, thinned, data set
      rec.df <- rec.df.orig[df.keep, , drop = FALSE]
      dname <- names(rec.df)
      # dname<- dname[!dname %in% c("X", "Y", "Class")]
      # colnames(rec.df) <- append("Longitude", "Latitude", "Class", dname)
      reduced.rec.dfs[[rep]] <- rec.df
    } # close for seq_lenclose

    reduced.rec.order <- unlist(lapply(reduced.rec.dfs, nrow))
    reduced.rec.order <- order(reduced.rec.order, decreasing = TRUE)
    reduced.rec.dfs <- reduced.rec.dfs[reduced.rec.order]
    List[[seq.class]] <- reduced.rec.dfs[[reps]]
  } # for sp close
  # convert List to data frame using do.call function
  thin.df <- do.call(rbind.data.frame, List)
  # dname<- names(df)
  # cname<- cname[!cname %in% c("X", "Y", "Class")]
  # colnames(df) <- append("Longitude", "Latitude", "Class",cname)
  return(thin.df)
} # function close
