##------------- fun 01 spatial thinnning class -------------------------------##
#' Spatial thinning of multi claass point data
#' @description The function is a wrapper around thin from spThin package.
#' It makes spatial thinning easy for multi-class input data set. This function is handy and flexible:
#' can support xy coordinates, spatialPoints, or simple features. It assumes that data are in projected
#' planar coordinates system; preferably Universal Transverse Mercator (UTM).
#' There is no restrictions on number of columns in the data set as long as data is in the required format.
#'
#' @param data The input data. It can be either an 'sf' object, 'SpatialPoints' object, or a data frame.
#' @param spatial Logical value indicating whether the input data is spatial data (either 'sf' or 'SpatialPoints'). Default is FALSE.
#' @param coords The names of the coordinate variables to use for spatial operations. By default, it is set to c("X", "Y").
#' @param distance The distance threshold for thinning. Points within this distance will be reduced to a single point.
#' @param class multi-class variable or strata variable. For each class
#' thinning will be applied based on specified distance.
#'
#' @importFrom sf st_geometry
#' @importFrom sf st_coordinates
#' @importFrom stats dist
#'
#' @return A data frame containing the spatially thinned classes.
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
#' tdat <- landcover
#'
#' # Run spatial thinning (sf format)
#' stc <- spatial_thinning_class(data = tdat,spatial = TRUE,
#' coords = NULL,distance = 10000,class = "Class_name")
#'
#'
#' \dontrun{
#' data <- landcover
#' spatial_thinning_class(data = landcover, spatial = TRUE, coords = NULL,
#' distance = 10000, class = "Class_name")
#' }
#'
#'
#'
spatial_thinning_class <- function(data, spatial = FALSE, coords = c("X", "Y"),
                                   distance, class = "class") {
  if (!is.null(coords)) {
    coords <- tidyselect::eval_select(rlang::enquo(coords), data = data)
  }
  if (is.null(coords) && is.null(spatial)) {
    rlang::abort("Either `coords` should be present as variables in `data`, or data should be in `sf` or `spatial` object")
  }
  if (!spatial == FALSE) {
    if (methods::is(data, "SpatialPoints")) {
      data <- sf::st_as_sf(data)
      xy <- data.frame(sf::st_coordinates(data))
      sf::st_geometry(data) <- NULL
      data <- cbind(data, xy)
      coords <- names(xy)
    } else if (methods::is(data, "sf")) {
      data <- data
      xy <- data.frame(sf::st_coordinates(data))
      sf::st_geometry(data) <- NULL
      data <- cbind(data, xy)
      coords <- names(xy)
    } else if (!is.null(coords)) {
      data <- data
    } else {
      stop("Input data should be a `sf` or `SpatialPoints` object when coords are not provided")
    }
  }

  column_names <- colnames(data)
  species <- unique(data[[which(names(data) == class)]])
  species_length <- length(species)

  data_split <- split(data, f = data[, class])
  result_list <- list()

  for (class_seq in seq(data_split)) {
    print(paste0("Running spatial random thinning of class: ", class_seq))

    reduced_rec_dfs <- vector("list", 1)
    original_rec_df <- data_split[[class_seq]]

    distance_matrix_save <- as.matrix(stats::dist(original_rec_df[, coords], method = "euclidean")) < distance
    diag(distance_matrix_save) <- FALSE
    distance_matrix_save[is.na(distance_matrix_save)] <- FALSE
    sum_vector_save <- rowSums(distance_matrix_save)
    df_keep_save <- rep(TRUE, length(sum_vector_save))

    distance_matrix <- distance_matrix_save
    sum_vector <- sum_vector_save
    df_keep <- df_keep_save

    while (any(distance_matrix) && sum(df_keep) > 1) {
      remove_rec <- which(sum_vector == max(sum_vector))
      if (length(remove_rec) > 1) {
        remove_rec <- sample(remove_rec, 1)
      }
      sum_vector <- sum_vector - distance_matrix[, remove_rec]
      sum_vector[remove_rec] <- 0L
      distance_matrix[remove_rec, ] <- FALSE
      distance_matrix[, remove_rec] <- FALSE
      df_keep[remove_rec] <- FALSE
    }

    reduced_rec_df <- original_rec_df[df_keep, , drop = FALSE]
    reduced_rec_dfs[[1]] <- reduced_rec_df

    result_list[[class_seq]] <- reduced_rec_dfs[[1]]
  }

  result_df <- do.call(rbind.data.frame, result_list)
  return(result_df)
}
