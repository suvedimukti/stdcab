
#' Multiple Variogram Fitting
#'
#' @description This function returns experimental and fitted variogram information for multiple variables. For each variable \code{\link[automap]{autofitVariogram}} is used.
#'
#' @param data input data set one of sp, sf or data.frame with X and Y as variables
#' @param coords (vector) pair of coordinates if data type is aspatial
#' @param length (integer) the length of sequence used to produced fitted data.
#'
#' @return
#' A data frame with combined information from all variables as response is returned.
#' The dataframe contains the experimental variogram information, the fitted variogram model information.
#' The sums of squares (sserr) between the sample variogram and the fitted variogram model is dropped, please refer to \code{\link[automap]{autofitVariogram}} or \code{\link{fit_variogram}}
#' @export
#'
#' @examples
#'
#' \dontrun{
#' data("landcover")
#' multiple_variogram(data = landcover,coords = NULL, length = 99)
#' }
#'
#' @seealso  \code{\link[automap]{autofitVariogram}}, \code{\link{fit_variogram}},
#' \code{\link{plot_variogram}}
#'
#'
multiple_variogram <- function(data, coords = c("X", "Y"), length = 99)
# write for detecting coordinates.
{
  #- check if it is sp
  # class_df<- class(data)
  # require("sf")
  # require("sp")
  # require("rgdal")
  # require("gstat")
  # require("automap")
  #----------------------------------------------------------------------------#
  if (!is.null(coords)) {
    coords <- tidyselect::eval_select(rlang::enquo(coords), data = data)
  }
  if ((!is.null(coords) && !methods::is(data, "sf")) || (!is.null(coords) && !methods::is(data, "SpatialPointsDataFrame"))) {
    rlang::abort("either `coords` should be present as variables in `data`.\n Or data should be of `sf` or `spatial` object")
  }
  if (methods::is(data, "sf")) {
    data <- sf::as_Spatial(data)
  }
  if (methods::is(data, "data.frame")) {
    sp::coordinates(data) <- cbind(data[, coords[1]], data[, coords[2]])
    data@data[, c("X", "Y")] <- NULL
  } else {
    data <- data
  }
  #-------------------------------------------------------------------------------#

  my_list <- list()
  for (i in names(data)) { # number of columns
    print(paste0("running semivariance analysis for variable : ", i))

    elist <- automap::autofitVariogram(data[[i]] ~ 1, input_data = data)
    #------------ fitted data
    fitted <- data.frame(dist = seq(0.01, max(elist$exp_var$dist), length = length))

    #------------- fitted gamma
    fitted$line <- gstat::variogramLine(elist$var_model, dist_vector = fitted$dist)$gamma
    fitted$model <- as.character(elist[["var_model"]][["model"]][[2]])
    fitted$var <- as.character(i)
    #-----------empirical data
    emperical <- as.data.frame(elist$exp_var)
    emperical$range <- elist$var_model[2, 3]
    emperical$sill <- elist$var_model[2, 2]
    emperical$nugg <- elist$var_model[1, 2]
    emperical$model <- as.character(elist[["var_model"]][["model"]][[2]])
    emperical$var <- as.character(i)

    fit <- list()
    mod <- list()
    fit[[1]] <- fitted
    mod[[1]] <- emperical
    f1 <- function(...) merge(..., all = TRUE, by = "dist")
    bind <- Map(f = f1, fit, mod)
    ltodf <- as.data.frame(bind)
    ltodf$id <- as.character(ltodf$id)
    my_list[[i]] <- ltodf
  } #-close for loop

  cdata <- do.call(rbind.data.frame, my_list)

  return(cdata)
}
