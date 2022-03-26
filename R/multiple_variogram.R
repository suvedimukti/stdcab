#' Multiple Variogram Fitting
#'
#' @description This function returns experimental and fitted variogram information for multiple variables.
#' For each variable \code{\link[automap]{autofitVariogram}} is used. The function supports
#'    spatial data in simple features (sf), SpatialPointsDataFrame(sp) or data.frame.
#'    If the input data is in `data.frame`, the X, and Y should be present in the data as variables.
#'
#' @param data input data set one of sp, sf or data.frame with X and Y as variables
#' @param coords (vector) pair of coordinates if data type is aspatial
#' @param length (integer) the length of sequence used to produced fitted data.
#'
#'
#' @return
#' A data frame with combined information from all variables as response is returned.
#' The `data.frame` contains the experimental variogram information, the fitted variogram model information.
#' The sums of squares (sserr) between the sample variogram dropped,
#' For further information please refer to \code{\link[automap]{autofitVariogram}} or \code{\link{fit_variogram}}
#'
#' @references
#' \href{https://CRAN.R-project.org/package=automap}{automatp}: A index page for automap package on [CRAN](https://cran.r-project.org)
#'
#'  Hiemstra, P.H., Pebesma, E.J., Twenhofel, C.J.W. and G.B.M. Heuvelink, 2008. Real-time automatic interpolation of ambient gamma dose rates from
#'  the Dutch Radioactivity Monitoring Network. Computers & Geosciences.[DOI:](http://dx.doi.org/10.1016/j.cageo.2008.10.011)
#'
#' @export
#'
#' @examples
#'
#' ## Read data
#' lcdat<- landcover
#'
#' # subset data
#'
#' lcdat <- lcdat[, c("MBLU", "MNDVI", "MNIR", "MPC1", "MPC2", "MPC23", "MRED", "MSAVI")]
#'
#' # fit variograms
#'
#' fmv<- multiple_variogram(data = lcdat,coords = NULL,length = 99 )
#'
#' \dontrun{
#' data("landcover")
#' fit_multiple_variogram(data = landcover,coords = NULL, length = 99)
#' }
#'
#' @seealso  \code{\link[automap]{autofitVariogram}}, \code{\link{fit_variogram}},
#' \code{\link{plot_variogram}}
#'
#'
 multiple_variogram <- function(data, coords = c("X", "Y"), length = 99)
# write for detecting coordinates.
{

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
