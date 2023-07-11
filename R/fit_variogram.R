#--------------- funs 09- fit variogram ---------------------------------------#
#' Variogram Fitting on Quantitative Data
#'
#' @description This function is a wrapper around \code{\link[automap]{autofitVariogram}}.
#' The function runs a simple and ordinary krigging. For example, if the r
#' esponse variable is `R` then the formula that will be supplied in the data is `R~1`.
#'
#' Five experimental variogram models are tested automatically on selected
#' response variable. The function supports spatial data in simple features
#' (sf), SpatialPointsDataFrame(sp) or data.frame.
#' The later format requires X, and Y as variables in the data.frame.
#'
#' @param data An object of \code{\link[sp]{SpatialPointsDataFrame-class}},
#' \code{\link{sf}} or \code{\link{data.frame}}
#' @param response A response variable within quotes e.g. "response"
#' @param coords (vector) A pair of coordinate when input data is data.frame
#' and not "sf" or "sp" object. The X (Longitude) and Y (Latitude) should be
#' column variables in the input data set
#'
#' @return
#' An object of a type autofitVariogram is returned. This object contains
#' the experimental variogram,the fitted variogram model and the sums of
#' squares (sserr) between the sample variogram and the fitted variogram model.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("landcover")
#'
#' vfit<- fit_variogram(data = landcover,response = "MPC1",coords = NULL)
#' vfit
#' }
#'
#' @seealso \code{\link[automap]{autofitVariogram}}, \code{\link{fit_multiple_variogram}},
#' \code{\link{plot_variogram}}
#'
#' @details
#' Five different types of geostatistical models are tested
#' through \code{\link[automap]{autofitVariogram}}, they are:Spherical (Sph),
#' Gaussian (Gau), Exponential (Exp), Matern family (Mat), and
#' M.Stein's parameterization on Matern model.
#' For more information please refer to original \code{\link[automap]{autofitVariogram}} function from automap package.
#'
#'
#' @references
#'
#' Hiemstra, P.H., Pebesma, E.J., Twenhofel, C.J.W. and G.B.M. Heuvelink, 2008.
#' Real-time automatic interpolation of ambient gamma dose rates from the Dutch
#' Radioactivity Monitoring Network. Computers & Geosciences.[DOI:](http://dx.doi.org/10.1016/j.cageo.2008.10.011)
#'
#' Pebesma, E.J., 2004. Multivariable geostatistics in S: the gstat package. Computers & Geosciences, 30: 683-691.
#' Benedikt Gräler, Edzer Pebesma and Gerard Heuvelink, 2016. Spatio-Temporal Interpolation using gstat.  The R
#' Journal 8(1), 204-218
#'

fit_variogram <- function(data, response, coords = c("X", "Y")) {
  #----------------------------------------------------------------------------#
  pkg <- c("sf", "automap")

  pkname <- names(which(sapply(sapply(pkg, find.package, quiet = TRUE), length) == 0))
  if (length(pkname) > 0) {
    message(" this function requires ", paste0(pkg, collapse = ", "), " package(s) for spatial clustering", "\n would you like to install it now?\n1: yes\n2: no")
    user <- readline(prompt = paste0("Selection: "))
    if (tolower(user) %in% c("1", "yes", "y")) {
      utils::install.packages(pkname, lib = NULL)
    } else {
      stop("please install required packages")
    }
    invisible(lapply(pkg, library, character.only = TRUE))
  }
  #----------------------------------------------------------------------------#
  if (!is.null(coords)) {
    coords <- data[, coords, drop = FALSE]
  }
  if ((is.null(coords) && !methods::is(data, "sf")) &&
    (is.null(coords) && !methods::is(data, "spatVector"))) {
    stop("Either `coords` should be present as variables in `data`.\nOr data should be in `sf` or `spatial` object.")
  }
  if (!methods::is(data, "sf")) {
    data <- sf::st_as_sf(data)
  } else {
    data <- data
  }

  fit_var <- automap::autofitVariogram(data[[response]] ~ 1, data)

  return(fit_var)
}
