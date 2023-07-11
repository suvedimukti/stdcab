#--------------- funs 10- plot variogram --------------------------------------#
#' Plot Single Sevariance Model
#'
#' @description
#' This function uses ggplot2 framework to plot semivariogram object results from
#' \code{\link{fit_variogram}}.
#'
#' @param object An output from \code{\link{fit_variogram}} function
#' @param length (integer) the length of sequence used to produced fitted data.
#' @param show_range (logical) whether to display range in the plot
#'
#'
#' @return A list with plot and tibble with semivariogram information
#' @export
#'
#' @seealso  \code{\link[automap]{autofitVariogram}}, \code{\link{fit_variogram}},
#' \code{\link{plot_variogram}}
#'
#' @references
#'
#' Pebesma, E.J., 2004. Multivariable geostatistics in S: the gstat package. Computers & Geosciences, 30: 683-691.
#' Benedikt Gräler, Edzer Pebesma and Gerard Heuvelink, 2016. Spatio-Temporal Interpolation using gstat.  The R
#' Journal 8(1), 204-218
#'
#' Hiemstra, P.H., Pebesma, E.J., Twenhofel, C.J.W. and G.B.M. Heuvelink, 2008. Real-time automatic interpolation of ambient gamma dose rates from
#' the Dutch Radioactivity Monitoring Network. Computers & Geosciences.[DOI:](http://dx.doi.org/10.1016/j.cageo.2008.10.011)
#'
#' H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
#'
#' @examples
#'
#' ##  Run semivariance on Principal Component Axis1
#' # read data
#' data("landcover")
#'
#' vfit  <- fit_variogram(data = landcover, response = "MPC1", coords = NULL)
#' vplot <- plot_variogram(object = vfit, length = 99, show_range = TRUE)
#'
#' # plot
#'  vplot$plot
#'
#' vfit_pc2  <- fit_variogram(data = landcover, response = "MPC1", coords = NULL)
#' vplot_pc2  <- plot_variogram(object = vfit_pc2, length = 99, show_range = FALSE)
#'
#' # plot
#'  vplot_pc2$plot
#'
#' \dontrun{
#' data("landcover")
#'
#' vfit  <- fit_variogram(data = landcover,response = "ASYM",coords = NULL)
#' vplot <- plot_variogram(object = vfit, length = 99, show_range = TRUE)
#'
#' # plot variogram
#'  vplot$plot
#' # data
#' vplot$data
#'
#' }
#'
#'
plot_variogram <- function(object, length = 99, show_range = TRUE) {

  # function start ----------------------#

  if (is.null(length)) {
    length <- 99
  } else if (is.character(length)) {
    length <- as.numeric(length)
  }

  fitted <- data.frame(dist = seq(0.01, max(object$exp_var$dist), length = length))

  fitted[, "line"] <- gstat::variogramLine(object$var_model, dist_vector = fitted$dist)$gamma
  fitted[, "model"] <- as.character(object[["var_model"]][["model"]][[2]])
  fitted[, "var"] <- as.character("var")


  #-----------empirical data
  emperical <- as.data.frame(object$exp_var)
  emperical[, "range"] <- object$var_model[2, 3]
  emperical[, "sill"] <- object$var_model[2, 2]
  emperical[, "nugg"] <- object$var_model[1, 2]
  emperical[, "model"] <- as.character(object[["var_model"]][["model"]][[2]])


  #-------------- make df and combine

  fit <- list()
  mod <- list()

  fit[[1]] <- fitted

  mod[[1]] <- emperical

  f1 <- function(...) merge(..., all = TRUE, by = "dist")

  bind <- Map(f = f1, fit, mod)

  ltodf <- as.data.frame(bind)
  ltodf[, "id"] <- as.character(ltodf[, "id"])

  #------------------------------------------------------------------------------

  #------------------------------------------------------------------------------#

  obj1 <- ltodf %>% dplyr::mutate(model = dplyr::coalesce(model.x, model.y)) %>%
    # var   = dplyr::coalesce(var.x, var.y)) %>%
    dplyr::select(model, var, dist, line, np, gamma, sill, nugg, range, dir.hor, dir.ver, id)

  obj <- tidyr::pivot_longer(
    data = obj1, cols = c("line", "gamma"), names_to = "type",
    values_to = "value", values_drop_na = TRUE
  )

  # delete vars
  del_var <- c("np", "dir.hor", "dir.ver", "id")

  obj[!colnames(obj) %in% del_var]

  pdata <- obj %>% dplyr::filter(type == "gamma")

  ldata <- obj %>% dplyr::filter(type == "line")


  semivar <- ggplot2::ggplot(data = pdata, ggplot2::aes(x = dist, y = value)) +
    ggplot2::geom_point(
      show.legend = FALSE, size = 4,
      position = ggplot2::position_jitter(
        width = 0.01,
        height = 0.01, seed = 1
      ), na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = ldata, colour = "blue", size = 1.5,
      alpha = 0.8, show.legend = FALSE
    ) +
    # ggplot2::geom_text(data = pdata, stat = "identity",
    #                    label = round(pdata$np, 3),
    #                    position = ggplot2:: position_jitter(width = 0.01,
    #                                         height = 0.01, seed = 1),
    #                     color = "purple", na.rm = TRUE)+

    # geom_vline(xintercept = mean(pdata$range), linetype = "dashed")+
    # facet_wrap(~var,nrow = 8,scales = "free")
    ggplot2::scale_y_continuous(limits = c(
      min(pdata$value, ldata$value),
      max(pdata$value, ldata$value)
    )) +
    ggplot2::scale_x_continuous(limits = c(0, max(pdata$dist),
                                           breaks = seq(from = 0, to = max(pdata$dist), length.out = 5)
    )) +
    ggplot2::xlab("Distance (m)") +
    ggplot2::ylab("Semivariance(\u03B3)") +
    ggplot2::theme_bw(12)

  if (show_range == TRUE) {
    semivar <- semivar + ggplot2::geom_vline(xintercept = mean(pdata$range), linetype = "dashed")
  } else {
    semivar
  }


  Retlist <- list(
    plot = semivar,
    data = obj
  )
  return(Retlist)
} #----- full function close
