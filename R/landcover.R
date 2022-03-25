#' @title Land Use Land Cover (LULC) Data For Classification Problem
#' @description The land use land cover data were generated using heads-up (onscreen)
#' digitization using National Agriculture Imagery Program (NAIP) digital
#' orthoimagery data acquired in 2018.
#' @format A data frame with 1922 rows and 43 variables:
#' \describe{
#'   \item{\code{Class_name}}{double land use land cover classes 1 through 7}
#'   \item{\code{ASYM}}{double Assymetry}
#'   \item{\code{BDRI}}{double Boundary Index}
#'   \item{\code{COMPT}}{double Compactness}
#'   \item{\code{DENS}}{double Density}
#'   \item{\code{MASM}}{double Mean Angular Second Momentum}
#'   \item{\code{MAVG}}{double Mean Average  texture}
#'   \item{\code{MAXD}}{double maximum Difference in spectral values}
#'   \item{\code{MBLU}}{double mean Blue}
#'   \item{\code{MDIS}}{double Mean Dissimilarity}
#'   \item{\code{MENT}}{double mean Entropy}
#'   \item{\code{MGRN}}{double Mean Green}
#'   \item{\code{MHOM}}{double Mean Homogeneity}
#'   \item{\code{MNDSI}}{double Mean Normalized Difference Soil Index}
#'   \item{\code{MNDVI}}{double Mean Normalized Difference Vegetation Index}
#'   \item{\code{MNDWI}}{double Mean Normalized Difference Water Index}
#'   \item{\code{MNIR}}{double Mean Neari nfrared}
#'   \item{\code{MPC1}}{double Mean Principal Component Axis 1}
#'   \item{\code{MPC2}}{double double Mean Principal Component Axis 2}
#'   \item{\code{MPC23}}{double double Mean Principal Component Axis 3}
#'   \item{\code{MRED}}{double Mean Red}
#'   \item{\code{MSAVI}}{double Mean Soil Adjusted Vegetation Index}
#'   \item{\code{MSTD}}{double mean Standarad Deviation}
#'   \item{\code{RECT}}{double Rectantular Fit}
#'   \item{\code{ROUND}}{double Roundness}
#'   \item{\code{SASM}}{double Standard deviation of Angular Second momentum}
#'   \item{\code{SAVG}}{double Standard deviation of AVG}
#'   \item{\code{SAVI}}{double Standard deviation of SAVI}
#'   \item{\code{SBLU}}{double Standard deviation of Blue}
#'   \item{\code{SDIS}}{double Standard deviation of Dissimilarity}
#'   \item{\code{SENT}}{double Standard deviation of Entropy}
#'   \item{\code{SGRN}}{double Standard deviation of Green}
#'   \item{\code{SHOM}}{double Standard deviation of Homogeneity}
#'   \item{\code{SHPI}}{double Shape Index}
#'   \item{\code{SNDSI}}{double Standard deviation of NDSI}
#'   \item{\code{SNDVI}}{double Standard deviation of NDVI}
#'   \item{\code{SNIR}}{double Standard deviation of Near infrared}
#'   \item{\code{SPC1}}{double Standrad deviation of PCA1}
#'   \item{\code{SPC2}}{double Standard deviation of PCA2}
#'   \item{\code{SPC3}}{double Standard deviation of PCA3}
#'   \item{\code{SRED}}{double Standard deviation of Red}
#'   \item{\code{SSTD}}{double Standard deviation of Standard Deviation}
#'   \item{\code{geometry}}{list Geometry Information}
#'}
#' @details Slightly doctored data set of land use land cover training data generated based on
#' on-screen digitization.
#' The data has 41 predictor variable and 1 dependent multi-class variable with
#' 1922 records.
#'
#'
"landcover"
