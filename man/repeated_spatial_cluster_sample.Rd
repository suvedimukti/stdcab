% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repeated_spatial_cluster_sample.R
\name{repeated_spatial_cluster_sample}
\alias{repeated_spatial_cluster_sample}
\title{Repeated Spatial Cluster Cross-Validation for Tidy Modeling}
\usage{
repeated_spatial_cluster_sample(
  data = data,
  v = 10,
  repeats = 1,
  coords = c("X", "Y"),
  strata = NULL,
  breaks = 4,
  pool = 0.1,
  spatial = FALSE,
  clust_method = "kmeans",
  dist_clust = NULL,
  ...
)
}
\arguments{
\item{data}{data input data set one of sp, sf or data.frame with X and Y as variables}

\item{v}{number of partitions of the data set or number of clusters}

\item{repeats}{number of repetitions of partition of data set}

\item{coords}{(vector) pair of coordinates if data type is aspatial or data.frame}

\item{strata}{(character) strata variable; default is NULL, as it does not yield good results with stratification based on class/strata}

\item{breaks}{(integer) A single number giving the number of bins desired to stratify a numeric stratification variable}

\item{pool}{(numeric) A proportion of data used to determine if a particular group is too small and should be pooled into another group. Default is 0.1 \code{\link[rsample]{vfold_cv}}}

\item{spatial}{(logical) if data set is spatial (when sf or sp) or aspatial (data.frame)}

\item{clust_method}{one of partitioning (default = kmeans) or one of hierarchical methods(\code{\link[stats]{hclust}})}

\item{dist_clust}{the agglomeration method to be used. This should be one of “ward.D”, “ward.D2”, “single”, “complete”,
“average” (= UPGMA), “mcquitty” (= WPGMA), “median” (= WPGMC) or “centroid” (= UPGMC).
the dist_clust in the function is method in stats::hclust}

\item{...}{currently not used}
}
\value{
A tibble with classes `spatial_cv`, `rset`, `tbl_df`, `tbl`, and `data.frame.` The
results include a column for the data split objects and one or more identification variables.
For a single repeat, there will be one column called id that has a character string with the fold identifier.
For repeats, id is the repeat number and an additional column called id2 that contains the fold information (within repeat).
}
\description{
Repeated spatial cluster sampling splits the data into V groups using partitioning
(`kmeans`)/ hierarchical(`hclust`) clustering of some variables, typically
 spatial coordinates. A resample of the analysis data works as
 in \code{\link{spatial_cluster_sample}} but with repeats.
 The number or resamples is equal to fold * repeats, resample sizes are not equal across folds and repeats.
}
\details{
The variables in the `coords` argument, if input data is data.frame or extracted from sp, or sf data are used for  clustering of
 the data into disjointed sets. These clusters are used as the folds for cross-validation. Depending on how the
 data are distributed spatially. The function is similar to repeated cross validation or v-fold cross validation \code{\link[rsample]{vfold_cv}}
 but for spatial data with clustering.
}
\examples{

### spatial repeated sampling
data("landcover")

## Using partitioning clustering
set.seed(1318)

srs<- repeated_spatial_cluster_sample(data = landcover,v = 2, repeats = 2,
      coords = NULL,spatial = TRUE, clust_method = "kmeans",
      dist_clust = NULL)
srs
## Hierarchical clustering
set.seed(1318)

srs_hw2<- repeated_spatial_cluster_sample(data = landcover,v = 2, repeats = 2,
                        coords = NULL, spatial = TRUE, clust_method = "hclust",
                        dist_clust = "ward.D2")

# average method (UPGMA)
set.seed(1319)
srs_havg<- repeated_spatial_cluster_sample(data = landcover, v = 2, repeats = 2,
                           coords = NULL, spatial = TRUE, clust_method = "hclust",
                           dist_clust = "average")


\dontrun{
data("landcover")

rscv<- repeated_spatial_cluster_sample(data = landcover,coords = NULL, v = 10,
      repeats = 5, spatial = TRUE, clust_method = "kmeans",
      dist_clust = NULL, breaks = 4, pool = 0.1)

rscv
}

}
\references{
A. Brenning, "Spatial cross-validation and bootstrap for the assessment of
prediction rules in remote sensing: The R package sperrorest," 2012 IEEE
International Geoscience and Remote Sensing Symposium, Munich, 2012,
pp. 5372-5375, doi: 10.1109/IGARSS.2012.6352393.

Julia Silge (2021). spatialsample: Spatial Resampling Infrastructure. https://github.com/tidymodels/spatialsample,
https://spatialsample.tidymodels.org.

Julia Silge, Fanny Chow, Max Kuhn and Hadley Wickham (2021). rsample: General Resampling Infrastructure. R package version 0.1.1.
https://CRAN.R-project.org/package=rsample
}
