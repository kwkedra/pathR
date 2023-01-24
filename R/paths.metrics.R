#' Tree-level metrics of branching orientation and path detection rate
#'
#' Outputs several tree-level metrics: 
#' detect.full (full paths detection rate), 
#' detect.all (full and partial paths detection rate), 
#' plagio.full (percent of full plagiotropic paths), 
#' plagio.all (percent of full and partial plagiotropic paths).
#' 
#' @param distal.pts an object created using the function get.distal.pts(). 
#' @param paths.smooth an object created using the function smooth.paths().
#' @param plot logical, if TRUE the histogram of segment angles is plotted.
#' @return metrics as a data.frame.
#'
#' @examples
#' # read point cloud
#' pts <- read.cloud(file = "C:/.../point_cloud_file_name.txt", sep = " ", plot = TRUE) 
#' # get distal points
#' distal.pts <- get.distal.pts(pts, min.z = 1.3, plot = TRUE)
#' # get paths
#' paths <- find.paths(pts, distal.pts, proximal.z = 0.0, min.range  = .02, max.range  = .08)
#' # normalize segment length
#' paths.smooth <- smooth.paths(paths, length.f = .10)
#' # get tree-level metrics
#' paths.metrics(distal.pts, paths.smooth, plot = TRUE)
#'
#' @export
paths.metrics <- function(distal.pts, paths.smooth, plot=TRUE) {
status.full <- which(paths.smooth$path.detection=="full")
digits <- 2
  detect.full <- round( length(status.full) / nrow(distal.pts) *100 ,digits)
  detect.all <- round( length(paths.smooth$path.detection) / nrow(distal.pts) *100 ,digits)
  plagio.full <- round( length(which(paths.smooth$path.status[status.full]==2)) / length(status.full) *100 ,digits)
  plagio.all  <- round( length(which(paths.smooth$path.status==2)) / length(paths.smooth$path.status) *100 ,digits)
dat. <- data.frame(
Acronym=c("detect.full","detect.all","plagio.full","plagio.all"),
Value=c(detect.full,detect.all,plagio.full,plagio.all), 
Unit=rep("%",4),
Description=c(
"full paths detection rate",
"full and partial paths detection rate",
"percent of full plagiotropic paths",
"percent of full and partial plagiotropic paths"
))
  if (plot==TRUE) {
  x11()
  hist(paths.smooth$segment.angles, main="Histogram of segment angles", xlab="Angle (degrees)", xlim=c(-100,100),  seq(-100,100,5) )
  }
return(dat.)
}