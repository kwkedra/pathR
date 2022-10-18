#' Plot classified paths  
#'
#' The paths (classified as plagiotropic or orthotropic) are projected onto vertical and horizontal planes.
#' 
#' @param pts and object created using the function read.cloud().
#' @param distal.pts an object created using the function get.distal.pts(). 
#' @param paths.smooth and object created using the function smooth.paths().
#' @return no data are returned.
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
#' # plot
#' project.paths(pts, distal.pts, paths.smooth)
#'
#' @export
project.paths <- function(pts, distal.pts, paths.smooth) {
cex1 = 1; cex2 = .8
x11(width=14, height=7); par(mfrow=c(1,2))
plot(pts[,1], pts[,2], asp=1, main="XY paths", xlab="X (m)", ylab="Y (m)", pch=16, cex=.2, col=rgb(0,0,0,.02))
abline(h=0, lty=2); abline(v=0, lty=2)
points( distal.pts[,1:2], col=1, pch=1, cex=cex1 )
for ( j in 1:length(paths.smooth[[3]]) ) {
 points(paths.smooth[[3]][[j]][1,1], paths.smooth[[3]][[j]][1,2], col=paths.smooth[[2]][j], pch=16, cex=cex2)
 points( paths.smooth[[3]][[j]][,c(1,2)], col=paths.smooth[[2]][j] , ty="l") }
plot(pts[,1], pts[,3], asp=1, main="XZ paths", xlab="X (m)", ylab="Z (m)", pch=16, cex=.2, col=rgb(0,0,0,.02))
abline(h=0, lty=2)
points( distal.pts[,c(1,3)], col=1, pch=1, cex=cex1 )
for ( j in 1:length(paths.smooth[[3]]) ) {
 points(paths.smooth[[3]][[j]][1,1], paths.smooth[[3]][[j]][1,3], col=paths.smooth[[2]][j], pch=16, cex=cex2)
 points( paths.smooth[[3]][[j]][,c(1,3)], col=paths.smooth[[2]][j] , ty="l") }
if(length(dimnames(table(paths.smooth[[2]]))[[1]])>1){
legend("bottomleft", legend=c("ortho.", "plagio.", "undet."), pch=21, pt.bg=c(3,2,0), bg="white")
}else{
if(dimnames(table(paths.smooth[[2]]))[[1]]=="3"){
legend("bottomleft", legend=c("ortho.", "undet."), pch=21, pt.bg=c(3,0), bg="white")}
if(dimnames(table(paths.smooth[[2]]))[[1]]=="2"){
legend("bottomleft", legend=c("plagio.", "undet."), pch=21, pt.bg=c(2,0), bg="white")}}
}