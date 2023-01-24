#' Sample the point cloud  
#'
#' Get the starting positions of paths. The output set of points includes: all points forming the crown projection convex chull (CPCH), a tree's highest point, and a set of randomly selected points. There are n points sampled per each meter of tree vertical extent. The probability of point selection is proportional to its distance from tree base.   
#' 
#' @param pts an object created using the function read.cloud().
#' @param min.z value, minimal height of points sampled.
#' @param n.points value, number of points sampled per each meter of tree vertical extent; if missing, the value is set to 100.
#' @param plot logical, if TRUE the sampled points are plotted in respect to horizontal and vertical planes. 
#' @return a data.frame with sampled points coordinates.
#'
#' @examples
#' # read point cloud
#' pts <- read.cloud(file = "C:/.../point_cloud_file_name.txt", sep = " ", plot = TRUE) 
#' # get distal points
#' distal.pts <- get.distal.pts(pts, min.z = 1.3, plot = TRUE)
#'
#' @export
get.distal.pts <- function(pts, min.z, n.points, plot) {
if(missing(n.points)){n.points<-100}
sampled.pts <- round(max(pts[,3])*n.points,0)
h <- pts[which(pts[,3]==max(pts[,3])),]
chull <- pts[chull(data.frame(pts[,1], pts[,2])),]
  if(min(chull[,3])<min.z) {
  chull <- chull[which(chull[,3]>=min.z),]
  message("CPCH points trimmed") }
num.pts <- as.numeric(rownames(pts[which(pts[,3]>min.z),]))
max.dist <- max( sqrt((pts[,1])^2+(pts[,2])^2+(pts[,3]-min.z)^2) )
prbs <- sqrt((pts[,1])^2+(pts[,2])^2+(pts[,3]-min.z)^2) / max.dist
set.seed(n.points); idx.pts <- sample(num.pts, sampled.pts, prob=prbs[num.pts]) 
r.pts <- pts[idx.pts,]
if(plot){
x11(width=14, height=7); par(mfrow=c(1,2))
plot(pts[,c(1,2)], asp=1, pch=16, cex=.2, col=rgb(0,0,0,.01), main="XY sampled points", xlab="X (m)", ylab="Y (m)")
 points(r.pts[,c(1,2)], pch=1); points(chull[,c(1,2)], pch=22, bg=6); points(h[,c(1,2)], pch=24, bg=7)
plot(pts[,c(1,3)], asp=1, pch=16, cex=.2, col=rgb(0,0,0,.01), main="XZ sampled points", xlab="X (m)", ylab="Z (m)")
 points(r.pts[,c(1,3)], pch=1); points(chull[,c(1,3)], pch=22, bg=6); points(h[,c(1,3)], pch=24, bg=7)
abline(h=0, lty=2); abline(h=min.z, lty=3)
legend("bottomleft", legend=c("height", "CPCH", "random"), pch=c(24,22,21), pt.bg=c(7,6,0), bg="white")
}
distal.pts <- rbind(h, chull, r.pts)
rownames(distal.pts) <- c(1:nrow(distal.pts))
return(distal.pts)
}