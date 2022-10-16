#' Smooth paths 
#'
#' Normalize segment length within the paths.
#' 
#' @param paths and object created using the function find.paths().
#' @param length.f value, new segment length (should be larger than max.range in find.paths()).
#' @return a list including smoothed paths.
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
#'
#' @export
smooth.paths <- function(paths, length.f) {
idx.f <- which(as.numeric(summary(paths[[3]])[,1])>3)
paths2 <- paths[[3]][idx.f]
paths <- list(path.detection=paths[[1]], path.status=paths[[2]][idx.f], paths=paths2)
paths.new <- list()
for ( j in 1:length(paths[[3]]) ) {
path <-  paths[[3]][[j]]
p <- 1
n <- 1
path.new <- c()
dist <- sqrt( (path[p,1]-path[p+n,1])^2 + (path[p,2]-path[p+n,2])^2 + (path[p,3]-path[p+n,3])^2 ) 
h.dists <- c()
v.dists <- c()
d3.dists <- c()
while ( p < (nrow(path)-1) ) {
path.new <- rbind(path.new, path[p,] )
  while ( dist < length.f & n+p+1 < nrow(path) ) {  
  n <- n+1
  dist <- sqrt( (path[p,1]-path[p+n,1])^2 + (path[p,2]-path[p+n,2])^2 + (path[p,3]-path[p+n,3])^2 ) 
  }
p <- p + n
n <- 1
dist <- sqrt( (path[p,1]-path[p+n,1])^2 + (path[p,2]-path[p+n,2])^2 + (path[p,3]-path[p+n,3])^2 )
}
if ( !is.null(path.new) ) {
if (nrow(path.new) > 1) {
for( i in 2:nrow(path.new) ){
h.dist <- sqrt( (path.new[i-1,1]-path.new[i,1])^2 + (path.new[i-1,2]-path.new[i,2])^2  )
v.dist <- path.new[i-1,3]-path.new[i,3]
d3.dist <- sqrt( (path.new[i-1,1]-path.new[i,1])^2 + (path.new[i-1,2]-path.new[i,2])^2 + (path.new[i-1,3]-path.new[i,3])^2)
h.dists <- c(h.dists, h.dist)
v.dists <- c(v.dists, v.dist)
d3.dists <- c(d3.dists, d3.dist)
}
path.new2 <- data.frame(path.new, v.dist=c(NA,v.dists), h.dist=c(NA,h.dists), angle=c(NA, atan(v.dists/h.dists)/pi*180 ), L3d=c(NA,d3.dists), num=j )
}else{ path.new2 <- data.frame(path.new, v.dist=NA, h.dist=NA, angle=NA, L3d=NA, num=j ) }
}else{ path.new2 <- data.frame(node.x=NA, node.y=NA, node.z=NA, v.dist=NA, h.dist=NA, angle=NA, L3d=NA, num=j )}
rownames(path.new2) <- c(1:nrow(path.new2))
paths.new <- append(paths.new, list(path.new2) )
}
for(i in 1:length(paths.new)) {
path.i <- paths.new[[i]]
if(nrow(path.i)>2){
  if(path.i[2,"angle"]<0 & path.i[3,"angle"]>0){
paths.new[[i]] <- paths.new[[i]][-1,]
paths.new[[i]][1,4:7] <- NA
rownames(paths.new[[i]]) <- c(1:nrow(paths.new[[i]]))
}}}
n.seg. <- c()
for(ii in 1:length(paths.new)){n.seg.<-c(n.seg., nrow(paths.new[[ii]])-1)}
idx.n.seg. <- which(n.seg.>1)
paths.new <- paths.new[idx.n.seg.]
detection <- c()
detection[which(!paths[[2]][idx.n.seg.]=="FALSE")] <- "full"
detection[which(paths[[2]][idx.n.seg.]=="FALSE")] <- "part"
angles <- c()
status <- c()
L3d <- c()
for( iii in 1:length(paths.new) ) {
angles <- c( angles, paths.new[[iii]][2:nrow(paths.new[[iii]]),"angle"] )
status <- c( status, min(paths.new[[iii]][2:nrow(paths.new[[iii]]),"angle"]) > 0)
L3d <- c( L3d, paths.new[[iii]][2:nrow(paths.new[[iii]]),"L3d"] ) 
}
status <- as.numeric(status)+2
return(list(path.detection=detection, path.status=status, paths=paths.new, segment.angles=angles, segment.length=L3d))
}