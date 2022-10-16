#' Path detection
#'
#' Detect paths from distal points to a proximal point. Plagiotropic path: includes at least one segment <=0.0 degree angle; orthotropic path: all segment angles >0.0 degree; 0 degree indicates a perfectly horizontal path segment.
#' 
#' @param pts and object created using the function read.cloud().
#' @param distal.pts an object created using the function get.distal.pts(). 
#' @param proximal.z value, end point height (typically, tree base [0.0], or breast height).
#' @param min.range value, minimal search radius.
#' @param max.range value, maximal search radius.
#' @return a list including the created paths.
#'
#' @examples
#' # read point cloud
#' pts <- read.cloud(file = "C:/.../point_cloud_file_name.txt", sep = " ", plot = TRUE) 
#' # get distal points
#' distal.pts <- get.distal.pts(pts, min.z = 1.3, plot = TRUE)
#' # get paths
#' paths <- find.paths(pts, distal.pts, proximal.z = 0.0, min.range  = .02, max.range  = .08)
#'
#' @export
find.paths <- function(pts, distal.pts, proximal.z, min.range, max.range) {
paths <- list()
for ( i in 1:nrow(distal.pts) ) {
node <- distal.pts[i,]
nodes <- c()
endwhile <- 0
while ( node[,3] > (proximal.z) & endwhile==0 ) {
node.x <- node[,1]; node.y <- node[,2]; node.z <- node[,3]
nodes <- rbind(nodes, cbind(node.x,node.y,node.z))
pts.x <- pts[which(pts[,1] > node.x-max.range & pts[,1] < node.x+max.range),] 
pts.y <- pts.x[which(pts.x[,2] > node.y-max.range & pts.x[,2] < node.y+max.range),]
pts.z <- pts.y[which(pts.y[,3] > node.z-max.range & pts.y[,3] < node.z),]
dists <-  sqrt( (pts.z[,1]-node.x)^2 + (pts.z[,2]-node.y)^2 + (pts.z[,3]-node.z)^2 )
pts.z <- pts.z[which(dists < max.range),]
dists <-  sqrt( (pts.z[,1]-node.x)^2 + (pts.z[,2]-node.y)^2 + (pts.z[,3]-node.z)^2 )
	if ( nrow(pts.z)> 0 ) {
	if ( max(dists) > min.range ) {
	pts.z <- pts.z[which(dists > min.range ),]
	dists <-  sqrt( (pts.z[,1]-node.x)^2 + (pts.z[,2]-node.y)^2 + (pts.z[,3]-node.z)^2 )
	}
	node = pts.z[which(dists==min(dists))[1],1:3]
}else{ endwhile <- 1 }
}
rownames(nodes) <- c(1:nrow(nodes))
paths <- append(paths, list(nodes))
 inf. <- paste0("vertical direction: ", round(i/nrow(distal.pts)*100,0), "%")
 if(exists("n")){cat(paste0(rep("\b",n),collapse=""))}
 cat(inf.)
 n <- nchar(inf.)
 flush.console()
}
path.status <- c()
for ( j in 1:length(paths) ) {
 if ( length(paths[[j]]) > 0 ){
 path.status <- c(path.status, min(paths[[j]][,3]) < (proximal.z+0.1) )
 }else{ path.status <- c(path.status, FALSE) }
}
cat("\n"); rm(n); j<-0
paths2 <- list()
for ( i in which(path.status==FALSE) ) {
j<-j+1
node <- distal.pts[i,]
nodes <- c()
endwhile <- 0
direction <- "H"
dist.start <- sqrt( (node[,1])^2 + (node[,2])^2  )
while ( node[,3] > (proximal.z) & endwhile==0 ) {
node.x <- node[,1]; node.y <- node[,2]; node.z <- node[,3]
nodes <- rbind(nodes, cbind(node.x,node.y,node.z))
pts.x <- pts[which(pts[,1] > node.x-max.range & pts[,1] < node.x+max.range),] 
pts.y <- pts.x[which(pts.x[,2] > node.y-max.range & pts.x[,2] < node.y+max.range),]
pts.z <- pts.y[which(pts.y[,3] > node.z-max.range & pts.y[,3] < node.z+max.range),]
dists <- sqrt( (pts.z[,1]-node.x)^2 + (pts.z[,2]-node.y)^2 + (pts.z[,3]-node.z)^2 )
pts.z <- pts.z[which(dists < max.range),]
dist.test <- sqrt( (node.x)^2 + (node.y)^2  )
if( dist.test > (1/2)*dist.start & direction == "H" ) {
	dists2 <- sqrt( (pts.z[,1])^2 + (pts.z[,2])^2  )
	pts.z <- pts.z[which(dists2 < dist.test),]
	dists <-  sqrt( (pts.z[,1]-node.x)^2 + (pts.z[,2]-node.y)^2 + (pts.z[,3]-node.z)^2 )
	if ( length(dists) == 1 ) {if( dists==0 ) { direction <- "HV" }}
	} else {
	direction <- "HV"
	dist.test <- sqrt( (node.x)^2 + (node.y)^2 + (node.z-proximal.z)^2 )
	dists2 <- sqrt( (pts.z[,1])^2 + (pts.z[,2])^2 + (pts.z[,3]-proximal.z)^2 )
	pts.z <- pts.z[which(dists2 < dist.test),]
	dists <-  sqrt( (pts.z[,1]-node.x)^2 + (pts.z[,2]-node.y)^2 + (pts.z[,3]-node.z)^2 )
	}
	if ( nrow(pts.z)> 0 ) {
	if ( max(dists) > min.range ) {
	pts.z <- pts.z[which(dists > min.range ),]
	dists <-  sqrt( (pts.z[,1]-node.x)^2 + (pts.z[,2]-node.y)^2 + (pts.z[,3]-node.z)^2 )
	}
	node = pts.z[which(dists==min(dists))[1],1:3]
	}else{ endwhile <- 1 }
}
rownames(nodes) <- c(1:nrow(nodes))
paths2 <- append(paths2, list(nodes))
 inf. <- paste0("horizontal direction: ", round(j/length(which(path.status==FALSE))*100,0), "%")
 if(exists("n")){cat(paste0(rep("\b",n),collapse=""))}
 cat(inf.)
 n <- nchar(inf.)
 flush.console()
}
cat("\n")
 if ( length(paths2) > 0 ) {
path2.status <- c()
for ( j in 1:length(paths2) ) {
 if ( length(paths2[[j]]) > 0 ){
 path2.status <- c(path2.status, min(paths2[[j]][,3]) < (proximal.z+0.1) )
 }else{ path2.status <- c(path2.status, FALSE) }
}
paths[which(path.status==FALSE)] <- paths2
path2.status[which(path2.status)] <- "plagio"
path.status[which(path.status==FALSE)] <- path2.status
}
path.status[which(path.status=="TRUE")] <- "ortho"
path.status <- as.factor(path.status)
path.detection <- round(length(which(!path.status==FALSE)) / length(path.status) *100 ,2)
return(list(path.detection=path.detection, path.status=path.status, paths=paths))
}