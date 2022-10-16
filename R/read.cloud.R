#' Read TLS point cloud
#'
#' This function reads TLS point cloud into R, 
#' the allowed formats include .txt, .csv, .pts (containing three columns, with the x, y and z point coordinates), 
#' row names and column names are not allowed.
#'
#' @param file point cloud file directory and name.
#' @param sep character, separator.
#' @param plot logical, if TRUE the point cloud is plotted.
#' @return Point cloud as a data.frame.
#'
#' @export
read.cloud <- function(file, sep, plot) {
pts <- read.table(file, sep=sep)[,c(1:3)]
zb <- min(pts[,3])
pts[,3] <- pts[,3] - zb
pts.base <- pts[which(pts[,3]<.3 & pts[,3]>.2),1:2] 
idx.hull <- chull( pts.base )
xb <- mean( c(min(range(pts.base[idx.hull,1])), max(range(pts.base[idx.hull,1]))) )
yb <- mean( c(min(range(pts.base[idx.hull,2])), max(range(pts.base[idx.hull,2]))) )
pts[,1] <- pts[,1] - xb
pts[,2] <- pts[,2] - yb
x <- pts[,1];y <- pts[,2];z <- pts[,3]
idx.base <- which(z>min(z)+0.29 & z<min(z)+0.31)
x.base <- mean(x[idx.base])
y.base <- mean(y[idx.base])
if(plot){
x11(width=14, height=7); par(mfrow=c(1,2))
plot(pts[,c(1,2)], asp=1, pch=16, cex=.2, col=rgb(0,0,0,.05), main="XY point cloud", xlab="X (m)", ylab="Y (m)")
plot(pts[,c(1,3)], asp=1, pch=16, cex=.2, col=rgb(0,0,0,.05), main="XZ point cloud", xlab="X (m)", ylab="Z (m)")
abline(h=0, lty=2)
}
return(pts)
}