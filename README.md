# pathR: Tree plagiotropy-orthotropy quantification using TLS point clouds. R package version 1.0.1.

This package uses just base R, thus no other packages (dependencies) will be installed.

### Installation and basic information: 

``` r
install.packages("remotes")
remotes::install_github("kwkedra/pathR")
library("pathR")
?get.distal.pts
?paths.metrics
?project.paths
```


### Multiple-trees assessment:

``` r
# set folder with TLS point clouds:  
setwd("...")
# get the list of files:
files <- list.files()
# run a for loop to assess all trees automatically:
pathR_metrics <- c()
for(i in 1:length(files)) {
pts <- read.cloud(file = files[i], sep = " ", plot = TRUE) 
distal.pts <- get.distal.pts(pts, min.z = 1.3, plot = TRUE)
paths <- find.paths(pts, distal.pts, proximal.z = 0.0, min.range  = .02, max.range  = .08)
paths.smooth <- smooth.paths(paths, length.f = .10)
path.metrics <- paths.metrics(distal.pts, paths.smooth, plot = TRUE)
dat. <- t(path.metrics[,2]); colnames(dat.) <- path.metrics[,1]
pathR_metrics <- rbind(pathR_metrics, dat.)
project.paths(pts, distal.pts, paths.smooth)
}
pathR_metrics
```
