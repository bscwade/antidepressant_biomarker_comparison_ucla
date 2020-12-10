plotComps <- function(x, refvec){
  
  library(png)
  library(grid)
  library(gridExtra)
  
  tmp <- refvec[which(refvec$XName==paste0('X',x)), ]
  comp1 <- as.numeric(gsub('V', '', strsplit(as.character(tmp$Comps), '-')[[1]][1])) - 1 # zero indexing
  comp2 <- as.numeric(gsub('V', '', strsplit(as.character(tmp$Comps), '-')[[1]][2])) - 1 # zero indexing
  
  img1 <-  rasterGrob(as.raster(readPNG(sprintf('~/GoogleDrive/BMAP/Studies/Depression_Studies/ACNP_2019/Results/Melodic_IC_2_5.sum/%04d.png', comp1))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(sprintf('~/GoogleDrive/BMAP/Studies/Depression_Studies/ACNP_2019/Results/Melodic_IC_2_5.sum/%04d.png', comp2))), interpolate = FALSE)

  grid.arrange(img1, img2, ncol = 2)

}