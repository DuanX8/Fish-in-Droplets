# Fish-in-Droplets
# Description

## Here, a droplets-based microfluidic platform that enables highly multiplexed in vivo profiling and validation of compounds in hundreds of animals is demonstrated.

# Code used in the paper is described as follows:
## R fold：(1)preprocessed.R: Reading raw images and calculate the pixel difference between images. T-score BAMs were obtained after the preprocessed images.(2)clustering.R: Applying PCA on the T-score BAMs to obtain 20 PCs. Self-diffusion was used to enhance the compound network connection. The clustering labs were obtained by hierarchical clustering.(3) plot.R: plot the brain activity maps and clustering dendrogram.(4) self-diffusion.R: Implement sele-diffusion process.
## matlab fold: heartbeat.m: Analyzed the heart rate in Matlab and plot the corresponding heart rate variation curve to different compounds.

# Run environment: R version 4.1.3 
# Dependency R packages: tiff;EBImage;ConsensuClusterPlus.

# Instructions
### load(/demo.RData)
### Tmap<- tmatcal(demo)
### pca<-pca(Tmap)
### ft<-pca$x[, 1:20]
### sd<-self.diffusion(affs(ft))
### cor.mat <- cor(t(sd))
### fet<-as.dist(1-cor.mat)
### clus<-ConsensuClusterPlus(fet)
