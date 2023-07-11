# Fish-in-Droplets
# Description
## R fold：(1)preprocessed.R: Reading raw images and calculate the pixel difference between images. T-score BAMs were obtained after the preprocessed images.(2)clustering.R: Applying PCA on the T-score BAMs to obtain 20 PCs. Self-diffusion was used to enhance the compound network connection. The clustering labs were obtained by hierarchical clustering.(3) plot.R: plot the brain activity maps and clustering dendrogram.(4) self-diffusion.R: Implement sele-diffusion process.
## matlab fold: heartbeat.m: Analyzed the heart rate in Matlab and plot the corresponding heart rate variation curve to different compounds

# Run environment: R version 4.1.3 (2022-03-10)
# Dependency: tiff package;EBImage package;ConsensuClusterPlus package

# Instructions
## load(/demo.RData)
## Tmap<- tmatcal(demo)
## pca<-pca(Tmap)
## ft<-pca$x[, 1:20]
## sd<-self.diffusion(affs(ft))
## cor.mat <- cor(t(sfd))
## fet<-as.dist(1-cor.mat)
## lus<-ConsensuClusterPlus(sd)
