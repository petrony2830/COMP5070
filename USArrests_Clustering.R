# install cluster library if you don't already have it!
library(cluster)

#################################################################
########################## US Arrests Data ######################
################################################################

# US Arrests is a part of the R base package however we will work with a special,
# edited version of the file to include more information.

# To see the R in-built version you would type:

head(USArrests)

# Calculate the distance matrix, using euclidean distance
d = dist(USArrests,method="euclidean")

# Take a look at d - you should see a matrix that has entries in the lower half (the matrix is symmetric so
# only these lower half entries are required)
d


################################################################
######################## Hierarchical Clustering ###############
################################################################


################# AGNES with different linkages ################

# For hierarchical clustering we can use the hclust() function which takes in the distance matrix
# and also requires the linkage type (single, complete, average)
#
# Remember, you can compare your output with the accompanying pdf file USArrests_AGNES_Output.pdf to ensure
# you're on the right track :)

# Try single linkage first
hs <- hclust(d,method="single")

# Define the height of the dendrogram (this will depend on the size of the values in hs)
heightseq <- seq(0, 40, 2)

# Note a call to max(hs$height) will help set an appropriate heigh
max(hs$height)

# Note the use of hang=-1 : this makes sure the output is prettier to read.
# Try removing hang=-1 and see what happens
plot(hs, col="darkblue", col.main="#45ADA8", col.lab="#7C8071",col.axis="#F38630", lwd=1.5, lty=3,cex=.75, sub='', hang=-1, axes=FALSE,main='US Arrests: AGNES with Single Linkage')
# add axis
axis(side=2, at=heightseq, col="#F38630",labels=FALSE, lwd=1)
# add text in margin
mtext(heightseq, side=2, at=heightseq,line=1, col="#A38630", las=2)

# ############# For the rest of the linkage types, the only change required is the method chosen in hclust()

# Now try complete linkage
hc <- hclust(d,method="complete")
plot(hc, col="darkblue", col.main="#45ADA8", col.lab="#7C8071",col.axis="#F38630", lwd=1.5, lty=3,cex=.75, sub='', hang=-1, axes=FALSE,main='US Arrests: AGNES with Complete Linkage')
# add axis
axis(side=2, at=heightseq, col="#F38630",labels=FALSE, lwd=1)
# add text in margin
mtext(heightseq, side=2, at=heightseq,line=1, col="#A38630", las=2)

# Average Linkage
ha = hclust(d,method="average")
plot(ha, col="darkblue", col.main="#45ADA8", col.lab="#7C8071",col.axis="#F38630", lwd=1.5, lty=3,cex=.75, sub='', hang=-1, axes=FALSE,main='US Arrests: AGNES with Average Linkage')
# add axis
axis(side=2, at=heightseq, col="#F38630",labels=FALSE, lwd=1)
# add text in margin
mtext(heightseq, side=2, at=heightseq,line=1, col="#A38630", las=2)

# Ward's Method
# Define the height of the dendrogram again since the values for Wards will be LARGE as it takes 
# into account variance
heightseq <- seq(0, 2200, 200)

hw <- hclust(d,method="ward")
plot(hw, col="darkblue", col.main="#45ADA8", col.lab="#7C8071",col.axis="#F38630", lwd=1.5, lty=3,cex=.75, sub='', hang=-1, axes=FALSE,main='US Arrests: AGNES with Wards Method')
# add axis
axis(side=2, at=heightseq , col="#F38630",labels=FALSE, lwd=1)
# add text in margin
mtext(heightseq , side=2, at=heightseq ,line=1, col="#A38630", las=2)


########################## DIANA: US Arrests Data ######################

# Note: currently DIANA allows for only manhattan and Euclidean distances
# The default is Euclidean distance

hc.diana <- diana(d)
plot(hc.diana, which = 2, cex = 0.6, main = "Dendrogram", xlab = "")
mtext("Divisive clustering of USArrests", outer = TRUE)

plot(hc.diana, which=2,col="darkblue", col.main="#45ADA8", col.lab="#7C8071",col.axis="#F38630", lwd=1.5, lty=3,cex=.75, sub='', hang=-1, axes=FALSE,main="USArrests: DIANA")
# add axis
axis(side=2, at=heightseq, col="#F38630",labels=FALSE, lwd=1)
# add text in margin
mtext(heightseq, side=2, at=heightseq,line=1, col="#A38630", las=2)


################################################################
######################## Partitional Clustering ################
################################################################


# ##################### k-means ################################

# Kmeans is a bit trickier.  We need to determine k, the number of clusters
# to extract.  To do this we are going to calculate SSE.


# Determine the sample size n and the number of variables we have p, for the 
# calculation
n = dim(USArrests)[1]; p = dim(USArrests)[2]

# Compute variances to determine how many clusters k to extract
SSE <- (n - 1) * sum(apply(USArrests,2,var)) 

# We compute the SSE for 1 to 15 clusters (ie k=1,...,15) and plot the 15 values
# The 'scree' plot suggests about 3 or 4
for (i in 2:15) {
  SSE[i] <- sum(kmeans(USArrests,centers=i,nstart=25)$withinss)
}

plot(1:15, SSE, type="b", xlab="Number of Clusters", ylab="Sum of squares within groups",pch=19, col="blue")

# Try k-means with 4 clusters (centers=4)
pc.km <- kmeans(USArrests, centers = 4)

# Now produce a plot of the four clusters themeselves
clusplot(d, pc.km$cluster, diss = TRUE, cex=0.7,col.p='midnightblue', col.clus='seagreen3',col.txt='blue', labels=3)


# ################  This code is for PART III.  

# Can cycle through plots and choose using the 'best' silhouette
asw <- numeric(20)
for (k in 2:20) {
   km <- kmeans(USArrests, centers = k)
   si <- silhouette(km$cluster,d)
   asw[k] <- mean(si[,3])
}

k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
plot(1:20, asw, type= "h", main = "k-means clustering assessment",
     xlab= "k  (# clusters)", ylab = "average silhouette width")
axis(1, k.best, paste("best",k.best,sep="\n"), col = "seagreen3", col.axis = "seagreen3")

pc.km <- kmeans(USArrests, centers = 4)

# Produce the silhouette plot
si <- silhouette(pc.km$cluster,d)
ssi <- summary(si)
#plot(si, col = c("green", "blue"))    # silly!
plot(si, col = "navy")