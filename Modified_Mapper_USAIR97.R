#################################################################
## First open and run Modified_Mapper.R



########################################################################
########## Load Data ###########

# vertex and edge data
v_data <- read.csv("V_USAIR97.csv") # vertex data
e_data <- read.csv("E_USAIR97.csv") # edge data
# change y.coord of vertex
v_data$y.coord <- v_data$y.coord*(-1) 
# edge data with simple (or unweighte) weight
n=nrow(e_data)
v.s <- rep(1,n)
e.s_data <- e_data
e.s_data$weight <- rep(1,n) 
rm(n,v.s )

########################################################################



########################################################################
########## graph, adjacent matrix, and (graph) distance #######
########## from data

# weighted and simple (or unweighted) graphs from data frame
g <- graph_from_data_frame(e_data, directed = FALSE, vertices = v_data)
g.s <- graph_from_data_frame(e.s_data, directed = FALSE, vertices = v_data)
# vertex location matrix
v_lm <- as.matrix(v_data[,3:4])
# edge weight matrix or weighted adjacent matrix
#  (weight = normalized distance if connected 
#   email communication with Mrvar)
W <- as_adjacency_matrix(g, attr="weight", sparse = FALSE)
W.s <- as_adjacency_matrix(g.s, attr="weight", sparse = FALSE)
# distance of a weighted graph = length (by weights) of all the shortest paths
# If all weights are positive, then Dijkstra's algorithm is used.
d <- distances(g) # graph distance matrix
d.s <- distances(g.s)
########################################################################



#####################################################################
########## plot mapper graph ##########

filter <- list(d.s[,13]) # list(filter_1,filter_2,....)
n_intervals <- c(5)   # c(n_1,n_2,...)
perc <- 50
n_bins <- 10

# plot mapper graph
Mapper(filter, n_intervals, perc, n_bins)

#########################################





#######################################################################
##########################################################################
########## clustering  ###########################

##### Example 1 by USAIR 97 #####

##### hierarchical clustering by single #####
points_in_this_level <- m[["points_in_level_set"]][[1]]
d_1 <- as.matrix(d)[points_in_this_level,points_in_this_level]
level_dist_object <- as.dist(
  as.matrix(d)[points_in_this_level,points_in_this_level])
level_max_dist <- max(level_dist_object)
level_hclust   <- hclust( level_dist_object, method="single" )
level_heights  <- level_hclust$height
# plot dendrogram, hang = -1 (put the labels at the same height)
plot(level_hclust, hang = -1, cex=0.6)
###############################################################



##### cutoff by cluster_cutoff_at_first_empty_bin
heights <- level_heights
diam <- level_max_dist
num_bins_when_clustering <- 10
bin_breaks <- seq( from = min(heights), to=diam, 
                   by =( diam - min(heights) )/num_bins_when_clustering )
# histogram
myhist <- hist(c(heights, diam), breaks=bin_breaks, plot=TRUE)
#
z <- (myhist$counts == 0)
# cutoff
level_cutoff <- myhist$mids[ min(which(z == TRUE)) ]
##
plot(level_hclust, hang = -1, cex=0.6)
abline(h=level_cutoff, col="red")




##### indexing vertices #####
# level_external_indices <- points_in_this_level[level_hclust$order]
## This seems wrong; it should be:
level_external_indices <- as.numeric( level_hclust$labels )
##
level_internal_indices <- as.vector(cutree(list(
  merge = level_hclust$merge, 
  height = level_hclust$height,
  labels = level_external_indices), 
  h=level_cutoff))
## checking
level_hclust[["labels"]][ level_internal_indices == 2 ]
level_hclust[["labels"]][ level_external_indices == 8 ]
level_hclust[["labels"]][ level_hclust$order == 2 ]
level_internal_indices[ level_hclust$labels == "8" ]
################################################################





#################################################################
##### Example 2 #####


##### data #####
D_m <- cbind( c(0,1,2,6), c(3,3,3,3) )
rownames(D_m) <- c("a","b","c","d")
D <- data.frame( D_m )
# plot D
# limit of xy-axes
lim <- c(-0.2,6.3,-0.2,6.3)
plot(D, #xlim=lim[1:2], ylim=lim[3:4],
     col="gray90",pch = 19, cex=3,
     panel.last = grid())
text(D, labels=rownames(D), cex=0.9, font=2) # labelling points



##### clutering #####
# distance matrix
d_D <- dist(D, method = "euclidean")
# hierarchical clutering by single
hc <- hclust(d_D, method = "single")
# plot dendrogram
plot(hc, hang=-1)



##### cutoff #####
# heights
heights <- hc$height
# diameter = largest element in distance matrix
diam <- max(d_D)
# num_bins_when_clustering
num_bins_when_clustering <- 10
# bin_breaks
bin_breaks <- seq( from = min(heights), to=diam, 
                   by =( diam - min(heights) )/num_bins_when_clustering )
# histogram
myhist <- hist(c(heights, diam), breaks=bin_breaks, plot=TRUE)
#
z <- (myhist$counts == 0)
# cutoff
cutoff_D <- myhist$mids[ min(which(z == TRUE)) ]
# plot dendrogram
plot(hc, hang=-1)
abline(h=cutoff_D, col="red")




##### indexing vertices #####
external_indices <- rownames(D)
internal_indices <- as.vector(cutree(list(
  merge = hc$merge, 
  height = hc$height,
  labels = external_indices), 
  h=cutoff_D))
# checking
external_indices[internal_indices==1]
D[external_indices==4,]






