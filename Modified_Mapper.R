
########################################################################
########## modified Mapper ##########
## changed one line (line 245) in clustering from mapper code
########################################################################



##### install packages #####

install.packages("devtools")
install.packages("igraph") 
install.packages("mclust")
install.packages("scales") # rescale
install.packages("lsa") # cosine similarity 

library(devtools)
library(mclust)
library(ggplot2)
library(igraph)
library(networkD3) # Interactive Plot
library(gplots) # hist2d
library(scales)
library(lsa)

############################################################################



############################################################
# function from the level set flat index (lsfi) 
# to the level set multi-index (lsmi)

lsmi_from_lsfi <- function( lsfi, num_intervals ) {
  # inputs:
  # lsfi = an integer in the range 1:prod(v)
  # num_intervals = c(i1,i1,...) a vector of numbers of intervals
  # output:
  # f+1 = a vector of multiindices with length filter_output_dim
  j <- c(1,num_intervals) # put 1 in front to make indexing easier in the product prod(j[1:k])
  f <- c()
  for (k in 1:length(num_intervals)) {
    # use lsfi-1 to shift from 1-based indexing to 0-based indexing
    f[k] <- floor( (lsfi-1) / prod(j[1:k])) %% num_intervals[k]
  }
  #print(f+1)
  # lsmi = f+1 = level set multi index
  return(f+1) # shift from 0-based indexing back to 1-based indexing
}

# the inverse function
lsfi_from_lsmi <- function( lsmi, num_intervals ) {
  lsfi <- lsmi[1]
  if (length(num_intervals) > 1) {
    for (i in 2:length(num_intervals)) {
      lsfi <- lsfi + prod(num_intervals[1:(i-1)]) * (lsmi[i]-1)
    }
  }
  return(lsfi)
}
###################################################################




###########################################################################
##### cluster_cutoff_at_first_empty_bin function #####

cluster_cutoff_at_first_empty_bin <- function(heights, diam, num_bins_when_clustering) {
  
  # if there are only two points (one height value), then we have a single cluster
  if (length(heights) == 1) {
    if (heights == diam) {
      cutoff <- Inf
      return(cutoff)
    }
  }
  
  bin_breaks <- seq(from=min(heights), to=diam, 
                    by=(diam - min(heights))/num_bins_when_clustering)
  if (length(bin_breaks) == 1) { bin_breaks <- 1 }
  
  myhist <- hist(c(heights,diam), breaks=bin_breaks, plot=FALSE)
  z <- (myhist$counts == 0)
  if (sum(z) == 0) {
    cutoff <- Inf
    return(cutoff)
  } else {
    #  which returns the indices of the logical vector (z == TRUE), min gives the smallest index
    cutoff <- myhist$mids[ min(which(z == TRUE)) ]
    return(cutoff)
  }
  
}
####################################################################



#######################################################################
############################################################################
##### TDA Mapper function #####

mapper <- function(dist_object, filter_values, num_intervals, percent_overlap, num_bins_when_clustering) {
  ##### begin documentation ############
  # inputs
  # f : X \subset R^n \to R^k, a filter function on a data set with numpoints observations
  # filter_values = data.frame(y_1, y_2,..., y_k), where each y_i is a vector of length num_points
  # num_intervals = c(i_1, i_2,..., i_k), a vector of number of intervals for each variable y_i
  # percent_overlap = c(p_1, p_2,..., p_k), a vector of percent overlap for adjacent intervals within each variable y_i
  ##### end documentation ###############
  
  
  #     #filter_output_dim <- length(filter_values)
  #     if (length(num_intervals) == 1) {
  #         num_points <- length(filter_values)
  #         filter_output_dim <- 1
  #         num_levelsets <- num_intervals
  # 
  #         # define some vectors of length k = number of columns = number of variables
  #         filter_min <- min(filter_values)
  #         filter_max <- max(filter_values)
  #         interval_width <- (filter_max - filter_min) / num_intervals
  # 
  #         } else {
  # #    filter_values <- as.matrix(filter_values)
  #         num_points <- dim(filter_values)[1] # number of rows = number of observations 
  #         filter_output_dim <- dim(filter_values)[2] # number of columns = number of variables = length(num_intervals)
  #         num_levelsets <- prod(num_intervals)
  #         
  #         # define some vectors of length k = number of columns = number of variables
  #         filter_min <- as.vector(sapply(filter_values,min))
  #         filter_max <- as.vector(sapply(filter_values,max))
  #         interval_width <- (filter_max - filter_min) / num_intervals
  # 
  #    }
  
  # class(filter_values[,1]) = numeric, which has dim(filter_values[,1]) = NULL,
  # so we coerce filter_values to a data.frame so that its dim is not NULL
  filter_values <- data.frame(filter_values) 
  num_points <- dim(filter_values)[1] # number of rows = number of observations 
  filter_output_dim <- dim(filter_values)[2] # number of columns = number of variables = length(num_intervals)
  num_levelsets <- prod(num_intervals)
  
  # define some vectors of length k = number of columns = number of variables
  filter_min <- as.vector(sapply(filter_values,min))
  filter_max <- as.vector(sapply(filter_values,max))
  interval_width <- (filter_max - filter_min) / num_intervals
  
  
  # initialize variables    
  vertex_index <- 0
  level_of_vertex <- c()
  points_in_vertex <- list()
  points_in_level_set <- vector( "list", num_levelsets )
  vertices_in_level_set <- vector( "list", num_levelsets )
  # for future development
  # cutree_in_level_set <- vector( "list", num_levelsets )
  
  
  #### begin plot the filter function ##############
  #     # Reality check
  #     # Plot the filter values
  #     plot(filter_values[,1], filter_values[,2], type="n")
  #     # cex = font size as a proportion of default
  #     text(filter_values[,1], filter_values[,2], labels=1:num_points, cex=0.5) 
  #     # midpoint of overlapping intervals
  #     abline(v = filter_min[1]+interval_width[1]*(0:num_intervals[1]), 
  #            h = filter_min[2]+interval_width[2]*(0:num_intervals[2]), col="red")
  #     # left and right interval boundaries
  #     abline(v = filter_min[1]+interval_width[1]*(0:num_intervals[1])
  #            -0.5*interval_width[1]*percent_overlap[1]/100, col = "blue", lty = 3)
  #     abline(v = filter_min[1]+interval_width[1]*(0:num_intervals[1])
  #            +0.5*interval_width[1]*percent_overlap[1]/100, 
  #            col = "blue", lty = 3)
  #     # bottom and top interval boundaries
  #     abline(h = filter_min[2]+interval_width[2]*(0:num_intervals[2])
  #            -0.5*interval_width[2]*percent_overlap[2]/100, col = "blue", lty = 3)
  #     abline(h = filter_min[2]+interval_width[2]*(0:num_intervals[2])
  #            +0.5*interval_width[1]*percent_overlap[2]/100, 
  #            col = "blue", lty = 3)
  #### end plot the filter function ########## 
  
  
  
  # begin loop through all level sets
  for (lsfi in 1:num_levelsets) {
    
    ################################
    # begin covering
    
    # level set flat index (lsfi), which is a number, has a corresponding 
    # level set multi index (lsmi), which is a vector
    lsmi <- lsmi_from_lsfi( lsfi, num_intervals )
    
    lsfmin <- filter_min + (lsmi - 1) * interval_width - 0.5 * interval_width * percent_overlap/100
    lsfmax <- lsfmin + interval_width + interval_width * percent_overlap/100
    
    # begin loop through all the points and assign them to level sets
    for (point_index in 1:num_points) {
      # compare two logical vectors and get a logical vector, 
      # then check if all entries are true
      if ( all( lsfmin <= filter_values[point_index,] & 
                filter_values[point_index,] <= lsfmax ) ) {
        points_in_level_set[[lsfi]] <- c( points_in_level_set[[lsfi]], 
                                          point_index )
      }
    } 
    # end loop through all the points and assign them to level sets
    
    # end covering
    ######################################
    
    ######################################
    # begin clustering
    
    points_in_this_level <- points_in_level_set[[lsfi]]
    num_points_in_this_level <- length(points_in_level_set[[lsfi]])
    
    if (num_points_in_this_level == 0) {
      num_vertices_in_this_level <- 0
    }
    
    if (num_points_in_this_level == 1) {
      #warning('Level set has only one point')
      num_vertices_in_this_level <- 1
      level_internal_indices <- c(1)
      level_external_indices <- points_in_level_set[[lsfi]]
    }
    
    if (num_points_in_this_level > 1) {
      # heirarchical clustering
      level_dist_object <- as.dist(
        as.matrix(dist_object)[points_in_this_level,points_in_this_level])
      level_max_dist <- max(level_dist_object)
      level_hclust   <- hclust( level_dist_object, method="single" )
      level_heights  <- level_hclust$height
      
      # cut the cluster tree
      # internal indices refers to 1:num_points_in_this_level
      # external indices refers to the row number of the original data point
      
      level_cutoff   <- cluster_cutoff_at_first_empty_bin(level_heights, level_max_dist, num_bins_when_clustering)
      ## change level_external_indices color="red"
      #level_external_indices <- points_in_this_level[level_hclust$order]
      level_external_indices <- level_external_indices <- as.numeric( level_hclust$labels )
      ##
      level_internal_indices <- as.vector(cutree(list(
        merge = level_hclust$merge, 
        height = level_hclust$height,
        labels = level_external_indices), 
        h=level_cutoff))
      num_vertices_in_this_level <- max(level_internal_indices)
      
    }
    
    # end clustering
    ######################################
    
    ######################################
    # begin vertex construction
    
    # check admissibility condition
    if (num_vertices_in_this_level > 0) { 
      
      vertices_in_level_set[[lsfi]] <- vertex_index + (1:num_vertices_in_this_level)
      
      for (j in 1:num_vertices_in_this_level) {
        
        vertex_index <- vertex_index + 1
        level_of_vertex[vertex_index] <- lsfi
        points_in_vertex[[vertex_index]] <- level_external_indices[level_internal_indices == j]
        
      }
    }
    
    # end vertex construction
    ######################################
    
  } # end loop through all level sets
  
  
  ########################################
  #  begin simplicial complex
  
  # create empty adjacency matrix
  adja <- mat.or.vec(vertex_index, vertex_index)
  
  # loop through all level sets
  for (lsfi in 1:num_levelsets) {
    
    # get the level set multi-index from the level set flat index
    lsmi <- lsmi_from_lsfi(lsfi,num_intervals)
    
    # Find adjacent level sets +1 of each entry in lsmi 
    # (within bounds of num_intervals, of course).
    # Need the inverse function lsfi_from_lsmi to do this easily.
    for (k in 1:filter_output_dim) {
      
      # check admissibility condition is met
      if (lsmi[k] < num_intervals[k]) {
        lsmi_adjacent <- lsmi + diag(filter_output_dim)[,k]
        lsfi_adjacent <- lsfi_from_lsmi(lsmi_adjacent, num_intervals)
      } else { next }
      
      # check admissibility condition is met
      if (length(vertices_in_level_set[[lsfi]]) < 1 |
          length(vertices_in_level_set[[lsfi_adjacent]]) < 1) { next }
      
      # construct adjacency matrix
      for (v1 in vertices_in_level_set[[lsfi]]) {
        for (v2 in vertices_in_level_set[[lsfi_adjacent]]) {
          adja[v1,v2] <- (length(intersect(
            points_in_vertex[[v1]],
            points_in_vertex[[v2]])) > 0)
          adja[v2,v1] <- adja[v1,v2]
        }
      }
      
    }
    
    
  }
  
  #  end simplicial complex
  #######################################
  
  mapperoutput <- list(adjacency = adja,
                       num_vertices = vertex_index,
                       level_of_vertex = level_of_vertex,
                       points_in_vertex = points_in_vertex,
                       points_in_level_set = points_in_level_set,
                       vertices_in_level_set = vertices_in_level_set
  )
  
  class(mapperoutput) <- "TDAmapper"
  
  return(mapperoutput)
  
  
} # end mapper function

###########################################################################





#############################################################
########## TDA Mapper function ##########

Mapper <- function(filter, n_intervals, perc, n_bins){
  # number of levels
  n_levels <- prod(n_intervals)
  # from R TDA mapper package
  m <<- mapper(dist_object = d,    # we may use W.s or d.s as distance
               filter_values = filter,
               num_intervals = n_intervals, 
               percent_overlap = perc,
               num_bins_when_clustering = n_bins 
  )
  # making mapper graph
  g_m <<- graph.adjacency(m$adjacency, mode="undirected")
  # color of vertex via rainbow function
  v_level <- rep(0, m$num_vertices)
  for (i in 1:m$num_vertices){
    v_level[i] <- m$level_of_vertex[i]
  }
  V(g_m)$color <- rainbow( n_levels, alpha=0.5 )[v_level]
  # size of vertex
  vertex.size <- rep(0,m$num_vertices)
  for (i in 1:m$num_vertices){
    points.in.vertex <- m$points_in_vertex[[i]]
    vertex.size[i] <- length((m$points_in_vertex[[i]]))
  }
  V(g_m)$size <- rescale(vertex.size, to=c(8, 40) ) # default= 10
  # size of vertex label
  V(g_m)$label.cex <- rescale(vertex.size, to=c(0.6, 1.1) ) # default = 1
  # make title
  title = paste(#"filter=", comment(filter),  
    " num_levels=",as.character(n_levels), 
    " perc=",as.character(perc), 
    " num_bins=", as.character(n_bins),
    " :  num_vertices", as.character(m$num_vertices)
  )
  # plot mapper graph
  plot(g_m, main = title, layout = layout_nicely
       # vertex.size=10, vertex.label.cex = 1
  )
  # Fruchterman-Reingold is one of the most used force-directed layout algorithms
}

#######################################################################




