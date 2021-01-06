
# Importing data and installing the igraph package

#We are going to use the igraph package in R 
#We use the campnet dataset discussed in Borgatti 
#Dataset(campnet.txt) and attribute file (campattr.txt) are available from canvas.
#For more information on the data, see: https://sites.google.com/site/ucinetsoftware/datasets/campdata

#First we need to install the igraph package
install.packages("igraph")
library(igraph)
#now import the campnet data. Be sure that the two files are in your R working directory 
#the format of the network data is called an edgelist.
#header = False because we don't have a name to the two columns
campnet <- read.csv("campnet.txt", header=FALSE)
#the attribute file contains information on the nodes such as their gender, their role. It also already includes a centrality measure (betweenness) but this will be calculated separately later on in the script. 
#import the attribute file
campattr <- read.csv("campattr.txt")
#look at the format. 
class(campnet)
#should be a dataframe
#look at the first rows of the file
head(campnet)
head(campattr)

#We need to transform the data frame format into an igraph object to be able to use igraph's functions.
#in this step we also already add the attributes to the vertices
#as this is a directed network we set directed to TRUE
g <- graph_from_data_frame(campnet, directed = TRUE, vertices = campattr)
#look at the file format
class(g)
#now has become an igraph format
#look at the first rows
head(g)
#if you print out the g object it lists the edges.
print(g)
#you can also access the matrix using
g[]

#we can  count the number of nodes
vcount(g)
#and count the number of edges
ecount(g)

#we can plot the network with the kamada-kawai layout. 
#We replicate the figure 2.3 in borgatti which shows the directed network in graph format
plot(g,edge.arrow.size=.4,layout=layout_with_kk,main="campnet dataset")
#replicate figure 9.3 with gender attribute
plot(g,edge.arrow.size=.4,layout=layout_with_kk,main="campnet dataset with gender",vertex.color=vertex_attr(g)$Gender)

#Change into an undirected network by collapsing
g_undirected <- as.undirected(g, mode = "collapse")
plot(g_undirected,edge.arrow.size=.4,layout=layout_with_kk,main="campnet dataset undirected")
#if you look at the matrix representation you see that it has become symmetric
g_undirected[]

#all_simple_paths(g, "HOLLY", to = V(g), mode = c("out"))

#get distances between nodes. matrix 2.2 in borgatti. we take the directed graph and therefore add mode=out and mode=in
distances(g,mode="out")
distances(g,mode="in")

#find the number of strong components. strong components take direction into account
g.components <- components(g, mode = c("strong"))
print(g.components)

#the following line does the same thing but now just returns the number
count_components(g, mode = c("strong"))

#add attributes of component membership
V(g)$components <- g.components$membership

#examine attributes to check if it was added
vertex_attr(g)

#plot the graph with components
plot(g,edge.arrow.size=.4,layout=layout_with_kk,main="campnet dataset with components",vertex.color=vertex_attr(g)$components)


#to illustrate the discussion of matrix multiplication in 2.6 of Borgatti et al., we need to transform the igraph
#object back to a matrix object
#this next line converts the igraph object to a regular matrix
g_matrix <- get.adjacency(g_undirected,sparse=FALSE)
class(g_matrix)
print(g_matrix)
#matrix multiplication by itself to get number of walks of length 2 between i and j
g_matrix%*%g_matrix
#matrix multiplication to third power to get number of walks of length 3 between i and j
g_matrix%*%g_matrix%*%g_matrix
g_matrix%*%g_matrix%*%g_matrix%*%g_matrix

# Whole Network Measures

#measure the density. 
graph.density(g)
graph.density(g_undirected)

#Find density in subgroups:
#extract the womens network
g.women <- induced_subgraph(g, V(g)$Gender == 1)
#we can also plot only the women in the network
plot(g.women,edge.arrow.size=.2,layout=layout_with_kk,main="campnet dataset women",vertex.color=vertex_attr(g.women)$Gender)
#density among women. should be 0.357.
graph.density(g.women)
#density among men. should be 0.278.
g.men <- induced_subgraph(g, V(g)$Gender == 2)
plot(g.men,edge.arrow.size=.2,layout=layout_with_kk,main="campnet dataset men",vertex.color=vertex_attr(g.men)$Gender)
graph.density(g.men)

#Compactness
#Compactness is not available in igraph. 
#But we can calculate it by defining a function:
Compactness <- function(x) {
  gra.geo <- distances(x) ## get geodesics
  gra.rdist <- 1/gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances
  comp.igph <- mean(gra.rdist, na.rm=TRUE) 
  return(comp.igph)
}
Compactness(g)

#Reciprocity
reciprocity(g,ignore.loops = TRUE,mode="ratio")

#Transitivity 
#overall graph clustering coefficient
transitivity(g,type='average')
#this is the weighted overall clustering coefficient on the undirected network, which is the same as the 9.4 formula in Borgatti, but for undirected networks. 
transitivity(g)

#Centralization
#Calculations for the example from figure 9.8 in Borgatti
C=matrix(c(0,1,1,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1,0,1,0,0,1,1,0),nrow=5,ncol=5) 
print(C)
c <- graph_from_adjacency_matrix(C, mode="undirected")
plot(c)
degree(c)
centr_degree(c,mode="all",loops=FALSE,normalized=TRUE)
#applied to campnet data
centr_degree(g_undirected,loops=FALSE,normalized = TRUE)

#Diameter
diameter(g)

#Centrality Measures
#We do all the below calculations for the undirected network

# Degree centrality
deg <- degree(g_undirected)
print(deg)
#with the directed graph it would count up the in-degrees and outdegrees
deg_in <- degree(g,mode="in")
deg_out <- degree(g,mode="out")
deg_total <- degree(g,mode="total")
print(deg_in)
print(deg_out)
print(deg_total)

plot(g_undirected,edge.arrow.size=.4,layout=layout_with_kk,main="campnet dataset size=degree",vertex.size=deg*5)

#Eigenvector centrality
ev_g_un <- evcent(g_undirected)
print(ev_g_un)
ev_g_un$vector
plot(g_undirected,layout=layout_with_kk,main="campnet dataset size=eigenvector centrality",vertex.size=ev_g_un$vector*20)

#Beta centrality or Bonacich power centrality

#when b = 0 then this equals degree centrality
beta_centrality <- power_centrality(g_undirected, exponent=0)
print(beta_centrality)
plot(g_undirected,layout=layout_with_kk,main="campnet dataset size=beta centrality b=0",vertex.size=beta_centrality*20)

#we take a value of beta that approximates 1/largest eigenvalue. the largest eigenvalue is given by the eigenvector centrality function evcent
#you can also calculate it
e <- eigen(g_matrix)
e$values
max(e$values)
1/max(e$values)
#with beta at its highest, beta centrality should approximate eigenvector centrality
beta_centrality <- power_centrality(g_undirected, exponent=0.245)
print(beta_centrality)
plot(g_undirected,layout=layout_with_kk,main="campnet dataset size=beta centrality b=0.245",vertex.size=beta_centrality*10)

#Closeness centrality
#Closeness centrality measures how many steps is required to access every other vertex from a given vertex
closeness_g_un <- closeness(g_undirected, normalized=TRUE)
print(closeness_g_un)
plot(g_undirected,layout=layout_with_kk,main="campnet dataset size=closeness centrality",vertex.size=closeness_g_un*50)

#Betweenness centrality 
#Betweenness centrality measures the number of shortest paths going through a specific vertex
betweenness_g_un <- betweenness(g_undirected)
print(betweenness_g_un)
plot(g_undirected,layout=layout_with_kk,main="campnet dataset size=betweenness centrality",vertex.size=betweenness_g_un)
