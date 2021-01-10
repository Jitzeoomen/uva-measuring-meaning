##########################################################
##week 2.1. two mode networks and structural equivalence##
##########################################################

##########################################################
########## from two-mode to one-mode networks ############
##########################################################

#to illustrate how to use matrix multiplication to transform 
#a 2-mode network into a 1-mode network, we make use of the 
#davis' women by event data set  
#the input data is an edge list in csv format
davis <- read.csv("davis.csv", header=FALSE)
class(davis)
head(davis)
#we can transform the data frame format into an igraph object.
library(igraph)
g <- graph_from_data_frame(davis, directed = FALSE, vertices = NULL)
#to visualize the graph we have to save it as a bipartite network
g2M <-g
V(g2M)$type <- bipartite_mapping(g2M)$type  # Add the "type" attribute
plot(g2M,layout=layout_with_kk,main="Southern Women Dataset - Bipartite")
#to do the matrix multiplication we save it as two-mode matrix
twomode_matrix <- as_incidence_matrix(g2M)
#this should give you the women by event matrix
twomode_matrix[]
#we can transpose the matrix and get the event by women matrix
t(twomode_matrix[])
#to convert the two-mode matrix to a one-mode matrix we can use matrix multiplication
#do matrix multiplication to get person and event network
#the person by person matrix
p <- twomode_matrix %*% t(twomode_matrix)
p[]
#the event by event matrix
e <- t(twomode_matrix) %*% twomode_matrix
e[]


#As discussed by Borgatti in section 13.2, it can be a good idea to normalize the overlap to take into account 
#the size of events and/or the frequence of participation of the women.
#The following lines execute the normalization as proposed by Bonacich '72.
#Mathematically, the normalization works in the following way -- formulated for the transformation into a person-by-person matrix:
#Consider two actors i and j and let X be the product of the number events they both attended and the number of events they both did not attend 
#let Y be the product of the number events i attended and j did not with the number of events j attended and i did not. 
#If X=Y the normalized entry is 0.5 otherwise it is (X-SQRT(XY))/(X-Y).
#As far as I know the Bonacich normalization is not part of the standard measures in igraph 
#but the following script will calculate it for you

#we do it first for the person-by-person network
p_norm <- p
total_events <- ncol(e)
total_events
person_matrix_diagonal <- diag(p)
nrow(p)
ncol(p)

# Create the loop with r and c to iterate over the matrix
for (i in 1:nrow(p)) {
  for (j in 1:ncol(p)) { 
    a <- p[i,j]
    print(a)
    b <- p[i,i]-p[i,j]
    #print(b)
    c <- p[j,j]-p[j,i]
    #print(c)
    d <- total_events-a-b-c
    #print(d)
    if (a*d==b*c) {
      p_norm[i,j]=50
    } else {
      p_norm[i,j] = ((a*d - sqrt (a*d*b*c))  / ( (a * d)  -  (b * c)))*100
    }
    #print(e_norm)
  }
}
p_norm[]

#and now for the event-by-event matrix 
e_norm <- e
total_persons <- ncol(p)
total_persons
event_matrix_diagonal <- diag(e)
nrow(e)
ncol(e)

# Create the loop with r and c to iterate over the matrix
for (i in 1:nrow(e)) {
  for (j in 1:ncol(e)) { 
    a <- e[i,j]
    print(a)
    b <- e[i,i]-e[i,j]
    #print(b)
    c <- e[j,j]-e[j,i]
    #print(c)
    d <- total_persons-a-b-c
    #print(d)
    if (a*d==b*c) {
      e_norm[i,j]=50
    } else {
      e_norm[i,j] = ((a*d - sqrt (a*d*b*c))  / ( (a * d)  -  (b * c)))*100
      }
    #print(e_norm)
  }
}
e_norm[]
#you can compare the results for the event-by-event matrix with matrix 13.4 in borgatti.  

############################################################################################
#Here we will replicate the analysis done by John Mohr in Soldiers, Mothers, Tramps and Others
#He uses the Concor algorithm to do the blockmodeling. This is not available in igraph. 
#Adam Slez has rewritten the program in R. This is not available through CRAN but we can
#download it directly from github. We need the devtools package to do this.
############################################################################################
library(devtools)
devtools::install_github("aslez/concoR")
library(concoR)
#we import the mohr soldiers data
soldiers <- read.csv2("mohr_matrix_soldiers.txt", header=TRUE)
soldiers[]
#the first column has the names. need to remove those to be able to calculate the correlation between all columns
soldiers1 <- subset(soldiers,select=-X)
str(soldiers1)
#we take the correlation among the columns
soldiers_cor <- cor(soldiers1)
soldiers_cor
#to follow along with the discussion of the correlation between seamen and widows select the correlation between these two identities
cor(soldiers1$SEAMEN,soldiers1$WIDOWS)
#the correlation is relatively low with 0.21. but as we'll see, they will be classified as similar based on having similar patterns of correlation with *other* status identities
#to see this, we apply the concor algorithm. 
#p=3 means that we look at a 3-level split. which is what Mohr reports in his figure 1.
blks3 <- concor_hca(list(soldiers_cor),p=3) 
blks3
#SEAMEN and WIDOWS are now grouped together in block 5 (together with BLIND_NG IMMIGN_NG SOLDIERS)
#This means that these status identities are structurally equivalent, i.e. have similar positions vis-a-vis other status identities
#In other words, both relate in similar manners to other status identities
#So even if they do not strongly correlate with each other, they do have a similar position in the overall discourse structure
#To further investigate the blockmodel solution we can also look at the groupings at lower splits.
#This should show the same divisions as in figure 1
blks2 <- concor_hca(list(soldiers_cor),p=2) 
blks2
blks1 <- concor_hca(list(soldiers_cor),p=1) 
blks1
#########################################################################






