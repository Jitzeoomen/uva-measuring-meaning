library(galoislattice)
library(igraph)

#the galoislattice package contains the davis data on the southern women data
data("southernwomenG")
class(southernwomenG)

# create a Galois Lattice with a reduced label
Galois <- do_galois_lattice(southernwomenG, label = "reduced")

# plot the Galois Lattice with the according layout
plot(Galois, layout = galois_layout(Galois))

A <- c(1,1,0)
B <- c(1,0,1)
C <- c(0,1,1)
D <- c(0,0,1)
E <- c(1,0,0)
my_m <- rbind(A,B,C,D,E)
colnames(my_m)<-c("loving","hateful","jealous")
rownames(my_m) <- c("DECISIVE","SEXY","DOMINANT","DEPENDENT","SUPPORTIVE")
Galois <- do_galois_lattice(my_m,label = "reduced")
plot(Galois, layout = galois_layout(Galois))
