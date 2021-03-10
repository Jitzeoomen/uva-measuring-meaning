#we load in example data used in Applied Correspondence Analysis by Sten-Erik Clausen
N = matrix(c(395, 147, 694, 2456, 153, 327, 1758, 916, 1347), 
           nrow = 3,
           dimnames = list(
             "Region" = c("Oslo", "Mid Norway", "North Norway"),
             "Crime" = c("Burglary", "Fraud", "Vandalism")))
#have a look at the raw frequency table
print(N)

#first we normalize the matrix by dividing all cell entries by the total frequency
n = sum(N)
print(n)
P = N / n
print(P)

#we calculate the average row profile, or i.e. the column masses
column.masses = colSums(P)
print(column.masses)
#we calculate the average column profile, or i.e. the row masses
row.masses = rowSums(P)
print(row.masses)

#using the row and column masses we can calculate the expected frequency in each cell
E = row.masses %o% column.masses
print(E)

#the residuals is the difference between the observed and expected frequencies
R = P - E
print(R)

#the residials dividing by the expected value and multiplying by square root of expected values to get the chi-square distances
I = R / E
print(I)
Z = I * sqrt(E)
print(Z)

#this matrix can then be submitted to SVD
SVD = svd(Z)
rownames(SVD$u) = rownames(P) 
rownames(SVD$v) = colnames(P)
print(SVD)

#eigenvalues are singular values squared 
eigenvalues = SVD$d^2
#we calculate the proportion of the singular values to the total which indicated the explained variance per dimension
prop.table(eigenvalues)
standard.coordinates.rows = sweep(SVD$u, 1, sqrt(row.masses), "/")
print(standard.coordinates.rows)
standard.coordinates.columns = sweep(SVD$v, 1, sqrt(column.masses), "/")
print(standard.coordinates.columns)
principal.coordinates.rows = sweep(standard.coordinates.rows, 2, SVD$d, "*")
print(principal.coordinates.rows)
principal.coordinates.columns = sweep(standard.coordinates.columns, 2, SVD$d, "*")
print(principal.coordinates.columns)

#symmetric plot takes the principal coordinates of rows and columns
plot(principal.coordinates.columns[,1:2], xlim=c(-0.8,0.8), ylim=c(-0.6,0.6))
points(principal.coordinates.rows[,1:2])
text(principal.coordinates.rows,labels=rownames(principal.coordinates.rows),pos=1)
text(principal.coordinates.columns,labels=rownames(principal.coordinates.columns),pos=1)

#asymmetric plot takes the principal coordinates of rows
plot(principal.coordinates.rows[,1:2], xlim=c(-0.8,0.8), ylim=c(-0.6,0.6))
points(standard.coordinates.columns[,1:2])
text(principal.coordinates.rows,labels=rownames(principal.coordinates.rows),pos=1)
text(standard.coordinates.columns,labels=rownames(principal.coordinates.columns),pos=1)

#compare the results with the factominer CA solution
res.ca <- CA(N, graph = TRUE)
fviz_ca_biplot(res.ca, repel = TRUE)
eig.val <- get_eigenvalue(res.ca)
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 100))
fviz_ca_biplot(res.ca, repel = TRUE)

