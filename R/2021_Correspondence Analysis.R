install.packages("FactoMineR") 
install.packages("factoextra")
install.packages("gplots")
install.packages("corrplot")
library("gplots")
library("FactoMineR")
library("factoextra")
library("corrplot")

#we illustrate CA with a dataset in FactoMineR
data(housetasks)

#calculate chi-square test
chisq <- chisq.test(housetasks)
chisq

#do the CA
res.ca <- CA(housetasks, graph = FALSE)
print(res.ca)

#get eigenvalues
eig.val <- get_eigenvalue(res.ca)
eig.val

#inspect plot of eigenvalues
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 80))

#plot the CA. this is a symmetric plot 
fviz_ca_biplot(res.ca, repel = TRUE)

#inspect rows
row <- get_ca_row(res.ca)
row

# Coordinates
print(row$coord)

#The cos2 measures the degree of association between rows/columns and a particular axis
#The sum of the cos2 for rows on all the CA dimensions is equal to one.
print(row$cos2)
corrplot(row$cos2, is.corr=FALSE)

# Contributions to the principal components
#The row variables with the larger value, contribute the most to the definition of the dimensions
print(row$contrib)
corrplot(row$contrib, is.corr=FALSE)

# Contributions of rows to dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)

#Can do the same for the columns
