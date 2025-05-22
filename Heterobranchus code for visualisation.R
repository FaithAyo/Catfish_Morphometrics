#INSTALL PACKAGES

# Load necessary libraries
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(ggplot2) # For visualization
library(FactoMineR) # For PCA
library(factoextra) # For PCA visualization
library(psych)   # For KMO test

gc()  # Runs garbage collection to free up memory
rm(list = ls())  # Clears all variables/functions from the workspace

# Step 1: Load the dataset
file_path <- file.choose()  # This will open a file dialog for you to select the file
data <- read_excel(file_path)

#load the sheet names
sheet_names <- excel_sheets(file_path)
print(sheet_names)

#lead each of the sheet
clarias_data <- read_excel(file_path, sheet = "Sheet2")
Hybrid_data <- read_excel(file_path, sheet = "Sheet3")
heterobranchus_data <- read_excel(file_path, sheet = "Sheet4")



# Step 2: View the first few rows of the dataset
head(.data)
View(Hybrid_data)
print(colnames(data))


# Step 3: Perform the KMO test
kmo_result <- KMO(data)
print(kmo_result)  # show KMO result which is kaiser-meyer-olkin factor adequacy: MSA means measure of sample adequacy

#perform KMO for heterobranchus
Hetero_kmo_result <- KMO(heterobranchus_data)

# Step 4: Check if KMO value is acceptable
if (Hetero_kmo_result$MSA >= 0.6) {
  print("The data is suitable for PCA.")
} else {
  print("The data is not suitable for PCA.")
}

#check the KMO
View(Hetero_kmo_result)

Hetero_pca_data <- prcomp(heterobranchus_data)
print(Hetero_pca_data) 

#Run PCA
hetero_pca <- PCA(heterobranchus_data, scale.unit = TRUE, ncp = 5, graph = TRUE)
?PCA

#EXTRACT the eigenvalue
hetero_eig_value <- get_eigenvalue(hetero_pca)

hetero_eig_value

#visualise the eigen value, this produces a screeplot
 fviz_eig(hetero_pca)
 fviz_screeplot(hetero_pca)
 #or use this which adds label to the scree plot
 fviz_eig(hetero_pca, addlabels = TRUE, ylim = c(0, 60))
 
 

 #extract the results of individuals and variables respectively
 get_pca_ind(hetero_pca) 
 get_pca_var(hetero_pca)
 
 #visualise the results of individuals and variables respectively
 fviz_pca_ind(hetero_pca) 
 fviz_pca_var(hetero_pca)
 #you can acess different component of the variables
 #to know the variable that contribut the most 
 fviz_cos2(hetero_pca, choice = "var", axes = 1:2)
 
 #colour by cos2 values: quality on the factor map
 fviz_pca_var(hetero_pca, col.var = "cos2", gradient.cols = c("white", "blue", "red"),
              repel = TRUE,
              legend.title = "Variable contrib."
              title = "Heterobranchus longifilis PCA -Biplot")
 ?fviz_pca_var
 fviz_pca_var(hetero_pca, 
              col.var = "cos2", 
              gradient.cols = c("white", "blue", "red"),
              repel = TRUE,
              legend.title = "Variable contrib.",
              title = "Heterobranchus longifilis PCA - Biplot")
 
 
 #make a biplot of individuals and variables
 fviz_pca_biplot(hetero_pca)
 
 #contributions of the variables like the top 10
 fviz_contrib(hetero_pca, choice ="var", axes = 1:2, top = 10)

 #the most important or contributing variables can be highlighted on the correlation
 #plot as follows
 fviz_pca_var(hetero_pca, col.var = "contrib", gradient.cols = c("white", "green", "blue", "red"))
 
#change the transperency of the contribution
 fviz_pca_var(hetero_pca, alpha.var = "contrib") 
 
 ##
 set.seed(123)
 trying <- kmeans(var , centers = 3, nstart = 25)
 grp <- as.factor(trying$cluster)

 #for individual
 fviz_pca_ind(hetero_pca, col.ind = "cos2", gradient.cols = c("white", "blue", "red"),
              repel = TRUE) #avoid text overlapping
 
 
 #AN EXAMPLE ON HOW to add concentration ellipse
# fviz_pca_ind(hetero_pca, geom.ind = "point", #show points only
 #             col.ind = hetero_pca$var, #colour by groups
  #            palette = c("green", "red", "blue"),
   #           addEllipses = TRUE, #CONCENTRATION ELLIPSES
    #          legend.title = "Groups")
 
 
 
#biplot of individual and variable
 fviz_pca_biplot(hetero_pca, repel = TRUE, col.var = "red",
                 col.ind = "blue",
                 )
 

 fviz_pca_biplot(hetero_pca, repel = TRUE, col.var = "red",
                 col.ind = "black",
 )
 
 fviz_pca_biplot(hetero_pca,
                 label = "var",         # Only show variable labels
                 repel = TRUE,
                 col.var = "red",       # Color for variable arrows
                 col.ind = "black"      # Color for individual points
 )
 
#graph of variables
 hetero_var <-get_pca_var(hetero_pca)
 #highlight the most contributing variables for each dimension
 library("corrplot")
 #visualise the cos2 of the variables
corrplot(hetero_var$cos2, is.corr = FALSE) 

#visualise the contribution
corrplot(hetero_var$contrib, is.corr = FALSE)
corrplot(hetero_var$contrib, is.corr = TRUE) # JUST TESTING AN ERROR OCCURED
?corrplot()

#draw a heat map
loadings <- hetero_pca$var$coord  # Coordinates = loadings in PCA
library(pheatmap)

# Limit to first 3 or 4 PCs (optional, depending on variance explained)
install.packages("pheatmap")  # Only once
library(pheatmap)             # Every session

pheatmap(loadings[, 1:3], 
         cluster_rows = TRUE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE,
         color = colorRampPalette(c("blue", "white", "red"))(50),
         main = "PCA Loadings Heatmap")

library(reshape2)
library(ggplot2)

# Reshape for ggplot2
loadings_long <- melt(loadings[, 1:3])  # first 3 PCs
colnames(loadings_long) <- c("Trait", "PC", "Loading")

# Heatmap using ggplot2
ggplot(loadings_long, aes(x = PC, y = Trait, fill = Loading)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Heatmap of PCA Loadings", x = "Principal Component", y = "Trait")
