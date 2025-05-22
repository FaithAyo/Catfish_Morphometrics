#INSTALL PACKAGES

# Load necessary libraries
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(ggplot2) # For visualization
library(FactoMineR) # For PCA
library(factoextra) # For PCA visualization
library(psych)   # For KMO test
library(pheatmap) #for heatmap
library(corrplot) #visualisation of correlation matric





gc()  # Runs garbage collection to free up memory
rm(list = ls())  # Clears all variables/functions from the workspace

# Step 1: Load the dataset
file_path <- file.choose()  # This will open a file dialog for you to select the file
data <- read_excel(file_path)

#load the sheet names
sheet_names <- excel_sheets(file_path)
print(sheet_names)

#load each of the sheet
clarias_data <- read_excel(file_path, sheet = "Sheet2")
Hybrid_data <- read_excel(file_path, sheet = "Sheet3")
heterobranchus_data <- read_excel(file_path, sheet = "Sheet4")



# Step 2: View the first few rows of the dataset
head(.data)
View(clarias_data)
print(colnames(clarias_data))


#step 3
#perform KMO for heterobranchus
clarias_kmo_result <- KMO(clarias_data)

# Step 4: Check if KMO value is acceptable
if (clarias_kmo_result$MSA >= 0.6) {
  print("The data is suitable for PCA.")
} else {
  print("The data is not suitable for PCA.")
}

#view KMO result
#or
print(clarias_kmo_result$MSA)


#Step 4
#Run PCA
clarias_pca <- PCA(clarias_data, scale.unit = TRUE, ncp = 5, graph = TRUE)
# will return graph of individuals and variables


#EXTRACT the eigenvalue
clarias_eig_value <- get_eigenvalue(clarias_pca)

clarias_eig_value

#visualise the eigen value, this produces a screeplot


#step 5: draw a scree plot
#or use this which adds label to the scree plot
fviz_eig(clarias_pca, addlabels = TRUE, ylim = c(0, 35))#you can change this scale could be (0, 60)
#colour screeplot
green_shades <- colorRampPalette(c("darkred", "pink"))(10)
fviz_eig(clarias_pca, 
         addlabels = TRUE, 
         ylim = c(0, 35),
         barfill = green_shades,
         barcolor = "black")



#step 6 draw a biplot

#extract the results of individuals and variables respectively
get_pca_ind(clarias_pca) 
get_pca_var(clarias_pca)



#the most important or contributing variables can be highlighted on the correlation
#plot as follows

#biplot of individual and variable

fviz_pca_biplot(clarias_pca,
                label = "var",         # Only show variable labels
                repel = TRUE,
                col.var = "contrib", 
                gradient.cols = c("white", "green", "blue", "red"),    # Color for variable arrows
                col.ind = "black"      # Color for individual points
)


#step 7: draw a heatmap


#draw a heat map
clarias_loadings <- clarias_pca$var$coord  # Coordinates = loadings in PCA
#library(pheatmap)

#"Hybrid_PCA Loadings Heatmap"
pheatmap(clarias_loadings[, 1:4], #you can have the different dimensions
         cluster_rows = TRUE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE,
         color = colorRampPalette(c("blue", "white", "red"))(40)) #what does this number signify


