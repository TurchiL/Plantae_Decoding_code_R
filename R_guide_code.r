## R script for our guide on how to use R for basic (plant) data analysis

## Upload your data -- using iris dataset as example
## iris csv dataset downloaded from https://gist.github.com/netj/8836201
input_file <- "C:/Users/lt258356.INTRA/Downloads/iris_data.csv" # this is called a "variable"

## read your input data as a data frame (tabular structure)
df <- read.table(input_file,sep=",",header=TRUE) # sep specifies the "separator" (in this case commas), header = TRUE to consider the first line as the "title" of each column
head(df)
names(df)


## install ggplot2
install.packages("ggplot2")
library(ggplot2)

#### Plots ####
## Histogram showing the distribution of sepal length
ggplot(df, aes(x = sepal.length)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Sepal Length", x = "Sepal Length", y = "Count")
ggsave("C:/Users/lt258356.INTRA/Downloads/sepal_length_histogram.png") # save plot


## Density plot of sepal length
ggplot(df, aes(x = sepal.length)) +
  geom_density(fill = "skyblue", color = "black", alpha=.7) +
  labs(title = "Density plot of Sepal Length", x = "Sepal Length", y = "Count")
ggsave("C:/Users/lt258356.INTRA/Downloads/sepal_length_kernel_density.png") # save plot


## Scatter plot with linear regression line of sepal vs petal length
ggplot(df, aes(x = sepal.length, y = petal.length)) + #df$sepal.length, df$petal.length
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Petal vs Sepal Length", x = "Sepal Length", y = "Petal Length")
ggsave("C:/Users/lt258356.INTRA/Downloads/linear_regression.png") # save plot


## Boxplot of petal length by species
ggplot(df, aes(x = variety, y = petal.length, fill=variety)) +
  geom_boxplot() +
  labs(title = "Box Plot of Petal Length by Species", x = "Species", y = "Petal Length")
ggsave("C:/Users/lt258356.INTRA/Downloads/petal_length_by_species.png") # save plot


## Violin plot of petal width by species
ggplot(df, aes(x = variety, y = petal.width, fill = variety)) +
  geom_violin() +
  labs(title = "Petal Width by Species", x = "Species", y = "Petal Width")
ggsave("C:/Users/lt258356.INTRA/Downloads/violin_petal_width_by_species.png") # save plot


## Violin + data points + boxplot
ggplot(df, aes(x = variety, y = petal.width, fill = variety)) +
  geom_violin() +
  geom_jitter(width=0.15,height=0, col="gray20",alpha=0.3)+
  geom_boxplot(fill="white",width=0.1)+ # add boxplot over violins
  labs(title = "Petal Width by Species", x = "Species", y = "Petal Width")
ggsave("C:/Users/lt258356.INTRA/Downloads/violin_jitter_boxplot_petal_width_by_species.png") # save plot


## Heatmaps
install.packages("pheatmap")
library(pheatmap)

#Data processing for Heatmap
Heatmap_data <- df[apply(df, 1, function(x) sd(x) != 0),]

#Heatmap
Heatmap <- pheatmap(Heatmap_data, scale="row")






#### Statistical tests ####
## Perform the t-test for different levels of the categorical variable Species 
t_test <- t.test(sepal.length ~ variety, data = df, 
                 variety %in% c("Versicolor", "Virginica"))
print(t_test) # print the t-test


## Correlations
# Quantify correlation between two variables
correlation <- cor(df$sepal.length, df$petal.length)
print(correlation) # Print the correlation value

# test correlation
# Perform the correlation test
cor_test <- cor.test(df$sepal.length, df$petal.length)
print(cor_test) # Print the correlation value, p-values and confidence interval


## ANOVA
# one-way analysis of variance (ANOVA) 
anova_analysis <- aov(petal.length ~ variety, data = df)
summary(anova_analysis) # Summarize the ANOVA results

# two-way ANOVA
Two_way_anova <- aov(petal.length ~ sepal.length * sepal.width, data = df)
summary(Two_way_anova) #Summarize the two-way ANOVA results



## PCA
# Load the "PCAtools" package
install.packages("PCAtools")
library(PCAtools)

# Run PCA
PCA <- pca(df, scale= TRUE, removeVar = 0.1)

# Visualize PCA
biplot(PCA)