###### model creating using the snapper data set


library(ggplot2)  # For plotting
library(dplyr)    # For data manipulation


# upload the data
snappers <- read.csv("Datos_Florida/snappers.csv")
head(snappers, 5)

### select only numerical and relevant variables
snappers <- snappers %>%
  select(MEAN_RUG,
         MIN_DEPTH,
         Bare_Substrate,
         CORAL,
         MACROALGAE,
         SPONGES,
         Coral_Richness,
         Coral_Shannon,
         Coral_evenness,
         lobster,
         conch,
         diadema,
         total_biomass)

library(tidyverse)
library(MASS)     # For stepAIC function
library(caret)    # For data partitioning and model training
library(gridExtra)# arrange multiple plots

#### eliminar NA from data frame
# identify complete raws
filas_completas <- complete.cases(snappers)

# Crear un nuevo data frame con solo las filas completas
snappers_complete <- snappers[filas_completas, ]

# Convert all columns to numeric
for (i in 1:ncol(snappers_complete)) {
  snappers_complete[, i] <- as.numeric(snappers_complete[, i])
}

# Create a plot for each variable
# Define the variables to plot
vars <- c("MEAN_RUG", "MIN_DEPTH", "Bare_Substrate", "CORAL", "MACROALGAE", "SPONGES", "Coral_Richness", "Coral_Shannon", "lobster", "conch", "diadema")

####### multiplot for histgrams
# Create a list to store the plots
plot_histogram <- function(df_hist) {
hist_plot_list <- list()

# Create a plot for each variable and add it to the list
for (var in vars) {
  hist_plot_list[[var]] <- ggplot(snappers_complete, aes_string(x = var)) +
    geom_histogram(fill = "steelblue", color = "black") +
    xlab(var) +
    ylab("Frequency") +
    theme_minimal()
}
# Arrange the plots in a grid
do.call(grid.arrange, hist_plot_list)
}

# Call the function
plot_histogram(snappers_complete)


########## Scatter plot to visualize relationships
# Function to create scatterplots for each variable against 'Y'
plot_scatterplots <- function(df, y_var = "total_biomass") {
  plot_list <- list()
  
  for (var in names(df)) {
    if (var != y_var) {  # Avoid plotting against itself
      plot_list[[var]] <- ggplot(df, aes_string(x = var, y = y_var)) +
        geom_point() +
        xlab(var) +
        ylab(y_var) +
        theme_minimal()
    }
  }
  
  do.call("grid.arrange", plot_list)
}

# Call the function
plot_scatterplots(snappers_complete)

###### Check for multicollinearity
library(car)
cor(snappers_complete[, c("MEAN_RUG", "MIN_DEPTH", "Bare_Substrate", "CORAL", "MACROALGAE", "SPONGES", "Coral_Richness", "Coral_Shannon", "lobster", "conch", "diadema")])

# Transform variables log transformation
snappers_transform <- snappers_complete %>%
  mutate(across(where(is.numeric), ~log1p(.)))

cor(snappers_transform[, c("MEAN_RUG", "MIN_DEPTH", "Bare_Substrate", "CORAL", "MACROALGAE", "SPONGES", "Coral_Richness", "Coral_Shannon", "lobster", "conch", "diadema")])


# Call the function
plot_scatterplots(snappers_transform)
plot_histogram(snappers_transform)

ggplot(snappers_transform, aes(x = MEAN_RUG)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of MEAN_RUG", x = "MEAN_RUG", y = "Frequency") +
  theme_classic()

# Fit the model
model <- lm(total_biomass ~ log_MEAN_RUG + sqrt_MIN_DEPTH + ..., data = snappers)

# Evaluate model performance
summary(model)
plot(model)








###############################
# correlation for all variables
# improved correlation matrix
library(corrplot)

corrplot(cor(snappers, use = "pairwise.complete.obs"),
         method = "number",
         type = "upper" # show only upper side
)

# correlation tests for whole dataset
library(Hmisc)
res <- rcorr(as.matrix(snappers_num)) # rcorr() accepts matrices only

# display p-values (rounded to 3 decimals)
round(res$P, 3)

library(correlation)
snappers_num_complete <- na.omit(snappers_num)
correlation::correlation(snappers_num_complete, include_factors = TRUE, method = "auto")
correlation::correlation(snappers_num, use = "pairwise.complete.obs",
                         include_factors = TRUE, method = "auto"
)


# Función para crear un gráfico QQ-plot y realizar la prueba de Shapiro-Wilk
evaluar_normalidad <- function(snappers_num, variable) {
  ggplot(snappers_num, aes_string(sample = variable)) + 
    stat_qq() + 
    ggtitle(paste("QQ-plot para", variable))
  
  shapiro.test(pull(snappers_num, variable))
}

# Aplicar la función a cada columna
for (variable in names(snappers_num)) {
  print(ggplot2::ggplot(snappers_num, aes_string(sample = variable)) + 
          stat_qq() + 
          ggtitle(paste("QQ-plot para", variable)))
  
  print(shapiro.test(pull(snappers_num, variable)))
}



matriz_correlacion <- cor(snappers, use = "pairwise.complete.obs")

# Visualizar la matriz
corrplot(matriz_correlacion, method = "color")