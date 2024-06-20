# Charger les bibliothèques nécessaires
library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(glmnet)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(plotly)
library(rsm)
library(splines)
library(car)
library(ggplot2)
library(lmtest)
library(MASS)
library(reshape2)
library(randomForest)
# Charger les données
setwd("C:/Users/c.gueusquin/INTER RHONE/Stagiaires - General/2024 Cloé Gueusquin/2 - Modèle et Rapport/Modèles")

# Charger et préparer les données
data <- read.csv2("C:/Users/c.gueusquin/INTER RHONE/Stagiaires - General/2024 Cloé Gueusquin/2 - Modèle et Rapport/data/data_tot.csv", header = TRUE)
# Nettoyer les données en supprimant les valeurs manquantes
data_clean <- na.omit(data)

#####################
# ANALYSE UNIVARIEE #
#####################

ggplot(data_clean, aes(x = EP_56)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  xlim(0, 400) +
  ggtitle("Distribution de EP_56 sur l'intervalle de 0 à 400") +
  xlab("EP_56") +
  ylab("Fréquence")

# Visualiser les paires de variables
ggpairs(data_clean, columns = 2:7)

# Répartiton de nos variables 
ggplot(data_clean, aes(x = pH)) + geom_histogram(binwidth = 0.1) + ggtitle("Distribution du pH")
ggplot(data_clean, aes(x = T)) + geom_histogram(binwidth = 1) + ggtitle("Distribution de la Température")
ggplot(data_clean, aes(x = SO2a)) + geom_histogram(binwidth = 0.1) + ggtitle("Distribution de SO2 actif")
ggplot(data_clean, aes(x = TAV)) + geom_histogram(binwidth = 1) + ggtitle("Distribution du TAV")
ggplot(data_clean, aes(x = sucres)) + geom_histogram(binwidth = 0.5) + ggtitle("Distribution des Sucres")
ggplot(data_clean, aes(x = lies)) + geom_histogram(binwidth = 1) + ggtitle("Distribution des Lies")
ggplot(data_clean, aes(x = EP_56)) + geom_histogram(binwidth = 0.5) + ggtitle("Distribution de EP_56")


###########################
# ANALYSE BIVARIEE        #
###########################
# Diagrammes de dispersion pour les relations entre variables
ggplot(data_clean, aes(x = pH, y = EP_56)) + geom_point() + ggtitle("pH vs EP_56")
ggplot(data_clean, aes(x = T, y = EP_56)) + geom_point() + ggtitle("Température vs EP_56")
ggplot(data_clean, aes(x = SO2a, y = EP_56)) + geom_point() + ggtitle("SO2 actif vs EP_56")
ggplot(data_clean, aes(x = TAV, y = EP_56)) + geom_point() + ggtitle("TAV vs EP_56")
ggplot(data_clean, aes(x = sucres, y = EP_56)) + geom_point() + ggtitle("Sucres vs EP_56")
ggplot(data_clean, aes(x = lies, y = EP_56)) + geom_point() + ggtitle("Lies vs EP_56")

columns_to_convert <- c('pH', 'T', 'SO2a', 'TAV', 'sucres', 'lies', 'EP_56')
for (col in columns_to_convert) {
  data_clean[[col]] <- as.numeric(sub(",", ".", data_clean[[col]]))
}

normality_tests <- lapply(columns_to_convert, function(col) shapiro.test(data_clean[[col]]))
names(normality_tests) <- columns_to_convert
print(normality_tests)

# Calculer la corrélation de Spearman
cor_spearman <- cor(data_clean[, columns_to_convert], method = "spearman")
cor_spearman_test <- cor.test(data_clean$pH, data_clean$EP_56, method = "spearman", exact = FALSE)
print(cor_spearman)
print(cor_spearman_test)

# Calculer la corrélation de Kendall
cor_kendall <- cor(data_clean[, columns_to_convert], method = "kendall")
cor_kendall_test <- cor.test(data_clean$pH, data_clean$EP_56, method = "kendall")
print(cor_kendall)
print(cor_kendall_test)

# Visualiser les matrices de corrélation
corrplot(cor_spearman, method = "circle", title = "Corrélation de Spearman")
corrplot(cor_kendall, method = "circle", title = "Corrélation de Kendall")

# Visualiser les relations bivariées
ggpairs(data_clean[, columns_to_convert], lower = list(continuous = wrap("points", alpha = 0.3)))

# Visualiser les relations bivariées
ggpairs(data_clean[, columns_to_convert], lower = list(continuous = wrap("points", alpha = 0.3)))

# Scatter plots pour certaines relations significatives
ggplot(data_clean, aes(x = T, y = EP_56)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Relation entre Température (T) et EP_56")

ggplot(data_clean, aes(x = SO2a, y = EP_56)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Relation entre SO2 actif (SO2a) et EP_56")


#####################
# TEST DE LINEARITE #
#####################

ggplot(data_clean, aes(x = T, y = EP_56)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Relation entre Température (T) et EP_56")

ggplot(data_clean, aes(x = SO2a, y = EP_56)) +
  
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Relation entre SO2 actif (SO2a) et EP_56")

# Régression simple pour analyse des résidus
model_T <- lm(EP_56 ~ T, data = data_clean)
model_SO2a <- lm(EP_56 ~ SO2a, data = data_clean)

# Diagnostics des résidus
par(mfrow = c(2, 2))
plot(model_T)
plot(model_SO2a)

# Component + Residual Plots
crPlots(model_T)
crPlots(model_SO2a)

# Modèle de régression linéaire
model <- lm(EP_56 ~ T + SO2a + TAV, data = data_clean)

# Test de Breusch-Pagan
bptest(model)

# Test de Shapiro-Wilk
shapiro.test(residuals(model))

# Transformation logarithmique de la variable dépendante
data_clean$log_EP_56 <- log(data_clean$EP_56 + 1)  # Ajouter 1 pour éviter log(0)

# Modèle de régression avec la variable transformée
model_log <- lm(log_EP_56 ~ T + SO2a, data = data_clean)
summary(model_log)

# Diagnostic des résidus transformés
par(mfrow = c(2, 2))
plot(model_log)

# Test de Shapiro-Wilk sur les résidus transformés
shapiro_test_log <- shapiro.test(residuals(model_log))
print(shapiro_test_log)

# Q-Q plot pour les résidus transformés
qqnorm(residuals(model_log))
qqline(residuals(model_log), col = "red")


# Trouver la meilleure transformation Box-Cox
bc <- boxcox(EP_56 ~ T + SO2a, data = data_clean, plotit = TRUE)
lambda <- bc$x[which.max(bc$y)]

# Appliquer la transformation Box-Cox
data_clean$bc_EP_56 <- ((data_clean$EP_56 ^ lambda) - 1) / lambda

# Modèle de régression avec la variable transformée
model_bc <- lm(bc_EP_56 ~ T + SO2a, data = data_clean)
summary(model_bc)

# Diagnostic des résidus transformés
par(mfrow = c(2, 2))
plot(model_bc)

# Test de Shapiro-Wilk sur les résidus transformés
shapiro_test_bc <- shapiro.test(residuals(model_bc))
print(shapiro_test_bc)

# Q-Q plot pour les résidus transformés
qqnorm(residuals(model_bc))
qqline(residuals(model_bc), col = "red")


###########################
#   RPART - régresssion   #
###########################

set.seed(123)
trainIndex <- createDataPartition(data_clean$EP_56, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data_clean[ trainIndex,]
dataTest  <- data_clean[-trainIndex,]

# Ajuster un modèle de régression avec rpart en utilisant uniquement TAV, SO2a et T
model_rpart <- rpart(EP_56 ~ T + SO2a + TAV, data = dataTrain, method = "anova")

# Visualiser l'arbre de décision
rpart.plot(model_rpart, type = 2, extra = 101)

# Prédire les valeurs sur l'ensemble de test
predictions <- predict(model_rpart, dataTest)

# Calculer les métriques de performance
mse <- mean((predictions - dataTest$EP_56)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - dataTest$EP_56))

cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("Mean Absolute Error:", mae, "\n")

# Visualisation des résidus
residuals <- dataTest$EP_56 - predictions
par(mfrow = c(2, 2))
plot(model_rpart)

# Q-Q Plot des résidus
qqnorm(residuals)
qqline(residuals, col = "red")

# Diagnostic Plots
par(mfrow = c(2, 2))
plot(residuals, main = "Résidus", ylab = "Résidus", xlab = "Index")
abline(h = 0, col = "red")

# Boxplot pour visualiser les outliers
boxplot(data_clean$EP_56, main = "Boxplot de EP_56", ylab = "EP_56")

##############################
# randomforest - régressions #
##############################

# Comparaison avec un modèle de forêt aléatoire en utilisant uniquement TAV, SO2a et T
model_rf <- randomForest(EP_56 ~ T + SO2a + TAV, data = dataTrain, ntree = 500, importance = TRUE)
predictions_rf <- predict(model_rf, dataTest)

# Calculer les métriques de performance pour le modèle de forêt aléatoire
mse_rf <- mean((predictions_rf - dataTest$EP_56)^2)
rmse_rf <- sqrt(mse_rf)
mae_rf <- mean(abs(predictions_rf - dataTest$EP_56))

cat("Random Forest - Mean Squared Error:", mse_rf, "\n")
cat("Random Forest - Root Mean Squared Error:", rmse_rf, "\n")
cat("Random Forest - Mean Absolute Error:", mae_rf, "\n")

# Visualisation des résidus du modèle de forêt aléatoire
residuals_rf <- dataTest$EP_56 - predictions_rf
par(mfrow = c(2, 2))
plot(residuals_rf, main = "Résidus (Forêt Aléatoire)", ylab = "Résidus", xlab = "Index")
abline(h = 0, col = "red")

# Q-Q Plot des résidus (Forêt Aléatoire)
qqnorm(residuals_rf)
qqline(residuals_rf, col = "red")

########################
# rpart CLASSIFICATION #
########################
data_clean <- na.omit(data)

# Convertir les données en numériques et créer la variable de classification
data_clean <- data_clean %>%
  mutate(
    EP_56 = as.numeric(sub(",", ".", EP_56)),
    T = as.numeric(sub(",", ".", T)),
    SO2a = as.numeric(sub(",", ".", SO2a)),
    TAV = as.numeric(sub(",", ".", TAV)),
    Risk_Level = case_when(
      EP_56 <= 20 ~ "Risque 0",
      EP_56 <= 400 ~ "Risque 1",
      EP_56 <= 3000 ~ "Risque 2",
      EP_56 > 3000 ~ "Risque 3"
    )
  ) %>%
  select(EP_56, T, SO2a, TAV, Risk_Level)

data_clean$Risk_Level <- as.factor(data_clean$Risk_Level)

# Séparer les données en ensemble d'entraînement et de test
set.seed(123)
trainIndex <- createDataPartition(data_clean$Risk_Level, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data_clean[trainIndex,]
dataTest <- data_clean[-trainIndex,]

# Ajuster un modèle de classification avec rpart
model_rpart_class <- rpart(Risk_Level ~ T + SO2a + TAV, data = dataTrain, method = "class")

# Visualiser l'arbre de décision
rpart.plot(model_rpart_class, type = 2, extra = 104)

# Prédire les classes sur l'ensemble de test
predictions_class <- predict(model_rpart_class, dataTest, type = "class")

# Calculer les métriques de performance
conf_matrix <- confusionMatrix(predictions_class, dataTest$Risk_Level)

cat("Confusion Matrix:\n")
print(conf_matrix)

# Comparaison avec un modèle de forêt aléatoire en utilisant uniquement TAV, SO2a et T
model_rf_class <- randomForest(Risk_Level ~ T + SO2a + TAV, data = dataTrain, ntree = 500, importance = TRUE)
predictions_rf_class <- predict(model_rf_class, dataTest)

# Calculer les métriques de performance pour le modèle de forêt aléatoire
conf_matrix_rf <- confusionMatrix(predictions_rf_class, dataTest$Risk_Level)

cat("Random Forest - Confusion Matrix:\n")
print(conf_matrix_rf)




















































