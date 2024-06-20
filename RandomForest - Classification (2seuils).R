# Charger les bibliothèques nécessaires
library(caret)
library(randomForest)
library(dplyr)
library(ggplot2)
library(pROC)
library(rpart.plot)
library(rpart)

setwd("C:/Users/c.gueusquin/INTER RHONE/Stagiaires - General/2024 Cloé Gueusquin/2 - Modèle et Rapport/Modèles")

##################
# PRE-TRAITEMENT #
##################
# Charger et préparer les données
data <- read.csv2("C:/Users/c.gueusquin/INTER RHONE/Stagiaires - General/2024 Cloé Gueusquin/2 - Modèle et Rapport/data/data_tot.csv", header = TRUE)
data_clean <- data %>%
  select(EP_56, T, SO2a, TAV) %>%
  mutate(Risk_Level = case_when(
    EP_56 <= 20 ~ "Risque 0",
    EP_56 > 20 ~ "Risque 2"
  )) %>%
  na.omit()  # Supprimer les lignes avec des valeurs manquantes

ggplot(data, aes(x = EP_56)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of EP_56",
       x = "EP_56",
       y = "Count") +
  theme_minimal()

data_clean$Risk_Level <- as.factor(data_clean$Risk_Level)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
trainIndex <- createDataPartition(data_clean$Risk_Level, p = .8, list = FALSE, times = 1)
data_train <- data_clean[trainIndex, ]
data_test  <- data_clean[-trainIndex, ]

# Vérifier la répartition des classes dans les données d'entraînement
table(data_train$Risk_Level)

#####################
# MODELE PAR DEFAUT #
#####################

set.seed(123)

rf_model_simple <- randomForest(Risk_Level ~ T + SO2a + TAV, data = data_train)

rf_predictions <- predict(rf_model_simple, newdata = data_test) # Prédire les niveaux de risque sur les données de test

confusion_matrix <- confusionMatrix(rf_predictions, data_test$Risk_Level) # Évaluer la performance du modèle
print(confusion_matrix)

# Tracer la courbe ROC
rf_prob <- predict(rf_model_simple, newdata = data_test, type = "prob")
roc_data <- roc(response = data_test$Risk_Level, predictor = rf_prob[, 2], levels = rev(levels(data_test$Risk_Level)))

# Afficher l'AUC
auc_value <- auc(roc_data)
print(paste("AUC:", auc_value))

# Calculer les métriques pour le modèle simple
pred_values <- ifelse(rf_predictions == "Risque 0", 20, 3000)
actual_values <- ifelse(data_test$Risk_Level == "Risque 0", 20, 3000)

# Calculer MSE, RMSE, MAE, R² et R² ajusté
mse <- mean((pred_values - actual_values)^2)
rmse <- sqrt(mse)
mae <- mean(abs(pred_values - actual_values))
sst <- sum((actual_values - mean(actual_values))^2)
sse <- sum((pred_values - actual_values)^2)
r2 <- 1 - sse / sst
n <- length(actual_values)
p <- length(rf_model_simple$importance)
r2_adj <- 1 - ((1 - r2) * (n - 1) / (n - p - 1))

print(paste("MSE:", mse))
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R²:", r2))
print(paste("R² ajusté:", r2_adj))

# Calculer les métriques supplémentaires : Précision, Rappel et Score F1
precision <- posPredValue(rf_predictions, data_test$Risk_Level, positive = "Risque 2")
recall <- sensitivity(rf_predictions, data_test$Risk_Level, positive = "Risque 2")
f1_score <- (2 * precision * recall) / (precision + recall)

print(paste("Précision:", precision))
print(paste("Rappel:", recall))
print(paste("Score F1:", f1_score))

roc_df <- data.frame(
  specificity = roc_data$specificities,
  sensitivity = roc_data$sensitivities
)

ggplot(data = roc_df, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed") +
  labs(title = "ROC Curve",
       x = "Specificity",
       y = "Sensitivity") +
  theme_minimal() +
  annotate("text", x = 0.75, y = 0.25, label = paste("AUC:", round(auc_value, 2)), size = 5, color = "red")

################################### 
# MODELE OPTIMISE SANS SMOTE      #
###################################

## CHOIX DE NTREE ##
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

ntree_values <- seq(50, 500, by = 50)
results <- data.frame(ntree = integer(), Accuracy = numeric(), F1 = numeric(), Recall = numeric(), Specificity = numeric())

for (ntree in ntree_values) {
  set.seed(123)
  rf_model <- train(Risk_Level ~ T + SO2a + TAV, 
                    data = data_train, 
                    method = "rf", 
                    trControl = train_control, 
                    tuneGrid = data.frame(mtry = 2),  
                    ntree = ntree)
  
  rf_predictions <- predict(rf_model, newdata = data_test)
  
  # Calculer les métriques de classification
  confusion_mat <- confusionMatrix(rf_predictions, data_test$Risk_Level)
  accuracy <- confusion_mat$overall['Accuracy']
  f1 <- confusion_mat$byClass['F1']
  recall <- confusion_mat$byClass['Recall']
  specificity <- confusion_mat$byClass['Specificity']
  
  results <- rbind(results, data.frame(ntree = ntree, 
                                       Accuracy = accuracy,
                                       F1 = f1,
                                       Recall = recall,
                                       Specificity = specificity))
}

# Sélectionner le meilleur ntree basé sur la précision
best_ntree <- results[which.max(results$Accuracy), "ntree"]
print(paste("Best ntree:", best_ntree))

# Afficher les résultats complets
print(results)


## BEST MTRY ##
mtry_values <- 1:3
results <- data.frame(mtry = integer(), Accuracy = numeric(), F1 = numeric(), Recall = numeric(), Specificity = numeric())

for (mtry in mtry_values) {
  set.seed(123)
  rf_model <- train(Risk_Level ~ T + SO2a + TAV, 
                    data = data_train, 
                    method = "rf", 
                    trControl = train_control, 
                    tuneGrid = data.frame(mtry = mtry), 
                    ntree = best_ntree)
  
  rf_predictions <- predict(rf_model, newdata = data_test)
  
  # Calculer les métriques de classification
  confusion_mat <- confusionMatrix(rf_predictions, data_test$Risk_Level)
  accuracy <- confusion_mat$overall['Accuracy']
  f1 <- confusion_mat$byClass['F1']
  recall <- confusion_mat$byClass['Recall']
  specificity <- confusion_mat$byClass['Specificity']
  
  results <- rbind(results, data.frame(mtry = mtry, 
                                       Accuracy = accuracy,
                                       F1 = f1,
                                       Recall = recall,
                                       Specificity = specificity))
}

# Sélectionner le meilleur mtry basé sur la précision
best_mtry <- results[which.max(results$Accuracy), "mtry"]
print(paste("Best mtry:", best_mtry))

# Afficher les résultats complets
print(results)


## NODE SIZE ET MAX SIZE ##
maxnodes_values <- c(5, 10, 15, 20, 25)
nodesize_values <- c(1, 5, 10)
results <- data.frame(maxnodes = integer(), nodesize = integer(), Accuracy = numeric(), F1 = numeric(), Recall = numeric(), Specificity = numeric())

for (maxnodes in maxnodes_values) {
  for (nodesize in nodesize_values) {
    set.seed(123)
    rf_model <- train(Risk_Level ~ T + SO2a + TAV, 
                      data = data_train, 
                      method = "rf", 
                      trControl = train_control, 
                      tuneGrid = data.frame(mtry = best_mtry), 
                      ntree = best_ntree, 
                      nodesize = nodesize,
                      maxnodes = maxnodes)
    
    rf_predictions <- predict(rf_model, newdata = data_test)
    
    # Calculer les métriques de classification
    confusion_mat <- confusionMatrix(rf_predictions, data_test$Risk_Level)
    accuracy <- confusion_mat$overall['Accuracy']
    f1 <- confusion_mat$byClass['F1']
    recall <- confusion_mat$byClass['Recall']
    specificity <- confusion_mat$byClass['Specificity']
    
    results <- rbind(results, data.frame(maxnodes = maxnodes, 
                                         nodesize = nodesize, 
                                         Accuracy = accuracy,
                                         F1 = f1,
                                         Recall = recall,
                                         Specificity = specificity))
  }
}

best_combination <- results[which.max(results$Accuracy), ]
best_maxnodes <- best_combination$maxnodes
best_nodesize <- best_combination$nodesize
print(paste("Best maxnodes:", best_maxnodes, "Best nodesize:", best_nodesize))

# Afficher les résultats complets
print(results)


## VALIDATION
set.seed(123)
rf_model_best_params <- train(Risk_Level ~ T + SO2a + TAV, 
                              data = data_train, 
                              method = "rf", 
                              trControl = train_control, 
                              tuneGrid = data.frame(mtry = best_mtry), 
                              ntree = best_ntree, 
                              nodesize = best_nodesize,
                              maxnodes = best_maxnodes)

rf_predictions_best_params <- predict(rf_model_best_params, newdata = data_test)

# Calculer les métriques de classification
conf_matrix_optimized <- confusionMatrix(rf_predictions_best_params, data_test$Risk_Level)
accuracy_optimized <- conf_matrix_optimized$overall['Accuracy']
f1_optimized <- conf_matrix_optimized$byClass['F1']
recall_optimized <- conf_matrix_optimized$byClass['Recall']
specificity_optimized <- conf_matrix_optimized$byClass['Specificity']

print(paste("Accuracy (Modèle Optimisé):", accuracy_optimized))
print(paste("F1 Score (Modèle Optimisé):", f1_optimized))
print(paste("Recall (Modèle Optimisé):", recall_optimized))
print(paste("Specificity (Modèle Optimisé):", specificity_optimized))

# Afficher les chiffres de la matrice de confusion
print(conf_matrix_optimized$table)

# Visualiser la matrice de confusion avec ggplot
conf_matrix_df <- as.data.frame(conf_matrix_optimized$table)

ggplot(data = conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()

# Prédire les probabilités pour tracer la courbe ROC
rf_prob_2 <- predict(rf_model_best_params, newdata = data_test, type = "prob")
roc_data <- roc(response = data_test$Risk_Level, predictor = rf_prob_2[, 2], levels = rev(levels(data_test$Risk_Level)))

# Afficher l'AUC
auc_value <- auc(roc_data)
print(paste("AUC:", auc_value))

# Convertir les données ROC en data frame pour ggplot
roc_df_2 <- data.frame(
  specificity = roc_data$specificities,
  sensitivity = roc_data$sensitivities
)

# Tracer la courbe ROC avec ggplot
ggplot(data = roc_df_2, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed") +
  labs(title = "ROC Curve",
       x = "Specificity",
       y = "Sensitivity") +
  theme_minimal() +
  annotate("text", x = 0.75, y = 0.25, label = paste("AUC:", round(auc_value, 2)), size = 5, color = "red")

print(conf_matrix_optimized)

# Visualiser l'importance des variables
# Visualiser l'importance des variables
importance <- varImp(rf_model_best_params, scale = FALSE)
print(importance)

# Tracer l'importance des variables avec ggplot sous forme de points
importance_df <- data.frame(Variable = rownames(importance$importance), Importance = importance$importance$Overall)
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_point(size = 4, color = "blue") +
  coord_flip() +
  labs(title = "Variable Importance",
       x = "Variables",
       y = "Importance") +
  theme_minimal()


##################
# ARBRE DECISION #
##################
# Tracer un arbre de décision représentatif de la forêt simple
single_tree_simple <- getTree(rf_model_simple, k = 1, labelVar = TRUE)
rpart_tree_simple <- rpart(Risk_Level ~ T + SO2a + TAV, data = data_train)
rpart.plot(rpart_tree_simple, main = "Single Tree from Simple Random Forest")

# Tracer un arbre de décision représentatif de la forêt optimisée
single_tree <- getTree(rf_model_best_params$finalModel, k = 1, labelVar = TRUE)
rpart_tree <- rpart(Risk_Level ~ T + SO2a + TAV, data = data_train)
rpart.plot(rpart_tree, main = "Single Tree from Optimized Random Forest")


########################
# sauvegarde du modèle #
########################
saveRDS(rf_model_best_params, "rf_model_best_params.rds")
