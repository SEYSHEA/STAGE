# Charger les bibliothèques nécessaires
library(caret)
library(randomForest)
library(dplyr)
library(pROC)
library(DMwR)

# Charger les données
data <- read.csv2("C:/Users/c.gueusquin/INTER RHONE/Stagiaires - General/2024 Cloé Gueusquin/2 - Modèle et Rapport/data/data_tot.csv", header = TRUE)

# Nettoyer et préparer les données
data_clean <- data %>%
  mutate(
    EP_56 = as.numeric(sub(",", ".", EP_56)),
    T = as.numeric(T),
    SO2a = as.numeric(sub(",", ".", SO2a)),
    TAV = as.numeric(sub(",", ".", TAV))
  ) %>%
  dplyr::select(EP_56, T, SO2a, TAV) %>%
  mutate(
    Risk_Level = case_when(
      EP_56 <= 20 ~ "Risque 0",
      EP_56 <= 400 ~ "Risque 1",
      EP_56 <= 3000 ~ "Risque 2",
      EP_56 > 3000 ~ "Risque 3"
    )
  ) %>%
  na.exclude()  # Supprimer les lignes avec des valeurs manquantes tout en maintenant la correspondance des observations

data_clean$Risk_Level <- as.factor(data_clean$Risk_Level)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
trainIndex <- createDataPartition(data_clean$Risk_Level, p = .8, list = FALSE, times = 1)
data_train <- data_clean[trainIndex, ]
data_test  <- data_clean[-trainIndex, ]

# Appliquer SMOTE à l'ensemble d'entraînement
data_train_balanced <- SMOTE(Risk_Level ~ ., data = data_train, perc.over = 300, perc.under = 200)

# Définir la grille d'hyperparamètres
grid <- expand.grid(
  mtry = c(1, 2, 3),
  ntree = seq(50, 500, by = 50),
  nodesize = c(1, 5, 10)
)

# Initialiser un dataframe pour stocker les résultats
results <- data.frame(mtry = integer(), ntree = integer(), nodesize = integer(), Accuracy = numeric(), F1 = numeric(), Recall = numeric(), Specificity = numeric())

# Boucle pour trouver les meilleurs hyperparamètres
for (i in 1:nrow(grid)) {
  set.seed(123)
  rf_model <- randomForest(Risk_Level ~ T + TAV + SO2a, data = data_train_balanced,
                           mtry = grid$mtry[i], ntree = grid$ntree[i], nodesize = grid$nodesize[i])
  
  rf_predictions <- predict(rf_model, newdata = data_test)
  
  confusion_mat <- confusionMatrix(rf_predictions, data_test$Risk_Level)
  accuracy <- confusion_mat$overall['Accuracy']
  precision <- confusion_mat$byClass['Pos Pred Value']
  recall <- confusion_mat$byClass['Sensitivity']
  f1 <- ifelse((precision + recall) == 0, NA, 2 * (precision * recall) / (precision + recall))
  
  results <- rbind(results, data.frame(
    mtry = grid$mtry[i], 
    ntree = grid$ntree[i], 
    nodesize = grid$nodesize[i],
    Accuracy = accuracy,
    F1 = f1,
    Recall = recall,
    Specificity = confusion_mat$byClass['Specificity']
  ))
}

# Sélectionner les meilleurs hyperparamètres basés sur la précision
best_params <- results[which.max(results$Accuracy), ]
print(best_params)

# Entraîner le modèle avec les meilleurs hyperparamètres
set.seed(123)
rf_model_best <- randomForest(Risk_Level ~ T + TAV + SO2a, data = data_train_balanced,
                              mtry = best_params$mtry, ntree = best_params$ntree, nodesize = best_params$nodesize)

# Faire des prédictions sur l'ensemble de test avec le modèle optimisé
rf_predictions_best <- predict(rf_model_best, newdata = data_test)
predictions_prob_best <- predict(rf_model_best, newdata = data_test, type = "prob")

# Évaluer la performance du modèle optimisé
confusion_matrix_best <- confusionMatrix(rf_predictions_best, data_test$Risk_Level)
print(confusion_matrix_best)

# Calculer les métriques spécifiques à la classification pour chaque classe
precision_0 <- posPredValue(as.factor(rf_predictions_best == "Risque 0"), as.factor(data_test$Risk_Level == "Risque 0"))
recall_0 <- sensitivity(as.factor(rf_predictions_best == "Risque 0"), as.factor(data_test$Risk_Level == "Risque 0"))
f1_0 <- (2 * precision_0 * recall_0) / (precision_0 + recall_0)

precision_1 <- posPredValue(as.factor(rf_predictions_best == "Risque 1"), as.factor(data_test$Risk_Level == "Risque 1"))
recall_1 <- sensitivity(as.factor(rf_predictions_best == "Risque 1"), as.factor(data_test$Risk_Level == "Risque 1"))
f1_1 <- (2 * precision_1 * recall_1) / (precision_1 + recall_1)

precision_2 <- posPredValue(as.factor(rf_predictions_best == "Risque 2"), as.factor(data_test$Risk_Level == "Risque 2"))
recall_2 <- sensitivity(as.factor(rf_predictions_best == "Risque 2"), as.factor(data_test$Risk_Level == "Risque 2"))
f1_2 <- (2 * precision_2 * recall_2) / (precision_2 + recall_2)

cat("Précision pour Risque 0: ", precision_0, "\n")
cat("Rappel pour Risque 0: ", recall_0, "\n")
cat("Score F1 pour Risque 0: ", f1_0, "\n")

cat("Précision pour Risque 1: ", precision_1, "\n")
cat("Rappel pour Risque 1: ", recall_1, "\n")
cat("Score F1 pour Risque 1: ", f1_1, "\n")

cat("Précision pour Risque 2: ", precision_2, "\n")
cat("Rappel pour Risque 2: ", recall_2, "\n")
cat("Score F1 pour Risque 2: ", f1_2, "\n")

# Tracer les courbes ROC pour chaque classe
roc_risque0 <- roc(data_test$Risk_Level == "Risque 0", predictions_prob_best[, "Risque 0"])
roc_risque1 <- roc(data_test$Risk_Level == "Risque 1", predictions_prob_best[, "Risque 1"])
roc_risque2 <- roc(data_test$Risk_Level == "Risque 2", predictions_prob_best[, "Risque 2"])

# Plotting the ROC curves
plot(roc_risque0, col = "blue", main = "COURBE ROC")
plot(roc_risque1, col = "red", add = TRUE)
plot(roc_risque2, col = "green", add = TRUE)
legend("bottomright", legend = c("Risque 0", "Risque 1", "Risque 2"), col = c("blue", "red", "green"), lwd = 2)

# Calculer et afficher l'AUC pour chaque classe
auc_risque0 <- auc(roc_risque0)
auc_risque1 <- auc(roc_risque1)
auc_risque2 <- auc(roc_risque2)

cat("AUC for Risque 0: ", auc_risque0, "\n")
cat("AUC for Risque 1: ", auc_risque1, "\n")
cat("AUC for Risque 2: ", auc_risque2, "\n")

# Visualiser l'importance des variables
varImpPlot(rf_model_best, main = "Importance des Variables")

# Afficher les valeurs d'importance des variables
importance(rf_model_best)

# Extraire et visualiser le premier arbre de la forêt
tree_1 <- getTree(rf_model_best, k = 1, labelVar = TRUE)
colnames(tree_1) <- c("predictor", "split point", "status", "left daughter", "right daughter", "prediction")
# Convertir en data frame
tree_1_df <- as.data.frame(tree_1)
# Afficher les premières lignes pour vérifier la structure
head(tree_1_df)

reprtree:::plot.getTree(rf_model_best, k=1, main="Single Tree from Optimized Random Forest")
