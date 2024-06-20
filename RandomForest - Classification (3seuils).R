# Charger les bibliothèques nécessaires
library(randomForest)
library(caret)
library(dplyr)
library(pROC)
library(reprtree)

setwd("C:/Users/c.gueusquin/INTER RHONE/Stagiaires - General/2024 Cloé Gueusquin/2 - Modèle et Rapport/Modèles")

#################
# MODELE SIMPLE #
#################

# Charger et préparer les données
data <- read.csv2("C:/Users/c.gueusquin/INTER RHONE/Stagiaires - General/2024 Cloé Gueusquin/2 - Modèle et Rapport/data/data_tot.csv", header = TRUE)
data_clean <- na.omit(data)
data_clean <- data_clean %>%
  select(EP_56, T, SO2a, TAV)

# Définir les niveaux de risque
data_clean$Risk_Level <- cut(data_clean$EP_56, breaks=c(-Inf, 20, 400, Inf), labels=c("Risque 0", "Risque 1", "Risque 2"))

# Préparer les ensembles d'entraînement et de test
set.seed(42)
trainIndex <- createDataPartition(data_clean$Risk_Level, p = .7, list = FALSE, times = 1)
TrainData <- data_clean[trainIndex,]
TestData  <- data_clean[-trainIndex,]

# Fonction pour calculer les courbes ROC pour chaque classe
multi_roc <- function(true_labels, predicted_probs, class_levels) {
  roc_curves <- list()
  for (level in class_levels) {
    true_binary <- ifelse(true_labels == level, 1, 0)
    predicted_binary <- predicted_probs[, level]
    roc_curve <- roc(true_binary, predicted_binary)
    roc_curves[[level]] <- roc_curve
  }
  return(roc_curves)
}

################################### 
# MODELE SIMPLE                   #
###################################
# Entraîner le modèle Random Forest de base
rf_model_simple <- randomForest(Risk_Level ~ T + TAV + SO2a, data = TrainData)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(rf_model_simple, newdata = TestData)
predictions_prob <- predict(rf_model_simple, newdata = TestData, type = "prob")

# Évaluer les performances du modèle
conf_matrix <- confusionMatrix(predictions, TestData$Risk_Level)
accuracy <- conf_matrix$overall['Accuracy']
class_report <- conf_matrix$byClass

# Afficher les résultats
print(conf_matrix)
cat("Accuracy: ", accuracy, "\n")
print(class_report)

# Calculer les courbes ROC pour chaque classe
roc_curves <- multi_roc(TestData$Risk_Level, predictions_prob, levels(TestData$Risk_Level))

# Tracer les courbes ROC pour chaque classe
plot(roc_curves[[1]], col = "blue", main = "Courbes ROC modèle simple")
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], col = i, add = TRUE)
}
legend("bottomright", legend = names(roc_curves), col = 1:length(roc_curves), lwd = 2)

# Calculer et afficher l'AUC pour chaque classe
auc_values <- sapply(roc_curves, auc)
for (i in 1:length(auc_values)) {
  cat("AUC ", names(auc_values)[i], ": ", auc_values[i], "\n")
}

# Calculer les métriques supplémentaires
precision <- diag(conf_matrix$table) / rowSums(conf_matrix$table)
recall <- diag(conf_matrix$table) / colSums(conf_matrix$table)
f1_score <- (2 * precision * recall) / (precision + recall)

cat("Précision: ", precision, "\n")
cat("Rappel: ", recall, "\n")
cat("Score F1: ", f1_score, "\n")

# Visualiser l'importance des variables
var_imp <- importance(rf_model_simple)
varImpPlot(rf_model_simple, main = "Importance des variables", col = "blue", pch = 16)


#######################
# MODELE OPTIMISE     #
#######################

# Appliquer SMOTE à l'ensemble d'entraînement
TrainData_balanced <- SMOTE(Risk_Level ~ T + TAV + SO2a, data = TrainData, perc.over = 300, perc.under = 200)

# Hyperparameter tuning
ntree_values <- seq(50, 500, by = 50)
mtry_values <- 1:3
nodesize_values <- c(1, 5, 10)
results <- data.frame(ntree = integer(), mtry = integer(), nodesize = integer(), Accuracy = numeric(), F1 = numeric(), Recall = numeric(), Precision = numeric())

for (ntree in ntree_values) {
  for (mtry in mtry_values) {
    for (nodesize in nodesize_values) {
      set.seed(123)
      rf_model <- randomForest(Risk_Level ~ T + TAV + SO2a, data=TrainData_balanced, ntree=ntree, mtry=mtry, nodesize=nodesize)
      
      # Faire des prédictions sur l'ensemble de test
      predictions <- predict(rf_model, newdata=TestData)
      
      # Évaluer les performances du modèle
      conf_matrix <- confusionMatrix(predictions, TestData$Risk_Level)
      accuracy <- conf_matrix$overall['Accuracy']
      f1 <- mean(conf_matrix$byClass['F1'])
      recall <- mean(conf_matrix$byClass['Recall'])
      precision <- mean(conf_matrix$byClass['Precision'])
      
      results <- rbind(results, data.frame(ntree = ntree, mtry = mtry, nodesize = nodesize, Accuracy = accuracy, F1 = f1, Recall = recall, Precision = precision))
    }
  }
}

# Sélectionner les meilleurs hyperparamètres basés sur la précision
best_params <- results[which.max(results$Accuracy), ]
best_ntree <- best_params$ntree
best_mtry <- best_params$mtry
best_nodesize <- best_params$nodesize

cat("Best ntree: ", best_ntree, "\n")
cat("Best mtry: ", best_mtry, "\n")
cat("Best nodesize: ", best_nodesize, "\n")

# Entraîner le modèle Random Forest avec les meilleurs hyperparamètres
rf_model_best <- randomForest(Risk_Level ~ T + TAV + SO2a, data=TrainData_balanced, ntree=best_ntree, mtry=best_mtry, nodesize=best_nodesize, localImp = TRUE)

# Faire des prédictions sur l'ensemble de test
predictions_best <- predict(rf_model_best, newdata=TestData)
predictions_prob_best <- predict(rf_model_best, newdata=TestData, type="prob")

# Évaluer les performances du modèle optimisé
conf_matrix_best <- confusionMatrix(predictions_best, TestData$Risk_Level)
accuracy_best <- conf_matrix_best$overall['Accuracy']
class_report_best <- conf_matrix_best$byClass

# Afficher les résultats
print(conf_matrix_best)
cat("Accuracy: ", accuracy_best, "\n")
print(class_report_best)

# Fonction pour créer une variable binaire pour une classe donnée
create_binary_labels <- function(labels, positive_class) {
  factor(ifelse(labels == positive_class, positive_class, paste0("Not_", positive_class)),
         levels = c(positive_class, paste0("Not_", positive_class)))
}

# Tracer les courbes ROC pour chaque classe
roc_risque0 <- roc(create_binary_labels(TestData$Risk_Level, "Risque 0"), predictions_prob_best[, "Risque 0"])
roc_risque1 <- roc(create_binary_labels(TestData$Risk_Level, "Risque 1"), predictions_prob_best[, "Risque 1"])
roc_risque2 <- roc(create_binary_labels(TestData$Risk_Level, "Risque 2"), predictions_prob_best[, "Risque 2"])

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

# Calculer les métriques supplémentaires : Précision, Rappel, Score F1 pour chaque classe
precision_0 <- posPredValue(factor(predictions_best == "Risque 0"), factor(TestData$Risk_Level == "Risque 0"))
precision_1 <- posPredValue(factor(predictions_best == "Risque 1"), factor(TestData$Risk_Level == "Risque 1"))
precision_2 <- posPredValue(factor(predictions_best == "Risque 2"), factor(TestData$Risk_Level == "Risque 2"))

recall_0 <- sensitivity(factor(predictions_best == "Risque 0"), factor(TestData$Risk_Level == "Risque 0"))
recall_1 <- sensitivity(factor(predictions_best == "Risque 1"), factor(TestData$Risk_Level == "Risque 1"))
recall_2 <- sensitivity(factor(predictions_best == "Risque 2"), factor(TestData$Risk_Level == "Risque 2"))

f1_score_0 <- (2 * precision_0 * recall_0) / (precision_0 + recall_0)
f1_score_1 <- (2 * precision_1 * recall_1) / (precision_1 + recall_1)
f1_score_2 <- (2 * precision_2 * recall_2) / (precision_2 + recall_2)

cat("Précision pour Risque 0: ", precision_0, "\n")
cat("Rappel pour Risque 0: ", recall_0, "\n")
cat("Score F1 pour Risque 0: ", f1_score_0, "\n")

cat("Précision pour Risque 1: ", precision_1, "\n")
cat("Rappel pour Risque 1: ", recall_1, "\n")
cat("Score F1 pour Risque 1: ", f1_score_1, "\n")

cat("Précision pour Risque 2: ", precision_2, "\n")
cat("Rappel pour Risque 2: ", recall_2, "\n")
cat("Score F1 pour Risque 2: ", f1_score_2, "\n")



reprtree:::plot.getTree(rf_model_simple, k=1, main="Modèle par défaut")
reprtree:::plot.getTree(rf_model_best, k=1, main="Modèle optimisé")