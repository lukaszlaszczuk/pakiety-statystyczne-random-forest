library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(randomForest)
library(MLmetrics)
library(ranger)
library(mlr)
library(rpart)
library(rpart.plot)

#---------funkcje pomocnicze-------------------------#
df_change_types <- function(df, target) {
  char_cols <- sapply(df, class) == "character"
  df[, char_cols] <- as.data.frame(lapply(df[, char_cols], as.factor))
  df[, target] <- as.factor(c(df[, target])[[1]])
  df
}

test_train_split <- function(df, target_var, proportion) {
  train.index <- createDataPartition(c(df[, target_var])[[1]], p = proportion, list = FALSE)
  # Podział stratified test-train split
  df_train <- df[train.index, ]
  x_train <- df_train %>% select(-target_var)
  y_train <- df_train %>% select(target_var)
  df_test <- df[-train.index, ]
  x_test <- df_test %>% select(-target_var)
  y_test <- df_test %>% select(target_var)
  y_test <- c(y_test)[[1]]
  list(df_train, x_test, y_test)
}

set_threshold <- function(y_proba, y_test) {
  vec <- c()
  for (threshold in seq(0.01,1,0.01)) {
    y_pred <- factor(as.numeric((y_proba > threshold)[,2]))
    r1 <- model_evaluate(y_test, y_pred)
    f1 <- r1$byClass["F1"]
    vec <- append(vec, f1)
  }
  results <- data.frame(seq(0.01, 1, 0.01), vec)
  threshold <- results[which.max(results$vec), 1]
  threshold
}

ohe_column <- function(df, feature){
  for(unique_value in unique(df[, feature])){
    df[paste(feature, substr(unique_value,1,3), sep = ".")] <- ifelse(df[, feature] == as.character(unique_value), 1, 0)
  }
  df <- df %>% select(-feature)
  df
}

model_evaluate <- function(y_true, y_pred) {
  c_mat <- confusionMatrix(y_pred, y_true, positive="1", mode = "prec_recall")
  c_mat
}

create_fi_df <- function(tree_model) {
  s <- summary(tree_model)
  importances_df <- as.data.frame(s$variable.importance)
  importances_df$names <- rownames(importances_df)
  colnames(importances_df) <- c("importances", "names")
  importances_df
}

#---------decision tree--------------#
set.seed(41)
df <- read_csv("data/bank.csv")
df <- df_change_types(df, 'y')  # some data processing
# df$previous <- cut(df$previous, breaks=c(-1,0,1,3,10,max(df$previous)), labels = c("no calls", "1 call", "2 or 3 calls","4 to 10 calls",'hot line'))
# df$pdays <- cut(df$pdays, breaks=c(-2,0,100,max(df$pdays)), labels = c("none", "to_100_days", "above_100_days"))
# df$balance <- log10(df$balance + (abs(min(df$balance))+1))
# df$duration <- as.vector(scale(df$duration))
# df$age <- as.vector(scale(df$age))
df_splits <- test_train_split(df, 'y', 0.75)
df_train_1 <- df_splits[[1]]
x_test <- df_splits[[2]]
y_test <- df_splits[[3]]

#---------przeuczone drzewo------------#
tree <- rpart(y ~ ., data = df_train, cp = 0, minsplit=1)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
y_proba <- predict(tree, newdata = df_train[,-17])
y_pred <- factor(as.numeric((y_proba > 0.5)[,2]))
y_train <- c(df_train[,17])[[1]]
r <- model_evaluate(y_train, y_pred)
e_1_tr <- r$byClass["F1"]  # F1 score na zbiorze treningowym


y_proba <- predict(tree, newdata = x_test)
y_pred <- factor(as.numeric((y_proba > 0.5)[,2]))
r1 <- model_evaluate(y_test, y_pred)
e_1_te <- r1$byClass["F1"]

#--------drzewo regularyzowane-----------#
tree <- rpart(y ~ ., data = df_train)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
y_proba <- predict(tree, newdata = df_train[,-17])
y_train <- c(df_train[,17])[[1]]
threshold <- set_threshold(y_proba, y_train)
y_pred <- factor(as.numeric((y_proba > threshold)[,2]))

r <- model_evaluate(y_train, y_pred)
e_2_tr <- r$byClass["F1"]  # F1 score na zbiorze treningowym

y_proba <- predict(tree, newdata = x_test)
y_pred <- factor(as.numeric((y_proba > threshold)[,2]))
r1 <- model_evaluate(y_test, y_pred)


#---------------------------------------#

importances_df <- create_fi_df(tree_model = tree)
ggplot(importances_df, mapping = aes(x = names, y = importances, fill = importances)) + geom_col()

rpart.rules(tree)  # decyzje drzewa jako char

#----------random forest-------------#
#------------------ranger--------------#
rf <- ranger(y ~ ., df_train, classification = TRUE, class.weights = c(1, 9), probability = TRUE,
            importance = "permutation")
y_proba <- predictions(predict(rf, df_train[,-17]))
y_train <- c(df_train[,17])[[1]]
threshold <- set_threshold(y_proba, y_train)
y_pred <- factor(as.numeric((y_proba > threshold)[,2]))

r <- model_evaluate(y_train, y_pred)
ranger_tr <- r$byClass["F1"]  # F1 score na zbiorze treningowym

y_proba <- predictions(predict(rf, x_test))
y_pred <- factor(as.numeric((y_proba > threshold)[,2]))
r1 <- model_evaluate(y_test, y_pred)
ranger_te <- r1$byClass["F1"]
print(ranger_tr)
print(ranger_te)

fi <- rf$variable.importance
importances_df <- as.data.frame(fi)
importances_df$names <- rownames(importances_df)
colnames(importances_df) <- c("importances", "names")
ggplot(importances_df, aes(x=names, y=importances, fill=importances)) + geom_col()
#------------------------------------#


#------------parameter tuning in mlr (optymalizacja rangera)--------------------#
task <- makeClassifTask(id = 'df_train',
                        data = df_train,
                        target = 'y',
                        positive = "1")

learner <- makeLearner('classif.ranger', predict.type = "prob", importance = "permutation")
learner_1 <- makeWeightedClassesWrapper(learner, wcw.weight = 9)
rdesc <- makeResampleDesc("CV", iters = 5)

ps <- makeParamSet(makeDiscreteParam("mtry", seq(1, 7, 2)),
                   makeDiscreteParam('min.node.size', seq(1,20,3)),
                   makeDiscreteParam("num.trees", c(100, 200, 500, 1000)))

ctrl <- makeTuneControlRandom(maxit = 20L, tune.threshold = TRUE)

res <- tuneParams(learner_1, task, rdesc, par.set = ps,
                  control = ctrl)
res$x  # best params
lrn <- setHyperPars(learner_1, par.vals = res$x)
m <- train(lrn, task)

fi <- getFeatureImportanceLearner(lrn, m)
importances_df <- as.data.frame(fi)
importances_df$names <- rownames(importances_df)
colnames(importances_df) <- c("importances", "names")
importances_df
ggplot(importances_df, aes(x=names, y=importances, fill=importances)) + geom_col()

y_pred <- predict(m, newdata = df_train[,-17])
y_proba <- y_pred$data[,1:2]
y_pred_bin <- factor(as.numeric((y_proba > 0.5)[,2]))
ranger_optimized <- model_evaluate(y_pred_bin, c(df_train[,17])[[1]])
ranger_optimized_tr <- ranger_optimized$byClass["F1"]
print(ranger_optimized_tr)

y_pred <- predict(m, newdata = x_test)
y_proba <- y_pred$data[,1:2]


y_pred_bin <- factor(as.numeric((y_proba > 0.5)[,2]))

resu <- model_evaluate(y_test, y_pred_bin)
print(resu)
save(m, file="ranger_model.RData")

#------------------compare with h2o automl model------------------------------------------------------#
library(h2o)
h2o.init()

####--uncomment to build a model with one hot encoded categorical features--------#
# ohe_columns <- c('job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 'pdays', 'month', 'poutcome', 'previous')
# for (column in ohe_columns) {
#   df <- ohe_column(df, column)
# }

df <- as.h2o(df)
# df[, 7:57] <- as.factor(df[, 7:57])
df$y <- as.factor(df$y)
h2o.getTypes(df)

df.split <- h2o.splitFrame(df, ratios=0.75, seed = 41)
df.train <- df.split[[1]]
df.test <- df.split[[2]]
aml <- h2o.automl(y = "y",
                  training_frame = df.train,
                  nfolds = 10,
                  balance_classes = TRUE,
                  max_models = 10,
                  sort_metric = "AUCPR",
                  verbosity = "info")
best_model <- aml@leader

save(best_model, file="h2o_leader.RData")

test_colidx <- grep('^y$', colnames(df))
x_test <- df.test[, -test_colidx]
y_test <- df.test[, test_colidx]

y_test <- c(as.data.frame(y_test))[[1]]
y_pred <- h2o.predict(best_model, x_test)[,1]
y_pred <- c(as.data.frame(y_pred))[[1]]

evaluate_aml <- model_evaluate(y_test, y_pred)
evaluate_aml$byClass["F1"]
