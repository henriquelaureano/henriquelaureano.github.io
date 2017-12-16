# ================================================================= #
#               Codigos - Lista 3 - est171 - Aprendizado de Maquina #
#                                       Departamento de Estatistica #
#                              Universidade Federal de Minas Gerais #
#                                                                   #
#   Henrique Aparecido Laureano e Magno Tairone de Freitas Severino #
#                                                   Outubro de 2016 #
# ================================================================= #

## Dados ========================================================= ##
setwd("~/Downloads")
mydata <- read.csv("train.csv", header = TRUE) 
test <- read.csv("test.csv", header = TRUE)

mydata[ , 1] <- as.factor(mydata[ , 1])

random <- sample(1:dim(mydata)[1])
treinamento <- random[1:round(.8 * dim(mydata)[1])]
validacao <- random[(round(.8 * dim(mydata)[1]) + 1):dim(mydata)[1]]

## Plotando as figuras =========================================== ##
library(jpeg)

view <- function(row){
  c <- 1
  m <- matrix(NA, 28, 28)
  for (i in 1:28){
    for (j in 1:28){
      m[i, j] <- train[row, 1 + c]
      c <- c + 1}}
  return(t(apply(m, 2, rev)))}

par(mfrow = c(2, 3), mar = c(2, 2, 2, 2) + .1)

for (i in 1:5) image(view(i), col = grey(c(0:255)/256), axes = FALSE)

## Bagging ======================================================= ##
library(randomForest)

t1 <- Sys.time() ; ajuste <- randomForest(
  x = mydata[treinamento, -1], y = mydata[treinamento, 1]
  , mtry = dim(mydata)[2] - 1
  , importance = FALSE) ; t2 <- Sys.time()
t2 - t1 ## Time difference of 1.230357 hours

varImpPlot(ajuste)
predito <- predict(ajuste, newdata = mydata[validacao, ])
mean(predito != mydata[validacao, 1]) ## RISCO BAGGING .0834
plot(ajuste)

predito.test <- predict(ajuste, newdata = test)

## Arvore de Classificacao ======================================= ##
library(tree)

formula <- paste("label ~ "
                 , paste0("pixel", 0:782, "+", collapse = ""))
formula <- paste0(formula, "pixel783")

t1 <- Sys.time() ; ajuste <-
  tree(formula, data = mydata[treinamento, ]) ; t2 <- Sys.time()
t2 - t1 ## Time difference of 14.38227 secs

plot(ajuste)
text(ajuste, pretty = 0)
vcErro <- cv.tree(ajuste, FUN = prune.tree)
plot(vcErro)
bestSize <- vcErro$size[which.min(vcErro$dev)]

ajusteVC <- prune.tree(ajuste, best = bestSize)
plot(ajusteVC)
text(ajusteVC, pretty = 0)

## Apos poda, resultado e a mesma arvore inicial, com 14 nodos
par(mfrow = c(1, 2))
plot(ajuste)
text(ajuste, pretty = 0)
plot(ajusteVC)
text(ajusteVC, pretty = 0)

tree.pred <- predict(ajusteVC , xvalidation , type = "class")
mean(tree.pred != yvalidation) ## RISCO ARVORE .3481

tree.pred.test <- predict(ajusteVC, test, type = "class")

## Boosting ====================================================== ##
library(bst)
library(adabag)

t1 <- Sys.time() ; ajuste.bst <-
  boosting(label ~ ., data = mydata[treinamento, ]
           , control = rpart.control(maxdepth=6)) ; t2 <- Sys.time()
t2 - t1 ## Time difference of 1.408647 hours

bst.pred <-
  predict.boosting(ajuste.bst, newdata = mydata[validacao, ])

mean(bst.pred$class != mydata[validacao, 1]) ## RISCO BOOSTING .206

bst.pred.test <- predict.boosting(ajuste.bst, newdata = test)

## SVM =========================================================== ##
library (e1071)

t1 <- Sys.time()
ajuste.svm <- svm(label ~ ., data = mydata[treinamento, ]
                  , type = "C-classification"
                  , kernel = "linear") ; t2 <- Sys.time()
t2 - t1 ## Time difference of 20.76242 mins

ypred <- predict(ajuste.svm, mydata[validacao, -1])
mean(ypred != mydata[validacao, 1]) ## RISCO SVM LINEAR .1086905

ypred <- predict(ajuste.svm, test)

## KNN =========================================================== ##
library(class)

## k = 1
t1 <- Sys.time() ; reg.knn <- knn(
  train = mydata[treinamento, -1], test = mydata[validacao, -1]
  , cl = mydata[treinamento, 1], prob = FALSE) ; t2 <- Sys.time()
t2 - t1 ## Time difference of 12.86209 mins

mean(reg.knn != mydata[validacao, 1]) ## RISCO KNN k = 1 .03380952

## K = 3
t1 <- Sys.time() ; reg.knn = knn(
  train = mydata[treinamento, -1], test = mydata[validacao, -1]
  , cl = mydata[treinamento, 1], prob = FALSE
  , k = 3) ; t2 <- Sys.time()
t2 - t1 ## Time difference of 33.80355 mins

mean(reg.knn != mydata[validacao, 1]) ## RISCO KNN k = 3 .03642857

# ================================================================= #
#                                                           Fim = ) #
# ================================================================= #