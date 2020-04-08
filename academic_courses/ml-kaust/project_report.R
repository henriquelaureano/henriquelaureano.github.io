## ----setup, include=FALSE-----------------------------------------------------
# <r code> ===================================================================== #
library(knitr)

tema <- knit_theme$get("acid")

knit_theme$set(tema)

opts_chunk$set(size='small'
               , cache=TRUE
               , cache.path='cache/'
               , comment=NA
               , warning=FALSE
               , message=FALSE
               , fig.align='center'
               , dpi=100
               , fig.path='iBagens/'
               , fig.pos='H'
               , background='#ffffff'
               , results='hold'
               , fig.show='hold'
               , echo=FALSE)
# </r code> ==================================================================== #


## -----------------------------------------------------------------------------
# <r code> ===================================================================== #
path <- "~/Dropbox/KAUST/machine_learning/project/report/"

df <- read.csv(paste0(path, "bank-additional-full.csv"), header = TRUE, sep = ";")
df <- df[ , -c(5, 11, 13)]

id <- sample(1:nrow(df), round(nrow(df) / 10, 0))

train <- df[-id, ] ; test <- df[id, ]
# </r code> ==================================================================== #


## ----packages-----------------------------------------------------------------
# <r code> ===================================================================== #
library(MASS)
library(e1071)
library(klaR)
library(FNN)
library(randomForest)
library(rpart)
library(partykit)
library(pROC)
library(latticeExtra)
# </r code> ==================================================================== #


## ----pairs, fig.width=6.075, fig.height=6.075, fig.cap="Scatterplot lower triangular matrix and correlation upper triangular matrix for all the quantitative features presented in the Bank Marketing Dataset (BMD)."----
# <r code> ===================================================================== #
quant <- df[ , c(1, 10, 11, 13:17)]
pairs(quant
      , upper.panel = function(x, y, ...) {
        usr <- par("usr") ; on.exit(par(usr)) ; par(usr = c(0, 1, 0, 1))
        r <- cor(x, y)
        txt <- format(c(r, 0.123456789), digits = 2)[1]
        text(.5, .5, txt, cex = 1.25)
      }
      , diag.panel = function(a, b, ...) {
        usr <- par("usr") ; on.exit(par(usr)) ; par(usr = c(0, 1, 0, 1))
        rect(0, 0, 1, 1, col = "#37B9DA")
      }
      , pch = 19, gap = .25, xaxt = "n", yaxt = "n"
      , col = c("#0080FF", "#F3953E")[df$y]
      , label.pos = .5, oma = c(1, 1, 1, 1))
# </r code> ==================================================================== #


## ----barras, fig.width=10, fig.height=13.7, fig.cap="Bar plots for all the qualitative features presented in the Bank Marketing Dataset (BMD)."----
# <r code> ===================================================================== #
barras <- function(variable, nome, limite) {
  da = table(variable)
  barchart(sort(da)
           , col = "orange"
           , border = "transparent"
           , xlab = NULL
           , main = nome
           , scales = list(x = list(draw = FALSE))
           , xlim = limite
           , panel = function(...){
             panel.barchart(...)
             args <- list(...)
             panel.text(args$x, args$y, args$x, pos = 4, cex = .8)})
}
print(barras(df$job, "job", c(0, 15e3)),
      position = c(0, 3/4, 1/3, 1), more = TRUE)
print(barras(df$marital, "marital", c(0, 33e3)),
      position = c(1/3, 3/4, 2/3, 1), more = TRUE)
print(barras(df$education, "education", c(0, 19e3)),
      position = c(2/3, 3/4, 1, 1), more = TRUE)
print(barras(df$housing, "housing", c(0, 28e3)),
      position = c(0, 2/4, 1/3, 3/4), more = TRUE)
print(barras(df$loan, "loan", c(0, 44e3)),
      position = c(1/3, 2/4, 2/3, 3/4), more = TRUE)
print(barras(df$contact, "contact", c(0, 35e3)),
      position = c(2/3, 2/4, 1, 3/4), more = TRUE)
print(barras(df$month, "month", c(0, 18e3)),
      position = c(0, 1/4, 1/3, 2/4), more = TRUE)
print(barras(df$day_of_week, "day_of_week", c(0, 11e3)),
      position = c(1/3, 1/4, 2/3, 2/4), more = TRUE)
print(barras(df$poutcome, "poutcome", c(0, 47e3)),
      position = c(2/3, 1/4, 1, 2/4), more = TRUE)
print(barras(df$y, "y", c(0, 46e3)),
      position = c(1/3, 0, 2/3, 1/4))
# </r code> ==================================================================== #


## ----cache.path="cache_logistic/", results='hide', dependson="packages"-------
# <r code> ===================================================================== #
# ---------------------------------------------------------- logistic regression #
logist <- glm(y ~ ., family = binomial, train) ; logist <- stepAIC(logist)
# </r code> ==================================================================== #


## ----cache.path="cache_probit/", results='hide', dependson="packages"---------
# <r code> ===================================================================== #
# ------------------------------------------------------------ probit regression #
probit <- glm(y ~ ., family = binomial(link = "probit"), train)
probit <- stepAIC(probit)
# </r code> ==================================================================== #


## ----cache.path="cache_cauchit/", results='hide', dependson="packages"--------
# <r code> ===================================================================== #
# ----------------------------------------------------------- cauchit regression #
cauchit <- glm(y ~ ., family = binomial(link = "cauchit"), train)
cauchit <- stepAIC(cauchit)
# </r code> ==================================================================== #


## ----cache.path="cache_cloglog/", results='hide', dependson="packages"--------
# <r code> ===================================================================== #
# --------------------------------------------- complementary log-log regression #
cloglog <- glm(y ~ ., family = binomial(link = "cloglog"), train)
cloglog <- stepAIC(cloglog)
# </r code> ==================================================================== #


## ----cache.path="cache_lm/", results='hide', dependson="packages"-------------
# <r code> ===================================================================== #
# ------------------------------------------------------------ linear regression #
linear <- lm(as.numeric(y) ~ ., train) ; linear <- stepAIC(linear)
# </r code> ==================================================================== #


## -----------------------------------------------------------------------------
# <r code> ===================================================================== #
# ------------------------------------------------------------------ naive bayes #
naive <- naiveBayes(y ~ ., train)
# </r code> ==================================================================== #


## -----------------------------------------------------------------------------
# <r code> ===================================================================== #
# ------------------------------------------------- linear discriminant analysis #
lda.model <- lda(y ~ ., train)
# </r code> ==================================================================== #


## -----------------------------------------------------------------------------
# <r code> ===================================================================== #
# ---------------------------------------------- quadratic discriminant analysis #
qda.model <- qda(logist$formula, train)
# </r code> ==================================================================== #


## ----cache.path="cache_rda/", dependson="packages"----------------------------
# <r code> ===================================================================== #
# -------------------------------------------- regularized discriminant analysis #
rda.model <- rda(y ~ ., train)
# </r code> ==================================================================== #


## ----cache.path="cache_svm-linear/", dependson="packages"---------------------
# <r code> ===================================================================== #
# ------------------------------------------------ linear support vector machine #
svm.linear <- svm(y ~ ., kernel = "linear", train)
# </r code> ==================================================================== #


## ----cache.path="cache_svm-poly/", dependson="packages"-----------------------
# <r code> ===================================================================== #
# -------------------------------------------- polynomial support vector machine #
svm.poly <- svm(y ~ ., kernel = "polynomial", train)
# </r code> ==================================================================== #


## ----cache.path="cache_svm-radial/", dependson="packages"---------------------
# <r code> ===================================================================== #
# ------------------------------------------------ radial support vector machine #
svm.rad <- svm(y ~ ., kernel = "radial", train)
# </r code> ==================================================================== #


## ----cache.path="cache_svm-sigmoid/", dependson="packages"--------------------
# <r code> ===================================================================== #
# ----------------------------------------------- sigmoid support vector machine #
svm.sigm <- svm(y ~ ., kernel = "sigmoid", train)
# </r code> ==================================================================== #


## ----cache.path="cache_bagging/", dependson="packages"------------------------
# <r code> ===================================================================== #
# ---------------------------------------------------------------- random forest #
bag <- randomForest(x = train[, -18], y = train[ , 18], importance = FALSE)
# </r code> ==================================================================== #


## -----------------------------------------------------------------------------
# <r code> ===================================================================== #
# ---------------------------------------------------------- classification tree #
arvore <- rpart(y ~ ., train) ; arvore <- as.party(arvore)
# </r code> ==================================================================== #


## ----aucs, fig.height=9.5, fig.width=6.5, fig.cap="ROC curve for each model (in the test) with respective AUC and thresholds."----
# <r code> ===================================================================== #
par(mfrow = c(5, 3))
# ---------------------------------------------------------- logistic regression #
plot.roc(roc(test$y, predict(logist, test, type = "response"))
         , main = "Logistic"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ------------------------------------------------------------ probit regression #
plot.roc(roc(test$y, predict(probit, test, type = "response"))
         , main = "Probit"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ----------------------------------------------------------- cauchit regression #
plot.roc(roc(test$y, predict(cauchit, test, type = "response"))
         , main = "Cauchit"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# --------------------------------------------- complementary log-log regression #
plot.roc(roc(test$y, predict(cloglog, test, type = "response"))
         , main = "Comp. log-log"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ------------------------------------------------------------ linear regression #
plot.roc(roc(test$y, predict(linear, test, type = "response"))
         , main = "Least squares"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ------------------------------------------------------------------ naive bayes #
plot.roc(roc(test$y, as.numeric(predict(naive, test)))
         , main = "Naive Bayes"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ------------------------------------------------- linear discriminant analysis #
plot.roc(roc(test$y, as.numeric(predict(lda.model, test)$class))
         , main = "Linear discriminant"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ---------------------------------------------- quadratic discriminant analysis #
plot.roc(roc(test$y, as.numeric(predict(qda.model, test)$class))
         , main = "Quadratic discriminant"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# -------------------------------------------- regularized discriminant analysis #
plot.roc(roc(test$y, as.numeric(predict(rda.model, test)$class))
         , main = "Regularized discriminant"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ------------------------------------------------ linear support vector machine #
plot.roc(roc(test$y, as.numeric(predict(svm.linear, test)))
         , main = "Linear SVM"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# -------------------------------------------- polynomial support vector machine #
plot.roc(roc(test$y, as.numeric(predict(svm.poly, test)))
         , main = "Polynomial SVM"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ------------------------------------------------ radial support vector machine #
plot.roc(roc(test$y, as.numeric(predict(svm.rad, test)))
         , main = "Radial SVM"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ----------------------------------------------- sigmoid support vector machine #
plot.roc(roc(test$y, as.numeric(predict(svm.sigm, test)))
         , main = "Sigmoid SVM"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ------------------------------------------------------------ --- random forest #
plot.roc(roc(test$y, as.numeric(predict(bag, test)))
         , main = "Random forest"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# ---------------------------------------------------------- classification tree #
plot.roc(roc(test$y, as.numeric(predict(arvore, test)))
         , main = "Decision tree"
         #, print.thres = TRUE
         , print.auc = TRUE, print.auc.cex = 1.25, print.auc.adj = c(.5, 3)
         , auc.polygon = TRUE, max.auc.polygon = TRUE)
# </r code> ==================================================================== #

