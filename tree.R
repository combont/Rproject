install.packages("partykit")
install.packages("ElemStatLearn")
install.packages("randomForest")
install.packages("xgboost")

library(rpart)  # 분류 및 회귀트리
library(partykit)  # 트리출력
library(MASS)  # 유방암 및 피마 인디언 데이터
library(ElemStatLearn)  # 전립선 암 데이터
library(randomForest) # 랜덤 포레스트
library(xgboost)  # 그레디언트 부스트
library(caret)  # 하이퍼파라미터 조정

data(prostate)
head(prostate)

prostate$gleason <- ifelse(prostate$gleason == 6, 0, 1)
pros.train <- subset(prostate, train == T)[, 1:9]
pros.test <- subset(prostate, train == F)[, 1:9]

tree.pros <- rpart(lpsa ~., data= pros.train)

print(tree.pros$cptable)

plotcp(tree.pros)

cp <- min(tree.pros$cptable[5, ])
prune.tree.pros <- prune(tree.pros, cp = cp)

plot(as.party(tree.pros))

plot(as.party(prune.tree.pros))

party.pros.test <- predict(prune.tree.pros, newdata = pros.test)
rpart.resid <- party.pros.test - pros.test$lpsa
mean(rpart.resid^2)

#------------------------------------------------------------
# classification tree

data("biopsy")
head(biopsy)

biopsy <- biopsy[, -1] # ID 삭제 
names(biopsy) <- c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")  # 피처 이름 변경 
biopsy.v2 <- na.omit(biopsy)  # 값이 없는 관찰값 제거 
set.seed(123)  # 난수발생기 초기화
ind <- sample(2, nrow(biopsy.v2), replace = T, prob=c(0.7,0.3))
biop.train <- biopsy.v2[ind==1,]  # 학습 데이터 세트 
biop.test <- biopsy.v2[ind==2, ]  # 테스트 데이터 세트 

set.seed(123)
tree.biop <- rpart(class ~ ., data=biop.test)
tree.biop$cptable

cp <- min(tree.biop$cptable[3,])
prune.tree.biop <- prune(tree.biop, cp=cp)
plot(as.party(prune.tree.biop))

rparty.test <- predict(prune.tree.biop, newdata=biop.test, type="class")
table(rparty.test, biop.test$class)
(139+60)/209

#-------------------------------------------------------------
# random Forest
set.seed(123)
rf.pros <- randomForest(lpsa ~., data=pros.train)
rf.pros

plot(rf.pros)

which.min(rf.pros$mse)

set.seed(123)
rf.pros.2 <- randomForest(lpsa ~., data=pros.train, ntree=80)
rf.pros.2

varImpPlot(rf.pros.2, scale=T, main="Variable Importance Plot - PSA Score")

importance(rf.pros.2)

rf.pros.test <- predict(rf.pros.2, newdata=pros.test)
rf.resid <- rf.pros.test - pros.test$lpsa
mean(rf.resid^2)


set.seed(123)
rf.biop <- randomForest(class ~., data=biop.train)
rf.biop

plot(rf.biop)

which.min(rf.biop$err.rate[,1])

set.seed(123)
rf.biop.2 <- randomForest(class~., data=biop.train, ntree=125)
print(rf.biop.2)

rf.biop.test <- predict(rf.biop.2, newdata=biop.test, type="response")
table(rf.biop.test, biop.test$class)
(138+67)/209

varImpPlot(rf.biop.2)

#----------------------------------------------------------------
# pima indian random forest
data(Pima.tr)
data(Pima.te)
head(Pima.tr)

pima <- rbind(Pima.tr, Pima.te)
set.seed(502)
ind <- sample(2, nrow(pima), replace = T, prob=c(0.7, 0.3))
pima.train <- pima[ind==1,]
pima.test <- pima[ind==2,]

set.seed(321)
rf.pima <- randomForest(type~., data=pima.train)
rf.pima

which.min(rf.pima$err.rate[, 1])
set.seed(321)
rf.pima.2 <- randomForest(type~., data=pima.train, ntree=88)
print(rf.pima.2)

rf.pima.test <- predict(rf.pima.2, newdata=pima.test, type="response")
table(rf.pima.test, pima.test$type)
(74+37)/147

grid <- expand.grid(nrounds=c(75,100), colsample_bytree=1, min_child_weight=1,
                    eta=c(0.01,0.1,0.3), gamma=c(0.5,0.25), subsample=0.5, max_depth=c(2,3))

cntrl <- trainControl(method="cv", number=5, verboseIter=T, returnData=F, returnResamp="final")

set.seed(1)
train.xgb <- train(x=pima.train[, 1:7], y=pima.train[, 8], trControl=cntrl, tuneGrid=grid, method="xgbTree")

train.xgb

param <- list(objective = "binary:logistic", booster="gbtree", eval_metric="error",
              eta=0.1, max_depth=2, subsampe=0.5, colsample_bytree=1, gamma=0.5)
x <- as.matrix(pima.train[, 1:7])
y <- ifelse(pima.train$type == "Yes", 1, 0)
train.mat <- xgb.DMatrix(data=x, label=y)

set.seed(1)
xgb.fit <- xgb.train(params = param, data=train.mat, nrounds=75)

impMatrix <- xgb.importance(feature_names = dimnames(x)[[2]], model=xgb.fit)
impMatrix

xgb.plot.importance(impMatrix, main="Gain by Feature")

library(InformationValue)
pred <- predict(xgb.fit, x)
optimalCutoff(y, pred)
pima.testMat <- as.matrix(pima.test[, 1:7])
xgb.pima.test <- predict(xgb.fit, pima.testMat)
y.test <- ifelse(pima.test$type == "Yes", 1, 0)
confusionMatrix(y.test, xgb.pima.test, threshold = 0.39)
1 - misClassError(y.test, xgb.pima.test, threshold = 0.39)

plotROC(y.test, xgb.pima.test)

#------------------------------------------------------------
# feature selection by random forrest
install.packages("mlbench")
data(Sonar, package = "mlbench")
dim(Sonar)
table(Sonar$Class)

install.packages("Boruta")
library(Boruta)
set.seed(1)
feature.selection <- Boruta(Class ~., data=Sonar, doTrace=1)

feature.selection$timeTaken

table(feature.selection$finalDecision)

fNames <- getSelectedAttributes(feature.selection)
fNames

Sonar.features <- Sonar[, fNames]
dim(Sonar.features)
