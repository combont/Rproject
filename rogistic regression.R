library(MASS)
data("biopsy")
str(biopsy)

biopsy$ID <- NULL

names(biopsy) <- c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
names(biopsy)

table(complete.cases(biopsy))

biopsy.v2 <- na.omit(biopsy)

y <- ifelse(biopsy.v2$class == "malignant", 1, 0)
biopsy.v2$y <- as.factor(y)
head(biopsy.v2)
# 분류문제 시각화
library(reshape2)
library(ggplot2)

biop.m <- melt(data = biopsy.v2, id.vars = "class")

head(biop.m)
tail(biop.m)

ggplot(data = biop.m, aes(x=class, y=value)) + geom_boxplot() +
  facet_wrap(~ variable, ncol = 3)

# 상관관계 분석
library(corrplot)
bc <- cor(biopsy.v2[, 1:9])
corrplot.mixed(bc)

# 훈련데이터와 테스트 데이터 분리
set.seed(123)
idx <- sample(2, NROW(biopsy.v2), replace = T, prob = c(0.7, 0.3))
train <- biopsy.v2[idx==1,]
test <- biopsy.v2[idx==2,]
str(test)
NROW(train)
NROW(test)

table(train$class)
table(test$class)

# 로지스틱 회귀모형 생성
full.fit <- glm(class ~., data=train, family = binomial)
summary(full.fit)

confint(full.fit)

exp(coef(full.fit))

library(car)
vif(full.fit)

train.probs <- predict(full.fit, type="response")
train.probs[1:5]
train.probs2 <- as.factor(ifelse(train.probs <= 0.5, 0, 1))

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(train$y, train.probs2)

install.packages("InformationValue")
library(InformationValue)
test.prob <- predict(full.fit, newdata = test, type = "response")
misClassError(test$y, test.prob)
confusionMatrix(test$y, test.prob)


# 교차검증을 포함한 로지스틱 회귀 
install.packages("bestglm")
library(bestglm)
str(train)
train$y <- as.numeric(train$y)
train$y <- train$y - 1
head(train)
Xy <- train[, 1:9]
Xy <- data.frame(cbind(Xy, train$y))

bestglm(Xy = Xy, IC="CV", CVArgs = list(Method="HTF", K=10, REP=1),
        family = binomial)

reduce.fit <- glm(class ~ thick + u.size + nucl, family = binomial,
                  data = train)

test.cv.probs <- predict(reduce.fit, newdata = test, type = "response")
misClassError(test$y, test.cv.probs)
confusionMatrix(test$y, test.cv.probs)

# BIC를 통한 로지스틱 회귀 
bestglm(Xy=Xy, IC="BIC", family = binomial)

bic.fit <- glm(class ~ thick + adhsn + nucl + n.nuc, family = binomial, data=train)
test.bic.probs <- predict(bic.fit, newdata=test, type = "response")
misClassError(test$y, test.bic.probs)
confusionMatrix(test$y, test.bic.probs)


# 판별분석 적용
train2 <- train
train2$y <- NULL

lda.fit <- lda(class ~., data=train2)
lda.fit

plot(lda.fit, type="both")

# 관찰값이 악성일 확률 추출 및 오류율 확인
train.lda.probs <- predict(lda.fit)$posterior[,2]
train.lda.probs
misClassError(train$y, train.lda.probs)
confusionMatrix(train$y, train.lda.probs)

# 훈련데이터에서 모델 성능 평가
test.lda.probs <- predict(lda.fit, newdata = test)$posterior[,2]
misClassError(test$y, test.lda.probs)
confusionMatrix(test$y, test.lda.probs)


# 이차 판별분석
qda.fit <- qda(class ~., data=train2)
qda.fit

train.qda.probs <- predict(qda.fit)$posterior[,2]
misClassError(train$y, train.qda.probs)
confusionMatrix(train$y, train.qda.probs)

test.qda.probs <- predict(qda.fit, newdata = test)$posterior[,2]
misClassError(test$y, test.qda.probs)
confusionMatrix(test$y, test.qda.probs)
