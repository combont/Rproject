install.packages("readxl")
library(readxl)

custcharg5_ex <- read_excel("C:/Rstudy/custcharge5.xlsx")


str(custcharge5)


table(custcharge5$cons_cost)


NROW(charge.train)

NROW(charge.test)

table(charge.train$cons_cost)
table(charge.test$cons_cost)


set.seed(123)
rf.charge <- randomForest(cons_cost ~., data=charge.train)
rf.charge

plot(rf.charge)

which.min(rf.charge$err.rate[,1])

set.seed(123)
rf.charge2 <- randomForest(cons_cost ~., data=charge.train, ntree=436)
rf.charge2

rf.charge.test <- predict(rf.charge2, newdata=charge.test, type="response")
table(rf.charge.test, charge.test$cons_cost)
(7514+2679+3676) / 15330



rf_charge.test_table <- table(rf.charge.test, charge.test$cons_cost)
rf_charge.test_table$correct <- rf_charge.test_table$1 + rf_charge.test_table$2 + rf_charge.test_table$3

names(rf_charge.test_table) <- c("a", "b", "c")
rf_charge.test_table <- data.frame(rf_charge.test_table)
rf_charge.test_table

표준시설부담금 <- c(7514, 22, 191)
설계시설부담금 <- c(221, 2679, 449)
설계조정시설부담금 <- c(327, 251, 3676)
dataframe_ex <- data.frame(표준시설부담금, 설계시설부담금, 설계조정시설부담금)

rownames(dataframe_ex) <- c("표준", "설계", "설계조정")


dataframe_ex$정확도 <- c(0.9320, 0.9075, 0.8517)

dataframe_ex
varImpPlot(rf.charge2)

plot(rf.charge.test)
