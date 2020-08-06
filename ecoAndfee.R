library(readxl)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)


billecono <- read_excel("C:/billandecono.xlsx")
rm(billecono)

billecono <- billandecono

str(billecono)

with(billecono, boxplot(total_count))

with(billecono, boxplot(commerce_count))

ggplot(billecono, aes(x=year, y=total_count)) +
  geom_line()

ggplot(economics, aes(x=date, y=psavert)) +
  geom_line()

data("economics")
head(economics)
#------------------------------------------------------------------------

# 데이터 저장 및 확인
eco <- bill
View(eco)





# 시각화를 통한 전반전인 데이터 분포 확인
a1 <- ggplot(eco, aes(x=year, y=total_amount)) +
  geom_line() +
  ggtitle("전체 미수금액") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

a2 <- ggplot(eco, aes(x=year, y=commerce_amount)) +
  geom_line() +
  ggtitle("일반용 미수금액") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

a3 <- ggplot(eco, aes(x=year, y=industry_amount)) +
  geom_line() +
  ggtitle("산업용 미수금액") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

a4 <- ggplot(eco, aes(x=year, y=-cli)) +
  geom_line() +
  ggtitle("경기선행지수") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

a5 <- ggplot(eco, aes(x=year, y=-cci)) +
  geom_line() +
  ggtitle("경기동행지수") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

a6 <- ggplot(eco, aes(x=year, y=-kospiavg)) +
  geom_line() +
  ggtitle("코스피지수") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

grid.arrange(a1,a2,a3,a4,a5,a6, ncol=2)


# 이상치 여부 확인 
par(mfrow=c(3,2))
par(mar = c(4,5,2,2))
boxplot(eco$total_amount, main="전체 미수금액")
boxplot(eco$commerce_amount, main="일반용 미수금액")
boxplot(eco$industry_amount, main="산업용 미수금액")
boxplot(eco$cli, main="경기선행지수")
boxplot(eco$cci, main="경기동행지수")
boxplot(eco$kospiavg, main="코스피지수")


# 미수금액과 경제지표의 관계 산점도로 확인
par(mfrow=c(3,3))
par(mar = c(4,5,2,2))
with(eco, plot(total_amount, cli, xlab="전체 미수금액", ylab="경기선행지수", cex.lab=1.5))
with(eco, plot(total_amount, cci, xlab="전체 미수금액", ylab="경기동행지수", cex.lab=1.5))
with(eco, plot(total_amount, kospiavg, xlab="전체 미수금액", ylab="코스피지수", cex.lab=1.5))

with(eco, plot(commerce_amount, cli, xlab="일반용 미수금액", ylab="경기선행지수", cex.lab=1.5))
with(eco, plot(commerce_amount, cci, xlab="일반용 미수금액", ylab="경기동행지수", cex.lab=1.5))
with(eco, plot(commerce_amount, kospiavg, xlab="일반용 미수금액", ylab="코스피지수", cex.lab=1.5))

with(eco, plot(industry_amount, cli, xlab="산업용 미수금액", ylab="경기선행지수", cex.lab=1.5))
with(eco, plot(industry_amount, cci, xlab="산업용 미수금액", ylab="경기동행지수", cex.lab=1.5))
with(eco, plot(industry_amount, kospiavg, xlab="산업용 미수금액", ylab="코스피지수", cex.lab=1.5))


# 미수금액과 경제지표의 상관관계 모형 생성(선형 회귀모형)
tot_cli <- lm(cli ~ total_amount, data=eco)
summary(tot_cli)
tot_cci <- lm(cci ~ total_amount, data=eco)
summary(tot_cci)
tot_kospiavg <- lm(kospiavg ~ total_amount, data=eco)
summary(tot_kospiavg)

com_cli <- lm(cli ~ commerce_amount, data=eco)
summary(com_cli)
com_cci <- lm(cci ~ commerce_amount, data=eco)
summary(com_cci)
com_kospiavg <- lm(kospiavg ~ commerce_amount, data=eco)
summary(com_kospiavg)

ind_cli <- lm(cli ~ industry_amount, data=eco)
summary(ind_cli)
ind_cci <- lm(cci ~ industry_amount, data=eco)
summary(ind_cci)
ind_kospiavg <- lm(kospiavg ~ industry_amount, data=eco)
summary(ind_kospiavg)


# 회귀모형 선분 그리기
par(mfrow=c(2,1))
par(mar = c(5,5,2,2))
with(eco, plot(x=industry_amount, y=cli, xlab="산업용 미수금액", ylab="경기선행지수"))
abline(ind_cli, lwd=3, col="red")

with(eco, plot(x=industry_amount, y=cci, xlab="산업용 미수금액", ylab="경기동행지수"))
abline(ind_cci, lwd=3, col="red")



# 잔차 정규분포 만족 확인
library(car)
par(mfrow=c(2,1))
qqPlot(ind_cli)
qqPlot(ind_cci)


# 실제값과 예측값 비교
plot(x=ind_cli$fitted.values, y=eco$cli, 
     xlab="예측 경기선행지수", ylab="실제 경기선행지수")
plot(x=ind_cci$fitted.values, y=eco$cci, 
     xlab="예측 경기동행지수", ylab="실제 경기동행지수")

# 데이터 오류율 확인
eco$cli_pred <- predict(ind_cli)
eco$cci_pred <- predict(ind_cci)

eco <- data.frame(eco)

ggplot(data=eco, aes(x=ind_cli, y=cli)) +
  geom_point() +
  geom_smooth(data=eco, method = lm)


eco$cli_error <- abs(eco$cli - eco$cli_pred) / eco$cli * 100
eco$cci_error <- abs(eco$cci - eco$cci_pred) / eco$cci * 100



View(eco)

eco <- data.frame(eco)
eco
install.packages("xlsx")
library(xlsx)
write.xlsx(eco, "eco2.xlsx")


install.packages("e1071")
library(caret)
confusionMatrix(eco$cli, eco$cli_pred)
