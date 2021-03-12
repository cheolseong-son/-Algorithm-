# 담배값 인상전,후의 월별 매출액
# 귀무 가설 : 담배값 인상과 흡연은 관계가 없다.
# 대립 가설 : 담배값 인상과 흡연은 관계가 있다.


x <- c(70,72,62,64,71,76,0,65,75,72)
y <- c(70,74,65,68,72,74,61,66,76,75)

cor(x, y, method = "pearson")
cor(x, y, method = "spearman")
cor(x, y, method = 'kendal')

# 값이 애매할 때 가설 검정해야 한다.
cor.test(x, y, method = "pearson") 
# p-value = 0.008216 : 가격인상과 흡연은 상관관계가 있다.

# ========== 실 습 2 ===========
mydata <- read.csv("../data/cor.csv")
View(mydata)
# pop_growth : 인구 증가율
# elderly_rate : 65세 이상 노령인구 비율
# finance : 재정 자립도
# cultural_center : 인구 10만명당 문화 기반 시설 수

plot(mydata$pop_growth, mydata$elderly_rate) # 산포도 그래프

cor(mydata$pop_growth, mydata$elderly_rate, method = "pearson")
cor(mydata$pop_growth, mydata$elderly_rate, method = "spearman")
# 음의 상관관계임
# 상관계수 여러번 비교해야 할 때는 한번에 할 수 있는 방법
# 많은 변수에 대해 상관분석을 해야 한다면
attach(mydata)
x <- cbind(pop_growth, birth_rate, elderly_rate, finance, cultural_center)
cor(x)

#============== 실 습 3 =======================
install.packages("UsingR")
library(UsingR)

str(galton)
View(galton)
plot(child ~ parent, data = galton)
# 상관 테스트 
cor.test(galton$child, galton$parent)
# 0.7이상이면 굉장히 강한 관계라고 봐도 된다.
# 0.4이상이면 관계가 있구나, 0.3 약한 관계, 
#  p-value < 2.2e-16 : 차이가 있다.

# 회귀선
out <- lm(galton$child ~ galton$parent, data = galton)
summary(out)
# y = ax + b일 때 a = 0.64629, b= 23.94153     
# 그래프 그리기
abline(out, col="red")

plot(jitter(child,5) ~ jitter(parent,5), data = galton)
abline(out, col="red")
# jitter : 소수점 이하의 값을 다양하게 분포하도록 만들는 함수,
# 꽃잎 그래프
sunflowerplot(galton)


install.packages("SwissAir")
library(SwissAir)
View(AirQual)

Ox <- AirQual[ , c("ad.O3", "lu.O3", "sz.O3")] + AirQual[ , c("ad.NOx", "lu.NOx", "sz.NOx")] -
  AirQual[ , c("ad.NO", "lu.NO", "sz.NO")]

head(Ox)

names(Ox) <- c("ad", "lu", "sz")
head(Ox)

plot(lu~sz, data=Ox) # 너무 많이 모여 있어 데이터 양이 확인이 안됨

install.packages("hexbin")
library(hexbin)
bin <- hexbin(Ox$lu, Ox$sz, xbins = 80) # xbins는 6각형의 크기
plot(bin)

smoothScatter(Ox$lu, Ox$sz) # 몽환적 표현

# 히트맵과 비슷한 그래프
install.packages("IDPmisc")
library(IDPmisc)
iplot(Ox$lu, Ox$sz)





