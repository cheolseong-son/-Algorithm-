### 단순 선형 회귀 ### 
# --------------------

# 모델 생성
data(cars)
head(cars)
(m <- lm(dist ~ speed, cars))

# 선형 회귀 결과 추출
coef(m) # 회귀 계수
fitted(m)[1:4] # 적합된 값
residuals(m)[1:4] # 잔차(residuals)

# 적합된 값과 잔차의 합
fitted(m)[1:4] + residuals(m)[1:4] # 방법 1
cars$dist[1:4]  #  방법 2
confint(m)  # 회귀 계수의 신뢰 구간
deviance(m) # 잔차제곱 합


# 예측과 신뢰 구간
# 주행 속도가 3일 경우의 예측
(m <- lm(dist ~ speed, data = cars))
predict(m, newdata = data.frame(speed=3)) # -5.781869 : 제동 거리
coef(m)

predict(m, newdata = data.frame(speed=3), interval = "confidence")
predict(m, newdata = data.frame(speed=3), interval = "prediction")

# 모델 평가
summary(m) # 책 377page



# 분산 분석 및 모델 간의 비교
anova(m)
(full <- lm(dist ~ speed, data = cars))
(reduced <- lm(dist ~ speed, data=cars))

# full과 reduced 비교
anova(reduced, full)
# F 통계량은 89.567이며 p값은 아주 작게 나타났다.따라서 두 모델간의 유의한 차이가
# 이사독 결론을 내린다.
plot(m)

# 회귀 직선의 시각화
plot(cars$speed, cars$dist)
abline(coef(m))

# 그래프의 추정값의 신회구간을 포함하는 방법
# speed의 최솟값, 최댓값을 찾는다.
summary(cars$speed)
predict(m, newdata = data.frame(speed=seq(4.0, 25.0, .2)),
        interval = "confidence")

# 그래프로 그리기
speed <- seq(min(cars$speed), max(cars$speed), .1)
ys <- predict(m, newdata = data.frame(speed=speed), interval = "confidence")
matplot(speed, ys, type = 'n')
matlines(speed, ys, lty=c(1,2,2), col = 1) # 선형 회귀 식은 직선, 신뢰구간 점선



### 중선형 회귀 ### 
# -----------------
# 하나 이상의 독립변수가 사용된 선형 회귀다.

# 모델 생성 및 평가
(m <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris))
summary(m)
# p-value가 0.05보다 작아 모두 중요한 설명 변수이다.

# 범주형 변수
(m <- lm(Sepal.Length ~ ., data = iris))
summary(m)

# 데이터의 1행, 51행, 101행 데이터가 어떻게 코딩되어 모델에 사용되었는지 보기
model.matrix(m)[c(1,51,101), ]

# 분산 분석
anova(m)
# Pr(>F) : 0.0105370이므로 유의미한 설명 변수
# (가설 검정 시 Species의 계수를 0으로 볼 수 없다.)임을 알 수 있다.

# 중선형 회귀 모델 시각화
# 산점도
with(iris, plot(Sepal.Width, Sepal.Length,cex=.7, pch=as.numeric(Species)))
# cex= :점의 크기, pch= : 점의 형태

as.numeric(iris$Species)


m <- lm(Sepal.Length ~Sepal.Width + Species, data = iris)
coef(m)
abline(2.25, 0.80, lty=1)
abline(2.25 + 1.45, 0.80, lty=2)
abline(2.25 + 1.94, 0.80, lty=3)
# abline() : 첫번째 인자는 절편, 두번째 인자는 기울기, lty= : 선의 유형
legend("topright", levels(iris$Species), pch = 1:3, bg="white")
levels(iris$Species)


# 표현식을 위한 I()의 사용
x <- 1:1000
y <- x^2 + 3*x + 5 + rnorm(1000)
lm(y ~ I(x^2) + x)

# I를 사용하지 않으면 전혀 다른 결과를 얻는다.
lm(y ~ x^2)

# 다른 예제
x1 <- 1:1000
x2 <- 3 * x1
y <- 3 * (x1 + x2) + rnorm(1000)
lm(y ~ I(x1 + x2))
lm(y ~ x1 + x2) # I 사용 안함


# 변수의 변환
# 종속변수 log
x <- 101:200
y <- exp(3 * x  + rnorm(100))
lm(log(y) ~ x)

# 설명 변수 log()
x <- 101:200
y <- log(x) + rnorm(100)
lm(y ~ log(y))


# 상호 작용 : 독립변수간의 상호작용이 종속 변수에 영향을 주는 경우를 말한다.
#             또 이 영향을 주는 방법이 합이 아니라 곱의 형태일 때를 말한다.

data(Orange)
Orange
# Tree and Circumference 간의 상호 연관 관계를 plot()으로 시각화
with(Orange, plot(Tree, circumference, xlab = "tree", ylab = "circumference"))

# 다른 plot으로 시각화
with(Orange, interaction.plot(age, Tree, circumference))
# 나무의 수령이 높아집에 따라 둘레가 길어지는 추세가 관찰 그리고 어떤 나무인가에 
# 따라 둘레의 관계는 서로 다른 것으로 보인다.

# 나무는 순서가 있는 범주형 변수이므로 이를 순서가 없는 명목형 변수로 바꿔준다.
Orange[, "fTree"] <- factor(Orange[, "Tree"], ordered = FALSE)
# fTree, age, fTree:age를 설명 변수로 선형회귀를 수행하는 코드
m <- lm(circumference ~ Tree * age, data = Orange)
anova(m)
# fTree, age 두변수가 모두 p-value가 0.05보다 작아 유의한 것으로 나타남
# 
head(model.matrix(m))


# 이상치 : 주어진 회귀모델에 의해 잘 설명되지 않는 데이터 점들을 뜻한다.
#          이상치 검출에는 잔차, 특히 회면 스튜던트화 잔차가 사용
# 스튜던트화 잔차 : 잔차를 잔차의 표준편차로 나눈 값이다.
data(Orange)
m <- lm(circumference ~ age + I(age^2) + data = Orange)
rstudent(m)

# 이상치 찾아보기
Orange <- rbind(Orange, data.frame(Tree=as.factor(c(6,6,6)), age=c(118,484,664),
                                   circumference=c(177,50,30)))
tail(Orange)
m <- lm(circumference ~ age + I(age^2), data = Orange)
outlierTest(m)
# Bonferonni p값이 0.0001303으로 0.05보다 작은 값이 나와 이상치로 검출


# 변수 선택 : 책 404page
data(BostonHousing)
m <- lm(medv ~ ., data = BostonHousing)
m2 <- step(m, direction = 'both')
# AIC 값이 작을수록 더 좋은 모델을 뜻함

# 모든 경우에 대한 비교
data(BostonHousing)
m <- regsubsets(medv ~ .,data = BostonHousing)
summary(m)

# 수정 결정 계수 
summary(m)$bic
summary(m)$adjr2

# 시각화
plot(m, scale = 'adjr2')
# 수정 결정 계수는 0.722

# BIC를 기준으로 모델을 살펴보자
plot(m, scale = 'bic')
# bic가 가장 좋은 모델(작을수록 좋은 모델)은 절편 zn,chasl, nox, rm, dis, ptratio
# b, lstat을 포함한 모델로 이때 BIC값은  -600이다.











