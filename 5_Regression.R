#=============== 단순 회귀 분석 =================
#  y = ax + b    ::: a : 회귀계수, b : 독립변수 

str(women) # 미국여성을 대상으로 키와 몸무게 조사(30~39세): 인자와 파운드
View(women)

plot(weight ~ height, data = women)

fit <- lm(weight ~ height, data = women)
fit
# 선 그리기
abline(fit, col="blue")

plot(fit)

# 책 349p

par(mfrow = c(2,2))
plot(fit)

# 다항 회귀 분석
fit2 <- lm(weight ~ height + I(height^2),  data = women)
summary(fit2)
# Multiple R-squared:  0.9995 전보다 높아짐

par(mfrow = c(1,1))
plot(weight ~ height, data = women)
lines(women$height, fitted(fit2), col="red")

par(mfrow = c(2,2))
plot(fit2)

# =================== 실 습 ==================
mydata <- read.csv("../data/regression.csv")
View(mydata)

# social_welfare :  사회 복지 시설
# active_firms :  사업체 수
# urban_park : 도시 공업
# tris : 폐수 배출 업체
# kindergarten : 유치원


# 종속 변수 : birth_rate
# 독립 변수 : kindergarten
# 가설 : 유치원 수가 많으 지역에 합계 출산율도 높은가?
#         또는 합계 출산율이 유치원수에 영향을 받는가?

attach(mydata)
fit <- lm(birth_rate ~ kindergarten, data = mydata)
summary(fit) # 0.04684: 기울기, p-value: 0.01416 의미가 있다.
par(mfrow = c(2,2))
plot(fit)

fit2 <- lm(log(birth_rate) ~ log(kindergarten), data = mydata)
summary(fit2) # Multiple R-squared:  0.04382 조금 좋아짐
par(mfrow = c(2,2))
plot(fit2)

shapiro.test(resid(fit)) # 정규분포가 아니였지만 보정으로 정규분포가 됨
shapiro.test(resid(fit2)) # 정규분포 만족함

# 독립변수를 dummy로 변경(군:0, 시:1)
fit3 <- lm(birth_rate ~ dummy, data = mydata)
summary(fit3) # # 시가 -0.09476로 시가 음수, 군보다 춠생율이 시가 낮음

detach(mydata)
# ================ 실 습 2 =================
# www.kaggle.com : House sales Price in Kings country, USA

house <- read.csv("../data/kc_house_data.csv")
View(house)

# 종속 변수 :  price
# 독립 변수 : sqft_living(거실 크기)
reg1 <- lm(price ~ sqft_living, data = house)
summary(reg1)  # Std. Error(표준 오차) 
# Estimate(회귀계수, 비표준 계수) : 음수, 양수를 신경써야 한다.
# 거실 1피트 중가할때 가격 280.624$ 증가한다.

plot(house$sqft_living, house$price)
plot(reg1)
# Multiple R-squared:  0.4929 : 변수를 추가하면 숫자는 올라 간다.

# ===== 표준화 계수 =====
# 종속 변수 : price
# 독립 변수 : sqft_living, floors, waterfront

reg1 <- lm(price ~ sqft_living+floors + waterfront, data = house)
summary(reg1) # 숫자 올라간 것을 확인!

#  푲준화 계수 확인
install.packages("lm.beta")
library(lm.beta)

reg2 <- lm.beta(reg1)
summary(reg2)

# 더미 변수 
# 값이 오직 "0" 과 "1" 로만 이루어짐
# 이산형/범주형 변수를 연속형 변수처럼 사용
# 필요한 더미 변수의 갯수는 범주의 갯수 -1
# 예) 시군구이면 변수 2개 필요!

### 다중 공선성
# 원인 : 독립변수들끼리 너무 만힝 겹쳐서 발생하는 문제
# 확인방법
#   1) 산포도, 상관계수 : 상관계수가 0.9를 넘게 되면 다중공선성문제
#   2) VIF(Variance Infaltion Factor) : 분산 팽창 지수
#       - 일반적으로 10보다 크면 문제가 있다고 판단 (연속형 변수)
#       - 더미변수일 경우에는 3이상이면 문제가 있다고 본다.
#       - sqrt(vif) > 2 일 경우도 다중공선성을 의심


# 해결 방법
#     1) 변수를 뺀다
#     2) 주성분 분석

# ======================= 실습 3 =========================
house <- read.csv("../data/kc_house_data.csv", header = T)
View(house)

# 독립 변수 : sqft_living, bathrooms, sqft_lot, floors
# 변수들 간에 상관관계 확인
x = with(house, cbind(sqft_living, bathrooms, sqft_lot, floors))
cor(x)
cor(x, house$price) #  거실과 가장 관계가 높은 것을 확인 가능

reg1 <- lm(price ~ sqft_living, data = house)
summary(reg1)

reg2 <- lm(price ~ sqft_living + floors, data = house)
summary(reg2)

# 조절 변수(interactive term, 상호변수, 교호변수)
reg2_1 <- lm(price ~ sqft_living + floors + sqft_living*floors, data = house)
summary(reg2_1)
# floors -1.164e+05 : 양수에서 음수로 바꿨다.

# 다중 공선성 확인
install.packages("car")
library(car)

vif(reg2_1)
# 21.987677 : 다중공선성 문제가 있다. sqft_living:floors 빼야 한다.
# 
x <- with(house, cbind(floors, sqft_above, sqft_basement))
cor(x)
cor(x, house$price)

reg3 <- lm(price ~ floors + sqft_above + sqft_basement, data = house)
summary(reg3)

vif(reg3)

# ============= 실 습 4 ================
View(state.x77)
# 종속 변수 : Murder
states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy",
                                      "Income", "Frost")])
View(states)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
summary(fit)
# Pr값이 수입과 Frost와는 관계가 없는것으로 나옴

# 다중 공선성
sqrt(vif(fit))

# 이상 관측치 
#     1) 이상치(outlier) : 표준오차 2배 이상 크거나 작은 값
#     2) 큰 지레점(High leverage points) : 절편을 포함한 인수들의 
#                 숫자/n의 값이 2~3배 이상되는 관측치 ::: 5/50 = 0.1
#     3) 영향 관측치(Influential Observation, Cook's D) : 
#         독립변수의 수 / (샘플수 - 예측인자의 수 - 1) 
#       ** 4 / (50 - 4 - 1) = 0.1이 값보다 클 경우

influencePlot(fit, id = list(method = "identify"))

# y측 기준 : 이상치 
# x축 기준 : 큰 지레점
# 원의 크기 : 영향 관측치

#=========================================================================
# 회귀모형의 교정

par(mfrow = c(2,2))
plot(fit)
# 정규성을 만족하지 않을 때

powerTransform(states$Murder)

summary(powerTransform(states$Murder))


# 선형성을 만족하지 않을 때
boxTidwell(Murder ~ Population + Illiteracy, data = states)


# 등분산성을 만족하지 않을 때
ncvTest(fit)

spreadLevelPlot(fit)


#=========================================================================
# 회귀 모형의 선택
# Backward Stepwise Regression
# Forward stepwise REgresstion
# AIC(Akaike's information criterion)

fit1 <- lm(Murder ~ ., data = states)
summary(fit1)

fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
summary(fit2)

AIC(fit1, fit2) # AIC가 작아짐, 

# backward 
full.model <- lm(Murder ~., data = states)
reduced.model <- step(full.model, direction = "backward", trace = 0)
reduced.model
# AIC=93.76 가장 작으므로 Murder ~ Population + Illiteracy이 적합함을 알 수 있다.

# forward 
min.model <- lm(Murder ~ 1, data = states)
fwd.model <- step(min.model, direction = "forward", 

                  scope = (Murder ~ Population + Illiteracy + Income + Frost))
#=========================================================================

# all subset regression
install.packages("leaps")
library(leaps)

result <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, 
                     data = states, nbest = 4)
plot(result, scale="adjr2")
# adjr2 값이 높을수록 좋은 것


# ====실 습 4 ====
mydata <- read.csv("../data/regression.csv")
View(mydata)

str(mydata)
reg1 <- lm(birth_rate ~ cultural_center + social_welfare + active_firms +
           urban_park + doctors + tris + kindergarten + dummy,
         data = mydata)
summary(reg1)
# 양과 음의 방향이 존재, Estimate 이 값만 보고 해석하기엔 부족한게 많다.
# Pr(>|t|) : 값 0.05보다 큰것은 의미가 없다.
# Adjusted R-squared:  0.1032 
# p-value: 0.002444 : 모델은 적합하다.

# cultural_center,urban_park 제외
reg2 <- lm(birth_rate ~ social_welfare + active_firms +
            doctors + tris + kindergarten + dummy,
           data = mydata)

summary(reg2)
# 
# forward
full.model <- lm(birth_rate ~ 1, data = mydata)
fwd.model <- step(full.model, direction = "forward", 
                      scope =(birth_rate ~ cultural_center + social_welfare +
                                active_firms + urban_park + doctors + tris + 
                                kindergarten + dummy ))

# AIC는 큰 의미가 없다.
# 
# 정규분포
plot(reg2)
# 정규성 검정
shapiro.test(resid(reg2)) # p-value = 0.0004156 : 0.05보다 작기에 정규분포가 아님
library(car)
summary(powerTransform(mydata$social_welfare))
# lambda 

reg3 <- lm(log(birth_rate) ~ log(social_welfare) + log(active_firms) +
             log(doctors) + log(tris) + log(kindergarten) + dummy,
              data = mydata)
summary(reg3)
# 정규성 교정이 됨
shapiro.test(resid(reg3)) #  p-value = 0.1617 

# 다중 공선성
sqrt(vif(reg1))
sqrt(vif(reg2))
sqrt(vif(reg3))

plot(reg3)
# 독립변수들끼리 상관관계가 많지는 않음을 확인할 수 있다.

# 등분산성
ncvTest(reg1)
ncvTest(reg2)
ncvTest(reg3)

spreadLevelPlot(reg3)

# ============ 로지스틱 회귀 분석 ====================
# titanic
titanic <- read.csv("../data/train.csv", header = T)
View(titanic)

# 종속변수 : Survived(1: 생존, 0: 죽음)
# 독립변수 : Pclass (1st, 2nd, 3rd) --> 더미변수로 만들기
# pclass의 범주별로 3개 만들기
# 전처리
titanic$pclass1 <- ifelse(titanic$Pclass == 1, 1, 0)
titanic$pclass2 <- ifelse(titanic$Pclass == 2, 1, 0)
titanic$pclass3 <- ifelse(titanic$Pclass == 3, 1, 0)

View(titanic)

# 선형회귀로 먼저 해보기 ,,,,1st 기분
reg1 <- lm(Survived ~ pclass2 + pclass3, data = titanic)
summary(reg1)
#  Pr(>|t|) 유의하며, 1st은 양의 방향 즉 많이 생존, 2nd는 0.6263-0.15680= 만큼 생존확률
# 생사확인을 명확하게 해석하기가 힘들다. 그래서 로지스틱 회귀분석을 사용

reg2 <- glm(Survived ~ pclass2 + pclass3, data = titanic, family = binomial())
summary(reg2)




# Gendermale
exp(-2.4115) # 여성에 비해 남성의 생존률은 0.09배 높다.
(exp(-2.115) - 1)* 100 # 91% 낮다.
(1/exp(-2.4115) - 1)* 100 # 남성에 비해 여성의 생존률은 1015% 높다.


# ========================================================================
# survival 패키지를 이용한 colon
library(survival)

str(colon)
?colon

# 종속변수 : status (1 : 사망 or 재발, 0 : 재발없이 생존)
# 결측치 확인, 결측치 빼고 다시 
# extent: 전의여부
table(is.na(colon))
colon1 <- na.omit(colon)
table(is.na(colon1))

result <- glm(status ~ rx + sex + age + obstruct + perfor + adhere +
                nodes + differ + extent + surg, family = binomial, 
              data = colon1)
summary(result)
# 변수가 많아 중요한 변수를 뽑아서 다시 해본다.
# backward 
reduce.model = step(result)
summary(reduce.model)

library(moonBook)
extractOR(reduce.model)
# lcl 실내구간의 상한치, 하한치
# 과산포는 종속변수으 실제 분산이 이항 분포에서 기대되는 예측분산보다 
# 클때 생길 수 있으며 과산포는 검정이 표준오차를 만들어 
# 검정을 부정확하게 만들 수 있다. 

# 과산포가 없을 때 : binomial
# 과산포가 있을 때 : quasibinomial

fit <- glm(status ~ rx + obstruct + adhere +
                 nodes + extent + surg, family = binomial, data = colon1)

fit.od <- glm(status ~ rx + obstruct + adhere +
             nodes + extent + surg, family = quasibinomial, data = colon1)

summary(fit.od)
pchisq(summary(fit.od)$dispersion * fit$df.residual, fit$df.residual, 
       lower = F)
# 위의 값이 0.05보다 크면 과산포가 없다고 볼 수 있다.
ORplot(fit) # 1에 가까이 있을수록 의미가 없는 변수

ORplot(fit, main = "Plot for Odds Ratio", type = 2, 
       show.OR = F, show.CI = T, pch = 15, lwd = 3)













