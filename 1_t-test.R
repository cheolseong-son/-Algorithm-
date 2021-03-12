


# 목적 : 두개의 집단이 같은지 다른지 비교하기 위해서 사용
# 조건 
# 1. 결과값이 연속변수
#     아닐경우 : Mann-Whitney U test, Wilcoxen rank_sum test,
#                 Mann-Whitney-Wilcoxen test, MWW (같은것이다.)

# 2. 정규 분포 여부
#     아닐경우 : MWW

# 3. 등분산 여부 
#     아닐경우 : Welch's t-test

# 4. paired t-test일 경우 Wilcoxen signed rank test 공식 사용(비모수일 경우)
# 
# 위 세가지(1,2,3) 조건을 만족해야 t-test를 사용가능하다.
# =============================================================

# 정규분포 : 모수적 통계 방법
# 정규분포가 아닐 경우 : 비모수적 통계방법
# 모수적(데이터가 충분한 것)
 
# ===================== Power Analysis ========================
# 적정한 표보의 갯수를 산출
# cohen's d

ky<- read.csv("../data/KY.csv", header = T)
View(ky)

table(ky$group)

mean.1 <- mean(ky$score[ky$group == 1])
mean.2 <- mean(ky$score[ky$group == 2])

cat(mean.1, mean.2)
# 표준 편차
sd.1 <- sd(ky$score[ky$group == 1])
sd.2 <- sd(ky$score[ky$group == 2])
cat(sd.1, sd.1)

# cohen'd 공식
effect_size <- abs(mean.1 - mean.2) / sqrt((sd.1^2 + sd.2^2) /2)
effect_size

install.packages("pwr")
library(pwr)
?pwr.t.test
pwr.t.test(d=effect_size, type = "two.sample", 
           alternative ="two.sided", power = .8, sig.level = .05)
# alternative = : 약측검정인지 단측검정인지
# power = .8 : 80%의 검정력
# sig.level =  : 생략 가능, 0.05기본값을 가지고 있음

# ========== 사례1 : 두 집단간의 평균===========
install.packages("moonBook")
library(moonBook)
?acs

head(acs)
str(acs)
# 두 집단(남성과 여성)의 나이 차이를 알고 싶다.
# 귀무 가설 : 남성과 여성의 평규 나이에 대해 차이가 없다.
# 대립 가설 : 남성과 여성의 평균 나이에 대해 차이가 있다.

mean.man <- mean(acs$age[acs$sex == "Male"])
mean.woman <- mean(acs$age[acs$sex == "Female"])
cat(mean.man, mean.woman)

# ======== 정규 분포 테스트 ========
# 원인에 따라 결과가 달라지는 값 : 종속 변수, 설명변수 , 결과
# 원인을 독립변수
# moonBook에 포함도니 함수
moonBook:: densityplot(age ~ sex, data = acs)

# 귀무가설 : 정규분포가 맞다.
# 대립 가설 : 정규분포가 아니다.

shapiro.test(acs$age[acs$sex == "Male"])
# p-value가 95%, 0.05보다 크기에 정규분포가 맞다.

shapiro.test(acs$age[acs$sex == "Female"])
# p-value가 0.05보다 작기에 대립가설이 맞다.

#======= 등분산 테스트 ========
# 귀무가설 : 등분산이 맞다.
# 대립가설 : 등분산이 아니다.

var.test(age ~ sex, data = acs)
#  p-value = 0.3866이기에 0.05보다 크기에 95%에 들어가기에 등분산이 맞다.

# MWW 검정
wilcox.test(age ~ sex, data = acs)
# p-value < 2.2e-16 0.05보다 작고 95%에 못 들어가기에 대립가설이다.

# t-test
t.test(age ~ sex, data = acs, var.test = T, alt="two.sided")
# var.test = T : 등분산이 같다.
# p-value < 2.2e-16 :

# 등분산이 아닐경우 : Welch's test
t.test(age ~ sex, data = acs, var.test = F, alt="two.sided")
# p-value < 2.2e-16 위에 MWW, t-test와 같은 값을 가진다.
# 옵션에 따라 공식이 달라진다.

# ===== 사 례2 : 집단이 한개인 경우 =================================

# A회상의 건전지 수명이 1000시간 일때 무작위로뽑은 10개의 건전지 수명에 대해
# 샘플이 모집단과 다르다고 할 수 있는가?

# 귀무 가설 : 모집단의 평균과 같다.
# 대립 가설 : 모집단의 평균과 다르다.
a <- c(980,1008,968,1032,1012,1002,996,1017,990,955)
# 1) 연속변수 맞음
mean.a <- mean(a)
mean.a
# 2) 정규분포인가? 맞으면 t-test, 아니면 MWW
shapiro.test(a)
# p-value = 0.9781 0.05보다 크기에 정규분포이다.

t.test(a, mu=1000, alt="two.sided")
# 모집단 평균 뮤, 표본에서 구한 평균 x하이바


# 어떤 학급의 수학 평균성적이 55점, 0교시 수업을 하고 다시 성적을 보니

b <- c(58,49,38,99,32,88,62,30,55,65,44,55,57,53,88,42,39)
mean(b)
# 정규분포 학인 :p-value = 0.1099 맞다.

shapiro.test(b)

t.test(b, mu=55, alt="greater")
#  p-value = 0.4095 95%안 기무가설, 성적 오른 것이 아님, 효과 없음
# 0.05보다 작게 나와야 차이가 있고 효과가 있는 것임


# 책 359p
#===================== 사례3: Paired Sample T-Test ======================
str(sleep)
View(sleep)

before <- subset(sleep, group == 1, extra) # 첫번째 집단
before

after <- subset(sleep, group == 2, extra) # 두번째 집단
after
# 그래프 그리기
install.packages("PairedData")
library(PairedData)

# 나눠진 데이터 합치기
sleep2 <- paired(before, after)
sleep2
plot(sleep2, type="profile") + theme_bw() # 테마 : 블랙, 화이트

shapiro.test(sleep$extra[sleep$group==1])
# p-value = 0.4079, 0.05보다 크기에 정규분포이다.


# 정규분포 확인 
with(sleep, shapiro.test(extra[group==1])) # 정규분포
with(sleep, shapiro.test(extra[group==2]))# 정규분포

# 등분산 여부 확인
with(sleep, var.test(extra[group==1],extra[group==2]))
# p-value = 0.7427,  0.05보다 크기에 기무가설, 등분산

with(sleep,t.test(extra ~ group, data=sleep, paired=T))
# p-value = 0.002833, 0.05보다 작으므로 대립가설, 

# ======================== 실 습 1 =============================
# dummy : 0은 군, 1은 시를 나타냄
# 시와 군에 따라서 합계 출산율의 차이가 있는지 알아보려고 한다.
# 귀무 가설 : 차이가 없다.
# 대립 가설 : 차이가 있다.

mydata <- read.csv("../data/independent.csv")
View(mydata)
gun.mean <- mean(mydata$birth_rate[mydata$dummy == 0])
city.mean <- mean(mydata$birth_rate[mydata$dummy == 1])
cat(gun.mean, city.mean)

# 출산율이기에 연속변수이다.
# 정규 분포 확인
shapiro.test(mydata$birth_rate[mydata$dummy==1])
shapiro.test(mydata$birth_rate[mydata$dummy==0])
# 군 p-value = 0.009702, 0.05보다 작기에 정규분포가 아니다.
# 시 p-value = 0.001476 ---------------//---------------
# 정구분포가 아니기ㅣ에 wilcox.test 사용
wilcox.test(mydata$birth_rate ~ mydata$dummy, data = mydata)
# p-value = 0.04152, 차이가 있다.

t.test(mydata$birth_rate ~ mydata$dummy, data = mydata)
# t.test로 해봐도 차이가 있음을 알 수 있다.

# ============================ 실 습 2 ==============================
str(mtcars)
head(mtcars)

# am : 0은 오토, 1은 수동
# mpg :  연비
# 오토나 수동에 따라 연비가 같을까? 다를까/
mpg_mean_0 <- mean(mtcars$mpg[mtcars$am == 0])
mpg_mean_1 <- mean(mtcars$mpg[mtcars$am == 1])
cat(mpg_mean_0, mpg_mean_1)
#  정규분포 확인
shapiro.test(mtcars$mpg[mtcars$am == 0]) # p-value = 0.8987 정규분포
shapiro.test(mtcars$mpg[mtcars$am == 1]) # p-value = 0.5363정규분포
# 둘중 하나라도 정규분포가 아니면 그것은 정규분포가 아니다.

# 등분산 확인
var.test(mtcars[mtcars$am==1, 1], mtcars[mtcars$am==0, 1])
# p-value = 0.06691, 0.05보다 크기에 등분산 맞음, 아니면 wilcox

t.test(mtcars$mpg ~ mtcars$am, data = mtcars, var.test=T)
#  p-value = 0.001374, 0.05보다 작기에 차이가 있다.

# 좋다 나쁘다 일때
t.test(mtcars$mpg ~ mtcars$am, data = mtcars, var.test=T, alt="less")
# p-value = 0.0006868 : 크다.
# 오토가 수동보다 작다로 보기에 alt= "less"


#========================== 실 습 3 =============================
# 쥐의 몸무게가 전과 후의 변화가 있는지 없는지 판단
data <- read.csv("../data/pairedData.csv")
View(data)
# 데이터를 long형으로 변경
library(reshape2)
data1 <- melt(data, id=("ID"), variable.name = "GROUP", value.name = "RESULT")
data1

# 구조를 바꾸는 또 다른 방법
install.packages("tidyr")
library(tidyr)

?gather
data2 <- gather(data, key = "GROUP", value = "RESULT", -ID)
# ID를 기준으로 묶어주겠다.
data2

shapiro.test(data1$RESULT[data1$GROUP == "before"])
shapiro.test(data1$RESULT[data1$GROUP == "After"])

t.test(data1$RESULT ~ data1$GROUP, data = data1, paired=T)

# 구조를 바꾸지 않고 사용
t.test(data$before, data$After, paired = T)

# 그래프로 출력
before <- subset(data1, GROUP == "before", RESULT)
before # 값만 갖고 있는 데이터 프레임
after <- subset(data1, GROUP == "After", RESULT)
after 

data3 <- paired(before, after)
plot(data3, type="profile") + theme_bw()

# 다른 방법
moonBook::densityplot(RESULT ~ GROUP, data=data1)

# ======================다른 예제===============================
# 시 별로 2010년도와 2015년도의 출산율 차이가 있는가?

data <- read.csv("../data/paired.csv")
View(data)
str(data)
data2 <- gather(data, key = "Group" , value = "Result",-c(ID, cities) )
# group, result 값이외의 것은 묵어 준다.
data2

with(data2, shapiro.test(Result[Group =="birth_rate_2010"]))
with(data2, shapiro.test(Result[Group =="birth_rate_2015"]))
# 정규분포가 아니다.

wilcox.test(data2$Result ~ data2$Group, data = data2, paired=T)
#  p-value = 0.4513: 0.05보다 작기에 차이가 없는 것이다.

# ==================== 실 습 4 ==========================

# https:// www.kaggle.com/kappernielsen/independent-t-test-example

mat <- read.csv("../data/student-mat.csv", header = T)
str(mat)
# 수학점수가 남학생과 여학생에 따라서 같은지 다른지 검증
# 수학점수는 G1, G2, G3에 걸쳐서 나타나 있다.
summary(mat$G1)
summary(mat$G2)
summary(mat$G3)

table(mat$sex)

# 남녀별로 세번의 시험평균을 비교해보자
library(dplyr)
mat %>% select(sex, G1, G2, G3) %>% group_by(sex) %>% 
  summarise(mean_g2=mean(G1), mean_g2=mean(G2), mean_g3=mean(G3),
            cnt_g1=n(), cnt_g2=n(), cnt_g3=n(), 
            sd_g1=sd(G1), sd_g2=sd(G2), sd_g3=sd(G3))

# 정규분포 확인
shapiro.test(mat$G1[mat$sex == "M"])
# p-value = 0.01363 : 0.05보다 크기에 대립가설, 정규분포가 아니다.
shapiro.test(mat$G2[mat$sex == "M"])
shapiro.test(mat$G3[mat$sex == "M"])
shapiro.test(mat$G1[mat$sex == "F"])
shapiro.test(mat$G2[mat$sex == "F"])
shapiro.test(mat$G3[mat$sex == "F"])
# 데이터가 충분하면 정규분포라고 생각해도 무방하다.

# 등분산 확인
# var.test(종속변수 ~ 독립변수, )
var.test(G1 ~ sex, data = mat)
var.test(G2 ~ sex, data = mat)
var.test(G3 ~ sex, data = mat)
# p-value = 다 0.05보다 크기에 등분산이다.


# 양측 검정

t.test(G1 ~ sex, data=mat, var.eqal=T, alt="two.sided")
# p-value = 0.06898 : 0.05와 비슷하기에 차이가 없는걸로 봐도 된다.
t.test(G2 ~ sex, data=mat, var.eqal=T, alt="two.sided")
# p-value = 0.07144 : 큰 차이가 없다.
t.test(G3 ~ sex, data=mat, var.eqal=T, alt="two.sided")
# p-value = 0.03958 : 0.05보다 작기에 여학생엥 비해
# 남학생의 차이가 조금 있다.


t.test(G1 ~ sex, data=mat, var.eqal=T, alt="greater")
# greater면 여학생의 점수가 더 크다로 본다. p-value = 0.9655 차이 없음으로 나온다.
# less로 해야 한다. 차이가 있다.단측 검정
t.test(G1 ~ sex, data=mat, var.eqal=T, alt="less")
t.test(G2 ~ sex, data=mat, var.eqal=T, alt="less")
t.test(G3 ~ sex, data=mat, var.eqal=T, alt="less")


# 같은 학생 입장에서 G1과 G3에 대해서 변화가 (평균의 변화)있었는지 확인

# 첫번째 시험과 마지막 시험의 평균 비교
mat %>% select(G1, G3) %>% summarise(mean_g1 =mean(G1), mean_g3=mean(G3),
                                     )
# long type으로 변경
library(tidyr)
# G1, G3를 GROUP으로 묶음, 
mydata <- gather(mat, key = "GROUP", value = "RESULT", "G1", "G3")
View(mydata)

# 정규분포라고 가정하고 T.TEST사용
t.test(mydata$RESULT ~ mydata$GROUP, data = mydata, paired=T)
# p-value = 0.0004291 : 0.05보다 작기에 차이가 있다. 
# 시험성적이 더 나빠졌다고 해석 가능하다.
# 데이터가 30개 이상일 경우 정규분포로 보고 해도 상관 없다.
wilcox.test(mydata$RESULT ~ mydata$GROUP, data = mydata, paired=T)
#  p-value = 0.3153 : 0.05보다 크기에 차이가 없다고 해석

# 다른 방법(long type으로 바꾸지 않고 쓸 때)
t.test(mat$G1, mat$G3, paired = T)

























