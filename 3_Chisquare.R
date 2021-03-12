# ==== 조건 ====
# 1. Chi_square Test : 데이터 수가 충분할 때 사용 (정규분포)
# 2. Fisher's exact test : 데이터 수가 부족할 때 
# 3. Cochhran - armitage trend test :  명목변수가 서열변수일 때(trend)
# 일원 카이제곱(차이 검정), 이원 카이제곱(관계검정)
# 


# ========== 실 습 1 ===========
View(mtcars)

# 자동차 실린더 수와 변속기의 관계
table(mtcars$cyl, mtcars$am)

# 데이터 전처리
mtcars$tm <- ifelse(mtcars$am == 0, "auto", "manual")
result <- table(mtcars$cyl, mtcars$tm)
result

barplot(result)

# auto의 눈금이 벗어났기에 최댓값을 알 수 없기에 눈금 조정이 필요
barplot(result, ylim = c(0,20))

# 범례 추가
barplot(result, ylim = c(0,20), legend= rownames(result))

# 범례 형식
barplot(result, ylim = c(0,20), legend= paste(rownames(result), "cyl"))

# 그래프를 수직으로 나누기
barplot(result, ylim = c(0,20), legend= paste(rownames(result), "cyl"), 
        beside = T)
# 그래프를 수평으로 보기
barplot(result, ylim = c(0,20), legend= paste(rownames(result), "cyl"), 
        beside = T, horiz = T)
# 색상
mycol = c("tan1", "coral2", "firebrick2")
barplot(result, ylim = c(0,20), legend= paste(rownames(result), "cyl"), 
        beside = T, col=mycol)

# 카이제곱 검정
result 
# 행, 열 합계 : addmargins()
addmargins(result)

chisq.test(result) # p-value = 0.01265 : 0.05보다 작기에 차이가 있다.
# 하지만 데이터 수가 적기에 p-value를 믿을 수 없다.
# 이럴때에는 fisher.test()
fisher.test(result) # p-value = 0.009105 : 차이가 있다. 실린더와 변속기사이에는 
# 연관성이 있다고 해석 가능하다.

# ================ 실 습 2 ====================
mydata <- read.csv("../data/anova_two_way.csv")
View(mydata)

# ad_layer(시, 군, 구)와 multichild(다가구 자녀지원 조례)가 관계가 있느낙?
# 둘다 명목 변수 --> 카이제곱 사용
tab <- table(mydata$ad_layer, mydata$multichild)
chisq.test(tab) # 데이터 불충분하기에 fisher test()로 확인
fisher.test(tab) # 차이가 없다.

# ================ 실 습 3 ====================
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

welfare <- read.spss(file ="../data/koweps_hpc10_2015_beta1.sav", to.data.frame=T)
welfare <- rename(welfare, sex=h10_g3, birth=h10_g4, marriage=h10_g10,
                  religion=h10_g11, income=p1002_8aq1, code_job=h10_eco9,
                  code_region=h10_reg7)

welfare <- welfare[, c("sex", "birth", "marriage", "religion", "income",
                       "code_job", "code_region")]

# 성별과종교의 관련성 여부
tab <- table(welfare$sex, welfare$religion)
chisq.test(tab)


# ================ 실 습 4 : Cochran-Amitage Trend Test ====================
library(moonBook)
View(acs)

# 흡연자, 비흡연자, 금연자와 고혈압의 유뮤가 서로 연관이 있을까?

table(acs$HBP, acs$smoking)

# smoking의 순서 바꾸기
acs$smoking <- factor(acs$smoking, levels = c("Never", "Ex-smoker", "Smoker"))
result <- table(acs$HBP, acs$smoking)
result

# chisq.test(result) # p-value = 6.19e-10 : 관계(차이)가 있음, 
# 
?prop.trend.test
# x : 사건이 발생한 숫자
# y : 합계

# 고혈압이 발생한 사람의 숫자(x에 해당)
result[2, ]

# smoking 시도 횟수(n에 해당)
colSums(result)

prop.trend.test(result[2, ], colSums(result))

# 차이제곱의 그래프(기본 그래프) : 모자이크 그래프

mosaicplot(result, color = c("slategray3", "yellowgreen"))

# 색상표 참조
colors()
# 색상 확인
demo("colors")

# 행과 열의 위치 변경
mosaicplot(t(result), color = c("slategray3", "yellowgreen"))
# 레이블 
mosaicplot(t(result), color = c("slategray3", "yellowgreen"),
           ylab = "Hypertension", xlab = "Smoking")

mytable(smoking ~ age, data=acs) # age는 교란 변수로 작용한다.


