# 1) 난수 생성 및 분포 함수

rnorm(100, 0, 10) # 0~10사이 난수 100개
plot(density(rnorm(1000000, 0, 10))) # 밀도 그래프로 확인

# 확률 질량 함수
dpois(3,1)
(1^3*exp(-1)) / (factorial(3))

pnorm(0)
qnorm(0.5)

# 2) 기초 통계량
mean(1:5) # 평균 
var(1:5)  # 표본 분산
sum((1:5-mean(1:5))^2)/(5-1) # 표본 분산
sd(1:5) #표본 분산 편차


# 다섯 수치 요약
fivenum(1:11)
summary(1:11)

fivenum(1:4)
summary(1:4)

# 최빈값
(x <- factor(c('a', 'b', 'c', 'c','c', 'd', 'd')))
table(x) # 각 level별 빈도수를 구함
which.max(table(x)) # 가장 큰 값이 저장된 셀의 색인은 3
names(table(x))[3] # 가장 큰 값이 저장된 셀의 이름

### 표본 추출 ###
sample(1:10, 5) # 1~10숫자 중 5개 추출
sample(1:10, 5, replace = TRUE) # 중복 허용하여 추출

# 가중치를 고려한 표본 추출
sample(1:10, 5, replace = TRUE, prob = 1:10) 

# 층화 임의 추출
(x <- strata(c("Species"), size = c(3,3,3), method = "srswor", 
             data = iris))
getdata(iris, x)
strata(c("Species"), size = c(3,1,1), method = "srswr", 
       data = iris)

iris$Species2 <- rep(1:2, 75)
strata(c("Species", "Species2"), size = c(1,1,1,1,1,1), method = 'srswr',
       data = iris)
# 계통 추출
(x <- data.frame(x=1:10))
sampleBy(~1, frac = .3, data = x, systematic = TRUE)


### 분할표 ###
table(c('a', 'b', 'c', 'c', 'c', 'd'))
d <- data.frame(x=c('1', '2', '2', '1'),
                y=c('A', 'B', 'A', 'B'),
                num=c(3,4,8,7))
(xtabs(num ~ x + y, data = d))


(d2 <- data.frame(x=c('A', "A", 'A', "B", "B")))
(xtabs(~x, d2))

# 합, 비율의 계산
d <- data.frame(x=c('1', '2', '2', '1'),
                y=c('A', 'B', 'A', 'B'),
                num=c(3,5,8,7))

xt <- (xtabs(num ~ x + y, data = d))
margin.table(xt, 1) # 행 방향 더하기
margin.table(xt, 2) # 열 방향 더하기
margin.table(xt) # 전체 합

prop.table(xt, 1) # xt의 각 행을 각각10(=3+7), 13(=8+5)로 나눈 값
prop.table(xt, 2) # xt의 각 열을 각각 11(=3+8), 12(=7+5)로 나눈 값
prop.table(xt) # 전체합으로 나눈 값

# 독립성 검정
library(MASS)
data(survey)
str(survey)

head(survey[c('Sex', 'Exer')])
# 성별과 운동이 독립인지를 확인해보기우해 분할표를 만들어 보자
xtabs(~ Sex + Exer, data = survey)

# *** chisq.test() :카이 제곱 검정
# ---------------------------------

chisq.test(xtabs(~Sex + Exer, data = survey))
# p-value = 0.05731으로 0.05보다 커서 성별과 운동은 독립이다.
# 귀무가설 : "관련이 없다."는 형태의 가설
#           " 두 변수가 독립이다.", " 두 변수의 평균에 차이가 없다." 등
# 

# 대립가설 : "관련이 있다." 독립이 아니다. 평균차이가 있다.등 말할 수 있다.
# 대립가설은 '같지 않다.', '작다.', '크다.' 세 가지 형태로 나타냄
# '같지 않다'는 양측 검정, '크다'와 '작다'는 단측 검정

# 가설검정은 귀무가설을 참이라고 시작!


# *** 피셔의 정확 검정
#-----------------------
xtabs(~W.Hnd + Clap, data=survey)
chisq.test(xtabs(~W.Hnd + Clap, data = survey)) 
# 경고문구가 뜨기에 fisher.test()로 검정해야 한다.
fisher.test(xtabs(~W.Hnd + Clap, data = survey))
# 귀무가설 : 글씨를 쓰는 손과 박수를 칠 때 위에 오는 손이 독립적이다.
# p-value가 0.05보다 작으므로 귀무가설 기각, 대립가설 채택


# *** 맥니마 검정
# ----------------

Performance <- matrix(c(794, 86, 150, 570),
                      nrow = 2,
                      dimnames = list("1st Survey" = c('Approve',"Disapprove"),
                                      "2nd Survey" = c('Approve', 'Disapprove')))
Performance
mcnemar.test(Performance)
# p-value가 0.05보다 작기에 사건 전후에 차이가 발생, 즉 귀무가설 기각 
# 대립가설 채택이다.
# 귀무가설 : Approve, disappove에 차이가 없다
# 대립가설 : Approve, Disapprove비율에 차이가 있다.

binom.test(86, 86 + 150,.5)
# p-value < 0.05로 나타나 86이 86 + 150의 절반이라는 귀무가설이 기각


### 적합도 검정 ###
table(survey$W.Hnd)
chisq.test(table(survey$W.Hnd),p=c(.3,.7)) # 관측 데이터 30% : 70%의 분포
# p-value < 0.05 이므로 글씨를 왼손으로 쓰는 사람과 오른손으로 쓰는 사람의 비가
# 3:7 이라는 귀무가설을 기각한다.

# 샤피로 윌크 검정
# 1000개의 난수를 발생시킨 뒤 이 숫자들이 정규분포를 다르는지 검정
shapiro.test(rnorm(1000)) 
# p-value >0.05 이므로 데이터가 정규분포를 따른다는 귀무가설을 기각할 수 없다.

# 콜모고로프 스미르노프 검정
ks.test(rnorm(100), rnorm(100)) # 두 난수 데이터 간에 분포가 동일한지 본다.
# p-value = 0.8127 > 0.05이므로 두 난수가 같다는 귀무가설을 기각할 수 없다.

# 정규분포와 균등 분포간의 비교
ks.test(rnorm(100), runif(100))
# p-value = 1.554e-15 <0.05ㅂ다 작아서 서로 다른 분포로 판단.

# 평균 0, 분산 1인 정규분포로부터 뽑은 표본인지 확인
ks.test(rnorm(1000), "pnorm", 0, 1)
# p-value = 0.09689 > 0.05보다 크기에 귀무가설을 기각할 수 없다.


# Q-Q도
# N(10,1)의 정규분포를 따르는 난수 1000개를 구하고 Q-Q도를 그려 해당 숫자가 
# 정규분포를 따르는지 확인
x <- rnorm(1000, mean = 10, sd=1)
qqnorm(x)
qqline(x,lty=2)
# 직선관계가 잘 성립하기에 정규분포확률 그림이라고 할 수 있다.

#  균등 분포
x <- runif(1000)
qqnorm(x)
qqline(x, lwd=2) # 직선관계가 성립하지 않음을 알 수 있다.

# Q-Q도 항상 명확한 판단을 하는 것은 아니다. 직선 관계가 성립할지라도 고민이 필요
qqplot(runif(1000), min=1, max=10, 1:10)
# 직선관계가 잘 성립



### 상관 분석 ###
# 두 확률 변수 사이의 관련성을 파악하는 방법 (피어슨의 상관계수)
# 상관계수가 크면 데이터간에 관계가 존재한다는 의미, 보통 한쪽 값이 커질 때 
# 다른 쪽 값이 커지는 정도가 크다.
# 하지만 상관계수가 크다고 해서 변수 간에 인과관계가 있음을 뜻하지 않는다.

# 아이리스 데이터
# Sepal.Width, Sepal.Length의 피어슨 상관 계수를 구하는 예제
cor(iris$Sepal.Width, iris$Sepal.Length)
# -0.1175698로 상관계수가 작아 두 값 사이에 큰 상관관계가 없지만 Sepal.Width가
# 커짐에 따라 Sepal.Length가 작아지는 경향이 있음을 알 수 있다.

# Species를 제외한 모든 컬럼이 피어슨 상관계수 살펴보기
cor(iris[, 1:4])
# 양수로 표시된 것은 상관관계가 큼을 나타낸다.

symnum(cor(iris[,1:4]))
# +로 표시된것이 상관관계가 있음을 표시

# 그래프로 보기
install.packages("corrgram")
library(corrgram)
corrgram(iris, upper.panel = panel.conf)
# 양의 상관관계 : 오른쪽 위에서 왼쪽 아래 빗금표시
# 색의 짙기는 상관계수의 크기를 뜻해 관계가 클수록 짙어진다.


### 추정 및 검정 ###
x <- rnorm(30)
t.test(x)
# 모평균 값 mean of x : oooo, 95%신뢰구간로 추정, 기무가설은 모평균은 0이다고
# p-value값이 0.05보다 커서 귀무가설을 기각하지 못하기에 모평균을 0으로 본다.
# 이는 신뢰구간에 0이 포함되어 있다는 것으로도 알 수 있는 내용


# 평균이 10, 분산이 1인 정규분포N(10, 1)에서 30개의 표본을 뽑아 모평균 구간 추정
x <- rnorm(30, mean = 10)
t.test(x, mu = 10)
# 책 355page 
# mu=10을 지정했으므로 모평균이 10인지에 대한 가설 검증이 수행되었는데, 
# p-value > 0.05며 신뢰구간이 10을 포함하므로 귀무가설을 기각하지 못한다.

# 독립 이표본 평균
# 두 모집단에서 각각 표본을 추출한 뒤 표본으로부터 두 모집단의 평균이 같은지 보기
sleep
sleep2 <- sleep[, -3] # ID빼고 나머지 sleep2에 저장
sleep2
#  수면제별 수면시간 증가량의 평균
tapply(sleep2$extra, sleep2$group, mean)
# 다른 방법
summaryBy(extra~group, sleep2)

var.test(extra ~ group, sleep2)
# var.test()는 두 집단의 분산비에 대한 가설 검정을 수행
# p-value가 0.05보다 커서 귀무가설 분산의 비가 1이다.를 기각할 수 없다.
# 분산비의 95%신뢰구간 안에 1이 포함되어 있어 분산비가 1임을 반박할 증거가 업다고
# 읽어도 된다.


# 분산비가 같다고 보고 t.test() 적용
# paired=FALSE : 독립 이표본 검정을 뜻함
# paired=TRUE : 짝지은 이표본 검정을 뜻함
# var.equal=TRUE : 두 집단의 모분산이 같은지 여부를 뜻함
t.test(extra ~ group, data = sleep2, paired=FALSE, var.equal=TRUE)
# p-value > 0.05로 모평균에 차이가 없다는 가설을 기각할 수 없다. 또는 신뢰구간이
# 0을 포함하여 평균에 차이가 없다고 읽어도 된다.


# 짝지은 이표본 평균
with(sleep, t.test(extra[group=1], extra[group=2], paired = TRUE))
# p-value < 0.05로 귀무가설 : 모평균의 차이가 0이다.를 기각한다.
# 따라서 두 수면제의 수면 시간 증가정도가 다르다고 결론을 내린다.

# 이표본 분산
with(iris, var.test(Sepal.Width, Sepal.Length))
# p-value < 0.05이기에 모분산에 차이가 없다(ratio=1)는 귀무가설을 기각한다.

# 일표본 분산
# 동전 앞위면 확률ㅡ 100번 던져 42번 앞면 나왔다. 확률이 50%가 아니라고 할 수 있는가
prop.test(42, 100)
# p-value > 0.05이므로 동전이 앞면이 나올 활률이 0.5라는 귀무가설을 기각할 수 없다.
binom.test(42,100)
# 귀무가설을 기각하지 못한다.

# 이표본 비율
# 두개의 동전을 각각 100회, 90회 던졌을 때 각각 앞면이 45회, 55회 나왔다고 하자
# 두 동전의 앞면이 나올확률이 같은지 검정
prop.test(c(45, 55), c(100,90))
# p-value < 0.05이기에 두 동전의 앞면이 나올 확률이 같다는 가설을 기각한다.
# 즉 확률이 서로 다르다.


































































