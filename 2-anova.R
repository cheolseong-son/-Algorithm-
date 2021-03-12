# ===== 조 건 =====

# 1. 결과값이 연속변수
#     아닐 경우 : Kruskal-wllis H test
# 2. 정규 분포 여부
#     아닐 경우 : Kruskal-wllis H test
# 3. 등분산 여부
#     아닐 경우 : welch's anova
# 모든 조건을 만족하는 경우 anova를 사용하고 사루 검정은 tukey 사용


# ============= 사례1 : one way ANOVA ================

library(moonBook)
View(acs)

# LDLC : 저밀도 콜레스테롤 수치 : 종속변수
# Dx(진단 결과) : STEMI(급성심근경색), NSTEMI(만성심근경색), unstable angina : 독립변수
# 
moonBook::densityplot(LDLC ~ Dx, data=acs)

# 정규분포 검정
with(acs, shapiro.test(LDLC[Dx=="NSTEMI"])) # p-value = 1.56e-08: 0.05보다 작기에 정규분포 아니다.
with(acs, shapiro.test(LDLC[Dx=="STEMI"])) # p-value = 0.6066 : 정규분포이다.
with(acs, shapiro.test(LDLC[Dx=="Unstable Angina"])) # p-value = 2.136e-07 : 정규분포 아니다..

out = aov(LDLC ~ Dx, data = acs) # anova 함수
out
# Residuals(잔차, 오차, 편차) : 평균에서 얼마만큼 멀어져 있는지 구한 것 
shapiro.test(resid(out)) # p-value = 1.024e-11 < 0.05 정규분포가 아니다.

# 등분산 검정
bartlett.test(LDLC ~Dx, data = acs)
# p-value = 0.1857: 0.05보다 크기에 등분산 있음

# 정규분포이고 등분산일 경우
out = aov(LDLC ~Dx, data = acs)
summary(out)
# 0.00377 ** 0.05보다 작기에 차이가 있다. 별의 갯수는 차이의 강도이다.

# 연속변수가 아니거나 정규분포가 아닌 경우
kruskal.test(LDLC ~Dx, data = acs)
# p-value = 0.004669 0.05보다 작기에 정규분포가 아니며 차이가 있다.

# 등분산이 아닐 경우
oneway.test(LDLC ~Dx, data = acs, var.equal = F)
# p-value = 0.007471, 0.05보다 작기에 등분산 아니며 차이가 있다.
?oneway.test

# 사후 검정
#  aov()를 사용했을 경우 : TukeyHSD
TukeyHSD(out)
# p adj 값이 0.05보다 크기에 차이가 없고 작으면 차이가 있다.

# Kruskal.test() 를 사용했을 경우
install.packages("pgirmess")
library(pgirmess)

kruskalmc(acs$LDLC, acs$Dx)
# difference : True는 차이가 있음, False는 차이가 없음

str(InsectSprays)
View(InsectSprays)

moonBook::densityplot(count ~ spray, data = InsectSprays)
kruskalmc(InsectSprays$count, InsectSprays$spray)

# kruskalmc로 안되면 posthocTGH 사용
install.packages("userfriendlyscience")
library(userfriendlyscience)
posthocTGH(y=InsectSprays$count, x=InsectSprays$spray, method = "games-howell")

posthocTGH(x=acs$Dx, y=acs$LDLC, method = "games-howell")

# oneway.test() 를 사용했ㅇㄹ 경우의 사후 검정
install.packages("nparcomp")
library(nparcomp)

result <- mctp(LDLC ~ Dx, data = acs)
summary(result)
# Analysis의  p.Value를 보면 된다.

# ================= 실 습 1 ==================
head(iris)
# 품종별로 Sepal.width의 평균차이가 있는가? 
# 만약 있다면 어느 품종과 차이가 있는가

str(iris)
# 정규분포 확인
out <- aov(Sepal.Width ~ Species, data = iris)
shapiro.test(resid(out)) # 정규분포이다.

# 등분산 여부
bartlett.test(Sepal.Width ~ Species, data = iris) # 차이가 있다.

# 사후 검정
TukeyHSD(out)
# p adj : 0.05보다 작기에 세게 모두 차이가 있지만
# virginica-versicolor이 덜 차이가 있다.


# ================ 실 습 2 ====================
mydata <- read.csv("../data/anova_one_way.csv")
View(mydata)

# 시, 군, 구에 따라서 합계 출산율의 차이가 있는가? 
# 있다면 어느것과 차이가 있는가

# 정규분포로 봐도 무방
to_data <- aov(birth_rate ~ ad_layer, data = mydata)
shapiro.test(resid(to_data)) 
#  p-value = 5.788e-07 : 0.05보다 작기에 정규분포가 아니다.

# 정규분포가 아닌 경우는 Kruskal로 빠져야 한다.
kruskal.test(birth_rate ~ ad_layer, data = mydata)

# 정규분포라고 가정한 경우
bartlett.test(birth_rate ~ ad_layer, data = mydata)
# 차이가 있다.
# 차이가 없으면 사후검정 필요가 없음
summary(to_data)
moonBook::densityplot(birth_rate ~ ad_layer, data = mydata)

# kruskal일 경우의 사후검정
library(pgirmess)
library(userfriendlyscience)
kruskalmc(mydata$birth_rate, mydata$ad_layer)
# 자치구-자치군 87.57423     25.57137       TRUE : 차이가 있다.
# 자치구-자치시 72.67130     26.11097       TRUE : 차이가 있다.
# 자치군-자치시 14.90293     25.00975      FALSE : 차이가 약간의 차이가 있다.

posthocTGH(x=mydata$ad_layer, y=mydata$birth_rate, method = "games-howell")
# 세개 모두 차이가 있다.

# aov일 경우, 정규분포일 때
TukeyHSD(to_data)
# 세개 다 차이가 있다. 시-군은 나머지보다 약간의 차이가 있다.


# ========================== 실 습 3 ==============================
# 실습 데이터 : www.kaggle.com

telco <- read.csv("../data/WA_Fn-UseC_-Telco-Customer-Churn.csv")
View(telco)
# R에서 정규분포 검사는 5000개까지만 가능
# 정규분포 안해도 됨

# 독립변수 : paymentMethod(bank transfer, Credit Card, Electronic Check,
#            Mailed Check)
# 종속변수 : Total Charge(전체 지불 금액)


unique(telco$PaymentMethod) # 지불 방식 확인

# 각 지불방식별로 갯수(인원수)와 평균 금액을 조회

telco %>% select(PaymentMethod, TotalCharges) %>% 
  group_by(PaymentMethod) %>% 
  summarise(n=n(), mean=mean(TotalCharges, na.rm = T)) 
# na.rm T: 결측치 삭제하고

moonBook::densityplot(TotalCharges ~ PaymentMethod, data = telco)

# 정규분포 여부
with(telco, shapiro.test(TotalCharges[PaymentMethod =="Bank transfer (automatic)"]))
# p-value < 2.2e-16 : 정규분포가 아니다.
with(telco, shapiro.test(TotalCharges[PaymentMethod =="Credit card (automatic)"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod =="Electronic check"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod =="Mailed check"]))
# 전부 정규분포가 아니지만 데이터 갯수가 많기에 정규분포로 가정하고 풀어도 된다.

# 한번에 정규분포 확인하는 방법
out <- aov(TotalCharges ~ PaymentMethod, data = telco)
shapiro.test(resid(out)) # 데이터가 너무 커서 확인 불가!!

x <- telco$TotalCharges[telco$PaymentMethod == "Bank transfer (automatic)"]
x
x1 <- sample(x, 30, replace = F) # replace = F : 한번 뽑은건 안 뽑히게 랜덤 30개
shapiro.test(x1) # 100, 30, 정규분포 아님, 숫자가 작아지면 정규분포가 된다. 

# 앤더슨 달링 테스트
install.packages("nortest")
library(nortest)
# nortest::ad.test(out) # 길이가 같아야 한다.
# 

# 등분산 테스트
bartlett.test(TotalCharges ~ PaymentMethod, data = telco)
# p-value < 2.2e-16 : 등분산이 아니다.

# 등분산이 아니기에 wilch's anova 사용해야 한다.
oneway.test(TotalCharges ~ PaymentMethod, data = telco, var.equal = F )
# p-value < 2.2e-16 : 평균의 차이가 있다.

# 어디서 어떻게 차이가 있는지 **사후검정**을 해야한다.
library(nparcomp)
result <- mctp(TotalCharges ~ PaymentMethod, data = telco)
summary(result)
# 결과 해석 : p.Value 0.95077 95%로 2와 1일 같고 나머지가 차이가 있음을 알 수 있다.

plot(result) # 0에 가까울수록 차이가 없는것이다. 0ne_way _anova  방식

# 정규분포가 아니란 상황에서 테스트를 한다면
kruskal.test(TotalCharges ~ PaymentMethod, data = telco)

kruskalmc(telco$TotalCharges, telco$PaymentMethod)
# True : 차이가 있다. False : 차이가 없다.

library(ggplot2)
ggplot(telco, aes(telco$PaymentMethod, telco$TotalCharges)) + geom_boxplot()

# ===================== 사례 2 ========================

mydata <- read.csv("../data/anova_two_way.csv")
View(mydata)
# multichild : 다가구 자녀 지원
# 독립변수가 2개 


out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, 
           data=mydata)

shapiro.test(resid(out))
# p-value = 2.862e-06 : 정규분포가 아니지만 데이터 갯수가 많기에 정규분포로 
# 가정해도 무방하다.  ad_layer:multichild : 상호변수

summary(out)
 
# 사후검정 
result <- TukeyHSD(out)
plot(result)
result
# 해석 : 차이가 있는 부분은 군에서 지원이 다른 시, 구에 지원하는 것보다 효과적
#         
ggplot(mydata, aes(birth_rate, ad_layer, col=multichild)) + geom_boxplot()


# 상호변수를 제거
out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, 
           data=mydata)

shapiro.test(resid(out))
summary(out)
TukeyHSD(out)


# ======================= 실습 ===========================

telco <- read.csv("../data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# 독립변수 : Paymentmethod, contract
# 종속 변수 : TotalCharges


unique(telco$Contract)
# =============강사님 풀이============================================================
model <- aov(TotalCharges ~ PaymentMethod + Contract + PaymentMethod:Contract, 
             data = telco)
# 상호변수 *, :
model
summary(model)
# 모두 차이가 있다. 유의하다.

# 사후검정
result <- TukeyHSD(model)
result
# 해석
# $Contract에서  p adj 차이가 있음을 알 수 있다.
plot(result) # 해석하기 힘들다.
library(ggplot2)
ggplot(telco, aes(PaymentMethod, TotalCharges, col=Contract)) + geom_boxplot()
# 

# ============= 학생 풀이 ================================================================
# 사후검정
library(nparcomp)
result <- mctp(TotalCharges ~ PaymentMethod + Contract + PaymentMethod:Contract, data = telco)
summary(result)
# Credit card (automatic)-Bank transfer (automatic) 간 0.950769 신뢰도 만큼 총 요금 차이가 거의 없다.
# 나머지는 차이가 크다

TukeyHSD(out)

# 해석
# 어떤 결제 양식이든 2년 단위 구독이 가장 요금을 많이 벌어들인다고 추측할 수 있다.
# 우편 수표 지불의 경우 고객 수가 적은 만큼 각 Contract 별 차이가 적다.
# 그러나 나머지 지불 방식의 경우 월 단위 - 년 단위 - 2년 단위 순으로 Contract 별 요금 총액이 커진다.
# 특히 전자 수표 지불의 경우 가장 큰 Contract 별 차이를 보였다.
# 따라서 신규 고객 유치 및 기존 고객의 구독 기간 연장 시
# 월 단위 구독 보다는 1, 2년 단위 구독을 유도하는 것이 총 요금 증가에 긍정적인 영향를 미친다고 예측할 수 있다.
# 또한 추후 이용객 수도 적고 총 요금도 적은 우편 수표의 경우 폐지하거나 다른 지불 방식으로 변경하는 것을 유도할 필요가 있다.
# 여기 까지 two way방식


# 시간에 따라 여러번 검정 Two Way Anova

#------------------------------------- 사 례 3 ---------------------------------
# ==============================================================================
# RM ANOVA 
# 구형성(Sphericity) : 이미 독립성이 깨졌으므로 최대한 독립성과 무작위성을
#                      확보하기 위한 조건

#   1) 가정 : 반복 측정된 자료들의 시차에 따른 분산이 동일
#   2) MOuchly의 단위 행렬 검정 : p-value가 0.05보다 커야 함. 귀무가설로 가야 함
#   3 Greenhouse-Geisser와 Huynh-Feidt값이 1에 가까울수록 구형성이 타당


df = data.frame()
df = edit(df)
df 
str(df)

means <- c(mean(df$pre), mean(df$after3m), mean(df$after6m))
means

install.packages("gplots")
library(gplots)
plotCI(x=means, type="l", ylab = "score", xlab = "month", 
       main = "RM anova test")

multimodel <- lm(cbind(df$pre, df$after3m, df$after6m) ~ 1) # 선형 모델
trials <- factor(c("pre", "after3m", "after6m"), ordered = F)

library(car)

model1 <- Anova(multimodel, idata = data.frame(trials),
                idesign = ~trials, type = "III") # type : 제곱합, 
?Anova
summary(model1, multivariate=F)
# p-value가 0.05, 
# 사후 검정
library(reshape2)
df2 <- df
df2 <- melt(df2, id.vars = "id")
df2
colnames(df2) <- c("id", "time", "value") # 이름 바꾸기
df2
# 그래프로 그리기
# factor로 바꾸기
df2$id <- factor(df2$id)
df2$time <- factor(df2$time)
str(df2)
ggplot(df2, aes(time, value)) + geom_line(aes(group=id, col=id)) + 
  geom_point(aes(col=id))
library(dplyr)
# 평균 변화 그래프
df2.mean <- df2 %>% group_by(time) %>% 
  summarise(mean=mean(value), sd=sd(value))
ggplot(df2.mean, aes(time, mean)) + geom_point() + geom_line(group=1)

out <- aov(value ~ time + Error(id/time), data = df2)
summary(out)
# 사후 검정 전용함수
with(df2, pairwise.t.test(value, time, paired = T, p.adjust.method = "bonferroni" ))
# p.adjust.method = : 오류값이 증가되지 않게 하기위해 사용 
# 1 : pre, 2 : after3m, 3 : after6m ::::차이가 있음


#============================ 실 습 =======================================
# 7명의 학생이 총 4번의 시험을 보았다. 평균차이가 있는가? 있으면 어느것과 차이가 있는가?

ow <- read.csv("../data/onewaySample.csv", header = T)
ow <- ow[, 2:6] # x데이터 빼기기
ow

# 평균
ow_mean <- c(mean(ow$score0), mean(ow$score1), mean(ow$score3), mean(ow$score6))
ow_mean
plotCI(x=ow_mean, type = "l", ylab = "score", xlab = "month", 
       main = "RM anova test")
plot(ow_mean, type="o", lty=2, col=2)

multimodel <- lm(cbind(ow$score0, ow$score1, ow$score3, ow$score6) ~ 1) # 선형 모델
trials <- factor(c("score0", "score1", "score3","score6"), ordered = F)


model1 <- Anova(multimodel, idata = data.frame(trials),
                idesign = ~trials, type = "III") 

summary(model1, multivariate=F)
library(tidyr)
owlong <- gather(ow, key = "ID", value = "score") #long형으로 바꾸기
View(owlong)
owlong <- owlong[8:35, ] # ID 값 삭제후 나머지 사용

out <- aov(score ~ ID, data = owlong)
shapiro.test(resid(out))
summary(out)
# 차이가 있음
with(owlong, pairwise.t.test(score, ID, paired = T, p.adjust.method = "bonferroni" ))

# 다른 방법
TukeyHSD(out)

# 0.05 / 4 = 0.0125 값으로 측정을 해야한다. 본페르니 검정

# ================================ 실 습 2 =====================================
#-------------- 비모수일 경우(서열변수 or  정규분포가 아닌경우) ----------------
# Paired T-Test : Wilcoxen signed rank test
# RM anova : Friedman test

?friedman.test


RoundingTimes <-matrix(c(5.40, 5.50, 5.55,
           5.85, 5.70, 5.75,
           5.20, 5.60, 5.50,
           5.55, 5.50, 5.40,
           5.90, 5.85, 5.70,
           5.45, 5.55, 5.60,
           5.40, 5.40, 5.35,
           5.45, 5.50, 5.35,
           5.25, 5.15, 5.00,
           5.85, 5.80, 5.70,
           5.25, 5.20, 5.10,
           5.65, 5.55, 5.45,
           5.60, 5.35, 5.45,
           5.05, 5.00, 4.95,
           5.50, 5.50, 5.40,
           5.45, 5.55, 5.50,
           5.55, 5.55, 5.35,
           5.45, 5.50, 5.55,
           5.50, 5.45, 5.25,
           5.65, 5.60, 5.40,
           5.70, 5.65, 5.55,
           6.30, 6.30, 6.25),
         nrow = 22,
         byrow = TRUE,
         dimnames = list(1 : 22,
                         c("Round Out", "Narrow Angle", "Wide Angle")))
View(RoundingTimes)
# melt, gather로 함수묶어 준다.
rt <- melt(RoundingTimes)
rt
# 정규분포 확인
out <- aov(value ~ Var2, data = rt)
shapiro.test(resid(out))
#  p-value = 0.001112 : 0.05보다 작기에 정규분포가 아니다.
# 그래프로 먼저 확인
boxplot(value ~ Var2, data = rt)
friedman.test(RoundingTimes) #  p-value = 0.003805 : 차이가 있다.

#  사후검정
# https://www.r-statistics.com/2010/02/post-hoc-analysis-for-friedmans-test-r-code/

friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # formu is a formula of the shape:     Y ~ X | block
  # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
  
  # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
  
  
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")
    library(coin)
  }
  
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  
  
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]    # In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,    ### all pairwise comparisons
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  
  
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")    # this is the post hoc of the friedman test
      
      
      # plotting
      if(to.plot.parallel & to.plot.boxplot)    par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)    # adding some spacing from both sides of the plot
        
        if(color.blocks.in.cor.plot)
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }
        
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }
        
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  ,
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide")     #[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))    # adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
        
      }
      
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}
      return(list.to.return)
      
    }    else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }
  }
  
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}


install.packages("coin")
library(coin)

friedman.test.with.post.hoc(value ~ Var2 | Var1, rt)

# 0.05/3 = 0.01666667보다 작아야 대립가설이다.크면 차이가 없는것이다.


# =============================== 실 습 4 ======================================
# --------------------------- Two Way RM ANOVA ---------------------------------

df <- read.csv("../data/10_rmanova.csv")
View(df)
#  데이터를 long형으로 바꾸기
df1 <- reshape(df, direction = "long", varying = 3:6, sep = "")
df1
# long형으로 바꾸는 다른 방법
dfm <- melt(df, id=c("group", "id"), variable.name = "time", value.name = "month")
dfm

# 그패르 화가인 작업을 위해 factor 변환
df1$group <- factor(df1$group)
df1$id <- factor(df1$id)
df1$time <- factor(df1$time)
str(df1)
# 1은 실험군 2는 대조군
interaction.plot(df1$time, df1$group, df1$month)

out <- aov(month ~ group*time, data = df1)
summary(out)
#  Pr(>F)  모두 차이가 있다.
# 사후 검정
df_0 <- df1[df1$time == "0",]
df_1 <- df1[df1$time == "1",]
df_3 <- df1[df1$time == "3",]
df_6 <- df1[df1$time == "6",]

t.test(month ~ group, data = df_0) # p-value = 0.8076 : 0.05보다 크고시작지점에서는 차이가 없다.
t.test(month ~ group, data = df_1) # p-value = 0.01962 : 0.0083 보다 크기에 차이가 없다.
t.test(month ~ group, data = df_3)
t.test(month ~ group, data = df_6)
# 0.05/6 =  : p-value가 0.0083333보다 작을때를 차이가 있다고 봐야한다.
# 독립변수 2개, 종속 4개 4C2 확율로 계산해야 한다.







