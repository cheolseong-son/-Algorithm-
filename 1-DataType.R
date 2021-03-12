######## 변수 #########

goods = "냉장고"
goods

#  변수 사용시 객체 형태로 사용하는 것을 권장
goods.name = "냉장고"
goods.code = "ref001"
goods.price = 60000

goods.name

# 값을 대입할 때에는 = 보다는 <-사용
goods.name <-"냉장고"
goods.code <-"ref001"
goods.price <- 6000


# 데이터 타입 확인
class(goods.name)
class(goods.price)

# 도움말 활용
help(sum) # 오른쪽에 도큐먼트가 나온다.
?sum

# 함수 사용법 
args(sum)
example(sum)

########데이터 타입############
# **스칼라*** (0차원) :숫자, 문자,분린(boolean), 팩터,
# NA 값이 없음 
# null: 값은 있지만 변수에 할당 안됨
# * 기본 자료형 : numeric, character, logical, factor
  * 특수 자료형 : NA, NULL, NaN, Inf

# **벡터** (1차원) : 기본 데이터 타입, 한 줄로 데이터를 모아 놓은 형태, 배열의 개념
# 리스트 (3차원) : list, 여러 가지 타입의 변수가 들어감
# 행렬(2차원) Matrix
# 데이터프레임(2차원) : DataFrame
# 배열(3차원) : array, 

# 배열특징 : 1) 같은 형식끼리 묶어준다. 2) 크기가 정해져 있다.
#            3) 삽입, 삭제가 불편       4) 동일한 자료형만 저장



##벡터(vector)##
# 1) 기본 자료 구조
# 2) 1차원 배열 : 같은 형식의 데이터타입이 들어감
# # C() : combine, seq(), reP(),
v <- c(1,2,3,4,5)
v
class(v)
(v <- c(1,2,3,4,5)) # () 묶어바로 출력
# 범위 지정
a <- c(1:5)
print(a)
mode(a)
class(c(1,2,3,4,"5"))
#  seq()
?seq
# 일련번호 만든다. 순서대로
seq(from = 1, to = 10, by =2 )
seq(1,10,2)

# rep()
(rep(1:3, 3))

# 벡터의 접근
a <- c(1:50) 
a[10:45]
length(a) 
a[10:(length(a)-5)] # 10부터 50-5까지 
a[10:length(a)-5]   # 10부터 50까지 하고 인데스5를 양쪽에서 빼준 것

b <- c(13,-5,20:23,12,-2:3)
b
b[1]
b[c(2,4)] # 2번째 4번째 인덱스

b[c(4,5:8,7)]
b[-1] #첫 인데스만 빼고 나머지
b[-2] # 2번째 인덱스 빼고 나머지
b[-c(2,4)] # 2,4 인덱스 빼고 나머지지

# 집합 연산
x <- c(1,3,5,7)
y <- c(3,5)
union(x,y)       # 합집합
setdiff(x, y)    # 여집합 
intersect(x, y)  # 교집합

union(x,y);  setdiff(x, y); intersect(x, y)
# ; 사용하여 한번에 실행


# 컬럼명 지정
age <-c(30,35,40)
names(age) <-  c("홍길동", "임꺽정", "신돌석")
age

# 특정 변수의 데이터 제거
age <- NULL
age
x <- c(2,3)
x
x <-(2:3)
x


# factor : 범주형 데이터
# =======================
# 범주형인지 숫자형인지 파악해야 한다.
# 범주형으 데이터 값들 사이에 크기비교 불가한명목형과 정치적 성향과 같은 순서형으로 나뉜다.
# 숫자형은 셀 수 있는 이산형과 연속형이 있다.
gender <- c("man", "women","women", "man", "man") # 중복이 있기에 범주로 묶을 수 있다.
gender # gender는 문자열로 묶인 벡터이다.
class(gender)
is.factor(gender)
ngender <- as.factor(gender)
ngender
class(ngender)
mode(ngender)
is.factor(ngender) #factor인지 확인
plot(ngender) # 기본 막대 그래프로 보여줌
table(ngender) # 빈도수 확인

?factor
ofactor <- factor(gender, levels = c("women", "man"), ordered = TRUE )
ofactor
# ======================================================================
# 행렬 (Matrix)
# 1) 행과 열의 2차원 배열
# 2) 동일한 데이터 타입만 저장 가능
# 3) matrix(), rbind(), cbind() # row, column
m <- matrix(c(1:5))
m
m <- matrix(c(1:11), nrow = 2) # nrow는 행을 두개로 함 열 방향으로 채움
m
m <- matrix(c(1:11), nrow = 2, byrow = T) # nrow는 행을 두개로 함 byrow 열방향으로 채움움
m
class(m)
mode(m)

# rbind(), cbind()
x1 <- c(3,4,50:52)
x2 <- c(30,5,6:8,7,8)
x1
x2
mr <- rbind(x1, x2)
mr

mc <- cbind(x1, x2)
mc
# matrix 차수 확인
x<- matrix(c(1:9), ncol=3)
x
length(x); nrow(x); ncol(x)
# 컬럼명 지정
colnames(x) <- c("one", "two", "three")
x
colnames(x)

# apply
apply(x, 1, max) # 1은 행별로 가장 큰 수 
apply(x, 2, max) # 2는 열별로 가장 큰 수
apply(x, 1, sum) #  행을 다 더하기
apply(x, 2, sum) #  열을 다 더하기

# 행렬 데이터 접근
aws = read.delim("../data/Aws_sample.txt", sep="#") # 상위폴더로 나가는 경로
head(aws) # 상위 6개만 불러옴
(aws[1,1]) # 1행 1열

x1<- aws[1:3, 2:4]
x1

x2<- aws[9:11, 2:4]
x2
class(x2)

cbind(x1, x2)
rbind(x1, x2)


# ======================================
# data.frame
# 1) DB의 table과 유사
# 2) R에서 가장 많이 사용되는 구조
# 3) 컬럼단위로 서로 다른 데이터 타입 사용 가능
# 4) data.frmae(), read.csv(), read.delim(), read.table(), ...

no <-c(1,2,3)
name <-c("hong", "lee", "kim")
pay <-c( 150, 250, 300)

emp <-data.frame(NO = no, Name=name, Payment = pay)
emp
#  read.csv(), read.table()
getwd() # 현재 작업폴도 위치 알려줌
setwd("../data") #  작업 위치 data로 바꿈
getwd()


txtemp <-read.table("emp.txt", header=T, sep=" ") # 경로 표시 없이 바로 사용 가능
txtemp
csvemp <- read.csv("emp2.csv")
csvemp

csvemp1 <- read.csv("emp2.csv", header = T, col.names=c("사번", "이름", "급여"))
csvemp1

csvemp2 <-read.csv("emp2.csv", header = F, col.names = c("사번", "이름", "급여"))
csvemp2 


View(csvemp2)
################################################################################
# 2020/11/27
################################################################################
# 접근
# =============================
# csvemp2[,1]
csvemp2$사번   # $사용하여 가져오기
class(csvemp2$사번)


# 구조 확인
str(csvemp2)

#  기본 통계량 확인 :summary
#===============================
summary(csvemp2)

# apply
df <- data.frame(x=c(1:5),y=seq(2, 10, 2), z =c("a", "b", "c", "d", "e"))
df
apply(df[,c(1,2)], 2, sum) # 2는 열
apply(df[,c(1,2)], 1, sum) # 1은 행

# 데이터의 일부 추출
x1 <- subset(df, x>=3)
x1
x2 <- subset(df, x>=2 & y <= 6)
x2
# 병합
height <- data.frame(id =c(1,2), h = c(180, 175))
weight <- data.frame(id =c(1,2), h = c(80, 75))

user <- merge(height, weight, by.x="id", by.y="id")
user
#########################################################
# array
# 1) 행, 열, 면의 3차원 배열 형태의 객체 생성
# 2) array()
v <- c(1:12)
v
arr <- array(v, c(3,2,3))
arr
# 접근
arr[, , 1] # 첫번째 면
arr[, , 2]
arr[, , 3]

# 추출
arr[2,2,1] # 2행 2열 1면

################################################################
# list
################################################################
# 1) key와 vlaue를 한쌍
# 2) python에서의 dict와 유사
# 3) list()
x0 <- 1 # 스칼라 값
x1 <- data.frame(var1=c(1,2,3,), var2= c('a','b','c'))
x3 <- matrix(c(1:12), ncol=2)
x4 <- array(1:20, dim = c(2,5,2))

x5 <- list(c1 =x1, c2=x2, c3= x3, c4=x4)
x5
x5$c1
x5$c2

# 책 69 page
list1<- list(e =c("lee",5), "이순신", 95)
list1
list1[1]   # 면에 접근 list$1과 비슷
list1[[1] # 면 안의 값에 접근

un <- unlist(list1)
un
class(un)

# apply : lapply(), sapply()
# lapply : 2차원 이상의 데이터만 입력을 받는다.
#          vector를 입력받기 위한 방법으로 사용, 반환형은 list형이다
# sapply : 반환형이 행렬 또는 벡터로 반환

a <- list(c(1:5))
b <- list(c(6:10))
a
b
c <-c(a,b) # 리스트 데이터를 벡터로 묶음음
c
class(c)
x <- lapply(c, max)
x
class(x)
x1 <-unlist(x)
x1
y <- sapply(c, max)
y

# 기타 데이터 타입
#######################################
#####날짜 
Sys.Date()
Sys.time()

a <- "20/11/27"
a
class(a)

b <- as.Date(a)
class(b)

c <- as.Date(a, "%y/%m/%d")
c




















