############ 조건문##############

# 난수 준비
x <- runif(1) # 0~1사이 균일 분포
?runif
x

# x가 0보다 크면 절대값(abs())으로 값 출력
if (x >0){
  print(abs(x))
}
# x가 0.5보다 작으면 1-x를 출력하고 그렇지 않으면 x를 출력
if (x<0.5){
  print(1-x)
  
}else{
  print(x)
}
# Page 97
if (x<0.5) print(1-x) else print(x)
# ifelse() : 함수
ifelse(x<0.5, 1-x,x)

# 다중 조건
avg <- scan()

if (avg >= 90){
  print("당신의 학점은 A입니다. ")
}else if (avg>=80){
  print('당신의 학점은 B입니다.')
}else if (avg>=70){
  print('당신의 학점은 C입니다.')
}else if (avg>=60){
  print('당신의 학점은 D입니다.')
}else (avg>=80){
  print('당신의 학점은 F입니다.')
}


#=================================
# switch(비교문, 실행문, 실행문, ...)
a <- "중1" 
switch (a, "중1" = print("14살", "중2"=print("15살"))) 
switch (a, "중1" = "14살", "중2"= "15살") 
# a가 중1이면 14살을 출력해라. a가 중2이면 15살을 출력해라. 

b <- 3
switch(b, "14살", "15살", "16살") # 케이스가 정해지지 않으면 인데스 값을 출력


empname <- scan(what = "") # what = ""으로 써도 문자로 입력 받음
switch (empname, hong=250, lee=350, kim=200, kang=400
)
##############################################################################
##############################################################################
# 다중 조건 switch로 바꿔보기
# 101 page

avg <- scan()%/% 10
result <- switch(as.character(avg), "10"="A", "9" ="A",
                 "8"="B", "7"="C", "6"="D", "f")
cat("당신의 학점은", result, "입니다.")

# which()
x <- c(2:10)
x
which(x==3) # x에 3이 있는지? 있으면 몇번째에 있는지 알려 줌
x[which(x==3)]

m <- matrix(1:12, 3, 4) # 1부터 12까지 3행 4열 데이터
m

which(m%%3 == 0) # m을 3으로 나눈 값을 출력
?which

which(m%%3 == 0, arr.ind = T)  # 2차원에서 행열로 위치 알려 줌


no <- c(1:5)
name <- c("홍길동", "유비", "관우", "장비", "조자룡")
score <-c(85,78,89,90,74)
exam <- data.frame(학번=no, 이름=name, 성적=score)
exam

# 이름이 "장비"인 사람 검색
which(exam$이름=="장비비")
exam[4,]
exam[which(exam$이름=="장비비"),]


# 가장 높은 점수를 가진 학생은?
exam[which.max(exam$성적),] #  max
exam[which.min(exam$성적),] #  min

# any(), all()
x <- runif(5) # 난수 5개
x
# x값 중에서 0.8이상이 있는가?
any(x>=0.8)
# x의 값이 모두 0.9이하인가?
all(x<=0.9)

# 책 98 page
############### 반복문 #################
# for
sum <- 0
# 1~10까지 합계
for (i in seq(1,10, by=1)) {
  sum <- sum + i
}
sum

for (i in 1:10)  sum <- sum + i
sum

######################################################
###################### 함 수 #########################
######################################################
 
# 인자 없는 함수
test1 <- function(){
  x <- 10
  y <- 10
  return(x*y)
}
 test1()

# 인자 있는 함수
test2 <- function(x, y){
  a = x
  b = y
  return(sum(a,b))
}
test2(10,20)
test2(y=20, x=10)

# 가변 인수 : ...
test3 <- function(...){
# print(list(...))
  for (i in list(...))print(i)
}
test3(10)
test3(10, 20)
test3('3', "hong", 10)


test4 <- function(a, b, ...){
  print(a)
  print(b)
  print("------------------")
  test3(...)
}

test4(10,20,30,40)
##################### 문자열 함수#######################
# stringr : 정규 표현식 활용
install.packages("stringr")
library(stringr)

str1 <- "홍길동35이순신45임꺽정25"
str_extract(str1, "\\d{2}") # 숫자 출력하고 싶을 때, {2}숫자 자릿수
str_extract_all(str1, "\\d{2}") # 리스트로 출력
str_extract_all(str1, "[1-9]{2}")
class(str_extract_all(str1, "[1-9]{2}"))


str2 <- " hong1051이순신1002임꺽정25강감찬2005you2003cheol558"
str_extract_all(str2, "[a-zA-Z-가-힣]{3}")
str_extract_all(str2, "[a-zA-Z-가-힣]{3,}") # 영어, 한글 세글자 이상 출력

str_length(str2)  # 문자열 길이
length(str2)      # 

str_locate(str2, "이순신")
str_c(str2, "유비55") # 변수 추가 안됨
str2

str3<- "hongkd105, leess1002, you25, TOM400, 강감찬2005"
str_split(str3, ",")


# 기본 함수
sample <- data.frame(c1=c("abc_abcdefg", "abc_ABCDEF", "ccd"), c2=1:3)
sample

nchar(sample [1,1])
which(sample[,1]=="ccd")
toupper(sample[1,1])
tolower(sample[2,1])
substr(sample[,1], start=1, stop=2)

install.packages("splitstackshape")
library(splitstackshape)

cSplit(sample,splitCols = "c1", sep = "_")

# 기본 함수
paste0(sample[,1], sample[,2])
paste(sample[,1], sample[,2], sep = "@@")























