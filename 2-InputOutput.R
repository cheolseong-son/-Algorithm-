############# 키보드 입력#############
# 1) scan() : vector 입력 
# 2) edit() : 데이터 프레임 입력

a <- scan() # 숫자를 입력(엔터).입력을 중단할 경우 빈칸에 엔터
a

b <- scan(what = character())
b


df <- data.frame()
df <- edit(df)
df

#############파일 입력#################
#  read.csv()
# read.table()
# read.xlsx()
student <- read.table("../data/student.txt")
student

student1 <- read.table(file ="../data/student1.txt", header = T, sep = " ")
student

student2 <- read.table(file.choose(), header = T, sep = ";")
student2

student3 <- read.table(file ="../data/student3.txt",
                       header = T, na.strings = "-") # 공백 or 탭은 sep사용안해도 된다.
student3

student3 <- read.table(file ="../data/student3.txt",
                       header = T, na.strings = c("-", "+", "&")) # 공백 or 탭은 sep사용안해도 된다.
student3

# read.xlsx() 책 41 page
install.packages("xlsx")
library(rJava)
library(xlsx)
# studentx <- read.xlsx(file.choose(), sheetIndex = 1, encoding = "UTF-8") # sheetIndex 몇번째 인데스인지?

studentx <- read.xlsx(file.choose(), sheetName = "emp2", encoding = "UTF-8")
studentx
########### 화면 출력 ###############
# 변수명 
# cat()
# print()
x <- 10
y <- 20
z <- x + y
z
(z <- x + y)
# print("x + y의 결과는 " + as.character(z) + "입니다.") R에서는 안됨
cat("x + y의 결과는 ", as.character(z), "입니다.")


##########파일 출력 ###########
# write.table()
# write.csv() # 

write.table(studentx, "../data/stud1.txt") # 엑셀파일을 불러와 txt파일로 저장
write.table(studentx, "../data/stud2.txt", row.names = F) # 행의 순서 번호가 사라짐
write.table(studentx, "../data/stud3.txt", row.names = F, quote= F)

######### rda파일 출력 ###########
# save() : 저장
# load() : 불러 오기
save(studentx, file = "../data/stud6.rda")
rm(studentx) # ()안 변수 제거
studentx

load("../data/stud6.rda")
studentx

############# sink() #################
# 데이터 샘플
data()
?data()

data("iris")
head(iris)
tail(iris)
str(iris)

sink("../data/iris.txt")
head(iris)
tail(iris)
str(iris)

sink()
head(iris)



























