N=function(n){
  a=NULL
  
  # 控制模擬次數
  for (i in 1:n){
    b=0
    a[i]=1
    
    # 判斷甚麼時候均勻分布的隨機變數相加大於1
    # 並用a[i]紀錄迴圈次數
    while(b<1){
      x=runif(1)
      b=b+x
      a[i]=a[i]+1 
    }
  }
  return(a)
}

x1=N(1000)
x2=N(2000)
x3=N(5000)
x4=N(10000)
x5=N(100000)


x15=cbind(x1,x2,x3,x4,x5)
mean15=apply(x15,2,mean)
sd15=apply(x15,2,sd)
# 不能用apply


par(mfrow=c(1,2))
plot(mean15)
plot(sd15)


# 用ggplot2畫圖 --------------------------------------------------------------

library(ggplot2)
library(dplyr)
mean15 <- mean15 %>% as.data.frame()
mean15 <- mutate(mean15, ..=c("x1","x2","x3","x4","x5"))
colnames(mean15) <- c("N", "No.")
mean15 <- select(mean15, No., N)

ggplot(data = mean15, aes(x = No., y = N))+
  geom_point(size = 3)+
  ylim(3.6, 4)

sd15 <- sd15 %>% as.data.frame()
sd15 <- mutate(sd15, ..=c("x1","x2","x3","x4","x5"))
colnames(sd15) <- c("sd", "No.")
sd15 <- select(sd15, No., sd)

ggplot(data = sd15, aes(x = No., y = sd))+
  geom_point(size = 3)+
  ylim(0.83,1)


# 參考資料 --------------------------------------------------------------------
# http://blog.fens.me/r-apply/
# 
# apply函數
# apply函數是最常用的代替for迴圈的函數。
# apply函數可以對矩陣、資料框、陣列(二維、多維)，
# 按行或列進行迴圈計算，對子元素進行反覆運算，
# 並把子元素以參數傳遞的形式給自訂的FUN函數中，並以返回計算結果。
# 
# 函式定義：
# apply(X, MARGIN, FUN, ...)
# 
# 參數列表：
# X:陣列、矩陣、資料框
# MARGIN: 按行計算或按按列計算，1表示按行，2表示按列
# FUN: 自訂的調用函數
# …: 更多參數，可選





