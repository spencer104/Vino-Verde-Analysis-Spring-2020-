#04/28/2020 

rm(list = ls())
wine = read.csv("E:/Baruch/STat 300/redwinequality.csv")
head(wine)

#packages used 
#ggplot2 
#dplyr 
#GGally

caption = "Bar Graph to show distrbuion of quaility"
ggplot(wine, aes(quality)) + geom_bar()+ ggtitle(caption,)

# seperates wine by grade 3-4 is low qual, 5-6 is reg qual, and 7-8 is high qual 

x = (wine$quality < 5)                     
Qlow = wine[x,]
Qlow = Qlow %>% mutate(Grade="low")


y = ((wine$quality > 4) & (wine$quality < 7))
Qmid = wine[y,]
Qmid = Qmid %>% mutate(Grade="mid")

z=(wine$quality > 6)
Qhigh = wine[z,]
Qhigh = Qhigh %>% mutate(Grade="high")


wine = rbind(Qhigh,Qlow, Qmid) # bring all these data sets together so we can create new column grade which list each wine as high, mid, and low

# summary of each stats used in paper 
summary(wine)
summary(Qlow)
summary(Qmid)
summary(Qhigh)

# numebr of data points 
nrow(wine)
nrow(Qlow)
nrow(Qmid)
nrow(Qhigh)



wineHighMed = rbind(Qhigh, Qmid)                               # we are doing a two sampel t-test low HIgh, Alcahol
wineLowMed = rbind(Qlow, Qmid)
wineHighlow = rbind(Qhigh, Qlow)

# alcohol

vect = c(colnames(wine))
remove = c("Grade","quality")
vect %in% remove 
vect[! vect %in% remove]

# 

# t-tests for High MEdium quality wine 
for(i in 1:11){
  x = t.test(wineLowMed[,i] ~ wineLowMed$Grade,data = wineLowMed, var.equal = TRUE)
  if(x$p.value <= 0.05){ 
   print(vect[i])
   print("reject")
  } 
  else{ 
    print(vect[i])
    print("fails to reject")
    }
  print(t.test(wineLowMed[,i] ~ wineLowMed$Grade,data = wineLowMed, var.equal = TRUE))
}




# t-tests for High Low quality wine 
for(i in 1:11){
  x = t.test(wineHighlow[,i] ~ wineHighlow$Grade,data = wineHighlow, var.equal = TRUE)
  if(x$p.value <= 0.05){ 
    
    #print("reject")
  } 
  else{ 
    print(vect[i])
    print("fails to reject")
  }
  print(t.test(wineHighlow[,i] ~ wineHighlow$Grade,data = wineHighlow, var.equal = TRUE))
}


# t-tests for Medium low quality wine 
for(i in 1:11){
  x = t.test(wineHighlow[,i] ~ wineHighlow$Grade,data = wineHighlow, var.equal = TRUE)
  if(x$p.value <= 0.05){ 
    
    #print("reject")
  } 
  else{ 
    print(vect[i])
    #print("fails to reject")
  }
  #print(t.test(wineHighlow[,i] ~ wineHighlow$Grade,data = wineHighlow, var.equal = TRUE))
}


# our foward linear regression 
mdl = lm(quality ~ alcohol+ sulphates + pH + density + total.sulfur.dioxide + free.sulfur.dioxide + chlorides + residual.sugar + citric.acid + volatile.acidity + fixed.acidity , data = wine)

step(mdl, direction='backward')

# Plot for linear regression 
mdl1 = lm(formula = quality ~ alcohol + sulphates + pH + total.sulfur.dioxide + free.sulfur.dioxide + chlorides + volatile.acidity, data = wine)
plot(mdl1, lwd = 0.5)

