#setwd("C:/Users/Ibara Jin/Desktop/伊原尋/学习/STAT628统计实习/Module1")
library(dplyr)

data=read.table("Bodyfat.csv",sep=",",header=T)
head(data)

data_m2=data %>%
  transmute(vol=WEIGHT/DENSITY,
            age=AGE,
            necksq=NECK^2,
            chestsq=CHEST^2,
            waistsq=ABDOMEN^2,
            hipsq=HIP^2,
            thighsq=THIGH^2,
            kneesq=KNEE^2,
            anklesq=ANKLE^2,
            bicepsq=BICEPS^2,
            forearmsq=FOREARM^2)
head(data_m2)
plot(data_m2)
cor(data_m2[,3:11])

fit=lm(vol~necksq+chestsq+waistsq+hipsq+thighsq+kneesq+anklesq+bicepsq+forearmsq,
       data=data_m2)
summary(fit)
step=step(fit,direction = "forward")
summary(step)

data_m2=data_m2[-c(39,182),]
data_m2=data_m2[-c(39,182,159,175,206),]

fit1=lm(vol~.,data=data_m2[,c(1,6,8)])
fit2=lm(vol~.,data=data_m2[,c(1,6,9)])
fit3=lm(vol~.,data=data_m2[,c(1,6,10)])
fit4=lm(vol~.,data=data_m2[,c(1,6,11)])
summary(fit4)
par(mfrow=c(2,2))
plot(fit4)
vol_p=predict(fit4)

data_m2t=data[-c(39,182,159,175,206),] %>%
  mutate(vol_p=vol_p,
         den_p=WEIGHT/vol_p)
head(data_m2t)
fit_fin=lm(BODYFAT~0+den_p+ADIPOSITY+AGE,data = data_m2t)
summary(fit_fin)

data_m2s=data[-c(39,182,159,175,206),] %>%
  transmute(bodyfat=BODYFAT,
            base=mean(FOREARM),
            hip=HIP/FOREARM,
            hipsq=(HIP/FOREARM)^2,
            weight=WEIGHT,
            vol=WEIGHT/DENSITY,
            BMI=ADIPOSITY,
            age=AGE)
head(data_m2s)

fit_s1=lm(vol~hipsq+0,data=data_m2s)
summary(fit_s1)
vol_p=predict(fit_s1)
fit_s2=lm(weight~hip+0,data=data_m2s)
summary(fit_s2)
weight_p=predict(fit_s2)
data_m2s=data_m2s %>%
  mutate(den_p=weight_p/vol_p)
fit_sfin=lm(bodyfat~0+den_p+BMI+age,data = data_m2s)
summary(fit_sfin)
predict(fit_sfin)

# fit_sfin$coefficients[1]*fit_s2$coefficients[1]/fit_s1$coefficients[1]
# -20/96*95+21*1.5+0.12*22-1.96*sqrt(4.73)
