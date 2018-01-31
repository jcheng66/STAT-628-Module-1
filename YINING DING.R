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

fit=lm(vol~necksq+chestsq+waistsq+hipsq+thighsq+kneesq+anklesq+bicepsq+forearmsq,
       data=data_m2)
summary(fit)
step=step(fit,direction = "forward")
summary(step)

data_m2=data_m2[-c(159,175,206),]

plot(data_m2)
cor(data_m2[,3:11])

fit1=lm(vol~.,data=data_m2[,c(1,6,8)])
fit2=lm(vol~.,data=data_m2[,c(1,6,9)])
fit3=lm(vol~.,data=data_m2[,c(1,6,10)])
fit4=lm(vol~.,data=data_m2[,c(1,6,11)])
summary(fit4)
plot(fit4)
vol_p=predict(fit4)

data_m2t=data[-c(159,175,206),] %>%
  mutate(vol_p=vol_p,
         den_p=WEIGHT/vol_p)
head(data_m2t)
fit_fin=lm(BODYFAT~0+den_p+ADIPOSITY+AGE,data = data_m2t)
summary(fit_fin)





