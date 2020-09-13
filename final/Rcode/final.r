# I pledge my honor that I have abided by the Stevens Honor System.
# Eric Altenburg, David Horowitz, Lachlan Mountjoy

install.packages("psych")
install.packages("fBasics")

case <- c(1
          ,2
          ,3
          ,4
          ,5
          ,6
          ,7
          ,8
          ,9
          ,10
          ,11
          ,12
          ,13
          ,14
          ,15
          ,16
          ,17
          ,18
          ,19
          ,20
          ,21
          ,22
          ,23
          ,24
          ,25
          ,26
          ,27
          ,28
          ,29
          ,30)

taste <- c(12.3
           ,20.9
           ,39.0
           ,47.9
           ,5.6
           ,25.9
           ,37.3
           ,21.9
           ,18.1
           ,21.0
           ,34.9
           ,57.2
           ,0.7
           ,25.9
           ,54.9
           ,40.9
           ,15.9
           ,6.4
           ,18.0
           ,38.9
           ,14.0
           ,15.2
           ,32.0
           ,56.7
           ,16.8
           ,11.6
           ,26.5
           ,0.7
           ,13.4
           ,5.5)

acetic <- c(4.543
            ,5.159
            ,5.366
            ,5.759
            ,4.663
            ,5.697
            ,5.892
            ,6.078
            ,4.898
            ,5.242
            ,5.740
            ,6.446
            ,4.477
            ,5.236
            ,6.151
            ,6.365
            ,4.787
            ,5.412
            ,5.247
            ,5.438
            ,4.564
            ,5.298
            ,5.455
            ,5.855
            ,5.366
            ,6.043
            ,6.458
            ,5.328
            ,5.802
            ,6.176)

h2s <- c(3.135
         ,5.043
         ,5.438
         ,7.496
         ,3.807
         ,7.601
         ,8.726
         ,7.966
         ,3.850
         ,4.174
         ,6.142
         ,7.908
         ,2.996
         ,4.942
         ,6.752
         ,9.588
         ,3.912
         ,4.700
         ,6.174
         ,9.064
         ,4.949
         ,5.220
         ,9.242
         ,10.199
         ,3.664
         ,3.219
         ,6.962
         ,3.912
         ,6.685
         ,4.787)

lactic <- c(0.86
            ,1.53
            ,1.57
            ,1.81
            ,0.99
            ,1.09
            ,1.29
            ,1.78
            ,1.29
            ,1.58
            ,1.68
            ,1.90
            ,1.06
            ,1.30
            ,1.52
            ,1.74
            ,1.16
            ,1.49
            ,1.63
            ,1.99
            ,1.15
            ,1.33
            ,1.44
            ,2.01
            ,1.31
            ,1.46
            ,1.72
            ,1.25
            ,1.08
            ,1.25)

cheese <- data.frame(case, taste, acetic, h2s, lactic)

# 11.53
summary(cheese$taste)
library(psych)
describe(cheese$taste)

summary(cheese$acetic)
library(psych)
describe(cheese$acetic)

summary(cheese$h2s)
library(psych)
describe(cheese$h2s)

summary(cheese$lactic)
library(psych)
describe(cheese$lactic)

stem(cheese$taste)
stem(cheese$acetic)
stem(cheese$h2s)
stem(cheese$lactic)

qqnorm(cheese$taste, main = "Normal Probability Plot (Taste)")
qqline(cheese$taste)

qqnorm(cheese$acetic, main = "Normal Probability Plot (Acetic)")
qqline(cheese$acetic)

qqnorm(cheese$h2s, main = "Normal Probability Plot (H2S)")
qqline(cheese$h2s)

qqnorm(cheese$lactic, main = "Normal Probability Plot (Lactic)")
qqline(cheese$lactic)
library(fBasics)
qqnormPlot(cheese$lactic)

#11.54
plot(cheese$taste, cheese$acetic)
plot(cheese$taste, cheese$h2s)
plot(cheese$taste, cheese$lactic)
plot(cheese$acetic, cheese$h2s)
plot(cheese$acetic, cheese$lactic)
plot(cheese$h2s, cheese$lactic)

cor(cheese)
cor.test(cheese$taste, cheese$acetic)$p.value
cor.test(cheese$taste, cheese$h2s)$p.value
cor.test(cheese$taste, cheese$lactic)$p.value
cor.test(cheese$acetic, cheese$h2s)$p.value
cor.test(cheese$acetic, cheese$lactic)$p.value
cor.test(cheese$h2s, cheese$lactic)$p.value

#11.55 Taste as the reponse variable and acetic as explanatory
lmAT <- lm(taste ~ acetic, data=cheese)
plot(acetic, taste)
abline(lmAT)
summary(lmAT)

plot(lmAT)

plot(lmAT$residuals, h2s)
plot(lmAT$residuals, lactic)

#11.56 Using Taste as reponse and H2S as the explanatory
lmHT <- lm(taste ~ h2s, data=cheese)
plot(h2s, taste)
abline(lmHT)
summary(lmHT)

plot(lmHT)

plot(lmHT$residuals, acetic)
plot(lmHT$residuals, lactic)

#11.57 Using taste as reponse and Lactic as the explantory
lmLT <- lm(taste ~ lactic, data=cheese)
plot(lactic, taste)
abline(lmLT)
summary(lmLT)

plot(lmLT)

plot(lmLT$residuals, acetic)
plot(lmLT$residuals, h2s)

#11.58 - no code required

#11.59
lm59<-lm(cheese$taste ~ cheese$acetic + cheese$h2s)
summary(lm59)
plot(lm59)

#11.60
lm60<-lm(taste ~ cheese$h2s + cheese$lactic)
summary(lm60)
plot(lm60)

#11.61
lm61 <- lm(taste ~ cheese$h2s + cheese$lactic + cheese$acetic)
summary(lm61)
plot(lm61)
