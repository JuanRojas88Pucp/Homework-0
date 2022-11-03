#Analisis de componentes principales (PCA)
USArrests
prcomp(USArrests)
prcomp(USArrests, scale. = TRUE)
plot(prcomp(USArrests, scale. = TRUE))
summary(prcomp(USArrests, scale. = TRUE))
biplot(prcomp(USArrests, scale. = TRUE))

#Regresión lineal
year <- rep(2008:2010, each = 4)
quarter <- rep(1:4,3)
cpi <- c(162.2, 164.6, 166.5, 166.0, + 166.2, 167.0, 168.6, 169.5, + 171.0, 172.1, 173.3, 174.0)
plot(cpi, xaxt = "n", ylab = "CPI", xlab = "")
axis(1,labels = paste(year, quarter, sep = "Q"), at=1:12, las=3)
cor(year,cpi)


fit <- lm(cpi ~ year + quarter)
fit

summary(fit)

plot(fit)
par(mfrow = c(2,2))

library(scatterplot3d)
s3d <- scatterplot3d(year, quarter, cpi, highlight.3d = T, type = "h", lab = c(2,3))
s3d$plane3d(fit)

data2011 <- data.frame(year = 2011, quarter = 1:4)
cpi2011 <- predict(fit, newdata = data2011)
style <- c(rep(1,12),rep(2,4))
plot(c(cpi,cpi2011), xaxt = "n", ylab = "CPI", xlab = "", pch = style, col = style)
axis(1, at = 1:16, las = 3,labels = c(paste(year, quarter, sep = "Q"),"2011Q1","2011Q2","2011Q3","2011Q4"))

#Regresión logística

cedegren <- read.table("cedegreen.txt", header = T)
attach(cedegren)
ced.del <- cbind(sDel,sNoDel)
class(ced.del)
ced.logr <- glm(ced.del ~ cat + follows + factor(class), family = binomial("logit"))

summary(ced.logr)

ced.logr1 <- glm(ced.del ~ I(cat == "m") + I(cat == "v") + I(cat == "n") + follows + factor(class), family = binomial("logit"))

summary(ced.logr1)

anova(ced.logr, test = "Chisq")
drop1(ced.logr, test = "Chisq")

glm(ced.del ~ cat + follows + I(class == 1), family = binomial("logit"))
1-pchisq(232.7-198.63, 2)
glm(ced.del ~ cat + follows + I(class == 1) + I(class == 3), family = binomial("logit"))
1-pchisq(202.1-198.63, 1)
glm(ced.del ~ I(cat == "n") + I(cat == "v") + follows + factor(class), family = binomial("logit"))
1-pchisq(229.1-198.63, 2)


