d <- read.csv("Framingham.csv")

dim(d)

d <- subset(d, complete.cases(d))

set.seed(101)

idx <- sample(1:nrow(d), ceiling(nrow(d)/2))
## idx <- sample(nrow(d), replace = TRUE)

head(idx)

dtrain <- d[idx,]
dtest <- d[-idx,]

dim(dtrain)
dim(dtest)

names(dtrain)

dtrain$sex <- dtrain$sex -1
dtest$sex <- dtest$sex -1

fm <- chdfate ~ sex + age + bmi + sbp + dbp + scl

m <- glm(fm, data = dtrain, family = binomial)

summary(m)

exp(coef(m))

exp(confint(m))

p <- predict(m, newdata = dtest, type ="response")

summary(p)

table(p >= 0.5)
tab <- table(p >= 0.5, dtest$chdfate)

sum(diag(tab))/nrow(dtest)

library(lattice)

xyplot(p ~ bmi, data = dtest, type = "p")


predict(m, newdata = data.frame(sex = 0, 
                                age = 45, 
                                bmi = 35, 
                                sbp = 144, 
                                dbp = 90, 
                                scl = 255), 
        type = "response", se.fit = TRUE)
