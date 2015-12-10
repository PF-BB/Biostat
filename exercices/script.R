## chargement des données
library(MASS)
data(birthwt)


dim(birthwt)


## recodage de la variable race en facteur
table(birthwt$race)
ethn <- c("white", "black", "other")
birthwt$race <- factor(birthwt$race, levels = 1:3, 
                       labels = ethn)

## recodage de la variable ftv en variable discrète
table(birthwt$ftv)
birthwt$ftv[birthwt$ftv > 2] <- 2
birthwt$ftvc <- factor(birthwt$ftv, levels = 0:2,
                       labels =c("0", "1", "2+"))
table(birthwt$ftvc)


## modèles de régression

### régression linéaire
birthwt$sage <-  birthwt$age - mean(birthwt$age)

fm1 <- bwt ~ sage * ftvc + race
fm2 <- bwt ~ sage + ftvc + race
fm3 <- bwt ~ sage + ftvc

m1 <- lm(fm1, data = birthwt)
m2 <- lm(fm2, data = birthwt)
m3 <- lm(fm3, data = birthwt)

summary(m1)
summary(m2)
summary(m3)

anova(m1, m2)

### prédiction


d <- expand.grid(sage = seq(-5, 5, by = 1), 
                 ftvc = levels(birthwt$ftvc), 
                 race = levels(birthwt$race))

pp <- predict(m2, d)
d$pp <- pp

library(lattice)

xyplot(pp ~ sage | race, data = d, type = "a")

xyplot(pp ~ sage, data = d, groups = race, type = "a",
       auto.key = TRUE)

xyplot(pp ~ sage | ftvc, data = d, groups = race, type = "a",
       auto.key = TRUE)

### régression logistique


fm4 <- low ~ age + race + ftvc

m4 <- glm(fm4, data = birthwt, family = binomial)

summary(m4)
