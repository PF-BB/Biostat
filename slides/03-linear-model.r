## Code included in 04-linear-model.md.

n <- 10
x <- runif(n, 0, 10)
y <- 5.1 + 1.8 * x + rnorm(n)
summary(lm(y ~ x))

paint <- read.table("PAINT.DAT", header=TRUE)
xyplot(PCV ~ HAEMO, data=paint, type=c("p","r"))

lm.fit <- lm(PCV ~ scale(HAEMO, scale=FALSE), data=paint)
summary(lm.fit)
confint(lm.fit)
anova(lm.fit)

xyplot(resid(lm.fit) ~ HAEMO, data=paint)
xyplot(resid(lm.fit) ~ fitted(lm.fit))

influence.measures(lm.fit)

fitted(lm.fit)
predict(lm.fit, data.frame(HAEMO=seq(13, 18, by=1)))

x <- gl(5, 1, 10, labels=letters[1:5])
y <- rnorm(10)
model.matrix(y ~ x)

haemo.dec <- cut2(paint$HAEMO, g=10)
fm <- PCV ~ haemo.dec
summary(aov.fit <- aov(fm, data=paint))
summary(lm.fit <- lm(fm, data=paint))
.Last.value$sigma^2
grp.means <- tapply(paint$PCV, haemo.dec, mean)	
grp.means[2:10] - grp.means[1]
coef(lm.fit)

contr.treatment(10)
contr.sum(10)
contr.helmert(10)

fm <- y ~ x + a * b
mod1 <- lm(fm, data=dat)
update(mod1, . ~ . - a:b)  # supprime interaction AxB

data(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose)
fm <- len ~ supp * dose
replications(fm, data=ToothGrowth)
library(Hmisc)
f <- function(x) apply(x, 2, function(x) 
                       c(mean=mean(x), sd=sd(x)))
summary(fm, data=ToothGrowth, fun=f)

library(reshape2)
m <- acast(ToothGrowth, supp ~ dose, mean, value.var="len")

xyplot(len ~ dose, data=ToothGrowth, groups=supp,
       type=c("p","a"))

aov.fit <- aov(fm, data=ToothGrowth)
summary(aov.fit)
model.tables(aov.fit, type="means", se=TRUE, 
             cterms="supp:dose")

apply(m, 2, diff)

qqmath(~ resid(aov.fit))
bwplot(len ~ interaction(supp, dose), data=ToothGrowth)
bartlett.test(len ~ interaction(supp,dose),data=ToothGrowth)

data(anorexia)
anorexia$Treat <- relevel(anorexia$Treat, ref="Cont")
anorex.aov0 <- aov(Postwt ~ Prewt + Treat, data=anorexia)
anorex.aov1 <- aov(Postwt ~ Prewt * Treat, data=anorexia)
summary(anorex.aov0)

xyplot(Postwt ~ Prewt, data=anorexia, groups=Treat, 
       aspect="iso", type=c("p","r"))

anova(anorex.aov0, anorex.aov1)

summary.lm(anorex.aov1)

lm(Postwt ~ Prewt + Treat + offset(Prewt), data=anorexia)

chs <- read.table("cholesterol.txt", header=TRUE)
chs$Subject <- factor(chs$Subject)  # important
chs <- melt(chs, id.vars="Subject")
aov1 <- aov(value ~ variable + Error(Subject), data=chs)
summary(aov1)

library(nlme)
lme1 <- lme(value ~ variable, data=chs, 
            random= ~ 1 | Subject)
summary(lme1)
anova(lme1)
31.96^2/(31.96^2+7.61^2)  # ICC
intervals(lme1)
