## Code included in 06-logistic.md.

bp <- read.table("hdis.dat", header=TRUE)
blab <- c("<117","117-126","127-136","137-146",
          "147-156","157-166","167-186",">186")
clab <- c("<200","200-209","210-219","220-244",
          "245-259","260-284",">284")
bp <- within(bp, {
  bpress <- factor(bpress, labels=blab)
  chol <- factor(chol, labels=clab)
})
round(xtabs(hdis/total ~ bpress + chol, data=bp), 2)

midpoint <- function(x) {
  x <- as.numeric(unlist(strsplit(x, "-")))
  return(sum(x)/2)
}
val <- sapply(levels(bp$bpress)[-c(1,8)], midpoint)
dfrm <- aggregate(bp[,3:4], list(bpress=bp[,1]), sum)
dfrm$bpress <- c(val[1]-10, val, val[6]+15)
mod1 <- glm(cbind(hdis, total-hdis) ~ bpress, 
            data=dfrm, family=binomial)
summary(mod1)

data(birthwt, package="MASS")
ethn <- c("White","Black","Other")
birthwt$race <- factor(birthwt$race, labels=ethn)
fm <- low ~ age + lwt + race + ftv
glm1 <- glm(fm, data=birthwt, family=binomial)
summary(glm1)
confint(glm1)

log.odds <- predict(glm1, data.frame(age=mean(birthwt$age), 
                                     lwt=mean(birthwt$lwt),
                                     race="White", ftv=0))
exp(log.odds)/(1+exp(log.odds))  # 0.2483566

glm2 <- update(glm1, . ~ . - age - ftv)

anova(glm2, glm1, test="Chisq")

anova(update(glm2, . ~ . - race), glm2)

pchisq(5.4316, 2, lower.tail=FALSE)

library(car)
mmps(glm2, terms=~lwt)

influence.measures(glm2)

library(rms)
ddist <- datadist(birthwt)
options(datadist="ddist")
glm2b <- lrm(low ~ lwt + race, data=birthwt)
glm2b
exp(coef(glm2b))

Predict(glm2b, lwt, race, fun=plogis, conf.type="mean")

table(predict(glm1, type="resp")>=.5, birthwt$low)
