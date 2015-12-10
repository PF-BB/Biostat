library(knitr)
render_sweave()
opts_chunk$set(size = "small", fig.path = 'figs/05-', dev = "cairo_pdf", 
               dev.args = list(family = "Bitstream Vera Sans"),
               fig.width = 5, fig.height = 5, 
               out.width = ".6\\linewidth", fig.align = "center",
               message=FALSE, cache = TRUE)
opts_knit$set(progress = FALSE, verbose = FALSE) 
options(digits = 3, width = 70)
library(latticeExtra)
data(sleep)
t.test(extra ~ group, data = sleep)
t.test(extra ~ group, data = sleep, paired = TRUE)
## xyplot(extra ~ group, sleep, groups = ID, type = "a")
labs <- c("None","Tablet","Capsule","Coated")
fat <- data.frame(fecfat = c(44.5,33.0,19.1,9.4,71.3,51.2,
                            7.3,21.0,5.0,4.6,23.3,38.0,
                            3.4,23.1,11.8,4.6,25.6,36.0,
                            12.4,25.4,22.0,5.8,68.2,52.6),
                 pilltype = gl(4, 6, labels=labs),
                 subject = gl(6, 1))
head(fat)
library(nlme)
m <- lme(fecfat ~ pilltype, data = fat, 
         random = ~ 1 | subject)
anova(m)
intervals(m, which = "var-cov")
VarCorr(m)
sigma.s <- as.numeric(VarCorr(m)[1,2])
sigma.eps <- as.numeric(VarCorr(m)[2,2])
sigma.s^2 / (sigma.s^2 + sigma.eps^2)
ms <- anova(lm(fecfat ~ pilltype + subject,
               data=fat))[[3]]
vs <- (ms[2] - ms[3]) / nlevels(fat$pilltype)
vr <- ms[3]
vs / (vs + vr)
gls.fit <- gls(fecfat ~ pilltype, data=fat,
              corr=corCompSymm(form= ~ 1 | subject))
anova(gls.fit)
intervals(gls.fit, which = "var-cov")
## fat$pred <- predict(m)
## p1 <- xyplot(fecfat ~ reorder(pilltype, fecfat), data=fat,
##             groups=subject, type="a", xlab="Pill type",
##             ylab="Fecal fat (g/day)",
##             scales=list(y=list(at=seq(0, 80, by=20))),
##             par.settings = ggplot2like(),
##             axis = axis.grid)
## p2 <- xyplot(pred ~ reorder(pilltype, fecfat), data=fat,
##             groups=subject, type="a", xlab="Pill type",
##             ylab="Predicted fecal fat (g/day)",
##             scales=list(y=list(at=seq(0, 80, by=20))),
##             par.settings = ggplot2like(),
##             axis = axis.grid)
## gridExtra::grid.arrange(p1, p2)
m <- update(m, method = "ML")
m0 <- update(m, fixed = . ~ - pilltype)
anova(m, m0)
library(lme4)
data(sleepstudy)
reg.subj <- lmList(Reaction ~ Days | Subject, sleepstudy)
reg.subj.df <- data.frame(lapply(reg.subj, coef))
apply(reg.subj.df, 1, quantile, prob = c(.25, .75))
coef(lm(Reaction ~ Days, data = sleepstudy))
m1 <- lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
m2 <- lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
m3 <- lmer(Reaction ~ Days + (1 | Subject) + (0 + Days | Subject), data=sleepstudy)
anova(m1, m2, m3)
m4 <- aov(Reaction ~ Days + Subject, data = sleepstudy)
feff <- model.tables(m4, cterms="Subject")[[1]]$Subject
as.numeric(unlist(ranef(m1)$Subject / feff)[1])
data(Blackmore, package="car")
lex <- log(Blackmore$exercise + 5/60, 2)
m0 <- lme(lex ~ I(age-8)*group, 
         random= ~ I(age-8) | subject, 
         data=Blackmore)
m1 <- update(m0, random= ~ 1 | subject)
m2 <- update(m0, random= ~ I(age-8) - 1 | subject)
anova(m0, m1)
anova(m0, m2)
summary(m0)
