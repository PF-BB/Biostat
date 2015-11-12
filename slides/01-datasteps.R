library(knitr)
render_sweave()
opts_chunk$set(size = "small", fig.path = 'figs/01-', dev = "cairo_pdf", 
               dev.args = list(family = "Bitstream Vera Sans"),
               fig.width = 5, fig.height = 5, 
               out.width = ".6\\linewidth", fig.align = "center",
               message=FALSE)
opts_knit$set(progress = FALSE, verbose = FALSE) 
options(digits = 3, width = 70)
library(latticeExtra)
lattice.options(default.args = list(axis = axis.grid))
trellis.par.set(ggplot2like(lwd = 2.5))
v <- c(1,1,2,3,5,8,13)
v
print(v)
rm(v)
is.numeric(3.14)
is.double(3.14)
is.integer(3.14)
as.integer(3.14)
data(ToothGrowth)
head(ToothGrowth)
ls()
help(ToothGrowth)
str(ToothGrowth)
ToothGrowth[1,]
ToothGrowth[c(1,3),]
ToothGrowth[1:5, 2]
ToothGrowth[1:5, "supp"]
ToothGrowth[ToothGrowth$supp == "VC" & 
            ToothGrowth$dose == 0.5, 
            "len"]
ToothGrowth$supp[ToothGrowth$len < 11]
subset(ToothGrowth, supp == "VC" & dose == 0.5, len)
summary(ToothGrowth$len)
summary(ToothGrowth$supp)
summary(ToothGrowth)
head(ToothGrowth$dose)
unique(ToothGrowth$dose)
head(factor(ToothGrowth$dose))
## library(lattice)
## if (!require(latticeExtra))
##     install.packages("latticeExtra")
## lattice.options(default.args = list(axis = axis.grid))
## trellis.par.set(ggplot2like(lwd = 2.5))
library(gridExtra)
p1 <- xyplot(bwt ~ lwt, data = MASS::birthwt, groups = ht, type = c("p", "smooth", "g"), par.settings = simpleTheme(), xlab = "", ylab = "", main = "Thème par défaut")
p2 <- xyplot(bwt ~ lwt, data = MASS::birthwt, groups = ht, type = c("p", "smooth", "g"), par.settings = custom.theme.2(), xlab = "", ylab = "", main = "custom.theme.2()")
p3 <- xyplot(bwt ~ lwt, data = MASS::birthwt, groups = ht, type = c("p", "smooth", "g"), par.settings = theEconomist.theme(), xlab = "", ylab = "", main = "theEconomist.theme()")
grid.arrange(p1, p2, p3, nrow = 1)
## qqmath(~ len, data = ToothGrowth, dist = qunif)
qstats <- quantile(ToothGrowth$len)[2:4]
names(qstats) <- c(0.25,0.50,0.75)
p <- qqmath(~ len, data = ToothGrowth, dist = qunif, xlab="Fréquence cumulée",
           panel = function(...) {
               panel.qqmath(x, ...)
               for (i in 1:length(qstats))
                   panel.lines(c(-.5, rep(as.numeric(names(qstats)[i]), 2)),
                               c(rep(qstats[i], 2), 0), col="darkgrey")
               panel.text(rep(.05, 3), qstats, paste(as.numeric(names(qstats))*100, "%"),
                          cex=.8, pos=3, col="darkgrey")
           })
print(p)
histogram(~ len, data = ToothGrowth, type = "count")
p <- list()
rg <- seq(5, 15, by=5)
for (i in seq_along(rg))
  p[[i]] <- histogram(~ len, data=ToothGrowth, breaks=rg[i],
                       main=paste(rg[i], "intervalles"), par.settings = ggplot2like(),
                       ylab="", xlab="", type="density", border=NA)
p[[4]] <- histogram(~ len, data=ToothGrowth, type="density", ylab="", xlab="",
                     border=NA, par.settings = ggplot2like(),
                     panel=function(x, ...) {
                       panel.histogram(x, ...)
                       panel.mathdensity(dmath=dnorm, col="#BF3030", lwd = 3,
                                         args=list(mean=mean(x),sd=sd(x)))
                     },
                     main=paste("N(", round(mean(ToothGrowth$len),1), ";",
                       round(sd(ToothGrowth$len),1), ") superposée", sep=""))
do.call(grid.arrange, p)
histogram(~ len | supp, data = ToothGrowth, 
          breaks = seq(0, 40, by = 5))
densityplot(~ len, data = ToothGrowth)
p1 <- densityplot(~ len, data = ToothGrowth, adjust = 0.5, main = "adjust = 0.5", par.settings = ggplot2like())
p2 <- densityplot(~ len, data = ToothGrowth, adjust = 1, main = "adjust = 1 (défaut)", par.settings = ggplot2like())
p3 <- densityplot(~ len, data = ToothGrowth, adjust = 2, main = "adjust = 2", par.settings = ggplot2like())
grid.arrange(p1, p2, p3, nrow = 1)
densityplot(~ len, data = ToothGrowth, groups = supp, 
            from = 0, to = 40, auto.key = TRUE)
bwplot(len ~ supp, data = ToothGrowth, pch = "|")
dotplot(len ~ supp, ToothGrowth, jitter.x = TRUE)
fhs <- read.csv("data/Framingham.csv")
fhs <- subset(fhs, fhs$id != 9999 & complete.cases(fhs))
dim(fhs)
names(fhs)[1:5]
fhs$sex <- factor(fhs$sex, levels = 1:2, 
                 labels = c("M", "F"))
summary(fhs$sex)
aggregate(sbp ~ sex, data = fhs, mean)
aggregate(sbp ~ sex, data = fhs, sd)
summary(fhs$bmi)
fhs$bmi.cat <- cut(fhs$bmi, breaks=c(16,18.5,25,30,58), 
                  right = FALSE)
summary(fhs$bmi.cat)
levels(fhs$bmi.cat) <- c("Under","Normal","Over","Obese")
relevel(fhs$bmi.cat, ref = "Normal")
xtabs(~ sex + bmi.cat, data = fhs)
r <- xtabs(~ sex + bmi.cat, data = fhs)
margin.table(r, margin = 2)
prop.table(r, margin = 2)
pr <- prop.table(r, margin = 2)
as.data.frame(pr)
dotplot(bmi.cat ~ Freq, data = as.data.frame(pr), 
        groups = sex, xlab = "%", 
        type = c("p", "l"), auto.key = TRUE)
summary(fhs[,c("sbp", "age")])
cor(fhs$sbp, fhs$age)
cor(fhs$sbp, fhs$age, method = "spearman")
xyplot(sbp ~ age, data = fhs, type = c("p", "smooth"))
xyplot(sbp ~ age, data = fhs, groups = sex, 
       type = c("p", "smooth"), alpha = 0.5)
ch <- with(fhs, chull(sbp, age))
cor(fhs$sbp[-ch], fhs$age[-ch])
chi <- c(ch, ch[1])
p <- xyplot(sbp ~ age, data=fhs,
             panel=function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.lmline(x, y, ...)
               panel.lines(x[chi], y[chi], col="#BF3030", ...)
             })
print(p)
## data(birthwt, package="MASS")
