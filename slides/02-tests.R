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
binom.test(4, 10)
data(birthwt, package="MASS")
birthwt$smoke <- factor(birthwt$smoke, 
                        labels=c("No","Yes"))
t.test(bwt ~ smoke, data=birthwt, var.equal=TRUE)
bwplot(smoke ~ bwt, data=birthwt)
qqmath(~ bwt, data=birthwt, group=smoke)
bwplot(smoke ~ bwt, data=birthwt, 
       panel = function(x, y, ...) {
           panel.bwplot(x, y, ...)
           panel.points(x, jitter(as.numeric(y), 
                                  amount = .05), ...) })
t.test(extra ~ group, data=sleep, paired=TRUE)
xyplot(extra[group==2] ~ extra[group==1], data = sleep, 
       type = c("p", "g"), aspect = "iso", 
       abline = list(a = 0, b = 1))
library(foreign)
d <- read.dta("../data/polymorphism.dta")
head(d)
xtabs(~ genotype, data = d)
summary(d)
fm <- age ~ genotype
aggregate(fm, data = d, mean)
aggregate(fm, data = d, Hmisc::smean.sd)
bwplot(fm, data = d)
m <- aov(fm, data = d)
summary(m)
model.tables(m)
rfs(m)
