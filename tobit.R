
#https://stats.idre.ucla.edu/r/dae/tobit-models/

require(ggplot2)
require(GGally)
require(VGAM)

#演示数据
dat <- read.csv("https://stats.idre.ucla.edu/stat/data/tobit.csv")
summary(dat)


#直方图
# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 15) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(dat, aes(x = apt, fill=prog))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=15) +
  stat_function(fun = f, size = 1,
    args = list(var = dat$apt))

#柱形图
p + stat_bin(binwidth = 1) + stat_function(fun = f, size = 1, args = list(var = dat$apt, 
    bw = 1))
#相关图
cor(dat[, c("read", "math", "apt")])
# plot matrix
ggpairs(dat[, c("read", "math", "apt")])

#tobit回归
summary(m <- vglm(apt ~ read + math + prog, tobit(Upper = 800), data = dat))
#tobit模型选择
ctable <- coef(summary(m))
pvals <- 2 * pt(abs(ctable[, "z value"]), df.residual(m), lower.tail = FALSE)
cbind(ctable, pvals)

m2 <- vglm(apt ~ read + math, tobit(Upper = 800), data = dat)
(p <- pchisq(2 * (logLik(m) - logLik(m2)), df = 2, lower.tail = FALSE))

b <- coef(m)
se <- sqrt(diag(vcov(m)))
cbind(LL = b - qnorm(0.975) * se, UL = b + qnorm(0.975) * se)

dat$yhat <- fitted(m)[,1]
dat$rr <- resid(m, type = "response")
dat$rp <- resid(m, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(dat, {
  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  plot(yhat, rp, main = "Fitted vs Pearson Residuals")
  qqnorm(rp)
  plot(apt, rp, main = "Actual vs Pearson Residuals")
  plot(apt, yhat, main = "Actual vs Fitted")
})

# correlation
(r <- with(dat, cor(yhat, apt)))
# variance accounted for
r^2

#银行员工调查
bankdata <- read.csv("D:/working20180805/bank112x.csv", header=TRUE, sep=",")
summary(bankdata)
#满意度直方图(w1 to w5)
# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 10) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(bankdata, aes(x = s, fill=factor(w5)))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=10) +
  stat_function(fun = f, size = 1,
    args = list(var = bankdata$s))


#满意度直方图(h1 to h7)
# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 10) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(bankdata, aes(x = s, fill=factor(h7)))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=10) +
  stat_function(fun = f, size = 1,
    args = list(var = bankdata$s))
#条图
p + stat_bin(binwidth = 5) + stat_function(fun = f, size = 1, args = list(var = bankdata$s, 
    bw = 5))
#相关图
bankdata$TTO=bankdata$TTO*100
cor(bankdata[, c("TTO", "vas", "s")])
# plot matrix
ggpairs(bankdata[, c("TTO", "vas", "s")])

#满意度影响因素tobit回归
summary(mm<-vglm(s~factor(X1)+factor(X2)+factor(X3)+X4+factor(X5)+factor(X6)+factor(X7)+factor(X8)+factor(X9)+factor(X10)+factor(X11)+factor(X12)
 +factor(p1)+factor(p2)+factor(n1)+factor(n2)+factor(n3)+factor(n4)+factor(n5),tobit(Upper=100),data = bankdata))
#模型选择
ctable <- coef(summary(mm))
pvals <- 2 * pt(abs(ctable[, "z value"]), df.residual(mm), lower.tail = FALSE)
cbind(ctable, pvals)
mm2 <- vglm(s ~ factor(p1)+factor(p2)+factor(n1)+factor(n2)+factor(n3)+factor(n4)+factor(n5), tobit(Upper = 100), data = bankdata)

(pp <- pchisq(2 * (logLik(mm) - logLik(mm2)), df = 10, lower.tail = FALSE))

b <- coef(mm)
se <- sqrt(diag(vcov(mm)))
cbind(LL = b - qnorm(0.975) * se, UL = b + qnorm(0.975) * se)
summary(mm2)


#拟合优度检验(mm)
bankdata$yhat <- fitted(mm)[,1]
bankdata$rr <- resid(mm, type = "response")
bankdata$rp <- resid(mm, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(bankdata, {
  plot(yhat, rr, main = "拟合值 vs 残差")
  qqnorm(rr)
  plot(yhat, rp, main = "拟合值 vs Pearson 残差")
  qqnorm(rp)
  plot(s, rp, main = "观察值 vs Pearson 残差")
  plot(s, yhat, main = "观察值 vs 拟合值")
})

# correlation
(r <- with(bankdata, cor(yhat, s)))
# variance accounted for
r^2


#拟合优度检验(mm2)
bankdata$yhat <- fitted(mm2)[,1]
bankdata$rr <- resid(mm2, type = "response")
bankdata$rp <- resid(mm2, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(bankdata, {
  plot(yhat, rr, main = "拟合值 vs 残差")
  qqnorm(rr)
  plot(yhat, rp, main = "拟合值 vs Pearson 残差")
  qqnorm(rp)
  plot(s, rp, main = "观察值 vs Pearson 残差")
  plot(s, yhat, main = "观察值 vs 拟合值")
})

# correlation
(r <- with(bankdata, cor(yhat, s)))
# variance accounted for
r^2

#年龄直方图(post,post2)
# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 5) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(bankdata, aes(x = age1, fill=factor(post2)))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=5) +
  stat_function(fun = f, size = 1,
    args = list(var = bankdata$age1))

#vas直方图(eq1 to eq5 disease d1 EQ5D smoke drink)
# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 10) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(bankdata, aes(x = vas, fill=factor(drink)))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=10) +
  stat_function(fun = f, size = 1,
    args = list(var = bankdata$vas))

#EQ5D的tobit回归
summary(ss <- vglm(U2 ~ factor(eq1) + factor(eq4) + factor(eq5), tobit(Upper = 1), data = bankdata))
#拟合优度检验(mm2)
bankdata$yhat <- fitted(ss)[,1]
bankdata$rr <- resid(ss, type = "response")
bankdata$rp <- resid(ss, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(bankdata, {
  plot(yhat, rr, main = "拟合值 vs 残差")
  qqnorm(rr)
  plot(yhat, rp, main = "拟合值 vs 皮尔逊残差")
  qqnorm(rp)
  plot(U2, rp, main = "观察值 vs 皮尔逊残差")
  plot(U2, yhat, main = "观察值 vs 拟合值")
})

# correlation
(r <- with(bankdata, cor(yhat, U2)))
# variance accounted for
r^2

#VAS危险因素的tobit回归
summary(ss<-vglm(U2~factor(X1)+factor(X2)+factor(X3)+X4+factor(X5)+factor(X6)
+factor(X7)+factor(X8)+factor(X9)+factor(X10)+factor(X11)+factor(X12), tobit(Upper=1), data = bankdata))
#拟合优度检验
bankdata$yhat <- fitted(ss)[,1]
bankdata$rr <- resid(ss, type = "response")
bankdata$rp <- resid(ss, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(bankdata, {
  plot(yhat, rr, main = "拟合值 vs 残差")
  qqnorm(rr)
  plot(yhat, rp, main = "拟合值 vs 皮尔逊残差")
  qqnorm(rp)
  plot(U2, rp, main = "观察值 vs 皮尔逊残差")
  plot(U2, yhat, main = "观察值 vs 拟合值")
})

# correlation
(r <- with(bankdata, cor(yhat, U2)))
# variance accounted for
r^2

#EQ5D疾病tobit回归
summary(ss <- vglm(U2 ~X13, tobit(Upper = 100), data = bankdata))
#拟合优度检验(mm2)
bankdata$yhat <- fitted(ss)[,1]
bankdata$rr <- resid(ss, type = "response")
bankdata$rp <- resid(ss, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(bankdata, {
  plot(yhat, rr, main = "拟合值 vs 残差")
  qqnorm(rr)
  plot(yhat, rp, main = "拟合值 vs 皮尔逊残差")
  qqnorm(rp)
  plot(U2, rp, main = "观察值 vs 皮尔逊残差")
  plot(U2, yhat, main = "观察值 vs 拟合值")
})

# correlation
(r <- with(bankdata, cor(yhat, U2)))
# variance accounted for
r^2

#满意度量表tobit回归
summary(ss <- vglm(s ~factor(w1) + factor(w2) + factor(w3) + factor(w4) + factor(w5), tobit(Upper = 100), data = bankdata))
#拟合优度检验(ss)
bankdata$yhat <- fitted(ss)[,1]
bankdata$rr <- resid(ss, type = "response")
bankdata$rp <- resid(ss, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(bankdata, {
  plot(yhat, rr, main = "拟合值 vs 残差")
  qqnorm(rr)
  plot(yhat, rp, main = "拟合值 vs 皮尔逊残差")
  qqnorm(rp)
  plot(ss, rp, main = "观察值 vs 皮尔逊残差")
  plot(ss, yhat, main = "观察值 vs 拟合值")
})

# correlation
(r <- with(bankdata, cor(yhat, s)))
# variance accounted for
r^2


#情绪对满意度tobit回归
summary(s <- vglm(s ~ factor(p1)+factor(p2)+factor(n1)+factor(n2)+factor(n3)+factor(n4)+factor(n5),tobit(Upper = 100), data = bankdata))
#拟合优度检验(mm2)
bankdata$yhat <- fitted(s)[,1]
bankdata$rr <- resid(s, type = "response")
bankdata$rp <- resid(s, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(bankdata, {
  plot(yhat, rr, main = "拟合值 vs 残差")
  qqnorm(rr)
  plot(yhat, rp, main = "拟合值 vs 皮尔逊残差")
  qqnorm(rp)
  plot(vas, rp, main = "观察值 vs 皮尔逊残差")
  plot(vas, yhat, main = "观察值 vs 拟合值")
})

# correlation
(r <- with(bankdata, cor(yhat, s)))
# variance accounted for
r^2

#情绪分类满意度tobit回归
summary(s <- vglm(s ~ factor(p)+factor(neg),tobit(Upper = 100), data = bankdata))
#拟合优度检验(mm2)
bankdata$yhat <- fitted(s)[,1]
bankdata$rr <- resid(s, type = "response")
bankdata$rp <- resid(s, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(bankdata, {
  plot(yhat, rr, main = "拟合值 vs 残差")
  qqnorm(rr)
  plot(yhat, rp, main = "拟合值 vs 皮尔逊残差")
  qqnorm(rp)
  plot(vas, rp, main = "观察值 vs 皮尔逊残差")
  plot(vas, yhat, main = "观察值 vs 拟合值")
})

# correlation
(r <- with(bankdata, cor(yhat, s)))
# variance accounted for
r^2
#VAS调查
vasdata <- read.csv("D:/working20180805/VAS 2193.csv", header=TRUE, sep=",")
summary(vasdata)
# plot matrix
ggpairs(vasdata[, c("VAS", "VAS_N3_Europ", "VAS_N3_UK", "VAS_m_Finland", "VAS_m_Slovenia","VAS_m_Dema", "VAS_N3_Belgium", 
       "VAS_N3_NZealand", "VAS_N3_Spain","VAS_m_2008", "VAS_N3_2013", "VAS_m")])

