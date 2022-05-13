#第五章 多元线性回归

# 5.1 虚假相关
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

#标准化预测变量
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage))/
  sd(d$MedianAgeMarriage)

#拟合模型
m5.1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu ~ a + bA * MedianAgeMarriage.s,
    a ~ dnorm(10,10),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),data =d
)
precis(m5.1)

# 计算均值的分位数区间
MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)

#将结果绘制出来
plot(Divorce ~ MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

d$Marriage.s <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu ~ a + bR * Marriage.s,
    a ~ dnorm(10,10),
    bR ~ dnomr(0,1),
    sigma ~ dunif(0,10)
  ),data = d
)

# 5,1.2 拟合模型

m5.3 <- map(
  alist(
    Divorce <- dnorm(mu, sigma),
    mu ~ a + bR * Marriage.s + bA * MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
    ),
  data = d
)
precis(m5.3)
plot(precis(m5.3))

# 5.1.3 多元后验分布图
m5.4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b * MedianAgeMarriage.s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.4)
mu

#计算每个州的Map估计值
mu <- coef(m5.4)['a'] + coef(m5.4)['b'] * d$MedianAgeMarriage.s

# 计算每个州的残差
m.resid <- d$Marriage.s - mu
plot(Marriage.s ~ MedianAgeMarriage.s, d, col = rangi2)
abline(m5.4)
for (i in 1:length(m.resid)){
  x <- d$MedianAgeMarriage.s[i]
  y <- d$Marriage.s[i]
  lines(c(x,x), c(mu[i],y), lwd = 0.5,
        col = col.alpha('black', 0.7))
}
precis(m5.4)


# 5.2 隐藏的关系
library(rethinking)
data("milk")
d <- milk
dim(d)
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data =  d
)
#存在NA，会报错
table(is.na(d$neocortex.perc))
dcc <- d[complete.cases(d), ]

dim(dcc)


m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data =  dcc
)
precis(m5.5, digits = 3)
#typeof(coef(m5.5))

coef(m5.5)['bn']* (76 - 55)

np.seq <- 0 : 100
pred.data <- data.frame(neocortex.perc = np.seq)
mu <- link(m5.5, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, col = rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,],lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)

dcc$log.mass <- log(dcc$mass)
m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm * log.mass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),data = dcc
)
precis(m5.6)

np.lm <- seq(-3, 5, length.out = 100)
pred.data <- data.frame(log.mass = np.lm)
mu <- link(m5.6, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(kcal.per.g ~ log.mass, data = dcc, col = rangi2)
lines(np.lm, mu.mean)
lines(np.lm, mu.PI[1,], lty = 2)
lines(np.lm, mu.PI[2,], lty = 2)
m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc + bm * log.mass,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    bm ~dnorm(0,1),
    sigma ~ dunif(0, 1)
  ),data = dcc
)
precis(m5.7)

mean.log.mass <- mean(dcc$log.mass)
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc <- np.seq,
  log.mass = mean.log.mass
)
mu <- link(m5.7, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(kcal.per.g ~ neocortex.perc, data = dcc, type = 'n')

lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)

mean.neocortex.perc <- mean(dcc$neocortex.perc)
np.seq <- seq(-3, 5, length.out = 100)
pred.data <- data.frame(
  neocortex.perc <- mean.neocortex.perc,
  log.mass = np.seq
)
mu <- link(m5.7, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(kcal.per.g ~ log.mass, data = dcc, type = 'n')
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty= 2)
lines(np.seq, mu.PI[2,], lty= 2)


#5.3 添加变量起反作用

# 5.3.1 共线性
N <- 100
height <- rnorm(N, 10, 2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop *height + rnorm(N, 0, 0.02)
leg_right <- leg_prop * height + rnorm(N, 0, 0.02)
d <- data.frame(height,leg_left,leg_right)
m5.8 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left + br * leg_right,
    a ~dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.8)
plot(precis(m5.8))
post <- extract.samples(m5.8)
plot(bl ~ br, post, col = col.alpha(rangi2, 0.1), pch = 16)
sum_blbr <- post$bl + post$br
mean(sum_blbr)
dens(sum_blbr,col = rangi2, lwd = 2, xlab = 'sum of bl and br')

m5.9 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + bl *leg_left,
  a ~ dnorm(10,100),
  bl ~ dnorm(2, 10),
  sigma ~ dunif(0, 10)
),data = d
)
precis(m5.9, digits = 4)
mean(sum_blbr)

library(rethinking)
data("milk")
d <- milk
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0,1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)


m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.lactose,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0,1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.10, digits = 3)

precis(m5.11, digits = 3)

m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat + bl * perc.lactose,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0,1),
    bl ~ dnorm(0 ,1),
    sigma ~ dunif(0,10)
  ),data = d
)
precis(m5.12, digits = 3)
pairs(~ kcal.per.g + perc.fat + perc.lactose,
      data = d, col = rangi2)

cor(d$perc.fat, d$perc.lactose)

library(rethinking)
data("milk")
d <- milk
sim.coll <- function(r = 0.9){
  d$x <- rnorm(nrow(d),mean = r * d$perc.fat,
               sd = sqrt(1 - r^2) * var(d$perc.fat))
  m <- lm(kcal.per.g ~ perc.fat + x ,data = d)
  sqrt(diag(vcov(m)))[2]
}
rep.sim.coll <- function(r = 0.9,n = 100){
  stddev <- replicate(n, sim.coll(r))
  mean(stddev)
}
r.seq <- seq(from = 0, to = 0.99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r = z, n = 100))
plot(stddev ~ r.seq, type = 'l', col = rangi2, lwd = 2, xlab = 'correlation')

# 5.4分类变量
# 5.4.1 二项分类
library(rethinking)
data(Howell1)
d <- Howell1
str(d)
m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm * male,
    a ~ dnorm(178, 100),
    bm ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d
)
precis(m5.15)

post <- extract.samples(m5.15)
mu.male <- post$a + post$bm
PI(mu.male)

sta <- list(
  af = 178,
  am = 178,
  sigma = 30
)
m5.15b <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- af * (1- male) + am *male,
    af ~ dnorm(178, 100),
    am ~ dnorm(178, 100),
    sigma ~ dunif(0, 50)
  ),start = sta,data = d
)
precis(m5.15b)

# 5.4.2 多类别
data(milk)
d <- milk
unique(d$clade)
d$clade.nwm <- ifelse(d$clade == 'New World Monkey',
                      1,0)
d$clade.own <- ifelse(d$clade == 'Old World Monkey',
                      1,0)
d$clade.s <- ifelse(d$clade == 'Strepsirrhine',
                    1,0)
m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm(mu,sigma),
    mu <- a + b.nwm *clade.nwm + b.owm *clade.own + b.s * clade.s,
    a ~dnorm(0.6,10),
    b.nwm ~ dnorm(0,1),
    b.owm ~ dnorm(0,1),
    b.s ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),data = d
)
precis(m5.16)
post <- extract.samples(m5.16)
mu.ape <- post$a
mu.nwm <- post$a + post$b.nwm
mu.owm <- post$a +post$b.owm
mu.s <- post$a + post$b.s
precis(data.frame(mu.ape, mu.nwm, mu.owm,mu.s))


diff.nwm.own <- mu.nwm - mu.owm
quantile(diff.nwm.own, probs = c(0.025, 0.5, 0.975))
d$clade_id <- coerce_index(d$clade)
d$clade_id

m5.16_alt <- map(
  alist(
    kcal.per.g <- dnorm(mu,sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0.6, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.16_alt, depth = 2)



