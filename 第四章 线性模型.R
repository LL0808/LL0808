library(rethinking)
#导入rethinking包中的Howell1数据
data(Howell1)
#将Howell1数据赋值给d
d <- Howell1
#查看数据d数据库结构
str(d)
#只对身高这个变量进行研究，取出身高这一列
d$height
#进一步，只对成年人的身高这一列进行研究，取出成年人的身高
d2 <- d[d$age>=18,]
#最终取出的总体数量是352个身高数据
nrow(d2)
#绘制均值u的先验分布图，范围是从100到250
curve(dnorm(x,178,20),from = 100,to = 250)
#绘制标准差的先验分布图，坐标轴范围是-10到60
curve(dunif(x,0,50),from=-10,to=60)

#从总体中抽取样本,用处理后验分布的方式（即用抽样来处理）来处理先验分布
sample_mu <- rnorm(1e4,mean = 178,sd=20)#随即生成10000个服从均值为178，标准差为20的正态分布的向量
sample_sigma <- runif(1e4,min = 0,max = 50)#随即生成10000个区间为【0,50】的均匀分布的向量
prior_h <- rnorm(1e4,sample_mu,sample_sigma)#一一对应从先验中取值，所以说这是“平均”得到的期望分布
rethinking::dens(prior_h)#画出取出样本的概率密度图

#网格逼近后验分布 4.3.3
mu.list <- seq(from=140,to=160,length.out=200)#定义均值的取值范围
mu.sigma <- seq(from=4,to=9,length.out=200)#定义标准差的取值范围
post <- expand.grid(mu=mu.list,sigma=mu.sigma)#所有的均值和方差都结合一遍，共有200*200次结合
#计算每个均值和方差结合产生的似然值，取对数方便计算
post$LL <- sapply(1:nrow(post),function(i)sum(dnorm(
  d2$height,
  mean = post$mu[i],
  sd=post$sigma[i],
  log = TRUE
)))
#先验加上似然函数得到后验分布的核
post$prod <- post$LL+
  dnorm(post$mu,mean=178,sd=20,log=TRUE)+
  dunif(post$sigma,min=0,max=50,log = TRUE)
#得出后验分布最大的那个均值和标准差
post$prob <- exp(post$prod-max(post$prod))

#绘制简单的等高线
rethinking::contour_xyz(post$mu,post$sigma,post$prob)

#绘制简单的热图
rethinking::image_xyz(post$mu,post$sigma,post$prob)

#从后验分布中抽取样本 4.3.4
sample.rows <- sample(1:nrow(post),
                      size = 1e4,
                      replace = TRUE,prob = post$prob)#按概率取出行
sample.mu <- post$mu[sample.rows]#按取出的行取出均值作为样本分布
sample.sigam <- post$sigma[sample.rows]#按取出的行取出标准差作为样本分布
plot(sample.mu,sample.sigam,
     cex=0.5,pch=10,
     col=rethinking::col.alpha(rangi2,alpha = 0.1))#画出散点图

rethinking::dens(sample.mu)#绘制抽取的样本的均值的密度图
rethinking::dens(sample.sigam)#绘制抽取的样本的标准差的密度图
rethinking::HPDI(sample.mu)#得出均值最高密度区间
rethinking::HPDI(sample.sigam)#得出标准差的最高密度区间

#从d2$height中抽取20个样本
d3 <- sample(d2$height,size = 20)
#下面代码分析现在d3中的20个样本，只需修改之前的代码
mu.list <- seq(from=150,to=170,length.out=200)
sigma.list <- seq(from=4,to=20,length.out=200)
post2 <- expand.grid(mu=mu.list,sigma=sigma.list)
post$LL <- sapply(1:nrow(post2),function(i)
  sum(dnorm(d3,mean = post2$mu[i],
            sd=post2$sigma[i],log = TRUE)))
post2$prob <- post2$LL+dnorm(post2$mu,mean = 178,sd = 20,log = TRUE)+
  dunif(post2$sigma,min=0,max=50,log = TRUE)
post2
