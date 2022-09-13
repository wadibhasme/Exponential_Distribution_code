### pdf of exponential distribution
x=seq(0,5,by=0.01)
alp=2
f=alp*exp(-alp*x)
plot(x,f,type='l',xlab='x',ylab='f(x)',main='pdf of exponetial distribution')
############## cdf of exponential distribution
x=seq(0,5,by=0.01)
F=1-exp(-alp*x)
plot(x,F,type='l',xlab='x',ylab='F(x)',main='cdf of exponetial distribution')
##### maximum likelyhood estimator
par_val=seq(0.01,5,by=0.01)
alp=2  ## only for simulation
logl=c()
n=1000
samp=rexp(n,rate=2)
for(i in 1:length(par_val))
{
logl[i]=n*log(par_val[i])-par_val[i]*sum(samp)
}
m=max(logl)
mle=par_val[which(logl==m)]
tn=n/sum(samp)  ### Estimator
plot(par_val,logl,type="l",xlab='par_val',ylab='logl',main='maximun likelyhood estimator')
abline(h=m,col="red")
abline(v=mle,col="green")
############################## Property of estimator ##############
#### 1) convergence in probability
n=2000
it=500
alp=2
esp=0.2
prob=c()
for(i in 1:n)
{
est=c()
for(j in 1:it)
{
samp=rexp(i,rate=2)
est[j]=i/sum(samp)
}
ab=abs(est-alp)
prob[i]=length(which(ab<=esp))/it
print(i)
}
plot(1:n,prob,xlab='n',ylab='p(|Tn-alp|<=esp)',type="l",main="convergence in probability")
abline(h=1,col="red")
####### 2) Asymptotic Normality
n=1000
sam=c()
alp=2
inf=a/alp^2
it=500
for(i in 1:it)
{
samp=rexp(n,rate=2)
Tn=n/sum(samp)
sam[i]=sqrt(n*inf)*(Tn-alp)
}
hist(sam,prob=TRUE)
boxplot(sam,main="BOXPLOT")
qqnorm(samp)
qqline(samp,col="red")
############## Moment based estimator
## Here we use sample mean as an estimator of population mean
## 1) convergence in probability
alp=2
exp=1/alp
n=2000
it=500
esp=0.15
prob=c()
for(i in 1:n)
{
est=c()
for(j in 1:it)
{
samp=rexp(i,rate=alp)
est[j]=mean(samp)
}
ab=abs(est-exp)
prob[i]=length(which(ab<=esp))/it
print(i)
}
plot(1:n,prob,xlab='n',ylab='p(|Tn-alp|<=esp)',type="l",main="convergence in probability")
abline(h=1,col="red")
###### 2) Asymptotic Normality
n=2000
it=500
alp=2
exp=1/alp
estimate=c()
for(i in 1:it)
{
samp=rexp(n,rate=alp)
estimate[i]=mean(samp)
}
par(mfrow=c(2,2))
hist(estimate-exp)
boxplot(estimate-exp,main="BOXPLOT")
m=median(estimate-exp)
abline(h=m,col="red")
qqnorm(estimate-exp)
qqline(estimate-exp,col="green")
############## Percintile based estimator
### Here we use sample pth percintile as an estimator of population pth percintile
##  1) convergence in probability
par(mfrow=c(1,1))
sort_samp=function(n)
{
samp=rexp(n,rate=2)
sort_sa=sort(samp)
return(sort_sa)
}
alp=2
p=0.5  ## second quartile
n=2000
it=500
pop_pth_per=log(0.5)/-alp   ## we have calculated by inverse of cdf
esp=0.15
prob=c()
for(i in 1:n)
{
est=c()
for(j in 1:it)
{
s=sort_samp(i)
ind=(n*p)+1
est[j]=s[ind]
}
ab=abs(est-pop_pth_per)
prob[i]=length(which(ab<=esp))/it
print(i)
}
plot(1:n,prob,xlab='n',ylab='p(|Tn-alp|<=esp)',type="l",main="convergence in probability")
abline(h=1,col="red")
######### 2) Asymptotic normality
est=c()
it=500
n=1000
alp=2
p=0.5
pop_pth_per=log(0.5)/-alp
for(i in 1:it)
{
samp=rexp(n,rate=alp)
sort_sa=sort(samp)
ind=(n*p)+1
tn=sort_sa[ind]
est[i]=tn-pop_pth_per
}
hist(est)
boxplot(est)
m=median(est)
abline(h=m,col='red')
qqnorm(est)
qqline(est,col="red")
########################################






