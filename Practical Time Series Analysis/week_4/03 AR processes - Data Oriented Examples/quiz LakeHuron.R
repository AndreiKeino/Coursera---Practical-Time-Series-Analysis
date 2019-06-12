#Q 1
LakeHuron
plot(LakeHuron)
# Q 2
data<- LakeHuron
plot(diff(data)) # Edit this line

# Q 3
pacf(diff(data), main='PACF')

# Q 5

acf(diff(LakeHuron), main='ACF')$acf[1:3]

# Q 7

R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
r=NULL
r[1:2]=acf(diff(LakeHuron), plot=F)$acf[2:3]
R[1,2]=r[1] # only diagonal entries are edited
R[2,1]=r[1] # only diagonal entries are edited
R
b=matrix(r,nrow=2,ncol=1)
b

# Continue with a routine here to find the coefficients of the fitted model. 
# See parameter estimation in this lesson for help.

phi.hat=solve(R,b)[,1]
phi.hat

# Q 8

R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
r=NULL
r[1:2]=acf(diff(LakeHuron), plot=F)$acf[2:3]
R[1,2]=r[1] # only diagonal entries are edited
R[2,1]=r[1] # only diagonal entries are edited
R
b=matrix(r,nrow=2,ncol=1)
b
phi.hat<-solve(R,b)
phi.hat

c0=acf(diff(LakeHuron), type='covariance', plot=F)$acf[1]

var.hat=c0*(1-sum(phi.hat*r))
var.hat




