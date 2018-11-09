# Fig. 3
# script by Achim Poethke, Oliver Mitesser, Emanuel Fronhofer

########################################################################
# Copyright (C) 2018  Hans Joachim Poethke, Oliver Mitesser and Emanuel A. Fronhofer
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

########################################################################

paras=matrix(c(rep(3,9),rep(0.1,9),rep(4,9)),ncol=3,nrow=9)
paras[1,1]=1;paras[3,1]=5;
paras[4,2]=0.02;paras[6,2]=0.2;
paras[7,3]=2;paras[9,3]=8;
nparas=length(paras[,1])


# fertility function (can be used for vector arguments)
# parameters are global variables
# parameter transformation below
fert <- function(x) {Fmax*(1-exp(-(x/(Fmax*c0))^2))}

# mortality function (can be used for vector arguments)
mort <- function (x) 
{
  subfkt <- function (x)
  {
    if (x < oM) return(1)
    if (x >= oM) return(baseM)
  }
  return(sapply(x,subfkt))
}

# calculate difference of mortality and fertility
# for DESPOTIC groups when mean resources x0 are given
# later: find x0 such that diff = 0!
DESPdiff=function(x0){
  
  # calculate mean number of items (for a single individual)
  k=x0/theta
  
  # calculate gamma distribution of per capita amount of resources
  # IntBer: integration interval (has to start with value 0)
  dg=dgamma(IntBer,shape=N*k, scale = theta/N)
  dg[1]=0
  
  # check distribution
  # e.g. if integration interval is suitable
  if (sum(dg)<0.95) print("gamma problem DESP")
  dg=dg/sum(dg)
  
  # calculate mortality integral
  mu=(sum(dg[IntBer < oM])+baseM*sum(dg[IntBer >= oM]))*dx
  
  # remaining resources for reproduction
  xr=N*(IntBer[IntBer >= oM]-oM)
  
  # calculate fertility integral
  phi=dx*sum(dg[IntBer >= oM]*fert(xr))/N
  
  # return difference
  mu-phi
  
}

# calculate difference of mortality and fertility
# for EGALITERIAN groups when mean resources x0 are given
# later: find x0 such that diff = 0!
EGALdiff=function(x0){
  
  # calculate mean number of items (for a single individual)
  k=x0/theta
  
  # calculate gamma distribution of per capita amount of resources
  # IntBer: integration interval (has to start with value 0)
  dg=dgamma(IntBer,shape=N*k, scale = theta/N)
  dg[1]=0
  
  # check distribution
  if (dx*sum(dg)<0.95) print("gamma problem EGAL")
  dg=dg/sum(dg)
  
  # mortality
  mu=(sum(dg[IntBer < oM])+baseM*sum(dg[IntBer >= oM]))*dx
  
  # remaining resources for reproduction (per individual)
  xr=(IntBer[IntBer >= oM]-oM)	
  
  # fertility
  phi=sum(dg[IntBer >= oM]*fert(xr))*dx
  
  mu-phi
}


###############################################################
#### main programm
###############################################################

# item size
theta=5

# fertility function
# constant parameters
# sF = 0.5
# oF = 8

# mortality function
# constant parameter
oM = 1

# initialize lists to collect results
# DESPres/EGALres: resource means for a sequence of group sizes N
DESPres=list()
EGALres=list()

# initialize case counter
i0=0

# discretization
dx=0.01

# start loop over parameters
#for (Fmax in Fmaxvec){
#  for (baseM in baseMvec){
iz=5

Fmax=paras[iz,1]
baseM=paras[iz,2]
c0=paras[iz,3]

# screen output
print(c(Fmax, baseM))

# increase case counter
i0=i0+1

# transform parameters for fertility function
#cF = Fmax
#aF = -4 * sF/cF
#bF = -aF * oF

# Nmax for despotic groups
Nmax <- ceiling(Fmax/baseM)-1

# group sizes vector
N_s <- c(1:Nmax)

# Nmax for egaliterian groups
N2max=250

#################################
# find mean required amount fo resources 
# for despotic groups
# integration interval - could be smaller
IntBer <- seq(0.0,50,dx)

# collect mean resource values for specific Fmax and baseM
xvec=c()

# collect residual of optimization
rst=c()

for (N in 1:Nmax){
  
  #z=optimize(f=diff,lower=min(IntBer), upper=max(IntBer))
  z=uniroot(f=DESPdiff,lower=0.5, upper=5)
  xvec=c(xvec,z$root)
  #print(z)
  rst=c(rst,z$f.root)
  
  #xvec=c(xvec,z$minimum)
  #rst=c(rst,z$objective)
  
  # end of N loop
} 


#################################
# find mean required amount of resources 
# for egaliterian groups

# collect mean resource values for specific Fmax and baseM
xvec2=c()

# collect residual of optimization
rst2=c()

# integration interval - could be smaller
dx=0.002
IntBer <- seq(0.0,50,dx)

for (N in 1:N2max){
  
  #z=optimize(f=diff,lower=min(IntBer), upper=max(IntBer))
  z2=uniroot(f=EGALdiff,lower=0.5, upper=5)
  #z=optimize(f=diff)
  xvec2=c(xvec2,z2$root)
  #print(z2)
  rst2=c(rst2,z2$f.root)
  
  #xvec=c(xvec,z$minimum)
  #rst=c(rst,z$objective)
  
  # end of N loop
}

# store results  
DESPres[[i0]]=xvec
EGALres[[i0]]=xvec2


yval=DESPres[[1]]


###############################################
###############################################
###############################################
###############################################

x11(width=4.5,height=7)
par(mfrow=c(2,1),mar=c(2.5,4.5,0,0),oma=c(1.5,0,1,1))

qq=yval/yval[1]

ymin=min(qq)
xmin=which.min(qq)
Nmax=length(qq)

plot(qq,xaxt="n",yaxt="n",bty="o",xlab="",ylim=c(0.5,1.075),ylab="",type="b")
mtext(side=2,at=(1.075-0.5)/2+0.5, line=3.2,"Mean resouces needed")
mtext(side=2,at=(1.075-0.5)/2+0.5, line=2.2,"in equilibrium, "~ bar(x))

axis(1, at=c(1,xmin,Nmax),labels=c("1",expression(N[opt]),expression(N[max])))
axis(2, at=c(ymin, 1),labels=c(expression(bar(x)(N[opt])),expression(bar(x)(1))))

points(xmin,ymin,pch=19) 
lines(c(-10,xmin),c(ymin,ymin),lty=3)
points(1,1,pch=19) 
points(Nmax,qq[Nmax],pch=19) 
#lines(c(xmin,xmin),c(ymin,1))
lines(c(xmin,xmin),c(ymin,0.5),lty=3)
arrows(xmin, ymin, xmin, 1, code=3)
abline(h=1,lty=3)
text(13,0.9,"relative advantage\nof cooperation",cex=0.8)

lines(c(1,1),c(0,1),lty=3)
text(1.5,1.05,"A",cex=1.5, font=2)
#####################
#Mb2=1
#xs2=0
#Nmax2=floor((0.9999*Fmax)/Mb2)
#N2=1:Nmax2
#points(f1(N2,Fmax,n,xH,Mb,xs)/f1(1,Fmax,n,xH,Mb,xs),type="l",lty=2)

plot(1/qq,type="b",xaxt="n",yaxt="n",xlim=c(1,Nmax),ylim=c(0.9,1.9),bty="o",xlab="Group size, N",ylab="Carrying capacity, K")
axis(1, at=c(1,xmin,round(Nmax)),labels=c("1",xmin,round(Nmax)))
axis(2, at=c(1,1.2,1.4,1.6,1.8),labels=c(1,1.2,1.4,1.6,1.8))
points(xmin,1/ymin,pch=19) 
points(1,1,pch=19) 
points(Nmax,(1/qq)[Nmax],pch=19) 
lines(c(-10,xmin),c(1/ymin,1/ymin),lty=3)
lines(c(xmin, xmin),c(-10,1/ymin),lty=3)
lines(c(1,1),c(-10,1),lty=3)
lines(c(-10,1),c(1,1),lty=3)
text(1.5,1.86,"B",cex=1.5, font=2)
mtext(side=1,at=15,line=2.5,"Group size, N")
#dev.off()

#print(paste(theta,Fmax,baseM,c0))
#dev.copy2eps(file="figure_3.eps")

