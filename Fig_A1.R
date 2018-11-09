# Fig. A1
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


x11(width=8,height=8)
# fertility function 
fert <- function (x) Fmax*(1-exp(-((x/(c0*Fmax))^2)))

# mortality function (can be used for vector arguments)
mort <- function (x) 
{
  subfkt <- function (x)
  {
    if (x < oM) return(1)
    if (x >= oM) return(Mb)
  }
  return(sapply(x,subfkt))
}

# calculate difference of mortality and fertility
# for DESPOTIC groups when mean resources x0 are given
DESPdiff=function(x0){
  # calculate mean number of items (for a single individual)
  k=x0/theta
  # calculate gamma distribution of per capita amount of resources
  dg=dgamma(IntBer,shape=N*k, scale = theta/N)
  dg[1]=0
  # check distribution
  # e.g. if integration interval is suitable
  if (sum(dg)<0.95) print("gamma problem DESP")
  dg=dg/sum(dg)
  # calculate mortality integral
  mu=(sum(dg[IntBer < oM])+Mb*sum(dg[IntBer >= oM]))*dx
  
  # remaining resources for reproduction
  xr=N*(IntBer[IntBer >= oM]-oM)
  # calculate fertility integral
  phi=dx*sum(dg[IntBer >= oM]*fert(xr))/N
  
  # return difference
  phi-mu
}


###############################################################
#### main programm
###############################################################

oM = 1
dx=0.005
# IntBer: integration interval (has to start with value 0)
IntBer <- seq(0.0,50,dx)

#-------------------------------------------------------------
# produce plots for 3X3 parameter sets
par(mfrow=c(3,3),mar=c(3,3,0,0),oma=c(3,3,3,3))
#-------------------------------------------------------------

# parameter set 1, 2 and 3 --------------------------------------------
Fmax=3
c0=4
Mb=0.1
#calculate maximum viable group size
size=round(Fmax/Mb)-1
# item size theta
nopt=c(21,19,18)
ii=0
for (theta in c(0.1,1,10))
{
  ii=ii+1
  N1vec=seq(1,size,1)
  N2vec=seq(1,size,1)
  A=matrix(nrow=size,ncol=size)
  for (N1 in 1:size)
  {
    N=N1
    z=uniroot(f=DESPdiff,lower=0.1, upper=30)
    #nvec=c(nvec,N)
    x0=z$root
    for (N2 in 1:size)
    {
      N=N2
      aa=DESPdiff(x0) 
      A[N1,N2]=aa
    }  
  }
  contour(N1vec,N2vec,A,levels=c(-1,0.0,1),col=c("white","black"),xlab="", ylab="",
          xlim=c(1,size),ylim=c(1,size))
  .filled.contour(N1vec,N2vec,A,levels=c(-1,0.0,1),col=c("white","black"))
  lines(c(nopt[ii],nopt[ii]),c(0,nopt[ii]*(1.25)),lty=2)
  text(nopt[ii],nopt[ii]*(1.30),'Nopt')
}
# parameter set 4, 5 and 6 --------------------------------------------
Fmax=3
c0=4
Mb=0.2
size=round(Fmax/Mb)-1
# item size
nopt=c(11,9,8)
ii=0
for (theta in c(0.1,1,10))
{
  ii=ii+1
  N1vec=seq(1,size,1)
  N2vec=seq(1,size,1)
  A=matrix(nrow=size,ncol=size)
  for (N1 in 1:size)
  {
    N=N1
    z=uniroot(f=DESPdiff,lower=0.1, upper=30)
    x0=z$root
    for (N2 in 1:size)
    {
      N=N2
      aa=DESPdiff(x0) 
      A[N1,N2]=aa
    }  
  }
  contour(N1vec,N2vec,A,levels=c(-1,0.0,1),col=c("white","black"),xlab="population strategy", ylab="invader strategy",
          xlim=c(1,size),ylim=c(1,size))
  .filled.contour(N1vec,N2vec,A,levels=c(-1,0.0,1),col=c("white","black"))
  lines(c(nopt[ii],nopt[ii]),c(0,nopt[ii]*(1.2)),lty=2)
  text(nopt[ii],nopt[ii]*(1.25),'Nopt')
}
# parameter set 7, 8 and 9 --------------------------------------------
Fmax=5
c0=4
Mb=0.1
size=round(Fmax/Mb)-1
# item size
nopt=c(35,32,31)
ii=0
for (theta in c(0.1,1,10))
{
  ii=ii+1
  N1vec=seq(1,size,1)
  N2vec=seq(1,size,1)
  A=matrix(nrow=size,ncol=size)
  for (N1 in 1:size)
  {
    N=N1
    z=uniroot(f=DESPdiff,lower=0.1, upper=30)
    x0=z$root
    for (N2 in 1:size)
    {
      N=N2
      aa=DESPdiff(x0) 
      A[N1,N2]=aa
    }  
  }
  contour(N1vec,N2vec,A,levels=c(-1,0.0,1),col=c("white","black"),xlab="population strategy", ylab="invader strategy",
          xlim=c(1,size),ylim=c(1,size))
  .filled.contour(N1vec,N2vec,A,levels=c(-1,0.0,1),col=c("white","black"))
  lines(c(nopt[ii],nopt[ii]),c(0,nopt[ii]*(1.25)),lty=2)
  text(nopt[ii],nopt[ii]*(1.3),'Nopt')
}
mtext(expression(paste("Group size residents, ",N[R],sep="")),side=1,at=0.5,outer=T,line=1,cex=1.5)
mtext(expression(paste("Group size mutants, ",N[M],sep="")),side=2,at=0.5,outer=T,line=0,cex=1.5)

mtext(expression(paste(theta," = 0.1",sep="")),side=3,at=0.2,outer=T,line=1,cex=1)
mtext(expression(paste(theta," = 1",sep="")),side=3,at=0.525,outer=T,line=1,cex=1)
mtext(expression(paste(theta," = 10",sep="")),side=3,at=0.875,outer=T,line=1,cex=1)

mtext("standard",side=4,at=0.875,line=1,outer=T,cex=1)
mtext("increased mortality",side=4,at=0.525,line=1,outer=T,cex=1)
mtext("increased fertility",side=4,at=0.2,line=1,outer=T,cex=1)

###
#dev.copy2eps(file="figure_a1.eps")
###