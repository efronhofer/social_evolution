# Fig. A2
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
DESPdiff=function(N,x0){
  
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

# different values of Fmax and base mortality to be analyzed
#F max = 3; c 0 = 4; M b = 0.1; o M = 1

# ------------------------------------------constant parameters
# fertility function
Fmax=3
c0=4

# mortality function
# constant parameter
oM = 1
Mb=0.1

# item size
theta=1

#range for integration
dx=0.02
IntBer <- seq(0.0,50,dx)

#-------------------------------------------------------------

# group size
#N = seq(1,10,1)
mass=0.1
mass1=0.1;
mass2=0.1;
mass3=0.1;
mass4=0.1;
totfeed=20
feed=totfeed/(mass+mass1+mass2+mass3)
VmassA=c();
VmassB=c()
VmassC=c()
VmassD=c()
VmassE=c()
NN=c(13,16,19,22,25)

for (t in seq(1,1000,1))
{
  diff=DESPdiff(NN[1],feed)
  mass=mass+100*mass*diff
  VmassA=c(VmassA,mass);
  diff1=DESPdiff(NN[2],feed)
  mass1=mass1+100*mass1*diff1
  VmassB=c(VmassB,mass1)
  diff2=DESPdiff(NN[3],feed)
  mass2=mass2+100*mass2*diff2
  VmassC=c(VmassC,mass2)
  diff3=DESPdiff(NN[4],feed)
  mass3=mass3+100*mass3*diff3
  VmassD=c(VmassD,mass3)
  diff4=DESPdiff(NN[5],feed)
  mass4=mass4+100*mass4*diff4
  VmassE=c(VmassE,mass4)
  feed=totfeed/(mass+mass1+mass2+mass3+mass4)
}

x11(width=5,height=5)
par(mar=c(4.5,4.5,0.5,0.5))

plot(seq(0.2,1000,1.0),VmassA,xlim=c(0,1000),xlab="Time, t",ylab="Population size, N",type="l",
     lty=3,ylim=c(0,12),lwd=3,cex.lab=1.5,cex.axis=1.2)
lines(seq(1.0,1000,1.0),VmassB,lwd=3,lty=2)
lines(seq(1.0,1000,1.0),VmassC,lwd=3,lty=1)
lines(seq(1.0,1000,1.0),VmassD,lwd=3,lty=4)
lines(seq(1.0,1000,1.0),VmassE,lwd=3,lty=5)
text(80,8.5,"N=13",cex=1.2)
text(300,6.7,"N=16",cex=1.2)
text(500,1.1,"N=22",cex=1.2)
text(90,-0.15,"N=25",cex=1.2)
text(700,9.5,"N=19",cex=1.2)

#dev.copy2eps(file="figure_a2.eps")

