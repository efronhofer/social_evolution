# Figure 1
# script by Achim Poethke, Oliver Mitesser and Emanuel Fronhofer

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

rm(list=ls())

x11(width=6,height=3.5)
par(mfcol=c(1,2),oma = c(3.5,4,2,0),mar=c(0.5,0.5,0.5,0.5))

x=seq(0,20,0.1)
labs=c("A","B")

mean=4
scale1=0.25
shape1=mean/scale1
dg=dgamma(x,shape=shape1,scale=scale1)
plot(x,dg,type="l", ylim=c(0,0.8),xlab="resources found x", ylab="frequency",lty=2, xlim=c(0,15),lwd=1.5)
text(0.5,0.77,labs[1],cex=1.2, font=2)

scale2=1
shape2=mean/scale2
dg=dgamma(x,shape=shape2,scale=scale2)
points(x,dg,type="l",lty=1,lwd=1.5)

scale1=4
shape1=mean/scale1
dg=dgamma(x,shape=shape1,scale=scale1)
points(x,dg,type="l", lty=3,lwd=1.5)

scale2=16
shape2=mean/scale2
dg=dgamma(x,shape=shape2,scale=scale2)
points(x,dg,type="l",lty=4,lwd=1.5)

mtext(side=3,at=7.5,expression(paste("Effect of mean resource item size (",theta,")",sep="")),line=0.75)
legend("topright",lty=c(2,1,3,4),legend=c(expression(paste(theta," = 0.25",sep="")),expression(paste(theta," = 1",sep="")),expression(paste(theta," = 4",sep="")),expression(paste(theta," = 16",sep=""))),bty="n")

mean=4
scale1=1/2
shape1=mean/scale1
dg=dgamma(x,shape=shape1,scale=scale1)
plot(x,dg,type="l", ylim=c(0,0.8),yaxt="n",xlab="resources found x",lty=2, xlim=c(0,15),lwd=1.5)
text(0.5,0.77,labs[2],cex=1.2, font=2)
scale2=1
shape2=mean/scale2
dg=dgamma(x,shape=shape2,scale=scale2)
points(x,dg,type="l",lty=1,lwd=1.5)

scale1=1/4
shape1=mean/scale1
dg=dgamma(x,shape=shape1,scale=scale1)
points(x,dg,type="l", lty=3,lwd=1.5)

scale2=1/8
shape2=mean/scale2
dg=dgamma(x,shape=shape2,scale=scale2)
points(x,dg,type="l",lty=4,lwd=1.5)

mtext(side=3,at=7.5,expression(paste("Effect of group size (",N,")",sep="")),line=0.75)
legend("topright",lty=c(1,2,3,4),legend=c("N = 1","N = 2", "N = 4", "N = 8"),bty="n")


mtext(expression(paste("Resources, x ")),side=1,line=2.25,outer=TRUE,cex=1.2)
mtext(expression(paste("Probability density, P"[(N)])),side=2,line=2,outer=TRUE,cex=1.2)

#dev.copy2eps(file="figure_1.eps")
