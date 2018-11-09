# Figure 2
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

oM=1

xx=seq(0,20,0.1)

labs=c("A","B","C","D")
ylabs=c("fertility F","","","")
yaxts=c("s","n","n","n")

paras=matrix(c(rep(3,9),rep(0.1,9),rep(4,9)),ncol=3,nrow=9)
paras[1,1]=1;paras[3,1]=5;
paras[4,2]=0.02;paras[6,2]=0.2;
paras[7,3]=2;paras[9,3]=8;
nparas=length(paras[,1])

#############
x11(width=11,height=3)

par(mfcol=c(1,4),oma = c(4,4.5,2,4.5),mar=c(0.5,0.5,0.5,0.5))
i0=0

titles <- c("Standard scenario","Increased mortality","Increased cost of reproduction","Increased fecundity")

for (ipar in c(5,6,9,3)){
  Fmax=paras[ipar,1]
  baseM=paras[ipar,2]
  c0=paras[ipar,3]
  i0=i0+1
  plot(xx,fert(xx),type="l",ylim=c(0,3.5),xlab="", ylab=ylabs[i0], yaxt=yaxts[i0],cex.lab=1.5,cex=2,cex.axis=1.5)
  text(1,3.4,labs[i0],cex=2, font=2)
  par(new = T)
  plot(xx,mort(xx), axes = F, xlab = "", ylab ="", type="l", lty=2,ylim=c(0,1.1),cex.lab=1.5,cex=2,cex.axis=1.5)
  
  mtext(side=3,at=10,line=0.75,text=titles[i0])
  
  if(i0==1){
    legend("topright",lty=c(1,2),legend=c("Fertility, F","Mortality, M"),bty="n",cex=1.4)
  }
  
}

axis(side=4,cex.axis=1.5)
mtext(side = 4, line = 3.5, "Mortality, M",cex=1.2)
mtext(expression(paste("Resources, x")),side=1,line=3,outer=TRUE,cex=1.2)
mtext("Fertility, F",side=2,line=3,outer=TRUE,at=0.5,cex=1.2)

#dev.copy2eps(file="figure_2.eps")
