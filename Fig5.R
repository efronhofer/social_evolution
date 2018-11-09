# Fig. 5
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

sets=c(5,6,9,3)

x11(width=11,height=3)

par(mfcol=c(1,4),oma = c(4,4.5,2,0),mar=c(0.5,0.5,0.5,0.5))

#par(mfcol=c(1,4),oma = c(4,4.5,0,2.5),mar=c(1, 2, 2, 2) + 0.1)
labs=c("A","B","C","D")
yaxts=c("s","n","n","n")

pth=paste(getwd(),"/",sep="")

titles <- c("Standard scenario","Increased mortality","Increased cost of reproduction","Increased fecundity")


###############################################################
#### main programm
###############################################################

iz0=0
for (iz in sets){
  iz0=iz0+1
  
  # item size
  # theta=5
  thetas=c(0.1,0.25,0.5,1,2,4,8,16)
  #thetas=c(0.1,0.15,0.25,0.25,0.5,0.75,1,1.5,2,3,4,6,8,11,16)

  load(file=paste0(pth,"Fig5LRSData",iz0,".RObj"))
  plot(thetas,rminvec,xlab="",ylab="",type="b",yaxt=yaxts[iz0],ylim=c(0,1),log="x",cex=2,cex.axis=1.5,pch=2,xaxt="n")
  text(0.12,0.95,labs[iz0],cex=2, font=2)
  
  axis(side=1,at=c(0.1,0.3,1,3,10),labels=c(0.1,0.3,1,3,10),cex.axis=1.5)
  
  
  mtext(side=3,at=1.3,line=0.75,text=titles[iz0])
  
}

mtext(expression(paste("Environmental variance, ", theta)),side=1,line=3,outer=TRUE,cex=1.2)
#mtext(expression(paste("environmental variance ", theta)),side=1,line=2,outer=TRUE)
mtext(expression("Relatedness, r"[min]),side=2,line=2.5,outer=TRUE,cex=1.2)

#dev.copy2eps(file="figure_5.eps")