# Fig. 4
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

#########################################################################

library(compiler)
enableJIT(1)

NINT=300000

pth=paste(getwd(),"/",sep="")

labs=c("A","E","B","F","C","G","D","H")
ylabs1=c(expression(paste("Resources, ",bar(x))),"","","")
ylabs2=c(expression("Group size, N"[opt]),"","","")
yaxts=c("s","n","n","n")

titles <- c("Standard scenario","Increased mortality","Increased cost of reproduction","Increased fecundity")

###############################################################
#### main programm
###############################################################

thetas=c(0.1,0.25,0.5,1,2,4,8,16)
x11(width=11,height=5.25)
par(mfcol=c(2,4),oma = c(4,4.5,2,0),mar=c(0.5,0.5,0.5,0.5))

i0par=0
i0 = 0

for (ipar in c(5,6,9,3)){
  i0par=i0par+2
  
  
  load(file=paste0(pth,"Fig4Data",i0par,".RObj"))
  
  plot(thetas,DESPx,ylim=c(1,6.5), ylab=ylabs1[i0par/2],log="x",type="b",pch=2,xaxt="n",yaxt=yaxts[i0par/2],cex.lab=1.5,cex=2,cex.axis=1.5)
  points(thetas,EGALx,pch=1,type="b",cex=2)
  abline(v=1,lty=3)
  abline(v=10,lty=3)
  
  if(ipar==5){
    legend("topright",bty="n",pch=c(2,1),cex=1.75,legend=c("eusocial","egalitarian"))
  }
  
  text(0.11,6.3,labs[i0par-1],cex=2,font=2)
  
  i0 = i0 +1
  mtext(side=3,at=1.3,line=0.75,text=titles[i0])
  
  
  
  plot(thetas,DESPN,log="x",ylim=c(0,42), ylab=ylabs2[i0par/2],type="b",pch=2,yaxt=yaxts[i0par/2],cex.lab=1.5,cex=2,cex.axis=1.5,xaxt="n")
  points(thetas,EGALN,pch=1,type="b",cex=2)
  
  axis(side=1,at=c(0.1,0.3,1,3,10),labels=c(0.1,0.3,1,3,10),cex.axis=1.5)
  
  #legend(0.2,16,legend=c("egal.","desp."),pch=1:2)
  text(0.11,40,labs[i0par],cex=2, font=2)
  
  abline(v=1,lty=3)
  abline(v=10,lty=3)
  
  
  abline(h=1,lty=2)
  
  #dev.off()
  
  #pdf(paste0(pth,"theta3",".pdf"),width=8,height=7)
  
  
}    # paras end

mtext(expression(paste("Environmental variance, ", theta)),side=1,line=3,outer=TRUE,cex=1.2)
mtext(expression(paste("Resources, ",bar(x))),side=2,line=2.5,outer=TRUE,at=0.75,cex=1.2)
mtext(expression("Group size, N"[opt]),side=2,line=2,outer=TRUE,at=0.25,cex=1.2)

#dev.copy2eps(file="figure_4.eps")
