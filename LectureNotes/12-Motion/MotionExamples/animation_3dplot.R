# Animating 3D plots example 

#### Required Libraries ####
library(ggplot2)
library(scatterplot3d)
library(viridis)

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC/panelwingGPC")
#### Functions ####

myColorRamp <- function(colors, values, minmax) { 
  v <- (values - min(minmax))/diff(range(minmax))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

# Creates base scatterplot3d plot with selected parameters (2d domain),
# and an output value (z-axis). Data should be data frame containing all values.
# Parameters and values should be name of column
# e.g. "Camber" or "Caoa" as character strings. To add to plot, call
# this function as gpc3d.object<-gpc3d(...)

gpc3d2<-function(data,parameter1,parameter2,parameter3,value,alpha,xadj,yadj,colors){
  par1<-data[,which(names(data)==parameter1)]/1000
  par2<-data[,which(names(data)==parameter2)]
  par3<-data[,which(names(data)==parameter3)]
  val<-data[,which(names(data)==value)]
  if(is.na(colors[1])==TRUE){
    cols<-viridis(length(val),option="A")
  } else {
      cols <- myColorRamp(colors, val, range(val))
  }  
  gpc3d.object<-scatterplot3d(par1,par2,par3,
                              pch=16,color=cols,
                              xlab="Re (x1000)",
                              ylab=" ",
                              #ylab=parameter2,
                              zlab=parameter3,
                              angle=alpha,y.margin.add=1)
  dims <- par("usr")
  x <- dims[1]+ xadj*diff(dims[1:2])
  y <- dims[3]+ yadj*diff(dims[3:4])
  text(x,y,parameter2,srt=alpha,font=1,cex=1.0)
  gpc3d.object
  return(gpc3d.object)
}

point2dplot<-function(gpcdata,speciesdata,parameter,colors){
  gpcpar<-gpcdata[,which(names(gpcdata)==parameter)]
  speciespar<-speciesdata[,which(names(speciesdata)==parameter)]
  combined<-c(gpcpar,speciespar)
  ggplot(gpcdata,aes(Camber,AR,color=get(parameter)))+geom_point(size=3)+
    scale_color_gradient2(midpoint=mean(combined), 
                          limits=range(combined),
                          low=colors[1], mid=colors[2],high=colors[3], 
                          space ="Lab", guide=FALSE) +
    geom_point(data=speciesdata,mapping=aes(x=Median.Camber,
                                            y=Median.Aspect.Ratio,
                                            fill=get(parameter)),color="black",shape=21,size=3) +
    scale_fill_gradient2(midpoint=mean(combined), 
                         limits=range(combined),
                         low=colors[1], mid=colors[2],high=colors[3],
                         space ="Lab") +
    labs(fill=parameter)
}

#### Loading Data ####
parameters<-read.csv("birdwing_panelwingGPC_681_data.csv",header=TRUE) 

#### 3D Scatterplot Anim ####

w=5.25
h=5
# CLCD
i=75
for (i in seq(0,180,by=1)){
  png(filename=paste("animCLCD/CLCD",i,".png",sep=""),width=w,height=h,unit="in",res=200)
  gpc3d.object<-gpc3d2(parameters,"Camber","AR","Re","CLCD",i,0.85,0.2,c("steelblue","white"))
  gpc3d.object
  dev.off()
}

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC/paper/figures/gpcwingmodel")
png(filename="CLCD_gPC_raw.png",width=w,height=h,unit="in",res=300)
gpc3d.object<-gpc3d2(parameters,"Re","Camber","AR","slope",i,0.85,0.2,NA)
dev.off()