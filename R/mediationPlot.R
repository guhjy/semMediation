require(ggplot2)
require(lavaan)
require(plyr)

#' Make a data.frame for mediationPlot
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#'
#'@export
fit2df=function(fit){
   res=parameterEstimates(fit,standardized=TRUE)
   res
   ## latent variable
   res1=res[res$op!=":=",]
   res1
   text<-group<-x<-y<-latent<-c()
   count=0
   res1
   for(i in 1:nrow(res1)){
        #i=1
        temp=res1$lhs[i]
        temp
        if(!(temp %in% text)){
          text=c(text,temp)
          # whether temp is a latent varible
          tempres=any(res1[res1$lhs==temp,]$op=="=~")
          latent<-c(latent,tempres)
          # group determination #
          (tempgroup=seekGroup(temp,res1,group))

          group=c(group,tempgroup)

        }
        temp=res1$rhs[i]
        if(!(temp %in% text)){
            text=c(text,temp)
            # whether temp is a latent varible
            tempres=any(res1[res1$lhs==temp,]$op=="=~")
            latent<-c(latent,tempres)
            # group determination #
            (tempgroup=seekGroup(temp,res1,group))

            group=c(group,tempgroup)

        }
   }
   group
   df=data.frame(text,latent,group,stringsAsFactors = FALSE)
   df=addHpos(df)
   df
}


#' Add horizontal position
#'
#' @param df A data.frame made by fit2df function
addHpos=function(df){
    df1=df[substr(df$group,1,1)=="H",]
    df1
    if(nrow(df1)>0){
        for(i in 1:nrow(df1)){

            temp=unlist(strsplit(df1$group[i],",",fixed=TRUE))[2]
            temp
            pos=substr(df[df$text==temp,]$group,2,2)
            pos
            df[df$text==df1$text[i],]$group=paste0("H",pos,",",temp)
        }
    }
    df
}


#'Find group with variable name
#'
#'@param var A string to seek
#'@param  res A data.frame. Result of parameterEstimates function of package lavaan or subset.
seekGroup1=function(var,res){

  tempgroup<-""
  tofind<-var
  mode<-0

  res4<-res[res$op=='~',]

  (Left<-res4$lhs)
  (Right<-res4$rhs)


  if(any(Left==tofind)) {
     mode<-mode+1
     #print("mode<-mode+1")
   }
   if(any(Right==tofind)) {
     mode<-mode+2
     #print("mode<-mode+2")
   }
    mode
   if(mode==3) {
     tempgroup="M"
  } else if(mode==1) {
       tempgroup="Y"
  } else if(mode==2) tempgroup="X"
  tempgroup
  if(tempgroup==""){
    res5=res[res$op=='=~',]

    #if(tofind %in% res5$lhs) tempgroup="Y"
    if(tofind %in% res5$lhs) tempgroup="X"
  }

   #print(group)
   # print(mode)
   # print(tofind)
   # print(Left)
   # print(Right)
   # print(tempgroup)
   tempgroup

}


#'Find group with variable name
#'
#'@param var A string to seek
#'@param  res A data.frame. Result of parameterEstimates function of package lavaan or subset.
#'@param group A character vector
seekGroup2=function(var,res,group){

  res3=res[(res$rhs==var) & (res$op!="~~"),]
  temp=res3$lhs[1]
  if(is.na(temp)) result="X"
  else result=seekGroup1(temp,res)
  tempgroup=""
  if(result=="X") tempgroup="0"
  else if(substr(result,1,1)=="M") {
     # print("\n")
     # print(var)
     # print(temp)
     # print(result)
     # print(group)
    tempgroup="H"
  } else if(result=="Y") tempgroup="5"
  paste0(tempgroup,",",temp)
}

#' Count the group names start with "M"
#'@param group A string vectors
countM=function(group){
    result=0
    if(length(group)>0){
      for(i in 1:length(group)){
          if(substr(group[i],1,1)=="M") result=result+1
      }
    }
    result


}


#'Find group with variable name
#'
#'@param var A string to seek
#'@param  res A data.frame. Result of parameterEstimates function of package lavaan or subset.
#'@param group A string vector
seekGroup=function(var,res,group){
   # res=res1;var=temp
    (result=seekGroup1(var,res))
   if(result=="M"){
       count=countM(group)
       result=paste0(result,count+1)
   }
   if(result=="") result=seekGroup2(var,res,group)
   result
}



#' Add x and y position to data
#' @param df A data.frame. Result of fit2df function
addpos=function(df){
  for(i in 1:nrow(df)){
  df$group1[i]=unlist(strsplit(df$group[i],",",fixed=TRUE))[1]
  df$group2[i]=ifelse(df$group[i]==df$group1[i],"",unlist(strsplit(df$group[i],",",fixed=TRUE))[2])
  }
  df
  dfM=df[substr(df$group,1,1)=="M",]
  (countM=nrow(dfM))


  df$x=0
  if(nrow(df[df$group1=="X",])>0) df[df$group1=="X",]$x=1
  if(nrow(df[df$group1=="M1",])>0) df[df$group1=="M1",]$x=2
  if(countM==2) {
      if(nrow(df[df$group1=="M2",])>0) df[df$group1=="M2",]$x=2
  } else if(countM>2){
    condition1 <- (substr(df$group1,1,1) =="M") & (as.numeric(substr(df$group1,2,2))<=(countM+1)%/%2)

    if(nrow(df[condition1,])>0) df[condition1,]$x=2+as.numeric(substr(df[condition1,]$group1,2,2))-1
    condition2 <- (substr(df$group1,1,1) =="M") & (as.numeric(substr(df$group1,2,2))>(countM+1)%/%2)
    if(countM%%2==0){
        if(nrow(df[condition2,])>0) df[condition2,]$x=2+as.numeric(substr(df[condition2,]$group1,2,2))-1-countM%/%2
    } else{
        #if(nrow(df[condition2,])>0) df[condition2,]$x=2+as.numeric(substr(df[condition2,]$group1,2,2))-(countM+1)%/%2-1+ifelse(as.numeric(substr(df[condition2,]$group1,2,2))%%2==0,0,0.5)
        if(nrow(df[condition2,])>0) df[condition2,]$x=2+as.numeric(substr(df[condition2,]$group1,2,2))-(countM+1)%/%2-0.5

    }
  }
  if(nrow(df[df$group1=="H",])>0) df[df$group1=="H",]$x=3
  if(nrow(df[df$group1=="Y",])>0) df[df$group1=="Y",]$x=3+ifelse(countM>2,(countM-1) %/% 2,0)
  if(nrow(df[df$group1=="5",])>0) df[df$group1=="5",]$x=4+ifelse(countM>2,(countM-1) %/% 2,0)



  df$y=1
  if(nrow(df>2)){
     for(i in 2:nrow(df)){
       if(substr(df$group1[i],1,1)!="H"){
          if(df$group1[i]==df$group1[i-1]) df$y[i]=df$y[i-1]+1
       } else{
          if(df$group2[i]==df$group2[i-1]) df$y[i]=df$y[i-1]+1
       }
     }
  }

  for(i in 1:countM){

      if(nrow(df[df$group1==paste0("H",i),])>0) df[df$group1==paste0("H",i),]$x= df[df$group1==paste0("M",i),]$x
  }
  if(countM>0) {
    condition=substr(df$group,1,1)=="M"
    if(nrow(df[condition,])>0) df[condition,]$y=ifelse(as.numeric(substr(df[condition,]$group1,2,2))<=(countM+1)%/%2,1,2)
  }
  if(nrow(df[substr(df$group1,1,1)=="H",])>0){
    condition=substr(df$group,1,1)=="H"
    if(nrow(df[condition,])>0) df[condition,]$y=ifelse(as.numeric(substr(df[condition,]$group1,2,2))<=(countM+1)%/%2,1,2)
  }
  df
}


#' Make a positions
#'
#' @param center A number indicating the center position
#' @param step A number indicating the intervals betweeon items
#' @param count A number indicating the count of items
jitterPos=function(center,step,count){


   if(count==1) {
     result=center
   } else if(count>1){
     step=step+1
     start=center-(count-1)*step*0.5
     result=c()
     for(i in 1:count){
         result=c(result,start+step*(i-1))
     }
   }
   result
}


#' Make a positions
#'
#' @param start A number indicating the start position
#' @param end A number indicating the end position
#' @param count A number indicating the count of items
distributePos=function(start,end,count){
  if(count==1) result=(start+end)/2
  else {
    (interval=(end-start)/(count-1))
    (result=seq(start,end,interval))
    #result=result[2:(length(result)-1)]
  }
  result
}


#' Adjust position for arrows
#'
#'@param df A data.frame
#'@param maxx maximum x position
#'@param maxy maximum y position
#'@param height A number indicating height of the rectangle
#'@param width A number indicating width of the rectangle
adjustPos=function(df,maxx=80,maxy=30,height=3,width=5){

  #df=df1;maxx=60;maxy=30;height=3;width=5
  df$group3=df$y
  count<-nrow(df[df$group1=="0",])
  count<-c(count,nrow(df[df$group1=="X",]))
  count<-c(count,nrow(df[substr(df$group1,1,1)=="M",]))
  count<-c(count,nrow(df[df$group1=="Y",]))
  count<-c(count,nrow(df[df$group1=="5",]))
  countH<-nrow(df[substr(df$group1,1,1)=="H",])
  countH
  count

  ## maxrow 및 maxcol 계산
  res=plyr::ddply(df,"group1",nrow)
  res
  (maxrow=max(res[nchar(res$group1)==1,]$V1))

  res$xpos=3
  condition1<- (nchar(res$group1)==2)&(as.numeric(substr(res$group1,2,2))<=(count[3]+1)%/%2)
  if(nrow(res[condition1,])>0) res[condition1,]$xpos=2
  if(nrow(res[nchar(res$group1)==1,])>0) res[nchar(res$group1)==1,]$xpos=1

  ## maxgroup :  group count except "H"
  (maxgroup=nrow(res[substr(res$group1,1,1)!="H",]))
  upperCol=res[(substr(res$group1,1,1)=="H")&(res$xpos==2),]
  lowerCol=res[(substr(res$group1,1,1)=="H")&(res$xpos==3),]
  (maxcol=maxgroup+max(sum(upperCol$V1-1),sum(lowerCol$V1-1)))

  maxcol


  starty=25
  endy=min(starty-(height+1)*(maxrow+1),5)
  endy


  mediation=0
  if(count[3]>0) mediation=1

  minx=5
  (stepx=5*3)
  (maxx=max(maxx,(maxgroup-1)*stepx+2*minx))

  (xpos=seq(minx,maxx,stepx))

  x<-y<-text<-group<-c()

  ## group1=="X"
  if(count[2]>0) for(i in 1:count[2]){

    x=xpos[2]

    if(nrow(df[df$group1=="X",])>0){
       df[df$group1=="X",]$x=x
       df[df$group1=="X",]$y=distributePos(starty+2,endy-2,count[2])
    }
  }

  # group1=="0"
  if(count[1]>0)  for(i in 1:count[1]){

    x=xpos[1]
    if(nrow(df[df$group1=="0",])>0){
    df[df$group1=="0",]$x=x
    df[df$group1=="0",]$y=distributePos(starty,endy,count[1])
    }
  }

  # group1=="Y"

  if(count[4]>0) {


      if(nrow(df[df$group1=="Y",])>0){
    df[df$group1=="Y",]$x=df[df$group1=="Y",]$x*stepx+minx
    # df[df$group1=="Y",]$y=distributePos(starty,min(5,starty-count[4]*(height+1)),count[4])
    df[df$group1=="Y",]$y=distributePos(starty,endy,count[4])
      }
  }

  # group1=="5"

  if(count[5]>0) {
      if(nrow(df[df$group1=="5",])>0){
    df[df$group1=="5",]$x=df[df$group1=="5",]$x*stepx+minx
    df[df$group1=="5",]$y=distributePos(starty,endy,count[5])
      }
  }

  df
  # group1=="M"
  if(count[3]>0) {
      if(nrow(df[substr(df$group,1,1)=="M",])>0){
    (df[substr(df$group,1,1)=="M",]$x=df[substr(df$group,1,1)=="M",]$x*stepx+minx)
      }

    condition<-(substr(df$group,1,1)=="M")&(df$y==1)
    if(nrow(df[condition,])>0){
    df[condition,]$y=(starty+endy)/2+stepx+height/2
    }
    condition2<-(substr(df$group,1,1)=="M")&(df$y==2)
    if(nrow(df[condition2,])>0){
    df[condition2,]$y=(starty+endy)/2-stepx-height/2
    }

  }

  # group1=="H"
  if(countH>0) {

    condition<-(substr(df$group,1,1)=="H")&(df$y==1)
    tempcount=nrow(df[condition,])
    if(tempcount>0){
          df[condition,]$y=(starty+endy)/2+stepx*2+height/2
          df[condition,]$x=jitterPos(mean(unique(df[condition,]$x))*stepx+minx,width+1,tempcount)
    }
    condition2<-(substr(df$group,1,1)=="H")&(df$y==2)
    tempcount2=nrow(df[condition2,])
    if(tempcount2>0){
      df[condition2,]$y=(starty+endy)/2-stepx*2-height/2
      df[condition2,]$x=jitterPos(mean(unique(df[condition2,]$x))*stepx+minx,width+1,tempcount2)
    }


  }

   # unique(df$group1)
   # ("X" %in% unique(df$group1))&("0" %in% unique(df$group1))
   # (count=nrow(df[df$group=="X",]))
   # i=1
   # (temp=df[df$group=="X",]$text[i])
   # nrow(df[df$group2==temp])>0
   # df[df$text==temp,]$y
   # mean(df[df$group2==temp,]$y)
   #
  if(length(unique(df$group1))==2){
      if(("X" %in% unique(df$group1))&("0" %in% unique(df$group1))){
          count=nrow(df[df$group=="X",])
          for(i in 1:count){
              (temp=df[df$group=="X",]$text[i])
              if(nrow(df[df$group2==temp,])>0){
                  df[df$text==temp,]$y=mean(df[df$group2==temp,]$y)

              }

          }
          df[df$group1=="X",]$x=maxx-15
          df[df$group1=="0",]$x=15
      }
  }

  (xcount=nrow(df[df$group=="X",]))
  (mcount=nrow(df[df$group %in% c("M1","M2"),]))
  if((xcount>1)&(mcount==1)){
      df[df$group=="M1",]$y=mean(df[df$group=="X",]$y)
  }
  df

}


#' Make a data.frame fof ellipse
#' @param hlaxa An integer
#' @param hlaxb An integer
#' @param theta An integer
#' @param xc An integer indicating center of x position
#' @param yc An integer indicating center of y position
#' @param npoints An integer
ellipse=function (hlaxa = 1, hlaxb = 1, theta = 0, xc = 0, yc = 0,
                  npoints = 100)
{
  a <- seq(0, 2 * pi, length = npoints + 1)
  x <- hlaxa * cos(a)
  y <- hlaxb * sin(a)
  alpha <- angle(x, y)
  rad <- sqrt(x^2 + y^2)
  xp <- rad * cos(alpha + theta) + xc
  yp <- rad * sin(alpha + theta) + yc
  #if (newplot)
  #    plot(xp, yp, type = "l", ...)
  #else lines(xp, yp, ...)
  #invisible()
  df=data.frame(x=xp,y=yp)
  df
}


#' Calculate angle
#'
#' @param x An integer
#' @param y An integer
angle=function (x, y)
{
  angle2 <- function(xy) {
    x <- xy[1]
    y <- xy[2]
    if (x > 0) {
      atan(y/x)
    }
    else {
      if (x < 0 & y != 0) {
        atan(y/x) + sign(y) * pi
      }
      else {
        if (x < 0 & y == 0) {
          pi
        }
        else {
          if (y != 0) {
            (sign(y) * pi)/2
          }
          else {
            NA
          }
        }
      }
    }
  }
  apply(cbind(x, y), 1, angle2)
}


#' Make an ellipse
#' @param x An integer indicating x position
#' @param y An integer indicating y position
#' @param theta An integer
#' @param height An integer
#' @param width An integer
#' @param npoints An integer
#' @param color A string indicating color
#' @param fill A string indicating color
#' @param ... further arguments to be passed to geom_polygon
#'
#' @importFrom ggplot2 geom_polygon aes
add_ellipse=function(x=0,y=0, theta = 0, height=height,width=width,
                     npoints = 100,color="black",fill="white",...){
  ## xr=4
  xr=4*width/5
  ## yr=1.5
  yr=height/2
  df1=ellipse(xr,yr,theta=theta,xc=x,yc=y,npoints=npoints)
  geom_polygon(data=df1,aes(x=x,y=y),color=color,fill=fill,...)

}


#' Merge Data with position
#' @param res A data.frame. Result of parameterEstimates function of package lavaan or subset.
#' @param df2 A data.grame
#' @param whatLabels What should the edge labels indicate in the path diagram? Choices are c("est","std","name").
#'@param width A number indicating width of the rectangle
#'@param height A number indicating height of the rectangle
mergeDataPos=function(res,df2,whatLabels="est",width=5,height=3){
  #  res<-resCor;whatLabels="est";width=5;height=3
  x<-y<-xend<-yend<-label<-curvature<-group<-position1<-c()
  x1<-y1<-x2<-y2<-start<-end<-c()
  res
  xonly=-1
  if(length(unique(df2$group1))==2) xonly=1
  for(i in 1:nrow(res)){
    tempcurvature=-0.3
    tempx=df2[df2$text==res$rhs[i],]$x
    tempy=df2[df2$text==res$rhs[i],]$y
    tempxend=df2[df2$text==res$lhs[i],]$x
    tempyend=df2[df2$text==res$lhs[i],]$y
    latent=df2[df2$text==res$lhs[i],]$latent
    x1=c(x1,df2[df2$text==res$lhs[i],]$x)
    y1=c(y1,df2[df2$text==res$lhs[i],]$y)
    x2=c(x2,df2[df2$text==res$rhs[i],]$x)
    y2=c(y2,df2[df2$text==res$rhs[i],]$y)
    start=c(start,ifelse(df2[df2$text==res$lhs[i],]$latent,"ellipse","rect"))
    end=c(end,ifelse(df2[df2$text==res$rhs[i],]$latent,"ellipse","rect"))

    ## Correlation
    if(res$lhs[i]==res$rhs[i]) {
      margin=height/3
      tempcurvature=-2
      tempgroup=df2[df2$text==res$lhs[i],]$group1
      (H1group=df2[(substr(df2$group1,1,1)=="H")&(df2$group3==1),]$group1)
      (H2group=df2[(substr(df2$group1,1,1)=="H")&(df2$group3==2),]$group1)
      (M1group=df2[(substr(df2$group1,1,1)=="M")&(df2$group3==1),]$group1)
      (M2group=df2[(substr(df2$group1,1,1)=="M")&(df2$group3==2),]$group1)

      if(tempgroup %in% c("0",M1group,M2group)) {
        tempx=tempx-width/2-ifelse(latent,width/10,0)
        tempy=tempy-height/3
        tempxend=tempxend-width/2-ifelse(latent,width/10,0)
        tempyend=tempyend+height/3
        position=1
      }
      if(tempgroup %in% H1group){
        tempx=tempx-margin
        tempy=tempy+height/2
        tempxend=tempxend+margin
        tempyend=tempyend+height/2
        position=2
      }
      if(tempgroup=="5"){
        tempcurvature=2
        tempx=tempx+width/2
        tempy=tempy-margin
        tempxend=tempxend+width/2
        tempyend=tempyend+margin
        position=3
      }


      if(tempgroup %in% c("X","Y")) {
          tempcurvature=2
          tempx=tempx-margin
          tempy=tempy-height/2
          tempxend=tempxend+margin
          tempyend=tempyend-height/2
          position=4
      }
      if(tempgroup %in% H2group) {
        tempcurvature=2
        tempx=tempx-margin
        tempy=tempy-height/2
        tempxend=tempxend+margin
        tempyend=tempyend-height/2
        position=4
      }



    } else if(res$op[i]=="=~") {
      tempgroup1=df2[df2$text==res$rhs[i],]$group1
      if(length(tempgroup1)>0){
      if(tempgroup1=="0") tempx=tempx+width/2

      (H1group=df2[(substr(df2$group1,1,1)=="H")&(df2$group3==1),]$group1)
      (H2group=df2[(substr(df2$group1,1,1)=="H")&(df2$group3==2),]$group1)
      (M1group=df2[(substr(df2$group1,1,1)=="M")&(df2$group3==1),]$group1)
      (M2group=df2[(substr(df2$group1,1,1)=="M")&(df2$group3==2),]$group1)
      if(tempgroup1 %in% H1group) tempy=tempy-height/2
      if(tempgroup1 %in% H2group) tempy=tempy+height/2
      if(tempgroup1=="5") tempx=tempx-width/2
      }
      tempgroup=df2[df2$text==res$lhs[i],]$group1
      if(length(tempgroup)>0){
      if(tempgroup %in% M1group) tempyend=tempyend+height/2
      if(tempgroup %in% M2group) tempyend=tempyend-height/2
      if(tempgroup=="Y") tempxend=tempxend+width/2+ifelse(latent,width*3/10,0)
      if(tempgroup=="X") tempxend=tempxend-width/2-ifelse(latent,width*3/10,0)
      }
    } else if(res$op[i] %in% c("~","~~")) {
      position=1
      tempgroup1=df2[df2$text==res$rhs[i],]$group1
      if(length(tempgroup1)>0){
      (H1group=df2[(substr(df2$group1,1,1)=="H")&(df2$group3==1),]$group1)
      (H2group=df2[(substr(df2$group1,1,1)=="H")&(df2$group3==2),]$group1)
      (M1group=df2[(substr(df2$group1,1,1)=="M")&(df2$group3==1),]$group1)
      (M2group=df2[(substr(df2$group1,1,1)=="M")&(df2$group3==2),]$group1)
      latent=df2[df2$text==res$rhs[i],]$latent
      if(tempgroup1=="0") tempx=tempx+width/2
      if(tempgroup1 %in% H1group) tempy=tempy-height/2
      if(tempgroup1 %in% H2group) tempy=tempy+height/2
      if(tempgroup1=="5") tempx=tempx-width/2
      if(tempgroup1 %in% M1group) tempy=tempy-height/2
      if(tempgroup1 %in% M2group) tempy=tempy+height/2
      if(tempgroup1=="Y") tempx=tempx-width/2-ifelse(latent,width*3/10,0)
      if(tempgroup1=="X") tempx=tempx+width/2+ifelse(latent,width*3/10,0)
      }
      tempgroup=df2[df2$text==res$lhs[i],]$group1
      if(length(tempgroup)>0){
      latent=df2[df2$text==res$lhs[i],]$latent
      if(tempgroup=="0") tempxend=tempxend+width/2
      if(tempgroup %in% H1group) tempyend=tempyend-height/2
      if(tempgroup %in% H2group) tempyend=tempyend+height/2
      if(tempgroup=="5") tempxend=tempxend-width/2
      if(tempgroup %in% M1group) tempyend=tempyend-height/2
      if(tempgroup %in% M2group) tempyend=tempyend+height/2

      if(tempgroup=="Y") {
        tempxend=tempxend-width/2-ifelse(latent,width*3/10,0)
        position=1
      }
      if(tempgroup=="X") tempxend=tempxend+width/2+ifelse(latent,width*3/10,0)

      }
    }
    x=c(x,tempx)
    y=c(y,tempy)
    xend=c(xend,tempxend)
    yend=c(yend,tempyend)
    #df2
    if(res$op[i]=="~~") {
      curvature=c(curvature,tempcurvature)
      group=c(group,tempgroup)
      position1=c(position1,position)
    }
  }
  res$x=x
  res$y=y
  res$xend=xend
  res$yend=yend
  res$x1=x1
  res$y1=y1
  res$x2=x2
  res$y2=y2
  res$start=start
  res$end=end

  if(res$op[i]=="~~") {
    res$curvature=curvature
    res$group=group
    res$position=position1

  }
  if(whatLabels=="std") res$text=res[["std.all"]]
  else if(whatLabels=="name") res$text=res$label
  else res$text=res$est
  if(is.numeric(res$text)) res$text=sprintf("%0.2f",res$text)
  select=((res$op=="~~")&(res$lhs!=res$rhs)&(res$group=="X"))
  #res[select,]
  if(sum(select)>0){
      res[select,]$x= res[select,]$x1+xonly*width/2+xonly*ifelse(res[select,]$start=="rect",0,width*3/10)
      res[select,]$xend= res[select,]$x2+xonly*width/2+xonly*ifelse(res[select,]$start=="rect",0,width*3/10)
      res[select,]$curvature=0.3*xonly
      if(xonly) res[select,]$position=3
      else res[select,]$position=1
  }
  select=((res$op=="~~")&(res$lhs!=res$rhs)&(res$group=="Y"))
  if(sum(select)>0){
      res[select,]$x= res[select,]$x1+width/2+ifelse(res[select,]$start=="rect",0,width*3/10)
      res[select,]$xend= res[select,]$x2+width/2+ifelse(res[select,]$start=="rect",0,width*3/10)
      res[select,]$curvature=0.3
      res[select,]$position=3
  }
  select=((res$op=="~~")&(res$lhs!=res$rhs)&(res$group=="H1"))
  if(sum(select)>0){
      res[select,]$x= res[select,]$x1
      res[select,]$xend = res[select,]$x2
      res[select,]$y = res[select,]$y1+height/2
      res[select,]$yend = res[select,]$y2+height/2
      res[select,]$curvature=-0.3
      res[select,]$position=2
      #print(res[select,])
  }
  select=((res$op=="~~")&(res$lhs!=res$rhs)&(res$group=="5"))
  if(sum(select)>0){
      res[select,]$x= res[select,]$x1+width/2
      res[select,]$xend = res[select,]$x2+width/2
      res[select,]$y = res[select,]$y1
      res[select,]$yend = res[select,]$y2
      res[select,]$curvature=-0.3
      res[select,]$position=3
      #print(res[select,])
  }
  res

}

#' Merge Data with position for the indirect effect
#' @param resInd A data.frame.A subset of result of parameterEstimates function of package lavaan
#' @param res A data.frame. Result of parameterEstimates function of package lavaan or subset.
#' @param df2 A data.grame
#' @param whatLabels What should the edge labels indicate in the path diagram? Choices are c("est","std","name").
#'@param width A number indicating width of the rectangle
#'@param height A number indicating height of the rectangle
mergeDataPosInd=function(resInd,res,df2,whatLabels="est",width=5,height=3){
  x<-y<-latent<-label<-c()

  for(i in 1:nrow(resInd)){
    label=unlist(strsplit(resInd$rhs[i],"+",fixed=TRUE))
    label1=unlist(strsplit(label,"*",fixed=TRUE))[1]
    (lhs=res[res$label==label1,]$lhs)
    df2
    (tempx=df2[df2$text==lhs,]$x)
    (tempy=df2[df2$text==lhs,]$y)

    if(df2[df2$text==lhs,]$group3==1) {
      tempy=tempy-height*3/2
    } else tempy=tempy+height*3/2
    (templatent=df2[df2$text==lhs,]$latent)
    x=c(x,tempx)
    y=c(y,tempy)
    latent=c(latent,templatent)
  }
  resInd$x=x
  resInd$y=y
  resInd$latent=latent
  if(whatLabels=="std") resInd$text=resInd[["std.all"]]
  else if(whatLabels=="name") resInd$text=resInd$label
  else resInd$text=resInd$est
  if(is.numeric(resInd$text)) resInd$text=sprintf("%0.2f",resInd$text)
  resInd
}

#' Merge Data with position for the 2nd indirect effect
#' @param res2Ind A data.frame.A subset of result of parameterEstimates function of package lavaan
#' @param res A data.frame. Result of parameterEstimates function of package lavaan or subset.
#' @param df2 A data.grame
#' @param whatLabels What should the edge labels indicate in the path diagram? Choices are c("est","std","name").
#'@param width A number indicating width of the rectangle
#'@param height A number indicating height of the rectangle
mergeDataPos2Ind=function(res2Ind,res,df2,whatLabels="est",width=5,height=3){
  x<-y<-latent<-label<-c()

  resInd=res2Ind
  resInd
  for(i in 1:nrow(resInd)){

    (label1=unlist(strsplit(resInd$rhs[i],"*",fixed=TRUE))[1])
    (lhs=res[res$label==label1,]$lhs)
    (tempx=df2[df2$text==lhs,]$x)
    (tempy=df2[df2$text==lhs,]$y)
    (label2=unlist(strsplit(resInd$rhs[i],"*",fixed=TRUE))[2])
    (lhs2=res[res$label==label2,]$lhs)
    (tempy2=df2[df2$text==lhs2,]$y)
    tempy=(tempy+tempy2)/2
    (templatent=df2[df2$text==lhs,]$latent)
    x=c(x,tempx)
    y=c(y,tempy)
    latent=c(latent,templatent)
  }
  resInd$x=x
  resInd$y=y
  resInd$latent=latent
  if(whatLabels=="std") resInd$text=resInd[["std.all"]]
  else if(whatLabels=="name") resInd$text=resInd$label
  else resInd$text=resInd$est
  if(is.numeric(resInd$text)) resInd$text=sprintf("%0.2f",resInd$text)
  resInd
}


#' Add line type
#' @param res A data.frame. Result of parameterEstimates function of package lavaan or subset.
addLinetype=function(res){
  res$linetype="solid"
  if(nrow(res[is.na(res$pvalue),])>0) res[is.na(res$pvalue),]$linetype="dotted"
  if(nrow(res[(res$linetype=="solid")&(res$pvalue>0.05),])>0) {
    res[(res$linetype=="solid")&(res$pvalue>0.05),]$linetype="dotted"
  }
  res
}


#' Make a clean theme for ggplot
#' @param base_size An integer indicating base font size
#' @param base_family A character indicating base font family
#'
#' @importFrom ggplot2 theme_grey element_blank %+replace%
theme_clean=function(base_size=12,base_family="NanumGothic"){
    theme_grey(base_size,base_family=base_family) %+replace%
        theme(
            axis.title=element_blank(),
            axis.text=element_blank(),
            panel.background=element_blank(),
            panel.grid=element_blank(),
            axis.ticks.length=unit(0,"cm"),
            #axis.ticks.margin=unit(0,"cm"),
            panel.spacing=unit(0,"lines"),
            plot.margin=unit(c(0,0,0,0),"lines"),
            complete=TRUE
        )
}


#' Make a data.frame for mediationPlot
#'
#'@param fit A data.frame. Result of parameterEstimates function of package lavaan
#'@param maxx An integer indicating maximum x position
#'@param maxy An integer indicating maximum y position
#'@param height A number indicating height of the rectangle
#'@param width A number indicating width of the rectangle
#'@param whatLabels What should the edge labels indicate in the path diagram? Choices are c("est","std","name").
#'@param useLabel Whether use geom_label instead of geom_text. Default value is FALSE.
#'@param usecolor Whether use colors for variables. Default value is TRUE.
#'@param clean Whether use theme_clean. Default value is TRUE.
#'@param base_size An integer indicating the font size.
#'@param base_family A character indicating base font family
#'@param mediationOnly Whether or not draw mediation effect only. Default value is FALSE.
#'@param residuals Whether or not draw residuals(and variance). Default value is TRUE.
#'@param regression Whether or not draw regression. Default value is TRUE.
#'@param indirect Whether or not draw indirect effects. Default value is FALSE.
#'@param secondIndirect Whether or not draw 2nd indirect effects. Default value is FALSE.
#'@param total Whether or not draw total effect. Default value is FALSE.
#'@param mode  plot mode. 1 or 2.
#'
#'@importFrom lavaan parameterEstimates
#'@importFrom ggplot2 ggplot geom_rect geom_text theme_gray theme geom_label geom_segment geom_curve geom_text xlim ylim aes_string coord_fixed
#'
#'@export
mediationPlot=function(fit,maxx=80,maxy=30,height=5,width=5,whatLabels="std",useLabel=FALSE,usecolor=TRUE,
                       clean=TRUE,base_size=5,base_family="NanumGothic",
                       mediationOnly=FALSE,residuals=FALSE,regression=TRUE,
                       indirect=FALSE,secondIndirect=FALSE,total=FALSE,mode=1){

   # maxx=80;maxy=30;height=5;width=5;whatLabels="name";useLabel=TRUE;usecolor=TRUE
   # clean=TRUE;base_size=5;base_family="Arial"
   # mediationOnly=FALSE;residuals=TRUE;regression=TRUE
   #  indirect=FALSE;secondIndirect=FALSE;mode=1

  res=parameterEstimates(fit,standardized=TRUE)
  res
  #str(res)
  df=fit2df(fit)
  df
  #str(df)
  df1=addpos(df)
  df1

  if(mediationOnly) {
    Mgroup=df1[substr(df1$group1,1,1)=="M",]$group1
    Mgroup
    df1=df1[df1$group1 %in% c("X","Y",Mgroup),]
    res=res[(res$label!="")|((res$lhs %in% df1$text)&(res$rhs %in% df1$text)),]
    res
  }
  #str(df1)

  df2=adjustPos(df1,maxx=maxx,maxy=maxy,height=height,width=width)
  df2
  #cat("df2")
  #str(df2)
  df3=df2[df2$latent==FALSE,]
  df3
  df4=df2[df2$latent==TRUE,]
  df4


  p<-ggplot(data=df2,aes_string(x="x",y="y"))+
    xlim(min(df2$x)-12,max(df2$x)+12)+ylim(min(df2$y)-8,max(df2$y)+8)
  p
  if(nrow(df3)>0){
      if(usecolor) {
          p<-p+ geom_rect( data = df3,aes_string(fill="group1"),xmin=df3$x-width/2,xmax=df3$x+width/2,
                          ymin=df3$y-height/2,ymax=df3$y+height/2,colour="black",alpha=0.5)
      } else {
          p<-p+ geom_rect(data=df3,xmin=df3$x-width/2,xmax=df3$x+width/2,
                          ymin=df3$y-height/2,ymax=df3$y+height/2,color="black",fill="white",alpha=0.5)
      }
  }

  if(nrow(df4)>0){
      for(i in 1:nrow(df4)) p<-p+add_ellipse(df4$x[i],df4$y[i],height=height,width=width,fill=ifelse(usecolor,"yellow","white"))
  }
  p<-p+  geom_text(aes_string(label="text"),size=base_size,family=base_family)

  p
   #geom_point(data=df4,aes(fill=group1),size=30,shape=21)+

  if(clean) {
    p<-p+theme_clean(base_size=base_size,base_family=base_family)
  } else  p<-p+theme_grey(base_size=base_size,base_family=base_family)

  p<-p+theme(legend.position="none")

  p
  ## Measure
  p2<-p
  p<-p2
  #whatLabels="est"
  resMeasure=res[res$op=="=~",]
  resMeasure
  df2
  if(nrow(resMeasure)>0){
  resMeasure=mergeDataPos(resMeasure,df2,whatLabels,height=height,width=width)
  resMeasure=addLinetype(resMeasure)
  resMeasure
  if(mode==1){
        for(i in 1:nrow(resMeasure)){
            p<-p+addline(x1=resMeasure$x1[i],y1=resMeasure$y1[i],x2=resMeasure$x2[i],y2=resMeasure$y2[i],
               start=resMeasure$start[i],end=resMeasure$end[i],linetype=resMeasure$linetype[i],height=height,width=width)
            if(whatLabels!="name")
            p<-p+addlabel(x1=resMeasure$x1[i],y1=resMeasure$y1[i],x2=resMeasure$x2[i],y2=resMeasure$y2[i],
                          start=resMeasure$start[i],end=resMeasure$end[i],label=resMeasure$text[i],
                          useLabel=useLabel,size=base_size-1)
        }
        p
  } else{

        p<-p+geom_segment(data=resMeasure,aes_string(x="x",y="y",xend="xend",yend="yend"),
                    linetype=resMeasure$linetype,
                    arrow=arrow(angle=20,length=unit(0.3,"cm"),ends="first",type="closed"))
        if(whatLabels!="name"){
        if(useLabel) p<-p+geom_label(data=resMeasure,aes_string(x="(x+xend)/2",y="(y+yend)/2",label="text"),size=base_size-1)
        else p<-p+geom_label(data=resMeasure,aes_string(x="(x+xend)/2",y="(y+yend)/2",label="text"),label.size=0,size=base_size-1)
        }

  }

  }
  p
  p2<-p
  p<-p2

  ## Regressions

  if(regression){
  resReg=res[res$op=="~",]
  resReg
  if(nrow(resReg)>0){
  resReg=mergeDataPos(resReg,df2,whatLabels,height=height,width=width)
  resReg=addLinetype(resReg)
  resReg
  if(mode==1){
      for(i in 1:nrow(resReg)){
          #if(resReg$linetype[i]=="dotted") next
          #i=10
          if(whatLabels=="name"){
              p<-p+addline(x1=resReg$x2[i],y1=resReg$y2[i],x2=resReg$x1[i],y2=resReg$y1[i],
                           start=resReg$end[i],end=resReg$start[i],height=height,width=width)
          } else{
             p<-p+addline(x1=resReg$x2[i],y1=resReg$y2[i],x2=resReg$x1[i],y2=resReg$y1[i],
                       start=resReg$end[i],end=resReg$start[i],linetype=resReg$linetype[i],height=height,width=width)
          }
          p

      }
      for(i in 1:nrow(resReg)){

          p<-p+addlabel(x1=resReg$x2[i],y1=resReg$y2[i],x2=resReg$x1[i],y2=resReg$y1[i],
                        start=resReg$end[i],end=resReg$start[i],label=resReg$text[i],
                        useLabel=useLabel,size=base_size-1)
      }
      p

  } else {
  p<-p+geom_segment(data=resReg[substr(resReg$label,1,1)!="c",],aes_string(x="x",y="y",xend="xend",yend="yend"),
                    linetype=resReg[substr(resReg$label,1,1)!="c",]$linetype,
                    arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"))
  p<-p+geom_curve(data=resReg[substr(resReg$label,1,1)=="c",],aes_string(x="x",y="y",xend="xend",yend="yend"),
                  linetype=resReg[substr(resReg$label,1,1)=="c",]$linetype,curvature = 0.1,
                  arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"))

  if(useLabel) {
      p<-p+geom_label(data=resReg[substr(resReg$label,1,1)!="c",],aes_string(x="(x+xend)/2",y="(y+yend)/2",label="text"),size=base_size-1)
      p<-p+geom_label(data=resReg[substr(resReg$label,1,1)=="c",],aes_string(x="(x+xend)/2",y="(y+yend)/2-2",label="text"),size=base_size-1)
  }else {
      p<-p+geom_label(data=resReg[substr(resReg$label,1,1)!="c",],aes_string(x="(x+xend)/2",y="(y+yend)/2",label="text"),label.size=0,size=base_size-1)
      p<-p+geom_label(data=resReg[substr(resReg$label,1,1)=="c",],aes_string(x="(x+xend)/2",y="(y+yend)/2-2",label="text"),label.size=0,size=base_size-1)
  }

  p
  }

  }

  }



    ##  Correlation
    resCor=res[res$op=="~~",]
    if(!residuals) resCor=resCor[resCor$lhs!=resCor$rhs,]
    resCor
    df2
    if(nrow(resCor)>0){
      #height=3;width=5;whatLabels="est"
      resCor=mergeDataPos(resCor,df2,whatLabels,height=height,width=width)
      resCor=addLinetype(resCor)
      resCor
      #str(resCor)

      if(nrow(resCor[resCor$curvature==2,])>0)
        p<-p+geom_curve(data=resCor[resCor$curvature==2,],aes_string(x="x",y="y",xend="xend",yend="yend"),curvature=2.5,
                        #linetype=resCor[resCor$curvature==2,]$linetype,
                        arrow=arrow(angle=20,length=unit(0.2,"cm"),ends="both",type="closed"))
      if(nrow(resCor[resCor$curvature==-2,])>0)
        p<-p+geom_curve(data=resCor[resCor$curvature==-2,],aes_string(x="x",y="y",xend="xend",yend="yend"),curvature=-2.5,
                        #linetype=resCor[resCor$curvature==-2,]$linetype,
                        arrow=arrow(angle=20,length=unit(0.2,"cm"),ends="both",type="closed"))
      if(nrow(resCor[resCor$curvature==-0.3,])>0)
        p<-p+geom_curve(data=resCor[resCor$curvature==-0.3,],aes_string(x="x",y="y",xend="xend",yend="yend"),
                        #linetype=resCor[resCor$curvature==-0.3,]$linetype,
                        curvature=-0.2,
                        arrow=arrow(angle=20,length=unit(0.2,"cm"),ends="both",type="closed"))
      if(nrow(resCor[resCor$curvature==0.3,])>0)
          p<-p+geom_curve(data=resCor[resCor$curvature==0.3,],aes_string(x="x",y="y",xend="xend",yend="yend"),
                          #linetype=resCor[resCor$curvature==-0.3,]$linetype,
                          curvature=0.2,
                          arrow=arrow(angle=20,length=unit(0.2,"cm"),ends="both",type="closed"))

      if(whatLabels!="name"){
      if(useLabel) {
        #p<-p+geom_text(data=resCor[resCor$group %in% c("X","Y","H2"),],aes(x=(x+xend)/2,y=(y+yend)/2,label=label))
        if(nrow(resCor[resCor$position==4,])>0)
          p<-p+geom_label(data=resCor[resCor$position==4,],aes_string(x="(x+xend)/2",y="(y+yend)/2-height*3/4",label="text"),vjust=1.5)
        if(nrow(resCor[resCor$position==3,])>0)
          p<-p+geom_label(data=resCor[resCor$position==3,],aes_string(x="(x+xend)/2+width*1/2",y="(y+yend)/2",label="text"),hjust=-0.1)
        if(nrow(resCor[resCor$position==2,])>0)
          p<-p+geom_label(data=resCor[resCor$position==2,],aes_string(x="(x+xend)/2",y="(y+yend)/2+height*3/4",label="text"),vjust=-0.5)
        if(nrow(resCor[resCor$position==1,])>0)
          p<-p+geom_label(data=resCor[resCor$position==1,],aes_string(x="(x+xend)/2-width*1/2",y="(y+yend)/2",label="text"),hjust=1.1)
      }
      else {
        if(nrow(resCor[resCor$position==4,])>0)
          p<-p+geom_text(data=resCor[resCor$position==4,],aes_string(x="(x+xend)/2",y="(y+yend)/2-height*3/4",label="text"),vjust=1.5)
        if(nrow(resCor[resCor$position==3,])>0)
          p<-p+geom_text(data=resCor[resCor$position==3,],aes_string(x="(x+xend)/2+width*1/2",y="(y+yend)/2",label="text"),hjust=-0.1)
        if(nrow(resCor[resCor$position==2,])>0)
          p<-p+geom_text(data=resCor[resCor$position==2,],aes_string(x="(x+xend)/2",y="(y+yend)/2+height*3/4",label="text"),vjust=-0.5)
        if(nrow(resCor[resCor$position==1,])>0)
          p<-p+geom_text(data=resCor[resCor$position==1,],aes_string(x="(x+xend)/2-width*1/2",y="(y+yend)/2",label="text"),hjust=1.1)
      }
         }
    }


 p
  ## 2nd Indirect Effect
  if(secondIndirect){
  res2Ind=res[substr(res$label,1,6)=="second",]
  res2Ind
  if(nrow(res2Ind)>0){
    res2Ind=mergeDataPos2Ind(res2Ind,res,df2,whatLabels,height=height,width=width)
    res2Ind=addLinetype(res2Ind)
    res2Ind
    for(i in 1:nrow(res2Ind)){
      #i=1
      Reglist=unlist(strsplit(res2Ind$rhs[i],c("+"),fixed=TRUE))
      Reglist=unlist(strsplit(Reglist,c("*"),fixed=TRUE))
      Reglist
      resReglist=res[res$label %in% Reglist,]
      resReglist
      resReglist=mergeDataPos(resReglist,df2,whatLabels,height=height,width=width)
      resReglist$linetype=res2Ind$linetype[i]
      resReglist

      maxx=which.max(resReglist$xend)
      #p<-p+geom_segment(data=resReglist,aes(x=x,y=y,xend=xend,yend=yend),linetype=resReglist$linetype,
      #                  arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),color="blue",alpha=0.5,size=2)

      p<-p+geom_segment(data=resReglist[-maxx,],aes_string(x="x",y="y",xend="xend",yend="yend"),
                        linetype=resReglist[-maxx,]$linetype,
                        color="blue",alpha=0.5,size=1)
      p<-p+geom_segment(data=resReglist[maxx,],aes_string(x="x",y="y",xend="xend",yend="yend"),
                        linetype=resReglist[maxx,]$linetype,
                        arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),color="blue",alpha=0.5,size=1)
    p
    }
    if(whatLabels!="name") p<-p+geom_label(data=res2Ind,aes_string(x="x",y="y",label="text"),color="blue")

  }
  }
 p
 ## Indirect Effect
  if(indirect){

    resInd=res[substr(res$label,1,3)=="ind",]
    resInd
    if(nrow(resInd)>0){
      #height=3;width=5;whatLabels="est"
      resInd=mergeDataPosInd(resInd,res,df2,whatLabels,height=height,width=width)
      resInd=addLinetype(resInd)
      resInd
      for(i in 1:nrow(resInd)){

        Reglist=unlist(strsplit(resInd$rhs[i],"*",fixed=TRUE))
        resReglist=res[res$label %in% Reglist,]
        resReglist=mergeDataPos(resReglist,df2,whatLabels,height=height,width=width)
        resReglist$linetype=resInd$linetype[i]
        resReglist
        maxx=which.max(resReglist$xend)
        p<-p+geom_segment(data=resReglist[-maxx,],aes_string(x="x",y="y",xend="xend",yend="yend"),
                          linetype=resReglist[-maxx,]$linetype,
                          color="red",alpha=0.5,size=1)
        p<-p+geom_segment(data=resReglist[maxx,],aes_string(x="x",y="y",xend="xend",yend="yend"),
                          linetype=resReglist[maxx,]$linetype,
                          arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),color="red",alpha=0.5,size=1)


      }
      if(whatLabels!="name") p<-p+geom_label(data=resInd,aes_string(x="x",y="y",label="text"),color="red")

    }
  }
 if(total){
     res
     resTotal=res[substr(res$label,1,5)=="total",]
     resTotal
     if(nrow(resTotal)>0){
         #height=3;width=5;whatLabels="est"
         resTotal=mergeDataPosInd(resTotal,res,df2,whatLabels,height=height,width=width)
         resTotal=addLinetype(resTotal)
         resTotal
         for(i in 1:nrow(resTotal)){
             #i=1
             Reglist=unlist(strsplit(resTotal$rhs[i],"+",fixed=TRUE))
             Reglist=unlist(strsplit(Reglist,"*",fixed=TRUE))
             Reglist
             (resReglist=res[res$label %in% Reglist,])

             resReglist=mergeDataPos(resReglist,df2,whatLabels,height=height,width=width)
             #resReglist$linetype=resTotal$linetype[i]
             resReglist$effect="indirect"
             resReglist[substr(resReglist$label,1,1)=="c",]$effect="direct"
             resReglist=addLinetype(resReglist)
             resReglist
             select=resReglist$xend<max(resReglist$xend)
             resReglist[select,]
             resReglist[!select,]
             resReglist
             #p<-p2
             p<-p+geom_segment(data=resReglist[select,],aes_string(x="x",y="y",xend="xend",yend="yend",color="effect"),
                               linetype=resReglist[select,]$linetype,
                               alpha=0.5,size=1)
             p
             p<-p+geom_segment(data=resReglist[!select,],aes_string(x="x",y="y",xend="xend",yend="yend",color="effect"),
                               linetype=resReglist[!select,]$linetype,
                               arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),alpha=0.5,size=1)


         }
         select=(substr(res$label,1,1)=="c")
         (resTotal2=res[select,])
         resTotal2=mergeDataPos(resTotal2,df2,whatLabels,height=height,width=width)
         resTotal2$effect="direct"
         resTotal2$label=round(ifelse(whatLabels=="est",resTotal2$est,resTotal2$std.all),2)
         resTotal2$x=(resTotal2$x1+resTotal2$x2)/2
         resTotal2$y=(resTotal2$y1+resTotal2$y2)/2
         resTotal2=resTotal2[c("x","y","label","effect")]
         resTotal2
         select=(substr(res$label,1,3)=="ind")
         (resTotal3=res[select,])
         resTotal3=mergeDataPosInd(resTotal3,res,df2,whatLabels,height=height,width=width)
         resTotal3$effect="indirect"
         resTotal3$label=round(ifelse(whatLabels=="est",resTotal3$est,resTotal3$std.all),2)
         resTotal3=resTotal3[c("x","y","label","effect")]
         resTotal=rbind(resTotal2,resTotal3)
         if(whatLabels!="name"){
         if(useLabel) p<-p+geom_label(data=resTotal,aes_string(x="x",y="y",label="label",color="effect"))
         else p<-p+geom_label(data=resTotal,aes_string(x="x",y="y",label="label",color="effect"),label.size=0)
         }

     }
 }
  p<-p+coord_fixed()
  p

}


