#'Add a line to the semMediation plot
#'@param x1 The x coordinate of start point
#'@param y1 The y coordinate of start point
#'@param x2 The x coordinate of end point
#'@param y2 The y coordinate of end point
#'@param height A number indicating height of the rectangle
#'@param width A number indicating width of the rectangle
#'@param start The start geom. Either "rect" or "ellipse"
#'@param end The end geom. Either "rect" or "ellipse"
#'@param linetype The linetype passed to geom_segment()
#'@param size the size of geom_segment()
#'@param colour the colour of geom_segment()
#'@param ... Other arguments passed on to geom_segment()
#'
#'@importFrom grid arrow unit
addline=function(x1=0,y1=0,x2=10,y2=10,height=3,width=5,start="ellipse",end="rect",linetype="solid",size=0.2,colour="black",...){

      # x1=20;y1=26;x2=50;y2=25;start="rect";end="rect";height=5;width=5
      # linetype="solid";size=0.2;colour="black"
    # print(x1)
    # print(y1)
    # print(x2)
    # print(y2)
    # print(start)
    # print(end)
    (xend=get_endx(x1,y1,x2,y2,height=height,width=width,end=end))
    (xstart=get_startx(x1,y1,x2,y2,height=height,width=width,start=start))

    if(x1==x2){
        ystart=y1+ifelse(y1<y2,1,-1)*height/2
        yend=y2-ifelse(y1<y2,1,-1)*height/2
    } else{
        fun=function(x) (y2-y1)*(x-x1)/(x2-x1)+y1
        (yend=fun(xend))
        (ystart=fun(xstart))
    }


    ggplot(data=data.frame(x=c(-10,10)),aes_string("x"))+
        add_node(x1,y1,height=height,width=width,geom=start)+
        add_node(x2,y2,height=height,width=width,geom=end)+
        # stat_function(fun=fun1)+
        # stat_function(fun=fun2)+
        # stat_function(fun=g,colour="red")+
        xlim(c(min(x1,x2)-10,max(x1,x2)+10))+ylim(c(min(y1,y2)-10,max(y1,y2)+10))+
        geom_segment(x=xstart,y=ystart,xend=xend,yend=yend,arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"))


    # (yend=get_endy(x1,y1,x2,y2,height=height,width=width,end=end))
    # (ystart=get_starty(x1,y1,x2,y2,height=height,width=width,start=start))

    # p+geom_segment(x=xstart,y=ystart,xend=xend,yend=yend,arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),
    #              linetype=linetype,size=size,colour=colour)
   # if(y1!=y2) {
        geom_segment(x=xstart,y=ystart,xend=xend,yend=yend,arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),
                     linetype=linetype,size=size,colour=colour,...)
    # } else{
    #     geom_curve(x=xstart,y=ystart,xend=xend,yend=yend,arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),
    #                linetype=linetype,curvature=0.1,size=size,colour=colour,...)
    #     }

}

#'Add a label to the semMediation plot
#'@param x1 The x coordinate of start point
#'@param y1 The y coordinate of start point
#'@param x2 The x coordinate of end point
#'@param y2 The y coordinate of end point
#'@param height A number indicating height of the rectangle
#'@param width A number indicating width of the rectangle
#'@param start The start geom. Either "rect" or "ellipse"
#'@param end The end geom. Either "rect" or "ellipse"
#'@param label The label passed on to geom_label()
#'@param useLabel Whether use geom_label instead of geom_label. Default value is FALSE.
#'@param ... Other arguments passed on to geom_label()
addlabel=function(x1=0,y1=0,x2=10,y2=10,height=3,width=5,start="ellipse",end="rect",label=label,useLabel,...){

    xend=get_endx(x1,y1,x2,y2,height=height,width=width,end=end)
    xstart=get_startx(x1,y1,x2,y2,height=height,width=width,start=start)
    if(x1==x2){
        ystart=y1+ifelse(y1<y2,1,-1)*height/2
        yend=y2-ifelse(y1<y2,1,-1)*height/2
    } else{
        fun=function(x) (y2-y1)*(x-x1)/(x2-x1)+y1
        yend=fun(xend)
        ystart=fun(xstart)
    }
    x=(xstart+xend)/2
    y=(ystart+yend)/2

    # if(y1==y2){
    #     if(useLabel) geom_label(x=x,y=y,label=label,vjust=1.2,...)
    #     else geom_label(x=x,y=y,label=label,label.size=0,vjust=1.2,...)
    # } else

    if(x1==x2){
        if(useLabel) geom_label(x=x,y=y,label=label,vjust=-1.5,...)
        else geom_label(x=x,y=y,label=label,label.size=0,vjust=-1.5,...)
    } else{
        if(useLabel) geom_label(x=x,y=y,label=label,...)
        else geom_label(x=x,y=y,label=label,label.size=0,...)
    }


    #stat_function(fun=fun,xlim=c(x1,xend))

}



#'Get end x  position
#'@param x1 The x coordinate of start point
#'@param y1 The y coordinate of start point
#'@param x2 The x coordinate of end point
#'@param y2 The y coordinate of end point
#'@param height A number indicating height of the rectangle
#'@param width A number indicating width of the rectangle
#'@param end The end geom. Either "rect" or "ellipse"
get_endx=function(x1,y1,x2,y2,height,width,end="rect"){
    # x1=0;y1=0;x2=-30;y2=10;width=5;height=3;start="ellipse";end="ellipse"
    #x1=20;y1=26;x2=50;y2=25;width=5;height=5;start="rect";end="rect"

    if(abs(y1-y2)<height){
        if(end=="rect") x=x2-ifelse(x1<x2,1,-1)*width/2
        else x=x2-ifelse(x1<x2,1,-1)*width*4/5

    } else if(x1==x2){
        x=x1
    } else{
    fun1=function(x) (y2-y1)*(x-x1)/(x2-x1)+y1

    if(end=="rect"){
        if(y2>y1){
            fun2=function(x) y2-height/2
        } else {
            fun2=function(x) y2+height/2
        }
        g=function(x) fun1(x)-fun2(x)

        (x=uniroot(g,c(x1,x2))$root)
        if(x< (x2-width/2)) {
            x=x2-width/2
        } else if(x > (x2+width/2)) {
            x=x2+width/2
        }

    } else {
        if(y2>y1){
            fun2=function(x) y2-sqrt((1-(x-x2)^2/(width*4/5)^2)*(height/2)^2)

        } else {
            fun2=function(x) y2+sqrt((1-(x-x2)^2/(width*4/5)^2)*(height/2)^2)

        }
        g=function(x) fun1(x)-fun2(x)

        # ggplot(data=data.frame(x=c(-10,10)),aes(x))+
        #     add_ellipse(x1,y1,height=height,width=width)+
        #     add_ellipse(x2,y2,height=height,width=width)+
        #     stat_function(fun=fun1)+
        #     stat_function(fun=fun2,colour="blue")+
        #     stat_function(fun=g,colour="red")

        if(x2>x1) {
            (x=uniroot(g,c(x2,x2-width*4/5))$root)
        } else {
            (x=uniroot(g,c(x2,x2+width*4/5))$root)
        }

    }
    }
    x
}

#'Get start x  position
#'@param x1 The x coordinate of start point
#'@param y1 The y coordinate of start point
#'@param x2 The x coordinate of end point
#'@param y2 The y coordinate of end point
#'@param height A number indicating height of the rectangle
#'@param width A number indicating width of the rectangle
#'@param start The start geom. Either "rect" or "ellipse"
#'
#'@importFrom stats uniroot
get_startx=function(x1,y1,x2,y2,height,width,start="rect"){

    #  x1=35;y1=31.5;x2=50;y2=15;start="rect";height=5;width=7
    if(abs(y1-y2) < height){
        if(start=="rect") x=x1+ifelse(x1<x2,1,-1)*width/2
        else x=x1+ifelse(x1<x2,1,-1)*width*4/5
    } else if(x1==x2){
        x=x1
    } else{

        fun1=function(x) (y2-y1)*(x-x1)/(x2-x1)+y1

        if(start=="rect"){
            if(y2>y1){
                fun2=function(x) y1+height/2
            } else {
                fun2=function(x) y1-height/2
            }
            g=function(x) fun1(x)-fun2(x)

            # ggplot(data=data.frame(x=c(-10,10)),aes(x))+
            #     add_ellipse(x1,y1,height=height,width=width)+
            #     add_ellipse(x2,y2,height=height,width=width)+
            #     stat_function(fun=fun1)+
            #     stat_function(fun=fun2)+
            #     stat_function(fun=g,colour="red")

            if(x2>x1) {
                (x=uniroot(g,c(x1,x2))$root)
            } else {
                (x=uniroot(g,c(x2,x1))$root)
            }
            if(x>x1+width/2) x=x1+width/2
            if(x<x1-width/2) x=x1-width/2
        } else {
            if(y2>y1){
                fun2=function(x) y1+sqrt((1-(x-x1)^2/(width*4/5)^2)*(height/2)^2)
            } else {
                fun2=function(x) y1-sqrt((1-(x-x1)^2/(width*4/5)^2)*(height/2)^2)
            }
            g=function(x) fun1(x)-fun2(x)

            # ggplot(data=data.frame(x=c(-10,10)),aes(x))+
            #     add_ellipse(x1,y1,height=height,width=width)+
            #     add_ellipse(x2,y2,height=height,width=width)+
            #     stat_function(fun=fun1)+
            #     stat_function(fun=fun2)+
            #     stat_function(fun=g,colour="red")

            if(x2>x1) {
                (x=uniroot(g,c(x1,x1+width*4/5))$root)
            } else {
                (x=uniroot(g,c(x1-width*4/5,x1))$root)
            }
        }

    }


    x
}

add_node=function(x1,y1,height=5,width=5,geom="rect"){
    if(geom=="rect") add_rect(x1,y1,height,width)
    else add_ellipse(x1,y1,height=height,width=width)
}

add_rect=function(x1,y1,height=5,width=5){
    x=x1-width/2
    xend=x1+width/2
    y=y1-height/2
    yend=y1+height/2

    geom_rect(xmin=x,xmax=xend,ymin=y,ymax=yend,fill=NA,colour="black")
}


