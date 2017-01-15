#' Make a table with correlation
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param vanilla Logical. If true, vanilla.table is returned
#' @param addFooter Logical. If true, footer added
#'
#' @importFrom ReporteRs addFooterRow textProperties cellProperties parCenter
#' @importFrom mycor mycor
#' @export
corTable=function(fit,vanilla=TRUE,addFooter=FALSE){
    data=fit@Data@X[[1]]
    colnames(data)=fit@Data@ov$name
    data=data.frame(data)
    data
    result=mycor(data)
    result
    resp=p2asterisk(result$p)
    res=paste0(sprintf("%.2f",result$r),resp)
    res=matrix(res,ncol(result$r))
    res[upper.tri(res)]=""
    res[row(res)==col(res)]<-"1"
    rownames(res)=colnames(data)
    colnames(res)=colnames(data)
    res=as.data.frame(res)
    #str(res)
    Table=df2Flextable(res,vanilla=vanilla,add.rownames=TRUE)
    Table[,]=parCenter()
    if(addFooter){
    values=unique(as.vector(resp))
    temp=""
    if("***" %in% values) temp=pastecolon(temp,"***p<0.001")
    if("**" %in% values) temp=pastecolon(temp,"**p<0.01")
    if("*" %in% values) temp=pastecolon(temp,"*p<0.05")

    # Table=addHeaderRow( Table,value=paste0("N=",nrow(data)),colspan=ncol(result$r)+1,
    #                                       text.properties=textProperties(color="black",font.weight="normal"),
    #                                       par.properties=parProperties(text.align="right"),
    #                                       cell.properties = cellProperties( border.left.style="none", border.right.style="none",
    #                                                                         border.bottom.style="none") )
    Table=addFooterRow(Table,value=temp,colspan=ncol(result$r)+1,
                       text.properties=textProperties(color="black",font.weight="bold"),
                       cell.properties = cellProperties( border.left.style="none", border.right.style="none",
                                                         border.bottom.style="none"))
    }
    Table
}


#' paste two character with colon
#' @param temp a character
#' @param x a character
pastecolon=function(temp,x){
    if(temp=="") res=x
    else res=paste0(temp,";",x)
    res
}

#' Convert p values to asterisk
#' @param x a numeric vector or matrix
p2asterisk=function(x){
    ifelse(x<0.001,"***",ifelse(x<0.01,"**",ifelse(x<0.05,"*","")))
}



#'Extract model fit measures to data.frame
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @param names names of statistic to be extracted
#'
#' @importFrom lavaan fitMeasures
#' @export
#' @return A data.frame
modelFitTable=function(fit,digits=2,names=NULL){
    if(is.null(names)) {
        names=c("chisq","df","pvalue","cfi","gfi","agfi","tli","rmr","srmr","rmsea","rmsea.ci.lower","rmsea.ci.upper","aic","bic")
        newnames=c("chisq","df","p","CFI","GFI","AGFI","TLI","RMR","SRMR","RMSEA","lower","upper","AIC","BIC")

    }
    names
    value=fitMeasures(fit)[names]
    value
    res=data.frame(rbind(value))
    if(!is.null(newnames)) colnames(res)=newnames
    rownames(res)="statistic"
    res$x2df=res$chisq/res$df
    res=round(res,digits)
    res$RMSEA=paste0(res$RMSEA,"(",res$lower,"-",res$upper,")")
    colnames(res)[10]="RMSEA(95% CI)"
    res=res[c(1,2,15,3:10,13,14)]
    res
}

#'convert data.frame to Flextable
#'
#'@param df a data.frame
#'@param vanilla Logical. If true, vanilla.table is returned
#'@param bg background color of header
#'@param add.rownames logical value - should the row.names be included in the table.
#'@param mode string. Choices are c("html","pptx").
#'@param parRight logical - Whether the values aligned to the right
#'@param widths numeric vector
#'@importFrom ReporteRs FlexTable cellProperties setZebraStyle parRight parCenter textProperties vanilla.table setFlexTableWidths
#'
#'@export
#'
#' @return A Flextable
df2Flextable=function(df,vanilla=FALSE,bg="#5B7778",add.rownames=FALSE,mode="html",parRight=FALSE,widths=NULL){
    if(mode=="html"){
    colnames(df)[colnames(df)=="\u03B2"]="&#946;"
    colnames(df)[colnames(df)=="chisq"]="&#967;<sup>2</sup>"
    colnames(df)[colnames(df)=="x2df"]="&#967;<sup>2</sup>/df"
    colnames(df)[colnames(df)=="p"]="<i>p</i>"
    } else if(mode=="pptx"){
        colnames(df)[colnames(df)=="\u03B2"]="β"
        colnames(df)[colnames(df)=="chisq"]="χ2"
        colnames(df)[colnames(df)=="x2df"]="χ2/df"
        colnames(df)[colnames(df)=="p"]="p"
    }
    if(vanilla) {
        MyTable=vanilla.table(df,add.rownames = add.rownames)
    } else {
        MyTable=FlexTable(df,add.rownames = add.rownames
                      , body.cell.props = cellProperties( border.color = "#EDBD3E")
                      , header.cell.props = cellProperties( background.color = bg )
                      , header.text.props = textProperties(color = "white",font.weight = "bold"))
        MyTable=setZebraStyle( MyTable, odd = "#DDDDDD", even = "#FFFFFF" )
    }
    if(!is.null(widths)) MyTable=setFlexTableWidths(MyTable,widths=widths)
    MyTable[,,to='header']=parCenter()
    if(parRight) MyTable[,]=parRight()
    else MyTable[,]=parCenter()
    MyTable
}

#'convert parameterEstimates to data.frame
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param latent whether the latent variables be included in result
#' @param regression whether the regressions be included in result
#' @param mediation whether the mediation effects be included in result
#' @param covar whether the covariances be included in result
#' @param ci If TRUE, confidence intervals are added to the output
#' @param standardized Logical. If TRUE, standardized estimates are added to the output
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#'
#' @export
estimatesTable=function(fit,latent=TRUE,regression=TRUE,mediation=FALSE,covar=FALSE,ci=FALSE,standardized=TRUE,digits=2){
    #latent=TRUE;regression=TRUE;mediation=FALSE;covar=FALSE;ci=TRUE;standardized=TRUE;digits=2
    result=parameterEstimates(fit,ci=ci,standardized=standardized)
    result
    if(mediation){
       result=result[result$label!="",]
       result=result[-c(2)]
    } else{
        include=c()
        if(latent) include=c(include,"=~")
        if(regression) include=c(include,"~")
        if(covar) include=c(include,"~~")
        result=result[result$op %in% include,]
        #result=result[-c(2,4)]
        result=result[-c(2)]
    }
    no=ncol(result)
    result=result[-c(no-2,no)]
    result
    if(ci){
        result$est=paste0(round(result$est,digits),"(",round(result$ci.lower,digits),"-",round(result$ci.upper,digits),")")
        no=ncol(result)
        result=result[-c(no-1,no-2)]
    } else{
        result$est=round(result$est,digits)
    }
    result
    result$se=round(result$se,digits)
    result$z=round(result$z,digits)
    result$std.all=round(result$std.all,digits)
    result$pvalue=convertPvalue(result$pvalue)
    result
    if(mediation){
        result$lhs[substr(result$lhs,1,3)=="ind"]="indirect effect"
        result$lhs[substr(result$lhs,1,5)=="total"]="total effect"
        colnames(result)=c("Variables","Predictors","label","B","SE","z","p","\u03B2")

    } else{
        result=result[,names(result)!="label"]
        colnames(result)=c("Variables","Predictors","B","SE","z","p","\u03B2")
    }
    result[is.na(result)]=""
    result
}

#' convert vector of p values to string
#'
#' @param x vector of p values
convertPvalue=function(x){
    x=sprintf("%0.3f",x)
    x[x=="NA"]=""
    x[x=="0.000"]="< 0.001"
    x
}


#'@importFrom ggplot2 element_text
theme_clean2=function(base_size=12,axis.text.angle=45){
    theme_grey(base_size) %+replace%
        theme(
            panel.background=element_blank(),
            panel.grid=element_blank(),
            axis.title=element_blank(),
            axis.text.x=element_text(angle=axis.text.angle),
            axis.ticks.length=unit(0,"cm"),
            #axis.ticks.margin=unit(0,"cm"),
            #panel.margin=unit(0,"lines"),
            #plot.margin=unit(c(0,0,0,0),"lines"),
            complete=TRUE
        )
}

#' Draw correlation plot
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param label if 0, no label(default), if 1, use r value as label, if 2, use r value with significant mark as label
#' @param yreverse Logical. if true, reverse the order of y axis.
#' @param axis.text.angle axis.x.text.angle
#' @param ... Further arugement to be passed on to geom_text
#'
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete labs
#' @export
#'
#' @return A ggplot
#' @examples
#'
#'# require(mycor)
#'# require(ggplot)
#'# require(lavaan)
#'
#'# model='
#'# knowledge =~ general+symptoms+treatmt
#'# empathy =~ cognitiv+emotion+disposit+attitude
#'# intervention =~ classrm+instruct
#'# '
#'# mediationModel=makeEquation(X="knowledge",M="empathy",Y="intervention")
#'# model=paste0(model,mediationModel)
#'# fit=sem(model,data=ADHD)
#'# corPlot(fit)
corPlot=function(fit,label=2,yreverse=TRUE,axis.text.angle=0,...){
    data=fit@Data@X[[1]]
    colnames(data)=fit@Data@ov$name
    data=data.frame(data)
    p<-ggCor2(data,label=2,...)+scale_x_discrete(limits=colnames(data)[-length(colnames(data))])
    if(yreverse) p<-p+scale_y_discrete(limits=rev(colnames(data)[-1]))
    else p<-p+scale_y_discrete(limits=colnames(data))
    p<-p+theme_clean2(axis.text.angle=axis.text.angle)
    p<-p+theme(legend.position=c(0.8,0.8))+labs(fill="r value")
    p
}

#'@importFrom ggplot2 geom_tile scale_fill_gradient2 coord_equal xlab ylab ggtitle
#'@importFrom stats na.omit
#'@importFrom mycor mycor
ggCor2=function(data,label=0,colors=NULL,title=FALSE,interactive=FALSE,...){
    # df=iris;
    # result=mycor(iris)
    result=mycor(data,...)

    if(is.null(colors)) colors=c("#6D9EC1","white","#E46726")
    cor_mat<-result$r
    p_mat<-result$p

    diag( cor_mat ) <- NA
    diag( p_mat ) <- NA
    cor_mat[upper.tri(cor_mat)]=NA
    var1 <- rep( row.names(cor_mat), ncol(cor_mat) )
    var2 <- rep( colnames(cor_mat), each = nrow(cor_mat) )
    cor <- as.numeric(cor_mat)

    cor_mat <- data.frame( var1 = var1, var2 = var2,
                           cor = cor, stringsAsFactors = FALSE )
    pval=as.numeric(p_mat)
    cor_mat$label=ifelse(is.na(cor_mat$cor),"",sprintf("%0.2f",cor_mat$cor))
    label=2
    if(label==2) cor_mat$label=paste0(cor_mat$label,ifelse(is.na(pval),"",ifelse(pval<0.001,"***",ifelse(pval<0.01,"**",ifelse(pval<0.05,"*","")))))
    cor_mat$p=ifelse(is.na(pval),"",ifelse(pval<0.001,"< 0.001",sprintf(" = %0.3f",pval)))
    cor_mat
    cor_mat=na.omit(cor_mat)
    # ggplot creation and ggiraph printing ----
    p <- ggplot(data = cor_mat, aes_string(x = "var2", y = "var1") ) +
        geom_tile(aes(fill = cor), colour = "white") +
        scale_fill_gradient2(low = colors[1], mid = colors[2], high = colors[3], limits = c(-1, 1)) +
        coord_equal()+
        xlab("")+ylab("")
    if(title) {
        title=paste0(result$out$method,",",result$out$alternative)
        p<-p+ggtitle(title)
    }
    if(label>0) p<-p+geom_text(aes(label=label),...)
    p
}


#' Make a Cronbach alpha table
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#'
#' @importFrom psych alpha
#'
#' @export
fit2alpha=function(fit,digits=2){
    model=fit@Options$model
    submodels=unlist(strsplit(model,"\n"))
    latent=submodels[grep("=~",submodels)]
    latent=latent[grep("+",latent)]
    latent=gsub("[[:space:]]", "", latent)
    latent
    result=strsplit(latent,"=~")
    result
    result[[1]][2]
    strsplit(result[[1]][2],"+",fixed=TRUE)
    latentvar=c()
    indicator=c()
    vars=list()
    for(i in 1:length(result)){
        latentvar=c(latentvar,result[[i]][1])
        indicator=c(indicator,result[[i]][2])
        vars[[i]]=unlist(strsplit(result[[i]][2],"+",fixed=TRUE))
    }
    latentvar
    indicator
    vars


    data=fit@Data@X[[1]]
    colnames(data)=fit@Data@ov$name
    data=data.frame(data)
    data
    alpha<-lambda6<-c()
    for(i in 1:length(latentvar)){
        result=alpha(data[vars[[i]]])
        alpha=c(alpha,result$total$raw_alpha)
        lambda6=c(lambda6,result$total$`G6(smc)`)
    }
    alpha=round(alpha,digits)
    lambda6=round(lambda6,digits)
    res=data.frame(latentvar=latentvar,indicators=indicator,alpha=alpha,lambda6=lambda6)
    res
}

