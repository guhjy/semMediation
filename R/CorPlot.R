#' Draw correlation plot
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param label if 0, no label(default), if 1, use r value as label, if 2, use r value with significant mark as label
#' @param yreverse Logical. if true, reverse the order of y axis.
#' @param ... Further arugement to be passed on to ggCor
#'
#' @importFrom ggiraphExtra ggCor
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete
#' @export
#'
#' @return A ggplot
#' @examples
#'
#'# require(mycor)
#'# require(ggplot)
#'# require(ggiraphExtra)
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
#'# CorPlot(fit)
CorPlot=function(fit,label=2,yreverse=TRUE,...){
    data=fit@Data@X[[1]]
    colnames(data)=fit@Data@ov$name
    data=data.frame(data)
    p<-ggCor(data,label=2,...)+scale_x_discrete(limits=colnames(data))
    if(yreverse) p<-p+scale_y_discrete(limits=rev(colnames(data)))
    else p<-p+scale_y_discrete(limits=colnames(data))
    p
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
        names=c("chisq","df","pvalue","cfi","gfi","agfi","tli","rmr","srmr","rmsea","aic","bic")

    }
    names
    value=fitMeasures(fit)[names]
    value
    res=data.frame(rbind(value))
    rownames(res)="statistic"
    res=round(res,digits)
    res
}

#'convert data.frame to Flextable
#'
#'@param df a data.frame
#'@param bg background color of header
#'
#'@importFrom ReporteRs FlexTable cellProperties setZebraStyle parRight parCenter textProperties
#'
#'@export
#'
#' @return A Flextable
df2Flextable=function(df,bg="#5B7778"){
    colnames(df)[colnames(df)=="\u03B2"]="&#946;"
    MyTable=FlexTable(df,add.rownames = FALSE
                      , body.cell.props = cellProperties( border.color = "#EDBD3E")
                      , header.cell.props = cellProperties( background.color = bg )
                      , header.text.props = textProperties(color = "white",font.weight = "bold"))
    MyTable=setZebraStyle( MyTable, odd = "#DDDDDD", even = "#FFFFFF" )
    MyTable[,,to='header']=parCenter()
    MyTable[,]=parRight()
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
    result=parameterEstimates(fit,ci=ci,standardized=standardized)
    if(mediation){
       result=result[result$label!="",]
       result=result[-c(2)]
    } else{
        include=c()
        if(latent) include=c(include,"=~")
        if(regression) include=c(include,"~")
        if(covar) include=c(include,"~~")
        result=result[result$op %in% include,]
        result=result[-c(2,4)]
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
    result$se=round(result$se,digits)
    result$z=round(result$z,digits)
    result$std.all=round(result$std.all,digits)
    result$pvalue=convertPvalue(result$pvalue)
    if(mediation){
        result$lhs[substr(result$lhs,1,3)=="ind"]="indirect effect"
        result$lhs[substr(result$lhs,1,5)=="total"]="total effect"
        colnames(result)=c("Variables","Predictors","label","B","SE","z","pvalue","\u03B2")

    } else{
        colnames(result)=c("Variables","Predictors","B","SE","z","pvalue","\u03B2")
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





