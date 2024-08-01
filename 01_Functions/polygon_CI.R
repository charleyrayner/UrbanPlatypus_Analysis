# Polygon CI function

# Author: Annabel Smith

# ARUGUMENTS:

# x, data, x.subset, lower and upper must be in quotes
# x: a vector of data on the x-axis
# data: a data frame which includes x 
# x.subset: a vector of data for subsetting x. If there no subsets, omit this argument or set it at NULL to plot a single polygon. If there are subsets, enter the colname of data frame x to be used as the subset
# colour: either a single colour (if x.subset=NULL) or a vector of length(x.subset) defining different colours for the subsets
# lower: column name of the lower CI
# upper: column name of the upper CI

# update 13th Feb 2017: added an if to deal with two different cases that distinguish $uci.resp, etc.

# update 14th Feb 2024: I've added more ifs to deal with the CI notation in iNEXT. I also added the arguments for upper and lower, to make it easier to plot different data sets. Not yet widely tested, but working on a couple of different data sets. Not yet cleaned up; still some unnecessary ifs littered throughout. 

pg.ci<-function(x,data,x.subset=NULL,colour,lower,upper){
  
  # No subsets:
  # No subsets hasn't been updated with CI arguments
  if(is.null(x.subset)==T){
    xx<-paste(data,"$",x,sep="")
    # lci<- paste(data,"$lci",sep="")
    # uci<- paste(data,"$uci",sep="")
    if(length(grep(".resp",colnames(get(data))))>0) lci<-paste(data,"$lci.resp",sep="") else lci<-paste(data,"$lci",sep="")
    if(length(grep(".resp",colnames(get(data))))>0) uci<-paste(data,"$uci.resp",sep="") else uci<-paste(data,"$uci",sep="")
    
    xvec <- c(eval(parse(text=xx)), tail(eval(parse(text=xx)), 1), rev(eval(parse(text=xx))), eval(parse(text=xx))[1])
    yvec <- c(eval(parse(text=lci)), tail(eval(parse(text=uci)), 1), rev(eval(parse(text=uci))), eval(parse(text=lci))[1])
    polygon(xvec, yvec, col=colour, border=NA)
  } # close if no subsets
  
  # with subsets
  if(is.null(x.subset)==F){
    
    if(length(grep(lower,colnames(get(data))))>0) lci<-paste(data,"$",lower,sep="") else lci<-paste(data,"$uci",sep="")
    if(length(grep(upper,colnames(get(data))))>0) uci<-paste(data,"$",upper,sep="") else uci<-paste(data,"$uci",sep="")
    
    # Get data and vector that is used for subsetting:
    data.withsubset<-get(data)
    subset.all<-data.withsubset[,x.subset]
    
    # Specify subs.levs: levels for factors, unique numbers for binary variables, and first and third quartiles for continuous variables
    
    if(is.factor(subset.all)) subs.levs<-levels(subset.all)
    
    if(is.factor(subset.all)==F) {
      
      if(length(unique(subset.all))==2) subs.levs<-unique(subset.all) 

    } # close if subset is not a factor
    
    for (i in 1:length(subs.levs)){
      
      sub.thisrun<-subs.levs[i]
      x.thisrun<-data.withsubset[which(subset.all==sub.thisrun),x]
      # lci.thisrun<-data.withsubset[which(subset.all==sub.thisrun),"lci"]
      # uci.thisrun<-data.withsubset[which(subset.all==sub.thisrun),"uci"]
     
      if(length(grep(lower,colnames(data.withsubset)))>0) lci.thisrun<-data.withsubset[which(subset.all==sub.thisrun),lower] else lci.thisrun<-data.withsubset[which(subset.all==sub.thisrun),"lci"]
      if(length(grep(upper,colnames(data.withsubset)))>0) uci.thisrun<-data.withsubset[which(subset.all==sub.thisrun),upper] else uci.thisrun<-data.withsubset[which(subset.all==sub.thisrun),"uci"]
      
      xvec <- c(x.thisrun, tail(x.thisrun, 1), rev(x.thisrun), x.thisrun[1])
      yvec <- c(lci.thisrun, tail(uci.thisrun, 1), rev(uci.thisrun), lci.thisrun[1])
      polygon(xvec, yvec, col=colour[i], border=NA)
      
      # get() was not working for hexadecimal colours, even thought they are in quotes. Might need to change this back for other data sets:
      # polygon(xvec, yvec, col=get(colour[i]), border=NA)
      
    } # close for sub levels i
    
  } # close if subset present
  
} # close polygon function
