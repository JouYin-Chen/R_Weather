# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

getWeather <- function(x) {
  library(RCurl)
  library(XML)
  url <- paste('http://weather.yahooapis.com/forecastrss?w=',x,'&=c',sep="")
  doc = xmlTreeParse(getURL(url),useInternal = TRUE)

  ans<- getNodeSet(doc, "//yweather:atmosphere")
  humidity<-as.numeric(sapply(ans, xmlGetAttr, "humidity"))
  visibility<-as.numeric(sapply(ans, xmlGetAttr, "visibility"))
  pressure<-as.numeric(sapply(ans, xmlGetAttr, "pressure"))
  rising<-as.numeric(sapply(ans, xmlGetAttr, "rising"))

  ans<-getNodeSet(doc, "//item/yweather:condition")
  code<-sapply(ans, xmlGetAttr, "code")

  ans<-getNodeSet(doc, "//item/yweather:forecast[1]")
  low<-as.numeric(sapply(ans, xmlGetAttr, "low"))
  high<-as.numeric(sapply(ans, xmlGetAttr, "high"))

  print(paste(x,'==>',low,high,code,humidity,visibility,pressure,rising))
  cbind(low,high,code,humidity,visibility,pressure,rising)
}

filename<-function(date=Sys.time()){
  paste(format(date,"%Y%m%d"),".csv",sep="")
}

loadDate<-function(date){
  print(paste('Date','==>',date))
  city<-read.csv(file="WOEID.csv",header=FALSE,fileEncoding="utf-8", encoding="utf-8")
  names(city)<-c("en","woeid","zh",'prov','long','lat')
  city<-city[-nrow(city),]

  wdata<-do.call(rbind,lapply(city$woeid, getWeather))
  w<-cbind(city,wdata)
  write.csv(w,file=filename(date),row.names = FALSE,fileEncoding = "utf-8")
}

getColors2<-function(map,prov,ctype){
  ADCODE99 <- read.csv(file="ADCODE99.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
  fc <- function(x){ADCODE99$ADCODE99[which(x==ADCODE99$prov)]}
  code <- sapply(prov, fc)

  f=function(x,y) ifelse(x %in% y, which(y==x),0);
  colIndex=sapply(map$ADCODE99, f,code);
  ctype[which(is.na(ctype))]=19
  return(ctype[colIndex])

}

summary<-function(data=data,output=FALSE,path=''){
  library(maps)
  library(mapdata)
  library(maptools)
  library(RColorBrewer)
  colors<-c(rev(brewer.pal(9,"Blues")),rev(c('#b80137','#8c0287','#d93c5d','#d98698','#f6b400','#c4c4a7','#d6d6cb','#d1b747','#ffeda0')))

  temp<- data$code
  title <- "天氣"
  ofile <- paste(format(date,"%Y%m%d"),"_code.png",sep="")
  sign <- ''
  colors<-rev(colors)
  code <- read.csv(file="code.csv",header = TRUE,fileEncoding = "utf-8",encoding = "utf-8")
  labelcode<-read.csv(file="lablecode.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
  ctype <- sapply(temp, function(x){code$type[which(x==code$code)]})

  if(output)png(file=paste(path,ofile,sep=''),width=600,height=600)
  layout(matrix(data=c(1,2),nrow=1,ncol=2),widths = c(8,1),heights = c(1,2))
  par(mar=c(0,0,3,12),oma=c(0.2,0.2,0.2,0.2),mex=0.3)
  plot(map,border="white",col=colors[getColors2(map,data$prov,ctype)])
  points(data$long,data$lat,pch=19,col=rgb(0,0,0,0.3),cex=0.8)

  if(FALSE){
    grid()
    axis(1,lwd=0);axis(2,lwd=0);axis(3,lwd=0);axis(4,lwd=0);

  }
  text(100,58,title,cex=2)
  text(105,54,format(date,"%Y-%m-%d"))
  text(98,65,paste('天氣','http://apps.weibo.com/chinaweatherapp'))

  for(row in 1:nrow(data)){
    name <- as.character(data$zh[row])
    label <- labelcode$alias[labelcode$type==ctype[row]]
    x1 <- ceiling(row/7)
    x2 <- ifelse(row%%7==0,7,row%%7)
    x3 <- ctype[row]
    fontCol <- '#000000'
    if(x3<=5) fontCol<-head(colors,1)
    if(x3>=12) fontCol <- tail(colors,1)
    text(68+x1*11,17-x2*3,paste(name,' ',label,sign,sep=''),col=fontCol)

  }

  par(mar=c(5,0,15,10))
  image(x=1,y=1:length(colors),z=t(matrix(1:length(colors))),col=rev(colors),axes=FALSE,xlab="",ylab="",xaxt="n")
  axis(4, at = 1:(nrow(labelcode)-1), labels=rev(labelcode$alias)[-1], col = "white", las = 1)
  abline(h=c(1:(nrow(labelcode)-2)+0.5), col = "white", lwd = 2, xpd = FALSE)
  if(output)dev.off()
}
weather_html<-function(data=data,type='high',output=FALSE,path=''){
    if(type=='high'){
      df<-data[,c('prov','high')]
      names(df)<-c("prov","氣溫")
      title<- paste(format(date,"%Y-%m-%d"),"白天天氣概況",sep="")
      ofile<- paste(format(date,"%Y%m%d"),"_day.html",sep="")
    }else if(type=='low'){
      df<-data[,c('prov','low')]
      names(df)<-c("prov","氣溫")
      title<- paste(format(date,"%Y-%m-%d"),"夜間天氣概況",sep="")
      ofile<- paste(format(date,"%Y%m%d"),"_night.html",sep="")
    }

    df[,1]<-substr(df[,1],0,2)
    df[which(df$prov=='黑龍'),]$prov<-'黑龍江'
    df[which(df$prov=='內蒙'),]$prov<-'內蒙古'

    recharts.eMap <- eMap(df, namevar=1, datavar = 2, title=title)
    if(output){
      recharts.eMap$outList[c('chartid','type')]<-NULL
      writeLines(unlist(recharts.eMap$outList),paste(path,ofile,sep=''))
    }else{
      plot(recharts.eMap)
    }
}
