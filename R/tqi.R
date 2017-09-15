#' Track Quality Index
#'
#' Calculate TQI each
#' @pram None
#' @return TQI TABLE
#' @examples
#' 1. when setting the start and last point, distance critria is 200m
#' ex) 19000 <= x < 19400(o)
#' ex) 19050 <= x < 19075(X)
#'
#' 2.
#' - movingEverage : 50m
#'  startD_50 = start point -25m
#'  lastD_50 = last point + 24.75
#' ex) 19000~19400 -> 18975 ~ 19424.75
#'
#' - movingEverage : 200m
#'  startD_100 = startD - 100m
#'  lastD_100 = lastD + 99.75m
#' ex) 19000~19400 -> 18900 ~ 19499.75
#'
#' 3. setting Directory which has raw data of inspect result
devtools::use_package("dplyr")
devtools::use_package("csvread")
devtools::use_package("stringr")

#' @importFrom csvread map.coltypes
#' @importFrom csvread csvread
#' @importFrom compiler cmpfun
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @export


tqi=function(){
  A=cmpfun(function(){
    if(!require(dplyr)){
      library(dplyr)
    }
    if(!require(csvread)){
      library(csvread)
    }
    if(!require(stringr)){
      library(sringr)
    }

    fileList=list.files()
    print(fileList)

    startF=readline(prompt="몇번째 파일? :")
    startF=as.numeric(startF)
    lastF=startF #file parameter
    lastF=as.numeric(lastF)

    fileList=fileList[startF:lastF]

    print("1. m단위 입력")
    print("2. 범위를 200m로 구분되게 설정 ex)19000~19400")

    startD=readline(prompt="start: ")
    startD=as.numeric(startD)
    startD <<- startD
    lastD=readline(prompt="last: ")
    lastD=as.numeric(lastD)
    lastD<<-lastD

    startD_50=startD-25
    startD_50=as.numeric(startD_50)
    startD_50<<-startD_50

    lastD_50=lastD+24.75
    lastD_50=as.numeric(lastD_50)
    lastD_50<<-lastD_50


    startD_100=startD-100
    startD_100=as.numeric(startD_100)
    startD_100<<-startD_100

    lastD_100=lastD+99.75
    lastD_100=as.numeric(lastD_100)
    lastD_100<<-lastD_100

    range_no=(startD-1000)+0.25*(c(1:(4*lastD-4*startD+8001))-1)
    range_no<<-range_no

    range_TQI=(startD)+200*(c(1:((lastD-startD+200)/200))-1)
    range_TQI<<-range_TQI

    distance_temp=data.frame("distance"=range_no)
    i=1
    coltypes=map.coltypes(fileList[i],header=T)
    temp=csvread(fileList[i],coltypes=coltypes, header=T)
    names(temp)[1]="distance"

    print(names(temp))

    kind=readline(prompt="kind: ")
    kind=as.integer(kind)
    kind<<-kind

    if(kind==4|kind==5|kind==8|kind==9){
      longLevel=200
    }else if(kind==6|kind==7){
      longLevel=50
    }else{
      longLevel=0
    }

    longLevel<<-longLevel

    temp=left_join(distance_temp, temp[,c(1,kind)][!duplicated(temp[,c(1,kind)][,1]),],"distance")


    if(longLevel==50){
      temp=temp %>% filter(distance>=startD_50,distance<=lastD_50)
      if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0

      movingInclude=rep(0,100)
      i=101;for(i in 101:(length(temp[,1])-100)){
        movingInclude[i]=temp[i,2]-mean(temp[c((i-100):(i-1),i,(i+1):(i+100)),2])
        print(paste0(i,"/",length(temp[,1])-100))
      }#for(i)
      movingInclude=c(movingInclude,rep(0,100))
      temp=mutate(temp,movingInclude=movingInclude)

    }else if(longLevel==200){
      temp=temp %>% filter(distance>=startD_100,distance<=lastD_100)
      if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0
      movingInclude=rep(0,400)
      i=401;for(i in 401:(length(temp[,1])-400)){
        movingInclude[i]=temp[i,2]-mean(temp[c((i-400):(i-1),i,(i+1):(i+400)),2])
        print(paste0(i,"/",length(temp[,1])-400))
      }#for(i)
      movingInclude=c(movingInclude,rep(0,400))
      temp=mutate(temp,movingInclude=movingInclude)

    }else if(longLevel==0){
      temp=mutate(temp,movingInclude=temp[,2])
    }#if

    TQI=integer(0)
    i=1;for(i in 1:(length(range_TQI)-1)){

      TQI[i]= sd((temp %>% select(1,3) %>% filter(distance>=range_TQI[i],distance<range_TQI[i+1]))[,2])
      print(paste0(i,"/",(length(range_TQI)-1)))

    }

    original_TQI=mean(TQI)
    original_TQI <<- original_TQI

    temp=temp %>% mutate(rawTQI=rep(0,length(temp[,1])))

    table_TQI=data.frame(start=range_TQI[-length(range_TQI)],TQI=TQI)
    i=1;for(i in 1:length(table_TQI[,1])){

      TrueOrNot=((temp[,1]-table_TQI[i,1])>0)&((temp[,1]-table_TQI[i,1])<200)
      temp[TrueOrNot,4]=table_TQI[i,2]

      print(paste0(i,"/",length(table_TQI[,1])))
    }

    temp<<-temp

  }
  )
  A()
}

