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
#'
#' @export


tqiDBAggre=function(year,quater,plant_no,workspace_no){
  A=cmpfun(function(){

    year=as.integer(year)

    drv=JDBC("oracle.jdbc.driver.OracleDriver","/home/jsh/Downloads/ojdbc6.jar")
    conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")
    rs=dbSendQuery(conn,paste0("select startd, lastd from PLANTLOCATION where plant='",plant_no,"' and workspace='",workspace_no,"'"))
    d=dbFetch(rs)
    startD=as.integer(d[1,1])
    lastD=as.integer(d[1,2])

    rs=dbSendQuery(conn,paste0("select * from inspect",year))
    temp=dbFetch(rs)
    temp=temp[-1,]
    names(temp)[1]="distance"
    i=1;for(i in 1:length(temp)){
      temp[,i]=str_replace_all(temp[,i],"(\")","")
      temp[,i]=as.numeric(temp[,i])
    }
    temp_backup=temp

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

    ################33
    kind_v=c(1,1,3,4,5,6,7,8,9)
    count=3;for(count in 3:9){
      if(count!=3) temp_aggre=temp;

      temp=temp_backup
      kind=kind_v[count]

      if(kind==4|kind==5){
        longLevel=200
      }else if(kind==6|kind==7|kind==8){
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
          print(paste0("count=",count,"/",9," ",i,"/",length(temp[,1])-100))
        }#for(i)
        movingInclude=c(movingInclude,rep(0,100))
        temp=mutate(temp,movingInclude=movingInclude)

      }else if(longLevel==200){
        temp=temp %>% filter(distance>=startD_100,distance<=lastD_100)
        if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0
        movingInclude=rep(0,400)
        i=401;for(i in 401:(length(temp[,1])-400)){
          movingInclude[i]=temp[i,2]-mean(temp[c((i-400):(i-1),i,(i+1):(i+400)),2])
          print(paste0("count=",count,"/",9," ",i,"/",length(temp[,1])-400))
        }#for(i)
        movingInclude=c(movingInclude,rep(0,400))
        temp=mutate(temp,movingInclude=movingInclude)

      }else if(longLevel==0){
        temp=mutate(temp,movingInclude=temp[,2])
      }#if

      TQI=integer(0)
      i=1;for(i in 1:(length(range_TQI)-1)){

        TQI[i]= sd((temp %>% select(1,3) %>% filter(distance>=range_TQI[i],distance<range_TQI[i+1]))[,2])
        print(paste0("count=",count,"/",9," ",i,"/",(length(range_TQI)-1)))

      }

      original_TQI=mean(TQI)
      original_TQI <<- original_TQI

      temp=temp %>% mutate(rawTQI=rep(0,length(temp[,1])))

      table_TQI=data.frame(start=range_TQI[-length(range_TQI)],TQI=TQI)
      i=1;for(i in 1:length(table_TQI[,1])){

        TrueOrNot=((temp[,1]-table_TQI[i,1])>0)&((temp[,1]-table_TQI[i,1])<200)
        temp[TrueOrNot,4]=table_TQI[i,2]

        print(paste0("count=",count,"/",9," ",i,"/",length(table_TQI[,1])))
      }

      if(count!=3) temp=left_join(temp_aggre,temp[!duplicated(temp[,1]),],"distance")
      print(paste0(count,"/",9))
    }#for(count)
    temp<<-temp
    names(temp)=c("distance","GAGE","GAGE_moving","GAGE_TQI",
                  "PRL","PRL_moving","PRL_TQI",
                  "PRR","PRR_moving","PRR_TQI",
                  "ALL_","ALL_moving","ALL_TQI",
                  "ALR","ALR_moving","ALR_TQI",
                  "SUP","SUP_moving","SUP_TQI",
                  "TWIST","TWIST_moving","TWIST_TQI")
    try(rs<-dbExecute(conn,paste0("drop table TQI",year,"_",quater,"_",workspace_no)),
        silent=T)
    dbHasCompleted(rs)

    dbWriteTable(conn,paste0("TQI",year,"_",quater,"_",workspace_no),temp)
    dbDisconnect(conn)
  }
  )
  A()
}
