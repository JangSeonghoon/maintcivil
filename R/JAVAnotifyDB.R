#' tqi and notify in JAVA WEB
#'
#' return caution table for Showing in JAVA WEB
#' @param None
#' @return caution Tb(show spots to fix and working priorty)
devtools::use_package("magrittr")
devtools::use_package("dplyr")
devtools::use_package("csvread")
devtools::use_package("stringr")
devtools::use_package("rJava")
devtools::use_package("DBI")
devtools::use_package("RJDBC")

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
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @importFrom magrittr %>%
#' @export
JAVAnotify_db=function(startD,lastD,kind,equipOrNot,criteria_distance){
  A=cmpfun(
    function(){


      ######################
      #1. 데이터베이스 연결#
      ######################

      drv=JDBC("oracle.jdbc.driver.OracleDriver","/home/jsh/Downloads/ojdbc6.jar")
      conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")

      print(c("PRR","PRL","ALL","ALR","GAGE","TWIST","SUP"))
      # kind=readline(prompt="kind: ")

      ##################
      #2. @Param(kind) #
      ##################
      kind<<-kind


      ################
      #3. DB extract #
      ################
      if(kind=="PRL"){
        rs=dbSendQuery(conn,"select * from PRL")
        temp=dbFetch(rs)
      }else if(kind=="PRR"){
        rs=dbSendQuery(conn,"select * from PRR")
        temp=dbFetch(rs)
      }else if(kind=="ALL"){
        rs=dbSendQuery(conn,"select * from ALL")
        temp=dbFetch(rs)
      }else if(kind=="ALR"){
        rs=dbSendQuery(conn,"select * from ALR")
        temp=dbFetch(rs)
      }else if(kind=="GAGE"){
        rs=dbSendQuery(conn,"select * from GAGE")
        temp=dbFetch(rs)
      }else if(kind=="TWIST"){
        rs=dbSendQuery(conn,"select * from TWIST")
        temp=dbFetch(rs)
      }else if(kind=="SUP"){
        rs=dbSendQuery(conn,"select * from SUP")
        temp=dbFetch(rs)
      }
      names(temp)[1]="distance"
      i=1;for(i in 1:length(temp)){
        temp[,i]=str_replace_all(temp[,i],"(\")","")
        temp[,i]=as.numeric(temp[,i])
      }


      # print("1. m단위 입력")
      # print("2. 범위를 200m로 구분되게 설정 ex)19000~19400")
      #
      # startD=readline(prompt="start: ")
      # startD=as.numeric(startD)
      # startD <<- startD
      # lastD=readline(prompt="last: ")
      # lastD=as.numeric(lastD)
      # lastD<<-lastD



      ############################################
      #4. 계산을 위한 시작점/종료점 지정(50/100) #
      ############################################
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

      ############################################
      #5. range_no(25cm) / range_TQI(200m)       #
      ############################################
      range_no=(startD-1000)+0.25*(c(1:(4*lastD-4*startD+8001))-1)
      range_no<<-range_no

      range_TQI=(startD)+200*(c(1:((lastD-startD+200)/200))-1)
      range_TQI<<-range_TQI

      #####################################################################
      #6. distance_temp : UniqueKey값 : range_no(merge 할 때 없는 값들 NA)#
      #####################################################################
      distance_temp=data.frame("distance"=range_no)
      # i=1
      # coltypes=map.coltypes(fileList[i],header=T)
      # temp=csvread(fileList[i],coltypes=coltypes, header=T)

      ####################
      #7. 이동평균선 지정#
      ####################
      if(kind=="PRL"|kind=="PRR"|kind=="TWIST"|kind=="SUP"){
        longLevel=200
      }else if(kind=="ALL"|kind=="ALR"){
        longLevel=50
      }else{
        longLevel=0
      }

      longLevel<<-longLevel

      ####################################
      #8. distance_temp에 distance 끼우기#
      ####################################
      #temporary_should be revised
      quater=length(temp)
      temp=left_join(distance_temp, temp[,c(1,quater)][!duplicated(temp[,c(1,quater)][,1]),],"distance")


      ############################
      #9. (궤도틀림값-이동평균선)#
      ############################
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

      ################
      #10. TQI 구하기#
      ################
      TQI=integer(0)
      i=1;for(i in 1:(length(range_TQI)-1)){
        TQI[i]= sd((temp %>% select(1,3) %>% filter(distance>=range_TQI[i],distance<range_TQI[i+1]))[,2])
        print(paste0(i,"/",(length(range_TQI)-1)))
      }

      original_TQI=mean(TQI)
      original_TQI <<- original_TQI

      ########################
      #11. TQI(200m -> 0.25m)#
      ########################
      temp=temp %>% mutate(rawTQI=rep(0,length(temp[,1])))
      table_TQI=data.frame(start=range_TQI[-length(range_TQI)],TQI=TQI)
      i=1;for(i in 1:length(table_TQI[,1])){
        TrueOrNot=((temp[,1]-table_TQI[i,1])>0)&((temp[,1]-table_TQI[i,1])<200)
        temp[TrueOrNot,4]=table_TQI[i,2]

        print(paste0(i,"/",length(table_TQI[,1])))
      }
      temp<<-temp

      print("input TQI_file to directory of input")
      # directoryCheck=readline(prompt="directory: /home/jsh/Rwork/input right?(1.yes, 2.no)")

      directoryCheck=1
      directoryCheck=as.integer(directoryCheck)

      if(directoryCheck==1){
        setwd("/home/jsh/Rwork/input")
      }else if(directoryCheck==2){
        directory=readline(prompt="write your Directory:(without double quotation)")
        directory=as.character(directory)
        setwd(directory)
      }

      fileCheck=2
      # fileCheck=readline(prompt=paste0("1. I have seperated TQI file,though","\n","2. I've already executed tqi() function"))
      fileCheck=as.numeric(fileCheck)

      if(fileCheck==1){
        file_tqi=list.files()
        print(list.files())
        fileNo=readline(prompt="What number is your fileNo?")
        fileNo=as.integer(fileNo)
        coltypes=map.coltypes(file_tqi[fileNo],header=T)
        temp=csvread(file_tqi[fileNo],coltypes=coltypes,header=T)

        print("3.GAGE 4.PRR 6.ALL 8.TWIST 9.SUP")
        kind=readline(prompt="kind: ")
        kind=as.integer(kind)
        kind<<-kind
      }

      # equipOrNot=readline(prompt="1.equip 2.manual : ")
      # equipOrNot=as.integer(equipOrNot)

      ##################################################################
      #12. 장비/인력별 / 틀림별 목표기준,주의기준,보수기준,속도제한기준#
      ##################################################################
      if(equipOrNot==1){
        if(kind==3){
          criteria=0
          criteria_caution=0
          criteria_limit=0
        }else if(kind==4|kind==5){
          criteria=8
          criteria_caution=4
          criteria_limit=20
        }else if(kind==6|kind==7){
          criteria=7
          criteria_caution=4
          criteria_limit=17
        }else if(kind==8){
          criteria=8
          criteria_caution=3
          criteria_limit=21
        }else{
          criteria=10
          criteria_caution=3
          criteria_limit=30
        }

      }else{
        if(kind==3){
          criteria=0
          criteria_caution=0
          criteria_limit=0
        }else if(kind==4|kind==5){
          criteria=13
          criteria_caution=4
          criteria_limit=20
        }else if(kind==6|kind==7){
          criteria=9
          criteria_caution=4
          criteria_limit=17
        }else if(kind==8){
          criteria=10
          criteria_caution=3
          criteria_limit=21
        }else{
          criteria=20
          criteria_caution=3
          criteria_limit=30
        }
      }
      criteria=as.integer(criteria)
      criteria_caution=as.numeric(criteria_caution)
      criteria<<-criteria
      criteria_caution<<-criteria_caution

      # criteria_distance=readline(prompt="criteria of distance(m) : ")

      ###################################
      #13. 지적개소 및 속도제한개소 산출#
      ###################################
      alert=temp %>% mutate(speedLimit=ifelse(abs(temp$movingInclude)>=criteria_limit,1,0))
      alert=alert %>% select(1,3,5) %>% filter(abs(movingInclude)>=criteria) %>% arrange(distance)

      ###################
      #14. 지적개소 군집#
      ###################
      if(length(alert[,1])!=0){
      alert_range=alert %>% mutate(range=ifelse((floor(alert[,1])-c(0,alert[1:(length(alert[,1])-1),1]))<=criteria_distance,1,0))

      start=which(alert_range$range==0)
      last=c((which(alert_range$range==0)[-1]-1),length(alert[,1]))

      ##########################################
      #15. 군집내 지적개소 갯수 및 속도제한갯수#
      ##########################################
      i=1;column=integer(0);for(i in 1:length(start)){
        column[start[i]:last[i]]=i
      }
      alert=bind_cols(alert,data.frame("column"=column))
      alert<<-alert
      caution=data.frame(start=alert_range[start,1],
                         last=alert_range[last,1],
                         count=
                           (as.data.frame(unique((alert %>% group_by(column) %>% mutate(NROW(column)))[,c(4,5)])))[,2],
                         speedLimit=(as.data.frame((alert %>% group_by(column) %>% summarise(sum(speedLimit)))[2]))[,1]
      )

      ########################
      #16. 군집별 틀림값 변경#
      ########################
      z=1;for(z in 1:length(caution[,1])){

        if(z==1) {
          adjustTQI=integer(0);improveTQI=integer(0);indexTQI=integer(0);indexRaw=integer(0)
          backup=temp[,2]
        }

        rownumber=(alert %>% filter(column==z))[,1]
        rownames=(temp %>% mutate(rownames=seq(temp[,1])) %>% select(1,rownames) %>% filter(distance %in% rownumber ) )[,2]
        temp[rownames,2]=ifelse(temp[rownames,2]>0,criteria_caution,criteria_caution*(-1))

        #criteria_duplicate : asdf
        if(longLevel==50){
          criteria_duplicate=100
        }else if(longLevel==200){
          criteria_duplicate=400
        }else if(longLevel==0){
        }
        criteria_duplicate<<-criteria_duplicate

        #############################
        #17. 군집별 이동평균선 변경#
        #############################
# 이동평균선 보정
#
# 궤도틀림값 변경 -> 이동평균선 변경 	-> TQI변경
# <값 자체>	  <궤도틀림 기준 앞뒤 고려>
#
# 한 개 틀림값 보정이면 이동평균선도 바뀌기 때문에
# 그 값 이전 100/200 이후 100/200 이동평균선을 바꿔줘야 한다.
# dup -> 100인 이유는 50/200 이동평균선 최소값이 100이어서 그렇다.

        dup=1;for(dup in 1:100){

          if(dup!=1) duplicate_backup=D
          A=ifelse((rownames+dup)>criteria_duplicate&(rownames+dup)<length(temp[,1])-(criteria_duplicate-1),rownames+dup,0)
          B=ifelse((rownames-dup)>criteria_duplicate&(rownames-dup)<length(temp[,1])-(criteria_duplicate-1),rownames-dup,0)
          AB=c(A,B)
          if(dup==1)D=AB
          if(dup!=1) D=c(duplicate_backup,AB)
          print(paste0(dup,"/",100))
        }

        D=sort(unique(D))[-1]

        ########################################
        #18. 이동평균선 부분 변경 및 TQI 재산출#
        ########################################
        if(longLevel==50){
          temp=temp %>% filter(distance>=startD_50,distance<=lastD_50)
          movingInclude_revise=temp[,3]
          if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0

          i=1;for(i in 1:length(D)){
            movingInclude_revise[D[i]]=temp[D[i],2]-mean(temp[c((D[i]-100):(D[i]-1),D[i],(D[i]+1):(D[i]+100)),2])
            print(paste0("z=",z," ",i,"/",length(D)))
          }

        }else if(longLevel==200){
          temp=temp %>% filter(distance>=startD_100,distance<=lastD_100)
          movingInclude_revise=temp[,3]
          if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0

          i=1;for(i in 1:length(D)){
            movingInclude_revise[D[i]]=temp[D[i],2]-mean(temp[c((D[i]-400):(D[i]-1),D[i],(D[i]+1):(D[i]+400)),2])
            print(paste0("z=",z," ",i,"/",length(D)))
          }
        }else if(longLevel==0){
          movingInclude_revise=temp[,2]
        }

        TQI_fix=integer(0)
        i=1;for(i in 1:(length(range_TQI)-1)){

          TQI_fix[i]= sd((temp %>% mutate(adjust=movingInclude_revise) %>% select(1,adjust) %>% filter(distance>=range_TQI[i],distance<range_TQI[i+1]))[,2])
          print(paste0(i,"/",(length(range_TQI)-1)))
        }

        #################################
        #19. TQI 개선율 및 우선순위 산출#
        #################################
        adjustTQI[z]=mean(TQI_fix)
        improveTQI[z]=original_TQI-mean(TQI_fix)
        indexRaw[z]=(improveTQI[z]/original_TQI)*100
        indexTQI[z]=paste0( format( round((improveTQI[z]/original_TQI)*100,2),nsmall=2) , "%")

        if(z==length(caution[,1])) {
          caution=caution[,c(1:4)]

          caution=bind_cols(caution,adjustTQI=adjustTQI,original_TQI=rep(original_TQI,length(caution[,1])),improveTQI=improveTQI,indexTQI=indexTQI)
          if(length( (caution %>% filter(speedLimit!=0))[,1]  )!=0){
            caution_speed<-caution %>% filter(speedLimit!=0)
            caution_speed<-caution_speed %>% mutate(priority=rank(desc(caution_speed$indexRaw)))

            caution_normal<-caution %>% filter(speedLimit==0)
            caution_normal<-caution_normal %>% mutate(priority=length(caution_speed[,1])+rank(desc(caution_normal$indexRaw)))

            caution=bind_rows(caution_speed,caution_normal)
          }else{
            caution<-caution %>% filter(speedLimit==0) %>% mutate(priority=rank(desc(indexRaw)))

          }
          caution=arrange(caution, priority)
        }#fi(z)
        temp[,2]=backup
        temp<<-temp
      }#for(z)
      caution_priority<<-caution
      caution<<-caution
      rownumber<<-rownumber
      backup=temp[,2]
      backup<<-backup

      ########################################
      #20. ORACLE DB TEMPORARY TABLE RENEWAL #
      ########################################
      try(rs<-dbExecute(conn,"drop table temporary"),
          silent=T)
      dbHasCompleted(rs)
      execCaution=caution
      names(execCaution)=NULL
      dbWriteTable(conn,"temporary",execCaution[,c(1,2,3,4,8,9)],c("startD","lastD","count","speedlimit","indexTQI","priority"))
      dbDisconnect(conn)

      }else{
        caution=data.frame(start=c(0),last=c(0),count=c(0),speedLimit=c(0),adjustTQI=c(0),original_TQI=c(0),improveTQI=c(0),indexTQI=c(0),priority=c(0))
        try(rs<-dbExecute(conn,"drop table temporary"),
            silent=T)
        dbHasCompleted(rs)

        execCaution=caution
        names(execCaution)=NULL
        dbWriteTable(conn,"temporary",execCaution[,c(1,2,3,4,8,9)],c("startD","lastD","count","speedlimit","indexTQI","priority"))
        dbDisconnect(conn)
      }
      return(caution)
    }#function
  )#cmpfun
  A()

}#function


