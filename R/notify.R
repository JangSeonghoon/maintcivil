#' notify should fix
#'
#' For inform the spot of rail to fix
#' @param None
#' @return caution Tb(show spots to fix and working priorty)
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
notify=function(){
  A=cmpfun(

    function(){
      if(!require(dplyr)){
        library(dplyr)
      }
      if(!require(csvread)){
        library(csvread)
      }
      if(!require(stringr)){
        library(sringr)
      }
  
  print("input TQI_file to directory of input")
  directoryCheck=readline(prompt="directory: /home/jsh/Rwork/input right?(1.yes, 2.no)")
  directoryCheck=as.integer(directoryCheck)
  if(directoryCheck==1){
    setwd("/home/jsh/Rwork/input")
  }else if(directoryCheck==2){
    directory=readline(prompt="write your Directory:(without double quotation)")
    directory=as.character(directory)
    setwd(directory)
  }
  
  fileCheck=readline(prompt=paste0("1. I have seperated TQI file,though","\n","2. I've already executed tqi() function"))
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
  
  equipOrNot=readline(prompt="1.equip 2.manual : ")
  equipOrNot=as.integer(equipOrNot)
  
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


  criteria_distance=readline(prompt="criteria of distance(m) : ")


  alert=temp %>% mutate(speedLimit=ifelse(abs(temp$movingInclude)>=criteria_limit,1,0))
  alert=alert %>% select(1,3,5) %>% filter(abs(movingInclude)>=criteria) %>% arrange(distance)
  alert_range=alert %>% mutate(range=ifelse(floor(alert[,1])-c(0,alert[1:(length(alert[,1])-1),1])<=criteria_distance,1,0))

  start=which(alert_range$range==0)
  last=c((which(alert_range$range==0)[-1]-1),length(alert[,1]))

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

  z=1;for(z in 1:length(caution[,1])){

    if(z==1) {
      adjustTQI=integer(0);improveTQI=integer(0);indexTQI=integer(0);indexRaw=integer(0)
      backup=temp[,2]
    }

    rownumber=(alert %>% filter(column==z))[,1]
    rownames=(temp %>% mutate(rownames=seq(temp[,1])) %>% select(1,rownames) %>% filter(distance %in% rownumber ) )[,2]
    temp[rownames,2]=ifelse(temp[rownames,2]>0,criteria_caution,criteria_caution*(-1))

      if(longLevel==50){
      criteria_duplicate=100
    }else if(longLevel==200){
      criteria_duplicate=400
    }else if(longLevel==0){
    }
    criteria_duplicate<<-criteria_duplicate

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
      print(View(caution))
    }
    temp[,2]=backup
    temp<<-temp
  }
  caution_priority<<-caution
  caution<<-caution
  rownumber<<-rownumber
  backup=temp[,2]
  backup<<-backup
    }
  )
A()
  }


