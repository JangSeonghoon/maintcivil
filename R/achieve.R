#' Achievement
#'
#' For checking the effect of work done afterward
#' @param None
#' @return caution Tb(show spots to fix, working priorty,check column)
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
achievement=function(){



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

  click_count=readline(prompt="최초 생성입니까? (1.yes 2.no) :")
  click_count=as.integer(click_count)
  if (click_count==1){
    caution=caution[,1:9]
    caution=bind_cols(caution,"doOrNot"=data.frame(rep(0,length(caution[,1]))))
  }

  print(View(caution_priority))
  fix_finished=readline(prompt="작업완료 통지 번호(다수일 시 ','로 구분):")
  fix_finished=as.numeric(unlist(str_split(fix_finished,",")))
  caution[which(caution[,9] %in% fix_finished),10]=1
  caution<<-caution

  i=1;rownames1=integer(0);for(i in 1:length(fix_finished)){
    z=fix_finished[i]
    rownumber=(alert %>% filter(column==alert[alert$distance==caution[which(caution[,9]==z),1],4]))[,1]
    if(i!=1) rownames2=rownames1
    rownames=(temp %>% mutate(rownames=seq(temp[,1])) %>% select(1,rownames) %>% filter(distance %in% rownumber ) )[,2]
    rownames1=rownames
    if(i!=1) rownames1=c(rownames2,rownames1)
  }
  print(temp[rownames1,2])
  temp[rownames,2]=ifelse(temp[rownames,2]>0,criteria_caution,criteria_caution*(-1))
  print(temp[rownames1,2])
  temp1=temp
  temp<<-temp1

  if(longLevel==50){
    criteria_duplicate=100
  }else if(longLevel==200){
    criteria_duplicate=400
  }else if(longLevel==0){
  }
  criteria_duplicate<<-criteria_duplicate
  dup=1;for(dup in 1:100){

    if(dup!=1) duplicate_backup=D
    A=ifelse((rownames1+dup)>criteria_duplicate&(rownames1+dup)<length(temp1[,1])-(criteria_duplicate-1),rownames1+dup,0)
    B=ifelse((rownames1-dup)>criteria_duplicate&(rownames1-dup)<length(temp1[,1])-(criteria_duplicate-1),rownames1-dup,0)
    AB=c(A,B)
    if(dup==1)D=AB
    if(dup!=1) D=c(duplicate_backup,AB)
  }

  D=sort(unique(D))[-1]

  if(longLevel==50){
    temp1=temp1 %>% filter(distance>=startD_50,distance<=lastD_50)
    movingInclude_revise=temp1[,3]
    if(is.na(sum(temp1[,2]))) temp1[is.na(temp1[,2]),2]=0

    i=1;for(i in 1:length(D)){
      movingInclude_revise[D[i]]=temp1[D[i],2]-mean(temp1[c((D[i]-100):(D[i]-1),D[i],(D[i]+1):(D[i]+100)),2])
    }

  }else if(longLevel==200){
    temp1=temp1 %>% filter(distance>=startD_100,distance<=lastD_100)
    movingInclude_revise=temp1[,3]
    if(is.na(sum(temp1[,2]))) temp1[is.na(temp1[,2]),2]=0

    i=1;for(i in 1:length(D)){
      movingInclude_revise[D[i]]=temp1[D[i],2]-mean(temp1[c((D[i]-400):(D[i]-1),D[i],(D[i]+1):(D[i]+400)),2])
    }
  }else if(longLevel==0){
    movingInclude_revise=temp1[,2]
  }

  TQI_fix=integer(0)
  i=1;for(i in 1:(length(range_TQI)-1)){

    TQI_fix[i]= sd((temp1 %>% mutate(adjust=movingInclude_revise) %>% select(1,adjust) %>% filter(distance>=range_TQI[i],distance<range_TQI[i+1]))[,2])
  }

  adjustTQI=mean(TQI_fix)
  improveTQI=original_TQI-mean(TQI_fix)
  indexRaw=(improveTQI/original_TQI)*100
  indexTQI=paste0( format( round((improveTQI/original_TQI)*100,2),nsmall=2) , "%")

  print("==============================================================================================")
  print(paste0("개선된 TQI(기존 TQI) : ",round(adjustTQI,4)," (",round(original_TQI,4),")"))
  print(paste0("개선분 : ",round(improveTQI,4), " 개선률 :",indexTQI))
  print("==============================================================================================")
  print(View(caution))

  default=readline(prompt="새로고침 하시겠습니까?(1.YES 2.NO(계속 입력))") #초기화 parameter
  if(default==1) temp[,2]=backup

  caution<<-caution
  rownumber<<-rownumber
  click_count_backup<<-click_count
  temp<<-temp
 }
)
  A()

}
