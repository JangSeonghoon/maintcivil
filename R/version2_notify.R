#' notify_ver2 in JAVA WEB
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
notify_ver2=function(year,quater,workspace_no,carKind){
  A=cmpfun(
    function(){

      drv=JDBC("oracle.jdbc.driver.OracleDriver","/home/jsh/Downloads/ojdbc6.jar")
      conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")

      quater=ifelse(length(quater)==1,paste0("0",quater),quater)
      year=as.integer(year)

      rs=dbSendQuery(conn,paste0("select * from inspectRS",year,quater, " where workspace='",workspace_no,"'"))
      temp=dbFetch(rs)

      j=1;for(j in 1:length(temp)){
        temp[,j]=str_replace_all(temp[,j],"(\")","")
      }
      car=ifelse(carkind==1,"궤도검측차","종합검측차")
      temp=temp[temp$CARKIND==car,]
      i=c(2,3,4,5,6,10,11)
      k=1;for(k in 1:length(i)){

        temp[,i[k]]=as.numeric(temp[,i[k]])
        print(k)
      }
      temp[,8]=str_replace_all(temp[,8],"[.]","/")
      temp[,8]=as.Date(temp[,8])

      seq=integer(0)
      deadline=temp[,8]

      i=1;for(i in 1:length(temp[,1])){
        if(str_detect(temp[i,1],"TWIST")){
          seq[i]=1
          deadline[i]=deadline[i]+30
        }else if(str_detect(temp[i,1],"ALIGNMENT")){
          seq[i]=2
          deadline[i]=deadline[i]+60
        }else{
          seq[i]=3
          deadline[i]=deadline[i]+90
        }
      }
      remnant=deadline-Sys.Date()
      remnant_alert=ifelse(remnant<0,paste0(remnant*(-1),"일 지남"),
                     ifelse(remnant==0,"D-DAY",paste0(remnant,"일 남음")))
      remnant=as.character(remnant)
      len=length(temp)
      temp=temp %>% mutate(deadline,remnant,remnant_alert,seq) %>% arrange(seq,STARTD) %>% select(1:(len+3))

      caution=temp %>% select(8,12,1,4,5,6,3,2,9,14,13)
      caution=cbind(seq=seq(caution[,3]),caution)

      ########################################
      #13. ORACLE DB TEMPORARY TABLE RENEWAL #
      ########################################

      if(length(caution[,1])!=0){
        try(rs<-dbExecute(conn,"drop table temporary"),
            silent=T)
        dbHasCompleted(rs)
        execCaution=caution
        names(execCaution)=NULL
        dbWriteTable(conn,"temporary",execCaution)
        dbWriteTable
        dbDisconnect(conn)

      }else{
        caution=data.frame(start=c(0),last=c(0),count=c(0),speedLimit=c(0),adjustTQI=c(0),original_TQI=c(0),improveTQI=c(0),indexTQI=c(0),priority=c(0))
        try(rs<-dbExecute(conn,"drop table temporary"),
            silent=T)
        dbHasCompleted(rs)
        names(execCaution)=NULL
        execCaution=caution
        dbWriteTable(conn,"temporary",execCaution)
        dbDisconnect(conn)
      }
      return(caution)
    }#function
  )#cmpfun
  A()

}#function


