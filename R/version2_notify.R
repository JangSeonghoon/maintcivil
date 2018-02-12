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
devtools::use_package("plotly")
devtools::use_package("htmlwidgets")
devtools::use_package("tidyr")
devtools::use_package("compiler")

#' @importFrom compiler cmpfun
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr full_join
#' @importFrom csvread map.coltypes
#' @importFrom csvread csvread
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @importFrom RJDBC JDBC
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom htmlwidgets saveWidget
#' @importFrom tidyr spread
#' @export
#'
notify_ver=function(year,quater,workspace_no,carKind,direct){
  A=cmpfun(
    function(){

      if(Sys.info()['sysname']=="Windows"){
        path=
          paste0(
            Sys.getenv("CATALINA_HOME"),"/webapps/bigTeam/"
          )
      }else if(Sys.info()['sysname']=="Linux"){
        path="/home/jsh/eclipse-workspace/bigTeam/src/main/webapp/"
      }

      drv=JDBC("oracle.jdbc.driver.OracleDriver",paste0(path,"driver/ojdbc6.jar"))
      conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")

      quater=ifelse(length(quater)==1,paste0("0",quater),quater)
      year=as.integer(year)
      direct=as.integer(direct)
      direction=c("MM101","MM102","MM103","MM201","MM202","MM203","MM")

      try(rs<-dbSendQuery(conn,paste0("select * from inspectRS",year,quater,"_",workspace_no)),
          silent=T
      )


      temp=dbFetch(rs)


      if(direct!=7){
        temp=temp %>%
        filter(DIRECTION==direction[direct])
      }

      if(length(temp[,1])!=0){

        j=1;for(j in 1:length(temp)){
          temp[,j]=str_replace_all(temp[,j],"(\")","")
        }

        car=ifelse(carKind==1,"궤도검측차","종합검측차")
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
        remnant_seq=as.numeric(remnant)
        remnant=as.character(remnant)
        len=length(temp)
        temp=temp %>% mutate(deadline,remnant,remnant_alert,seq,remnant_seq) %>%
          arrange(remnant_seq) %>%
          select(1:(len+3))

        caution=temp %>% select(8,13,1,4,5,6,3,2,9,15,14,12)
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

        if(Sys.info()['sysname']=="Windows"){
          path=
            paste0(
              Sys.getenv("CATALINA_HOME"),"/webapps/bigTeam/html/"
            )

        }else if(Sys.info()['sysname']=="Linux"){
          path="/home/jsh/eclipse-workspace/bigTeam/src/main/webapp/html/"
        }


        if(
        list.files(paste0(path,"html/")) %>%
          str_detect(paste0(year,quater,workspace_no,direction[direct],carKind,"temp.html")) %>%
          sum()
        ==0){

          print("none")

        test=
          caution %>%
          mutate(PARAMETER=paste0(PARAMETER,"_",DIRECTION)) %>%
          select(9,4,8)

        start=min(caution$STARTD)-1
        last=max(caution$LASTD)+1
        n=(last-start)/0.001+1
        LOCATION=start+0.001*((1:n)-1)
        names(test)[1]="LOCATION"

        test=
          test %>%
          group_by(PARAMETER, LOCATION) %>%
          mutate(ind=row_number()) %>%
          spread("PARAMETER","MAX")

        test=as.data.frame(test)
        i=1;for(i in 1:length(test)){
          test[is.na(test[,i]),i]=0
          print(i)
        }

        LOC=data.frame(LOCATION=LOCATION)
        LOC[,1]=round(LOC[,1],digit=3)
        test[,1]=round(test[,1],digit=3)
        test=full_join(LOC,test,by="LOCATION")
        test=test[,-2]
        i=1;for(i in 1:length(test)){
          test[is.na(test[,i]),i]=0
          print(i)
        }


        graph_char=
          paste0("p <- plot_ly( x = ~ test$`LOCATION`, y = ~test$`",names(test)[2],"`, type = 'scatter', mode = 'lines', name = '",names(test)[2],"', fill = 'tozeroy', line = list(width = 0.5)) %>%"
          )

        i=3;for(i in 3:length(test)){
          graph_char=
            paste0(
              graph_char,
              paste0("add_trace(x = ~test$LOCATION, y = ~test$`",names(test)[i],"`, name = '",names(test)[i],"', fill = 'tozeroy') %>%")
            )
        }
        graph_char=
          paste0(
            graph_char,
            "layout(
            title = 'FIX',
            xaxis = list( rangeslider = list(type = 'LOCATION'),title=''),
            yaxis = list(title = 'INSPECT'))"
          )

        p=eval(parse(text=graph_char))

        saveWidget(
          p,
          paste0(path,"html/",year,quater,workspace_no,direction[direct],carKind,"temp.html")
        )

        }


      }else{
        try(rs<-dbExecute(conn,"drop table temporary"),
            silent=T)
        dbHasCompleted(rs)
      }
    }#function
  )#cmpfun
  A()

}#function


