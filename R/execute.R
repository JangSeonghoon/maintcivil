#'complete work
#'
#' @param year,quater,plant_no,workspace_no
#' @return None
devtools::use_package("rJava")
devtools::use_package("DBI")
devtools::use_package("RJDBC")
devtools::use_package("stringr")
#' @importFrom compiler cmpfun
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @importFrom stringr str_detect

#' @export
executeManual=function(num,year,quater,workspace_no,direct){
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

      direct=as.integer(direct)
      direction=c("MM101","MM102","MM103","MM201","MM202","MM203","MM")

      drv=JDBC("oracle.jdbc.driver.OracleDriver",paste0(path,"driver/ojdbc6.jar"))
      conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")
      quater=ifelse(length(quater)==1,paste0("0",quater),quater)
      # rs=dbSendQuery(conn,paste0("select * from INSPECTRS",year,quater,"_",workspace_no,"_BACKUP"))
      rs=dbSendQuery(conn,paste0("select * from INSPECTRS",year,quater,"_",workspace_no))
      result=dbFetch(rs)

      desc=dbSendQuery(conn,"select * from tab")
      desc=dbFetch(desc)

      temp<<-dbSendQuery(conn,"select * from temporary")
      temp<<-dbFetch(temp)

      compare=temp[num,9]

      if(
        ( desc[,1] %>%
        str_detect(paste0("INSPECTRS",year,quater,"_",workspace_no,"_BACKUP")) %>%
        sum() )==0
      ){
        dbWriteTable(conn,paste0("INSPECTRS",year,quater,"_",workspace_no,"_BACKUP"),result)
      }

      result$EXCEPT =
        result$EXCEPT %>%
        as.numeric()

      if(direct!=7){
        result=
          result %>%
          filter(!(EXCEPT %in% temp[num,9]&DIRECTION==direction[direct])) %>%
          apply(.,2,function(x) x=ifelse(is.na(x),"",x)) %>%
          as.data.frame()
      }else{
        result=
          result %>%
          filter(!(EXCEPT %in% temp[num,9]) ) %>%
          apply(.,2,function(x) x=ifelse(is.na(x),"",x)) %>%
          as.data.frame()
      }
      print(length(result$EXCEPT))


      try(rs<-dbExecute(conn,paste0("drop table INSPECTRS",year,quater,"_",workspace_no)),
             silent=T)
      dbHasCompleted(rs)

      dbWriteTable(conn,paste0("INSPECTRS",year,quater,"_",workspace_no),result)
      dbDisconnect(conn)
    }#function
 )#cmpfun
  A()
  }#function
