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
executeManual=function(num,year,quater,workspace_no){
  A=cmpfun(
    function(){

      drv=JDBC("oracle.jdbc.driver.OracleDriver","/home/jsh/Downloads/ojdbc6.jar")
      conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")
      quater=ifelse(length(quater)==1,paste0("0",quater),quater)
      # rs=dbSendQuery(conn,paste0("select * from INSPECTRS",year,quater,"_BACKUP"))
      rs=dbSendQuery(conn,paste0("select * from INSPECTRS",year,quater))
      result=dbFetch(rs)

      desc=dbSendQuery(conn,"select * from tab")
      desc=dbFetch(desc)

      temp=dbSendQuery(conn,"select * from temporary")
      temp=dbFetch(temp)

      compare=temp[num,9]

      result


      if(
      sum(str_detect(desc[,1],paste0("INSPECTRS",year,quater,"_BACKUP")))==0
      ){
        dbWriteTable(conn,paste0("INSPECTRS",year,quater,"_BACKUP"),result)
      }




         try(rs<-dbExecute(conn,paste0("drop INSPECTRS",year,quater)),
             silent=T)
         dbHasCompleted(rs)

        # qry=sqlAppendTable(conn,paste0("TQI",year,"_",quater,"_",workspace_no),result,row.names=F)

        # dbWriteTable(conn,paste0("TQI",year,"_",quater,"_",workspace_no),result,row.names=F)



        # qry=sqlCreateTable(conn,paste0("TQI",year,"_",quater,"_",workspace_no),result)
        # dbExecute(conn,qry)
        # dbSendQuery(conn,qry)
        # dbDisconnect(conn)
        i=1;for(i in 1:length(result)){
          result[is.na(result[,i]),i]=""
          result[is.na(result[,i]),i]=NULL
          # result[,i]=as.numeric(result[,i])

        }
        write.csv(result,paste0("/home/jsh/DB/BigTeam/TQI",year,"_",quater,"_",workspace_no,".csv"),row.names=F)
        result<<-result
        # return(num[1])



    }#function
 )#cmpfun
  A()
  }#function
