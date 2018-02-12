#'
#' create Inspect Table
#'
#' @param year, quater,workspace_no
#' @return
devtools::use_package("magrittr")
devtools::use_package("dplyr")
devtools::use_package("csvread")
devtools::use_package("rJava")
devtools::use_package("DBI")
devtools::use_package("RJDBC")

#'@importFrom csvread map.coltypes
#'@importFrom csvread csvread
#'@importFrom dplyr select
#'@importFrom RJDBC JDBC
#'@importFrom DBI dbConnect
#'@importFrom DBI dbWriteTable
#'@importFrom DBI dbDisconnect
#'@importFrom compiler cmpfun
#'@export
createInspect=function(year,quater,workspace_no){
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

quater=ifelse(nchar(quater)==1,paste0("0",quater),quater)
coltypes=map.coltypes(
  paste0(path,"RData/",workspace_no,"_",year,quater,".csv"),header=T)
db=csvread(
  paste0(path,"RData/",workspace_no,"_",year,quater,".csv"),coltypes=coltypes,header=T)

if(length(db[,1])!=0){
DIRECTION=
sapply(db[,5],function(x)
  str_sub(x,nchar(x)-4,nchar(x))
  )

db=db %>% select(17,19,18,14,15,16,1,6,2,7,8)
names(db)=c("PARAMETER","EXCEPT","MAX","STARTD","LASTD","LEN","CARKIND","INSPECTDATE","SWITCH","PLANT","WORKSPACE")

db$INSPECTDATE=db$INSPECTDATE %>% as.Date()
db=db %>% mutate("DIRECTION"=DIRECTION)

drv=JDBC("oracle.jdbc.driver.OracleDriver",paste0(path,"driver/ojdbc6.jar"))
conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")

try(rs<-dbExecute(conn,paste0("drop table INSPECTRS",year,quater,"_",workspace_no)),
    silent=T)
dbHasCompleted(rs)

dbName=paste0("INSPECTRS",year,quater,"_",workspace_no)
dbWriteTable(conn,
             dbName,
             db)
dbDisconnect(conn)
}else{


}
}#function
)#cmpfun
A()
}
