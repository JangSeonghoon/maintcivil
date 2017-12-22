dpkg=c("RJDBC","dplyr","csvread")
sapply(dpkg,require,character.only=T)

#param
workspace_no=42423
year=2017
quater=4

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

quater=ifelse(nchar(quater)==1,paste0("0",quater),quater)
coltypes=map.coltypes(
  paste0("/home/jsh/eclipse-workspace/bigTeam/src/main/webapp/RData/",workspace_no,"_",year,quater,".csv"),header=T)
db=csvread(
  paste0("/home/jsh/eclipse-workspace/bigTeam/src/main/webapp/RData/",workspace_no,"_",year,quater,".csv"),coltypes=coltypes,header=T)
class(db)
db=db %>% dplyr::select(17,19,18,14,15,16,1,6,2,7,8)
names(db)=c("PARAMETER","EXCEPT","MAX","STARTD","LASTD","LEN","CARKIND","INSPECTDATE","SWITCH","PLANT","WORKSPACE")
db[,"INSPECTDATE"]=as.Date(db[,"INSPECTDATE"])
db

drv=JDBC("oracle.jdbc.driver.OracleDriver","/home/jsh/Downloads/ojdbc6.jar")
conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")

dbName=paste0("INSPECTRS",year,quater,"_",workspace_no)
dbWriteTable(conn,
             dbName,
             db)
dbDisconnect(conn)

}#function
)#cmpfun
A()
}
