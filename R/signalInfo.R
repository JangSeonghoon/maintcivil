#'
#' signal information
#'
#' @param workspace_no, startT,lastT,direction, order, kind
#' @return km of the signal
devtools::use_package("stringr")

#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom compiler cmpfun
#' @export
signal=function(workspace_no,startT,lastT,direction,order,kind){
A=cmpfun(
function(){

  if(Sys.info()['sysname']=="Windows"){
    path=
      paste0(
        Sys.getenv("CATALINA_HOME"),"/webapps/bigTeam/"
      )
  }else if(Sys.info()['sysname']=="Linux"){
    load("/home/jsh/eclipse-workspace/bigTeam/src/main/webapp/")
  }


startT=as.character(startT)
lastT=as.character(lastT)
direction=as.character(direction)
kind=as.character(kind)

workspace_no=floor(workspace_no/100)*100


load(path,"RData/DB(utf8).RData")


compare=eval(parse(text=paste0("signal_",workspace_no)))

compareSet=str_c(compare[,1],collape=",",compare[,2],collape=",",compare[,3],collape=",",compare[,4],collape="번,",compare[,6],collape="")

no=which(
str_detect(compareSet,startT)*
  str_detect(compareSet,lastT)*
  str_detect(compareSet,direction)*
  str_detect(compareSet,paste0(order,"번"))*
  str_detect(compareSet,kind)==1
)[1]

return(compare[no,5])
}
)
A()
}
