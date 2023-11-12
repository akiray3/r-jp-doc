library(needs)
needs(tidyverse)
needs(foreach)
(x <- c(-1:1, (-1:1)/0))

df <- data.frame(x = 1:10, y = 11:20)


codeList <- readLines("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/エラー収集.R")
result <- tryCatch(
  {
    eval(parse(text = codeList[1055]))
  },
  error = function(e) {
   c(message = conditionMessage(e))
  },
  warning = function(w) {
   conditionMessage(w)
  },
  message = function(m){
    conditionMessage(m)
  },
  finally = function(f){
    conditionMessage(f)
  }
)

df <- unlist(result) %>% as.data.frame()
codeList <- readLines("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/エラー収集.R")

ErrorList <- foreach::foreach(code = codeList[1:8000],.combine = rbind) %do% {
  result <- tryCatch(
    { 
      eval(parse(text = code))
    },
    error = function(e) {
      c(ErrorMessage = conditionMessage(e),ErrorType = "error")
    },
    warning = function(w) {
      c(ErrorMessage = conditionMessage(w),ErrorType = "warning")
    },
    message = function(m){
      c(ErrorMessage = conditionMessage(m),ErrorType = "message")
    },
    finally = function(f){
      c(ErrorMessage = conditionMessage(f),ErrorType = "finally")
    }
  ) 
  
 result%>% unlist() %>% as.data.frame()
}
codeList[7874]

ErrorList <- do.call(rbind,ErrorList)

write.csv(ErrorList,file = "/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/エラー収集.csv")
