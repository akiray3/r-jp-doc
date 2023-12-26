library(needs)
needs(tidyverse)
needs(foreach)
(x <- c(-1:1, (-1:1)/0))

df <- data.frame(x = 1:10, y = 11:20)
codeList <- readLines("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/エラー収集.R")
number <- 8
startNum<-26001
EndNum  <-27212
#英語
Sys.setenv(LANGUAGE="en")
ErrorListEn <- foreach::foreach(code = codeList[startNum:EndNum],.combine = rbind) %do% {
  result <- tryCatch(
    { 
      eval(parse(text = code))
    },
    error = function(e) {
      data.frame(ErrorMessage = conditionMessage(e),ErrorType = "error")
    },
    warning = function(w) {
      data.frame(ErrorMessage = conditionMessage(w),ErrorType = "warning")
    },
    message = function(m){
      data.frame(ErrorMessage = conditionMessage(m),ErrorType = "message")
    },
    finally = function(f){
     data.frame(ErrorMessage = conditionMessage(f),ErrorType = "finally")
    }
  ) 
  
 result%>% unlist() %>% as.data.frame()
}

df <- ErrorListEn %>% tibble::rownames_to_column() 
df2 <- df[grepl(pattern = "Error",x = df$rowname),]
df3 <- rename(.data = df2,error = .) 
df4 <- df3 %>% 
  distinct(error,.keep_all = TRUE)

write.csv(df4,file = paste0("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/ErrorEN",number,".csv"))

#日本語
Sys.setenv(LANGUAGE="ja")
ErrorListJp <- foreach::foreach(code = codeList[startNum:EndNum],.combine = rbind) %do% {
  result <- tryCatch(
    { 
      eval(parse(text = code))
    },
    error = function(e) {
      data.frame(ErrorMessage = conditionMessage(e),ErrorType = "error")
    },
    warning = function(w) {
      data.frame(ErrorMessage = conditionMessage(w),ErrorType = "warning")
    },
    message = function(m){
      data.frame(ErrorMessage = conditionMessage(m),ErrorType = "message")
    },
    finally = function(f){
      data.frame(ErrorMessage = conditionMessage(f),ErrorType = "finally")
    }
  ) 
  
  result%>% unlist() %>% as.data.frame()
}
ef <- ErrorListJp %>% tibble::rownames_to_column() 
ef2 <- ef[grepl(pattern = "Error",x = ef$rowname),]
ef3 <- rename(.data = ef2,error = .) 
ef4 <- ef3 %>% 
  distinct(error,.keep_all = TRUE)
write.csv(ef4,file = paste0("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/ErrorJP",number,".csv"))


#戻す
Sys.setenv(LANGUAGE="en")

#結合して保存
df5 <- readr::read_csv(file = paste0("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/ErrorEN",number,".csv")) %>% select(-1)
ef5 <- readr::read_csv(file = paste0("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/ErrorJP",number,".csv")) %>% select(-1)
edList <- left_join(df5,ef5,by = "rowname")
edList <- edList %>% rename(errorEN = error.x) %>% rename(errorJP = error.y)
jsonlite::write_json(edList,path = paste0("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/error収集/errorlist",number,".json"))

#まとめて一つにするとサイズが大きいために読み込めないらしい
#別々で読み込んで使用する