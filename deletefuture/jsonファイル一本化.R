#json 組み込み
library(needs)
needs(jsonlite)
needs(tidyverse)
setwd("/Users/aizawaharuka/Documents/GitHub/r-jp-doc")

#base grahics psych stats で分かれているsvgのパスとtagのデータを合わせる
#今は不要のhrefの列を削除する
#ファイルは一本化する
base <- jsonlite::fromJSON("infopage/base.json")
graphic <- jsonlite::fromJSON("infopage/graphics.json")
psych <- jsonlite::fromJSON("infopage/psych.json")
stat <- jsonlite::fromJSON("infopage/stats.json")

bind2 <-rbind(graphic,psych,stat)
addResult <- dplyr::bind_rows(base,bind2) %>% 
  select(-href) %>% 
  select(-id) %>% 
  rename(func = funcname)
head(addResult)
writejson <- toJSON(addResult,pretty = TRUE)
writeLines(writejson,"infopage/SvgAndTagList.json")


original <- jsonlite::fromJSON("help_db_jpn_main.json")
head(original)
join <- left_join(original,addResult,by = "func")
writejoin <- toJSON(join,pretty = TRUE)
writeLines(writejoin,"infopage/db_main_add.json")
