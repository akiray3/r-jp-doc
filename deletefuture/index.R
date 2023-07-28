library(needs)
needs(tidyverse)
needs(jsonlite)
#detailのページに組み込むための引数のリストを作る
json <- jsonlite::fromJSON("help_db_raw.json") %>% as_tibble()
table <- json %>% select(func,Arguments) %>% mutate(Arguments =  as.character(Arguments)) %>%
  replace_na(replace = list(Arguments= "")) %>% 
    separate(col = Arguments,sep = "<tr",into = paste0("arguments",0:60))
table[,3] <- replace_na(table$arguments1,replace = "")
table[,2:60] <- lapply(table[,2:60],function(x) gsub(pattern = "</code>.*",replacement = "",x))
table[,2:60] <- lapply(table[,2:60],function(x) gsub(pattern = "valign=\"top\"><td><code>",replacement = "",x))
table<- select(table,-arguments0)
write(toJSON(table),file = "detail/argument.json")
print(table,n = 40)


