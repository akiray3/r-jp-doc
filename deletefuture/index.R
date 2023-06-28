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


#日本語訳のtxtと合わせてみる
table_long <- table %>% pivot_longer(cols = 2:61,names_to = "num",values_to = "argments") %>% 
  drop_na()
line1 <- readLines("R/eng_jpn_1.txt") %>% as.tibble()
line2 <- readLines("R/eng_jpn_2.txt") %>% as_tibble()
jpn <- bind_rows(line1,line2)
jpn <- filter(jpn,!grepl(pattern = "^\\s*$",jpn$value))

bind_cols(table_long,jpn)
