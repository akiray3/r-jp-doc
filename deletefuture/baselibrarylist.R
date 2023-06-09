#関数一覧リンクの作成
needs(readxl)
needs(tidyverse)
dat <- readxl::read_xlsx("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/deletefuture/help_dbコピー関数名用.xlsx")
head(dat)
dat %>% mutate(html = func) %>% 
  mutate(html = str_remove(string = html,pattern = "\\.")) %>% 
  mutate(tagtxt = func) %>% 
  mutate(func = paste0("{ id:'",func,"', href:")) %>% 
  mutate(html = paste0("'/detail/base/",html,".html'")) %>% 
  mutate(tagtxt = paste0(",texttag:'",tagtxt,"'}")) %>% 
  mutate(func = paste0(func,html,tagtxt,",")) %>% 
  select(-"html") %>%
  select(-"tagtxt") %>% 
  writexl::write_xlsx("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/deletefuture/librarylist.xlsx")


#base関数用　関数紹介のページ作成
filenamelist <- dat %>%
  mutate(html = func) %>% 
  filter(pack == "stats") %>% 
  mutate(html = str_remove(string = html,pattern = "\\.")) %>% 
  select(html) %>% as.list()

for (i in filenamelist$html) {
  content <- paste0("<!DOCTYPE html><head>    <title>",i,"</title></head><body>    <div class=\"block\"></div>    <script src=\"script.js\"></script></body>")
  path <- file.path("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/detail/stats",paste0(i,".html"))
  write_lines(content,path)
}

