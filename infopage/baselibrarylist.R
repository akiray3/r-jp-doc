#base関数一覧リンクの作成
needs(readxl)
needs(tidyverse)
dat <- readxl::read_xlsx("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/infopage/deletefuture/rlibrarybase.xlsx")
head(dat)
dat %>% separate(col = "Information on package ‘base’",sep = " ",into = "new") %>% 
  mutate(html = new) %>% 
  mutate(html = str_remove(string = html,pattern = "\\.")) %>% 
  mutate(tagtxt = new) %>% 
  mutate(new = paste0("{ id:'",new,"', href:")) %>% 
  mutate(html = paste0("'/detail/base/",html,".html'")) %>% 
  mutate(tagtxt = paste0(",texttag:'",tagtxt,"'}")) %>% 
  mutate(new = paste0(new,html,tagtxt,",")) %>% 
  select(-"html") %>%
  select(-"tagtxt") %>% 
  writexl::write_xlsx("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/infopage/deletefuture/librarylist.xlsx")


#base関数用　関数紹介のページ作成
filenamelist <- dat %>% separate(col = "Information on package ‘base’",sep = " ",into = "new") %>% 
  mutate(html = new) %>% 
  mutate(html = str_remove(string = html,pattern = "\\.")) %>% 
  select(html) %>% as.list()

for (i in filenamelist$html) {
  content <- paste0("<!DOCTYPE html><head>    <title>",i,"</title></head><body>    <div class=\"block\"></div>    <script src=\"script.js\"></script></body>")
  path <- file.path("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/detail/base",paste0(i,".html"))
  write_lines(content,path)
}
