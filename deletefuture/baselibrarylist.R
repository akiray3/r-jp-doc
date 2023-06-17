#関数一覧リンクの作成
needs(readxl)
needs(tidyverse)
needs(foreach)
dat <- readxl::read_xlsx("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/deletefuture/help_dbコピー関数名用.xlsx")
head(dat)
dat2 <- dat %>% 
 mutate(funcoriginal = func) 
 
dat2$func <- gsub("[/\\\\?%*:|\"<>]","_",dat2$func)
dat2%>%
  mutate(html = func)%>%
  mutate(html = str_remove(string = html,pattern = "\\.")) %>% 
  mutate(func = paste0("{ id:'",func,"', href:")) %>% 
  mutate(html = paste0("'/detail/base/",html,".html'")) %>% 
  mutate(tagtxt = paste0(",texttag:'",tagtxt,"'}")) %>% 
  mutate(func = paste0(func,html,tagtxt,",")) %>% 
  select(-"html") %>%
  select(-"tagtxt") %>% 
  writexl::write_xlsx("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/deletefuture/librarylist.xlsx")

dat2%>%writexl::write_xlsx("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/deletefuture/関数名と変換前の比較.xlsx")
#base関数用　関数紹介のページ作成
filenamelist <- dat2 %>%
  filter(pack =="base") %>% 
  mutate(func = str_remove(string = func,pattern = "\\.")) %>% 
  select(func) %>% as.list()%>%unlist()%>%unique()
funcoriginal <- dat2%>%
  filter(pack == "base")%>%
  select(funcoriginal)%>%as.list() %>% unlist() %>% unique()

foreach::foreach(a = filenamelist,b = funcoriginal)%do%{
  content <- paste0("
<!DOCTYPE html><head>    <title>", b, "</title>
 </head>
 <body>    
 <div class=\"block\"></div>    
 <div class=\"tabs\" style=\"display: flex; transform: translate(0, 0)\" >
   <button class=\"tab\" onclick=\"openTab(event,'Description') \">
     Description
   </button>
   <button class=\"tab\" onclick=\"openTab(event,'Arguments')\">Arguments</button>
   <button class=\"tab\" onclick=\"openTab(event,'Value')\">Value</button>
   <button class=\"tab\" onclick=\"openTab(event,'Details')\">Details</button>
   <button class=\"tab\" onclick=\"openTab(event,'Examples')\">Examples</button>
   <button class=\"tab\" onclick=\"openTab(event,'References')\">
     References
   </button>
   <button class=\"tab\" onclick=\"openTab(event,'See_Also')\">See_Also</button>
 </div>
<div class=\"tabcontents\" style=\"display: block;\"></div>
<link rel=\"stylesheet\" href=\"../style.css\" type=\"text/css\" />
<script src=\"../script.js\"></script>
</body>")    
  htmlpath <- file.path("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/detail/stats", paste0(a, ".html"))
  write_lines(content, path = htmlpath)
}
