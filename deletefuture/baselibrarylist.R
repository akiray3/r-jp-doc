#関数一覧リンクの作成
needs(readxl)
needs(tidyverse)
needs(foreach)
dat <- readxl::read_xlsx("/Users/aizawaharuka/Documents/GitHub/名称未設定/deletefuture/help_dbコピー関数名用.xlsx")
head(dat)
dat2 <- dat %>% 
 mutate(funcoriginal = func) %>% 
  mutate(filename = func)
dat2$filename <- gsub("[/]","symbol1",dat2$filename)
dat2$filename <- gsub("[\\\\]","symbol2",dat2$filename)
dat2$filename <- gsub("[?]","symbol3",dat2$filename)
dat2$filename <- gsub("[%]","symbol4",dat2$filename)
dat2$filename <- gsub("[*]","symbol5",dat2$filename)
dat2$filename <- gsub("[:]","symbol6",dat2$filename)
dat2$filename <- gsub("[|]","symbol7",dat2$filename)
dat2$filename <- gsub("[\"]","symbol8",dat2$filename)
dat2$filename <- gsub("[<]","symbol9",dat2$filename)
dat2$filename <- gsub("[>]","symbol10",dat2$filename)
dat2$filename <- gsub("[.]","symbol11",dat2$filename)
dat2%>%
  filter(pack == "base") %>% 
  mutate(func = paste0("{ id:'",func,"', href:")) %>% 
  mutate(html = paste0("'/detail/base/",filename,".html'")) %>% 
  mutate(funcoriginal = paste0(",funcname:'",funcoriginal,"'}")) %>% 
  mutate(func = paste0(func,html,funcoriginal,",")) %>% 
  select(-"html") %>%
  select(-"funcoriginal") %>% 
  writexl::write_xlsx("/Users/aizawaharuka/Documents/GitHub/名称未設定/deletefuture/librarylist.xlsx")

#dat2%>%writexl::write_xlsx("/Users/aizawaharuka/Documents/GitHub/r-jp-doc/deletefuture/関数名と変換前の比較.xlsx")
#base関数用　関数紹介のページ作成
#ファイル名として使用できない記号はファイル名が被らないようにそれぞれ別の文字に
#リストに直した時点でfuncnameとfilenameの並びがずれるので、１ファイル作成ごとに記号を置換してファイル名とhtmltitleに反映させる
detail <- c("base","psych","stats","graphics")
foreach::foreach(c = detail)%do%{
  funcoriginal <- dat2%>%
    filter(pack == c)%>%
    select(funcoriginal)%>%as.list() %>% unlist
  foreach::foreach(b = funcoriginal)%do%{
    content <- paste0("
    <!DOCTYPE html>
<head>
  <title>",b,"</title>
</head>
<body>
  <div class=\"block\"></div>

  <div class=\"tabcontents\" style=\"display: block\"></div>
  <div class=\"tabs\" style=\"display: block\">
    <div style=\"display: flex; transform: translate(0, 0)\">
      <button class=\"tab\" onclick=\"openTab(event,'Description')\" id=\"default\">
        Description
      </button>
      <button class=\"tab\" onclick=\"openTab(event,'Arguments')\">
        Arguments
      </button>
      <button class=\"tab\" onclick=\"openTab(event,'Value')\">Value</button>
      <button class=\"tab\" onclick=\"openTab(event,'Details')\">Details</button>
      <button class=\"tab\" onclick=\"openTab(event,'Examples')\">Examples</button>
      <button class=\"tab\" onclick=\"openTab(event,'References')\">
        References
      </button>
      <button class=\"tab\" onclick=\"openTab(event,'See_Also')\">See_Also</button>
    </div>
  </div>
  <link rel=\"stylesheet\" href=\"../style.css\" type=\"text/css\" />
  <script src=\"../script.js\"></script>
</body>
")    
    b <- gsub("[/]","symbol1",b)
    b <- gsub("[\\\\]","symbol2",b)
    b <- gsub("[?]","symbol3",b)
    b <- gsub("[%]","symbol4",b)
    b <- gsub("[*]","symbol5",b)
    b <- gsub("[:]","symbol6",b)
    b <- gsub("[|]","symbol7",b)
    b <- gsub("[\"]","symbol8",b)
    b <- gsub("[<]","symbol9",b)
    b <- gsub("[>]","symbol10",b)
    b <- gsub("[.]","symbol11",b)
    htmlpath <- file.path("/Users/aizawaharuka/Documents/GitHub/名称未設定/detail/", paste0(c,"/",b, ".html"))
    write_lines(content, file = htmlpath)
  }
  
}
