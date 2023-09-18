(x <- c(-1:1, (-1:1)/0))

df <- data.frame(x = 1:10, y = 11:20)

result <- tryCatch({
  base::lm(y ~ x, data = df)
}, error = function(e) {
  e
})

if (inherits(result, "error")) {
  error_message <- as.character(result)
  error_df <- data.frame(Error_Message = error_message)
  write.csv(error_df, file = "/Users/aizawaharuka/Documents/GitHub/名称未設定/deletefuture/エラーの内容.csv", row.names = FALSE)
} else {
  # エラーが発生しなかった場合の処理
}


#-

#-.Date 
#-.POSIXt 
#: 
#::
#:::
#!
#!.hexmode
#!.octmode
#!=
#(
#[
  test <- matrix(1:12, nrow = 3, ncol = 4)
    test[e]
#[.AsIs
#[.data.frame
#[.Date
#[.difftime
#[.Dlist
#[.DLLInfoList
#[.factor
#[.hexmode
#[.listof
#[.noquote
#[.numeric_version
#[.octmode
#[.POSIXct
#[.POSIXlt
#[.simple.list
#[.table
#[.warnings
#[[
#[[.data.frame
#[[.Date
#[[.factor
#[[.numeric_version
#[[.POSIXct
#[[.POSIXlt
#[[<-
#[[<-.data.frame
#[[<-.factor
#[[<-.numeric_version
#[[<-.POSIXlt
#[<-
#[<-.data.frame
#[<-.Date
#[<-.difftime
#[<-.factor
#[<-.numeric_version
#[<-.POSIXct
#[<-.POSIXlt
#{
#@
#@<-
#*
#*.difftime
#/
#/.difftime
#&
#&.hexmode
#&.octmode
#&&
#%*%
#%/%
#%%
#%in%
#%o%
#%x%
#^
#+
#+.Date
#+.POSIXt
#<
#<-
#<<-
#<=
#=
#==
#>
#>=
#|
#|.hexmode
#|.octmode
#||
#~
#$
#$.DLLInfo
#$.package_version
#$<-
#$<-.data.frame
#abbreviate
  abbreviate(names.arg = ,minlength = ,use.classes = ,dot = ,strict = ,method = ,named = )
  abbreviate(names.arg = x,minlength = ,use.classes = ,dot = ,strict = ,method = ,named = )
  abbreviate(names.arg = x,minlength = x,use.classes = ,dot = ,strict = ,method = ,named = )
  abbreviate(names.arg = x,minlength = x,use.classes = x,dot = ,strict = ,method = ,named = )
  abbreviate(names.arg = x,minlength = x,use.classes = x,dot = x,strict = ,method = ,named = )
  abbreviate(names.arg = x,minlength = x,use.classes = x,dot = x,strict = x,method = ,named = )
  abbreviate(names.arg = x,minlength = x,use.classes = x,dot = x,strict = x,method = x,named = )
  abbreviate(names.arg = x,minlength = x,use.classes = x,dot = x,strict = x,method = x,named = x)
  abbreviate(names.arg = "x",minlength = x,use.classes = x,dot = x,strict = x,method = x,named = x)  
  abbreviate(names.arg = df,minlength = x,use.classes = x,dot = x,strict = x,method = x,named = x) 
  
#abs
  abs(x = )
  abs(x = x)
  abs(x = "x")
  abs(x = df)
#acos
  acos(x = )
  acos(x = x)
  acos(x = "x")
  acos(x = df)

  
#acosh
  acosh(x = )
  acosh(x = x)
  acosh(x = "x")
  acosh(x = df)
#activeBindingFunction
  activeBindqingFunction(sym = ,env = )
  activeBindingFunction(sym = x,env = )
  activeBindingFunction(sym = x,env = x)  
  activeBindingFunction(sym = "x",env = )
  activeBindingFunction(sym = df,env = )
#addNA
  addNA(x = ,ifany = )
  addNA(x = x,ifany = )
  addNA(x = x,ifany = x)
  addNA(x = "x",ifany = )
  addNA(x = df,ifany = )
#addTaskCallback
  addTaskCallback(f = ,data = ,name = )
  addTaskCallback(f = x,data = ,name = )
  addTaskCallback(f = x,data = x,name = )
  addTaskCallback(f = x,data = x,name = x)
  addTaskCallback(f = "x",data = ,name = )
  addTaskCallback(f = df,data = ,name = )
#agrep
  agrep(pattern = ,x = ,max.distance = ,costs = ,ignore.case = ,value = ,fixed = ,useBytes = )
  agrep(pattern = x,x = ,max.distance = ,costs = ,ignore.case = ,value = ,fixed = ,useBytes = )
  agrep(pattern = x,x = x,max.distance = ,costs = ,ignore.case = ,value = ,fixed = ,useBytes = )
  agrep(pattern = x,x = x,max.distance = x,costs = ,ignore.case = ,value = ,fixed = ,useBytes = )
  agrep(pattern = x,x = x,max.distance = x,costs = x,ignore.case = ,value = ,fixed = ,useBytes = )
  agrep(pattern = x,x = x,max.distance = x,costs = x,ignore.case = x,value = ,fixed = ,useBytes = )
  agrep(pattern = x,x = x,max.distance = x,costs = x,ignore.case = x,value = x,fixed = ,useBytes = )
  agrep(pattern = x,x = x,max.distance = x,costs = x,ignore.case = x,value = x,fixed = x,useBytes = )
  agrep(pattern = x,x = x,max.distance = x,costs = x,ignore.case = x,value = x,fixed = x,useBytes = x)
  agrep(pattern = "x",x = x,max.distance = x,costs = x,ignore.case = x,value = x,fixed = x,useBytes = x)
  agrep(pattern = df,x = x,max.distance = x,costs = x,ignore.case = x,value = x,fixed = x,useBytes = x)
#agrepl
  agrepl(pattern = ,x = ,max.distance = ,costs = ,ignore.case = ,fixed = ,useBytes = )
  agrepl(pattern = x,x = ,max.distance = ,costs = ,ignore.case = ,fixed = ,useBytes = )
  agrepl(pattern = x,x = x,max.distance = ,costs = ,ignore.case = ,fixed = ,useBytes = )
  agrepl(pattern = x,x = x,max.distance = x,costs = ,ignore.case = ,fixed = ,useBytes = )
  agrepl(pattern = x,x = x,max.distance = x,costs = x,ignore.case = ,fixed = ,useBytes = )
  agrepl(pattern = x,x = x,max.distance = x,costs = x,ignore.case = x,fixed = ,useBytes = )
  agrepl(pattern = x,x = x,max.distance = x,costs = x,ignore.case = x,fixed = x,useBytes = )
  agrepl(pattern = x,x = x,max.distance = x,costs = x,ignore.case = x,fixed = x,useBytes = x)
  agrepl(pattern = "x",x = x,max.distance = x,costs = x,ignore.case = x,fixed = x,useBytes = x)
  agrepl(pattern = df,x = x,max.distance = x,costs = x,ignore.case = x,fixed = x,useBytes = x)
#alist
alist(5)
alist(x)
alist(x = 5)
alist("x = x")
alist(df)
#all
all(4,na.rm = true)
all(5)
all(x)
all("x")
all(df)
#all.equal
  base::all.equal(4,test[2,])
all.equal(x)
all.equal(x,y)
all.equal(df)
#all.equal.character
  
  base::all.equal("eset","taturo")
  all.equal(target = x,current = )
  all.equal(target = x,current = x)
    all.equal(target = "x",current = x)
  all.equal(target = df,current = x)

  
#all.equal.default
all.equal.default()
all.equal.default(target = ,current = )
all.equal.default(target = x,current = )
all.equal.default(target = x,current = x)
all.equal.default(target = "x",current = x)
all.equal.default(target = df,current = x)


#all.equal.environment
env1 <- new.env()
env1$a <- 1
env1$b <- 2

env2 <- new.env()
env2$a <- 4
env2$b <- 0
  base::all.equal.encvironment(env1,env2)
all.equal.environment(target = ,current = ,all.names = ,evaluate = )
all.equal.environment(target = x,current = ,all.names = ,evaluate = )
all.equal.environment(target = x,current = x,all.names = ,evaluate = )
all.equal.environment(target = x,current = x,all.names = x,evaluate = )
all.equal.environment(target = x,current = x,all.names = x,evaluate = x)
all.equal.environment(target = "x",current = x,all.names = x,evaluate = x)
all.equal.environment(target = df,current = x,all.names = x,evaluate = x)

#all.equal.envRefClass
all.equal.envRefClass(target = ,current = )
all.equal.envRefClass(target = x,current = )
all.equal.envRefClass(target = x,current = x)
all.equal.envRefClass(target = "x",current = x)
all.equal.envRefClass(target = df,current = x)
#all.equal.factor
all.equal.factor(target = ,current = ,check.attributes = )
all.equal.factor(target = x,current = ,check.attributes = )
all.equal.factor(target = x,current = x,check.attributes = )
all.equal.factor(target = x,current = x,check.attributes = x)
all.equal.factor(target = "x",current = x,check.attributes = x)
all.equal.factor(target = df,current = x,check.attributes = x)
#all.equal.formula
all.equal.formula(target = ,current = )
all.equal.formula(target = x,current = )
all.equal.formula(target = x,current = x)
all.equal.formula(target = "x",current = x)
all.equal.formula(target = df,current = x)
#all.equal.function
all.equal.function(target = ,current = ,check.environment = )
all.equal.function(target = x,current = ,check.environment = )
all.equal.function(target = x,current = x,check.environment = )
all.equal.function(target = x,current = x,check.environment = x)
all.equal.function(target = "x",current = x,check.environment = x)
all.equal.function(target = df,current = x,check.environment = x)
#all.equal.language
all.equal.language(target = ,current = )
all.equal.language(target = x,current = )
all.equal.language(target = x,current = x)
all.equal.language(target = "x",current = x)
all.equal.language(target = df,current = x)

#all.equal.list
  all.equal.list(5)
  all.equal.list(target = ,current = ,check.attributes = ,use.names = )
  all.equal.list(target = x,current = ,check.attributes = ,use.names = )
  all.equal.list(target = x,current = x,check.attributes = ,use.names = )
  all.equal.list(target = x,current = x,check.attributes = x,use.names = )
  all.equal.list(target = x,current = x,check.attributes = x,use.names = x)
  all.equal.list(target = "x",current = x,check.attributes = x,use.names = x)
  all.equal.list(target = df,current = x,check.attributes = x,use.names = x)
#all.equal.numeric
all.equal.numeric(target = ,current = ,tolerance = ,scale = ,countEQ = ,formatFUN = ,format(),check.attributes = )
all.equal.numeric(target = x,current = ,tolerance = ,scale = ,countEQ = ,formatFUN = ,format(),check.attributes = )
all.equal.numeric(target = x,current = x,tolerance = ,scale = ,countEQ = ,formatFUN = ,format(),check.attributes = )
all.equal.numeric(target = x,current = x,tolerance = x,scale = ,countEQ = ,formatFUN = ,format(),check.attributes = )
all.equal.numeric(target = x,current = x,tolerance = x,scale = x,countEQ = ,formatFUN = ,format(),check.attributes = )
all.equal.numeric(target = x,current = x,tolerance = x,scale = x,countEQ = x,formatFUN = ,format(),check.attributes = )
all.equal.numeric(target = x,current = x,tolerance = x,scale = x,countEQ = x,formatFUN = x,format(),check.attributes = )
all.equal.numeric(target = x,current = x,tolerance = x,scale = x,countEQ = x,formatFUN = x,format() = ,check.attributes = )
all.equal.numeric(target = x,current = x,tolerance = x,scale = x,countEQ = x,formatFUN = x,format() = x,check.attributes = )
all.equal.numeric(target = x,current = x,tolerance = x,scale = x,countEQ = x,formatFUN = x,format() = x,check.attributes = x)
all.equal.numeric(target = "x",current = x,tolerance = x,scale = x,countEQ = x,formatFUN = x,format() = x,check.attributes = x)
all.equal.numeric(target = df,current = x,tolerance = x,scale = x,countEQ = x,formatFUN = x,format() = x,check.attributes = x)
#all.equal.POSIXt
all.equal.POSIXt(target = ,current = ,tolerance = ,scale = ,check.tzone = )
all.equal.POSIXt(target = x,current = ,tolerance = ,scale = ,check.tzone = )
all.equal.POSIXt(target = x,current = x,tolerance = ,scale = ,check.tzone = )
all.equal.POSIXt(target = x,current = x,tolerance = x,scale = ,check.tzone = )
all.equal.POSIXt(target = x,current = x,tolerance = x,scale = x,check.tzone = )
all.equal.POSIXt(target = x,current = x,tolerance = x,scale = x,check.tzone = x)
all.equal.POSIXt(target = "x",current = x,tolerance = x,scale = x,check.tzone = x)
all.equal.POSIXt(target = df,current = x,tolerance = x,scale = x,check.tzone = x)
#all.equal.raw
all.equal.raw(target = ,current = ,check.attributes = ) 
all.equal.raw(target = x,current = ,check.attributes = )
all.equal.raw(target = x,current = x,check.attributes = )
all.equal.raw(target = x,current = x,check.attributes = x)
all.equal.raw(target = "x",current = x,check.attributes = x)
all.equal.raw(target = df,current = x,check.attributes = x)

#all.names
all.names(expr = ,functions = ,max.names = ,unique = )
all.names(expr = x,functions = ,max.names = ,unique = )
all.names(expr = x,functions = x,max.names = ,unique = )
all.names(expr = x,functions = x,max.names = x,unique = )
all.names(expr = x,functions = x,max.names = x,unique = x)
all.names(expr = "x",functions = x,max.names = x,unique = x)
all.names(expr = df,functions = x,max.names = x,unique = x)
#all.vars
all.vars(expr = ,functions = ,max.names = ,unique = )
all.vars(expr = x,functions = ,max.names = ,unique = )
all.vars(expr = x,functions = x,max.names = ,unique = )
all.vars(expr = x,functions = x,max.names = x,unique = )
all.vars(expr = x,functions = x,max.names = x,unique = x)
all.vars(expr = "x",functions = x,max.names = x,unique = x)
all.vars(expr = df,functions = x,max.names = x,unique = x)
#any
any(4,na.rm = true)
any(5)
any(x)
any("x")
any(df)
#anyDuplicated
anyDuplicated(x = ,fromLast = )
anyDuplicated(x = x,fromLast = )
anyDuplicated(x = x,fromLast = x)
anyDuplicated(x = "x",fromLast = )
anyDuplicated(x = df,fromLast = )
#anyDuplicated.default
anyDuplicated.default(x = ,fromLast = )
anyDuplicated.default(x = x,fromLast = )
anyDuplicated.default(x = x,fromLast = x)
anyDuplicated.default(x = "x",fromLast = )
anyDuplicated.default(x = df,fromLast = )
#anyDuplicated.matrix
anyDuplicated.matrix(x = ,fromLast = )
anyDuplicated.matrix(x = x,fromLast = )
anyDuplicated.matrix(x = x,fromLast = x)
anyDuplicated.matrix(x = "x",fromLast = )
anyDuplicated.matrix(x = df,fromLast = )
#anyNA
anyNA(x = ,recursive = )
anyNA(x = x,recursive = )
anyNA(x = x,recursive = x)
anyNA(x = "x",recursive = )
anyNA(x = df,recursive = )
#aperm
aperm(a = ,perm = )
aperm(a = x,perm = )
aperm(a = x,perm = x)
aperm(a = "x",perm = )
aperm(a = df,perm = )
#aperm.default
aperm.default(a = ,perm = ,resize = )
aperm.default(a = x,perm = ,resize = )
aperm.default(a = x,perm = x,resize = )
aperm.default(a = x,perm = x,resize = x)
aperm.default(a = "x",perm = x,resize = x)
aperm.default(a = df,perm = x,resize = x)
#allowInterrupts
allowInterrupts(expr = )
allowInterrupts(expr = x)
allowInterrupts(expr = "x")
allowInterrupts(expr = df)

#any
any(na.rm = )
any(na.rm = x)
any(na.rm = "x")
any(na.rm = df)

#anyDuplicated
anyDuplicated(x = )
anyDuplicated(x = x)
anyDuplicated(x = x)
anyDuplicated(x = "x")
anyDuplicated(x = df)
#anyDuplicated.default
anyDuplicated.default(x = )
anyDuplicated.default(x = x)
anyDuplicated.default(x = x)
anyDuplicated.default(x = "x")
anyDuplicated.default(x = df)
#anyDuplicated.matrix
anyDuplicated.matrix(x = )
anyDuplicated.matrix(x = x)
anyDuplicated.matrix(x = x)
anyDuplicated.matrix(x = "x")
anyDuplicated.matrix(x = df)
#anyNA
anyNA(x = )
anyNA(x = x)
anyNA(x = x)
anyNA(x = "x")
anyNA(x = df)
#aperm
aperm(a = )
aperm(a = x)
aperm(a = x)
aperm(a = "x")
aperm(a = df)
#aperm.default
aperm.default(a =1 )
aperm.default(a = x)
aperm.default(a = x)
aperm.default(a = "x")
aperm.default(a = x,perm = ,resize = )
aperm.default(a = x,perm = x,resize = )
aperm.default(a = x,perm = x,resize = x)
aperm.default(a = "x",perm = x,resize = x)
aperm.default(a = df,perm = x,resize = x)
anyDuplicated(x = ,incomparables = )
anyDuplicated(x = x,incomparables = )
anyDuplicated(x = x,incomparables = x)

#anyDuplicated.array
anyDuplicated.array(x = ,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.array(x = x,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.array(x = x,incomparables = x,MARGIN = ,fromLast = )
anyDuplicated.array(x = x,incomparables = x,MARGIN = x,fromLast = )
anyDuplicated.array(x = x,incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.array(x = "x",incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.array(x = df,incomparables = x,MARGIN = x,fromLast = x)
#anyDuplicated.data.frame
anyDuplicated.data.frame(x = ,incomparables = ,fromLast = )
anyDuplicated.data.frame(x = x,incomparables = ,fromLast = )
anyDuplicated.data.frame(x = x,incomparables = x,fromLast = )
anyDuplicated.data.frame(x = x,incomparables = x,fromLast = x)
anyDuplicated.data.frame(x = "x",incomparables = x,fromLast = x)
anyDuplicated.data.frame(x = df,incomparables = x,fromLast = x)
#anyDuplicated.default
anyDuplicated.default(x = ,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = x)
anyDuplicated.default(x = "x",incomparables = x,fromLast = x)
anyDuplicated.default(x = df,incomparables = x,fromLast = x)
#anyDuplicated.matrix
anyDuplicated.matrix(x = ,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = "x",incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = df,incomparables = x,MARGIN = x,fromLast = x)
#anyNA
anyNA(x = ,recursive = )
anyNA(x = x,recursive = )
anyNA(x = x,recursive = x)
anyNA(x = "x",recursive = )
anyNA(x = df,recursive = )
#aperm
aperm(a = ,perm = )
aperm(a = x,perm = )
aperm(a = x,perm = x)
aperm(a = "x",perm = )
aperm(a = df,perm = )
#aperm.default
aperm.default(a = ,perm = ,resize = )
aperm.default(a = x,perm = ,resize = )
aperm.default(a = x,perm = x,resize = )
aperm.default(a = x,perm = x,resize = x)
aperm.default(a = "x",perm = x,resize = x)
aperm.default(a = df,perm = x,resize = x)
#anyDuplicated.default
anyDuplicated.default(x = ,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = x)
anyDuplicated.default(x = "x",incomparables = x,fromLast = x)
anyDuplicated.default(x = df,incomparables = x,fromLast = x)
#anyDuplicated.matrix
anyDuplicated.matrix(x = ,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = "x",incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = df,incomparables = x,MARGIN = x,fromLast = x)
#anyNA
anyNA(x = ,recursive = )
anyNA(x = x,recursive = )
anyNA(x = x,recursive = x)
anyNA(x = "x",recursive = )
anyNA(x = df,recursive = )
#aperm
aperm(a = ,perm = )
aperm(a = x,perm = )
aperm(a = x,perm = x)
aperm(a = "x",perm = )
aperm(a = df,perm = )
#aperm.default
aperm.default(a = ,perm = ,resize = )
aperm.default(a = x,perm = ,resize = )
aperm.default(a = x,perm = x,resize = )
aperm.default(a = x,perm = x,resize = x)
aperm.default(a = "x",perm = x,resize = x)
aperm.default(a = df,perm = x,resize = x)
#anyDuplicated.default
anyDuplicated.default(x = ,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = x)
anyDuplicated.default(x = "x",incomparables = x,fromLast = x)
anyDuplicated.default(x = df,incomparables = x,fromLast = x)
#anyDuplicated.matrix
anyDuplicated.matrix(x = ,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = "x",incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = df,incomparables = x,MARGIN = x,fromLast = x)
#anyNA
anyNA(x = ,recursive = )
anyNA(x = x,recursive = )
anyNA(x = x,recursive = x)
anyNA(x = "x",recursive = )
anyNA(x = df,recursive = )
#aperm
aperm(a = ,perm = )
aperm(a = x,perm = )
aperm(a = x,perm = x)
aperm(a = "x",perm = )
aperm(a = df,perm = )
#aperm.default
aperm.default(a = ,perm = ,resize = )
aperm.default(a = x,perm = ,resize = )
aperm.default(a = x,perm = x,resize = )
aperm.default(a = x,perm = x,resize = x)
aperm.default(a = "x",perm = x,resize = x)
aperm.default(a = df,perm = x,resize = x)
#anyDuplicated.default
anyDuplicated.default(x = ,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = x)
anyDuplicated.default(x = "x",incomparables = x,fromLast = x)
anyDuplicated.default(x = df,incomparables = x,fromLast = x)
#anyDuplicated.matrix
anyDuplicated.matrix(x = ,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = "x",incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = df,incomparables = x,MARGIN = x,fromLast = x)
#anyNA
anyNA(x = ,recursive = )
anyNA(x = x,recursive = )
anyNA(x = x,recursive = x)
anyNA(x = "x",recursive = )
anyNA(x = df,recursive = )
#aperm
aperm(a = ,perm = )
aperm(a = x,perm = )
aperm(a = x,perm = x)
aperm(a = "x",perm = )
aperm(a = df,perm = )
#aperm.default
aperm.default(a = ,perm = ,resize = )
aperm.default(a = x,perm = ,resize = )
aperm.default(a = x,perm = x,resize = )
aperm.default(a = x,perm = x,resize = x)
aperm.default(a = "x",perm = x,resize = x)
aperm.default(a = df,perm = x,resize = x)
#anyDuplicated.default
anyDuplicated.default(x = ,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = ,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = )
anyDuplicated.default(x = x,incomparables = x,fromLast = x)
anyDuplicated.default(x = "x",incomparables = x,fromLast = x)
anyDuplicated.default(x = df,incomparables = x,fromLast = x)

#anyDuplicated.matrix
anyDuplicated.matrix(x = ,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = ,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = ,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = )
anyDuplicated.matrix(x = x,incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = "x",incomparables = x,MARGIN = x,fromLast = x)
anyDuplicated.matrix(x = df,incomparables = x,MARGIN = x,fromLast = x)
#anyNA
anyNA(x = ,recursive = )
anyNA(x =x ,recursive = )
anyNA(x = x,recursive = x)
anyNA(x = "x",recursive = )
anyNA(x = df,recursive = )
#anyNA.data.frame
anyNA.data.frame(x = ,recursive = )
anyNA.data.frame(x = x,recursive = )
anyNA.data.frame(x = x,recursive = x)
anyNA.data.frame(x = "x",recursive = )
anyNA.data.frame(x = df,recursive = )
#anyNA.numeric_version
anyNA.numeric_version(x = ,recursive= )
anyNA.numeric_version(x = x,recursive= )
anyNA.numeric_version(x = x,recursive= x)
anyNA.numeric_version(x = "x",recursive= )
anyNA.numeric_version(x = df,recursive= )
#anyNA.POSIXlt
anyNA.POSIXlt(x = ,recursive = )  
anyNA.POSIXlt(x = x,recursive = )
anyNA.POSIXlt(x = x,recursive = x)
anyNA.POSIXlt(x = "x",recursive = )
anyNA.POSIXlt(x = df,recursive = )
#aperm
aperm(a = ,perm = )
originaldf <- matrix(1:12, nrow = 3, ncol = 4)
change <- c(2)
originaldf <- aperm(originaldf,change)
#aperm.default
aperm.default(a = ,perm = ,resize = )
aperm.default(a = x,perm = ,resize = )
aperm.default(a = x,perm = x,resize = )
aperm.default(a = x,perm = x,resize = x)
aperm.default(a = "x",perm = x,resize = x)
aperm.default(a = df,perm = x,resize = x)


#aperm.table
aperm.table(a = ,perm = ,resize =,keep.class = )
aperm.table(a = x,perm = ,resize =,keep.class = )
aperm.table(a = x,perm = x,resize =,keep.class = )
aperm.table(a = x,perm = x,resize = x,keep.class = )
aperm.table(a = x,perm = x,resize = x,keep.class = x)

#append
append(x = ,values = ,after = )
append(x = x,values = ,after = )
append(x = x,values = x,after = )
append(x = x,values = x,after = x)
append(x = "x",values = x,after = x)
append(x = df,values = x,after = x)
aptest <- c(1,2,3,4)
appendValue <- 5
  base::append(aptest,after = ６,value = appendValue)
#apply
applyTest <- c(1,2,3,5)
  base::apply(applyTest,c(4,6),FUN = mean)
apply(X = ,MARGIN = ,FUN = ,simplify = )
apply(X = x,MARGIN = ,FUN = ,simplify = )
apply(X = x,MARGIN = x,FUN = ,simplify = )
apply(X = x,MARGIN = x,FUN = ,simplify = x)
apply(X = "x",MARGIN = x,FUN = x,simplify = x)  
apply(X = df,MARGIN = x,FUN = x,simplify = x)
#Arg
Arg(z = )
Arg(z = x)
Arg(z = "x")
Arg(z = df)

#args
args(f1)
args(name = )
args(name = x)
args(name = "x")
args(name = df)
#array
array(1:13,dim = 3:4,dimnames = list(c("a","b","c"),c("d","e","f","g")))
array(data = ,dim = ,dimnames = )
array(data = x,dim = ,dimnames = )
array(data = x,dim = x,dimnames = )
array(data = x,dim = x,dimnames = x)
  array(data = "x",dim = x,dimnames = x)
array(data = df,dim = x,dimnames = x)



  array(1:13,dimnames = list(c("x","y")))

#arrayInd
arrayInd(1:12,dim = c(3,4))
arrayInd(1:12,dim = c(3,4),dims = c(3,4))
arrayInd(ind = ,.dim = ,.dimnames = ,useNames = )
arrayInd(ind = x,.dim = ,.dimnames = ,useNames = )
arrayInd(ind = x,.dim = x,.dimnames = ,useNames = )
arrayInd(ind = x,.dim = x,.dimnames = x,useNames = )
arrayInd(ind = x,.dim = x,.dimnames = x,useNames = x)
arrayInd(ind = "x",.dim = x,.dimnames = x,useNames = x)
arrayInd(ind = df,.dim = x,.dimnames = x,useNames = x)

#as.array
as.array(x = )
as.array(x = x)
as.array(x = "x")
as.array(x = df)
arrayTest <- matrix(1:12, nrow = 3, ncol = 4)
#as.array.default
as.array.default(x = )
as.array.default(x = x)
as.array.default(x = x)
as.array.default(x = "x")
as.array.default(x = df)
#as.call
as.call(x = )
as.call(x = x)
as.call(x = "x")
as.call(x = df)
#as.character
as.character(x = )
as.character(x = x)
as.character(x = "x")
as.character(x = df)
#as.character.condition
as.character.condition(x = )
as.character.condition(x = x)
as.character.condition(x = "x")
as.character.condition(x = df)
#as.character.Date 
as.character.Date(x = )
as.character.Date(x = x)
as.character.Date(x = "x")
as.character.Date(x = df)
#as.character.default
as.character.default(x = )
as.character.default(x = x)
as.character.default(x = "x")
as.character.default(x = df)
#as.character.error 
as.character.error(x = )
as.character.error(x = x)
as.character.error(x = "x")
as.character.error(x = df)
#as.character.factor 
as.character.factor(x = )
as.character.factor(x = x)
as.character.factor(x = "x")
as.character.factor(x = df)
#as.character.hexmode 
as.character.hexmode(x = )
as.character.hexmode(x = x)
as.character.hexmode(x = "x")
as.character.hexmode(x = df)
#as.character.numeric_version
as.character.numeric_version(x = )
as.character.numeric_version(x = x)
as.character.numeric_version(x = "x")
as.character.numeric_version(x = df)
#as.character.octmode
as.character.octmode(x = )
as.character.octmode(x = x)
as.character.octmode(x = "x")
as.character.octmode(x = df)
#as.character.POSIXt
as.character.POSIXt(x = )
as.character.POSIXt(x = x)
as.character.POSIXt(x = "x")
as.character.POSIXt(x = df)
#as.character.srcref
as.character.srcref(x = ,useSource = ,to = )
as.character.srcref(x = x,useSource = ,to = )
as.character.srcref(x = x,useSource = x,to = )
as.character.srcref(x = x,useSource = x,to = x)
as.character.srcref(x = x,useSource = x,to = x)
as.character.srcref(x = "x",useSource = x,to = x)
as.character.srcref(x = df,useSource = x,to = x)
#as.complex
as.complex(x = )
as.complex(x = x)
as.complex(x = "x")
as.complex(x = df)
#as.data.frame
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9)
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
as.data.frame.array(x = x,row.names = ,optional = )
as.data.frame.array(x = x,row.names = x,optional = )
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = "x",row.names = x,optional = x)
as.data.frame.array(x = df,row.names = x,optional = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = "x",row.names = x,optional = x)
as.data.frame.AsIs(x = df,row.names = x,optional = x)
#as.data.frame.character 
as.data.frame.character(x = ,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = "x",stringsAsFactors = x)
as.data.frame.character(x = df,stringsAsFactors = x)
#as.data.frame.complex
as.data.frame.complex(x = ,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = "x",row.names = x,optional = x,nm = )
as.data.frame.complex(x = df,row.names = x,optional = x,nm = )
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
as.data.frame.data.frame(x = x,row.names = )
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = "x",row.names = x)
as.data.frame.data.frame(x = df,row.names = x)
#as.data.frame.Date 
as.data.frame.Date(x = ,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = x )
as.data.frame.Date(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.Date(x = df,row.names = x,optional = x,nm = x)
#as.call
as.call(x = )
as.call(x = x)
as.call(x = "x")
as.call(x = df)

#as.character
as.character(x = )
as.character(x = x)
as.character(x = "x")
as.character(x = df)
#as.character.condition
as.character.condition(x = )
as.character.condition(x = x)
as.character.condition(x = "x")
as.character.condition(x = df)
#as.character.Date 
as.character.Date(x = )
as.character.Date(x = x)
as.character.Date(x = "x")
as.character.Date(x = df)
#as.character.default
as.character.default(x = )
as.character.default(x = x)
as.character.default(x = "x")
as.character.default(x = df)
#as.character.error 
as.character.error(x = )
as.character.error(x = x)
as.character.error(x = "x")
as.character.error(x = df)
#as.character.factor 
as.character.factor(x = )
as.character.factor(x = x)
as.character.factor(x = "x")
as.character.factor(x = df)
#as.character.hexmode 
as.character.hexmode(x = )
as.character.hexmode(x = x)
as.character.hexmode(x = "x")
as.character.hexmode(x = df)
#as.character.numeric_version
as.character.numeric_version(x = )
as.character.numeric_version(x = x)
as.character.numeric_version(x = "x")
as.character.numeric_version(x = df)
#as.character.octmode
as.character.octmode(x = )
as.character.octmode(x = x)
as.character.octmode(x = "x")
as.character.octmode(x = df)
#as.character.POSIXt
as.character.POSIXt(x = )
as.character.POSIXt(x = x)
as.character.POSIXt(x = "x")
as.character.POSIXt(x = df)
#as.character.srcref
as.character.srcref(x = ,useSource = ,to = )
as.character.srcref(x = x,useSource = ,to = )
as.character.srcref(x = x,useSource = x,to = )
as.character.srcref(x = x,useSource = x,to = x)
as.character.srcref(x = x,useSource = x,to = x)
as.character.srcref(x = "x",useSource = x,to = x)
as.character.srcref(x = df,useSource = x,to = x)
#as.complex
as.complex(x = )
as.complex(x = x)
as.complex(x = "x")
as.complex(x = df)
#as.data

#as.character.condition
as.character.condition(x = )
as.character.condition(x = x)
as.character.condition(x = "x")
as.character.condition(x = df)
#as.character.Date 
as.character.Date(x = )
as.character.Date(x = x)
as.character.Date(x = "x")
as.character.Date(x = df)

#as.character.default
as.character.default(x = )
as.character.default(x = x)
as.character.default(x = "x")
as.character.default(x = df)
#as.character.error 
as.character.error(x = )
as.character.error(x = x)
as.character.error(x = "x")
as.character.error(x = df)
#as.character.factor 
as.character.factor(x = )
as.character.factor(x = x)
as.character.factor(x = "x")
as.character.factor(x = df)
#as.character.hexmode 
as.character.hexmode(x = )
as.character.hexmode(x = x)
as.character.hexmode(x = "x")
as.character.hexmode(x = df)
#as.character.numeric_version
as.character.numeric_version(x = )
as.character.numeric_version(x = x)
as.character.numeric_version(x = "x")
as.character.numeric_version(x = df)
#as.character.octmode
as.character.octmode(x = )
as.character.octmode(x = x)
as.character.octmode(x = "x")
as.character.octmode(x = df)
#as.character.POSIXt
as.character.POSIXt(x = )
as.character.POSIXt(x = x)
as.character.POSIXt(x = "x")
as.character.POSIXt(x = df)
#as.character.srcref
as.character.srcref(x = ,useSource = ,to = )
as.character.srcref(x = x,useSource = ,to = )
as.character.srcref(x = x,useSource = x,to = )
as.character.srcref(x = x,useSource = x,to = x)
as.character.srcref(x = x,useSource = x,to = x)
as.character.srcref(x = "x",useSource = x,to = x)
as.character.srcref(x = df,useSource = x,to = x)
#as.complex
as.complex(x = )
as.complex(x = x)
as.complex(x = "x")
as.complex(x = df)
#as.data.frame
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9)
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
as.data.frame.array(x = x,row.names = ,optional = )
as.data.frame.array(x = x,row.names = x,optional = )
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = "x",row.names = x,optional = x)
as.data.frame.array(x = df,row.names = x,optional = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = "x",row.names = x,optional = x)
as.data.frame.AsIs(x = df,row.names = x,optional = x)
#as.data.frame.character 
as.data.frame.character(x = ,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = "x",stringsAsFactors = x)
as.data.frame.character(x = df,stringsAsFactors = x)
#as.data.frame.complex
as.data.frame.complex(x = ,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = "x",row.names = x,optional = x,nm = )
as.data.frame.complex(x = df,row.names = x,optional = x,nm = )
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
as.data.frame.data.frame(x = x,row.names = )
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = "x",row.names = x)
as.data.frame.data.frame(x = df,row.names = x)
#as.character.POSIXt
as.character.POSIXt(x = )
as.character.POSIXt(x = x)
as.character.POSIXt(x = "x")
as.character.POSIXt(x = df)
#as.character.srcref
as.character.srcref(x = ,useSource = ,to = )
as.character.srcref(x = x,useSource = ,to = )
as.character.srcref(x = x,useSource = x,to = )
as.character.srcref(x = x,useSource = x,to = x)
as.character.srcref(x = "x",useSource = x,to = x)
as.character.srcref(x = df,useSource = x,to = x)
#as.complex
as.complex(x = )
as.complex(x = x)
as.complex(x = "x")
as.complex(x = df)
#as.data.frame
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9)
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
as.data.frame.array(x = x,row.names = ,optional = )
as.data.frame.array(x = x,row.names = x,optional = )
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = "x",row.names = x,optional = x)
as.data.frame.array(x = df,row.names = x,optional = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = "x",row.names = x,optional = x)
as.data.frame.AsIs(x = df,row.names = x,optional = x)
#as.data.frame.character 
as
#as.data.frame
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9)
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
as.data.frame.array(x = x,row.names = ,optional = )
as.data.frame.array(x = x,row.names = x,optional = )
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = "x",row.names = x,optional = x)
as.data.frame.array(x = df,row.names = x,optional = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)

#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = "x",row.names = x,optional = x)
as.data.frame.AsIs(x = df,row.names = x,optional = x)
#as.data.frame.character 
as.data.frame.character(x = ,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = "x",stringsAsFactors = x)
as.data.frame.character(x = df,stringsAsFactors = x)
#as.data.frame.complex
as.data.frame.complex(x = ,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = "x",row.names = x,optional = x,nm = )
as.data.frame.complex(x = df,row.names = x,optional = x,nm = )
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
as.data.frame.data.frame(x = x,row.names = )
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = "x",row.names = x)
as.data.frame.data.frame(x = df,row.names = x)
#as.data.frame.Date 
as.data.frame.Date(x = ,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = x )
as.data.frame.Date(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.Date(x = df,row.names = x,optional = x,nm = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9 )
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
as.data.frame.data.frame(x = x,row.names = )
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = "x",row.names = x)
as.data.frame.data.frame(x = df,row.names = x)
#as.data.frame.Date 
as.data.frame.Date(x = ,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = x )
as.data.frame.Date(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.Date(x = df,row.names = x,optional = x,nm = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9)
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
as.data.frame.array(x = x,row.names = ,optional = )
as.data.frame.array(x = x,row.names = x,optional = )
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = "x",row.names = x,optional = x)
as.data.frame.array(x = df,row.names = x,optional = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)

#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = "x",row.names = x,optional = x)
as.data.frame.AsIs(x = df,row.names = x,optional = x)
#as.data.frame.character 
as.data.frame.character(x = ,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = "x",stringsAsFactors = x)
as.data.frame.character(x = df,stringsAsFactors = x)
#as.data.frame.complex
as.data.frame.complex(x = ,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = "x",row.names = x,optional = x,nm = )
as.data.frame.complex(x = df,row.names = x,optional = x,nm = )
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
as.data.frame.data.frame(x = x,row.names = )
aas.data.frame.data.frame(x = x,row.names =)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = "x",row.names = x)
as.data.frame.data.frame(x = df,row.names = x)
#as.data.frame.Date 
as.data.frame.Date(x = ,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = x )
as.data.frame.Date(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.Date(x = df,row.names = x,optional = x,nm = x)

#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9)
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
as.data.frame.array(x = x,row.names = ,optional = )
as.data.frame.array(x = x,row.names = x,optional = )
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = "x",row.names = x,optional = x)
as.data.frame.array(x = df,row.names = x,optional = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)

#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = "x",row.names = x,optional = x)
as.data.frame.AsIs(x = df,row.names = x,optional = x)
#as.data.frame.character 
as.data.frame.character(x = ,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = "x",stringsAsFactors = x)
as.data.frame.character(x = df,stringsAsFactors = x)
#as.data.frame.complex
as.data.frame.complex(x = ,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = "x",row.names = x,optional = x,nm = )
as.data.frame.complex(x = df,row.names = x,optional = x,nm = )
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
as.data.frame.data.frame(x = x,row.names = )
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = "x",row.names = x)
as.data.frame.data.frame(x = df,row.names = x)
#as.data.frame.Date 
as.data.frame.Date(x = ,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = x )
as.data.frame.Date(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.Date(x = df,row.names = x,optional = x,nm = x)

#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9)
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
as.data.frame.array(x = x,row.names = ,optional = )
as.data.frame.array(x = x,row.names = x,optional = )
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = "x",row.names = x,optional = x)
as.data.frame.array(x = df,row.names = x,optional = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)

#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = "x",row.names = x,optional = x)
as.data.frame.AsIs(x = df,row.names = x,optional = x)
#as.data.frame.character 
as.data.frame.character(x = ,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = "x",stringsAsFactors = x)
as.data.frame.character(x = df,stringsAsFactors = x)
#as.data.frame.complex
as.data.frame.complex(x = ,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = "x",row.names = x,optional = x,nm = )
as.data.frame.complex(x = df,row.names = x,optional = x,nm = )
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
as.data.frame.data.frame(x = x,row.names = )
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = "x",row.names = x)
as.data.frame.data.frame(x = df,row.names = x)
#as.data.frame.Date 
as.data.frame.Date(x = ,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = x )
as.data.frame.Date(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.Date(x = df,row.names = x,optional = x,nm = x)

#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)
list <- list(a = 1:3, b = 4:9)
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
as.data.frame.array(x = x,row.names = ,optional = )
as.data.frame.array(x = x,row.names = x,optional = )
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = x,row.names = x,optional = x)
as.data.frame.array(x = "x",row.names = x,optional = x)
as.data.frame.array(x = df,row.names = x,optional = x)
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
as.data.frame(x = x,row.names = ,optional = )
as.data.frame(x = x,row.names = x,optional = )
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = x,row.names = x,optional = x)
as.data.frame(x = "x",row.names = x,optional = x)
as.data.frame(x = df,row.names = x,optional = x)

#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = ,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = )
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = x,row.names = x,optional = x)
as.data.frame.AsIs(x = "x",row.names = x,optional = x)
as.data.frame.AsIs(x = df,row.names = x,optional = x)
#as.data.frame.character 
as.data.frame.character(x = ,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = )
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = x,stringsAsFactors = x)
as.data.frame.character(x = "x",stringsAsFactors = x)
as.data.frame.character(x = df,stringsAsFactors = x)
#as.data.frame.complex
as.data.frame.complex(x = ,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = ,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = ,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = x,row.names = x,optional = x,nm = )
as.data.frame.complex(x = "x",row.names = x,optional = x,nm = )
as.data.frame.complex(x = df,row.names = x,optional = x,nm = )
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
as.data.frame.data.frame(x = x,row.names = )
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = x,row.names = x)
as.data.frame.data.frame(x = "x",row.names = x)
as.data.frame.data.frame(x = df,row.names = x)
#as.data.frame.Date 
as.data.frame.Date(x = ,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = ,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = ,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = )
as.data.frame.Date(x = x,row.names = x,optional = x,nm = x )
as.data.frame.Date(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.Date(x = df,row.names = x,optional = x,nm = x)
#as.data.frame.default
as.data.frame.default(x = )
as.data.frame.default(x = x)
as.data.frame.default(x = "x")
as.data.frame.default(x = df)

#as.data.frame.difftime
as.data.frame.difftime(x = ,row.names = ,optional = ,nm = )
as.data.frame.difftime(x = x,row.names = ,optional = ,nm = )
as.data.frame.difftime(x = x,row.names = x,optional = ,nm = )
as.data.frame.difftime(x = x,row.names = x,optional = x,nm = )
as.data.frame.difftime(x = x,row.names = x,optional = x,nm = )
as.data.frame.difftime(x = "x",row.names = x,optional = x,nm = )
as.data.frame.difftime(x = df,row.names = x,optional = x,nm = )
#as.data.frame.factor
as.data.frame.factor(x = ,row.names = ,optional = ,nm = )
as.data.frame.factor(x = x,row.names = ,optional = ,nm = )
as.data.frame.factor(x = x,row.names = x,optional = ,nm = )
as.data.frame.factor(x = x,row.names = x,optional = x,nm = )
as.data.frame.factor(x = x,row.names = x,optional = x,nm = )
as.data.frame.factor(x = "x",row.names = x,optional = x,nm = )
as.data.frame.factor(x = df,row.names = x,optional = x,nm = )
#as.data.frame.integer
as.data.frame.integer(x = ,row.names = ,optional = ,nm = )
as.data.frame.integer(x = x,row.names = ,optional = ,nm = )
as.data.frame.integer(x = x,row.names = x,optional = ,nm = )
as.data.frame.integer(x = x,row.names = x,optional = x,nm = )
as.data.frame.integer(x = x,row.names = x,optional = x,nm = )
as.data.frame.integer(x = "x",row.names = x,optional = x,nm = )
as.data.frame.integer(x = df,row.names = x,optional = x,nm = )
#as.data.frame.list
as.data.frame.list(x = ,row.names = ,optional = ,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = ,optional = ,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = ,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = "x",row.names = x,optional = x,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = df,row.names = x,optional = x,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
#as.data.frame.logical
as.data.frame.logical(x = ,row.names = ,optional = ,nm = )
as.data.frame.logical(x = x,row.names = ,optional = ,nm = )
as.data.frame.logical(x = x,row.names = x,optional = ,nm = )
as.data.frame.logical(x = x,row.names = x,optional = x,nm = )
as.data.frame.logical(x = x,row.names = x,optional = x,nm = )
as.data.frame.logical(x = "x",row.names = x,optional = x,nm = )
as.data.frame.logical(x = df,row.names = x,optional = x,nm = )
#as.data.frame.matrix
as.data.frame.matrix(x = ,row.names = ,optional = ,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = ,optional = ,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = x,optional = ,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = x,optional = x,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = x,optional = x,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = "x",row.names = x,optional = x,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = df,row.names = x,optional = x,make.names = ,stringsAsFactors = )
#as.data.frame.model.matrix
as.data.frame.model.matrix(x = ,row.names = ,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = ,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = "x",row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = df,row.names = x,optional = x,make.names = )
#as.data.frame.noquote
as.data.frame.noquote(x = ,row.names = ,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = ,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = x,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = x,nm = )
as.data.frame.noquote(x = "x",row.names = x,optional = x,nm = )
as.data.frame.noquote(x = df,row.names = x,optional = x,nm = )
#as.data.frame.numeric
as.data.frame.numeric(x = ,row.names = ,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = ,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric(x = "x",row.names = x,optional = x,nm = )
as.data.frame.numeric(x = df,row.names = x,optional = x,nm = )
#as.data.frame.numeric_version
as.data.frame.numeric_version(x = ,row.names = ,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = ,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric_version(x = "x",row.names = x,optional = x,nm = )
as.data.frame.numeric_version(x = df,row.names = x,optional = x,nm = )
#as.data.frame.ordered
as.data.frame.ordered(x = ,row.names = ,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = ,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = x,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = x,nm = )
as.data.frame.ordered(x = "x",row.names = x,optional = x,nm = )
as.data.frame.ordered(x = df,row.names = x,optional = x,nm = )
#as.data.frame.POSIXct
as.data.frame.POSIXct(x = ,row.names = ,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = ,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXct(x = "x",row.names = x,optional = x,nm = )
as.data.frame.POSIXct(x = df,row.names = x,optional = x,nm = )
#as.data.frame.POSIXlt
as.data.frame.POSIXlt(x = ,row.names = ,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = ,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXlt(x = "x",row.names = x,optional = x,nm = )
as.data.frame.POSIXlt(x = df,row.names = x,optional = x,nm = )
#as.data.frame.raw
as.data.frame.raw(x = ,row.names = ,optional = ,nm = )
as.data.frame.raw(x = x,row.names = ,optional = ,nm = )
as.data.frame.raw(x = x,row.names = x,optional = ,nm = )
as.data.frame.raw(x = x,row.names = x,optional = x,nm = )
as.data.frame.raw(x = x,row.names = x,optional = x,nm = )
as.data.frame.raw(x = "x",row.names = x,optional = x,nm = )
as.data.frame.raw(x = df,row.names = x,optional = x,nm = )
#as.data.frame.table
as.data.frame.table(x = ,row.names = ,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = ,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = "x",row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = df,row.names = x,optional = x,responseName = ,stringsAsFactors = )
#as.data.frame.ts
as.data.frame.ts(x = ,row.names = ,optional = ,nm = )
as.data.frame.ts(x = x,row.names = ,optional = ,nm = )
as.data.frame.ts(x = x,row.names = x,optional = ,nm = )
as.data.frame.ts(x = x,row.names = x,optional = x,nm = )
as.data.frame.ts(x = x,row.names = x,optional = x,nm = )
as.data.frame.ts(x = "x",row.names = x,optional = x,nm = )
as.data.frame.ts(x = df,row.names = x,optional = x,nm = )
#as.data.frame.vector
as.data.frame.vector(x = ,row.names = ,optional = ,nm = )
as.data.frame.vector(x = x,row.names = ,optional = ,nm = )
as.data.frame.vector(x = x,row.names = x,optional = ,nm = )
as.data.frame.vector(x = x,row.names = x,optional = x,nm = )
as.data.frame.vector(x = x,row.names = x,optional = x,nm = )
as.data.frame.vector(x = "x",row.names = x,optional = x,nm = )
as.data.frame.vector(x = df,row.names = x,optional = x,nm = )
#as.Date 
as.Date(x = ,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = "x",format = ,tz = )
as.Date(x = df,format = ,tz = )

#as.Date.character
as.Date.character(x = ,format = ,tryFormats = ,optional = )
as.Date.character(x = x,format = ,tryFormats = ,optional = )
as.Date.character(x = x,format = x,tryFormats = ,optional = )
as.Date.character(x = x,format = x,tryFormats = x,optional = )
as.Date.character(x = x,format = x,tryFormats = x,optional = x)
as.Date.character(x = "x",format = x,tryFormats = x,optional = x)
as.Date.character(x = df,format = x,tryFormats = x,optional = x)

#as.data.frame.factor
as.data.frame.factor(x = ,row.names = ,optional = ,nm = )
as.data.frame.factor(x = x,row.names = ,optional = ,nm = )
as.data.frame.factor(x = x,row.names = x,optional = ,nm = )
as.data.frame.factor(x = x,row.names = x,optional = x,nm = )
as.data.frame.factor(x = x,row.names = x,optional = x,nm = x)
as.data.frame.factor(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.factor(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.integer
as.data.frame.integer(x = ,row.names = ,optional = ,nm = )
as.data.frame.integer(x = x,row.names = ,optional = ,nm = )
as.data.frame.integer(x = x,row.names = x,optional = ,nm = )
as.data.frame.integer(x = x,row.names = x,optional = x,nm = )
as.data.frame.integer(x = x,row.names = x,optional = x,nm = x)
as.data.frame.integer(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.integer(x = df,row.names = x,optional = x,nm = x)
#as.data.frame.list
as.data.frame.list(x = ,row.names = ,optional = ,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = ,optional = ,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = ,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = x,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = x,col.names = x,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = x,col.names = x,fix.empty.names = x,check.names = ,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = x,col.names = x,fix.empty.names = x,check.names = x,stringsAsFactors = )
as.data.frame.list(x = x,row.names = x,optional = x,cut.names = x,col.names = x,fix.empty.names = x,check.names = x,stringsAsFactors = x)
as.data.frame.list(x = "x",row.names = x,optional = x,cut.names = x,col.names = x,fix.empty.names = x,check.names = x,stringsAsFactors = x)
as.data.frame.list(x = "x",row.names = x,optional = x,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
as.data.frame.list(x = df,row.names = x,optional = x,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )

#as.data.frame.logical
as.data.frame.logical(x = ,row.names = ,optional = ,nm = )
as.data.frame.logical(x = x,row.names = ,optional = ,nm = )
as.data.frame.logical(x = x,row.names = x,optional = ,nm = )
as.data.frame.logical(x = x,row.names = x,optional = x,nm = )
as.data.frame.logical(x = x,row.names = x,optional = x,nm = x)
as.data.frame.logical(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.logical(x = df,row.names = x,optional = x,nm = x)
#as.data.frame.matrix
as.data.frame.matrix(x = ,row.names = ,optional = ,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = ,optional = ,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = x,optional = ,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = x,optional = x,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = x,optional = x,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = x,optional = x,make.names = x,stringsAsFactors = )
as.data.frame.matrix(x = x,row.names = x,optional = x,make.names = x,stringsAsFactors = x)
as.data.frame.matrix(x = x,row.names = x,optional = x,make.names = x,stringsAsFactors = x)
as.data.frame.matrix(x = x,row.names = x,optional = x,make.names = x,stringsAsFactors = x)
as.data.frame.matrix(x = "x",row.names = x,optional = x,make.names = x,stringsAsFactors = x)
as.data.frame.matrix(x = "x",row.names = x,optional = x,make.names = ,stringsAsFactors = )
as.data.frame.matrix(x = df,row.names = x,optional = x,make.names = ,stringsAsFactors = )

#as.data.frame.model.matrix
as.data.frame.model.matrix(x = ,row.names = ,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = ,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = "x",row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = "x",row.names = x,optional = x,make.names=)
as.data.frame.model.matrix(x = df,row.names = x,optional = x,make.names=x)

#as.data.frame.model.matrix
as.data.frame.model.matrix(x = ,row.names = ,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = ,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = ,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = x,row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = "x",row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = "x",row.names = x,optional = x,make.names = )
as.data.frame.model.matrix(x = df,row.names = x,optional = x,make.names = )

#as.data.frame.noquote
as.data.frame.noquote(x = ,row.names = ,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = ,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = x,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = x,nm = )
as.data.frame.noquote(x = "x",row.names = x,optional = x,nm = )
as.data.frame.noquote(x = df,row.names = x,optional = x,nm = )

#as.data.frame.numeric
as.data.frame.numeric(x = ,row.names = ,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = ,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric(x = "x",row.names = x,optional = x,nm = )
as.data.frame.numeric(x = df,row.names = x,optional = x,nm = )

#as.data.frame.noquote
as.data.frame.noquote(x = ,row.names = ,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = ,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = ,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = x,nm = )
as.data.frame.noquote(x = x,row.names = x,optional = x,nm = )
as.data.frame.noquote(x = "x",row.names = x,optional = x,nm = )
as.data.frame.noquote(x = df,row.names = x,optional = x,nm = )

#as.data.frame.numeric_version
as.data.frame.numeric_version(x = ,row.names = ,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = ,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric_version(x = "x",row.names = x,optional = x,nm = )
as.data.frame.numeric_version(x = df,row.names = x,optional = x,nm = )

#as.data.frame.ordered
as.data.frame.ordered(x = ,row.names = ,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = ,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = x,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = x,nm = )
as.data.frame.ordered(x = "x",row.names = x,optional = x,nm = )
as.data.frame.ordered(x = df,row.names = x,optional = x,nm = )

#as.data.frame.POSIXct
as.data.frame.POSIXct(x = ,row.names = ,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = ,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = x,nm = x)
as.data.frame.POSIXct(x = "x",row.names = x,optional = x,nm = )
as.data.frame.POSIXct(x = df,row.names = x,optional = x,nm = )
#as.data.frame.numeric
as.data.frame.numeric(x = ,row.names = ,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = ,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = ,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric(x = x,row.names = x,optional = x,nm = x)
as.data.frame.numeric(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.numeric(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.POSIXlt
as.data.frame.POSIXlt(x = ,row.names = ,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = ,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = x,nm = x)
as.data.frame.POSIXlt(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.POSIXlt(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.raw
as.data.frame.raw(x = ,row.names = ,optional = ,nm = )
as.data.frame.raw(x = x,row.names = ,optional = ,nm = )
as.data.frame.raw(x = x,row.names = x,optional = ,nm = )
as.data.frame.raw(x = x,row.names = x,optional = x,nm = )
as.data.frame.raw(x = x,row.names = x,optional = x,nm = x)
as.data.frame.raw(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.raw(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.table
as.data.frame.table(x = ,row.names = ,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = ,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = "x",row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = "x",row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = df,row.names = x,optional = x,responseName = ,stringsAsFactors = )

#as.data.frame.ts
as.data.frame.ts(x = ,row.names = ,optional = ,nm = )
as.data.frame.ts(x = x,row.names = ,optional = ,nm = )
as.data.frame.ts(x = x,row.names = x,optional = ,nm = )
as.data.frame.ts(x = x,row.names = x,optional = x,nm = )
as.data.frame.ts(x = x,row.names = x,optional = x,nm = x)
as.data.frame.ts(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.ts(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.vector
as.data.frame.vector(x = ,row.names = ,optional = ,nm = )
as.data.frame.vector(x = x,row.names = ,optional = ,nm = )
as.data.frame.vector(x = x,row.names = x,optional = ,nm = )
as.data.frame.vector(x = x,row.names = x,optional = x,nm = )
as.data.frame.vector(x = x,row.names = x,optional = x,nm = x)
as.data.frame.vector(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.vector(x = df,row.names = x,optional = x,nm = x)

#as.Date 
as.Date(x = ,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = "x",format = ,tz = )
as.Date(x = df,format = ,tz = )

#as.Date.character
as.Date.character(x = ,format = ,tryFormats = ,optional = )
as.Date.character(x = x,format = ,tryFormats = ,optional = )
as.Date.character(x = x,format = x,tryFormats = ,optional = )
as.Date.character(x = x,format = x,tryFormats = x,optional = )
as.Date.character(x = x,format = x,tryFormats = x,optional = x)
as.Date.character(x = "x",format = x,tryFormats = x,optional = x)
as.Date.character(x = df,format = x,tryFormats = x,optional = x)
#as.data.frame.numeric_version
as.data.frame.numeric_version(x = ,row.names = ,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = ,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = ,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = x,nm = )
as.data.frame.numeric_version(x = x,row.names = x,optional = x,nm = x)
as.data.frame.numeric_version(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.numeric_version(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.ordered
as.data.frame.ordered(x = ,row.names = ,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = ,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = x,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = x,nm = x)
as.data.frame.ordered(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.ordered(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.POSIXct
as.data.frame.POSIXct(x = ,row.names = ,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = ,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = x,nm = x)
as.data.frame.POSIXct(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.POSIXct(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.POSIXlt
as.data.frame.POSIXlt(x = ,row.names = ,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = ,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = x,nm = x)
as.data.frame.POSIXlt(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.POSIXlt(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.raw
as.data.frame.raw(x = ,row.names = ,optional = ,nm = )
as.data.frame.raw(x = x,row.names = ,optional = ,nm = )
as.data.frame.raw(x = x,row.names = x,optional = ,nm = )
as.data.frame.raw(x = x,row.names = x,optional = x,nm = )
as.data.frame.raw(x = x,row.names = x,optional = x,nm = x)
as.data.frame.raw(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.raw(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.table
as.data.frame.table(x = ,row.names = ,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = ,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = "x",row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = "x",row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = df,row.names = x,optional = x,responseName = ,stringsAsFactors = )

#as.data.frame.ts
as.data.frame.ts(x = ,row.names = ,optional = ,nm = )
as.data.frame.ts(x = x,row.names = ,optional = ,nm = )
as.data.frame.ts(x = x,row.names = x,optional = ,nm = )
as.data.frame.ts(x = x,row.names = x,optional = x,nm = )
as.data.frame.ts(x = x,row.names = x,optional = x,nm = x)
as.data.frame.ts(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.ts(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.vector
as.data.frame.vector(x = ,row.names = ,optional = ,nm = )
as.data.frame.vector(x = x,row.names = ,optional = ,nm = )
as.data.frame.vector(x = x,row.names = x,optional = ,nm = )
as.data.frame.vector(x = x,row.names = x,optional = x,nm = )
as.data.frame.vector(x = x,row.names = x,optional = x,nm = x)
as.data.frame.vector(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.vector(x = df,row.names = x,optional = x,nm = x)

#as.Date 
as.Date(x = ,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = "x",format = ,tz = )
as.Date(x = df,format = ,tz = )

#as.Date.character
as.Date.character(x = ,format = ,tryFormats = ,optional = )
as.Date.character(x = x,format = ,tryFormats = ,optional = )
as.Date.character(x = x,format = x,tryFormats = ,optional = )
as.Date.character(x = x,format = x,tryFormats = x,optional = )
as.Date.character(x = x,format = x,tryFormats = x,optional = x)
as.Date.character(x = "x",format = x,tryFormats = x,optional = x)
as.Date.character(x = df,format = x,tryFormats = x,optional = x)

#as.data.frame.ordered
as.data.frame.ordered(x = ,row.names = ,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = ,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = ,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = x,nm = )
as.data.frame.ordered(x = x,row.names = x,optional = x,nm = x)
as.data.frame.ordered(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.ordered(x = df,row.names = x,optional = x,nm = x)
#as.data.frame.POSIXct
as.data.frame.POSIXct(x = ,row.names = ,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = ,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = ,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXct(x = x,row.names = x,optional = x,nm = x)
as.data.frame.POSIXct(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.POSIXct(x = df,row.names = x,optional = x,nm = x)
#as.data.frame.POSIXlt
as.data.frame.POSIXlt(x = ,row.names = ,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = ,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = ,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = x,nm = )
as.data.frame.POSIXlt(x = x,row.names = x,optional = x,nm = x)
as.data.frame.POSIXlt(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.POSIXlt(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.raw
as.data.frame.raw(x = ,row.names = ,optional = ,nm = )
as.data.frame.raw(x = x,row.names = ,optional = ,nm = )
as.data.frame.raw(x = x,row.names = x,optional = ,nm = )
as.data.frame.raw(x = x,row.names = x,optional = x,nm = )
as.data.frame.raw(x = x,row.names = x,optional = x,nm = x)
as.data.frame.raw(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.raw(x = df,row.names = x,optional = x,nm = x)

#as.data.frame.table
as.data.frame.table(x = ,row.names = ,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = ,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = ,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = ,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = )
as.data.frame.table(x = x,row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = "x",row.names = x,optional = x,responseName = x,stringsAsFactors = x)
as.data.frame.table(x = df,row.names = x,optional = x,responseName = x,stringsAsFactors = x)

#as.data.frame.ts
as.data.frame.ts(x = ,row.names = ,optional = ,nm = )
as.data.frame.ts(x = x,row.names = ,optional = ,nm = )
as.data.frame.ts(x = x,row.names = x,optional = ,nm = )
as.data.frame.ts(x = x,row.names = x,optional = x,nm = )
as.data.frame.ts(x = x,row.names = x,optional = x,nm = x)
as.data.frame.ts(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.ts(x = df,row.names = x,optional = x,nm = x)
#as.data.frame.vector
as.data.frame.vector(x = ,row.names = ,optional = ,nm = )
as.data.frame.vector(x = x,row.names = ,optional = ,nm = )
as.data.frame.vector(x = x,row.names = x,optional = ,nm = )
as.data.frame.vector(x = x,row.names = x,optional = x,nm = )
as.data.frame.vector(x = x,row.names = x,optional = x,nm = x)
as.data.frame.vector(x = "x",row.names = x,optional = x,nm = x)
as.data.frame.vector(x = df,row.names = x,optional = x,nm = x)
#as.Date 
as.Date(x = ,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = x,tz = )
as.Date(x = x,format = x,tz = x)
as.Date(x = "x",format = x,tz = x)
as.Date(x = df,format = x,tz = x)

#as.Date.character
as.Date.character(x = ,format = ,tz = )
as.Date.character(x = x,format = ,tz = )
as.Date.character(x = x,format = x,tz = )
as.Date.character(x = x,format = x,tz = x)
as.Date.character(x = "x",format = x,tz = x)
as.Date.character(x = df,format = x,tz = x)
#as.Date.default
as.Date.default(x = ,format = ,tz = )
as.Date.default(x = x,format = ,tz = )
as.Date.default(x = x,format = x,tz = )
as.Date.default(x = x,format = x,tz = x)
as.Date.default(x = "x",format = x,tz = x)
as.Date.default(x = df,format = x,tz = x)
#as.Date.factor
as.Date.factor(x = )
as.Date.factor(x = x)
as.Date.factor(x = "x")
as.Date.factor(x = df)
#as.Date.numeric
as.Date.numeric(x = ,origin = )
as.Date.numeric(x = x,origin = )
as.Date.numeric(x = x,origin = x)
as.Date.numeric(x = "x",origin = x)
as.Date.numeric(x = df,origin = x)
#as.Date.POSIXct
as.Date.POSIXct(x = ,tz = )
as.Date.POSIXct(x = x,tz = )
as.Date.POSIXct(x = x,tz = x)
as.Date.POSIXct(x = "x",tz = x)
as.Date.POSIXct(x = df,tz = x)

#as.Date.POSIXlt
as.Date.POSIXlt(x = ,tz = )
as.Date.POSIXlt(x = x,tz = )
as.Date.POSIXlt(x = x,tz = x)
as.Date.POSIXlt(x = "x",tz = x)
as.Date.POSIXlt(x = df,tz = x)
#as.difftime 
as.difftime(x = ,units = )
as.difftime(x = x,units = )
as.difftime(x = x,units = x)
as.difftime(x = "x",units = x)
as.difftime(x = df,units = x)
#as.double 
as.double(x = )
as.double(x = x)
as.double(x = "x")
as.double(x = df)

#as.double.difftime
as.double.difftime(x = ,units = )
as.double.difftime(x = x,units = )
as.double.difftime(x = x,units = x)
as.double.difftime(x = "x",units = x)
as.double.difftime(x = df,units = x)
#as.double.POSIXlt
as.double.POSIXlt(x = )
as.double.POSIXlt(x = x)
as.double.POSIXlt(x = "x")
as.double.POSIXlt(x = df)
#as.environment 
as.environment(x = )
as.environment(x = x)
as.environment(x = "x")
as.environment(x = df)
#as.expression 
as.expression(x = )
as.expression(x = x)
as.expression(x = "x")
as.expression(x = df)
#as.expression.default
as.expression.default(x = )
as.expression.default(x = x)
as.expression.default(x = "x")
as.expression.default(x = df)
#as.factor
as.factor(x = )
as.factor(x = x)
as.factor(x = "x")
as.factor(x = df)
#as.function
as.function(x = )
as.function(x = x)
as.function(x = "x")
as.function(x = df)
#as.function.default
as.function.default(x = )
as.function.default(x = x)
as.function.default(x = "x")
as.function.default(x = df)
#as.hexmode
as.hexmode(x = )
as.hexmode(x = x)
as.hexmode(x = "x")
as.hexmode(x = df)
#as.integer
as.integer(x = )
as.integer(x = x)
as.integer(x = "x")
as.integer(x = df)
#as.list
as.list(x = )
as.list(x = x)
as.list(x = "x")
as.list(x = df)

#as.list.data.frame
as.list.data.frame(x = ,row.names = ,optional = )
as.list.data.frame(x = x,row.names = ,optional = )
as.list.data.frame(x = x,row.names = x,optional = x)
as.list.data.frame(x = "x",row.names = x,optional = x)
as.list.data.frame(x = df,row.names = x,optional = x)

#as.environment 
as.environment(x = )
as.environment(x = x)
as.environment(x = "x")
as.environment(x = df)

#as.expression 
as.expression(x = )
as.expression(x = x)
as.expression(x = "x")
as.expression(x = df)
#as.expression.default
as.expression.default(x = )
as.expression.default(x = x)
as.expression.default(x = "x")
as.expression.default(x = df)
#as.factor
as.factor(x = )
as.factor(x = x)

#as.function
as.function(x = )
as.function(x = x)
as.function(x = "x")
as.function(x = df)
#as.function.default
as.function.default(x = )
as.function.default(x = x)
as.function.default(x = "x")
as.function.default(x = df)
#as.hexmode
as.hexmode(x = )
as.hexmode(x = x)
as.hexmode(x = "x")
as.hexmode(x = df)
#as.integer
as.integer(x = )
as.integer(x = x)

#as.list
as.list(x = )
as.list(x = x)
as.list(x = "x")
as.list(x = df)

#as.list.data.frame
as.list.data.frame(x = ,row.names = ,optional = )
as.list.data.frame(x = x,row.names = ,optional = )
as.list.data.frame(x = x,row.names = x,optional = x)
as.list.data.frame(x = "x",row.names = x,optional = x)
as.list.data.frame(x = df,row.names = x,optional = x)

#as.list.Date
as.list.Date(x = )
as.list.Date(x = x)
as.list.Date(x = "x")
as.list.Date(x = df)
#as.list.default
as.list.default(x = )
as.list.default(x = x)
as.list.default(x = "x")
as.list.default(x = df)
#as.list.difftime
as.list.difftime(x = )
as.list.difftime(x = x)
as.list.difftime(x = "x")
as.list.difftime(x = df)
#as.list.environment
as.list.environment(x = )
as.list.environment(x = x)
as.list.environment(x = "x")
as.list.environment(x = df)
#as.list.factor
as.list.factor(x = )
as.list.factor(x = x)
as.list.factor(x = "x")
as.list.factor(x = df)
#as.list.function
as.list.function(x = )
as.list.function(x = x)
as.list.function(x = "x")
as.list.function(x = df)
#as.list.numeric_version
as.list.numeric_version(x = )
as.list.numeric_version(x = x)
as.list.numeric_version(x = "x")
as.list.numeric_version(x = df)

#as.list.POSIXct
as.list.POSIXct(x = )
as.list.POSIXct(x = x)
as.list.POSIXct(x = "x")
as.list.POSIXct(x = df)
#as.list.POSIXlt
as.list.POSIXlt(x = )
as.list.POSIXlt(x = x)
as.list.POSIXlt(x = "x")
as.list.POSIXlt(x = df)
#as.logical
as.logical(x = )
as.logical(x = x)
as.logical(x = "x")
as.logical(x = df)
#as.logical.factor
as.logical.factor(x = )
as.logical.factor(x = x)
as.logical.factor(x = "x")
as.logical.factor(x = df)
#as.matrix
  as.matrix(x = x)
  as.matrix(x = "x")
  as.matrix(x = df)
#as.function.default
as.function.default(x = )
as.function.default(x = ,envir = )
as.function.default(x = x,envir = )
as.function.default(x = "x",envir = x)
as.function.default(x = df,envir = x)
as.function.default(x = df,envir = )

#as.hexmode
as.hexmode(x = )
as.hexmode(x = x)
as.hexmode(x = "x")
as.hexmode(x = df)
#as.integer
as.integer(x = )
as.integer(x =x )
as.integer(x = "x")
as.integer(x = df)

#as.list
as.list(x = )
as.list(x =x )
as.list(x = "x")
as.list(x = df)

#as.list.data.frame
as.list.data.frame(x = ,row.names = ,optional = )
as.list.data.frame(x = x,row.names = ,optional = )
as.list.data.frame(x = x,row.names = x,optional = x)
as.list.data.frame(x = "x",row.names = x,optional = x)
as.list.data.frame(x = df,row.names = x,optional = x)
as.list.data.frame(x = )
#as.list.Date
as.list.Date(x = )
as.list.Date(x = x)
as.list.Date(x = "x")
as.list.Date(x = df)

#as.list.default
as.list.default(x = )
as.list.default(x = x)
as.list.default(x = "x")
as.list.default(x = df)

#as.list.difftime
as.list.difftime(x = )
as.list.difftime(x =x )
as.list.difftime(x = "x")
as.list.difftime(x = df)
#as.list.environment
as.list.environment(x = )
as.list.environment(x = ,all.names = ,sorted = )
as.list.environment(x = x,all.names = ,sorted = )
as.list.environment(x = x,all.names = x,sorted = )
as.list.environment(x = x,all.names = x,sorted = x)
as.list.environment(x = "x",all.names = x,sorted = x)
as.list.environment(x = df,all.names = x,sorted = x)

as.list.environment(x = "x",all.names = ,sorted = )


#as.list.factor
as.list.factor(x = )
as.list.factor(x = x)
as.list.factor(x = "x")
as.list.factor(x = df)

#as.list.function
as.list.function(x = )
as.list.function(x = x)
as.list.function(x = "x")
as.list.function(x = df)
#as.list.numeric_version
as.list.numeric_version(x = )
as.list.numeric_version(x = x)
as.list.numeric_version(x = "x")
as.list.numeric_version(x = df)

#as.list.POSIXct
as.list.POSIXct(x = )
as.list.POSIXct(x = x)
as.list.POSIXct(x = "x")
as.list.POSIXct(x = df)
#as.list.POSIXlt
as.list.POSIXlt(x = )
as.list.POSIXlt(x = x)
as.list.POSIXlt(x = "x")
as.list.POSIXlt(x = df)
#as.logical
as.logical(x = )
as.logical(x = x)
as.logical(x = "x")
as.logical(x = df)
#as.logical.factor
as.logical.factor(x = )
as.logical.factor(x = x)
as.logical.factor(x = "x")
as.logical.factor(x = df)
#as.matrix
as.matrix(x = )
as.matrix(x = x)
as.matrix(x = "x")
as.matrix(x = df)
#as.matrix.data.frame
as.matrix.data.frame(x = ,row.names = )
as.matrix.data.frame(x = x,row.names = )
as.matrix.data.frame(x = x,row.names = x)
as.matrix.data.frame(x = "x",row.names = x)
as.matrix.data.frame(x = df,row.names = x)
#as.matrix.default
as.matrix.default(x = )
as.matrix.default(x = x)
as.matrix.default(x = "x")
as.matrix.default(x = df)
#as.matrix.noquote
as.matrix.noquote(x = )
as.matrix.noquote(x = x)
as.matrix.noquote(x = "x")
as.matrix.noquote(x = df)
#as.matrix.POSIXlt
as.matrix.POSIXlt(x = )
as.matrix.POSIXlt(x = x)
as.matrix.POSIXlt(x = "x")
as.matrix.POSIXlt(x = df)
#as.name
as.name(x = )
as.name(x = x)
as.name(x = "x")
as.name(x = df)
#as.null
as.null(x = )
as.null(x = x)
as.null(x = "x")
as.null(x = df)
#as.null.default
as.null.default(x = )
as.null.default(x = x)
as.null.default(x = "x")
as.null.default(x = df)
#as.numeric
as.numeric(x = )
as.numeric(x = x)
as.numeric(x = "x")
as.numeric(x = df)
#as.numeric_version
as.numeric_version(x = )
as.numeric_version(x = x)
as.numeric_version(x = "x")
as.numeric_version(x = df)
#as.octmode
as.octmode(x = )
as.octmode(x = x)
as.octmode(x = "x")
as.octmode(x = df)
#as.ordered
as.ordered(x = )
as.ordered(x = x)
as.ordered(x = "x")
as.ordered(x = df)
#as.package_version
as.package_version(x = )
as.package_version(x = x)
as.package_version(x = "x")
as.package_version(x = df)
#as.pairlist
as.pairlist(x = )
as.pairlist(x = x)
as.pairlist(x = "x")
as.pairlist(x = df)
#as.POSIXct
as.POSIXct(x = ,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = x,format = ,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.Date
as.POSIXct.Date(x = ,tz = )
as.POSIXct.Date(x = x,tz = )
as.POSIXct.Date(x = x,tz = x)
as.POSIXct.Date(x = "x",tz = x)
as.POSIXct.Date(x = df,tz = x)
#as.POSIXct.default
as.POSIXct.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = ,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.numeric
as.POSIXct.numeric(x = ,tz = )
as.POSIXct.numeric(x = x,tz = )
as.POSIXct.numeric(x = x,tz = x)
as.POSIXct.numeric(x = "x",tz = x)
as.POSIXct.numeric(x = df,tz = x)
#as.POSIXct.POSIXlt
as.POSIXct.POSIXlt(x = ,tz = )
as.POSIXct.POSIXlt(x = x,tz = )
as.POSIXct.POSIXlt(x = x,tz = x)
as.POSIXct.POSIXlt(x = "x",tz = x)
as.POSIXct.POSIXlt(x = df,tz = x)
#as.POSIXlt
as.POSIXlt(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = x,format = ,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.character
as.POSIXlt.character(x = ,tz = )
as.POSIXlt.character(x = x,tz = )
as.POSIXlt.character(x = x,tz = x)
as.POSIXlt.character(x = "x",tz = x)
as.POSIXlt.character(x = df,tz = x)
#as.POSIXlt.Date
as.POSIXlt.Date(x = ,tz = )
as.POSIXlt.Date(x = x,tz = )
as.POSIXlt.Date(x = x,tz = x)
as.POSIXlt.Date(x = "x",tz = x)
as.POSIXlt.Date(x = df,tz = x)
#as.POSIXlt.default
as.POSIXlt.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.factor
as.POSIXlt.factor(x = ,tz = )
as.POSIXlt.factor(x = x,tz = )
as.POSIXlt.factor(x = x,tz = x)
as.POSIXlt.factor(x = "x",tz = x)
as.POSIXlt.factor(x = df,tz = x)
#as.POSIXlt.numeric
as.POSIXlt.numeric(x = ,tz = )
as.POSIXlt.numeric(x = x,tz = )
as.POSIXlt.numeric(x = x,tz = x)
as.POSIXlt.numeric(x = "x",tz = x)
as.POSIXlt.numeric(x = df,tz = x)
#as.POSIXlt.POSIXct
as.POSIXlt.POSIXct(x = ,tz = )
as.POSIXlt.POSIXct(x = x,tz = )
as.POSIXlt.POSIXct(x = x,tz = x)
as.POSIXlt.POSIXct(x = "x",tz = x)
as.POSIXlt.POSIXct(x = df,tz = x)
#as.qr
as.qr(x = ,pivot = )
as.qr(x = x,pivot = )
as.qr(x = x,pivot = x)
as.qr(x = "x",pivot = x)
as.qr(x = df,pivot = x)
#as.raw
as.raw(x = )
as.raw(x = x)
as.raw(x = "x")
as.raw(x = df)
#as.single
as.single(x = )
as.single(x = x)
as.single(x = "x")
as.single(x = df)
#as.single.default
as.single.default(x = )
as.single.default(x = x)
as.single.default(x = "x")
as.single.default(x = df)
#as.symbol
as.symbol(x = )
as.symbol(x = x)
as.symbol(x = "x")
as.symbol(x = df)
#as.table
as.table(x = )
as.table(x = x)
as.table(x = "x")
as.table(x = df)
#as.table.default
as.table.default(x = )
as.table.default(x = x)
as.table.default(x = "x")
as.table.default(x = df)
#as.vector
as.vector(x = ,mode = )
as.vector(x = x,mode = )
as.vector(x = x,mode = x)
as.vector(x = "x",mode = x)
as.vector(x = df,mode = x)
#as.vector.factor
as.vector.factor(x = )
as.vector.factor(x = x)
as.vector.factor(x = "x")
as.vector.factor(x = df)

#asin
asin(x = )
asin(x = x)
asin(x = "x")
asin(x = df)

#as.data.frame.table
as.data.frame.table(x = )
as.data.frame.table(x = )
as.data.frame.table(x = )
#as.Date 
as.Date(x = ,format = ,tz = )
as.Date(x = x,format = ,tz = )
as.Date(x = x,format = x,tz = )
as.Date(x = x,format = x,tz = x)
as.Date(x = x,format = x,tz = x)
as.Date(x = "x",format = x,tz = x)
as.Date(x = df,format = x,tz = x)

#as.Date.character
as.Date.character(x = ,format = ,tz = )
as.Date.character(x = x,format = ,tz = )
as.Date.character(x = x,format = x,tz = )
as.Date.character(x = x,format = x,tz = x)
as.Date.character(x = x,format = x,tz = x)
as.Date.character(x = "x",format = x,tz = x)
as.Date.character(x = df,format = x,tz = x)
#as.Date.default
as.Date.default(x = ,format = ,tz = )
as.Date.default(x = x,format = ,tz = )
as.Date.default(x = x,format = x,tz = )
as.Date.default(x = x,format = x,tz = x)
as.Date.default(x = x,format = x,tz = x)
as.Date.default(x = "x",format = x,tz = x)
as.Date.default(x = df,format = x,tz = x)
#as.Date.factor
as.Date.factor(x = )
as.Date.factor(x = x)
as.Date.factor(x = "x")
as.Date.factor(x = df)
#as.Date.numeric
as.Date.numeric(x = ,origin = )
as.Date.numeric(x = x,origin = )
as.Date.numeric(x = x,origin = x)
as.Date.numeric(x = "x",origin = x)
as.Date.numeric(x = df,origin = x)
#as.Date.POSIXct
as.Date.POSIXct(x = ,tz = )
as.Date.POSIXct(x = x,tz = )
as.Date.POSIXct(x = x,tz = x)
as.Date.POSIXct(x = "x",tz = x)
as.Date.POSIXct(x = df,tz = x)

#as.Date.POSIXlt
as.Date.POSIXlt(x = ,tz = )
as.Date.POSIXlt(x = x,tz = )
as.Date.POSIXlt(x = x,tz = x)
as.Date.POSIXlt(x = "x",tz = x)
as.Date.POSIXlt(x = df,tz = x)
#as.difftime 
as.difftime(x = ,units = )
as.difftime(x = x,units = )
as.difftime(x = x,units = x)
as.difftime(x = "x",units = x)
as.difftime(x = df,units = x)
#as.double 
as.double(x = )
as.double(x = x)
as.double(x = "x")
as.double(x = df)

#as.double.difftime
as.double.difftime(x = ,units = )
as.double.difftime(x = x,units = )
as.double.difftime(x = x,units = x)
as.double.difftime(x = "x",units = x)
as.double.difftime(x = df,units = x)
#as.double.POSIXlt
as.double.POSIXlt(x = )
as.double.POSIXlt(x = x)
as.double.POSIXlt(x = "x")
as.double.POSIXlt(x = df)
#as.environment 
as.environment(x = )
as.environment(x = x)
as.environment(x = "x")
as.environment(x = df)
#as.expression 
as.expression(x = )
as.expression(x = x)
as.expression(x = "x")
as.expression(x = df)
#as.expression.default
as.expression.default(x = )
as.expression.default(x = x)
as.expression.default(x = "x")
as.expression.default(x = df)
#as.factor
as.factor(x = )
as.factor(x = x)
as.factor(x = "x")
as.factor(x = df)
#as.function
as.function(x = )
as.function(x = x)
as.function(x = "x")
as.function(x = df)
#as.function.default
as.function.default(x = )
as.function.default(x = x)
as.function.default(x = "x")
as.function.default(x = df)
#as.hexmode
as.hexmode(x = )
as.hexmode(x = x)
as.hexmode(x = "x")
as.hexmode(x = df)
#as.integer
as.integer(x = )
as.integer(x = x)
as.integer(x = "x")
as.integer(x = df)
#as.list
as.list(x = )
as.list(x = x)
as.list(x = "x")
as.list(x = df)

#as.list.data.frame
as.list.data.frame(x = ,row.names = ,optional = )
as.list.data.frame(x = x,row.names = ,optional = )
as.list.data.frame(x = x,row.names = x,optional = x)
as.list.data.frame(x = "x",row.names = x,optional = x)
as.list.data.frame(x = df,row.names = x,optional = x)

#as.list.Date
as.list.Date(x = )
as.list.Date(x = x)
as.list.Date(x = "x")
as.list.Date(x = df)
#as.list.default
as.list.default(x = )
as.list.default(x = x)
as.list.default(x = "x")
as.list.default(x = df)
#as.list.difftime
as.list.difftime(x = )
as.list.difftime(x = x)
as.list.difftime(x = "x")
as.list.difftime(x = df)
#as.list.environment
as.list.environment(x = )
as.list.environment(x = x)
as.list.environment(x = "x")
as.list.environment(x = df)
#as.list.factor
as.list.factor(x = )
as.list.factor(x = x)
as.list.factor(x = "x")
as.list.factor(x = df)
#as.list.function
as.list.function(x = )
as.list.function(x = x)
as.list.function(x = "x")
as.list.function(x = df)
#as.list.numeric_version
as.list.numeric_version(x = )
as.list.numeric_version(x = x)
as.list.numeric_version(x = "x")
as.list.numeric_version(x = df)

#as.list.POSIXct
as.list.POSIXct(x = )
as.list.POSIXct(x = x)
as.list.POSIXct(x = "x")
as.list.POSIXct(x = df)
#as.list.POSIXlt
as.list.POSIXlt(x = )
as.list.POSIXlt(x = x)
as.list.POSIXlt(x = "x")
as.list.POSIXlt(x = df)
#as.logical
as.logical(x = )
as.logical(x = x)
as.logical(x = "x")
as.logical(x = df)
#as.logical.factor
as.logical.factor(x = )
as.logical.factor(x = x)
as.logical.factor(x = "x")
as.logical.factor(x = df)
#as.matrix
as.matrix(x = )
as.matrix(x = x)
as.matrix(x = "x")
as.matrix(x = df)
#as.matrix.data.frame
as.matrix.data.frame(x = ,row.names = )
as.matrix.data.frame(x = x,row.names = )
as.matrix.data.frame(x = x,row.names = x)
as.matrix.data.frame(x = "x",row.names = x)
as.matrix.data.frame(x = df,row.names = x)
#as.matrix.default
as.matrix.default(x = )
as.matrix.default(x = x)
as.matrix.default(x = "x")
as.matrix.default(x = df)
#as.matrix.noquote
as.matrix.noquote(x = )
as.matrix.noquote(x = x)
as.matrix.noquote(x = "x")
as.matrix.noquote(x = df)
#as.matrix.POSIXlt
as.matrix.POSIXlt(x = )
as.matrix.POSIXlt(x = x)
as.matrix.POSIXlt(x = "x")
as.matrix.POSIXlt(x = df)
#as.name
as.name(x = )
as.name(x = x)
as.name(x = "x")
as.name(x = df)
#as.null
as.null(x = )
as.null(x = x)
as.null(x = "x")
as.null(x = df)
#as.null.default
as.null.default(x = )
as.null.default(x = x)
as.null.default(x = "x")
as.null.default(x = df)
#as.numeric
as.numeric(x = )
as.numeric(x = x)
as.numeric(x = "x")
as.numeric(x = df)
#as.numeric_version
as.numeric_version(x = )
as.numeric_version(x = x)
as.numeric_version(x = "x")
as.numeric_version(x = df)
#as.octmode
as.octmode(x = )
as.octmode(x = x)
as.octmode(x = "x")
as.octmode(x = df)
#as.ordered
as.ordered(x = )
as.ordered(x = x)
as.ordered(x = "x")
as.ordered(x = df)
#as.package_version
as.package_version(x = )
as.package_version(x = x)
as.package_version(x = "x")
as.package_version(x = df)
#as.pairlist
as.pairlist(x = )
as.pairlist(x = x)
as.pairlist(x = "x")
as.pairlist(x = df)
#as.POSIXct
as.POSIXct(x = ,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = x,format = ,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.Date
as.POSIXct.Date(x = ,tz = )
as.POSIXct.Date(x = x,tz = )
as.POSIXct.Date(x = x,tz = x)
as.POSIXct.Date(x = "x",tz = x)
as.POSIXct.Date(x = df,tz = x)
#as.POSIXct.default
as.POSIXct.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = ,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.numeric
as.POSIXct.numeric(x = ,tz = )
as.POSIXct.numeric(x = x,tz = )
as.POSIXct.numeric(x = x,tz = x)
as.POSIXct.numeric(x = "x",tz = x)
as.POSIXct.numeric(x = df,tz = x)
#as.POSIXct.POSIXlt
as.POSIXct.POSIXlt(x = ,tz = )
as.POSIXct.POSIXlt(x = x,tz = )
as.POSIXct.POSIXlt(x = x,tz = x)
as.POSIXct.POSIXlt(x = "x",tz = x)
as.POSIXct.POSIXlt(x = df,tz = x)
#as.POSIXlt
as.POSIXlt(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = x,format = ,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.character
as.POSIXlt.character(x = ,tz = )
as.POSIXlt.character(x = x,tz = )
as.POSIXlt.character(x = x,tz = x)
as.POSIXlt.character(x = "x",tz = x)
as.POSIXlt.character(x = df,tz = x)
#as.POSIXlt.Date
as.POSIXlt.Date(x = ,tz = )
as.POSIXlt.Date(x = x,tz = )
as.POSIXlt.Date(x = x,tz = x)
as.POSIXlt.Date(x = "x",tz = x)
as.POSIXlt.Date(x = df,tz = x)
#as.POSIXlt.default
as.POSIXlt.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.factor
as.POSIXlt.factor(x = ,tz = )
as.POSIXlt.factor(x = x,tz = )
as.POSIXlt.factor(x = x,tz = x)
as.POSIXlt.factor(x = "x",tz = x)
as.POSIXlt.factor(x = df,tz = x)
#as.POSIXlt.numeric
as.POSIXlt.numeric(x = ,tz = )
as.POSIXlt.numeric(x = x,tz = )
as.POSIXlt.numeric(x = x,tz = x)
as.POSIXlt.numeric(x = "x",tz = x)
as.POSIXlt.numeric(x = df,tz = x)
#as.POSIXlt.POSIXct
as.POSIXlt.POSIXct(x = ,tz = )
as.POSIXlt.POSIXct(x = x,tz = )
as.POSIXlt.POSIXct(x = x,tz = x)
as.POSIXlt.POSIXct(x = "x",tz = x)
as.POSIXlt.POSIXct(x = df,tz = x)
#as.qr
as.qr(x = ,pivot = )
as.qr(x = x,pivot = )
as.qr(x = x,pivot = x)
as.qr(x = "x",pivot = x)
as.qr(x = df,pivot = x)
#as.raw
as.raw(x = )
as.raw(x = x)
as.raw(x = "x")
as.raw(x = df)
#as.single
as.single(x = )
as.single(x = x)
as.single(x = "x")
as.single(x = df)
#as.single.default
as.single.default(x = )
as.single.default(x = x)
as.single.default(x = "x")
as.single.default(x = df)
#as.symbol
as.symbol(x = )
as.symbol(x = x)
as.symbol(x = "x")
as.symbol(x = df)
#as.table
as.table(x = )
as.table(x = x)
as.table(x = "x")
as.table(x = df)
#as.table.default
as.table.default(x = )
as.table.default(x = x)
as.table.default(x = "x")
as.table.default(x = df)
#as.vector
as.vector(x = ,mode = )
as.vector(x = x,mode = )
as.vector(x = x,mode = x)
as.vector(x = "x",mode = x)
as.vector(x = df,mode = x)
#as.vector.factor
as.vector.factor(x = )
as.vector.factor(x = x)
as.vector.factor(x = "x")
as.vector.factor(x = df)
#asin
asin(x = )
asin(x = x)
asin(x = "x")
asin(x = df)
#asinh
asinh(x = )
asinh(x = x)
asinh(x = "x")
asinh(x = df)
#atan
atan(x = )
atan(x = x)
atan(x = "x")
atan(x = df)
#atan2
atan2(y = ,x = )
atan2(y = y,x = )
atan2(y = y,x = x)
atan2(y = y,x = "x")
atan2(y = y,x = df)
#atanh
atanh(x = )
atanh(x = x)
atanh(x = "x")
atanh(x = df)
#attach
attach(pos = ,name = ,warn.conflicts = )
attach(pos = pos,name = ,warn.conflicts = )
attach(pos = pos,name = name,warn.conflicts = )
attach(pos = pos,name = name,warn.conflicts = warn.conflicts)
attach(pos = pos,name = name,warn.conflicts = "warn.conflicts")
attach(pos = pos,name = name,warn.conflicts = df)

#as.list.POSIXlt
as.list.POSIXlt(x = )
as.list.POSIXlt(x = x)
as.list.POSIXlt(x = "x")
as.list.POSIXlt(x = df)
#as.logical
as.logical(x = )
as.logical(x = x)
as.logical(x = "x")
as.logical(x = df)
#as.logical.factor
as.logical.factor(x = )
as.logical.factor(x = x)
as.logical.factor(x = "x")
as.logical.factor(x = df)
#as.matrix
as.matrix(x = )
as.matrix(x = x)
as.matrix(x = "x")
as.matrix(x = df)
#as.matrix.data.frame
as.matrix.data.frame(x = ,row.names = )
as.matrix.data.frame(x = x,row.names = )
as.matrix.data.frame(x = x,row.names = x)
as.matrix.data.frame(x = "x",row.names = x)
as.matrix.data.frame(x = df,row.names = x)
#as.matrix.default
as.matrix.default(x = )
as.matrix.default(x = x)
as.matrix.default(x = "x")
as.matrix.default(x = df)
#as.matrix.noquote
as.matrix.noquote(x = )
as.matrix.noquote(x = x)
as.matrix.noquote(x = "x")
as.matrix.noquote(x = df)
#as.matrix.POSIXlt
as.matrix.POSIXlt(x = )
as.matrix.POSIXlt(x = x)
as.matrix.POSIXlt(x = "x")
as.matrix.POSIXlt(x = df)
#as.name
as.name(x = )
as.name(x = x)
as.name(x = "x")
as.name(x = df)
#as.null
as.null(x = )
as.null(x = x)
as.null(x = "x")
as.null(x = df)
#as.null.default
as.null.default(x = )
as.null.default(x = x)
as.null.default(x = "x")
as.null.default(x = df)
#as.numeric
as.numeric(x = )
as.numeric(x = x)
as.numeric(x = "x")
as.numeric(x = df)

#as.POSIXct.POSIXlt
as.POSIXct.POSIXlt(x = ,tz = )
as.POSIXct.POSIXlt(x = x,tz = )
as.POSIXct.POSIXlt(x = x,tz = x)
as.POSIXct.POSIXlt(x = "x",tz = x)
as.POSIXct.POSIXlt(x = df,tz = x)
#as.POSIXlt
as.POSIXlt(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = x,format = ,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.character
as.POSIXlt.character(x = ,tz = )
as.POSIXlt.character(x = x,tz = )
as.POSIXlt.character(x = x,tz = x)
as.POSIXlt.character(x = "x",tz = x)
as.POSIXlt.character(x = df,tz = x)
#as.POSIXlt.Date
as.POSIXlt.Date(x = ,tz = )
as.POSIXlt.Date(x = x,tz = )
as.POSIXlt.Date(x = x,tz = x)
as.POSIXlt.Date(x = "x",tz = x)
as.POSIXlt.Date(x = df,tz = x)
#as.POSIXlt.default
as.POSIXlt.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.factor
as.POSIXlt.factor(x = ,tz = )
as.POSIXlt.factor(x = x,tz = )
as.POSIXlt.factor(x = x,tz = x)
as.POSIXlt.factor(x = "x",tz = x)
as.POSIXlt.factor(x = df,tz = x)
#as.POSIXlt.numeric
as.POSIXlt.numeric(x = ,tz = )
as.POSIXlt.numeric(x = x,tz = )
as.POSIXlt.numeric(x = x,tz = x)
as.POSIXlt.numeric(x = "x",tz = x)
as.POSIXlt.numeric(x = df,tz = x)
#as.POSIXlt.POSIXct
as.POSIXlt.POSIXct(x = ,tz = )
as.POSIXlt.POSIXct(x = x,tz = )
as.POSIXlt.POSIXct(x = x,tz = x)
as.POSIXlt.POSIXct(x = "x",tz = x)
as.POSIXlt.POSIXct(x = df,tz = x)
#as.qr
as.qr(x = ,pivot = )
as.qr(x = x,pivot = )
as.qr(x = x,pivot = x)
as.qr(x = "x",pivot = x)
as.qr(x = df,pivot = x)
#as.raw
as.raw(x = )
as.raw(x = x)
as.raw(x = "x")
as.raw(x = df)
#as.single
as.single(x = )
as.single(x = x)
as.single(x = "x")
as.single(x = df)
#as.single.default
as.single.default(x = )
as.single.default(x = x)
as.single.default(x = "x")
as.single.default(x = df)
#as.symbol
as.symbol(x = )
as.symbol(x = x)
as.symbol(x = "x")
as.symbol(x = df)
#as.table
as.table(x = )
as.table(x = x)
as.table(x = "x")
as.table(x = df)
#as.table.default
as.table.default(x = )
as.table.default(x = x)
as.table.default(x = "x")
as.table.default(x = df)
#as.vector
as.vector(x = ,mode = )
as.vector(x = x,mode = )
as.vector(x = x,mode = x)
as.vector(x = "x",mode = x)
as.vector(x = df,mode = x)
#as.vector.factor
as.vector.factor(x = )
as.vector.factor(x = x)
as.vector.factor(x = "x")
as.vector.factor(x = df)
#atan
atan(x = )
atan(x = x)
atan(x = "x")
atan(x = df)
#atan2
atan2(y = ,x = )
atan2(y = y,x = )
atan2(y = y,x = x)
atan2(y = y,x = "x")
atan2(y = y,x = df)
#atanh
atanh(x = )
atanh(x = x)
atanh(x = "x")
atanh(x = df)
#attach
attach(pos = ,name = ,warn.conflicts = )
attach(pos = pos,name = ,warn.conflicts = )
attach(pos = pos,name = name,warn.conflicts = )
attach(pos = pos,name = name,warn.conflicts = warn.conflicts)
attach(pos = pos,name = name,warn.conflicts = "warn.conflicts")
attach(pos = pos,name = name,warn.conflicts = df)

#as.Date.POSIXlt
as.Date.POSIXlt(x = ,tz = )
as.Date.POSIXlt(x = x,tz = )
as.Date.POSIXlt(x = x,tz = x)
as.Date.POSIXlt(x = "x",tz = x)
as.Date.POSIXlt(x = df,tz = x)
#as.difftime 
as.difftime(x = ,units = )
as.difftime(x = x,units = )
as.difftime(x = x,units = x)
as.difftime(x = "x",units = x)
as.difftime(x = df,units = x)
#as.double 
as.double(x = )
as.double(x = x)
as.double(x = "x")
as.double(x = df)
#as.double.difftime
as.double.difftime(x = ,units = )
as.double.difftime(x = x,units = )
as.double.difftime(x = x,units = x)
as.double.difftime(x = "x",units = x)
as.double.difftime(x = df,units = x)
#as.double.POSIXlt
as.double.POSIXlt(x = )
as.double.POSIXlt(x = x)
as.double.POSIXlt(x = "x")
as.double.POSIXlt(x = df)
#as.environment 
as.environment(x = )
as.environment(x = x)
as.environment(x = "x")
as.environment(x = df)
#as.expression 
as.expression(x = )
as.expression(x = x)
as.expression(x = "x")
as.expression(x = df)
#as.expression.default
as.expression.default(x = )
as.expression.default(x = x)
as.expression.default(x = "x")
as.expression.default(x = df)
#as.factor
as.factor(x = )
as.factor(x = x)
as.factor(x = "x")
as.factor(x = df)
#as.function
as.function(x = )
as.function(x = x)
as.function(x = "x")
as.function(x = df)
#as.hms 
as.hms(x = )
as.hms(x = x)
as.hms(x = "x")
as.hms(x = df)
#as.integer
as.integer(x = )
as.integer(x = x)
as.integer(x = "x")
as.integer(x = df)
#as.list
as.list(x = )
as.list(x = x)
as.list(x = "x")
as.list(x = df)
#as.list.data.frame
as.list.data.frame(x = ,row.names = )
as.list.data.frame(x = x,row.names = )
as.list.data.frame(x = x,row.names = x)
as.list.data.frame(x = "x",row.names = x)
as.list.data.frame(x = df,row.names = x)
#as.list.Date
as.list.Date(x = )
as.list.Date(x = x)
as.list.Date(x = "x")
as.list.Date(x = df)
#as.list.default
as.list.default(x = )
as.list.default(x = x)
as.list.default(x = "x")
as.list.default(x = df)
#as.list.difftime
as.list.difftime(x = )
as.list.difftime(x = x)
as.list.difftime(x = "x")
as.list.difftime(x = df)
#as.list.environment
as.list.environment(x = )
as.list.environment(x = x)
as.list.environment(x = "x")
as.list.environment(x = df)
#as.list.factor
as.list.factor(x = )
as.list.factor(x = x)
as.list.factor(x = "x")
as.list.factor(x = df)
#as.list.function
as.list.function(x = )
as.list.function(x = x)
as.list.function(x = "x")
as.list.function(x = df)
#as.list.numeric_version
as.list.numeric_version(x = )
as.list.numeric_version(x = x)
as.list.numeric_version(x = "x")
as.list.numeric_version(x = df)
#as.list.POSIXct
as.list.POSIXct(x = )
as.list.POSIXct(x = x)
as.list.POSIXct(x = "x")
as.list.POSIXct(x = df)
#as.list.POSIXlt
as.list.POSIXlt(x = )
as.list.POSIXlt(x = x)
as.list.POSIXlt(x = "x")
as.list.POSIXlt(x = df)
#as.logical
as.logical(x = )
as.logical(x = x)
as.logical(x = "x")
as.logical(x = df)
#as.logical.factor
as.logical.factor(x = )
as.logical.factor(x = x)
as.logical.factor(x = "x")
as.logical.factor(x = df)
#as.matrix
as.matrix(x = )
as.matrix(x = x)
as.matrix(x = "x")
as.matrix(x = df)
#as.matrix.data.frame
as.matrix.data.frame(x = ,row.names = )
as.matrix.data.frame(x = x,row.names = )
as.matrix.data.frame(x = x,row.names = x)
as.matrix.data.frame(x = "x",row.names = x)
as.matrix.data.frame(x = df,row.names = x)
#as.matrix.default
as.matrix.default(x = )
as.matrix.default(x = x)
as.matrix.default(x = "x")
as.matrix.default(x = df)
#as.matrix.noquote
as.matrix.noquote(x = )
as.matrix.noquote(x = x)
as.matrix.noquote(x = "x")
as.matrix.noquote(x = df)
#as.matrix.POSIXlt
as.matrix.POSIXlt(x = )
as.matrix.POSIXlt(x = x)
as.matrix.POSIXlt(x = "x")
as.matrix.POSIXlt(x = df)
#as.name
as.name(x = )
as.name(x = x)
as.name(x = "x")
as.name(x = df)
#as.null
as.null(x = )
as.null(x = x)
as.null(x = "x")
as.null(x = df)
#as.null.default
as.null.default(x = )
as.null.default(x = x)
as.null.default(x = "x")
as.null.default(x = df)
#as.numeric
as.numeric(x = )
as.numeric(x = x)
as.numeric(x = "x")
as.numeric(x = df)
#as.numeric_version
as.numeric_version(x = )
as.numeric_version(x = x)
as.numeric_version(x = "x")
as.numeric_version(x = df)
#as.octmode
as.octmode(x = )
as.octmode(x = x)
as.octmode(x = "x")
as.octmode(x = df)
#as.ordered
as.ordered(x = )
as.ordered(x = x)
as.ordered(x = "x")
as.ordered(x = df)
#as.matrix
as.matrix(x = x,rownames.force = )
as.matrix(x = x,rownames.force = x)
as.matrix(x = "x",rownames.force = x)
as.matrix(x = df,rownames.force = x)
as.matrix(x = )
#as.matrix.data.frame
as.matrix.data.frame(x = ,row.names = )
as.matrix.data.frame(x = x,rownames.force = )
as.matrix.data.frame(x = x,rownames.force =x )
as.matrix.data.frame(x = "x",rownames.force =x )
as.matrix.data.frame(x = df,rownames.force =x )
#as.matrix.default
as.matrix.default(x = )
as.matrix.default(x = x)
as.matrix.default(x = "x")
as.matrix.default(x = df)
#as.matrix.noquote
as.matrix.noquote(x = ,rownames.force = )
as.matrix.noquote(x = )
as.matrix.noquote(x = x)
as.matrix.noquote(x = "x")
as.matrix.noquote(x = df)

#as.matrix.POSIXlt
as.matrix.POSIXlt(x = )
as.matrix.POSIXlt(x = x)
as.matrix.POSIXlt(x = "x")
as.matrix.POSIXlt(x = df)
#as.name
as.name(x = )
as.name(x = x)
as.name(x = "x")
as.name(x = df)
#as.null
as.null(x = )
as.null(x = x)
as.null(x = "x")
as.null(x = df)
#as.null.default
as.null.default(x = )
as.null.default(x = x)
as.null.default(x = "x")
as.null.default(x = df)
#as.numeric
as.numeric(x = )
as.numeric(x = x)
as.numeric(x = "x")
as.numeric(x = df)
#as.numeric_version
as.numeric_version(x = )
as.numeric_version(x = x)
as.numeric_version(x = "x")
as.numeric_version(x = df)
#as.octmode
as.octmode(x = )
as.octmode(x = x)
as.octmode(x = "x")
as.octmode(x = df)
#as.ordered
as.ordered(x = )
as.ordered(x = x)
as.ordered(x = "x")
as.ordered(x = df)
#as.package_version
as.package_version(x = )
as.package_version(x = x)
as.package_version(x = "x")
as.package_version(x = df)
#as.pairlist
as.pairlist(x = )
as.pairlist(x = x)
as.pairlist(x = "x")
as.pairlist(x = df)
#as.POSIXct
as.POSIXct(x = ,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = x,format = ,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.Date
as.POSIXct.Date(x = ,tz = )
as.POSIXct.Date(x = x,tz = )
as.POSIXct.Date(x = x,tz = x)
as.POSIXct.Date(x = "x",tz = x)
as.POSIXct.Date(x = df,tz = x)
#as.POSIXct.default
as.POSIXct.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = ,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.numeric
as.POSIXct.numeric(x = ,tz = )
as.POSIXct.numeric(x = x,tz = )
as.POSIXct.numeric(x = x,tz = x)
as.POSIXct.numeric(x = "x",tz = x)
as.POSIXct.numeric(x = df,tz = x)
#as.POSIXct.POSIXlt
as.POSIXct.POSIXlt(x = ,tz = )
as.POSIXct.POSIXlt(x = x,tz = )
as.POSIXct.POSIXlt(x = x,tz = x)
as.POSIXct.POSIXlt(x = "x",tz = x)
as.POSIXct.POSIXlt(x = df,tz = x)
#as.POSIXlt
as.POSIXlt(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = x,format = ,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.character
as.POSIXlt.character(x = ,tz = )
as.POSIXlt.character(x = x,tz = )
as.POSIXlt.character(x = x,tz = x)
as.POSIXlt.character(x = "x",tz = x)
as.POSIXlt.character(x = df,tz = x)
#as.POSIXlt.Date
as.POSIXlt.Date(x = ,tz = )
as.POSIXlt.Date(x = x,tz = )
as.POSIXlt.Date(x = x,tz = x)
as.POSIXlt.Date(x = "x",tz = x)
as.POSIXlt.Date(x = df,tz = x)
#as.POSIXlt.default
as.POSIXlt.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = ,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.factor
as.POSIXlt.factor(x = ,tz = )
as.POSIXlt.factor(x = x,tz = )
as.POSIXlt.factor(x = x,tz = x)
as.POSIXlt.factor(x = "x",tz = x)
as.POSIXlt.factor(x = df,tz = x)
#as.POSIXlt.numeric
as.POSIXlt.numeric(x = ,tz = )
as.POSIXlt.numeric(x = x,tz = )
as.POSIXlt.numeric(x = x,tz = x)
as.POSIXlt.numeric(x = "x",tz = x)
as.POSIXlt.numeric(x = df,tz = x)
#as.POSIXlt.POSIXct
as.POSIXlt.POSIXct(x = ,tz = )
as.POSIXlt.POSIXct(x = x,tz = )
as.POSIXlt.POSIXct(x = x,tz = x)
as.POSIXlt.POSIXct(x = "x",tz = x)
as.POSIXlt.POSIXct(x = df,tz = x)
#as.qr
as.qr(x = ,pivot = )
as.qr(x = x,pivot = )
as.qr(x = x,pivot = x)
as.qr(x = "x",pivot = x)
as.qr(x = df,pivot = x)
#as.raw
as.raw(x = )
as.raw(x = x)
as.raw(x = "x")
as.raw(x = df)
#as.single
as.single(x = )
as.single(x = x)
as.single(x = "x")
as.single(x = df)
#as.single.default
as.single.default(x = )
as.single.default(x = x)
as.single.default(x = "x")
as.single.default(x = df)
#as.symbol
as.symbol(x = )
as.symbol(x = x)
as.symbol(x = "x")
as.symbol(x = df)
#as.table
as.table(x = )
as.table(x = x)
as.table(x = "x")
as.table(x = df)
#as.table.default
as.table.default(x = )
as.table.default(x = x)
as.table.default(x = "x")
as.table.default(x = df)
#as.vector
as.vector(x = ,mode = )
as.vector(x = x,mode = )
as.vector(x = x,mode = x)
as.vector(x = "x",mode = x)
as.vector(x = df,mode = x)
#as.vector.factor
as.vector.factor(x = )
as.vector.factor(x = x)
as.vector.factor(x = "x")
as.vector.factor(x = df)
#asin
asin(x = )
asin(x = x)
asin(x = "x")
asin(x = df)
#asinh
asinh(x = )
asinh(x = x)
asinh(x = "x")
asinh(x = df)
#asNamespace
asNamespace(ns = )
asNamespace(ns = ns)
asNamespace(ns = "ns")
asNamespace(ns = df)
#asplit
asplit(x = ,dims = ,drop = )
asplit(x = x,dims = ,drop = )
asplit(x = x,dims = x,drop = )
asplit(x = x,dims = x,drop = x)
asplit(x = x,dims = x,drop = "x")
asplit(x = x,dims = x,drop = df)
asplit(x = x,dims = "x",drop = df)
asplit(x = x,dims = df,drop = df)
asplit(x = "x",dims = df,drop = df)
asplit(x = df,dims = df,drop = df)
#asS3
asS3(object = ,class = )
asS3(object = object,class = )
asS3(object = object,class = "class")
asS3(object = object,class = df)
#asS4
asS4(object = ,class = )
asS4(object = object,class = )
asS4(object = object,class = "class")
asS4(object = object,class = df)
#assign
assign(x = ,value = ,pos = ,envir = ,inherits = )
assign(x = x,value = ,pos = ,envir = ,inherits = )
assign(x = x,value = value,pos = ,envir = ,inherits = )
assign(x = x,value = value,pos = pos,envir = ,inherits = )
assign(x = x,value = value,pos = pos,envir = envir,inherits = )
assign(x = x,value = value,pos = pos,envir = envir,inherits = inherits)
assign(x = "x",value = value,pos = pos,envir = envir,inherits = inherits)
assign(x = df,value = value,pos = pos,envir = envir,inherits = inherits)
assign(x = ,value = value,pos = pos,envir = envir,inherits = inherits)
assign(x = ,value = value,pos = pos,envir = envir,inherits = inherits)
assign(x = ,value = value,pos = pos,envir = envir,inherits = inherits)
assign(x = ,value = value,pos = pos,envir = envir,inherits = inherits)
#atan
atan(x = )
atan(x = x)
atan(x = "x")
atan(x = df)
#atan2
atan2(y = ,x = )
atan2(y = y,x = )
atan2(y = y,x = x)
atan2(y = y,x = "x")
atan2(y = y,x = df)
#atanh
atanh(x = )
atanh(x = x)
atanh(x = "x")
atanh(x = df)
#attach
attach(pos = ,name = ,warn.conflicts = )
attach(pos = pos,name = ,warn.conflicts = )
attach(pos = pos,name = name,warn.conflicts = )
attach(pos = pos,name = name,warn.conflicts = warn.conflicts)
attach(pos = pos,name = name,warn.conflicts = "warn.conflicts")
attach(pos = pos,name = name,warn.conflicts = df)

#as.Date.POSIXlt
as.Date.POSIXlt(x = ,tz = )
as.Date.POSIXlt(x = x,tz = )
as.Date.POSIXlt(x = x,tz = x)
as.Date.POSIXlt(x = "x",tz = x)
as.Date.POSIXlt(x = df,tz = x)
#as.difftime 
as.difftime(x = ,units = )
as.difftime(x = x,units = )
as.difftime(x = x,units = x)
as.difftime(x = "x",units = x)
as.difftime(x = df,units = x)
#as.double 
as.double(x = )
as.double(x = x)
as.double(x = "x")
as.double(x = df)
#as.double.difftime
as.double.difftime(x = ,units = )
as.double.difftime(x = x,units = )
as.double.difftime(x = x,units = x)
as.double.difftime(x = "x",units = x)
as.double.difftime(x = df,units = x)
#as.double.POSIXlt
as.double.POSIXlt(x = )
as.double.POSIXlt(x = x)
as.double.POSIXlt(x = "x")
as.double.POSIXlt(x = df)
#as.environment 
as.environment(x = )
as.environment(x = x)
as.environment(x = "x")
as.environment(x = df)
#as.expression 
as.expression(x = )
as.expression(x = x)
as.expression(x = "x")
as.expression(x = df)
#as.expression.default
as.expression.default(x = )
as.expression.default(x = x)
as.expression.default(x = "x")
as.expression.default(x = df)
#as.factor
as.factor(x = )
as.factor(x = x)
as.factor(x = "x")
as.factor(x = df)
#as.function
as.function(x = )
as.function(x = x)
as.function(x = "x")
as.function(x = df)
#as.hms 
as.hms(x = )
as.hms(x = x)
as.hms(x = "x")
as.hms(x = df)
#as.integer
as.integer(x = )
as.integer(x = x)
as.integer(x = "x")
as.integer(x = df)
#as.list
as.list(x = )
as.list(x = x)
as.list(x = "x")
as.list(x = df)
#as.list.data.frame
as.list.data.frame(x = ,row.names = )
as.list.data.frame(x = x,row.names = )
as.list.data.frame(x = x,row.names = x)
as.list.data.frame(x = "x",row.names = x)
as.list.data.frame(x = df,row.names = x)
#as.list.default
as.list.default(x = )
as.list.default(x = x)
as.list.default(x = "x")
as.list.default(x = df)
#as.list.difftime
as.list.difftime(x = )
as.list.difftime(x = x)
as.list.difftime(x = "x")
as.list.difftime(x = df)
#as.list.environment
as.list.environment(x = )
as.list.environment(x = x)
as.list.environment(x = "x")
as.list.environment(x = df)
#as.list.factor
as.list.factor(x = )
as.list.factor(x = x)
as.list.factor(x = "x")
as.list.factor(x = df)
#as.list.function
as.list.function(x = )
as.list.function(x = x)
as.list.function(x = "x")
as.list.function(x = df)
#as.list.numeric_version
as.list.numeric_version(x = )
as.list.numeric_version(x = x)
as.list.numeric_version(x = "x")
as.list.numeric_version(x = df)
#as.list.POSIXct
as.list.POSIXct(x = )
as.list.POSIXct(x = x)
as.list.POSIXct(x = "x")
as.list.POSIXct(x = df)
#as.list.POSIXlt
as.list.POSIXlt(x = )
as.list.POSIXlt(x = x)
as.list.POSIXlt(x = "x")
as.list.POSIXlt(x = df)
#as.logical
as.logical(x = )
as.logical(x = x)
as.logical(x = "x")
as.logical(x = df)
#as.logical.factor
as.logical.factor(x = )
as.logical.factor(x = x)
as.logical.factor(x = "x")
#as.name
as.name(x = )
as.name(x = x)
as.name(x = "x")
as.name(x = df)
#as.null
as.null(x = )
as.null(x = x)
as.null(x = "x")
as.null(x = df)
#as.null.default
as.null.default(x = )
as.null.default(x = x)
as.null.default(x = "x")
as.null.default(x = df)
#as.numeric
as.numeric(x = )
as.numeric(x = x)
as.numeric(x = "x")
as.numeric(x = df)
#as.numeric_version
as.numeric_version(x = )
as.numeric_version(x = x)
as.numeric_version(x = "x")
as.numeric_version(x = df)
#as.octmode
as.octmode(x = )
as.octmode(x = x)
as.octmode(x = "x")
as.octmode(x = df)
#as.ordered
as.ordered(x = )
as.ordered(x = x)
as.ordered(x = "x")
as.ordered(x = df)
#as.package_version
as.package_version(x = )
as.package_version(x = x)
as.package_version(x = "x")
as.package_version(x = df)
#as.pairlist
as.pairlist(x = )
as.pairlist(x = x)
as.pairlist(x = "x")
as.pairlist(x = df)
#as.POSIXct
as.POSIXct(x = ,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = x,format = ,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.Date
as.POSIXct.Date(x = ,tz = )
as.POSIXct.Date(x = x,tz = )
as.POSIXct.Date(x = x,tz = x)
as.POSIXct.Date(x = "x",tz = x)
as.POSIXct
as.name(x = )
as.name(x = x)
as.name(x = "x")
as.name(x = df)
#as.null
as.null(x = )
as.null(x = x)
as.null(x = "x")
as.null(x = df)
#as.null.default
as.null.default(x = )
as.null.default(x = x)
as.null.default(x = "x")
as.null.default(x = df)
#as.numeric
as.numeric(x = )
as.numeric(x = x)
as.numeric(x = "x")
as.numeric(x = df)
#as.numeric_version
as.numeric_version(x = )
as.numeric_version(x = x)
as.numeric_version(x = "x")
as.numeric_version(x = df)
#as.octmode
as.octmode(x = )
as.octmode(x = x) 
as.octmode(x = "x")
as.octmode(x = df)
#as.ordered
as.ordered(x = )
as.ordered(x = x)
as.ordered(x = "x")
as.ordered(x = df)
#as.package_version
as.package_version(x = )
as.package_version(x =x )
as.package_version(x = "x")
as.package_version(x = df)
#as.pairlist
as.pairlist(x = )
as.pairlist(x = x)
as.pairlist(x = "x")
as.pairlist(x = df)
#as.POSIXct
as.POSIXct(x = ,tz = ,format = ,tryFormats = )
as.POSIXct(x = x,tz = )
as.POSIXct(x = x,tz = x)
as.POSIXct(x = x,tz = x,format = )
as.POSIXct(x = x,tz = x,format = x)
as.POSIXct(x = x,tz = x,format = x,tryFormats = )
as.POSIXct(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.Date
as.POSIXct.Date(x = ,tz = )
as.POSIXct.Date(x = x)
as.POSIXct.Date(x = x,tz = )
as.POSIXct.Date(x = x,tz = x)
as.POSIXct.Date(x = "x",tz = x)
as.POSIXct.Date(x = df,tz = x)
#as.POSIXct.default
as.POSIXct.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXct.default(x = x,tz = )
as.POSIXct.default(x = x,tz = x)
as.POSIXct.default(x = x,tz = x,format = )
as.POSIXct.default(x = x,tz = x,format = x)
as.POSIXct.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXct.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXct.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXct.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXct.numeric
as.POSIXct.numeric(x = ,tz = )
as.POSIXct.numeric(x = x,tz = ,origin = )
as.POSIXct.numeric(x = x,tz = x,origin = )
as.POSIXct.numeric(x = "x",tz = x,origin = )
as.POSIXct.numeric(x = df,tz = x,origin = )

#as.POSIXct.POSIXlt
as.POSIXct.POSIXlt(x = ,tz = )
as.POSIXct.POSIXlt(x = x,tz = )
as.POSIXct.POSIXlt(x = x,tz = x)
as.POSIXct.POSIXlt(x = "x",tz = x)
as.POSIXct.POSIXlt(x = df,tz = x)
#as.POSIXlt
as.POSIXlt(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt(x = x,tz = )
as.POSIXlt(x = x,tz = x)
as.POSIXlt(x = x,tz = x,format = )
as.POSIXlt(x = x,tz = x,format = x)
as.POSIXlt(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.character
as.POSIXlt.character(x = ,tz = )
as.POSIXlt.Date(x = x)
as.POSIXlt.Date(x = x,tz = )
as.POSIXlt.Date(x = x,tz = x)
as.POSIXlt.Date(x = "x",tz = x)
as.POSIXlt.Date(x = df,tz = x)
#as.POSIXlt.Date
as.POSIXlt.Date(x = ,tz = )
as.POSIXlt.Date(x = x)
as.POSIXlt.Date(x = x,tz = )
as.POSIXlt.Date(x = x,tz = x)
as.POSIXlt.Date(x = "x",tz = x)
as.POSIXlt.Date(x = df,tz = x)

#as.POSIXlt.default
as.POSIXlt.default(x = ,tz = ,format = ,tryFormats = )
as.POSIXlt.default(x = ,tz = ,optional = )
as.POSIXlt.default(x = x,tz = )
as.POSIXlt.default(x = x,tz = x)
as.POSIXlt.default(x = x,tz = x,format = )
as.POSIXlt.default(x = x,tz = x,format = x)
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = )
as.POSIXlt.default(x = x,tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = "x",tz = x,format = x,tryFormats = x)
as.POSIXlt.default(x = df,tz = x,format = x,tryFormats = x)
#as.POSIXlt.factor
as.POSIXlt.factor(x = ,tz = )
as.POSIXlt.factor(x = x)
as.POSIXlt.factor(x = x,tz = )
as.POSIXlt.factor(x = x,tz = x)
as.POSIXlt.factor(x = "x",tz = x)
as.POSIXlt.factor(x = df,tz = x)
#as.POSIXlt.numeric
as.POSIXlt.numeric(x = ,tz = )
as.POSIXlt.numeric(x = x)
as.POSIXlt.numeric(x = x,tz = )
as.POSIXlt.numeric(x = x,tz = x)
as.POSIXlt.numeric(x = "x",tz = x)
as.POSIXlt.numeric(x = df,tz = x)
#as.POSIXlt.POSIXct
as.POSIXlt.POSIXct(x = ,tz = )
as.POSIXlt.POSIXct(x = x)
as.POSIXlt.POSIXct(x = x,tz = )
as.POSIXlt.POSIXct(x = x,tz = x)
as.POSIXlt.POSIXct(x = "x",tz = x)
as.POSIXlt.POSIXct(x = df,tz = x)
#as.qr
as.qr(x = ,pivot = )
as.qr(x = x)
as.qr(x = x,pivot = )
as.qr(x = x,pivot = x)
as.qr(x = "x",pivot = x)
as.qr(x = df,pivot = x)
#as.raw
as.raw(x = )
as.raw(x = x)
as.raw(x = "x")
as.raw(x = df)
#as
as(object = ,Class = ,strict = ,ext = )
as(object = x,Class = ,strict = ,ext = )
as(object = x,Class = Class,strict = ,ext = )
as(object = x,Class = Class,strict = strict,ext = )
as(object = x,Class = Class,strict = strict,ext = ext)
as(object = x,Class = x,strict = x,ext = x)
as(object = x,Class = df,strict = x,ext = x)
as(object = x,Class = ,strict = x,ext = x)
as(object = x,Class = ,strict = x,ext = x)
as(object = x,Class = ,strict = x,ext = x)
as(object = x,Class = ,strict = x,ext = x)


#as.POSIXlt.factor
as.POSIXlt.factor(x = ,tz = )
as.POSIXlt.factor(x = )
as.POSIXlt.factor(x = x)
as.POSIXlt.factor(x = x,tz = )
as.POSIXlt.factor(x = x,tz = x)
as.POSIXlt.factor(x = "x",tz = x)
as.POSIXlt.factor(x = df,tz = x)


#as.POSIXlt.numeric
as.POSIXlt.numeric(x = ,tz = )
as.POSIXlt.numeric(x = ,tz = ,origin = )
as.POSIXlt.numeric(x = x)
as.POSIXlt.numeric(x = x,tz = )
as.POSIXlt.numeric(x = x,tz = x)
as.POSIXlt.numeric(x = "x",tz = x)
as.POSIXlt.numeric(x = df,tz = x)

#as.POSIXlt.POSIXct
as.POSIXlt.POSIXct(x = ,tz = )
as.POSIXlt.POSIXct(x = x,tz = )
as.POSIXlt.POSIXct(x = x,tz = x)
as.POSIXlt.POSIXct(x = "x",tz = x)
as.POSIXlt.POSIXct(x = df,tz = x)
#as.qr
as.qr(x = ,pivot = )
as.qr(x = x)
as.qr(x = x,pivot = )
as.qr(x = x,pivot = x)
as.qr(x = "x",pivot = x)
as.qr(x = df,pivot = x)
#as.raw
as.raw(x = )
as.raw(x = )
as.raw(x = x)
as.raw(x = "x")
as.raw(x = df)
#as.single
as.single(x = )
as.single(x = x)
as.single(x = "x")
as.single(x = df)


#as.single.default
as.single.default(x = )
as.single.default(x = x)
as.single.default(x = "x")
as.single.default(x = df)


#as.symbol
as.symbol(x = )
as.symbol(x = x)
as.symbol(x = "x")
as.symbol(x = df)
#as.table
as.table(x = )
as.table(x = x)
as.table(x = "x")
as.table(x = df)
#as.table.default
as.table.default(x = )
as.table.default(x = x)
as.table.default(x = "x")
as.table.default(x = df)
#as.vector
as.vector(x = ,mode = )
as.vector(x = x,mode = )
as.vector(x = x,mode = x)
as.vector(x = "x",mode = x)
as.vector(x = df,mode = x)
#as.vector.factor
as.vector.factor(x = )
as.vector.factor(x = x,mode = )
as.vector.factor(x = x,mode = x)
as.vector.factor(x = "x",mode = x)
as.vector.factor(x = df,mode = x)

#asin
asin(x = )
asin(x = x)
asin(x = "x")
asin(x = df)
#asinh
asinh(x = )
asinh(x = x)
asinh(x = "x")
asinh(x = df)

#asNamespace
asNamespace(ns = )
asNamespace(ns = ,base.OK = )
asNamespace(ns = x)
asNamespace(ns = x,base.OK = )
asNamespace(ns = "x",base.OK = )
asNamespace(ns = df,base.OK = )
#asplit
asplit(x = ,dims = ,drop = )
asplit(x = ,MARGIN = )
asplit(x = x,dims = ,drop = )
asplit(x = x,dims = x,drop = )
asplit(x = x,dims = x,drop = x)
asplit(x = x,MARGIN = )
asplit(x = x,MARGIN = x)
asplit(x = x,MARGIN = x,drop = )
asplit(x = x,MARGIN = x,drop = x)
asplit(x = "x",MARGIN = x)
asplit(x = df,MARGIN = x)



#asS3
asS3(object = ,class = )
asS3(object = ,flag = ,complete = )
asS3(object = object,class = )
asS3(object = object,class = "class")
asS3(object = object,class = df)
asS3(object = x,flag = ,complete = )
asS3(object = x,flag = x,complete = )
asS3(object = x,flag = x,complete = x)
asS3(object = "x",flag = ,complete = x)
asS3(object = df,flag = ,complete = x)

#asS4
asS4(object = ,class = )
asS4(object = ,flag = ,complete = )
asS4(object = x,flag = ,complete = )
asS4(object = x,flag = x,complete = )
asS4(object = x,flag = x,complete = x)
asS4(object = "x",flag = ,complete = x)
asS4(object = df,flag = ,complete = x)

#assign
assign(x = ,value = ,pos = ,envir = ,inherits = )
assign(x = ,value = ,pos = ,envir = ,inherits = ,immediate = )
assign(x = x,value = ,pos = ,envir = ,inherits = ,immediate = )
assign(x = x,value = x,pos = ,envir = ,inherits = ,immediate = )
assign(x = x,value = x,pos = x,envir = ,inherits = ,immediate = )
assign(x = x,value = x,pos = x,envir = x,inherits = ,immediate = )
assign(x = x,value = x,pos = x,envir = x,inherits = x,immediate = )
assign(x = x,value = x,pos = x,envir = x,inherits = x,immediate = x)
assign(x = x,value = x,pos = x,envir = x,inherits = x,immediate = df)
assign(x = x,value = x,pos = x,envir = x,inherits = df,immediate = df)
assign(x = x,value = x,pos = x,envir = df,inherits = df,immediate = df)
assign(x = x,value = x,pos = df,envir = df,inherits = df,immediate = df)
assign(x = x,value = df,pos = df,envir = df,inherits = df,immediate = df)
assign(x = df,value = df,pos = df,envir = df,inherits = df,immediate = df)
assign(x = "df",value = df,pos = df,envir = df,inherits = df,immediate = df)


#atan
atan(x = )
atan(x = x)
atan(x = "x")
atan(x = df)
#atan2
atan2(y = ,x = )
atan2(y = x,x = )
atan2(y = x,x = x)
atan2(y = x,x = "x")
atan2(y = x,x = df)

#atanh
atanh(x = )
atanh(x = x)
atanh(x = "x")
atanh(x = df)
#attach
attach(pos = ,name = ,warn.conflicts = ,pos = ,name = ,warn.conflicts = )
attach(what = ,pos = ,name = ,warn.conflicts = )
attach(what = x,pos = ,name = ,warn.conflicts = )
attach(what = x,pos = x,name = ,warn.conflicts = )
attach(what = x,pos = x,name = x,warn.conflicts = )
attach(what = x,pos = x,name = x,warn.conflicts = x)
attach(what = x,pos = x,name = x,warn.conflicts = df)
attach(what = x,pos = x,name = df,warn.conflicts = df)
attach(what = x,pos = df,name = df,warn.conflicts = df)
attach(what = df,pos = df,name = df,warn.conflicts = df)
attach(what = "df",pos = df,name = df,warn.conflicts = df)
#attachNamespace
attachNamespace(package = ,pos = )
attachNamespace(ns = ,pos = ,depends = ,exclude = ,include.only = )
attachNamespace(package = x,pos = ,depends = ,exclude = ,include.only = )
attachNamespace(package = x,pos = x,depends = ,exclude = ,include.only = )
attachNamespace(package = x,pos = x,depends = x,exclude = ,include.only = )
attachNamespace(package = x,pos = x,depends = x,exclude = x,include.only = )
attachNamespace(package = x,pos = x,depends = x,exclude = x,include.only = x)
attachNamespace(package = x,pos = x,depends = x,exclude = df,include.only = x)
attachNamespace(package = x,pos = x,depends = df,exclude = df,include.only = x)
attachNamespace(package = x,pos = df,depends = df,exclude = df,include.only = x)
attachNamespace(package = df,pos = df,depends = df,exclude = df,include.only = x)
attachNamespace(package = "df",pos = df,depends = df,exclude = df,include.only = x)
#attr
attr(x = ,which = ,exact = )
attr(x = x,which = ,exact = )
attr(x = x,which = x,exact = )
attr(x = x,which = x,exact = x)
attr(x = x,which = x,exact = df)
attr(x = x,which = df,exact = df)
attr(x = df,which = df,exact = df)
attr(x = "df",which = df,exact = df)
#attr.all.equal
attr.all.equal(x = ,y = ,tolerance = ,scale = ,check.attributes = )
attr.all.equal(target = ,current = ,check.attributes = ,check.names = )
attr.all.equal(target = x,current = ,check.attributes = ,check.names = )
attr.all.equal(target = x,current = x,check.attributes = ,check.names = )
attr.all.equal(target = x,current = x,check.attributes = x,check.names = )
attr.all.equal(target = x,current = x,check.attributes = x,check.names = x)
attr.all.equal(target = x,current = x,check.attributes = x,check.names = df)
attr.all.equal(target = x,current = x,check.attributes = df,check.names = df)
attr.all.equal(target = x,current = df,check.attributes = df,check.names = df)
attr.all.equal(target = df,current = df,check.attributes = df,check.names = df)
attr.all.equal(target = "df",current = df,check.attributes = df,check.names = df)
#attr<-
`attr<-`(x = ,which = ,value = )
`attr<-`(x = x,which = ,value = )
`attr<-`(x = x,which = x,value = )
`attr<-`(x = x,which = x,value = x)
`attr<-`(x = x,which = x,value = df)
`attr<-`(x = x,which = df,value = df)
`attr<-`(x = df,which = df,value = df)
`attr<-`(x = "df",which = df,value = df)
#attributes
attributes(object = )
attributes(x = )
attributes(x = x)
attributes(x = "x")
attributes(x = df)
#attributes<-
`attributes<-`(x = ,value = )
`attributes<-`(x = x,value = )
`attributes<-`(x = x,value = x)
`attributes<-`(x = x,value = df)
`attributes<-`(x = df,value = df)
`attributes<-`(x = "df",value = df)
#autoload
autoload(pkgname = ,file = ,character.only = )
autoload(name = ,package = ,reset = )
autoload(name = x,package = ,reset = )
autoload(name = x,package = x,reset = )
autoload(name = x,package = x,reset = x)
autoload(name = x,package = x,reset = df)
autoload(name = x,package = df,reset = df)
autoload(name = df,package = df,reset = df)
autoload(name = "df",package = df,reset = df)
#autoloader
autoloader(pkgname = ,file = ,character.only = )
autoloader(name = ,package = )
autoloader(name = x,package = )
autoloader(name = x,package = x)
autoloader(name = x,package = df)
autoloader(name = df,package = df)
autoloader(name = "df",package = df)


#attachNamespace
attachNamespace(package = ,pos = )
attachNamespace(ns = ,pos = ,depends = ,exclude = ,include.only = )
attachNamespace(ns = x,pos = ,depends = ,exclude = ,include.only = )
attachNamespace(ns = x,pos = x,depends = ,exclude = ,include.only = )
attachNamespace(ns = x,pos = x,depends = x,exclude = ,include.only = )
attachNamespace(ns = x,pos = x,depends = x,exclude = x,include.only = )
attachNamespace(ns = x,pos = x,depends = x,exclude = x,include.only = x)
attachNamespace(ns = x,pos = x,depends = x,exclude = df,include.only = x)
attachNamespace(ns = x,pos = x,depends = df,exclude = df,include.only = x)
attachNamespace(ns = x,pos = df,depends = df,exclude = df,include.only = x)
attachNamespace(ns = df,pos = df,depends = df,exclude = df,include.only = x)
attachNamespace(ns = "df",pos = df,depends = df,exclude = df,include.only = x)

#attr
attr(x = ,which = ,exact = )
attr(x = x,which = ,exact = )
attr(x = x,which = x,exact = )
attr(x = x,which = x,exact = x)
attr(x = x,which = x,exact = df)
attr(x = x,which = df,exact = df)
attr(x = df,which = df,exact = df)
attr(x = "df",which = df,exact = df)
#attr.all.equal
attr.all.equal(x = ,y = ,tolerance = ,scale = ,check.attributes = )
attr.all.equal(target = ,current = ,check.attributes = ,check.names = )
attr.all.equal(target = x,current = ,check.attributes = ,check.names = )
attr.all.equal(target = x,current = x,check.attributes = ,check.names = )
attr.all.equal(target = x,current = x,check.attributes = x,check.names = )
attr.all.equal(target = x,current = x,check.attributes = x,check.names = x)
attr.all.equal(target = x,current = x,check.attributes = x,check.names = df)
attr.all.equal(target = x,current = x,check.attributes = df,check.names = df)
attr.all.equal(target = x,current = df,check.attributes = df,check.names = df)
attr.all.equal(target = df,current = df,check.attributes = df,check.names = df)
attr.all.equal(target = "df",current = df,check.attributes = df,check.names = df)
#attr<-
`attr<-`(x = ,which = ,value = )
`attr<-`(x = x,which = ,value = )
`attr<-`(x = x,which = x,value = )
`attr<-`(x = x,which = x,value = x)
`attr<-`(x = x,which = x,value = df)
`attr<-`(x = x,which = df,value = df)
`attr<-`(x = df,which = df,value = df)
`attr<-`(x = "df",which = df,value = df)
#attributes
attributes(object = )
attributes(x = )
attributes(x = x)
attributes(x = "x")
attributes(x = df)
#attributes<-
`attributes<-`(x = ,value = )
`attributes<-`(x = x,value = )
`attributes<-`(x = x,value = x)
`attributes<-`(x = x,value = df)
`attributes<-`(x = df,value = df)
`attributes<-`(x = "df",value = df)
#autoload
autoload(pkgname = ,file = ,character.only = )
autoload(name = ,package = ,reset = )
autoload(name = x,package = ,reset = )
autoload(name = x,package = x,reset = )
autoload(name = x,package = x,reset = x)
autoload(name = x,package = x,reset = df)
autoload(name = x,package = df,reset = df)
autoload(name = df,package = df,reset = df)
autoload(name = "df",package = df,reset = df)
#autoloader
autoloader(pkgname = ,file = ,character.only = )
autoloader(name = ,package = )
autoloader(name = x,package = )
autoloader(name = x,package = x)
autoloader(name = x,package = df)
autoloader(name = df,package = df)
autoloader(name = "df",package = df)

#attachNamespace
attachNamespace(package = ,pos = )
attachNamespace(ns = ,pos = ,depends = ,exclude = ,include.only = )
attachNamespace(ns = x,pos = ,depends = ,exclude = ,include.only = )
attachNamespace(ns = x,pos = x,depends = ,exclude = ,include.only = )
attachNamespace(ns = x,pos = x,depends = x,exclude = ,include.only = )
attachNamespace(ns = x,pos = x,depends = x,exclude = x,include.only = )
attachNamespace(ns = x,pos = x,depends = x,exclude = x,include.only = x)
attachNamespace(ns = x,pos = x,depends = x,exclude = df,include.only = x)
attachNamespace(ns = x,pos = x,depends = df,exclude = df,include.only = x)
attachNamespace(ns = x,pos = df,depends = df,exclude = df,include.only = x)
attachNamespace(ns = df,pos = df,depends = df,exclude = df,include.only = x)
attachNamespace(ns = "df",pos = df,depends = df,exclude = df,include.only = x)

#attr
attr(x = ,which = ,exact = )
attr(x = x,which = ,exact = )
attr(x = x,which = x,exact = )
attr(x = x,which = x,exact = x)
attr(x = x,which = x,exact = df)
attr(x = x,which = df,exact = df)
attr(x = df,which = df,exact = df)
attr(x = "df",which = df,exact = df)
#attr.all.equal
attr.all.equal(x = ,y = ,tolerance = ,scale = ,check.attributes = )
attr.all.equal(target = ,current = ,check.attributes = ,check.names = )
attr.all.equal(target = x,current = ,check.attributes = ,check.names = )
attr.all.equal(target = x,current = x,check.attributes = ,check.names = )
attr.all.equal(target = x,current = x,check.attributes = x,check.names = )
attr.all.equal(target = x,current = x,check.attributes = x,check.names = x)
attr.all.equal(target = x,current = x,check.attributes = x,check.names = df)
attr.all.equal(target = x,current = x,check.attributes = df,check.names = df)
attr.all.equal(target = x,current = df,check.attributes = df,check.names = df)
attr.all.equal(target = df,current = df,check.attributes = df,check.names = df)
attr.all.equal(target = "df",current = df,check.attributes = df,check.names = df)
#attr<-
`attr<-`(x = ,which = ,value = )
`attr<-`(x = x,which = ,value = )
`attr<-`(x = x,which = x,value = )
`attr<-`(x = x,which = x,value = x)
`attr<-`(x = x,which = x,value = df)
`attr<-`(x = x,which = df,value = df)
`attr<-`(x = df,which = df,value = df)
`attr<-`(x = "df",which = df,value = df)
#attributes
attributes(object = )
attributes(x = )
attributes(x = x)
attributes(x = "x")
attributes(x = df)
#attributes<-
`attributes<-`(x = ,value = )
`attributes<-`(x = x,value = )
`attributes<-`(x = x,value = x)
`attributes<-`(x = x,value = df)
`attributes<-`(x = df,value = df)
`attributes<-`(x = "df",value = df)
#autoload
autoload(pkgname = ,file = ,character.only = )
autoload(name = ,package = ,reset = )
autoload(name = x,package = ,reset = )
autoload(name = x,package = x,reset = )
autoload(name = x,package = x,reset = x)
autoload(name = x,package = x,reset = df)
autoload(name = x,package = df,reset = df)
autoload(name = df,package = df,reset = df)
autoload(name = "df",package = df,reset = df)
#autoloader
autoloader(name = ,package = )
autoloader(name = x,package = )
autoloader(name = x,package = x)
autoloader(name = x,package = df)
autoloader(name = df,package = df)
autoloader(name = "df",package = df)

#attr<-
`attr<-`()
`attr<-`(x)
`attr<-`("x")
`attr<-`(df)

`attr<-`(x = ,which = ,value = )
`attr<-`(x = x,which = ,value = )
#attributes
attributes(object = )
attributes(x = )
attributes(x = x)
attributes(x = "x")
attributes(x = df)
#attributes<-
`attributes<-`()
`attributes<-`(x)
`attributes<-`("x")
`attributes<-`(df)
#autoload
autoload(pkgname = ,file = ,character.only = )
autoload(name = ,package = ,reset = )
autoload(name = x,package = ,reset = )
autoload(name = x,package = x,reset = )
autoload(name = x,package = x,reset = x)
autoload(name = x,package = x,reset = df)
autoload(name = x,package = df,reset = df)
autoload(name = df,package = df,reset = df)
autoload(name = "df",package = df,reset = df)

#autoloader
autoloader(pkgname = ,file = ,character.only = )
autoloader(name = ,package = )
autoloader(name = x,package = )
autoloader(name = x,package = x)
autoloader(name = x,package = df)
autoloader(name = df,package = df)
autoloader(name = "df",package = df)

#backsolve
backsolve(r = ,x = ,k = ,upper.tri = )
backsolve(r = ,x = ,k = ,upper.tri = ,transpose = )
backsolve(r = x,x = ,k = ,upper.tri = ,transpose = )
backsolve(r = x,x = x,k = ,upper.tri = ,transpose = )
backsolve(r = x,x = x,k = x,upper.tri = ,transpose = )
backsolve(r = x,x = x,k = x,upper.tri = x,transpose = )
backsolve(r = x,x = x,k = x,upper.tri = x,transpose = x)
backsolve(r = x,x = x,k = x,upper.tri = df,transpose = x)
backsolve(r = x,x = x,k = df,upper.tri = df,transpose = x)
backsolve(r = x,x = df,k = df,upper.tri = df,transpose = x)
backsolve(r = x,x = df,k = df,upper.tri = df,transpose = df)
backsolve(r = x,x = df,k = df,upper.tri = df,transpose = "df")
backsolve(r = x,x = df,k = df,upper.tri = "df",transpose = "df")
backsolve(r = x,x = df,k = "df",upper.tri = "df",transpose = "df")
backsolve(r = x,x = "df",k = "df",upper.tri = "df",transpose = "df")
backsolve(r = "x",x = "df",k = "df",upper.tri = "df",transpose = "df")
#baseenv
baseenv()
baseenv(x)
baseenv("x")
baseenv(df)
#basename
basename(path = ,suffix = )
basename(path = )
basename(path =x )
basename(path =df )
basename(path ="x" )

basename(path = x,suffix = )
basename(path = x,suffix = x) 
basename(path = x,suffix = df)
basename(path = x,suffix = "df")
basename(path = path,suffix = )

#besselI
besselI(x = ,nu = )
besselI(x = ,nu = ,expon.scaled = )
besselI(x = x,nu = ,expon.scaled = )
besselI(x = x,nu = x,expon.scaled = )
besselI(x = x,nu = x,expon.scaled = x)
besselI(x = x,nu = x,expon.scaled = df)
besselI(x = x,nu = df,expon.scaled = df)
besselI(x = df,nu = df,expon.scaled = df)
besselI(x = "df",nu = df,expon.scaled = df)


#besselJ
besselJ(x = ,nu = )
besselJ(x = ,nu = )
besselJ(x = x,nu = )
besselJ(x = x,nu = x)

#besselK
besselK(x = ,nu = )
besselK(x = x,nu = ,expon.scaled = )
besselK(x = x,nu = x,expon.scaled = )
besselK(x = x,nu = x,expon.scaled = x)
besselK(x = x,nu = x,expon.scaled = df)
besselK(x = x,nu = df,expon.scaled = df)
besselK(x = df,nu = df,expon.scaled = df)
besselK(x = "df",nu = df,expon.scaled = df)
#besselY
besselY(x = ,nu = )
besselY(x = x,nu = )
besselY(x = x,nu = x)
besselY(x = x,nu = df)
besselY(x = df,nu = df)
besselY(x = "df",nu = df)
#beta
beta(x = ,y = )
beta(a = x,b = )
beta(a = x,b = x)
beta(a = x,b = df)
beta(a = df,b = df)
beta(a = "df",b = df)
#bindingIsActive
bindingIsActive(x = )
bindingIsActive(x = x)
bindingIsActive(x = "x")
bindingIsActive(x = df)


#bindingIsLocked
bindingIsLocked(x = )
bindingIsLocked(x = x)
bindingIsLocked(x = "x")
bindingIsLocked(x = df)

#bindtextdomain
bindtextdomain(domain = ,dirname = )
bindtextdomain(domain = x,dirname = )
bindtextdomain(domain = x,dirname = x)
bindtextdomain(domain = x,dirname = df)
bindtextdomain(domain = df,dirname = df)
bindtextdomain(domain = "df",dirname = df)
#bitwAnd
bitwAnd(x = ,y = )
bitwAnd(x = x,y = )
bitwAnd(x = x,y = x)
bitwAnd(x = x,y = df)
bitwAnd(x = df,y = df)
bitwAnd(x = "df",y = df)


#bitwNot
bitwNot(x = )
bitwNot(x = x)
bitwNot(x = "x")
bitwNot(x = df)
#bitwOr
bitwOr(x = ,y = )
bitwOr(x = x,y = )
bitwOr(x = x,y = x)
bitwOr(x = x,y = df)
bitwOr(x = df,y = df)
bitwOr(x = "df",y = df)

#bitwShiftL
bitwShiftL(x = ,n = )
bitwShiftL(x = x,n = )
bitwShiftL(x = x,n = x)
bitwShiftL(x = x,n = df)
bitwShiftL(x = df,n = df)
bitwShiftL(x = "df",n = df)

#bitwShiftR
bitwShiftR(x = ,n = )
bitwShiftR(x = x,n = )
bitwShiftR(x = x,n = x)
bitwShiftR(x = x,n = df)
bitwShiftR(x = df,n = df)
bitwShiftR(x = "df",n = df)
#bitwXor
bitwXor(a = ,b = )
bitwXor(a = x,b = )
bitwXor(a = x,b = x)
bitwXor(a = x,b = df)
bitwXor(a = df,b = df)
bitwXor(a = "df",b = df)
#body 
body(x = )
body(fun = )
body(x = x)
body(fun = x)
body(x = "x")
body(fun = "x")
body(x = df)
body(fun = df)

#body<-
`body<-`()
`body<-`(x)
`body<-`("x")
`body<-`(df)

#bquote
bquote(expr = x)
bquote(expr = "x")
bquote(expr = df)

#break
break(x)
break("x")
break(df)

#browser
browser(expr = x)
browser(expr = "x")
browser(expr = df)

#browserCondition
browserCondition(expr = x)
browserCondition(expr = "x")
browserCondition(expr = df)

#browserSetDebug
browserSetDebug(flag = )
browserSetDebug(n = x)
browserSetDebug(n = "x")
browserSetDebug(n = df)

#browserText
browserText(expr = )
browserText(n = x)
browserText(n = "x")
browserText(n = df)
#builtins
builtins()
builtins(internal = x)
builtins(internal = "x")
builtins(internal = df)

#by
by(data = ,INDICES = ,FUN = ,...,simplify = )
by(data = ,INDICES = ,FUN = ,simplify = )
by(data = x,INDICES = ,FUN = ,simplify = )
by(data = x,INDICES = x,FUN = ,simplify = )
by(data = x,INDICES = x,FUN = x,simplify = )
by(data = x,INDICES = x,FUN = x,simplify = x)
by(data = x,INDICES = x,FUN = x,simplify = df)
by(data = x,INDICES = x,FUN = df,simplify = df)
by(data = x,INDICES = df,FUN = df,simplify = df)
by(data = df,INDICES = df,FUN = df,simplify = df)
by(data = "df",INDICES = df,FUN = df,simplify = df)


#by.data.frame
by.data.frame(data = ,INDICES = ,FUN = ,...,simplify = )
by.data.frame(data = ,INDICES = ,FUN = ,simplify = )
by.data.frame(data = x,INDICES = ,FUN = ,simplify = )
by.data.frame(data = x,INDICES = x,FUN = ,simplify = )
by.data.frame(data = x,INDICES = x,FUN = x,simplify = )
by.data.frame(data = x,INDICES = x,FUN = x,simplify = x)
by.data.frame(data = x,INDICES = x,FUN = x,simplify = df)
by.data.frame(data = x,INDICES = x,FUN = df,simplify = df)
by.data.frame(data = x,INDICES = df,FUN = df,simplify = df)
by.data.frame(data = df,INDICES = df,FUN = df,simplify = df)
by.data.frame(data = "df",INDICES = df,FUN = df,simplify = df)
#by.default
by.default(data = ,INDICES = ,FUN = ,...,simplify = )
by.default(data = ,INDICES = ,FUN = ,simplify = )
by.default(data = x,INDICES = ,FUN = ,simplify = )
by.default(data = x,INDICES = x,FUN = ,simplify = )
by.default(data = x,INDICES = x,FUN = x,simplify = )
by.default(data = x,INDICES = x,FUN = x,simplify = x)
by.default(data = x,INDICES = x,FUN = x,simplify = df)
by.default(data = x,INDICES = x,FUN = df,simplify = df)
by.default(data = x,INDICES = df,FUN = df,simplify = df)
by.default(data = df,INDICES = df,FUN = df,simplify = df)
by.default(data = "df",INDICES = df,FUN = df,simplify = df)
#bzfile
bzfile(description = ,open = ,encoding = ,compression = ,close = )
bzfile(description = ,open = ,encoding = ,compression = )
bzfile(description = x,open = ,encoding = ,compression = )
bzfile(description = x,open = x,encoding = ,compression = )
bzfile(description = x,open = x,encoding = x,compression = )
bzfile(description = x,open = x,encoding = x,compression = x)
bzfile(description = x,open = x,encoding = x,compression = df)
bzfile(description = x,open = x,encoding = df,compression = df)
bzfile(description = x,open = df,encoding = df,compression = df)
bzfile(description = df,open = df,encoding = df,compression = df)
bzfile(description = "df",open = df,encoding = df,compression = df)

#c
c(...)
c()
c(x)
c("x")
c(df)
#c.Date
c.Date(...)
c.Date(recursive = )
c.Date(recursive = x)
c.Date(recursive = "x")
c.Date(recursive = df)


#c.difftime
c.difftime(...)
c.difftime(recursive = )
c.difftime(recursive = x)
c.difftime(recursive = "x")
c.difftime(recursive = df)
#c.factor
c.factor(...)
c.factor(recursive = )
c.factor(recursive = x)
c.factor(recursive = "x")
c.factor(recursive = df)
#c.noquote
c.noquote(...)
c.noquote(recursive = )
c.noquote(recursive = x)
c.noquote(recursive = "x")
c.noquote(recursive = df)
#c.numeric_version
c.numeric_version(...)
c.numeric_version(recursive = )
c.numeric_version(recursive = x)
c.numeric_version(recursive = "x")
c.numeric_version(recursive = df)
#c.POSIXct
c.POSIXct(...)
c.POSIXct(recursive = )
c.POSIXct(recursive = x)
c.POSIXct(recursive = "x")
c.POSIXct(recursive = df)
#c.POSIXlt
c.POSIXlt(...)
c.POSIXlt(recursive = )
c.POSIXlt(recursive = x)
c.POSIXlt(recursive = "x")
c.POSIXlt(recursive = df)
#c.warnings
c.warnings(...)
c.warnings(recursive = )
c.warnings(recursive = x)
c.warnings(recursive = "x")
c.warnings(recursive = df)
#call
call(name = ,...)
call(name = )
call(name = x)
call(name = "x")
call(name = df)


#callCC
callCC(fun = )
callCC(fun = x)
callCC(fun = "x")
callCC(fun = df)
#capabilities
capabilities()
capabilities(x)
capabilities("x")
capabilities(df)
#casefold
casefold(x = ,upper = )
casefold(x = x,upper = )
casefold(x = x,upper = x)
casefold(x = x,upper = df)
casefold(x = df,upper = df)
casefold(x = "df",upper = df)

#cat
cat(...,file = ,sep = ,fill = ,labels = ,append = )
cat(file = ,sep = ,fill = ,labels = ,append = )
cat(file = x,sep = ,fill = ,labels = ,append = )
cat(file = x,sep = x,fill = ,labels = ,append = )
cat(file = x,sep = x,fill = x,labels = ,append = )
cat(file = x,sep = x,fill = x,labels = x,append = )
cat(file = x,sep = x,fill = x,labels = x,append = x)
cat(file = x,sep = x,fill = x,labels = x,append = df)
cat(file = x,sep = x,fill = x,labels = df,append = df)
cat(file = x,sep = x,fill = df,labels = df,append = df)
cat(file = x,sep = df,fill = df,labels = df,append = df)
cat(file = x,sep = "df",fill = df,labels = df,append = df)
cat(file = x,sep = "df",fill = "df",labels = df,append = df)
cat(file = x,sep = "df",fill = "df",labels = "df",append = df)
cat(file = x,sep = "df",fill = "df",labels = "df",append = "df")
cat(file = df,sep = "df",fill = "df",labels = "df",append = df)
cat(file = "df",sep = "df",fill = "df",labels = "df",append = df)

#cbind
cbind(...)
cbind(deparse.level = )
cbind(deparse.level = x)
cbind(deparse.level = "x")
cbind(deparse.level = df)

#cbind.data.frame
cbind.data.frame(...)
cbind.data.frame(deparse.level =)
cbind.data.frame(deparse.level = x)
cbind.data.frame(deparse.level = "x")
cbind.data.frame(deparse.level = df)
#ceiling
ceiling(x = )
ceiling(x = x)
ceiling(x = df)

#char.expand
char.expand(x = )
char.expand(x = x)
char.expand(x = "x")
char.expand(x = df)
#character
character(length = )
character(length = )
character(length = x)
character(length = "x")
character(length = df)
#charmatch
charmatch(x = ,table = ,nomatch = )
echarmatch(x = x,table = ,nomatch = )
charmatch(x = x,table = x,nomatch = )
charmatch(x = x,table = x,nomatch = x)
charmatch(x = x,table = x,nomatch = df)
charmatch(x = x,table = df,nomatch = df)
charmatch(x = df,table = df,nomatch = df)
charmatch(x = "df",table = df,nomatch = df)
#charToRaw
charToRaw(x = )
charToRaw(x = x)

charToRaw(x = "x")
charToRaw(x = df)
#chartr
chartr(old = ,new = ,x = )
chartr(old = x,new = ,x = )
chartr(old = x,new = x,x = )
chartr(old = x,new = x,x = x)
chartr(old = x,new = x,x = df)
chartr(old = x,new = df,x = df)
chartr(old = df,new = df,x = df)
chartr(old = "df",new = df,x = df)
#check_tzones
check_tzones(tz1 = ,tz2 = )
check_tzones()
check_tzones(x)
check_tzones("x")
check_tzones(df)


#chkDots
chkDots(...)
chkDots(which.call = ,allowed = )
chkDots(which.call = x,allowed = )
chkDots(which.call = x,allowed = x)
chkDots(which.call = x,allowed = df)
chkDots(which.call = df,allowed = df)
chkDots(which.call = "df",allowed = df)
#chol
chol(x = x,pivot = true,LINPACK = true,tol = x)
chol(x = x)
chol(x = "test")
chol(x = df)

#chol.default
chol.default(x = x,pivot = true,LINPACK = true)
chol.default(x = ,pivot = ,LINPACK = ,tol = )
chol.default(x = x  ,pivot = ,LINPACK = ,tol = )
chol.default(x = x  ,pivot = x,LINPACK = ,tol = )
chol.default(x = x  ,pivot = x,LINPACK = x,tol = )
chol.default(x = x  ,pivot = x,LINPACK = x,tol = x)
chol.default(x = x  ,pivot = x,LINPACK = x,tol = df)
chol.default(x = x  ,pivot = x,LINPACK = df,tol = df)
chol.default(x = x  ,pivot = df,LINPACK = df,tol = df)
chol.default(x = x  ,pivot = "df",LINPACK = df,tol = df)
chol.default(x = x  ,pivot = "df",LINPACK = "df",tol = df)
chol.default(x = "x"  ,pivot = "df",LINPACK = "df",tol = "df")


#chol2inv
chol2inv(x = x,size = NCOL(x),LINPACK = true)
chol2inv(x = ,size = ,LINPACK = )
chol2inv(x = x,size = ,LINPACK = )
chol2inv(x = x,size = x,LINPACK = )
chol2inv(x = x,size = x,LINPACK = x)
chol2inv(x = x,size = x,LINPACK = df)
chol2inv(x = x,size = df,LINPACK = df)
chol2inv(x = df,size = df,LINPACK = df)
chol2inv(x = "df",size = df,LINPACK = df)

#choose
choose(n =x,k = x)
choose(n = x,k = )
choose(n = x,k = x)
choose(n = df,k = x)
choose(n = "x",k = x)
#class
class(x = x)
class(x = )
class(x = "x")
class(x = df)

#class<-
`class<-`(x = ,value = )
`class<-`()
`class<-`(x)
`class<-`("x")
`class<-`(df)
#clearPushBack
clearPushBack(connection = )
#close
close(con = )
#close.connection
close.connection(con = )

#close.srcfile
close.srcfile(con = )
#close.srcfilealias
close.srcfilealias(con = )
#closeAllConnections
closeAllConnections(all = )
#col
col(x = )
#colMeans
colMeans(x = ,na.rm = )
#colnames
colnames(x = )
#colnames<-
`colnames<-`(x = ,value = )
#colSums
colSums(x = ,na.rm = )
#commandArgs
commandArgs(trailingOnly = )
#comment
comment(object = )
#comment<-
`comment<-`(object = ,value = )
#complex
complex(real = ,imag = ,length.out = ,imaginary = ,modulus = ,argument = )
#computeRestarts
computeRestarts(cond = )
#conditionCall
conditionCall(condition = ,c = )

#conditionCall.condition
conditionCall.condition(condition = ,c = )

#conditionMessage
conditionMessage(condition = ,c = )
#conditionMessage.condition
conditionMessage.condition(condition = ,c = )
#conflictRules
conflictRules(pkg = ,mask.ok = ,exclude = )
conflictRules(pkg = ,mask.ok = ,exclude = )
#conflicts
conflicts(pkg = ,mask.ok = ,exclude = )

#Conj
Conj(x = ,z = )
Conj(z = )

#contributors
contributors(pkg = ,file = )
#cos
cos(x = )
#cosh
cosh(x = )
#cospi
cospi(x = )
#crossprod
crossprod(x = ,y = )
#Cstack_info
Cstack_info()
#cummax
cummax(x = )

#cummin
cummin(x = )
#cumprod
cumprod(x = )
#cumsum
tryCatch({
  cumsum(x = "abc")
  
},error = function(e){
  e
})
tryCatch({
  cumsum(x = abc)
  
},error = function(e){
  e
})
#curlGetHeaders
curlGetHeaders(url = ,handle = ,.opts = )
#cut
tryCatch({
  cut(x = ,breaks = 1,labels = ,include.lowest = ,right = ,dig.lab = ,ordered_result = )},error = function(e){
    e
  })
tryCatch({cut(x= 2 )},error = function(e){e})
tryCatch({cut(x = 1,breaks = 2,labels = test,include.lowest = ,right = ,dig.lab = ,ordered_result = )
},error = function(e){e})
tryCatch({cut(x = 1,breaks = 2,labels = "test",include.lowest = ,right = ,dig.lab = ,ordered_result = )
},error = function(e){e})
cut(x = 1,2,include.lowest = "TRUE")
cut(x = 1,2,right = ,dig.lab = "test",ordered_result = )
cut(x = 1,2,right = ,dig.lab = ,ordered_result = TEST)

#cut.Date
cut.Date(x = ,breaks = ,labels = ,start.on.monday = ,include.lowest = )
cut.Date(x = 1,breaks = ,labels = ,start.on.monday = ,include.lowest = )
cut.Date(x = "2023-9-3",breaks = ,labels = ,start.on.monday = ,include.lowest = )

#cut.default
cut.default(x = ,breaks = ,labels = ,include.lowest = ,right = ,dig.lab = ,ordered_result = )
#cut.POSIXt
cut.POSIXt(x = ,breaks = ,labels = ,start.on.monday = ,include.lowest = )
#data.class
data.class(x = )
#data.frame
data.frame(...,row.names = ,check.rows = ,check.names = ,stringsAsFactors = ,fix.empty.names = )
#data.matrix
data.matrix(x = ,rownames.force = )
#date
date()
#debug
debug(fun = )
#debuggingState
debuggingState(fun = )
#debugonce
debugonce(fun = )
#default.stringsAsFactors
default.stringsAsFactors()
#delayedAssign
delayedAssign(x = ,value = ,assign.env = ,eval.env = )
#deparse
deparse(x = ,nlines = ,width.cutoff = ,backtick = )
#deparse1
deparse1(x = ,nlines = ,width.cutoff = ,backtick = )
#det
det(x = ,method = )
#detach
detach(pos = ,name = ,unload = )
#determinant
determinant(x = ,log = ,method = )
#determinant.matrix
determinant.matrix(x = ,log = ,method = )
#dget
dget(file = ,srcfile = )
#diag
diag(x = ,nrow = ,ncol = )
#diag<-
`diag<-`(x = ,value = )
#diff
diff(x = ,lag = ,differences = ,... = )
#diff.Date
diff.Date(x = ,lag = ,differences = ,... = )
#diff.default
diff.default(x = ,lag = ,differences = ,... = )
#diff.difftime
diff.difftime(x = ,lag = ,differences = ,... = )
#diff.POSIXt
diff.POSIXt(x = ,lag = ,differences = ,... = )
#difftime
difftime(time1 = ,time2 = ,units = )
#digamma
digamma(x = )
#dim
dim(x = )
#dim.data.frame
dim.data.frame(x = )
#dim<-
`dim<-`(x = ,value = )
#dimnames 
dimnames(x = )
#dimnames.data.frame
dimnames.data.frame(x = )
#dimnames<-
`dimnames<-`(x = ,value = )
#dimnames<-.data.frame
dimnames<-.data.frame(x = ,value = )
#dir
dir(path = ,pattern = ,all.files = ,full.names = ,recursive = ,ignore.case = ,include.dirs = ,no.. = )
#dir.create
dir.create(path = ,showWarnings = ,recursive = ,mode = )

#dir.exists
dir.exists(path = )
#dirname
dirname(path = )
#do.call
do.call(what = ,args = ,quote = )
#dontCheck
dontCheck(pkg = )
#double
double(length = )
#dput
dput(x = ,file = ,control = )
#dQuote
dQuote(x = )
#drop
drop(x = )
drop(x = )
#droplevels
droplevels(x = )
#droplevels.data.frame
droplevels.data.frame(x = )
#droplevels.factor
droplevels.factor(x = )
#dump
dump(object = ,file = ,...)
dump(list = ,file = ,append = ,control = ,envir = ,evaluate = )
#duplicated
duplicated(x = ,fromLast = )
#duplicated.array
duplicated.array(x = ,fromLast = )
#duplicated.data.frame
duplicated.data.frame(x = ,fromLast = )
#duplicated.default
duplicated.default(x = ,fromLast = )
#duplicated.matrix
duplicated.matrix(x = ,fromLast = )
#duplicated.numeric_version
duplicated.numeric_version(x = ,fromLast = )
#duplicated.POSIXlt
duplicated.POSIXlt(x = ,fromLast = )
#duplicated.warnings
duplicated.warnings(x = ,fromLast = )
#dyn.load
dyn.load(file = ,local = )
#dyn.unload
dyn.unload(file = )
#dynGet
dynGet(x = ,envir = )
#eapply
eapply(env = ,FUN = ,...,inherits = )
#eigen
eigen(x = ,only.values = ,EISPACK = )
#emptyenv
emptyenv()
#enc2native
enc2native(x = )
#enc2utf8
enc2utf8(x = )
#encodeString
encodeString(x = ,to = ,sub = )
#Encoding
Encoding(x = )

#Encoding<-
`Encoding<-`(x = ,value = )
#endsWith
endsWith(x = ,suffix = ,ignore.case = )
#enquote
enquote(x = )
#env.profile
env.profile(env = ,all.names = ,includeBase = ,includeInternals = ,includeEmpty = ,includeFunctions = ,includeSpecials = ,includeObjects = ,includeSearch = ,includeGlobals = ,includeBase = ,includeInternals = ,includeEmpty = ,includeFunctions = ,includeSpecials = ,includeObjects = ,includeSearch = ,includeGlobals = )
#environment
environment(fun = )
#environment<-
`environment<-`(fun = ,value = )
#environmentIsLocked
environmentIsLocked(fun = )
#environmentName
environmentName(fun = )
#errorCondition
errorCondition(condition = ,c = )

#eval
eval(expr = ,envir = ,enclos = )
#eval.parent
eval.parent(expr = ,n = )
#evalq
evalq(expr = ,envir = ,enclos = )
#exists
exists(x = ,where = ,frame = )
#exp
exp(x = )
#expand.grid
expand.grid(...)
#expm1
expm1(x = )
#expression
expression(...)
#extSoftVersion
extSoftVersion(pkg = )
#F
F()
#factor
factor(x = ,levels = ,labels = ,exclude = )
#factorial
factorial(x = )
#fifo
fifo(path = )
#file
file(description = ,open = ,encoding = ,compression = ,close = )
#file.access
file.access(path = ,mode = )
#file.append
file.append(file = ,text = )
#file.choose
file.choose(new = )
#file.copy
file.copy(from = ,to = ,overwrite = ,recursive = ,copy.mode = ,copy.date = )
#file.create
file.create(...)
#file.exists
file.exists(file = )
#file.info
file.info(file = )
#file.link
file.link(from = ,to = )
#file.mode
file.mode(file = )
#file.mtime
file.mtime(file = )
#file.path
file.path(...)
file.path(fsep = )

#file.remove
file.remove(...)
file.remove()
#file.rename
file.rename(from = ,to = )
#file.show
file.show(file = ,header = ,delete.file = )
#file.size
file.size(file = )
#file.symlink
file.symlink(from = ,to = )
#Filter
Filter(f = ,x = )
#Find
Find(f = ,x = )
#find.package
find.package(package = ,quiet = ,verbose = ,lib.loc = )
#findInterval
findInterval(x = ,vec = ,rightmost.closed = ,all.inside = )
#findPackageEnv
findPackageEnv(pkg = ,lib.loc = )

#findRestart
findRestart(name = ,where = )
#floor
floor(x = )
#flush
flush(con = )
#flush.connection
flush.connection(con = )
#for
for(i in 1:10){
  print(i)
}
#force
force(x = )
#forceAndCall
forceAndCall(call = ,env = )
#formals
formals(fun = )
#formals<-
`formals<-`(fun = ,value = )
`formals<-`()
#format
format(x = ,trim = ,digits = ,nsmall = ,scientific = ,width = ,justify = ,exponent = ,big.mark = ,big.interval = ,small.mark = ,small.interval = ,decimal.mark = ,zero.print = ,drop0trailing = )
#format.AsIs
format.AsIs(x = ,trim = ,digits = ,nsmall = ,scientific = ,width = ,justify = ,exponent = ,big.mark = ,big.interval = ,small.mark = ,small.interval = ,decimal.mark = ,zero.print = ,drop0trailing = )
#format.data.frame
format.data.frame(x = ,trim = ,digits = ,nsmall = ,scientific = ,width = ,justify = ,exponent = ,big.mark = ,big.interval = ,small.mark = ,small.interval = ,decimal.mark = ,zero.print = ,drop0trailing = )
#format.Date
format.Date(x = ,format = ,tz = )
#format.default
format.default(x = ,trim = ,digits = ,nsmall = ,scientific = ,width = ,justify = ,exponent = ,big.mark = ,big.interval = ,small.mark = ,small.interval = ,decimal.mark = ,zero.print = ,drop0trailing = )
#format.difftime
format.difftime(x = ,units = ,...)
#format.factor
format.factor(x = ,trim = ,digits = ,nsmall = ,scientific = ,width = ,justify = ,exponent = ,big.mark = ,big.interval = ,small.mark = ,small.interval = ,decimal.mark = ,zero.print = ,drop0trailing = )
#format.hexmode
format.hexmode(x = ,width = )
#format.info
format.info(x = ,digits = ,nsmall = )
#format.libraryIQR
format.libraryIQR(x = ,digits = ,nsmall = )
#format.numeric_version
format.numeric_version(x = ,...)
#format.octmode
format.octmode(x = ,width = )
#format.packageInfo
format.packageInfo(x = ,digits = ,nsmall = )
#format.POSIXct
format.POSIXct(x = ,format = ,tz = ,usetz = )
#format.POSIXlt
format.POSIXlt(x = ,format = ,tz = ,usetz = )
#format.pval
format.pval(x = ,eps = ,digits = ,nsmall = ,...)
#format.summaryDefault
format.summaryDefault(x = ,digits = ,nsmall = )
#formatC
formatC(x = ,format = ,digits = ,nsmall = ,width = ,flag = ,digits.secs = ,...)
#formatDL
formatDL(x = ,quote = ,na.encode = ,row.names = ,col.names = ,sep = ,eol = ,na = ,...)
#forwardsolve
forwardsolve(r = ,x = ,k = ,upper.tri = )
#function
function(...)
#gamma
gamma(x = )
#gc
gc(verbose = )
#gc.time
gc.time(on = )
#gcinfo
gcinfo(verbose = )

#gctorture
gctorture(verbose = )
#gctorture2
gctorture2(verbose = )
#get
get(x = ,pos = ,envir = )
#get0
get0(x = ,envir = )
#getAllConnections
getAllConnections()
#getCallingDLL
getCallingDLL(f = ,doStop = )
#getCallingDLLe
getCallingDLLe(e = )
#getConnection
getConnection(con = )
#getDLLRegisteredRoutines
getDLLRegisteredRoutines(DLLInfo = ,name = ,type = ,allNames = )

#getDLLRegisteredRoutines.character
getDLLRegisteredRoutines.character(DLLInfo = ,name = ,type = ,allNames = )
#getDLLRegisteredRoutines.DLLInfo
getDLLRegisteredRoutines.DLLInfo(DLLInfo = ,name = ,type = ,allNames = )
#getElement
getElement(x = ,name = ,exact = )
#geterrmessage
geterrmessage()
#getExportedValue
getExportedValue(pkg = ,name = )
#getHook
getHook(which = )

#getLoadedDLLs
getLoadedDLLs()
#getNamespace
getNamespace(name = ,which.lib.loc = )
#getNamespaceExports
getNamespaceExports(ns = ,all.names = )
#getNamespaceImports
getNamespaceImports(ns = ,all.names = )
#getNamespaceInfo
getNamespaceInfo(ns = ,what = )
#getNamespaceName
getNamespaceName(ns = )
#getNamespaceUsers
getNamespaceUsers(ns = ,all.names = )
#getNamespaceVersion
getNamespaceVersion(ns = )
#getNativeSymbolInfo
getNativeSymbolInfo(sym = d,where = )
getNativeSymbolInfo(sym = ,where = r)
getNativeSymbolInfo(sym = ,where = ,name = test,PACKAGE = ,unlist = ,withRegistrationInfo = )
getNativeSymbolInfo(sym = ,where = ,name = "sum",PACKAGE = "base",unlist = ,withRegistrationInfo = )

#getOption
getOption(x = ,default = ,complete = ,help = ,recursive = ,no.deparse = ,no.defaults = ,no.warn = ,keep.source = ,skip.silent = ,width = )
#getRversion
getRversion()
#getSrcLines
getSrcLines(con = ,offset = ,n = )
#getTaskCallbackNames
getTaskCallbackNames()
#gettext
gettext(x = )
#gettextf
gettextf(...,fmt ="Hello,%$",domain = ,trim = )
gettextf(g)
gettextf("world",fmt ="Hello,%$",domain = ,trim = )

gettextf("world",fmt ="Hello,%s",domain = ,trim = false)

gettextf("world",fmt ="Hello,%$",domain = true,trim = )

#getwd
getwd(e)
#gl
gl(n = 1,k = ,length = ,labels = ,ordered = )
gl(n = 1,k = 3,length = 4,labels = test,ordered = )
gl(n = 1,k = 3,length = ,labels = ,ordered = TEST)
gl(n = 1,k = 3,length = TEST,labels = ,ordered = )

#globalCallingHandlers
globalCallingHandlers(4)
globalCallingHandlers(TEST)
#globalenv
globalenv(5)
globalenv(test)
globalenv("test")
#gregexec
gregexec(pattern = 4,text = ,ignore.case = ,perl = ,fixed = ,useBytes = )
gregexec(pattern = "4",text = test,ignore.case = ,perl = ,fixed = ,useBytes = )
gregexec(pattern = 4,text = "test",ignore.case = false,perl = ,fixed = ,useBytes = )
gregexec(pattern = 4,text = "test",ignore.case = ,perl = true,fixed = ,useBytes = )
gregexec(pattern = 4,text = "test",ignore.case = ,perl = ,fixed = test,useBytes = )
gregexec(pattern = 4,text = "test",ignore.case = ,perl = ,fixed = ,useBytes = test)

#gregexpr
gregexpr(pattern = ,text = ,ignore.case = ,perl = ,fixed = ,useBytes = )

#grep
grep(pattern = ,x = ,ignore.case = ,perl = ,value = ,fixed = ,useBytes = )
#grepl
grepl(pattern = ,x = ,ignore.case = ,perl = ,fixed = ,useBytes = )
#grepRaw
grepRaw(pattern = ,x = ,ignore.case = ,perl = ,value = ,fixed = ,useBytes = )
#grouping
grouping(x = )
#gsub
gsub(pattern = ,replacement = ,x = ,ignore.case = ,perl = ,fixed = ,useBytes = )
#gzcon
gzcon(con = ,open = ,encoding = ,compression = ,close = )
#gzfile
gzfile(description = ,open = ,encoding = ,compression = ,close = )
#I
I(x = )
#iconv
iconv(x = ,from = ,to = ,sub = )
#iconvlist
iconvlist()
#icuGetCollate
icuGetCollate(locale = )
#icuSetCollate
icuSetCollate(locale = )
#identical
identical(x = ,y = )
#identity
identity(x = )
#if
if(test = ,yes = ,no = )
  if()
#ifelse
ifelse(test = ,yes = ,no = )
#Im
Im(x = ,z = )
#importIntoEnv
importIntoEnv(from = ,to = ,env = ,keep.source = )
#infoRDS
infoRDS(file = )
#inherits
inherits(x = ,what = )
#integer
integer(length = )
#interaction
interaction(...,sep = ,lex.order = ,decreasing = )
#interactive
interactive()
#intersect
intersect(x = ,y = )
#intToBits
intToBits(x = )
#intToUtf8
intToUtf8(x = )
#inverse.rle
inverse.rle(x = )
#invisible
invisible(x = )
invisible(x = )
#invokeRestart
invokeRestart(restart = ,...)
invokeRestart(r = )
#invokeRestartInteractively
invokeRestartInteractively(restart = ,...)
invokeRestartInteractively(r = )
#is.array
is.array(x = )
#is.atomic
is.atomic(x = )
#is.call
is.call(x = )
#is.character
is.character(x = )
#is.complex
is.complex(x = )
#is.data.frame
is.data.frame(x = )
#is.double
is.double(x = )
#is.element
is.element(el = ,set = )
#is.environment
is.environment(x = )
#is.expression
is.expression(x = )
#is.factor
is.factor(x = )
#is.finite
is.finite(x = )
#is.function
is.function(x = )
#is.infinite
is.infinite(x = )
#is.integer
is.integer(x = )
#is.language
is.language(x = )
#is.list
is.list(x = )
#is.loaded
is.loaded(name = ,where = )
#is.logical
is.logical(x = )
#is.matrix
is.matrix(x = )
#is.na
is.na(x = )
#is.na.data.frame
is.na.data.frame(x = )
#is.na.numeric_version
is.na.numeric_version(x = )

#is.na.POSIXlt
is.na.POSIXlt(x = )
#is.na<-
`is.na<-`(x = ,value = )
#is.na<-.default
`is.na<-.default`(x = ,value = )
#is.na<-.factor
`is.na<-.factor`(x = ,value = )
#is.na<-.numeric_version
`is.na<-.numeric_version`(x = ,value = )
#is.name
is.name(x = )
#is.nan
is.nan(x = )
#is.null
is.null(x = )
#is.numeric
is.numeric(x = )
#is.numeric_version
is.numeric_version(x = )
#is.numeric.Date
is.numeric.Date(x = )
#is.numeric.difftime
is.numeric.difftime(x = )
#is.numeric.POSIXt
is.numeric.POSIXt(x = )
#is.object
is.object(x = )
#is.ordered
is.ordered(x = )
#is.package_version
is.package_version(x = )
#is.pairlist
is.pairlist(x = )
#is.primitive
is.primitive(x = )
#is.qr
is.qr(x = )
#is.R
is.R()
#is.raw
is.raw(x = )
#is.recursive
is.recursive(x = )
#is.single
is.single(x = )
#is.symbol
is.symbol(x = )
#is.table
is.table(x = )
#is.unsorted
is.unsorted(x = )
#is.vector
is.vector(x = ,mode = )
#isa
isa(x = ,class1 = ,class2 = )
#isatty
isatty(con = )
#isBaseNamespace
isBaseNamespace(ns = )
#isdebugged
isdebugged(fun = )
#isFALSE
isFALSE(x = )
#isIncomplete
isIncomplete(x = )
#isNamespace
isNamespace(ns = ,which = )
#isNamespaceLoaded
isNamespaceLoaded(pkg = ,quiet = )
#ISOdate
ISOdate(year = ,month = ,day = ,hour = ,min = ,sec = ,tz = )
#ISOdatetime
ISOdatetime(year = ,month = ,day = ,hour = ,min = ,sec = ,tz = )
#isOpen
isOpen(con = )
#isRestart
isRestart(restart = )

#isS4
isS4(x = )
#isSeekable
isSeekable(con = )
#isSymmetric
isSymmetric(x = ,tol = )
#isSymmetric.matrix
isSymmetric.matrix(x = ,tol = )
#isTRUE
isTRUE(x = )
#jitter
jitter(x = ,factor = )
#julian
julian(x = )
#julian.Date
julian.Date(x = )
#julian.POSIXt
julian.POSIXt(x = )
#kappa
kappa(x = )
#kappa.default
kappa.default(x = )
#kappa.lm
kappa.lm(x = )
#kappa.qr
kappa.qr(x = )

#kronecker
kronecker(X = ,Y = )
#l10n_info
l10n_info()
#La_library
La_library()
#La_version
La_version()
#La.svd
La.svd(x = ,nu = ,nv = ,LINPACK = )
#labels
labels(x = )
#labels.default
labels.default(x = )
#lapply
lapply(X = ,FUN = ,...)
#lazyLoad
lazyLoad(name = ,lib = ,keep.source = ,compress = ,envhook = )
#lazyLoadDBexec
lazyLoadDBexec(con = ,statement = ,...)
#lazyLoadDBfetch
lazyLoadDBfetch(con = ,statement = ,...)
#lbeta
lbeta(x = ,y = )
#lchoose
lchoose(n = ,k = )
#length
length(x = )
#length.POSIXlt 
length.POSIXlt(x = )
#length<-
`length<-`(x = ,value = )
#length<-.Date
`length<-.Date`(x = ,value = )
#length<-.difftime
`length<-.difftime`(x = ,value = )
#length<-.factor
`length<-.factor`(x = ,value = )
#length<-.POSIXct
`length<-.POSIXct`(x = ,value = )

#length<-.POSIXlt
`length<-.POSIXlt`(x = ,value = )
#lengths
lengths(x = )
#letters
letters
#LETTERS
LETTERS
#levels
levels(x = )
#levels.default
levels.default(x = )
#levels<-
`levels<-`(x = ,value = )
#levels<-.factor
`levels<-.factor`(x = ,value = )
#lfactorial
lfactorial(x = )
#lgamma
lgamma(x = )
#libcurlVersion
libcurlVersion()
#library
library(package = ,help = ,pos = ,lib.loc = ,character.only = ,logical.return = ,warn.conflicts = ,quietly = ,verbose = ,deparse.level = ,unload = )
#library.dynam
library.dynam(file = ,package = ,local = )
#library.dynam.unload
library.dynam.unload(package = ,local = )
#licence
licence()
#license
license()
#list
list(...)
list()
#list.dirs
list.dirs(path = ,full.names = ,recursive = ,ignore.case = )
#list.files
list.files(path = ,pattern = ,all.files = ,full.names = ,recursive = ,ignore.case = ,include.dirs = ,no.. = )
#list2DF
list2DF(x = ,row.names = ,keep.row.names = ,stringsAsFactors = )
#list2env
list2env(x = ,envir = ,parent = )
#load
load(file = ,envir = ,verbose = )
#loadedNamespaces
loadedNamespaces()
#loadingNamespaceInfo
loadingNamespaceInfo(pkg = )
#loadNamespace
loadNamespace(pkg = ,lib.loc = )
#local
local({...})
local(expr = ,envir = )
#lockBinding
lockBinding(sym = ,env = )
#lockEnvironment
lockEnvironment(env = )
#log
log(x = ,base = )
#log10
log10(x = )
#log1p
log1p(x = )
#log2
log2(x = )
#logb
logb(x = ,base = )
#logical
logical(length = )
#lower.tri
lower.tri(x = ,diag = )
#ls
ls(name = ,pos = ,envir = )
#make.names
make.names(x = ,unique = )
#make.unique
make.unique(names = )
#makeActiveBinding
makeActiveBinding(sym = ,fun = ,env = )
#Map
Map(f = ,...)
#mapply
mapply(FUN = ,...,MoreArgs = ,SIMPLIFY = ,USE.NAMES = )
#margin.table
margin.table(x = ,margin = ,FUN = )
#marginSums
marginSums(x = ,margin = )
#mat.or.vec
mat.or.vec(nrow = ,ncol = )
#match
match(x = ,table = ,nomatch = ,incomparables = )
#match.arg
match.arg(arg = ,choices = ,several.ok = )
#match.call
match.call(substitute = ,envir = )
#match.fun
match.fun(FUN = )
#Math.data.frame
Math.data.frame(x = )
#Math.Date
Math.Date(x = )
#Math.difftime
Math.difftime(x = )
#Math.factor
Math.factor(x = )
#Math.POSIXt
Math.POSIXt(x = )
#matrix
matrix(data = ,nrow = ,ncol = ,byrow = ,dimnames = )
#max
max(...)
max(na.rm = )
#max.col
max.col(x = ,ties.method = )
#mean
mean(x = ,...)
mean(x = )
#mean.Date
mean.Date(x = ,na.rm = )
#mean.default
mean.default(x = ,na.rm = )
#mean.difftime
mean.difftime(x = ,na.rm = )
#mean.POSIXct
mean.POSIXct(x = ,na.rm = )
#mean.POSIXlt
mean.POSIXlt(x = ,na.rm = )
#mem.maxNSize
mem.maxNSize(nsize = )
#mem.maxVSize
mem.maxVSize(vsize = )
#memCompress
memCompress(from = ,type = )
#memDecompress
memDecompress(from = ,type = )
#memory.profile
memory.profile()
#merge
merge(x = ,y = ,...)
merge(x = ,y = )
#merge.data.frame
merge.data.frame(x = ,y = ,...)
merge.data.frame(x = ,y = ,by = ,by.x = ,by.y = ,all = ,all.x = ,all.y = ,sort = ,suffixes = ,no.dups = ,incomparables = )
#merge.default
merge.default(x = ,y = ,...)
merge.default(x = ,y = )
#message
message(...)
message(domain = ,appendLF = )
#mget
mget(x = ,envir = ,inherits = ,mode = )
#min
min(...)
min(na.rm = )
#missing
missing(x = )
#Mod
Mod(x = ,y = )
#mode
mode(x = )
#mode<-
`mode<-`(x = ,value = )
`mode<-`()
#month.abb
month.abb

#month.name
month.name
#months
months(x = ,abbreviate = )
months(x = ,abbreviate = )
#months.Date
months.Date(x = ,abbreviate = )
#months.POSIXt
months.POSIXt(x = ,abbreviate = )
#mostattributes<-
`mostattributes<-`(x = ,value = )
`mostattributes<-`()
#names
names(x = )     
#names.POSIXlt
names.POSIXlt(x = )
#names<-
`names<-`(x = ,value = )
#names<-.POSIXlt
`names<-.POSIXlt`(x = ,value = )
#namespaceExport
namespaceExport(ns = ,exports = )
#namespaceImport
namespaceImport(ns = ,imports = ,from = )
#namespaceImportClasses
namespaceImportClasses(ns = ,classes = ,from = )
#namespaceImportFrom
namespaceImportFrom(ns = ,imports = ,from = )
#namespaceImportMethods
namespaceImportMethods(ns = ,methods = ,from = )
#nargs
nargs()
#nchar
nchar(x = ,type = )
#ncol
ncol(x = )
#NCOL
NCOL(x = )
#Negate
Negate(f = )
#new.env
new.env(hash = ,parent = ,size = )
new.env(hash = ,parent = ,size = )
#next
next(n = )  
#NextMethod
NextMethod(...)
NextMethod(generic = ,object = )
#ngettext
ngettext(msgid = ,msgid_plural = ,n = )
ngettext(n = ,msg1 = ,msg2 = ,domain = )
#nlevels
nlevels(x = )
#noquote
noquote(x = )
#norm
norm(x = ,type = ,...)
norm(x = ,type = )
#normalizePath
normalizePath(path = ,winslash = ,mustWork = )
#nrow
nrow(x = )
#NROW
NROW(x = )
#nullfile
nullfile()
#numeric
numeric(length = )
#numeric_version
numeric_version(x = )
#numToBits
numToBits(x = )
#numToInts
numToInts(x = )
#nzchar
nzchar(x = )
#objects
objects(name = ,pos = ,envir = )
#oldClass
oldClass(x = )
#oldClass<-
`oldClass<-`(x = ,value = )
#OlsonNames
OlsonNames()
#on.exit
on.exit(expr = ,add = )
#open
open(con = ,open = ,encoding = ,compression = ,text = )
#open.connection
open.connection(con = ,open = ,encoding = ,compression = ,text = )
#open.srcfile
open.srcfile(file = ,encoding = )
#open.srcfilealias
open.srcfilealias(file = ,encoding = )
#open.srcfilecopy
open.srcfilecopy(file = ,encoding = )
#Ops.data.frame
Ops.data.frame(e1 = ,e2 = )
#Ops.Date
Ops.Date(e1 = ,e2 = )
#Ops.difftime
Ops.difftime(e1 = ,e2 = )
#Ops.factor
Ops.factor(e1 = ,e2 = )
Ops.factor(e1 = ,e2 = )
#Ops.numeric_version
Ops.numeric_version(e1 = ,e2 = )
Ops.numeric_version(e1 = ,e2 = )
#Ops.ordered
Ops.ordered(e1 = ,e2 = )
Ops.ordered(e1 = ,e2 = )
#Ops.POSIXt
Ops.POSIXt(e1 = ,e2 = )
Ops.POSIXt(e1 = ,e2 = )
#options
options(...)
options(add.smooth = ,askpass = ,asksecret = ,bitmapType = ,browser = ,browserNLdisabled = ,buildtools.check = ,buildtools.with = ,CBoundsCheck = ,check.bounds = ,citation.bibtex.max = ,connectionObserver = ,continue = ,contrasts = ,defaultPackages = ,demo.ask = ,deparse.cutoff = ,deparse.max.lines = ,device = ,device.ask.default = ,digits = ,download.file.method = ,dvipscmd = ,echo = ,editor = ,encoding = ,example.ask = ,expressions = ,ggvis.renderer = ,help_type = ,help.search.types = ,help.try.all.packages = ,)
#order
order(...)
order(na.last = ,decreasing = ,method = )
#ordered
ordered(x = ,levels = ,labels = ,exclude = )
#outer
outer(X = ,Y = ,FUN = ,...)
outer(X = ,Y = ,FUN = )
#package_version
package_version(x = )
#packageEvent
packageEvent(pkg = ,event = ,lib.loc = )
#packageHasNamespace
packageHasNamespace(pkg = ,lib.loc = )
#packageNotFoundError
packageNotFoundError(pkg = ,lib.loc = )
#packageStartupMessage
packageStartupMessage(...)
packageStartupMessage(domain = ,appendLF = )
#packBits
packBits(x = )
#pairlist
pairlist(...)
pairlist()
#parent.env
parent.env(envir = )
#parent.env<-
`parent.env<-`(envir = ,value = )
#parent.frame
parent.frame(n = )
#parse
parse(text = ,srcfile = ,keep.source = )
#parseNamespaceFile
parseNamespaceFile(file = ,env = )
parseNamespaceFile(package = ,package.lib = ,mustExist = )
#paste
paste(...,sep = ,collapse = ,sep = ,collapse = )
paste(sep = ,collapse = ,recycle0 = )
#paste0
paste0(...,sep = ,collapse = ,sep = ,collapse = )
paste0(collapse = ,recycle0 = )
#path.expand
path.expand(path = )
path.expand(path = )
#path.package
path.package(package = ,quiet = ,verbose = ,lib.loc = )
path.package(package = ,quiet = )
#pcre_config
pcre_config()
pcre_config()

#pi
pi

#pipe
pipe(command = ,open = ,encoding = ,text = )
#plot
plot(x = ,y = ,...)
plot(x = ,y = )
#pmatch
pmatch(x = ,table = ,nomatch = )
#pmax
pmax(...)
pmax(na.rm = )
#pmax.int
pmax.int(...)
pmax.int(na.rm = )
#pmin
pmin(...)
pmin(na.rm = )
#pmin.int
pmin.int(...)
pmin.int(na.rm = )
#polyroot
polyroot(x = )
#pos.to.env
pos.to.env(pos = ,envir = )
#Position
Position(f = ,x = )
#pretty
pretty(x = ,n = ,min.n = ,max.n = ,...)
pretty(x = )
#pretty.default
pretty.default(x = ,n = ,min.n = ,max.n = ,...)
pretty.default(x = ,n = ,min.n = ,shrink.sml = ,high.u.bias = ,u5.bias = ,eps.correct = ,f.min = )
#prettyNum
prettyNum(x = ,big.mark = ,big.interval = ,small.mark = ,small.interval = ,decimal.mark = ,scientific = ,drop0trailing = )
#print
print(x = ,...)
print(x = )
#print.AsIs
print.AsIs(x = ,...)
print.AsIs(x = )
#print.by
print.by(x = ,...)
print.by(x = ,vsep = )
#print.condition
print.condition(x = ,...)
print.condition(x = )
#print.connection
print.connection(x = ,...)
print.connection(x = )
#print.data.frame
print.data.frame(x = ,...)
print.data.frame(x = ,digits = ,quote = ,right = ,row.names = ,max = )
#print.Date
print.Date(x = ,...)
print.Date(x = ,max = )
#print.default
print.default(x = ,...)
print.default(x = ,digits = ,quote = ,na.print = ,print.gap = ,right = ,max = ,width = ,useSource = )
#print.difftime
print.difftime(x = ,...)
print.difftime(x = ,digits = )
#print.Dlist
print.Dlist(x = ,...)
print.Dlist(x = )
#print.DLLInfo
print.DLLInfo(x = ,...)
print.DLLInfo(x = )
#print.DLLInfoList
print.DLLInfoList(x = ,...)
print.DLLInfoList(x = )
#print.DLLRegisteredRoutines
print.DLLRegisteredRoutines(x = ,...)
print.DLLRegisteredRoutines(x = )
#print.eigen
print.eigen(x = ,...)
print.eigen(x = )
#print.factor
print.factor(x = ,...)
print.factor(x = ,quote = ,max.levels = ,width = ,)
#print.function
print.function(x = ,...)
print.function(x = ,useSource = )
#print.hexmode
print.hexmode(x = ,...)
print.hexmode(x = )
#print.libraryIQR
print.libraryIQR(x = ,...)
print.libraryIQR(x = )
#print.listof
print.listof(x = ,...)    
#print.NativeRoutineList
print.NativeRoutineList(x = ,...)
print.NativeRoutineList(x = )
#print.noquote
print.noquote(x = ,...)
print.noquote(x = ,quote = ,right = )
#print.numeric_version
print.numeric_version(x = ,...)
print.numeric_version(x = ,quote = )

#print.octmode
print.octmode(x = ,...) 
print.octmode(x = )
#print.packageInfo
print.packageInfo(x = ,...)
print.packageInfo(x = )
#print.POSIXct
print.POSIXct(x = ,...)
print.POSIXct(x = ,tz = ,usetz = ,max = )
#print.POSIXlt
print.POSIXlt(x = ,...)
print.POSIXlt(x = ,tz = ,usetz = ,max = )
#print.proc_time
print.proc_time(x = ,...)
print.proc_time(x = )
#print.restart
print.restart(x = ,...)
print.restart(x = )
#print.rle
print.rle(x = ,...)
print.rle(x = ,digits = ,prefix = )
#print.simple.list
print.simple.list(x = ,...)
print.simple.list(x = )
#print.srcfile
print.srcfile(x = ,...)
print.srcfile(x = )
#print.srcref
print.srcref(x = ,...)
print.srcfile(x = )
#print.summary.table
print.summary.table(x = ,...)
print.summary.table(x = ,digits = )
#print.summary.warnings
print.summary.warnings(x = ,...)
print.summary.warnings(x = )
#print.summaryDefault
print.summaryDefault(x = ,...)
print.summaryDefault(x = ,digits = )
#print.table
print.table(x = ,...)
print.table(x = ,digits = ,quote = ,na.print = ,zero.print = ,right = ,justify = )
#print.warnings
print.warnings(x = ,...)
print.warnings(x = ,tags = ,header = )
#prmatrix
prmatrix(x = ,quote = ,right = ,max = ,...)
prmatrix(x = ,rowlab = ,collab = ,quote = ,right = ,na.print = )
#proc.time
proc.time()
#prod
prod(...)   
prod(na.rm = )
#prop.table
prop.table(x = ,margin = ,...)
prop.table(x = ,margin = )
#proportions
proportions(x = ,margin = ,...)
proportions(x = ,margin = )
#provideDimnames
provideDimnames(x = ,value = )
provideDimnames(x = ,sep = ,base = ,unique = )
#psigamma
psigamma(x = ,deriv = )
psigamma(x = ,deriv = )
#pushBack
pushBack(x = ,value = )
pushBack(data = ,connection = ,newLine = ,encoding = )
#pushBackLength
pushBackLength(x = ,value = )
#q
q(...)
q(save = ,status = ,runLast = )
#qr
qr(x = ,...)
qr(x = )
#qr.coef
qr.coef(qr = ,y = )
#qr.default
qr.default(x = ,tol = )
#qr.fitted
qr.fitted(qr = ,y = )
#qr.Q
qr.Q(qr = )
#qr.qty
qr.qty(qr = ,y = ) 
#qr.qy
qr.qy(qr = ,y = )
#qr.R
qr.R(qr = )
#qr.resid
qr.resid(qr = ,y = )
#qr.solve
qr.solve(qr = ,y = )
#qr.X
qr.X(qr = )
#quarters
quarters(x = ,abbreviate = )
#quarters.Date
quarters.Date(x = ,abbreviate = )
#quarters.POSIXt
quarters.POSIXt(x = ,abbreviate = )
#quit
quit(save = ,status = ,runLast = )
quit(save = ,status = ,runLast = )
#quote
quote(...)
quote(expr = )
#R_system_version
R_system_version()
R_system_version(x = ,strict = )
#R.home
R.home(component = )
R.home(component = )
#R.version
R.version()
R.version
#R.Version
R.Version()
R.Version()
#R.version.string
R.version.string()
R.version.string
#range
range(...)
range(na.rm = )
#range.default
range.default(...)
range.default(na.rm = ,finite = )
#rank
rank(x = ,ties.method = ,...)
rank(x = ,na.last = ,ties.method = )
#rapply
rapply(object = ,f = ,...,how = ,classes = ,deflt = )
#raw
raw(length = )
#rawConnection
rawConnection(con = ,open = ,blocking = ,encoding = ,text = ,raw = ,local = )
#rawConnectionValue
rawConnectionValue(con = )
#rawShift
rawShift(con = ,n = )
#rawToBits
rawToBits(x = )
rawToBits(x = )
#rawToChar
rawToChar(x = ,multiple = )
#rbind
rbind(...)
rbind(deparse.level = )
#rbind.data.frame
rbind.data.frame(...)
rbind.data.frame(deparse.level = ,make.row.names = ,stringsAsFactors = ,factor.exclude =)
#rcond
rcond(x = ,norm = )
#Re
Re(x = )
#read.dcf
read.dcf(file = ,fields = ,all = ,sub = ,encoding = )
#readBin
readBin(con = ,what = ,n = ,size = ,signed = ,endian = )
#readChar
readChar(con = ,nchars = ,useBytes = ,encoding = )
#readline
readline(prompt = ,default = ,...)
readline(prompt = )
#readLines
  readLines(con = ,n = ,ok = ,warn = ,encoding = ,skipNul = )
#readRDS
readRDS(file = )
#readRenviron
readRenviron(file = )
#Recall
Recall(...)
Recall()
#Reduce
Reduce(f = ,x = ,init = ,right = ,accumulate = )
#reg.finalizer
reg.finalizer(x = ,f = ,onexit = ,weak = )
#regexec
regexec(pattern = ,text = ,ignore.case = ,perl = ,fixed = ,useBytes = )
#regexpr
regexpr(pattern = ,text = ,ignore.case = ,perl = ,fixed = ,useBytes = )
#registerS3method
registerS3method(generic = ,class = ,method = )
#registerS3methods
registerS3methods(generic = ,classes = ,methods = )
#regmatches
regmatches(x = ,m = ,invert = )
#regmatches<-
`regmatches<-`(x = ,m = ,value = )
`regmatches<-`()
#remove
remove(...)
remove(list = ,pos = ,envir = ,inherits = )
#removeTask
#rep
rep(x = ,...)
rep(x = )
#rep_len
rep_len(x = ,length.out = )
#rep.Date
rep.Date(x = ,times = )
#rep.difftime
rep.difftime(x = ,times = )
#rep.factor
rep.factor(x = ,times = )
rep.factor(x = )
#rep.int
rep.int(x = ,times = )
rep.int(x = ,times = )
#rep.numeric_version
rep.numeric_version(x = ,times = )
rep.numeric_version(x = )
#rep.POSIXct
rep.POSIXct(x = ,times = )
rep.POSIXct(x = )
#rep.POSIXlt
rep.POSIXlt(x = ,times = )
#repeat
repeat
#replace
replace(x = ,list = ,values = )
#replicate
replicate(n = ,expr = ,simplify = )
#require
require(package = ,lib.loc = ,quietly = ,warn.conflicts = ,character.only = )
#requireNamespace
requireNamespace(package = ,lib.loc = ,quietly = )
#restartDescription
restartDescription(restart = )
#restartFormals
restartFormals(restart = )
#retracemem
retracemem(x = )
#return
return(value = )
#returnValue
returnValue(expr = )
#rev
rev(x = )
#rev.default
rev.default(x = )
#rle
rle(x = )
#rm
rm(...)
rm(list = ,pos = ,envir = ,inherits = )
#RNGkind
RNGkind(kind = ,normal.kind = )
#RNGversion
RNGversion()
#round
round(x = ,digits = )
#round.Date
round.Date(x = ,digits = )
#round.POSIXt
round.POSIXt(x = ,digits = )
#row
row(x = )
#row.names
row.names(x = )
#row.names.data.frame
row.names.data.frame(x = )
#row.names.default
row.names.default(x = )
#row.names<-
`row.names<-`(x = ,value = )
`rownames<-`()
#row.names<-.data.frame
`row.names<-.data.frame`(x = ,value = )
`row.names<-.data.frame`()
#row.names<-.default
`row.names<-.default`(x = ,value = )
`row.names<-.default`()
#rowMeans
rowMeans(x = ,na.rm = )
rowMeans(x = ,na.rm = ,dims = )
#rownames
rownames(x = )
rownames(x = ,do.NULL = ,prefix = )
#rownames<-
`rownames<-`(x = ,value = )
`rownames<-`()
#rowsum
rowsum(x = ,group = ,reorder = ,...)
rowsum(x = ,group = ,reorder = )
#rowsum.data.frame
rowsum.data.frame(x = ,group = ,reorder = ,...)
rowsum.data.frame(x = ,group = ,reorder = ,na.rm = )
#rowsum.default
rowsum.default(x = ,group = ,reorder = ,...)
rowsum.default(x = ,group = ,reorder = ,na.rm = )
#rowSums
rowSums(x = ,na.rm = )
#sample
sample(x = ,size = ,replace = ,prob = )
#sample.int
sample.int(n = ,size = ,replace = ,prob = )
#sapply
sapply(X = ,FUN = ,...,simplify = ,USE.NAMES = )
#save
save(...,list = ,file = ,ascii = ,version = ,envir = ,compress = ,compression_level = )
#save.image
save.image(file = ,compress = ,compression_level = )
#saveRDS
saveRDS(object = ,file = ,compress = ,compression_level = )
#scale
scale(x = ,center = ,scale = )
#scale.default
scale.default(x = ,center = ,scale = )
#scan
scan(file = ,what = ,nmax = ,n = ,sep = ,quote = ,dec = ,skip = ,nlines = ,na.strings = ,flush = ,fill = ,strip.white = ,quiet = ,blank.lines.skip = ,multi.line = ,comment.char = ,allowEscapes = ,encoding = ,skipNul = )
#search
search()
#searchpaths
searchpaths(which = )
#seek
seek(con = ,where = ,origin = )
#seek.connection
seek.connection(con = ,where = ,origin = )
#seq
seq(...)
seq()
#seq_aloeng
seq_along(along.with = )
#seq_len
seq_len(length.out = )
#seq.Date
seq.Date(from = ,to = ,by = )
#seq.default
seq.default(from = ,to = ,by = )
#seq.int
seq.int(from = ,to = ,by = )
#seq.POSIXt
seq.POSIXt(from = ,to = ,by = )
#sequence
sequence(nvec = )
#sequence.default
sequence.default(nvec = )
#serialize
serialize(object = ,connection = ,ascii = ,xdr = )
#serverSocket
serverSocket(port = ,host = ,backlog = ,blocking = ,timeout = ,reuse = ,encoding = ,text = )
#set.seed
set.seed(seed = )
#setdiff
setdiff(x = ,y = )
#setequal
setequal(x = ,y = )
#setHook
setHook(which = ,fun = ,package = ,lib.loc = )
#setNamespaceInfo
setNamespaceInfo(ns = ,info = ,field = ,value = )
#setSessionTimeLimit
setSessionTimeLimit(elapsed = )
#setTimeLimit
setTimeLimit(elapsed = )
#setwd
setwd(dir = )
#showConnections
showConnections(all = ,...)
showConnections(all = )
#shQuote
shQuote(x = )
#sign
sign(x = )
#signalCondition
signalCondition(cond = )
#signif
signif(x = ,digits = )
#simpleCondition
simpleCondition(message = ,call = )
#simpleError
simpleError(message = )
#simpleMessage
simpleMessage(message = )
#simpleWarning
simpleWarning(message = )
#simplify2array
simplify2array(x = ,higher = )
#sin
sin(x = )
#single
single(length = )
#sinh
sinh(x = )
#sink
sink(file = ,append = ,type = ,split = ,...)
sink(file = ,append = ,type = ,split = )
#sink.number
sink.number()
#sinpi
sinpi(x = )
#slice.index
slice.index(x = ,drop = )
#socketAccept
socketAccept(con = ,server = ,timeout = ,blocking = ,encoding = ,text = )
#socketConnection
socketConnection(host = ,port = ,server = ,blocking = ,open = ,timeout = ,encoding = ,text = )
#socketSelect
socketSelect(con = ,timeout = ,blocking = ,encoding = ,text = )
#socketTimeout
socketTimeout(con = ,timeout = ,blocking = ,encoding = ,text = )
#solve
solve(a = ,b = ,tol = ,LINPACK = ,...)
solve(a = ,b = )
#solve.default
solve.default(a = ,b = ,tol = ,LINPACK = ,...)
solve.default(a = ,b = ,tol = ,LINPACK = )
#solve.qr
solve.qr(qr = ,b = ,tol = )
#sort
sort(x = ,decreasing = ,method = )
#sort.default
sort.default(x = ,decreasing = ,method = )
#sort.int
sort.int(x = ,decreasing = )
#sort.list
sort.list(x = ,decreasing = )
#sort.POSIXlt
sort.POSIXlt(x = ,decreasing = )
#source
  source(file = ,local = ,echo = ,print.eval = ,exprs = ,spaced = ,verbose = ,prompt.echo = ,max.deparse.length = ,width.cutoff = ,deparseCtrl = ,chdir = ,encoding = ,continue.echo = ,skip.echo = ,keep.source = )
source(file = ,local = ,echo = ,print.eval = ,exprs = ,spaced = ,verbose = ,prompt.echo = ,max.deparse.length = ,width.cutoff = ,deparseCtrl = ,chdir = ,encoding = ,continue.echo = ,skip.echo = ,keep.source = )
#split
split(x = ,f = ,drop = ,...)
split(x = ,f = ,drop = )
#split.data.frame
split.data.frame(x = ,f = ,drop = ,...)
split.data.frame(x = ,f = ,drop = )
#split.Date
split.Date(x = ,f = ,drop = ,...)
split.Date(x = ,f = ,drop = )
#split.default
split.default(x = ,f = ,drop = ,...)
split.default(x = ,f = ,drop = ,sep = ,lex.order = )
#split.POSIXct
split.POSIXct(x = ,f = ,drop = ,...)
split.POSIXct(x = ,f = ,drop = )
#split<-
`split<-`(x = ,f = ,drop = ,...)
`split<-`()
#split<-.data.frame
`split<-.data.frame`(x = ,f = ,drop = ,...)
`split<-.data.frame`()
#split<-.default
`split<-.default`(x = ,f = ,drop = ,...)
`split<-.default`()
#sprintf
sprintf(fmt = ,...)
sprintf(fmt = )
#sqrt
sqrt(x = )
#sQuote
sQuote(x = )
#srcfile
srcfile(file = ,encoding = )
#srcfilealias
srcfilealias(file = ,encoding = )
#srcfilecopy
srcfilecopy(file = ,encoding = )
#srcref
srcref(file = ,srcfile = ,wholeSrcref = )
#standardGeneric
standardGeneric(f = ,def = ,group = ,valueClass = ,signature = ,package = ,useAsDefault = )
#startsWith
startsWith(x = ,prefix = ,ignore.case = )
#stderr
stderr()
stderr()
#stdin
stdin()
stdin()
#stdout
stdout()
stdout()
#stop
stop(...)
stop(call. = ,domain = )
#stopifnot
stopifnot(...)
stopifnot(exprs = ,exprObject = ,local = )
#storage.mode
storage.mode(x = )
#storage.mode<-
`storage.mode<-`(x = ,value = )
#str2expression
str2expression(x = )
#str2lang
str2lang(x = )
#strftime
strftime(x = ,format = ,tz = ,usetz = )
#strptime
strptime(x = ,format = ,tz = ,usetz = )
#strrep
strrep(str = ,times = )
#strsplit
strsplit(x = ,split = ,fixed = ,perl = ,useBytes = )
#strtoi
strtoi(x = ,base = ,strict = )
#strtrim
strtrim(str = ,width = )
#structure
structure(.Data = ,class = ,...)
structure(.Data = )
#strwrap
strwrap(str = ,width = ,indent = ,exdent = ,prefix = ,initial = ,strict = )
#sub
sub(pattern = ,replacement = ,x = ,ignore.case = ,perl = ,fixed = ,useBytes = )
#subset
subset(x = ,subset = ,select = ,drop = )
#subset.data.frame
subset.data.frame(x = ,subset = ,select = ,drop = )
#subset.default
subset.default(x = ,subset = ,select = ,drop = )
#subset.matrix
subset.matrix(x = ,subset = ,select = ,drop = )
#substitute
substitute(expr = ,env = )
#substr
substr(x = ,start = ,stop = )
#substr<-
`substr<-`(x = ,start = ,stop = ,value = )
`substr<-`()
#substring
substring(text = ,first = ,last = )
substring(text = ,first = ,last = )
#substring<-
`substring<-`(text = ,first = ,last = ,value = )
`substring<-`()
#sum
sum(...)
sum(na.rm = )
#summary
summary(object = ,...)
summary(object = )
#summary.connection
summary.connection(con = ,...)
summary.connection(object = )
#summary.data.frame
summary.data.frame(object = ,...)
summary.data.frame(object = ,maxsum = ,digits = )
#Summary.data.frame
Summary.data.frame(object = ,...)
Summary.data.frame(na.rm = )
#summary.Date
summary.Date(object = ,...)
summary.Date(object = ,digits = )
#Summary.Date
Summary.Date(object = ,...)
Summary.Date(na.rm = )
#summary.default
summary.default(object = ,...)
summary.default(object = ,digits = ,quantile.type = )
#Summary.difftime
Summary.difftime(object = ,...)
Summary.difftime(na.rm = )
#summary.factor
summary.factor(object = ,...)
summary.factor(object = ,maxsum = )
#Summary.factor
Summary.factor(object = ,...)
Summary.factor(na.rm = )
#summary.matrix
summary.matrix(object = ,...)
summary.matrix(object = )
#Summary.numeric_version
Summary.numeric_version(object = ,...)
Summary.numeric_version(na.rm = )
#Summary.ordered
Summary.ordered(object = ,...)
Summary.ordered(na.rm = )
#summary.POSIXct
summary.POSIXct(object = ,...)
Summary.POSIXct(na.rm = )
#Summary.POSIXct
Summary.POSIXct(object = ,...)
Summary.POSIXct(na.rm = )
#summary.POSIXlt
summary.POSIXlt(object = ,...)
summary.POSIXlt(object = ,digits = )
#Summary.POSIXlt
Summary.POSIXlt(object = ,...)
Summary.POSIXlt(na.rm = )
#summary.proc_time
summary.proc_time(object = ,...)
summary.proc_time(object = )

#summary.srcfile
summary.srcfile(object = ,...)
summary.srcfile(object = )

#summary.srcref
summary.srcref(object = ,...)
summary.srcref(object = ,useSource = )
#summary.table
summary.table(object = ,...)
summary.table(object = )

#summary.warnings
summary.warnings(object = ,...)
summary.warnings(object = )

#suppressMessages
suppressMessages(expr = )
#suppressPackageStartupMessages
suppressPackageStartupMessages(expr = )
suppressPackageStartupMessages(expr = )
#suppressWarnings
suppressWarnings(expr = )
suppressWarnings(expr = ,classes = )
#suspendInterrupts
suspendInterrupts()
suspendInterrupts(expr = )
#svd
svd(x = ,nu = ,nv = ,LINPACK = ,...)
svd(x = ,nu = ,nv = ,LINPACK = )
#sweep
sweep(x = ,MARGIN = ,STATS = ,FUN = ,...)
sweep(x = ,MARGIN = ,STATS = ,FUN = ,check.margin = )

#switch
switch(expr = ,...)
switch(EXPR = )
#sys.call
sys.call(which = )
sys.call(which = )
#sys.calls
sys.calls()
sys.calls()

#Sys.chmod
Sys.chmod(file = ,mode = ,use_umask = )
#Sys.Date
Sys.Date()
#sys.frame
sys.frame(which = )
#sys.frames
sys.frames()
#sys.function
sys.function(which = )

#Sys.getenv
Sys.getenv(x = )
#Sys.getlocale
Sys.getlocale(category = )
#Sys.getpid
Sys.getpid()
#Sys.glob
Sys.glob(pattern = ,recursive = ,full.names = )
#Sys.info
Sys.info()
#sys.load.image
sys.load.image(file = )
#Sys.localeconv
Sys.localeconv()
#sys.nframe
sys.nframe()
#sys.on.exit
sys.on.exit(expr = ,add = )
#sys.parent
sys.parent(which = )
#sys.parents
sys.parents()
#Sys.readlink
Sys.readlink(path = )
#sys.save.image
sys.save.image(file = )
#Sys.setenv
Sys.setenv(...)
Sys.setenv()
#Sys.setFileTime
Sys.setFileTime(path = ,times = )
#Sys.setlocale
Sys.setlocale(category = ,locale = )
#Sys.sleep
Sys.sleep(secs = )
#sys.source
sys.source(file = ,envir = ,...)
sys.source(file = ,envir = ,chdir = ,keep.source = ,keep.parse.data = ,toplevel.env = )
#sys.status
sys.status()
#Sys.time
Sys.time()
#Sys.timezone
Sys.timezone(location = )

#Sys.umask
Sys.umask(mode = )
#Sys.unsetenv
Sys.unsetenv(x = )
#Sys.which
Sys.which(names = ,all = ,path = )
#system
system(command = ,intern = ,ignore.stderr = ,ignore.stdout = ,wait = ,input = ,show.output.on.console = ,minimized = ,invisible = ,...)
system(command = ,intern = ,ignore.stdout = ,ignore.stderr = ,wait = ,input = ,show.output.on.console = ,minimized = ,invisible = ,timeout = )
#system.file
system.file(package = ,...)
system.file(package = ,lib.loc = ,mustWork = )
#system.time
system.time(expr = )
#system2
system2(command = ,args = ,stdout = ,stderr = ,stdin = ,input = ,env = ,wait = ,show.output.on.console = ,minimized = ,invisible = ,...)
system2(command = ,args = ,stdout = ,stderr = ,stdin = ,input = ,env = ,wait = ,minimized = ,invisible = ,timeout = )
#t
t(x = )
#T
T(x = )
#t.data.frame
t.data.frame(x = )
#t.default
t.default(x = )
#table
table(...)
table(exclude = ,useNA = ,dnn = ,deparse.level = )
#tabulate
tabulate(x = ,nbins = )
#tan
tan(x = )
tan(x = )
#tanh
tanh(x = )
#tanpi
tanpi(x = )
#tapply
tapply(X = ,INDEX = ,FUN = ,...,simplify = ,default = )
#taskCallbackManager
taskCallbackManager()
#tcrossprod
tcrossprod(x = ,y = )
#tempdir
tempdir()
#tempfile
tempfile(pattern = ,tmpdir = )
#textConnection
textConnection(value = ,encoding = ,local = )
#textConnectionValue
textConnectionValue(con = )
#tolower
tolower(x = )
#topenv
topenv(env = )
#toString
toString(x = )
#toString.default
toString.default(x = )
#toupper
toupper(x = )
#trace
trace(what = ,tracer = ,exit = ,at = ,print = ,signature = ,where = ,edit = )
#traceback
traceback(x = ,max.lines = )
#tracemem
tracemem(x = )
#tracingState
tracingState(on = )

#transform
transform(x = ,...)
transform(`_data` = )
#transform.data.frame
transform.data.frame(x = ,...)
transform.data.frame(`_data` = )
#transform.default
transform.default(x = ,...)
transform.default(`_data` = )

#trigamma
trigamma(x = )
#trimws
trimws(x = ,which = c("both","left","right"))
#trunc
trunc(x = )
#trunc.Date
trunc.Date(x = )
#trunc.POSIXt
trunc.POSIXt(x = )
#truncate
truncate(x = )
#truncate.connection
truncate.connection(con = )
#try
try(expr = ,silent = )
#tryCatch
tryCatch(expr = ,...,finally = ,silent = )
#tryInvokeRestart
tryInvokeRestart(what = ,...)
tryInvokeRestart(r = )
#typeof
typeof(x = )
#unclass
unclass(x = )
#undebug
undebug(f = )
#union
union(x = ,y = )
#unique
unique(x = ,...)
unique(x = ,incomparables = )
#unique.array
unique.array(x = ,...)
unique.array(x = ,incomparables = ,MARGIN = ,fromLast = )
#unique.data.frame
unique.data.frame(x = ,...)
unique.data.frame(x = ,incomparables = ,fromLast = )
#unique.default
unique.default(x = ,...)
unique.default(x = ,incomparables = ,fromLast = ,nmax = )
#unique.matrix
unique.matrix(x = ,...)
unique.matrix(x = ,incomparables = ,MARGIN = ,fromLast = )
#unique.numeric_version
unique.numeric_version(x = ,...)
unique.numeric_version(x = ,incomparables = )
#unique.POSIXlt
unique.POSIXlt(x = ,...)
unique.POSIXlt(x = ,incomparables = )
#unique.warnings
unique.warnings(x = ,...)
unique.warnings(x = ,incomparables = )
#units
units(x = )
#units.difftime
units.difftime(x = )
#units<-
`units<-`(x = ,value = )
#units<-.difftime
`units<-.difftime`(x = ,value = )
#unix.time
unix.time()
#unlink
unlink(x = ,recursive = ,force = )
#unlist
unlist(x = ,recursive = ,use.names = )
#unloadNamespace
unloadNamespace(package = )
#unlockBinding
unlockBinding(sym = ,env = )
#unname
unname(x = )
#unserialize
unserialize(x = ,refhook = )
#unsplit
unsplit(value = ,f = ,drop = )
#untrace
untrace(what = ,signature = ,where = )
#untracemem
untracemem(x = )
#unz
unz(description = ,filename = ,open = ,encoding = ,text = )
#upper.tri
upper.tri(x = ,diag = )
#url
url(description = ,open = ,encoding = ,text = )
#UseMethod
UseMethod(generic = ,object = )
#utf8ToInt
utf8ToInt(x = )
#validEnc
validEnc(x = )
#validUTF8
validUTF8(x = )
#vapply
vapply(X = ,FUN = ,FUN.VALUE = ,...,USE.NAMES = )
#vector
vector(mode = ,length = )
#Vectorize
Vectorize(f = ,SIMPLIFY = ,USE.NAMES = )
#version
version()
#warning
warning(...)
warning(call. = ,immediate. = ,noBreaks. = ,domain = )
#warningCondition
warningCondition(message = ,call = )
#warnings
warnings(...)
warnings()
#weekdays
weekdays(x = ,abbreviate = )
#weekdays.Date
weekdays.Date(x = ,abbreviate = )
#weekdays.POSIXt
weekdays.POSIXt(x = ,abbreviate = )
#which
which(x = ,arr.ind = ,useNames = )
#which.max
which.max(x = )
#which.min
which.min(x = )
which.min(x = )
#while
while(test = ,expr = )
while()
  #with
with(data = ,expr = ,...)
with(data = ,expr = )
#with.default
with.default(data = ,expr = ,...)
with.default(data = ,expr = )

#withAutoprint
withAutoprint(expr = )

#withCallingHandlers
withCallingHandlers(expr = ,...)
withCallingHandlers(expr = )
#within
within(data = ,expr = ,...)
within(data = ,expr = )
#within.data.frame
within.data.frame(data = ,expr = ,...)
within.data.frame(data = ,expr = )

#within.list
within.list(data = ,expr = ,...)
within.list(data = ,expr = ,keepAttrs = )
#withRestarts
withRestarts(expr = ,...)
withRestarts(expr = )
#withVisible
withVisible(expr = )
#write
write(x = ,file = ,nmax = ,append = ,sep = ,na = ,...)
write(x = ,file = ,ncolumns = ,append = ,sep = )
#write.dcf
write.dcf(x = ,file = ,...)
write.dcf(x = ,file = ,append = ,useBytes = ,indent = ,width = ,keep.white = )
#writeBin
writeBin(object = ,con = ,size = ,...)
writeBin(object = ,con = ,size = ,endian = ,useBytes = )
#writeChar
writeChar(object = ,con = ,eos = ,useBytes = )
writeChar(object = ,con = ,nchars = ,eos = ,useBytes = )
#writeLines
writeLines(text = ,con = ,sep = ,useBytes = )
writeLines(text = ,con = ,sep = ,useBytes = )
#xor
xor(x = ,y = )
#xpdrows.data.frame
xpdrows.data.frame(x = ,...)
xpdrows.data.frame(x = ,old.rows = ,new.rows = )
#xtfrm
xtfrm(x = )
#xtfrm.AsIs
xtfrm.AsIs(x = )
#xtfrm.data.frame
xtfrm.data.frame(x = )
#xtfrm.Date
xtfrm.Date(x = )
#xtfrm.default
xtfrm.default(x = )
#xtfrm.difftime
xtfrm.difftime(x = )
#xtfrm.factor
xtfrm.factor(x = )
#xtfrm.numeric_version
xtfrm.numeric_version(x = )
#xtfrm.POSIXct
xtfrm.POSIXct(x = )
#xtfrm.POSIXlt
xtfrm.POSIXlt(x = )
#xzfile
xzfile(description = ,open = ,encoding = ,text = )
#zapsmall
zapsmall(x = ,digits = )
zapsmall(x = ,digits = )
#abline
abline(a = ,b = ,h = ,v = ,reg = ,coef = ,untf = )
abline(a = ,b = ,h = ,v = ,reg = ,coef = ,untf = )
#arrows
arrows(x0 = ,y0 = ,x1 = ,y1 = ,angle = ,code = ,length = ,...)
arrows(x0 = ,y0 = ,x1 = ,y1 = ,length = ,angle = ,code = ,col = ,lty = ,lwd = )
#assocplot
assocplot(x = ,...)
assocplot(x = ,col = ,space = ,main = ,xlab = ,ylab = )
#axis
axis(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,hadj = ,padj = ,...)
axis(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,hadj = ,padj = ,gap.axis = )
#Axis
Axis(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,hadj = ,padj = ,...)
Axis(x = ,at = ,side = ,labels = )
#axis.Date
axis.Date(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,format = ,digits = ,cex.axis = ,tcl = ,mgp = ,las = ,...)
axis.Date(side = ,x = ,at = ,format = ,labels = )
#axis.POSIXct
axis.POSIXct(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,format = ,digits = ,cex.axis = ,tcl = ,mgp = ,las = ,...)
axis.POSIXct(side = ,x = ,at = ,format = ,labels = )
#axTicks
axTicks(side = ,...)
axTicks(side = ,axp = ,usr = ,log = ,nintLog = )
#barplot
barplot(...)
barplot(height = )
#barplot.default
barplot.default(...)
barplot.default(height = ,width = ,space = ,names.arg = ,legend.text = ,beside = ,horiz = ,density = ,angle = ,col = ,border = ,main = ,sub = ,xlab = ,ylab = ,xlim = ,ylim = ,xpd = ,log = ,axes = ,axisnames = ,cex.axis = ,cex.names = ,inside = ,plot = ,axis.lty = ,offset = ,add = ,ann = ,args.legend = )
#box
boxplot(x = ,...)
boxplot(x = )
#boxplot
boxplot(...)
boxplot(x = )
#boxplot.default
boxplot.default(...)
boxplot.default(x = ,range = ,width = ,varwidth = ,notch = ,outline = ,names = ,plot = ,border = ,col = ,log = ,pars = ,ann = ,horizontal = ,add = ,at = )
#boxplot.matrix
boxplot.matrix(x = ,...)
boxplot.matrix(x = ,use.cols = )
#bxp
bxp(list = ,horizontal = ,...)
bxp(z = ,notch = ,width = ,varwidth = ,outline = ,notch.frac = ,log = ,border = ,pars = ,frame.plot = ,horizontal = ,ann = ,add = ,at = ,show.names = )
#cdplot
cdplot(x = ,...)
cdplot(x = )
#clip
clip(x = ,y = )
clip(x1 = ,x2 = ,y1 = ,y2 = )
#close.screen
close.screen(which = )
#co.intervals
co.intervals(x = ,...)
co.intervals(x = ,number = ,overlap = )
#contour
contour(x = ,y = ,z = ,nlevels = ,levels = ,labels = ,xlim = ,ylim = ,zlim = ,zlab = ,main = ,sub = ,xlab = ,ylab = ,asp = ,axes = ,frame.plot = ,...)
contour(x = )
#contour.default
contour.default(x = ,y = ,z = ,nlevels = ,levels = ,labels = ,xlim = ,ylim = ,zlim = ,zlab = ,main = ,sub = ,xlab = ,ylab = ,asp = ,axes = ,frame.plot = ,...)
contour.default(x = ,y = ,z = ,nlevels = ,levels = ,labels = ,xlim = ,ylim = ,zlim = ,labcex = ,drawlabels = ,method = ,vfont = ,axes = ,frame.plot = ,col = ,lty = ,lwd = ,add = )
#coplot
coplot(formula = ,data = ,...)
coplot(formula = ,data = ,given.values = ,panel = ,rows = ,columns = ,show.given = ,col = ,pch = ,bar.bg = ,xlab = ,ylab = ,subscripts = ,axlabels = ,number = ,overlap = ,xlim = ,ylim = )
#curve
curve(expr = ,from = ,to = ,n = ,add = ,type = ,xname = ,xaxs = ,yaxs = ,...)
curve(expr = ,from = ,to = ,n = ,add = ,type = ,xname = ,xlab = ,ylab = ,log = ,xlim = )
#dotchart
dotchart(x = ,labels = ,groups = ,gcolor = ,color = ,cex = ,pch = ,pt.cex = ,bg = ,main = ,xlab = ,ylab = ,...)
dotchart(x = ,labels = ,groups = ,gdata = ,offset = ,ann = ,xaxt = ,frame.plot = ,log = ,cex = ,pt.cex = ,pch = ,gpch = ,bg = ,color = ,gcolor = ,lcolor = ,xlim = ,main = ,xlab = ,ylab = )
#erase.screen
erase.screen(which = )
#filled.contour
filled.contour(x = ,y = ,z = ,levels = ,color.palette = ,color = ,plot.title = ,plot.axes = ,key.title = ,key.axes = ,asp = ,xaxs = ,yaxs = ,...)
filled.contour(x = ,y = ,z = ,xlim = ,ylim = ,zlim = ,levels = ,nlevels = ,color.palette = ,col = ,plot.title = ,plot.axes = ,key.title = ,key.axes = ,asp = ,xaxs = ,yaxs = ,las = ,axes = ,frame.plot = )
#fourfoldplot
fourfoldplot(x = ,color = ,conf.level = ,margin = ,main = ,...)
fourfoldplot(x = ,color = ,conf.level = ,std = ,margin = ,space = ,main = ,mfrow = ,mfcol = )
#frame
frame()
#grconvertX
grconvertX(x = ,from = ,to = )
#grconvertY
grconvertY(y = ,from = ,to = )
grconvertY(y = ,from = ,to = )
#grid
grid(nx = ,ny = ,col = ,lty = ,lwd = ,equilogs = ,...)
grid(nx = ,ny = ,col = ,lty = ,lwd = ,equilogs = )
#hist
hist(x = ,breaks = ,freq = ,probability = ,include.lowest = ,right = ,density = ,angle = ,col = ,border = ,main = ,xlim = ,ylim = ,xlab = ,ylab = ,axes = ,plot = ,labels = ,nclass = ,warn.unused = ,offset = ,...)
hist(x = )
#hist.default
hist.default(x = ,breaks = ,freq = ,probability = ,include.lowest = ,right = ,density = ,angle = ,col = ,border = ,main = ,xlim = ,ylim = ,xlab = ,ylab = ,axes = ,plot = ,labels = ,nclass = ,warn.unused = ,offset = ,...)
hist.default(x = ,breaks = ,freq = ,probability = ,include.lowest = ,right = ,fuzz = ,density = ,angle = ,col = ,border = ,main = ,xlim = ,ylim = ,xlab = ,ylab = ,axes = ,plot = ,labels = ,nclass = ,warn.unused = )
#identify
identify(x = ,n = ,plot = ,labels = ,offset = ,...)
identify(x = )
#image
image(x = ,y = ,z = ,xlim = ,ylim = ,zlim = ,axes = ,xlab = ,ylab = ,breaks = ,col = ,col.axis = ,col.lab = ,...)
image(x = )
#image.default
image.default(x = ,y = ,z = ,xlim = ,ylim = ,zlim = ,axes = ,xlab = ,ylab = ,breaks = ,col = ,col.axis = ,col.lab = ,...)
image.default(x = ,y = ,z = ,zlim = ,xlim = ,ylim = ,col = ,add = ,xaxs = ,yaxs = ,xlab = ,ylab = ,breaks = ,oldstyle = ,useRaster = )
#layout
layout(...)
layout(mat = ,widths = ,heights = ,respect = )
#layout.show
layout.show(n = ,...)
layout.show(n = )
#lcm
lcm(x = )

#legend
legend(x = ,y = ,legend = ,fill = ,col = ,border = ,lty = ,lwd = ,pch = ,angle = ,density = ,bty = ,bg = ,box.lwd = ,box.lty = ,box.col = ,pt.bg = ,cex = ,pt.cex = ,pt.lwd = ,xjust = ,yjust = ,x.intersp = ,y.intersp = ,adj = ,text.width = ,text.col = ,text.font = ,merge = ,trace = ,plot = ,ncol = ,horiz = ,title = ,inset = ,xpd = ,title.col = ,title.adj = ,seg.len = ,...)
legend(x = ,y = ,legend = ,fill = ,col = ,border = ,lty = ,lwd = ,pch = ,angle = ,density = ,bty = ,bg = ,box.lwd = ,box.lty = ,box.col = ,pt.bg = ,cex = ,pt.cex = ,pt.lwd = ,xjust = ,yjust = ,x.intersp = ,y.intersp = ,adj = ,text.width = ,text.col = ,text.font = ,merge = ,trace = ,plot = ,ncol = ,horiz = ,title = ,inset = ,xpd = ,title.col = ,title.adj = ,title.cex = ,title.font = ,seg.len = )
#lines
lines(x = ,y = ,type = ,...)
lines(x = )
#lines.default
lines.default(x = ,y = ,type = ,...)
lines.default(x = ,y = ,type = )
#locator
locator(n = ,type = ,...)
locator(n = ,type = )

#matlines
matlines(x = ,y = ,type = ,...)
matlines(x = ,y = ,type = ,lty = ,lwd = ,pch = ,col = )
#matplot
matplot(x = ,y = ,type = ,...)
matplot(x = ,y = ,type = ,lty = ,lwd = ,lend = ,pch = ,col = ,cex = ,bg = ,xlab = ,ylab = ,xlim = ,ylim = ,log = ,add = ,verbose = )
#matpoints
matpoints(x = ,y = ,type = ,...)
matpoints(x = ,y = ,type = ,lty = ,lwd = ,pch = ,col = )
#mosaicplot
mosaicplot(x = ,...)
mosaicplot(x = )
#mtext
mtext(text = ,side = ,line = ,outer = ,at = ,adj = ,padj = ,cex = ,col = ,font = ,...)
#pairs
pairs(x = ,labels = ,panel = ,lower.panel = ,upper.panel = ,diag.panel = ,text.panel = ,label.pos = ,cex.labels = ,font.labels = ,...)
#pairs.default
pairs.default(x = ,labels = ,panel = ,lower.panel = ,upper.panel = ,diag.panel = ,text.panel = ,label.pos = ,cex.labels = ,font.labels = ,...)
#panel.smooth
panel.smooth(x = ,y = ,col = ,bg = ,pch = ,cex = ,col.smooth = ,span = ,iter = ,family = ,...)
#par
par(...)
par(no.readonly = )
#persp
persp(x = ,y = ,z = ,theta = ,phi = ,expand = ,col = ,border = ,ticktype = ,nticks = ,...)
#pie
pie(x = ,labels = ,edges = ,radius = ,clockwise = ,init.angle = ,density = ,angle = ,col = ,border = ,lty = ,main = ,...)
#plot
plot(x = ,y = ,type = ,...)
plot(x = ,y = )
#plot.default
plot.default(x = ,y = ,type = ,...)
plot.default(x = ,y = ,type = ,xlim = ,ylim = ,log = ,main = ,sub = ,xlab = ,ylab = ,ann = ,axes = ,frame.plot = ,panel.first = ,panel.last = ,asp = ,xgap.axis = ,ygap.axis = )
#plot.design
plot.design(x = ,...)
plot.design(x = ,y = ,fun = ,data = ,ylim = ,xlab = ,ylab = ,main = ,ask = ,xaxt = ,axes = ,xtick = )
#plot.function
plot.function(x = ,from = ,to = ,add = ,...)
plot.function(x = ,y = ,to = ,from = ,xlim = ,ylab = )
#plot.new
plot.new()
#plot.window
plot.window(xlim = ,ylim = ,log = ,asp = ,...)
plot.window(xlim = ,ylim = ,log = ,asp = )
#plot.xy
plot.xy(xy = ,type = ,...)
plot.xy(xy = ,type = ,pch = ,lty = ,col = ,bg = ,cex = ,lwd = )
#points
points(x = ,y = ,...)
points(x = )
#points.default
points.default(x = ,y = ,...)
points.default(x = ,y = ,type = )
#polygon
polygon(x = ,y = ,...)
polygon(x = ,y = ,density = ,angle = ,border = ,col = ,lty = ,fillOddEven = )
#polypath
polypath(x = ,y = ,...)
polypath(x = ,y = ,border = ,col = ,lty = ,rule = )
#rasterImage
rasterImage(x = ,y = ,image = ,col = ,...)
rasterImage(image = ,xleft = ,ybottom = ,xright = ,ytop = ,angle = ,interpolate = )
#rect
rect(xleft = ,ybottom = ,xright = ,ytop = ,...)
rect(xleft = ,ybottom = ,xright = ,ytop = ,density = ,angle = ,col = ,border = ,lty = ,lwd = )
#rug
rug(x = ,side = ,ticksize = ,line.col = ,...)
rug(x = ,ticksize = ,side = ,lwd = ,col = ,quiet = )
#screen
screen(...)
screen(n = ,new = )
#segments
segments(x0 = ,y0 = ,x1 = ,y1 = ,...)
#smoothScatter
smoothScatter(x = ,y = ,nbin = ,xlab = ,ylab = ,main = ,sub = ,xlim = ,ylim = ,...)
#spineplot
spineplot(x = ,...)
#split.screen
split.screen(figs = ,...)
#stars
stars(x = ,...)
#stem
stem(x = ,scale = ,width = ,at = ,axes = ,...)
#strheight
strheight(x = ,units = ,...)
#stripchart
stripchart(x = ,...)
#strwidth
strwidth(x = ,units = ,...)
#sunflowerplot
sunflowerplot(x = ,...)
#symbols
symbols(x = ,y = ,...)
#text
text(x = ,y = ,labels = ,adj = ,pos = ,offset = ,vfont = ,cex = ,col = ,font = ,...)
#text.default
text.default(x = ,y = ,labels = ,adj = ,pos = ,offset = ,vfont = ,cex = ,col = ,font = ,...)
#title
title(main = ,sub = ,xlab = ,ylab = ,line = ,outer = ,...)
#xinch
xinch(x = )
#xspline
xspline(x = ,y = ,shape = ,open = ,...)
#xyinch
xyinch(x = ,y = )
#yinch
yinch(y = )
#%+%
`%+%`(e1 = ,e2 = )
#acs
acs(x = ,...)
#alpha
alpha(x = ,...)
#alpha.ci
alpha.ci(x = ,...)
#anova.psych
anova.psych(x = ,...)
#AUC
AUC(x = ,...)
#autoR
autoR(x = ,...)
#bassAckward
bassAckward(x = ,...)
#bassAckward.diagram
bassAckward.diagram(x = ,...)

#Bechtoldt
Bechtoldt(x = ,...)
#Bechtoldt.1
Bechtoldt.1(x = ,...)
#Bechtoldt.2
Bechtoldt.2(x = ,...)
#bestItems
bestItems(x = ,...)
#bestScales
bestScales(x = ,...)
#bfi
bfi(x = ,...)
#bfi.keys
bfi.keys(x = ,...)
#bi.bars
bi.bars(x = ,...)
#bifactor
bifactor(x = ,...)
#bigCor
bigCor(x = ,...)
#biplot.psych
biplot.psych(x = ,...)
#biquartimin
biquartimin(x = ,...)
#biserial
biserial(x = ,...)
#block.random
block.random(x = ,...)
#bock.table
bock.table(x = ,...)
#cattell
cattell(x = ,...)
#char2numeric
char2numeric(x = ,...)
#Chen
Chen(x = ,...)
#chi2r
chi2r(x = ,...)
#circ.sim
circ.sim(x = ,...)
#circ.sim.plot
circ.sim.plot(x = ,...)
#circ.simulation
circ.simulation(x = ,...)
#circ.tests
circ.tests(x = ,...)
#circadian.cor
circadian.cor(x = ,...)
#circadian.F
circadian.F(x = ,...)
#circadian.linear.cor
circadian.linear.cor(x = ,...)
#circadian.mean
circadian.mean(x = ,...)
#circadian.phase
circadian.phase(x = ,...)
#circadian.reliability
circadian.reliability(x = ,...)
#circadian.sd
circadian.sd(x = ,...)
#circadian.stats
circadian.stats(x = ,...)
#circular.cor
circular.cor(x = ,...)
#circular.mean
circular.mean()
#cluster.cor
cluster.cor(x = ,...)
#cluster.fit
cluster.fit(x = ,...)
#cluster.loadings
cluster.loadings(x = ,...)
#cluster.plot
cluster.plot(x = ,...)
#cluster2keys
cluster2keys(x = ,...)
#cohen.d
cohen.d(x = ,...)
#cohen.d.by
cohen.d.by(x = ,...)
#cohen.d.ci
cohen.d.ci(x = ,...)
#cohen.kappa
cohen.kappa(x = ,...)
#cohen.profile
cohen.profile(x = ,...)
#comorbidity
comorbidity(x = ,...)
#con2cat
con2cat(x = ,...)
#congeneric.sim
congeneric.sim(x = ,...)
#congruence
congruence(x = ,...)
#cor.ci
cor.ci(x = ,...)
#cor.plot
cor.plot(x = ,...)
#cor.plot.upperLowerCi
cor.plot.upperLowerCi(x = ,...)
#cor.smooth
cor.smooth(x = ,...)
#cor.smoother
cor.smoother(x = ,...)
#cor.wt
cor.wt(x = ,...)
#cor2
cor2(x = ,...)
#cor2cov
cor2cov(x = ,...)
#cor2dist
cor2dist(x = ,...)
#corCi
corCi(x = ,...)
#corFiml
corFiml(x = ,...)
#corPlot
corPlot(x = ,...)
#corPlotUpperLowerCi
corPlotUpperLowerCi(x = ,...)
#corr.p
corr.p(x = ,...)
#corr.test
corr.test(x = ,...)
#correct.cor
correct.cor(x = ,...)
#cortest
cortest(x = ,...)
#cortest.bartlett
cortest.bartlett(x = ,...)
#cortest.jennrich
cortest.jennrich(x = ,...)
#cortest.mat
cortest.mat(x = ,...)
#cortest.normal
cortest.normal(x = ,...)
#cosinor
cosinor(x = ,...)
#cosinor.period
cosinor.period(x = ,...)
#cosinor.plot
cosinor.plot(x = ,...)
#count.pairwise
count.pairwise(x = ,...)
#crossValidation
crossValidation(x = ,...)
#cs
cs(x = ,...)
#cta
cta(x = ,...)
#cta.15
cta.15(x = ,...)
#d.ci
d.ci(x = ,...)
#d.robust
d.robust(x = ,...)
#d2CL
d2CL
#d2OVL
d2OVL
#d2OVL2
d2OVL2
#d2r
d2r
#d2t
d2t
#d2U3
d2U3
#densityBy
densityBy(x = ,...)
#describe
describe(x = ,...)
#describe.by
describe.by(x = ,...)
#describeBy
describeBy(x = ,...)
#describeData
describeData(x = ,...)
#describeFast
describeFast(x = ,...)
#dia.arrow
dia.arrow(x = ,...)
#dia.cone
dia.cone(x = ,...)
#dia.curve
dia.curve(x = ,...)
#dia.curved.arrow
dia.curved.arrow(x = ,...)
#dia.ellipse
dia.ellipse(x = ,...)
#dia.ellipse1
dia.ellipse1(x = ,...)
#dia.rect
dia.rect(x = ,...)
#dia.self
dia.self(x = ,...)
#dia.shape
dia.shape(x = ,...)
#dia.triangle
dia.triangle(x = ,...)
#diagram
diagram(x = ,...)
#directSl
directSl(x = ,...)

#distance 
distance(x = ,...)
#draw.cor
draw.cor(x = ,...)
#draw.tetr
draw.tetr(x = ,...)
#dummy.code
dummy.code(x = ,...)
#Dwyer
Dwyer(x = ,...)
#eigen.loadings
eigen.loadings(x = ,...)
#ellipses
ellipses(x = ,...)
#equamax
equamax(x = ,...)
#error.bars
error.bars(x = ,...)
#error.bars.by
error.bars.by(x = ,...)
#error.bars.tab
error.bars.tab(x = ,...)
#error.crosses
error.crosses(x = ,...)
#error.dots
error.dots(x = ,...)
#errorCircles
errorCircles(x = ,...)
#esem
esem(x = ,...)
#esem.diagram
esem.diagram(x = ,...)
#extension.diagram
extension.diagram(x = ,...)
#fa
fa(x = ,...)
#fa.congruence
fa.congruence(x = ,...)
#fa.diagram
fa.diagram(x = ,...)
#fa.extend
fa.extend(x = ,...)
#fa.extension
fa.extension(x = ,...)
#fa.graph
fa.graph(x = ,...)
#fa.lookup
fa.lookup(x = ,...)
#fa.multi
fa.multi(x = ,...)
#fa.multi.diagram
fa.multi.diagram(x = ,...)
#fa.organize
fa.organize(x = ,...)
#fa.parallel
fa.parallel(x = ,...)
#fa.parallel.poly
fa.parallel.poly(x = ,...)
#fa.plot
fa.plot(x = ,...)
#fa.poly
fa.poly(x = ,...)
#fa.pooled
fa.pooled(x = ,...)
#fa.random
fa.random(x = ,...)
#fa.rgraph
fa.rgraph(x = ,...)
#fa.sapa
fa.sapa(x = ,...)
#fa.sort
fa.sort(x = ,...)

#fa.stats
fa.stats(x = ,...)
#fa2irt
fa2irt(x = ,...)
#faBy
faBy(x = ,...)
#fac
fac(x = ,...)
#faCor
faCor(x = ,...)
#factor.congruence
factor.congruence(x = ,...)
#factor.fit
factor.fit(x = ,...)
#factor.minres
factor.minres(x = ,...)
#factor.model
factor.model(x = ,...)
#factor.pa
factor.pa(x = ,...)
#factor.plot
factor.plot(x = ,...)
#factor.residuals
factor.residuals(x = ,...)

#factor.rotate
factor.rotate(x = ,...)
#factor.scores
factor.scores(x = ,...)
#factor.stats
factor.stats(x = ,...)
#factor.wls
factor.wls(x = ,...)
#factor2cluster
factor2cluster(x = ,...)
#faRotate
faRotate(x = ,...)
#faRotations
faRotations(x = ,...)
#fisherz
fisherz(x = ,...)
#fisherz2r
fisherz2r(x = ,...)
#fparse
fparse(x = ,...)
#fromTo
fromTo(x = ,...)
#g2r
g2r(x = ,...)
#Garcia
Garcia(x = ,...)
#geometric.mean
geometric.mean(x = ,...)

#glb
glb(x = ,...)
#glb.algebraic
glb.algebraic(x = ,...)
#glb.fa
glb.fa(x = ,...)

#Gleser
Gleser(x = ,...)
#Gorsuch
Gorsuch(x = ,...)
#guttman
guttman(x = ,...)
#Harman.5
Harman.5(x = ,...)
#Harman.8
Harman.8(x = ,...)
#Harman.Burt
Harman.Burt(x = ,...)

#Harman.Holzinger
Harman.Holzinger(x = ,...)

#Harman.political
Harman.political(x = ,...)

#harmonic.mean
harmonic.mean(x = ,...)
#headtail
headtail(x = ,...)
#headTail
headTail(x = ,...)
#het.diagram
het.diagram(x = ,...)
#histBy
histBy(x = ,...)
#Holzinger
Holzinger(x = ,...)

#Holzinger.9
Holzinger.9(x = ,...)
#ICC
ICC(x = ,...)
#iclust
iclust(x = ,...)
 #ICLUST
ICLUST(x = ,...)
#ICLUST.cluster
ICLUST.cluster(x = ,...)

#iclust.diagram
iclust.diagram(x = ,...)
#ICLUST.graph
ICLUST.graph(x = ,...)
#ICLUST.rgraph
ICLUST.rgraph(x = ,...)
#iclust.sort
iclust.sort(x = ,...)
#ICLUST.sort
ICLUST.sort(x = ,...)
#interbattery
interbattery(x = ,...)
#interp.boxplot
interp.boxplot(x = ,...)
#interp.median
interp.median(x = ,...)
#interp.q
interp.q(x = ,...)
#interp.qplot.by
interp.qplot.by(x = ,...)
#interp.quantiles
interp.quantiles(x = ,...)
#interp.quart
interp.quart(x = ,...)
#interp.quartiles
interp.quartiles(x = ,...)
#interp.values
interp.values(x = ,...)
#irt.0p
  irt.0p
#irt.1p
  irt.1p
#irt.2p
  irt.2p
#irt.discrim
  irt.discrim(x = ,...)
#irt.fa
  irt.fa(x = ,...)
#irt.item.diff.rasch  
  irt.item.diff.rasch(x = ,...)
#irt.person.rasch
  irt.person.rasch(x = ,...)
#irt.responses
  irt.responses(x = ,...)
#irt.se
  irt.se(x = ,...)
#irt.select
  irt.select(x = ,...)
#irt.stats.like
  irt.stats.like(x = ,...)
#irt.tau
  irt.tau(x = ,...)
#isCorrelation
  isCorrelation(x = ,...)
#isCovariance
  isCovariance(x = ,...)
#item.dichot
  item.dichot(x = ,...)
#item.lookup
  item.lookup(x = ,...)
#item.sim
  item.sim(x = ,...)
#item.validity
  item.validity(x = ,...)
#kaiser
  kaiser(x = ,...)
#keys.lookup
  keys.lookup(x = ,...)
#keys2list
  keys2list(x = ,...)
#keysort
  keysort(x = ,...)
#KMO
  KMO(x = ,...)
#kurtosi
  kurtosi(x = ,...)
#lavaan.diagram
  lavaan.diagram(x = ,...)
#levels2numeric
  levels2numeric(x = ,...)  
#logistic
  logistic(x = ,...)
#logistic.grm
  logistic.grm(x = ,...)
#logit
  logit(x = ,...)
#lookup
  lookup(x = ,...)
#lookupFromKeys
  lookupFromKeys(x = ,...)
#lookupItems
  lookupItems(x = ,...)
#lowerCor
  lowerCor(x = ,...)
#lowerMat
  lowerMat(x = ,...)
#lowerUpper
  lowerUpper(x = ,...)
#lsat6
  lsat6(x = ,...)
#lsat7
  lsat7(x = ,...)
#m2d
  m2d(x = ,...)
#m2t
  m2t(x = ,...) 

#make.congeneric
  make.congeneric(x = ,...)
#make.hierarchical
  make.hierarchical(x = ,...)
#make.irt.stats
  make.irt.stats(x = ,...)
#make.keys
  make.keys(x = ,...)
#makePositiveKeys
  makePositiveKeys(x = ,...)  
#manhattan
  manhattan(x = ,...)
#mardia
  mardia(x = ,...)
#mat.regress
  mat.regress(x = ,...)
#mat.sort
  mat.sort(x = ,...)
#matPlot
  matPlot(x = ,...)
#matReg
  matReg(x = ,...)
#matSort
  matSort(x = ,...)
#mediate
  mediate(x = ,...)
#mediate.diagram
  mediate.diagram(x = ,...)
#minkowski
  minkowski(x = ,...)
#mixed.cor
  mixed.cor(x = ,...)
#mixedCor
  mixedCor(x = ,...)
#mlArrange
  mlArrange(x = ,...)
#mlPlot
  mlPlot(x = ,...)
#mlr
  mlr(x = ,...)
#moderate.diagram
  moderate.diagram(x = ,...)
#mssd
  mssd(x = ,...)
#multi.arrow
  multi.arrow(x = ,...)
#multi.curved.arrow
  multi.curved.arrow(x = ,...)
#multi.hist
  multi.hist(x = ,...)
#multi.rect
  multi.rect(x = ,...)
#multi.self
  multi.self(x = ,...)
#multilevel.reliability
  multilevel.reliability(x = ,...)
#nchar2numeric
  nchar2numeric(x = ,...)
#nfactors
  nfactors(x = ,...)
#omega
  omega(x = ,...)
#omega.diagram
  omega.diagram(x = ,...)
#omega.graph
  omega.graph(x = ,...)
#omegaDirect
  omegaDirect(x = ,...)
#omegaFromSem
  omegaFromSem(x = ,...)
#omegah
  omegah(x = ,...)
#omegaSem
  omegaSem(x = ,...)
#outlier
  outlier(x = ,...) 
#p.rep
  p.rep(x = ,...)
#p.rep.f
  p.rep.f(x = ,...)
#p.rep.r
  p.rep.r(x = ,...) 
#p.rep.t
  p.rep.t(x = ,...)
#paired.r
  paired.r(x = ,...)
#pairs.panels
  pairs.panels(x = ,...)  
#pairwiseCount
  pairwiseCount(x = ,...)
#pairwiseCountBig
  pairwiseCountBig(x = ,...)
#pairwiseDescribe
  pairwiseDescribe(x = ,...)
#pairwiseImpute
  pairwiseImpute(x = ,...)
#pairwisePlot
  pairwisePlot(x = ,...)
#pairwiseReport
  pairwiseReport(x = ,...)
#pairwiseSample
  pairwiseSample(x = ,...)
#pairwiseZero
  pairwiseZero(x = ,...)
#parcels
  parcels(x = ,...)
#partial.r
  partial.r(x = ,...)
#pca
  pca(x = ,...)
#phi
  phi(x = ,...)
#phi.demo
  phi.demo(x = ,...)
#phi.list
  phi.list(x = ,...)
#phi2poly
  phi2poly(x = ,...)
#phi2poly.matrix
  phi2poly.matrix(x = ,...)
#phi2tetra
  phi2tetra(x = ,...)
#Pinv
  Pinv(x = ,...)
#plot.irt
  plot.irt(x = ,...)  
#plot.poly
  plot.poly(x = ,...)
#plot.poly.parallel
  plot.poly.parallel(x = ,...)
#plot.psych
  plot.psych(x = ,...)
#plot.reliability
  plot.reliability(x = ,...)
#plot.residuals
  plot.residuals(x = ,...)
#polar
  polar(x = ,...)
#poly.mat
  poly.mat(x = ,...)
#polychoric
  polychoric(x = ,...)
#polydi
  polydi(x = ,...)
#polyserial
  polyserial(x = ,...)
#predict.psych
  predict.psych(x = ,...) 
#predicted.validity
  predicted.validity(x = ,...)
#principal
  principal(x = ,...)
#print.psych
  print.psych(x = ,...)
#Procrustes
  Procrustes(x = ,...)
#progressBar
  progressBar(x = ,...)
#Promax
  Promax(x = ,...)
#psych
  psych(x = ,...)
#psych.misc
  psych.misc(x = ,...)
#quickView
  quickView(x = ,...)
#r.con
  r.con(x = ,...) 
#r.test
  r.test(x = ,...)
#r2c
  r2c(x = ,...)
#r2chi
  r2chi(x = ,...)

#r2d
  r2d(x = ,...)
#r2t
  r2t(x = ,...)
#radar
  radar(x = ,...)
#rangeCorrection
  rangeCorrection(x = ,...)
#reflect
  reflect(x = ,...)
#Reise
  Reise(x = ,...)
#reliability
  reliability(x = ,...)
#rescale
  rescale(x = ,...)
#resid.psych
  resid.psych(x = ,...)
#residuals.psych
  residuals.psych(x = ,...) 
#response.frequencies
  response.frequencies(x = ,...)

#responseFrequency
  responseFrequency(x = ,...)
#reverse.code
  reverse.code(x = ,...)  
#rmssd
  rmssd(x = ,...)
#sat.act
  sat.act(x = ,...)
#scaling.fits
  scaling.fits(x = ,...)
#scatter.hist
  scatter.hist(x = ,...)
#scatterHist
  scatterHist(x = ,...)
#schmid
  schmid(x = ,...)
#Schmid
  Schmid(x = ,...)
#schmid.leiman
  schmid.leiman(x = ,...)
#score.alpha
  score.alpha(x = ,...) 
#score.irt
  score.irt(x = ,...)
#score.irt.2
  score.irt.2(x = ,...)
#score.irt.poly
  score.irt.poly(x = ,...)
#score.items
  score.items(x = ,...)
#score.multiple.choice
  score.multiple.choice(x = ,...)
#scoreBy
  scoreBy(x = ,...)
#scoreFast
  scoreFast(x = ,...)
#scoreIrt
  scoreIrt(x = ,...)
#scoreIrt.1pl
  scoreIrt.1pl(x = ,...)
#scoreIrt.2pl
  scoreIrt.2pl(x = ,...)
#scoreItems
  scoreItems(x = ,...)
#scoreOverlap
  scoreOverlap(x = ,...)
#scoreVeryFast
  scoreVeryFast(x = ,...) 
#scoreWtd
  scoreWtd(x = ,...)
#scree
  scree(x = ,...)
#scrub
  scrub(x = ,...)
#SD
  SD(x = ,...)
#selectFromKeys
  selectFromKeys(x = ,...)  
#sem.diagram
  sem.diagram(x = ,...)
#sem.graph
  sem.graph(x = ,...)
#set.cor
  set.cor(x = ,...)
#setCor
  setCor(x = ,...)
#setCor.diagram
  setCor.diagram(x = ,...)
#setCorLookup
  setCorLookup(x = ,...)
#shannon
  shannon(x = ,...)
#sim
  sim(x = ,...)
#sim.anova
  sim.anova(x = ,...)
#sim.bonds
  sim.bonds(x = ,...)
#sim.circ
  sim.circ(x = ,...)
#sim.congeneric
  sim.congeneric(x = ,...)
#sim.correlation
  sim.correlation(x = ,...) 
#sim.dichot
  sim.dichot(x = ,...)
#sim.general
  sim.general(x = ,...)
#sim.hierarchical
  sim.hierarchical(x = ,...)

#sim.irt
  sim.irt(x = ,...)
#sim.item
  sim.item(x = ,...)
#sim.minor
  sim.minor(x = ,...)
#sim.multi
  sim.multi(x = ,...)
#sim.multilevel
  sim.multilevel(x = ,...)
#sim.npl
  sim.npl(x = ,...)
#sim.npn
  sim.npn(x = ,...)
#sim.omega
  sim.omega(x = ,...)
#sim.parallel
  sim.parallel(x = ,...)  
#sim.poly
  sim.poly(x = ,...)  
#sim.poly.ideal
  sim.poly.ideal(x = ,...)
#sim.poly.ideal.npl
  sim.poly.ideal.npl(x = ,...)
#sim.poly.ideal.npn
  sim.poly.ideal.npn(x = ,...)
#sim.poly.mat
  sim.poly.mat(x = ,...)
#sim.poly.npl
  sim.poly.npl(x = ,...)
#sim.poly.npn
  sim.poly.npn(x = ,...)
#sim.rasch
  sim.rasch(x = ,...)
#sim.simplex
  sim.simplex(x = ,...)
#sim.spherical
  sim.spherical(x = ,...)
#sim.structural
  sim.structural(x = ,...)
#sim.structure
  sim.structure(x = ,...) 
#sim.VSS
  sim.VSS(x = ,...)
#simCor
  simCor(x = ,...)
#simulation.circ
  simulation.circ(x = ,...)
#skew
  skew(x = ,...)
#smc
  smc(x = ,...) 
#spider
  spider(x = ,...)
#splitHalf
  splitHalf(x = ,...)
#statsBy
  statsBy(x = ,...)
#statsBy.boot
  statsBy.boot(x = ,...)
#statsBy.boot.summary
  statsBy.boot.summary(x = ,...)
#structure.diagram
  structure.diagram(x = ,...)
#structure.graph
  structure.graph(x = ,...)
#structure.list
  structure.list(x = ,...)
#structure.sem
  structure.sem(x = ,...)
#summary.psych
  summary.psych(x = ,...)
#super.matrix
  super.matrix(x = ,...)
  
#superCor
  superCor(x = ,...)
  
#superMatrix
  superMatrix(x = ,...)
#t2d
  t2d(x = ,...)
  
#t2r
  t2r(x = ,...)
#table2df
  table2df(x = ,...)
#table2matrix
  table2matrix(x = ,...)
  
#tableF
  tableF(x = ,...)
  
#Tal_Or
  Tal_Or(x = ,...)  
#Tal.Or
  Tal.Or(x = ,...)
#target.rot
  target.rot(x = ,...)
#TargetQ
  TargetQ(x = ,...)
#TargetT
  TargetT(x = ,...) 
  
#tenberge
  tenberge(x = ,...)
  
#test.all
  test.all(x = ,...)
  
#test.irt
  test.irt(x = ,...)

#test.psych
  test.psych(x = ,...)
  
#testRetest
  testRetest(x = ,...)
  
#tetrachoric
  tetrachoric(x = ,...)
#thurstone
  thurstone(x = ,...)
#Thurstone
  Thurstone(x = ,...)
#Thurstone.33
  Thurstone.33(x = ,...)
  
#Thurstone.9
  Thurstone.9(x = ,...)
  
#topBottom
  topBottom(x = ,...)
  
#tr
  tr(x = ,...)
#Tucker
  Tucker(x = ,...)

#unidim
  unidim(x = ,...)
  
#varimin
  varimin(x = ,...)
  
#vgQ.bimin
  vgQ.bimin(x = ,...)
#vgQ.targetQ
  vgQ.targetQ(x = ,...)
#vgQ.varimin
  vgQ.varimin(x = ,...)
  
#violin
  violin(x = ,...)
  
#violinBy
  violinBy(x = ,...)
#vss
  vss(x = ,...)
  
#VSS
  VSS(x = ,...)
  
#VSS.parallel
  VSS.parallel(x = ,...)
#VSS.plot
  VSS.plot(x = ,...)  
#VSS.scree
  VSS.scree(x = ,...)
#VSS.sim
  VSS.sim(x = ,...)
#VSS.simulate
  VSS.simulate(x = ,...)
  
#West
  West(x = ,...)
  
#winsor
  winsor(x = ,...)
#winsor.mean
  winsor.mean(x = ,...)
#winsor.means
  winsor.means(x = ,...)
  
#winsor.sd
  winsor.sd(x = ,...)
  
#winsor.var
  winsor.var(x = ,...)
#withinBetween
  withinBetween(x = ,...)

#wkappa
  wkappa(x = ,...)
  
#Yule
  Yule(x = ,...)
#Yule.inv
  Yule.inv(x = ,...)
#Yule2phi
  Yule2phi(x = ,...)

#Yule2phi.matrix
  Yule2phi.matrix(x = ,...)
#Yule2poly
  Yule2poly(x = ,...)
  
#Yule2poly.matrix
  Yule2poly.matrix(x = ,...)
#Yule2tetra
  Yule2tetra(x = ,...)
#YuleBonett
  YuleBonett(x = ,...)
  
#YuleCor
  YuleCor(x = ,...)
　
#acf
acf(x = ,lag.max = ,type = ,plot = ,na.action = ,demean = )
#acf2AR
acf2AR(x = ,lag.max = ,type = ,plot = ,na.action = ,demean = )
#add.scope
add.scope(x = ,...)
add.scope(terms1 = ,terms2 = )
#add1
add1(x = ,...)
add1(object = ,scope = )
#addmargins
addmargins(x = ,...)
addmargins(A = ,margin = ,FUN = ,quiet = )
#aggregate
aggregate(x = ,...)
aggregate(x = )
#aggregate.data.frame
aggregate.data.frame(x = ,...)
aggregate.data.frame(x = ,by = ,FUN = ,simplify = ,drop = )

#aggregate.ts
aggregate.ts(x = ,...)
aggregate.ts(x = ,nfrequency = ,FUN = ,ndeltat = ,ts.eps = )
#AIC
AIC(x = ,...)
AIC(object = ,k = )
#alias
alias(x = ,...)
alias(object = )
#anova
anova(x = ,...)
anova(object = )
#ansari.test
ansari.test(x = ,...)
ansari.test(x = )
#aov
aov(x = ,...)
aov(formula = ,data = ,projections = ,qr = ,contrasts = )
#approx
approx(x = ,...)
approx(x = ,y = ,xout = ,method = ,n = ,yleft = ,yright = ,rule = ,f = ,ties = ,na.rm = )
#approxfun
approxfun(x = ,...)
approxfun(x = ,y = ,method = ,yleft = ,yright = ,rule = ,f = ,ties = ,na.rm = )
#ar
ar(x = ,...)
ar(x = ,aic = ,order.max = ,method = ,na.action = ,series = )
#ar.burg
ar.burg(x = ,...)
ar.burg(x = )
#ar.mle
ar.mle(x = ,...)
ar.mle(x = ,aic = ,order.max = ,na.action = ,demean = ,series = )
#ar.ols
ar.ols(x = ,...)
ar.ols(x = ,aic = ,order.max = ,na.action = ,demean = ,intercept = ,series = )
#ar.yw
ar.yw(x = ,...)
ar.yw(x = )
#arima
arima(x = ,...)
arima(x = ,order = ,seasonal = ,xreg = ,include.mean = ,transform.pars = ,fixed = ,init = ,method = ,n.cond = ,SSinit = ,optim.method = ,optim.control = ,kappa = )
#arima.sim
arima.sim(x = ,...)
arima.sim(model = ,n = ,rand.gen = ,innov = ,n.start = ,start.innov = )
#arima0
arima0(x = ,...)
arima0(x = ,order = ,seasonal = ,xreg = ,include.mean = ,delta = ,transform.pars = ,fixed = ,init = ,method = ,n.cond = ,optim.control = )
#arima0.diag
arima0.diag(x = ,...)
arima0.diag()
#ARMAacf
ARMAacf(x = ,...)
ARMAacf(ar = ,ma = ,lag.max = ,pacf = )
#ARMAtoMA
ARMAtoMA(x = ,...)
ARMAtoMA(ar = ,ma = ,lag.max = )
#as.dendrogram
as.dendrogram(x = ,...)
as.dendrogram(object = )
#as.dist
as.dist(x = ,...)
as.dist(m = ,diag = ,upper = )
#as.formula
as.formula(x = ,...)
as.formula(object = ,env = )
#as.hclust
as.hclust(x = ,...)
as.hclust(x = )
#as.stepfun
as.stepfun(x = ,...)
as.stepfun(x = )
#as.ts
as.ts(x = ,...)
as.ts(x = )
#asOneSidedFormula
asOneSidedFormula(x = ,...)
asOneSidedFormula(object = )
#ave
ave(x = ,...)
ave(x = ,FUN = )
#bandwidth.kernel
bandwidth.kernel(x = ,...)
bandwidth.kernel(k = )
#bartlett.test
bartlett.test(x = ,...)
bartlett.test(x = )
#BIC
BIC(x = ,...)
BIC(object = )
#binom.test
binom.test(x = ,...)
binom.test(x = ,n = ,p = ,alternative = ,conf.level = )
#binomial
binomial(x = ,...)
binomial(link = )
#biplot
biplot(x = ,...)
biplot(x = )
#Box.test
Box.test(x = ,...)
Box.test(x = ,lag = ,type = ,fitdf = )

#bw.bcv
bw.bcv(x = ,nb = ,lower = ,upper = ,tol = )
#bw.nrd
bw.nrd(x = ,...)
bw.nrd(x = )
#bw.nrd0
bw.nrd0(x = ,...)
bw.nrd0(x = )
#bw.SJ
bw.SJ(x = ,...)
bw.SJ(x = ,nb = ,lower = ,upper = ,method = ,tol = )
#bw.ucv
bw.ucv(x = ,...)
bw.ucv(x = ,nb = ,lower = ,upper = ,tol = )
#C
C(x = ,...)
C(object =,contr = ,how.many = )
#cancor
cancor(x = ,...)
concor
#case.names
case.names(x = ,...)
case.names(object = )
#ccf
ccf(x = ,...)
ccf(x = ,y = ,lag.max = ,type = ,plot = ,na.action = )
#chisq.test
chisq.test(x = ,...)
chisq.test(x = ,y = ,correct = ,p = ,rescale.p = ,simulate.p.value = ,B = )
#cmdscale
cmdscale(x = ,...)
cmdscale(d = ,k = ,eig = ,add = ,x.ret = ,list. = )
#coef
coef(x = ,...)
coef(object = )
#coefficients
coefficients(x = ,...)
coefficients(object = )
#complete.cases
complete.cases(x = ,...)
complete.cases()
#confint
confint(x = ,...)
confint(object = ,parm = ,level = )
#confint.default
confint.default(x = ,...)
confint.default(object = ,parm = ,level = )
#confint.lm
confint.lm(x = ,...)
confint.lm(object = ,parm = ,level = )
#constrOptim
constrOptim(x = ,...)
constrOptim(theta = ,f = ,grad = ,ui = ,ci = ,mu = ,control = ,method = ,outer.iterations = ,outer.eps = ,hessian = )
#contr.helmert
contr.helmert(x = ,...)
contr.helmert(n = ,contrasts = ,sparse = )
#contr.poly
contr.poly(x = ,...)
contr.poly(n = ,scores = ,contrasts = ,sparse = )
#contr.SAS
contr.SAS(x = ,...)
contr.SAS(n = ,contrasts = ,sparse = )
#contr.sum
contr.sum(x = ,...)
contr.sum(n = ,contrasts = ,sparse = )
#contr.treatment
contr.treatment(x = ,...)
contr.treatment(n = ,base = ,contrasts = ,sparse = )
#contrasts
contrasts(x = ,...)
contrasts(x = ,contrasts = ,sparse = )
#contrasts<-
`contrasts<-`(x = ,...)
`contrasts<-`()
#convolve
convolve(x = ,...)
convolve(x = ,y = ,conj = ,type = )
#cooks.distance
cooks.distanceks.distance(x = ,...)
cooks.distance(model = )
#cophenetic
cophenetic(x = ,...)
cophenetic(x = )
#cor
cor(x = ,...)
cor(x = ,y = ,use = ,method = )
#cor.test
cor.test(x = ,...)
cor.test(x = )
#cov
cov(x = ,...)
cov(x = ,y = ,use = ,method = )
#cov.wt
cov.wt(x = ,...)
cov.wt(x = ,wt = ,cor = ,center = ,method = )
#cov2cor
cov2cor(x = ,...)
cov2cor(V = )

#covratio
covratio(x = ,...)
covratio(model = ,infl = ,res = )
#cpgram
cpgram(x = ,...)
cpgram(ts = ,taper = ,main = ,ci.col = )

#cutree
cutree(x = ,...)
cutree(tree = ,k = ,h = )
#cycle
cycle(x = ,...)
cycle(x = )
#D
D(x = ,...)
D(expr = ,name = )
#dbeta
  dbeta(x = ,shape1 = ,shape2 = ,ncp = ,log = )
#dbinom
  dbinom(x = ,size = ,prob = ,log = )
#dcauchy
  dcauchy(x = ,location = ,scale = ,log = )
#dchisq
  dchisq(x = ,df = ,ncp = ,log = )
  dchisq(x = ,df = ,ncp = ,log = )
#decompose
  decompose(x = ,...)
  decompose(x = ,type = ,filter = )
#delete.response
  delete.response(x = ,...)
  delete.response(termobj = )
#deltat
  deltat(x = ,...)
  deltat(x = )
#dendrapply
  dendrapply(x = ,...)
  dendrapply(X = ,FUN = )
#density
  density(x = ,...)
  density(x = )
#density.default
  density.default(x = ,...)
  density.default(x = ,bw = ,adjust = ,kernel = ,weights = ,window = ,width = ,give.Rkern = ,subdensity = ,n = ,from = ,to = ,cut = ,na.rm = )
#deriv
  deriv(x = ,...)
  deriv(expr = )
#deriv3
  deriv3(x = ,...)
  deriv(expr = )
#deviance
  deviance(x = ,...)
  deviance(object = )
#dexp
  dexp(x = ,rate = ,log = )
  dexp(x = ,rate = ,log = )
#df
  df(x = ,...)
  
#df.kernel
  df.kernel(x = ,...)
  df.kernel(k = )
#df.residual
  df.residual(x = ,...)
  df.residual(object = )
#DF2formula
  DF2formula(x = ,...)
  DF2formula(x = ,env = )
#dfbeta
  dfbeta(x = ,...)
  dfbeta(model = )
#dfbetas
  dfbetas(x = ,...)
  dfbetas(model = )
#dffits
  dffits(x = ,...)
  dffits(model = ,infl = ,res =)
#dgamma
  dgamma(x = ,shape = ,rate = ,log = )
#dgeom
  dgeom(x = ,prob = ,log = )
#dhyper
  dhyper(x = ,m = ,n = ,k = ,log = )
#diffinv
  diffinv(x = ,...)
  diffinv(x = )
#dist
  dist(x = ,...)
  dist(x = ,method = ,diag = ,upper = ,p = )
#dlnorm
  dlnorm(x = ,meanlog = ,sdlog = ,log = )
  dlnorm(x = ,meanlog = ,sdlog = ,log = )
#dlogis
  dlogis(x = ,location = ,scale = ,log = )
  dlogis(x = ,location = ,scale = ,log = )
#dmultinom
  dmultinom(x = ,size = ,prob = ,log = )
  dmultinom(x = ,size = ,prob = ,log = )
#dnbinom
  dnbinom(x = ,size = ,prob = ,log = )
  dnbinom(x = ,size = ,prob = ,mu = ,log = )
#dnorm
  dnorm(x = ,mean = ,sd = ,log = )
  dnorm(x = ,mean = ,sd = ,log = )
#dpois
  dpois(x = ,lambda = ,log = )
  dpois(x = ,lambda = ,log = )
#drop.scope
  drop.scope(x = ,...)
  drop.scope(terms1 = ,terms2 = )
#drop.terms
  drop.terms(x = ,...)
  drop.terms(termobj = ,dropx = ,keep.response = )
#drop1
  drop1(x = ,...)
  drop1(object = ,scope = )
#dsignrank
  dsignrank(x = ,n = ,log = )
  dsignrank(x = ,n = ,log = )
#dt
  dt(x = ,df = ,ncp = ,log = )
  dt(x = ,df = ,ncp = ,log = )
#dummy.coef
  dummy.coef(x = ,...)
  dummy.coef(object = )
#dummy.coef.lm
  dummy.coef.lm(x = ,...)
  dummy.coef.lm(object = ,use.na = )
#dunif
  dunif(x = ,min = ,max = ,log = )  
  dunif(x = ,min = ,max = ,log = )
#dweibull
  dweibull(x = ,shape = ,scale = ,log = )
  dweibull(x = ,shape = ,scale = ,log = )
#dwilcox
  dwilcox(x = ,m = ,n = ,log = )
  dwilcox(x = ,m = ,n = ,log = )
#ecdf
  ecdf(x = ,...)
  ecdf(x = )
#eff.aovlist
  eff.aovlist(x = ,...)
  eff.aovlist(aovlist = )
#effects
  effects(x = ,...)
  effects(object = )
#embed
  embed(x = ,...)
  embed(x = ,dimension = )
#end
  end(x = ,...)
  end(x = )
#estVar
  estVar(x = ,...)
  estVar(object = )
#expand.model.frame
  expand.model.frame(x = ,...)
  expand.model.frame(model = ,extras = ,envir = ,na.expand = )
#extractAIC
  extractAIC(x = ,...)
  extractAIC(fit = ,scale = ,k = )
#factanal
  factanal(x = ,...)
  factanal(x = ,factors = ,data = ,covmat = ,n.obs = ,subset = ,na.action = ,start = ,scores = ,rotation = ,control = )
#factor.scope
  factor.scope(x = ,...)
  factor.scope(factor = ,scope = )
#family
  family(x = ,...)
  family(object = )
#fft
  fft(x = ,...)
  fft(z = ,inverse = )
#filter
  filter(x = ,...)
  filter(x = ,filter = ,method = ,sides = ,circular = ,init = )
#fisher.test
  fisher.test(x = ,...)
  fisher.test(x = ,y = ,workspace = ,hybrid = ,hybridPars = ,control = ,or = ,alternative = ,conf.int = ,conf.level = ,simulate.p.value = ,B = )
#fitted
  fitted(x = ,...)
  fitted(object = )
#fitted.values
  fitted.values(x = ,...)
  fitted.values(object = )
#fivenum
  fivenum(x = ,...)
  fivenum(x = ,na.rm = )
#fligner.test
  fligner.test(x = ,...)
  fligner.test(x = )
#formula
  formula(x = ,...)
  formula(x = )
#frequency
  frequency(x = ,...)
  frequency(x = )
#friedman.test
  friedman.test(x = ,...)
  friedman.test(y = )
#ftable
  ftable(x = ,...)
  ftable(x = )
#Gamma
  Gamma(x = ,...)
  Gamma(link = )
#gaussian
  gaussian(x = ,...)
  gaussian(link = )
#get_all_vars
  get_all_vars(x = ,...)
  get_all_vars(formula = ,data = )
#getCall
  getCall(x = ,...)
  getCall(x = )
#getInitial
  getInitial(x = ,...)
  getInitial(object = ,data = )
#glm
  glm(x = ,...)
  glm(formula = ,family = ,data = ,weights = ,subset = ,na.action = ,start = ,etastart = ,mustart = ,offset = ,control = ,model = ,method = ,x = ,y = ,singular.ok = ,contrasts = )
#glm.control
  glm.control(x = ,...)
  glm.control(epsilon = ,maxit = ,trace = )
#glm.fit
  glm.fit(x = ,...)
  glm.fit(x = ,y = ,weights = ,start = ,etastart = ,mustart = ,offset = ,family = ,control = ,intercept = ,singular.ok = )
#hasTsp
  hasTsp(x = ,...)
  hasTsp(x = )
#hat
  hat(x = ,...)
  hat(x = ,intercept = )
#hatvalues
  hatvalues(x = ,...)
  hatvalues(model = )
#hclust
  hclust(x = ,...)
  hclust(d = ,method = ,members = )
#heatmap
  heatmap(x = ,...)
  heatmap(x = ,Rowv = ,Colv = ,distfun = ,hclustfun = ,reorderfun = ,add.expr = ,symm = ,revC = ,scale = ,na.rm = ,margins = ,ColSideColors = ,RowSideColors = ,cexRow = ,cexCol = ,labRow = ,labCol = ,main = ,xlab = ,ylab = ,keep.dendro = ,verbose = )
#HoltWinters
  HoltWinters(x = ,...)
  HoltWinters(x = ,alpha = ,beta = ,gamma = ,seasonal = ,start.periods = ,l.start = ,b.start = ,s.start = ,optim.start = ,optim.control = )
#influence
  influence(x = ,...)
  influence(model = )
#influence.measures
  influence.measures(x = ,...)
  influence.measures(model = ,infl = )
  
#integrate
  integrate(x = ,...)
  integrate(f = ,lower = ,upper = ,subdivisions = ,rel.tol = ,abs.tol = ,stop.on.error = ,keep.xy = ,aux = )
#interaction.plot
  interaction.plot(x = ,...)
  interaction.plot(x.factor = ,trace.factor = ,response = ,fun = ,type = ,legend = ,trace.label = ,fixed = ,xlab = ,ylab = ,ylim = ,lty = ,col = ,pch = ,xpd = ,leg.bg = ,leg.bty = ,xtick = ,xaxt = ,axes = )
#inverse.gaussian
  inverse.gaussian(x = ,...)
  inverse.gaussian(link = )
#IQR
  IQR(x = ,...)
  IQR(x = ,na.rm = ,type = )
#is.empty.model
  is.empty.model(x = ,...)
  is.empty.model(x = )
#is.leaf
  is.leaf(x = ,...)
  is.leaf(object = )
#is.mts
  is.mts(x = ,...)
  is.mts(x =)
#is.stepfun
  is.stepfun(x = ,...)
  is.stepfun(x = )
#is.ts
  is.ts(x = ,...)
  is.ts(x = )
#is.tskernel
  is.tskernel(x = ,...)
  is.tskernel(k = )
#isoreg
  isoreg(x = ,...)
  isoreg(x = ,y = )
#KalmanForecast
  KalmanForecast(x = ,...)
  KalmanForecast(n.ahead = ,mod = ,update = )
#KalmanLike
  KalmanLike(x = ,...)
  KalmanLike(y = ,mod =,nit =)
#KalmanRun
  KalmanRun(x = ,...)
  KalmanRun(y = ,mod = ,nit = ,update = )
#KalmanSmooth
  KalmanSmooth(x = ,...)
  KalmanSmooth(y = ,mod = ,nit = )
#kernapply
  kernapply(x = ,...)
  kernapply(x = )
#kernel
  kernel(x = ,...)
  kernel(coef = ,m = ,r = ,name = )
#kmeans
  kmeans(x = ,...)
  kmeans(x = ,centers = ,iter.max = ,nstart = ,algorithm = ,trace = )
#knots
  knots(x = ,...)
  knots(Fn = )
#kruskal.test
  kruskal.test(x = ,...)
  kruskal.test(x = )
#ks.test
  ks.test(x = ,...)
  ks.test(x = )
#ksmooth
  ksmooth(x = ,...)
  ksmooth(x = ,y = ,kernel = ,bandwidth = ,range.x = ,n.points = ,x.points = )
#lag
  lag(x = ,...)
  log(x = ,base = )
#lag.plot
  lag.plot(x = ,...)
  lag.plot(x = ,lags = ,layout = ,set.lags = ,main = ,asp = ,diag = ,diag.col = ,type = ,oma = ,ask = ,do.lines = ,labels = )
#line
  line(x = ,...)
  line(x = ,y = ,iter = )
#lm
  lm(x = ,...)
  lm(formula = ,data = ,subset = ,weights = ,na.action = ,method = ,model = ,x = ,y = ,qr = ,singular.ok = ,contrasts = ,offset = )
#lm.fit
  lm.fit(x = ,...)
  lm.fit(x = ,y = ,offset = ,method = ,tol = ,singular.ok = )
#lm.influence
  lm.influence(x = ,...)
  lm.influence(model = ,do.coef = )

#lm.wfit
  lm.wfit(x = ,...)
  lm.wfit(x = ,y = ,w = ,offset = ,method = ,tol = ,singular.ok = )
#loadings
  loadings(x = ,...)
  loadings(x = )

#loess
  loess(x = ,...)
  loess(formula = ,data = ,weights = ,subset = ,na.action = ,model = ,span = ,enp.target = ,degree = ,parametric = ,drop.square = ,normalize = ,family = ,method = ,control = )
  
#loess.control
  loess.control(x = ,...)
  loess.control(surface = ,statistics = ,trace.hat = ,cell = ,iterations = ,iterTrace = )

#loess.smooth
  loess.smooth(x = ,...)
  loess.smooth(x = ,y = ,span = ,degree = ,family = ,evaluation = )
#logLik
  logLik(x = ,...)
  logLik(object = )
#loglin
  loglin(x = ,...)
  loglin(table = ,margin = ,start = ,fit = ,eps = ,iter = ,param = ,print = )
#lowess
  lowess(x = ,...)
  lowess(x = ,y = ,f = ,iter = ,delta = )
#ls.diag
  ls.diag(x = ,...)
  ls.diag(ls.out = )
#ls.print
  ls.print(x = ,...)
  ls.print(ls.out = ,digits = ,print.it = )
#lsfit
  lsfit(x = ,...)
  lsfit(x = ,y = ,wt = ,intercept = ,tolerance = ,yname = )
#mad
  mad(x = ,...)
  mad(x = ,center = ,constant = ,na.rm = ,low = ,high = )
#mahalanobis
  mahalanobis(x = ,...)
  mahalanobis(x = ,center = ,cov = ,inverted = )
#make.link
  make.link(x = ,...)
  make.link(link = )
#makeARIMA
  makeARIMA(x = ,...)
  makeARIMA(phi = ,theta = ,Delta = ,kappa = ,SSinit = ,tol = )
#makepredictcall
  makepredictcall(x = ,...)
  makepredictcall(var = ,call = )
#manova
  manova(x = ,...)
  manova()
#mantelhaen.test
  mantelhaen.test(x = ,...)
  mantelhaen.test(x = ,y = ,z = ,alternative = ,correct = ,exact = ,conf.level = )
#mauchly.test
  mauchly.test(x = ,...)
  mauchly.test(object = )
#mcnemar.test
  mcnemar.test(x = ,...)
  mcnemar.test(x = ,y = ,correct = )
#median
  median(x = ,...)
  median(x = ,na.rm = )
#median.default
  median.default(x = ,...)
  median.default(x = ,na.rm = )
#medpolish
  medpolish(x = ,...)
  medpolish(x = ,eps = ,maxiter = ,trace.iter = ,na.rm = )
#model.extract
  model.extract(x = ,...)
  model.extract(frame = ,component = )
#model.frame
  model.frame(x = ,...) 
  model.frame(formula = )
#model.frame.default
  model.frame.default(x = ,...)
  model.frame.default(formula = ,data = ,subset = ,na.action = ,drop.unused.levels = ,xlev = )
#model.matrix
  model.matrix(x = ,...)
  model.matrix(object = )
#model.matrix.default
  model.matrix.default(x = ,...)
  model.matrix.default(object = ,data = ,contrasts.arg = ,xlev = )
#model.matrix.lm
  model.matrix.lm(x = ,...)
  model.matrix(object = )
#model.offset
  model.offset(x = ,...)
  model.offset(x = )
#model.response
  model.response(x = ,...)  
  model.response(data = ,type = )
#model.tables
  model.tables(x = ,...)
  model.tables(x = )
#model.weights
  model.weights(x = ,...) 
  model.weights(x = )
#monthplot
  monthplot(x = ,...)
  monthplot(x = )
#mood.test
  mood.test(x = ,...)
  mood.test(x = )
#mvfft
  mvfft(x = ,...)
  mvfft(z = ,inverse = )
#na.action
  na.action(x = ,...)
  na.action(object = )
#na.contiguous
  na.contiguous(x = ,...)
  na.contiguous(object = )
#na.exclude
  na.exclude(x = ,...)
  na.exclude(object = )
#na.fail
  na.fail(x = ,...)
  na.fail(object = )
#na.omit
  na.omit(x = ,...)
  na.omit(object = )
#na.pass
  na.pass(x = ,...)
  na.pass(object = )
#napredict
  napredict(x = ,...)
  napredict(omit = ,x = )
#naprint
  naprint(x = ,...)
  naprint(x = )
#naresid
  naresid(x = ,...)
  naresid(omit = ,x = )
#nextn
  nextn(x = ,...)
  nextn(n = ,factors = )
#nlm
  nlm(x = ,...)
  nlm(f = ,p = ,hessian = ,typsize = ,fscale = ,print.level = ,ndigit = ,gradtol = ,stepmax = ,steptol = ,iterlim = ,check.analyticals = )
#nlminb
  nlminb(x = ,...)
  nlminb(start = ,objective = ,gradient = ,hessian = ,scale = ,control = ,lower = ,upper = )
#nls
  nls(x = ,...)
  nls(formula = ,data = ,start = ,control = ,algorithm = ,trace = ,subset = ,weights = ,na.action = ,model = ,lower = ,upper = )
#nls.control
  nls.control(x = ,...)
  nls.control(maxiter = ,tol = ,minFactor = ,printEval = ,warnOnly = ,scaleOffset = ,nDcentral = )
#NLSstAsymptotic
  NLSstAsymptotic(x = ,...)
  NLSstAsymptotic(xy = )
#NLSstClosestX
  NLSstClosestX(x = ,...ar)
  NLSstClosestX(xy = ,yval = )
#NLSstLfAsymptote
  NLSstLfAsymptote(x = ,...)
  NLSstLfAsymptote(xy = )
#NLSstRtAsymptote
  NLSstRtAsymptote(x = ,...)
  NLSstRtAsymptote(xy = )
#nobs
  nobs(x = ,...)
  nobs(object = )
#numericDeriv
  numericDeriv(x = ,...)
  numericDeriv(expr = ,theta = ,rho = ,dir = ,eps = ,central = )
#offset
  offset(x = ,...)
  offset(object = )
  optimHess(par = ,fn = ,gr = ,control = )
#oneway.test
  oneway.test(x = ,...)
  oneway.test(formula = ,data = ,subset = ,na.action = ,var.equal = )
#optim
  optim(x = ,...)
  optim(par = ,fn = ,gr = ,method = ,lower = ,upper = ,control = ,hessian = )
#optimHess
  optimHess(x = ,...)
#optimise
  optimise(x = ,...)
  optimise(f = ,interval = ,lower = ,upper = ,maximum = ,tol = )
#optimize
  optimize(x = ,...)
  optimize(f = ,interval = ,lower = ,upper = ,maximum = ,tol = )
#order.dendrogram
  order.dendrogram(x = ,...)
  order.dendrogram(x = )
#p.adjust
  p.adjust(x = ,...)
  p.adjust(p = ,method = ,n = )
#p.adjust.methods
  p.adjust.methods(x = ,...)
  p.adjust.methods
#pacf
#pairwise.t.test
  pacf(x = ,...)
  pacf(x = ,lag.max = ,plot = ,na.action = )
#Pair
  Pair(x = ,...)    
  Pair(x = ,y = )
#pairwise.prop.test
pairwise.prop.test(x = ,n = ,p.adjust.method = )
pairwise.prop.test(x = ,n = ,p.adjust.method = )
#pairwise.t.test
pairwise.t.test(x = ,g = ,p.adjust.method = )
pairwise.t.test(x = ,g = ,p.adjust.method = ,pool.sd = ,paired = ,alternative = )
#pairwise.table
pairwise.table(x = ,g = ,p.adjust.method = )
pairwise.table(compare.levels = ,level.names = ,p.adjust.method = )
#pairwise.wilcox.test
pairwise.wilcox.test(x = ,g = ,p.adjust.method = )
pairwise.wilcox.test(x = ,g = ,p.adjust.method = ,paired = )
#pbeta
  pbeta(x = ,shape1 = ,shape2 = ,ncp = ,log.p = ) 
  pbeta(q = ,shape1 = ,shape2 = ,ncp = ,lower.tail = ,log.p = )
#pbinom
  pbinom(x = ,size = ,prob = ,lower.tail = ,log.p = )
  pbinom(q = ,size = ,prob = ,lower.tail = ,log.p = )
#pbirthday
  pbirthday(x = ,n = ,classes = ,lower.tail = ,log.p = )
  pbirthday(n = ,classes = ,coincident = )
#pcauchy
  pcauchy(x = ,location = ,scale = ,lower.tail = ,log.p = )
  pcauchy(q = ,location = ,scale = ,lower.tail = ,log.p = )
#pchisq
  pchisq(x = ,df = ,ncp = ,lower.tail = ,log.p = )
  pchisq(q = ,df = ,ncp = ,lower.tail = ,log.p = )
#pexp
  pexp(x = ,rate = ,lower.tail = ,log.p = )
  pexp(q = ,rate = ,lower.tail = ,log.p = )
#pf
  pf(q = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )
#pgamma
  pgamma(x = ,shape = ,rate = ,lower.tail = ,log.p = )
  pgamma(q = ,shape = ,rate = ,scale = ,lower.tail = ,log.p = )
#pgeom
  pgeom(x = ,prob = ,lower.tail = ,log.p = )
  pgeom(q = ,prob = ,lower.tail = ,log.p = )
#phyper
  phyper(x = ,m = ,n = ,k = ,lower.tail = ,log.p = )
  pf(x = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )
  phyper(q = ,m = ,n = ,k = ,lower.tail = ,log.p =)
#plclust
  plclust(x = ,...)
  plclust(tree = ,hang = ,unit = ,level = ,hmin = ,square = ,labels = ,plot. = ,axes = ,frame.plot = ,ann = ,main = ,sub = ,xlab = ,ylab = )
#plnorm
  plnorm(x = ,meanlog = ,sdlog = ,lower.tail = ,log.p = )
  plnorm(q = ,meanlog = ,sdlog = ,lower.tail = ,log.p = )
#plogis
  plogis(x = ,location = ,scale = ,lower.tail = ,log.p = )
  plogis(q = ,location = ,scale = ,lower.tail = ,log.p = )
#plot.ecdf
  plot.ecdf(x = ,...)
  plot.ecdf(x = ,ylab = ,verticals = ,col.01line = ,pch = )
#plot.spec.coherency
  plot.spec.coherency(x = ,...)
  plot.spec.coherency(x = ,ci = ,xlab = ,ylab = ,ylim = ,type = ,main = ,ci.col = ,ci.lty = )
#plot.spec.phase
  plot.spec.phase(x = ,...)
  plot.spec.phase(x = ,ci = ,xlab = ,ylab = ,ylim = ,type = ,main = ,ci.col = ,ci.lty = )
#plot.stepfun
  plot.stepfun(x = ,...)
  plot.stepfun(x = ,xval = ,xlim = ,ylim = ,xlab = ,ylab = ,main = ,add = ,verticals = ,do.points = ,pch = ,col = ,col.points = ,cex.points = ,col.hor = ,col.vert = ,lty = ,lwd = )
#plot.ts
  plot.ts(x = ,...)
  plot.ts(x = ,y = ,plot.type = ,xy.labels = ,xy.lines = ,panel = ,nc = ,yax.flip = ,mar.multi = ,oma.multi = ,axes = )
#pnbinom
  pnbinom(x = ,size = ,prob = ,lower.tail = ,log.p = )  
  pnbinom(q = ,size = ,prob = ,mu = ,lower.tail = ,log.p = )
#pnorm
  pnorm(x = ,mean = ,sd = ,lower.tail = ,log.p = )
  pnorm(q = ,mean = ,sd = ,lower.tail = ,log.p = )
#poisson
  poisson(x = ,...)
  poisson(link = )
#poisson.test
  poisson.test(x = ,...)
  poisson.test(x = ,T = ,r = ,alternative = ,conf.level = )
#poly
  poly(x = ,...)
  poly(x = ,degree = ,coefs = ,raw = ,simple = )
#polym
  polym(x = ,...)
  polym(degree = ,coefs = ,raw = )
#power
  power(x = ,...)
  power(lambda = )
#power.anova.test
  power.anova.test(x = ,...)
  power.anova.test(groups = ,n = ,between.var = ,within.var = ,sig.level = ,power = )
#power.prop.test
  power.prop.test(x = ,...)
  power.prop.test(n = ,p1 = ,p2 = ,sig.level = ,power = ,alternative = ,strict = ,tol = )
#power.t.test
  power.t.test(x = ,...)
  power.t.test(n = ,delta = ,sd = ,sig.level = ,power = ,type = ,alternative = ,strict = ,tol = )
#PP.test
  PP.test(x = ,...)
  PP.test(x = ,lshort = )
#ppoints
  ppoints(x = ,...)
  ppoints(n = ,a = )
#ppois
  ppois(x = ,lambda = ,lower.tail = ,log.p = )
  ppois(q = ,lambda = ,lower.tail = ,log.p = )
#ppr
  ppr(x = ,...)
  ppr(x = )
#prcomp
  prcomp(x = ,...)
  prcomp(x = )
#predict
  predict(x = ,...)
  predict(object = )
#predict.glm
  predict.glm(x = ,...)
  predict.lm(object = ,newdata = ,se.fit = ,scale = ,df = ,interval = ,level = ,type = ,terms = ,na.action = ,pred.var = ,weights = )
#predict.lm
  predict.lm(x = ,...)  
  predict.lm(object = ,newdata = ,se.fit = ,scale = ,df = ,interval = ,level = ,type = ,terms = ,na.action = ,pred.var = ,weights = )
#preplot
  preplot(x = ,...)
  preplot(object = )
#princomp
  princomp(x = ,...)
  princomp(x = )
#printCoefmat
  printCoefmat(x = ,...)
  printCoefmat(x = ,digits = ,signif.stars = ,signif.legend = ,dig.tst = ,cs.ind = ,tst.ind = ,zap.ind = ,P.values = ,has.Pvalue = ,eps.Pvalue = ,na.print = ,quote = ,right = )
#profile
  profile(x = ,...)
  profile(fitted = )
#proj
  proj(x = ,...)
  proj(object = )
#promax
  promax(x = ,...)
  promax(x = ,m = )
#prop.test
  prop.test(x = ,...)
  prop.test(x = ,n = ,p = ,alternative = ,conf.level = ,correct = )
#prop.trend.test
  prop.trend.test(x = ,...)
  prop.trend.test(x = ,n = ,score = )
#psignrank
  psignrank(x = ,n = ,lower.tail = ,log.p = ) 
  psignrank(q = ,n = ,lower.tail = ,log.p = )
#pt
  pt(x = ,df = ,ncp = ,lower.tail = ,log.p = )
  pt(q = ,df = ,ncp = ,lower.tail = ,log.p = )
#ptukey
  ptukey(x = ,q = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )
  ptukey(q = ,nmeans = ,df = ,nranges = ,lower.tail = ,log.p = )
#punif
  punif(x = ,min = ,max = ,lower.tail = ,log.p = )
  punif(q = ,min = ,max = ,lower.tail = ,log.p = )
#pweibull
  pweibull(x = ,shape = ,scale = ,lower.tail = ,log.p = )
  pweibull(q = ,shape = ,scale = ,lower.tail = ,log.p = )
#pwilcox
  pwilcox(x = ,m = ,n = ,lower.tail = ,log.p = )
  pwilcox(q = ,m = ,n = ,lower.tail = ,log.p = )
#qbeta
  qbeta(x = ,shape1 = ,shape2 = ,ncp = ,lower.tail = ,log.p = )
  qbeta(p = ,shape1 = ,shape2 = ,ncp = ,lower.tail = ,log.p = )
#qbinom
  qbinom(x = ,size = ,prob = ,lower.tail = ,log.p = )
  qbinom(p = ,size = ,prob = ,lower.tail = ,log.p = )
#qbirthday
  qbirthday(x = ,n = ,classes = ,lower.tail = ,log.p = )
  qbirthday(prob = ,classes = ,coincident = )
#qcauchy
  qcauchy(x = ,location = ,scale = ,lower.tail = ,log.p = )
  qcauchy(p = ,location = ,scale = ,lower.tail = ,log.p = )
#qchisq
  qchisq(x = ,df = ,ncp = ,lower.tail = ,log.p = )
  qchisq(p = ,df = ,ncp = ,lower.tail = ,log.p = )
#qexp
  qexp(x = ,rate = ,lower.tail = ,log.p = )
  qexp(p = ,rate = ,lower.tail = ,log.p = )
#qf
  qf(x = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )  
  qf(p = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )
#qgamma
  qgamma(x = ,shape = ,rate = ,lower.tail = ,log.p = )
  qgamma(p = ,shape = ,rate = ,scale = ,lower.tail = ,log.p = )
#qgeom
  qgeom(x = ,prob = ,lower.tail = ,log.p = )
  qgeom(p = ,prob = ,lower.tail = ,log.p = )
#qhyper
  qhyper(x = ,m = ,n = ,k = ,lower.tail = ,log.p = )
  qhyper(p = ,m = ,n = ,k = ,lower.tail = ,log.p = )
#qlnorm
  qlnorm(x = ,meanlog = ,sdlog = ,lower.tail = ,log.p = )
  qlnorm(p = ,meanlog = ,sdlog = ,lower.tail = ,log.p = )
#qlogis
  qlogis(x = ,location = ,scale = ,lower.tail = ,log.p = )  
  qlogis(p = ,location = ,scale = ,lower.tail = ,log.p = )
#qnbinom
  qnbinom(x = ,size = ,prob = ,lower.tail = ,log.p = )
  qnbinom(p = ,size = ,prob = ,mu = ,lower.tail = ,log.p = )
#qnorm
  qnorm(x = ,mean = ,sd = ,lower.tail = ,log.p = )
  qnorm(p = ,mean = ,sd = ,lower.tail = ,log.p = )
#qpois
  qpois(x = ,lambda = ,lower.tail = ,log.p = )
  qpois(p = ,lambda = ,lower.tail = ,log.p = )
  qpois(p = x,lambda = ,lower.tail = ,log.p = )
  qpois(p = x,lambda = x,lower.tail = ,log.p = )
  qpois(p = x,lambda = x,lower.tail = x,log.p = )
  qpois(p = x,lambda = x,lower.tail = x,log.p = x)
  qpois(p = "x",lambda = x,lower.tail = x,log.p = x)
  qpois(p = df,lambda = x,lower.tail = x,log.p = x)
  
#qqline
  qqline(x = ,...)
  qqline(y = ,datax = ,distribution = ,probs = ,qtype = )
  qqline(y = x,datax = ,distribution = ,probs = ,qtype = )
  qqline(y = x,datax = x,distribution = ,probs = ,qtype = )
  qqline(y = x,datax = x,distribution = x,probs = ,qtype = )
  qqline(y = x,datax = x,distribution = x,probs = x,qtype = )
  qqline(y = x,datax = x,distribution = x,probs = x,qtype = x)
  qqline(y = "x",datax = x,distribution = x,probs = x,qtype = x)
  qqline(y = df,datax = x,distribution = x,probs = x,qtype = x)
  
#qqnorm
  qqnorm(x = ,...)
  qqnorm(y = )
  qqnorm(y = x)
  qqnorm(y = "x")
  qqnorm(y = df)

#qqplot
  qqplot(x = ,...)
  qqplot(x = ,y = ,plot.it = ,xlab = ,ylab = )
  qqplot(x = x,y = ,plot.it = ,xlab = ,ylab = )
    qqplot(x = x,y = x,plot.it = ,xlab = ,ylab = )
  qqplot(x = x,y = x,plot.it = x,xlab = ,ylab = )
    qqplot(x = x,y = x,plot.it = x,xlab = x,ylab = )
      qqplot(x = x,y = x,plot.it = x,xlab = x,ylab = x)


#qsignrank
  qsignrank(x = ,n = ,lower.tail = ,log.p = )
  qsignrank(p = ,n = ,lower.tail = ,log.p = )
  qsignrank(p = x,n = ,lower.tail = ,log.p = )
  qsignrank(p = x,n = x,lower.tail = ,log.p = )
  qsignrank(p = x,n = x,lower.tail = x,log.p = )
  qsignrank(p = x,n = x,lower.tail = x,log.p = x)
  qsignrank(p = "x",n = x,lower.tail = x,log.p = x)
  qsignrank(p = df,n = x,lower.tail = x,log.p = x)
#qt
  qt(x = ,df = ,ncp = ,lower.tail = ,log.p = )  
  qt(p = ,df = ,ncp = ,lower.tail = ,log.p = )
  qt(p = x,df = ,ncp = ,lower.tail = ,log.p = )
  qt(p = x,df = x,ncp = ,lower.tail = ,log.p = )
  qt(p = x,df = x,ncp = x,lower.tail = ,log.p = )
  qt(p = x,df = x,ncp = x,lower.tail = x,log.p = )
  qt(p = x,df = x,ncp = x,lower.tail = x,log.p = x)
  qt(p = "x",df = x,ncp = x,lower.tail = x,log.p = x)
  qt(p = df,df = x,ncp = x,lower.tail = x,log.p = x)
  
#qtukey
  qtukey(x = ,q = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )
  qtukey(p = ,nmeans = ,df = ,nranges = ,lower.tail = ,log.p = )
  qtukey(p = x,nmeans = ,df = ,nranges = ,lower.tail = ,log.p = )
    qtukey(p = x,nmeans = x,df = ,nranges = ,lower.tail = ,log.p = )
    qtukey(p = x,nmeans = x,df = x,nranges = ,lower.tail = ,log.p = )
    qtukey(p = x,nmeans = x,df = x,nranges = x,lower.tail = ,log.p = )
    qtukey(p = x,nmeans = x,df = x,nranges = x,lower.tail = x,log.p = )
    qtukey(p = x,nmeans = x,df = x,nranges = x,lower.tail = x,log.p = x)

#quade.test
  quade.test(x = ,...)
  quade.test(y = )
  quade.test(y = x)
  quade.test(y = "x")
  quade.test(y = df)
#quantile
  quantile(x = ,...)
  quantile(x = )
  quantile(x = x)
  quantile(x = "x")
  quantile(x = df)
#quasi
  quasi(x = ,...)
  quasi(link = ,variance = )
  quasi(link = x,variance = )
  quasi(link = x,variance = x)

  quasi(link = "x",variance = )
  quasi(link = df,variance = )

#quasibinomial
  quasibinomial(x = ,...) 
  quasibinomial(link = )
  quasibinomial(link = x)
  quasibinomial(link = "x")
  quasibinomial(link = df)
#quasipoisson
  quasipoisson(x = ,...)
  quasipoisson(link = )
  quasipoisson(link = x)
  quasipoisson(link = "x")
    quasipoisson(link = df)


  
#qunif
  qunif(x = ,min = ,max = ,lower.tail = ,log.p = )
  qunif(p = ,min = ,max = ,lower.tail = ,log.p = )
  qunif(p = x,min = ,max = ,lower.tail = ,log.p = )
  qunif(p = x,min = x,max = ,lower.tail = ,log.p = )
  qunif(p = x,min = x,max = x,lower.tail = ,log.p = )
  qunif(p = x,min = x,max = x,lower.tail = x,log.p = )
  qunif(p = x,min = x,max = x,lower.tail = x,log.p = x)
  qunif(p = "x",min = ,max = ,lower.tail = ,log.p = )
  qunif(p = df,min = ,max = ,lower.tail = ,log.p = )

#qweibull
  qweibull(x = ,shape = ,scale = ,lower.tail = ,log.p = )
  qweibull(p = ,shape = ,scale = ,lower.tail = ,log.p = )
  qweibull(p = x,shape = ,scale = ,lower.tail = ,log.p = )
  qweibull(p = x,shape = x,scale = ,lower.tail = ,log.p = )
  qweibull(p = x,shape = x,scale = x,lower.tail = ,log.p = )
  qweibull(p = x,shape = x,scale = x,lower.tail = x,log.p = )
  qweibull(p = x,shape = x,scale = x,lower.tail = x,log.p = x)
  qweibull(p = "x",shape = x,scale = x,lower.tail = x,log.p = x)
  qweibull(p = df,shape = x,scale = x,lower.tail = x,log.p = x)
  
#qwilcox
  qwilcox(x = ,m = ,n = ,lower.tail = ,log.p = )
  qwilcox(p = ,m = ,n = ,lower.tail = ,log.p = )
  qwilcox(p = x,m = ,n = ,lower.tail = ,log.p = )
  qwilcox(p = x,m = x,n = ,lower.tail = ,log.p = )
  qwilcox(p = x,m = x,n = x,lower.tail = ,log.p = )
  qwilcox(p = x,m = x,n = x,lower.tail = x,log.p = )
  qwilcox(p = x,m = x,n = x,lower.tail = x,log.p = x)
  qwilcox(p = "x",m = x,n = x,lower.tail = x,log.p =  x)
  qwilcox(p = df,m = x,n = x,lower.tail = x,log.p =  x)
#r2dtable
  r2dtable(x = ,...)
  r2dtable(n = ,r = ,c = )
  r2dtable(n = x,r = ,c = )
  r2dtable(n = x,r = x,c = )
  r2dtable(n = x,r = x,c = x)
  r2dtable(n = "x",r = x,c = x)
  r2dtable(n = df,r = x,c = x)
  
#rbeta
  rbeta(x = ,shape1 = ,shape2 = ,ncp = )
  rbeta(n = ,shape1 = ,shape2 = ,ncp = )
  rbeta(n = x,shape1 = ,shape2 = ,ncp = )
  rbeta(n = x,shape1 =x ,shape2 = ,ncp = )
  rbeta(n = x,shape1 =x ,shape2 =x ,ncp = )
  rbeta(n = x,shape1 =x ,shape2 =x ,ncp = x)
  rbeta(n = "x",shape1 =x ,shape2 =x ,ncp = x)
  rbeta(n = df,shape1 =x ,shape2 =x ,ncp = x)
  
#rbinom
  rbinom(x = ,size = ,prob = )
  rbinom(n = ,size = ,prob = )
  rbinom(n = x,size = ,prob = )
  rbinom(n = x,size = x,prob = )
  rbinom(n = x,size = x,prob = x)
  rbinom(n = "x",size = x,prob = x)
  rbinom(n = df,size = x,prob = x)
#rcauchy
  rcauchy(x = ,location = ,scale = )
  rcauchy(n = ,location = ,scale = )
  rcauchy(n = x,location = ,scale = )
  rcauchy(n = x,location = x,scale = )
  rcauchy(n = x,location = x,scale = x)
  rcauchy(n = "x",location = x,scale = x)
  rcauchy(n = df,location = x,scale = x)
#rchisq
  rchisq(x = ,df = ,ncp = )
  rchisq(n = ,df = ,ncp = x)
  rchisq(n = x,df = ,ncp = x)
  rchisq(n = x,df = x,ncp = x)
  rchisq(n = x,df = df,ncp = x)
  rchisq(n = df,df = df,ncp = x)
  rchisq(n = "df",df = df,ncp = x)
#read.ftable
  read.ftable(x = ,...)
  read.ftable(file = ,sep = ,quote = ,row.var.names = ,col.vars = ,skip = )
  read.ftable(file = x,sep = ,quote = ,row.var.names = ,col.vars = ,skip = )
  read.ftable(file = "/Users/aizawaharuka/Documents/GitHub",sep = x,quote = ,row.var.names = ,col.vars = ,skip = )
  read.ftable(file = "/Users/aizawaharuka/Documents/GitHub",sep = x,quote = x,row.var.names = ,col.vars = ,skip = )
  read.ftable(file = "/Users/aizawaharuka/Documents/GitHub",sep = x,quote = x,row.var.names = x,col.vars = ,skip = )
  read.ftable(file = "/Users/aizawaharuka/Documents/GitHub",sep = x,quote = x,row.var.names = x,col.vars = x,skip = )
  read.ftable(file = "/Users/aizawaharuka/Documents/GitHub",sep = x,quote = x,row.var.names = x,col.vars = x,skip = x)
  read.ftable(file = "/Users/aizawaharuka/Documents/GitHub",sep = x,quote = x,row.var.names = true,col.vars = x,skip = x)

  #rect.hclust
  rect.hclust(x = ,...)
  rect.hclust(tree = ,k = ,which = ,x = ,h = ,border = ,cluster = )
  rect.hclust(tree =x ,k = ,which = ,x = ,h = ,border = ,cluster = )
  rect.hclust(tree =x ,k = ,which = ,x = ,h = ,border = ,cluster = )
  rect.hclust(tree =x ,k = x,which = ,x = ,h = ,border = ,cluster = )
  rect.hclust(tree =x ,k = ,which = x,x = ,h = ,border = ,cluster = )
  rect.hclust(tree =x ,k = ,which = ,x = x,h = ,border = ,cluster = )
  rect.hclust(tree =x ,k = ,which = ,x = ,h = x,border = ,cluster = )
  rect.hclust(tree =x ,k = ,which = ,x = ,h = ,border = x,cluster = )
  rect.hclust(tree =x ,k = ,which = ,x = ,h = ,border = ,cluster = x)
  rect.hclust(tree ="x" ,k = ,which = ,x = ,h = ,border = ,cluster = )
  rect.hclust(tree =df ,k = ,which = ,x = ,h = ,border = ,cluster = )
  
#reformulate
  reformulate(x = ,...)
  reformulate(termlabels = ,response = ,intercept = ,env = )
  reformulate(termlabels =x ,response = ,intercept = ,env = )
  reformulate(termlabels =x ,response = x,intercept = ,env = )
  reformulate(termlabels =x ,response = x,intercept = x,env = )
  reformulate(termlabels =x ,response = x,intercept = x,env = x)
  reformulate(termlabels ="x" ,response = x,intercept = x,env = x)
  reformulate(termlabels =df ,response = x,intercept = x,env = x)
#relevel
  relevel(x = ,...)
  relevel(x = ,ref = )
  relevel(x = x,ref = )
  relevel(x = x,ref = x)
  relevel(x = df,ref = x)
  relevel(x = "df",ref = x)
#reorder
  reorder(x = ,...)
  reorder(x = x)
  reorder(x = df)
  reorder(x = "df")
#replications
  replications(x = ,...)
  replications(formula = ,data = ,na.action = )
  replications(formula = x,data = ,na.action = )
  replications(formula = x,data = x,na.action = )
  replications(formula = x,data = x,na.action = x)
  replications(formula = "x",data = x,na.action = "na.fail")
  replications(formula = df,data = x,na.action = "na.fail")
#reshape
  reshape(x = ,...)
  reshape(data = ,varying = ,v.names = ,timevar = ,idvar = ,ids = ,times = ,drop = ,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = ,v.names = ,timevar = ,idvar = ,ids = ,times = ,drop = ,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = ,timevar = ,idvar = ,ids = ,times = ,drop = ,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = x,timevar = ,idvar = ,ids = ,times = ,drop = ,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = ,ids = ,times = ,drop = ,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = x,ids = ,times = ,drop = ,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = ,drop = ,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = x,drop = ,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = x,drop = x,direction = ,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = x,drop = x,direction = x,new.row.names = ,sep = ,split = )
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = x,drop = x,direction = x,new.row.names = x,sep = ,split = )  
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = x,drop = x,direction = x,new.row.names = x,sep = x,split = )
  reshape(data = x,varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = x,drop = x,direction = x,new.row.names = x,sep = x,split = x)
  reshape(data = "x",varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = x,drop = x,direction = x,new.row.names = x,sep = x,split = x)
  reshape(data = df,varying = x,v.names = x,timevar = x,idvar = x,ids = x,times = x,drop = x,direction = x,new.row.names = x,sep = x,split = x)
  
#resid
  resid(x = ,...)
  resid(object = x)
  resid(object = df)
  resid(object = "x")
  
#residuals
  residuals(x = ,...)
  residuals(object = x)
  residuals(object = df)
  residuals(object = "df")
#residuals.glm
  residuals.glm(x = ,...)
  residuals.glm(object = ,type = )
  residuals.glm(object = x,type = )
  residuals.glm(object = x,type = "df")
  residuals.glm(object = x,type = x)
  residuals.glm(object = df,type = x)
  residuals.glm(object = "df",type = x)

#residuals.lm
  residuals.lm(x = ,...)
  residuals.lm(object = ,type = )
  residuals.lm(object = x,type = )
  residuals.lm(object = x,type = "df")
  residuals.lm(object = x,type = x)
  residuals.lm(object = df,type = x)
#rexp
  rexp(x = ,rate = )
  rexp(n = ,rate = )
  rexp(n = x,rate = )
  rexp(n = x,rate = x)
  rexp(n = "x",rate = x)
  rexp(n = df,rate = x)

#rf
  rf(x = ,df1 = ,df2 = ,ncp = ) 
  rf(n = ,df1 = ,df2 = ,ncp = )
  rf(n=x,df1 = ,df2 = ,ncp = )
  rf(n=x,df1 = x,df2 = ,ncp = )
  rf(n=x,df1 = x,df2 = x,ncp = )
  rf(n=x,df1 = x,df2 = x,ncp = x)
  rf(n="x",df1 = x,df2 = x,ncp = x)
  rf(n=df,df1 = x,df2 = x,ncp = x)
  
#rgamma
  rgamma(x = ,shape = ,rate = )
  rgamma(n = ,shape = ,rate = ,scale = )
  rgamma(n = x,shape = ,rate = ,scale = )
  rgamma(n = x,shape = x,rate = ,scale = )
  rgamma(n = x,shape = x,rate = x,scale = )
  rgamma(n = x,shape = x,rate = ,scale = x)
  rgamma(n = "x",shape = x,rate = ,scale = x)
  rgamma(n = df,shape = x,rate = ,scale = x)

#rgeom
  rgeom(x = ,prob = )
  rgeom(n = ,prob = )
#rhyper
  rhyper(x = ,m = ,n = ,k = )
  rhyper(nn = ,m = ,n = ,k =)
#rlnorm
  rlnorm(x = ,meanlog = ,sdlog = )
  rlnorm(n = ,meanlog = ,sdlog = )
  rlnorm(n = x,meanlog = ,sdlog = )
  rlnorm(n = x,meanlog = x,sdlog = )
  rlnorm(n = x,meanlog = x,sdlog = x)
  rlnorm(n = "x",meanlog = x,sdlog = x)
  rlnorm(n = df,meanlog = x,sdlog = x)
#rlogis
  rlogis(n = ,location = ,scale = )
  rlogis(n = ,location = ,scale = )
  rlogis(n = x,location = ,scale = )
  rlogis(n = x,location = x,scale = )
  rlogis(n = df,location = x,scale = )
  rlogis(n = "df",location = x,scale = )
#rmultinom
  rmultinom(x = ,size = ,prob = )
  rmultinom(n = ,size = ,prob = )
  rmultinom(n =x ,size = ,prob = )
  rmultinom(n =x ,size = x,prob = )
  rmultinom(n =x ,size = x,prob = x)
  rmultinom(n =x ,size = x,prob = )
  rmultinom(n =x ,size = x,prob = x)
  rmultinom(n ="x" ,size = x,prob = x)
  rmultinom(n =df ,size = x,prob = x)
#rnbinom
  rnbinom(x = ,size = ,prob = ) 
  rnbinom(n = ,size = ,prob = ,mu = )
  rnbinom(n = x,size = ,prob = ,mu = )
  rnbinom(n = x,size = x,prob = ,mu = )
  rnbinom(n = x,size = x,prob = x,mu = )
  rnbinom(n = x,size = x,prob = ,mu = x)
  rnbinom(n = "x",size = x,prob = ,mu = x)
  rnbinom(n = df,size = x,prob = ,mu = x)

  #rnorm
  rnorm(x = ,mean = ,sd = )
  rnorm(n = ,mean = ,sd = )
  rnorm(n = "x",mean = ,sd = )
  rnorm(n = x,mean = x,sd = )
  rnorm(n = x,mean = ,sd = x)
  rnorm(n = df,mean = ,sd = x)
  rnorm(n = df,mean = df,sd = x)
  rnorm(n = x,mean = df,sd = df)
  
#rpois
  rpois(x = ,lambda = )
  rpois(n = ,lambda = x)
  rpois(n = x,lambda = )
  rpois(n = x,lambda = x)
  rpois(n = "x",lambda = x)
  rpois(n = "x",lambda = "x")
  rpois(n = df,lambda = "x")

  
#rsignrank
  rsignrank(x = ,n = )
  rsignrank(nn = ,n = x)
  rsignrank(nn = x,n = x)
  rsignrank(nn = "x",n = x)
  rsignrank(nn = x,n = "x")
  rsignrank(nn = x,n = df)
  rsignrank(nn = df,n = df)

  #rstandard
  rstandard(x = ,...) 
  rstandard(model = x)
rstandard(model = df)
rstandard("x")  
#rstudent
  rstudent(x = ,...)
  rstudent(model = x)
  rstudent(model = df)
  rstudent(model = "x")
#rt
  rt(x = ,df = ,ncp = ) 
  rt(n = ,df = ,ncp = )
  rt(n = x,df = ,ncp = )
  rt(n = ,df = x,ncp = )
  rt(n = ,df = ,ncp = x)
#runif
  runif(x = ,min = ,max = )
  runif(n = x,min = ,max = )
  runif(n = x,min = x,max = )
  runif(n = x,min = ,max = x)
#runmed
  runmed(x = ,...)
  runmed(x = ,k = ,endrule = ,algorithm = ,na.action = ,print.level = )  
  runmed(x = x ,k = ,endrule = ,algorithm = ,na.action = ,print.level = )
  runmed(x = x ,k = 3,endrule = ,algorithm = ,na.action = ,print.level = )
  runmed(x = x ,k = ,endrule = "test",algorithm = ,na.action = ,print.level = )
  runmed(x = x ,k = ,endrule = ,algorithm = "test",na.action = ,print.level = )
  runmed(x = x ,k = ,endrule = ,algorithm = ,na.action = "test",print.level = )
  runmed(x = x ,k = ,endrule = ,algorithm = ,na.action = ,print.level = "test")

#rweibull
  rweibull(x = x,shape = ,scale = )
  rweibull(x = ,shape = x,scale = )
  rweibull(x = ,shape = ,scale = x)
  
#rwilcox
  rwilcox(x = ,m = ,n = )
  rwilcox(nn = ,m = ,n = x)
  rwilcox(nn = x,m = ,n = x)
  rwilcox(nn = "x",m = x,n = x)
  rwilcox(nn = x,m = "x",n = x)
  
  #rWishart
  rWishart(x = ,df = ,Sigma = x)
  rWishart(x = x,df = ,Sigma = )
  rWishart(x = ,df = x,Sigma = )

  #scatter.smooth
  scatter.smooth(x = ,...)
  scatter.smooth(x = ,y = ,span = ,degree = ,family = ,xlab = ,ylab = ,ylim = ,evaluation = ,lpars = x)
  scatter.smooth(x = x,y = ,span = ,degree = ,family = ,xlab = ,ylab = ,ylim = ,evaluation = ,lpars = x)
  scatter.smooth(x = 1,y = ,span = ,degree = ,family = "test",xlab = ,ylab = ,ylim = ,evaluation = ,lpars = x)
  scatter.smooth(x = 1,y = ,span = ,degree = "test",family = ,xlab = ,ylab = ,ylim = ,evaluation = ,lpars = x)
  scatter.smooth(x = 1,y = ,span = ,degree = ,family = ,xlab = ,ylab = ,ylim = ,evaluation = "test",lpars = x)
  
  #screeplot
  screeplot(x = ,...)
  screeplot(x = x)
  #sd
  sd(x = ,...)
  sd(x = ,na.rm = x)
  sd(x = "x",na.rm = )
  #se.contrast
  se.contrast(x = ,...)
  se.contrast(object = x)
  se.contrast(object = 3)
  
  #selfStart
  selfStart(x = ,...)
  selfStart(model = ,initial = ,parameters = ,template = x)
  selfStart(model = x,initial = ,parameters = ,template = x)
  selfStart(model = ,initial = ,parameters = ,template = x)
  
  #setNames
  setNames(x = ,...)
  setNames(x = x)
  #shapiro.test
  shapiro.test(x = ,...)
  shapiro.test(x = )
  #sigma
  sigma(x = ,...)
  sigma(object = x)
  sigma(object = 4)
  simulate(x = ,...)
  simulate(object = ,nsim = ,seed = x)
  simulate(object = x,nsim = ,seed = x)
  simulate(object = 4,nsim = ,seed = x)
  #smooth
  smooth(x = ,...)
  smooth(x = ,kind = ,twiceit = ,endrule = ,do.ends = x)
  smooth(x = x,kind = ,twiceit = ,endrule = ,do.ends = x)
  smooth(x = 4,kind = ,twiceit = ,endrule = ,do.ends = x)
  #smooth.spline
  smooth.spline(x = ,...)
  smooth.spline(x = ,y = ,w = ,df = ,spar = ,lambda = ,cv = ,all.knots = ,nknots = ,keep.data = ,df.offset = ,penalty = ,control.spar = ,tol = ,keep.stuff =x)
  smooth.spline(x = x,y = ,w = ,df = ,spar = ,lambda = ,cv = ,all.knots = ,nknots = ,keep.data = ,df.offset = ,penalty = ,control.spar = ,tol = ,keep.stuff =x)
  
  #smoothEnds
  smoothEnds(x = ,...)
  smoothEnds(y = ,k = x)
  smoothEnds(y = 3,k = )
  #sortedXyData
  sortedXyData(x = ,...)
  sortedXyData(x = ,y = ,data = x)
  sortedXyData(x = x,y = ,data = x)
  ##sortedXyData(x = x,y = x,data = x)
  
  #spec.ar
  spec.ar(x = ,...)
  spec.ar(x = ,n.freq = ,order = ,plot = ,na.action = ,method = x)
  spec.ar(x = x,n.freq = ,order = ,plot = ,na.action = ,method = x)
  spec.ar(x = x,n.freq = ,order = ,plot = ,na.action = 4,method = )
  
#spec.pgram
  spec.pgram(x = ,...)
  spec.pgram(x = x,spans = ,kernel = ,taper = ,pad = ,fast = ,demean = ,detrend = ,plot = ,na.action = )
  spec.pgram(x = 4,spans = ,kernel = ,taper = ,pad = ,fast = ,demean = ,detrend = ,plot = ,na.action = )
  spec.pgram(x = x,spans = ,kernel = ,taper = ,pad = ,fast = ,demean = ,detrend = ,plot = ,na.action = 4)
  spec.pgram(x = x,spans = ,kernel = ,taper = ,pad = ,fast = ,demean = ,detrend = 4,plot = ,na.action = )
#spec.taper
  spec.taper(x = ,...)
  spec.taper(x = ,p = x)  
  spec.taper(x = ,p = 0)  
##spec.taper(x = x,p = 0)  

#spectrum
  spectrum(x = ,...)
  spectrum(x = x,method = )
  spectrum(x = "x",method = )
  spectrum(x = "x",method = x)
  
#spline
  spline(x = ,...)
  spline(x = ,y = ,n = ,method = ,xmin = ,xmax = ,xout = ,ties = x)
  spline(x = x,y = ,n = ,method = ,xmin = ,xmax = ,xout = ,ties = x)
  #spline(x = x,y = ,n = ,method = ,xmin = ,xmax = ,xout = ,ties = "test")
  spline(x = x,y = ,n = ,method = x,xmin = ,xmax = ,xout = ,ties = "test")
  #spline(x = x,y = ,n = ,method = ,xmin = x,xmax = ,xout = ,ties = "test")
  #spline(x = x,y = ,n = ,method = ,xmin = ,xmax = ,xout = x,ties = "test")
  
  #splinefun
  splinefun(x = ,...)
  splinefun(x = ,y = ,method = ,ties = x)
  splinefun(x = x,y = ,method = ,ties = x)
  splinefun(x = "x",y = ,method = ,ties = x)
  #splinefunH
  splinefunH(x = ,...)
  splinefunH(x = x,y = ,m = )  
  splinefunH(x = x,y = x,m = )  
  splinefunH(x = x,y = x,m = x)  
  splinefunH(x = x,y = x,m = true)  
  splinefunH(x = x,y = x,m = TRUE)  
  splinefunH(x = TRUE,y = x,m = x)  
  splinefunH(x = x,y = TRUE,m = )  

#SSasymp
  SSasymp(x = ,...)
  SSasymp(input = x,Asym = ,R0 = ,lrc = )
  SSasymp(input = x,Asym = ,R0 = x,lrc = )
  SSasymp(input = x,Asym = x,R0 = x,lrc = )
  SSasymp(input = x,Asym = "x",R0 = x,lrc = x)
  SSasymp(input = x,Asym = x,R0 = x,lrc = "x")
  SSasymp(input = x,Asym = x,R0 = "x",lrc = x)
  SSasymp(input = "x",Asym = x,R0 = x,lrc = x)
  SSasymp(input = x,Asym = x,R0 = i,lrc = x)
  
  #SSasympOff
  SSasympOff(x = ,...)
  SSasympOff(input = x,Asym = ,lrc = ,c0 = )
  SSasympOff(input = x,Asym = x,lrc = ,c0 = )
  SSasympOff(input = x,Asym = x,lrc = x,c0 = )
  SSasympOff(input = x,Asym = x,lrc = x,c0 = "x")
  SSasympOff(input = x,Asym = x,lrc = "x",c0 = x)
  SSasympOff(input = x,Asym = "x",lrc = x,c0 = x)
  SSasympOff(input = "x",Asym = x,lrc = x,c0 = x)
  
#SSasympOrig
  SSasympOrig(x = ,...)
  SSasymp(reacttime,R0 = x,Asym = x,lrc =x)
  SSasymp(input = ,Asym = x,R0 = ,lrc = )
  SSasymp(input = ,Asym = ,R0 = x,lrc = )
  SSasymp(input = ,Asym = x,R0 = x,lrc = )
  SSasymp(input = ,Asym = x,R0 = x,lrc = x)
  SSasymp(input = "x",Asym = x,R0 = x,lrc = x)
  SSasymp(input = x,Asym = "x",R0 = x,lrc = x)
  SSasymp(input = x,Asym = x,R0 = "x",lrc = x)
  SSasymp(input = x,Asym = x,R0 = x,lrc = "x")
  
  #SSbiexp

  SSbiexp(input = "test",A1 = x,lrc1 = x,A2 = x,lrc2 = x)
  SSbiexp(input = x,A1 = x,lrc1 = "test",A2 = x,lrc2 = x)
  SSbiexp(input = x,A1 = "test",lrc1 = x,A2 = x,lrc2 = x)
  SSbiexp(input = x,A1 = x,lrc1 = "test",A2 = x,lrc2 = x)
  SSbiexp(input = x,A1 = x,lrc1 = x,A2 = "test",lrc2 = x)
  SSbiexp(input = x,A1 = x,lrc1 = x,A2 = x,lrc2 = "test")

  SSbiexp(input = x,A1 = ,lrc1 = x,A2 = x,lrc2 = x)
  SSbiexp(input = x,A1 = ,lrc1 = x,A2 = ,lrc2 = x)
  SSbiexp(input = x,A1 = ,lrc1 = x,A2 =,lrc2 = )
  SSbiexp(input = ,A1 = ,lrc1 = x,A2 = ,lrc2 = )
  SSbiexp(input =x ,A1 = ,lrc1 = ,A2 = ,lrc2 = )
  SSbiexp(x = 3)
  SSbiexp(x = f,...)
#SSD
  SSD(x = 4)
  SSD(x =x)
  SSD(x = x,...)

#SSfol
  SSfol(x = ,...)
  SSfol(Dose = ,input = ,lKe = ,lKa = ,lCl = )
  SSfol(Dose = x,input = ,lKe = ,lKa = ,lCl = )
  SSfol(Dose = x,input = x,lKe = ,lKa = ,lCl = )
  SSfol(Dose = x,input = x,lKe = x,lKa = ,lCl = )
  SSfol(Dose = x,input = x,lKe = x,lKa = x,lCl = )
  SSfol(Dose = x,input = x,lKe = x,lKa = x,lCl = x)

#SSfpl
  SSfpl(x = ,...)
SSfpl(input = ,A = ,B = ,xmid = ,scal = x)
SSfpl(input = ,A = ,B = x,xmid = ,scal = x)
SSfpl(input = ,A =x,B = x,xmid = ,scal = x)
SSfpl(input = ,A = ,B = ,xmid = x,scal = x)
SSfpl(input = ,A = ,B = ,xmid = ,scal = x)
SSfpl(input = ,A = ,B = ,xmid = ,scal = "x")
SSfpl(input = ,A = ,B = ,xmid = "x",scal = x)

  #SSgompertz
  SSgompertz(x = ,...)
  SSgompertz(x = ,Asym = ,b2 = ,b3 = )
  SSgompertz(x = ,Asym = ,b2 = ,b3 = x)
  SSgompertz(x = x,Asym = ,b2 = ,b3 = x)
  SSgompertz(x = x,Asym = ,b2 = x,b3 = x)
  SSgompertz(x = x,Asym = "x",b2 = x,b3 = x)
#SSlogis
  SSlogis(x = ,...)
  SSlogis(input = ,Asym = ,xmid = ,scal = x)
  SSlogis(input = ,Asym = ,xmid = x,scal = x)
  SSlogis(input = x,Asym = ,xmid = x,scal = x)
  SSlogis(input = x,Asym = "x",xmid = x,scal = x)
  
  #SSmicmen
  SSmicmen(x = ,...)
  SSmicmen(input = ,Vm = ,K = x)
  SSmicmen(input = ,Vm =x,K = x)
  SSmicmen(input ="x" ,Vm =x ,K = x)
  SSmicmen(input = df,Vm = x,K = x)
  SSmicmen(input = x,Vm = df,K = x)
  
  #SSweibull
  SSweibull(x = ,...)
SSweibull(x = ,Asym = ,Drop = ,lrc = ,pwr = x)
SSweibull(x = x,Asym = ,Drop = ,lrc = x,pwr = )
SSweibull(x = ,Asym = ,Drop = x,lrc = x,pwr = x)
SSweibull(x = x,Asym = x,Drop = x,lrc = x,pwr = x)
SSweibull(x = x,Asym = x,Drop = x,lrc = x,pwr = "x")
SSweibull(x = x,Asym = x,Drop = x,lrc = "x",pwr = x)
SSweibull(x = x,Asym = x,Drop = "x",lrc = x,pwr = x)
SSweibull(x = x,Asym = "x",Drop = x,lrc = x,pwr = x)
SSweibull(x = "x",Asym = x,Drop = x,lrc = x,pwr = x)

  
#start
  start(x = ,...) 
start(df)
  #stat.anova
  stat.anova(x = ,...)
  stat.anova(table = ,test = ,scale = ,df.scale = ,n = )
  stat.anova(table = x,test = ,scale = ,df.scale = ,n = )
  stat.anova(table = df,test = x,scale = ,df.scale = ,n = )
  

  #step
  step(x = ,...)
  step(object = ,scope = ,scale = ,direction = ,trace = ,keep = ,steps = ,k = )
  step(object = x,scope = ,scale = ,direction = ,trace = ,keep = ,steps = ,k = )
  step(object = x,scope = x,scale = ,direction = ,trace = ,keep = ,steps = ,k = )
  step(object = x,scope = ,scale = x,direction = ,trace = ,keep = ,steps = ,k = )
  step(object = x,scope = ,scale = ,direction = x,trace = ,keep = ,steps = ,k = )
  step(object = x,scope = ,scale = ,direction = ,trace = x,keep = ,steps = ,k = )
  step(object = x,scope = ,scale = ,direction = ,trace = ,keep = x,steps = ,k = )
  step(object = x,scope = ,scale = ,direction = ,trace = ,keep = ,steps = x,k = )
  step(object = x,scope = ,scale = ,direction = ,trace = ,keep = ,steps = ,k = x)
  
#stepfun
  stepfun(x = ,...)
  stepfun(x = ,y = ,f = ,ties = ,right = )
  stepfun(x = x,y = ,f = ,ties = ,right = )
  stepfun(x = x,y = x,f = ,ties = ,right = )
  stepfun(x = x,y = x,f = x,ties = ,right = )
  stepfun(x = x,y = x,f = ,ties = "test",right = )
  stepfun(x = x,y = x,f = ,ties = ,right = "test")
  stepfun(x = x,y = x,f = ,ties = ,right = x)
  stepfun(x = x,y = x,f = ,ties = x,right = )
  stepfun(x = x,y = x,f = ,ties = ,right = x)

#stl
  stl(x = ,...)
  stl(x = ,s.window = ,s.degree = ,t.window = ,t.degree = ,l.window = ,l.degree = ,s.jump = ,t.jump = ,l.jump = ,robust = ,inner = ,outer = )
  stl(x = x,s.window = ,s.degree = ,t.window = ,t.degree = ,l.window = ,l.degree = ,s.jump = ,t.jump = ,l.jump = ,robust = ,inner = ,outer = )
  stl(x = x,s.window = x,s.degree = ,t.window = ,t.degree = ,l.window = ,l.degree = ,s.jump = ,t.jump = ,l.jump = ,robust = ,inner = ,outer = )
  stl(x = 1,s.window = ,s.degree = ,t.window = ,t.degree = ,l.window = ,l.degree = ,s.jump = ,t.jump = ,l.jump = ,robust = ,inner = ,outer = x)
#StructTS
  StructTS(x = ,...)
  StructTS(x = ,type = ,init = ,fixed = ,optim.control = x)
  StructTS(x =x ,type = ,init = ,fixed = ,optim.control = x)
  StructTS(x =x ,type = x,init = ,fixed = ,optim.control = x)
  StructTS(x =x ,type = ,init = x,fixed = ,optim.control = x)
  StructTS(x =x ,type = ,init = ,fixed = x,optim.control = x)
  
  #summary.aov
  summary.aov(x = ,...)
  summary.aov(object = ,intercept = ,split = ,expand.split = ,keep.zero.df = x)
  summary.aov(object = x,intercept = ,split = ,expand.split = ,keep.zero.df = x)
  summary.aov(object = x,intercept = x,split = ,expand.split = ,keep.zero.df = x)
  summary.aov(object = x,intercept = x,split = x,expand.split = ,keep.zero.df = x)
  summary.aov(object = x,intercept = x,split = x,expand.split = x,keep.zero.df = x)

  
  #summary.glm
  summary.glm(x = ,...)
  summary.glm(object = ,dispersion = ,correlation = ,symbolic.cor = x)
  summary.glm(object = x,dispersion = ,correlation = ,symbolic.cor = "x")
  summary.glm(object = x,dispersion = x,correlation = ,symbolic.cor = x)
  summary.glm(object = x,dispersion = x,correlation = x,symbolic.cor = x)


  #summary.lm
summary.lm(object = ,correlation = ,symbolic.cor = )
summary.lm(object = ,correlation = ,symbolic.cor = x)
summary.lm(object = x,correlation = ,symbolic.cor = "x")
summary.lm(object = x,correlation = x,symbolic.cor = )
summary.lm(object = x,correlation = x,symbolic.cor = x)
summary.lm(object = x,correlation = x,symbolic.cor = "x")
summary.lm(object = x,correlation = "x",symbolic.cor = )
summary.lm(object = x,correlation = "x",symbolic.cor = x)
summary.lm(object = x,correlation = "x",symbolic.cor = "x")
summary.lm(object = "x",correlation = x,symbolic.cor = )

#summary.manova
summary.manova(object = ,test = ,intercept = ,tol = )
summary.manova(object = x,test = ,intercept = ,tol = )
summary.manova(object = x,test = x,intercept = ,tol = )
summary.manova(object = x,test = ,intercept = x,tol = )
summary.manova( object = x,test = ,intercept = ,tol = x)

#summary.stepfun
summary.stepfun(x = ,...)
summary.stepfun(object = x)
summary.stepfun(df)
summary.stepfun("x")
#supsmu
supsmu(x = ,...)
supsmu(x = ,y = ,wt = ,span = ,periodic = ,bass = ,trace = x)
supsmu(x = x,y = ,wt = ,span = ,periodic = ,bass = ,trace = x)
supsmu(x = x,y = x,wt = ,span = ,periodic = ,bass = ,trace = x)
supsmu(x = x,y = x,wt = x,span = ,periodic = ,bass = ,trace = x)
supsmu(x = x,y = x,wt = x,span = x,periodic = ,bass = ,trace = x)
supsmu(x = x,y = x,wt = x,span = x,periodic = x,bass = ,trace = x)
supsmu(x = x,y = x,wt = x,span = x,periodic = x,bass = x,trace = x)
supsmu(x = x,y = x,wt = x,span = x,periodic = x,bass = x,trace = )
supsmu(x = x,y = x,wt = x,span = x,periodic = x,bass = x,trace = "x")
supsmu(x = df,y = x,wt = x,span = x,periodic = x,bass = x,trace = x)
supsmu(x = x,y = df,wt = x,span = x,periodic = x,bass = x,trace = x)
supsmu(x = x,y = x,wt = df,span = x,periodic = x,bass = x,trace = x)
supsmu(x = x,y = x,wt = x,span = df,periodic = x,bass = x,trace = x)

  #symnum
symnum(x = ,...)
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = ,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = x,symbols = ,legend = ,na = ,eps = ,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = x,legend = ,na = ,eps = ,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = x,na = ,eps = ,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = x,eps = ,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = x,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = ,numeric.x = x,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = ,numeric.x = ,corr = x,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = ,numeric.x = ,corr = ,show.max = x,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = ,numeric.x = ,corr = ,show.max = ,show.min = x,abbr.colnames = ,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = ,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = x,lower.triangular = ,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = ,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = x,diag.lower.tri = )
symnum(x = x,cutpoints = ,symbols = ,legend = ,na = ,eps = ,numeric.x = ,corr = ,show.max = ,show.min = ,abbr.colnames = ,lower.triangular = ,diag.lower.tri = x)
#t.test
t.test(x = ,...)
t.test(x = x)
t.test("x")
#termplot
termplot(x = ,...)
termplot(model = ,data = ,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = ,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = ,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = )
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = "x")
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = x,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = x,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = x,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = x,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = x,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = x,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = x,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = x,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = x,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = x,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = x,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = x,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = x,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = x,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = x,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = x,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = x,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = x,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = x,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = x,use.factor.levels = ,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = x,smooth = ,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = x,ylim = ,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = x,plot = ,transform.x = x)
termplot(model = x,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = x,transform.x = x)
termplot(model = "x",data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = x,transform.x = x)
termplot(model = df,data = x,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = x,transform.x = x)
termplot(model = x,data = df,envir = ,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = x,transform.x = x)
termplot(model = x,data = x,envir = df,partial.resid = ,rug = ,terms = ,se = ,xlabs = ,ylabs = ,main = ,col.term = ,lwd.term = ,col.se = ,lty.se = ,lwd.se = ,col.res = ,cex = ,pch = ,col.smth = ,lty.smth = ,span.smth = ,ask = ,use.factor.levels = ,smooth = ,ylim = ,plot = x,transform.x = x)

#terms
terms(x = ,...)
terms(x = x)
#terms.formula
terms.formula(x = ,...)
terms.formula(x = ,specials = ,abb = ,data = ,neg.out = ,keep.order = ,simplify = x)
terms.formula(x = ,specials = ,abb = ,data = ,neg.out = ,keep.order = x,simplify = )
terms.formula(x = x,specials = ,abb = ,data = ,neg.out = ,keep.order = ,simplify = x)
terms.formula(x = x,specials = x,abb = ,data = ,neg.out = ,keep.order = ,simplify = x)
terms.formula(x = x,specials = ,abb = 3,data = ,neg.out = ,keep.order = ,simplify = x)
terms.formula(x = x,specials = ,abb = ,data = 3,neg.out = ,keep.order = ,simplify = x)

#time
time(x = ,...)
time(x = df)

#toeplitz
toeplitz(x = ,...)
toeplitz(x = df)
#ts
ts(x = ,...)
ts(data = ,start = ,end = ,frequency = ,deltat = ,ts.eps = ,class =x ,names = )
ts(data = ,start = ,end = ,frequency = ,deltat = ,ts.eps = x,class = ,names = )
ts(data = ,start = ,end = ,frequency = ,deltat = x,ts.eps = ,class = ,names = )
ts(data = ,start = ,end = ,frequency = x,deltat = ,ts.eps = ,class = ,names = )
ts(data = ,start = ,end = df,frequency = ,deltat = ,ts.eps = ,class = ,names = )
ts(data = ,start = df,end = ,frequency = ,deltat = ,ts.eps = ,class = ,names = )

#ts.intersect
ts.intersect(x = ,...)

#ts.plot
ts.plot(x = ,...)
#ts.union
ts.union(x = ,...)
#tsdiag
tsdiag(x = ,...)
tsdiag(object = ,gof.lag = x)
tsdiag(x)
tsdiag(df)
tsdiag("x")
#tsp
tsp(x = ,...)
#tsp<-
`tsp<-`(x = ,...)
`tsp<-`(x)
`tsp<-`(3)
`tsp<-`(df)
`tsp<-`("x")
#tsSmooth
tsSmooth(x = ,...)
tsSmooth(object = x)
tsSmooth(object = "x")
tsSmooth(df)
#TukeyHSD
TukeyHSD(x = ,...)
TukeyHSD(x = ,which = ,ordered = ,conf.level = x)
TukeyHSD(x = x,which = ,ordered = ,conf.level = x)
TukeyHSD(x = "x",which = x,ordered = ,conf.level = x)
TukeyHSD(x = x,which = x,ordered = ,conf.level = x)
TukeyHSD(x = df)
TukeyHSD(x = x,which = df,ordered = ,conf.level = x)
TukeyHSD(x = x,which = x,ordered = df,conf.level = x)

#uniroot
uniroot(x = ,...)
uniroot(f = ,interval = ,lower = ,upper = ,f.lower = ,f.upper = ,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = ,interval = x,lower = ,upper = ,f.lower = ,f.upper = ,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = ,interval = df,lower = ,upper = ,f.lower = ,f.upper = ,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = x,interval = df,lower = ,upper = ,f.lower = ,f.upper = ,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = ,interval = df,lower = x,upper = ,f.lower = ,f.upper = ,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = ,interval = df,lower = ,upper = x,f.lower = ,f.upper = ,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = ,interval = df,lower = ,upper = ,f.lower = x,f.upper = ,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = x,interval = df,lower = ,upper = ,f.lower = ,f.upper = ,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = x,interval = df,lower = ,upper = ,f.lower = ,f.upper = x,extendInt = ,check.conv = ,tol = ,maxiter = ,trace = x)
uniroot(f = ,interval = df,lower = ,upper = ,f.lower = ,f.upper = ,extendInt = ,check.conv = x,tol = ,maxiter = ,trace = x)
uniroot(f = ,interval = df,lower = ,upper = ,f.lower = ,f.upper = ,extendInt = x,check.conv = ,tol = ,maxiter = ,trace = x)


#update
update(x = ,...)
update(object = x)
#update.default
update.default(x = ,...)
update.default(evaluate = ,object = ,formula. = x)
update.default(evaluate = ,object = x,formula. = x)
update.default(evaluate = x,object = df,formula. = x)

update.formula(x = ,...)
#update.formula
update.formula(old = ,new = x)
update.formula(old = x,new = )
update.formula(old = "x",new = )

#var
var(x = ,...)
var(x = ,y = ,na.rm = ,use = x)
var(x = ,y = x,na.rm = ,use = )
var(x =x ,y = ,na.rm = ,use = x)
#var.test
var.test(x = ,...)
var.test(x = x)
var.test(x = ,y = x)
var.test(x = "x",y = x)
var.test(x = ,y = "x")

#variable.names
variable.names(x = ,...)
#varimax
varimax(x = ,...)
varimax(x = ,normalize = ,eps = x)
varimax(x = x,normalize = ,eps = )
varimax(x = x,normalize = x,eps = x)

#vcov
vcov(x = ,...)
vcov(object = x)
vcov(2)
vcov(df)
#weighted.mean
weighted.mean(x = ,...)
weighted.mean(x = ,w = x)
weighted.mean(x =x,w = ,na.rm = x)
#weighted.residuals
weighted.residuals(x = ,...)
weighted.residuals(obj = ,drop0 = x)
weighted.residuals(obj = x,drop0 = )
weighted.residuals(xxx)
#weights
weights(x = ,...)
weights(object = x)
weights(object = 3)
weights(object = "x")
#wilcox.test
wilcox.test(x = ,...)
wilcox.test(x = x)
#window
window(x = ,...)

#window<-
`window<-`(x = ,...)
`window<-`(x)
`window<-`(df)
`window<-`(2)
#write.ftable
write.ftable(x = ,...)
write.ftable(x = ,file = , sep = , eol = , na = , justify = , quote = , row.names = , col.names = , append = , verbose = x)
write.ftable(x = ,file = , sep = , eol = , na = , justify = , quote = , row.names = , col.names = , append = x, verbose = )
write.ftable(x = ,file = , sep = , eol = , na = , justify = , quote = , row.names = , col.names = x, append = , verbose = )
write.ftable(x = ,file = , sep = , eol = , na = , justify = , quote = , row.names = x, col.names = , append = , verbose = )
write.ftable(x = ,file = , sep = , eol = , na = , justify = , quote = x, row.names = , col.names = , append = , verbose = )
write.ftable(x = ,file = , sep = , eol = , na = , justify = x, quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = ,file = , sep = , eol = , na = x, justify = , quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = ,file = , sep = , eol = x, na = , justify = , quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = ,file = , sep = x, eol = , na = , justify = , quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = ,file = x, sep = , eol = , na = , justify = , quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = x,file = , sep = , eol = , na = , justify = , quote = , row.names = , col.names = , append = , verbose = )

Ftable <- ftable(x = 2)
write.ftable(x = ,file = , sep = , eol = , na = , justify = , quote = , row.names = , col.names = , append = , verbose = "x")
write.ftable(x = x,file = , sep = , eol = , na = , justify = , quote = , row.names = , col.names = , append = "x", verbose = )
write.ftable(x = x,file = , sep = , eol = , na = , justify = , quote = , row.names = , col.names = "x", append = , verbose = )
write.ftable(x = ,file = , sep = , eol = , na = , justify = , quote = , row.names = "x", col.names = , append = , verbose = )
write.ftable(x = Ftable,file = , sep = , eol = , na = , justify = , quote = "x", row.names = , col.names = , append = , verbose = )
write.ftable(x = Ftable,file = , sep = , eol = , na = , justify = "x", quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = Ftable,file = , sep = , eol = , na = "x", justify = , quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = ,file = , sep = "x", eol = , na = , justify = , quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = Ftable,file = "x", sep = , eol = , na = , justify = , quote = , row.names = , col.names = , append = , verbose = )
write.ftable(x = "x",file = , sep = , eol = , na = , justify = , quote = , row.names = , col.names = , append = , verbose = )

#xtabs
xtabs(x = ,...)
xtabs(formula = ,data = ,subset = ,na.action = ,addNA = ,drop.unused.levels = x)
xtabs(formula = ,data = ,subset = ,na.action = ,addNA = x,drop.unused.levels = )
xtabs(formula = ,data = ,subset = ,na.action = x,addNA = ,drop.unused.levels = )
xtabs(formula = ,data = ,subset = x,na.action = ,addNA = ,drop.unused.levels = )
xtabs(formula = ,data = x,subset = ,na.action = ,addNA = ,drop.unused.levels = )
xtabs(formula = x,data = ,subset = ,na.action = ,addNA = ,drop.unused.levels = )
xtabs(formula = ,data = x,subset = ,na.action = ,addNA = ,drop.unused.levels = )
xtabs(formula = ,data = ,subset = ,na.action = ,addNA = "x",drop.unused.levels = )
xtabs(formula = ,data = ,subset = ,na.action = "x",addNA = ,drop.unused.levels = )
xtabs(formula = ,data = ,subset = "x",na.action = ,addNA = ,drop.unused.levels = )
xtabs(formula = ,data = "x",subset = ,na.action = ,addNA = ,drop.unused.levels = )
xtabs(formula = "x",data = ,subset = ,na.action = ,addNA = ,drop.unused.levels = )
