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
  result <- tryCatch({
    test[e]
  },error = function(e){
    e
  })
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
#abs
#acos
#acosh
#activeBindingFunction
#addNA
#addTaskCallback
#agrep
#agrepl
#alist
alist(5)
#all
all(4,na.rm = true)
all(5)

#all.equal
result <- tryCatch({
  base::all.equal(4,test[2,])
},error = function(e){
  e
})
#all.equal.character
result <- tryCatch({
  base::all.equal("eset","taturo")
},error = function(e){
  e
})
#all.equal.default
all.equal.default()
#all.equal.environment
env1 <- new.env()
env1$a <- 1
env1$b <- 2

env2 <- new.env()
env2$a <- 4
env2$b <- 0

result <- tryCatch({
  base::all.equal.encvironment(env1,env2)
},error = function(e){
  e
})
#all.equal.envRefClass
all.equal.envRefClass()
#all.equal.factor
all.equal.factor()
#all.equal.formula
all.equal.formula()
#all.equal.function
all.equal.function()
#all.equal.language
all.equal.language()
#all.equal.list
result <- tryCatch({all.equal.list(5)},error = function(e){
  e
})

#all.equal.numeric
all.equal.numeric(target = ,current = ,tolerance = ,scale = ,countEQ = ,formatFUN = ,format(),check.attributes = )
#all.equal.POSIXt
all.equal.POSIXt(target = ,current = ,tolerance = ,scale = ,check.tzone = )
#all.equal.raw
all.equal.raw(target = ,current = ,check.attributes = )
#all.names
all.names(expr = ,functions = ,max.names = ,unique = )
#all.vars
all.vars(expr = ,functions = ,max.names = ,unique = )
#allowInterrupts
allowInterrupts(expr = )
#any
any( ,na.rm = )
#anyDuplicated
anyDuplicated(x = ,incomparables = )
#anyDuplicated.array
anyDuplicated.array(x = ,incomparables = ,MARGIN = ,fromLast = )
#anyDuplicated.data.frame
anyDuplicated.data.frame(x = ,incomparables = ,fromLast = )
#anyDuplicated.default
anyDuplicated.default(x = ,incomparables = ,fromLast = )
#anyDuplicated.matrix
anyDuplicated.matrix(x = ,incomparables = ,MARGIN = ,fromLast = )
#anyNA
anyNA(x = ,recursive = )
#anyNA.data.frame
anyNA.data.frame(x = ,recursive = )
#anyNA.numeric_version
anyNA.numeric_version(x = ,recursive = )
#anyNA.POSIXlt
anyNA.POSIXlt(x = ,recursive = )
#aperm
aperm(a = ,perm = )
originaldf <- matrix(1:12, nrow = 3, ncol = 4)
change <- c(2)
result <- tryCatch({
originaldf <- aperm(originaldf,change)
},function(e){
  e
})
#aperm.default
aperm.default(a = ,perm = ,resize = )

#aperm.table
aperm.table(a = ,perm = ,resize =,keep.class = )
#append
append(x = ,values = ,after = )
aptest <- c(1,2,3,4)
appendValue <- 5
result <- tryCatch({
  base::append(aptest,after = ６,value = appendValue)
},error = function(e){
  e
})
#apply
applyTest <- c(1,2,3,5)
result <- tryCatch({
  base::apply(applyTest,c(4,6),FUN = mean)
},function(e){
  e
})

#Arg
Arg(z = )
#args
args(f1)
#array
array(1:13,dim = 3:4,dimnames = list(c("a","b","c"),c("d","e","f","g")))

result <- tryCatch({
  array(1:13,dimnames = list(c("x","y")))
},error = function(e){
  e
})

#arrayInd
result <- tryCatch({
arrayInd(1:12,dim = c(3,4))
},error = function(e){
  e
})
result <- tryCatch({
arrayInd(1:12,dim = c(3,4),dims = c(3,4))
},error = function(e){
  e
})

#as.array
as.array(x = )
arrayTest <- matrix(1:12, nrow = 3, ncol = 4)
result <- tryCatch({as.array()},error = function(e){e})
#as.array.default
as.array.default(x = )
#as.call
as.call(x = )
result <- tryCatch({as.call()},error = function(e){e})
result <- tryCatch({as.call(92)},error = function(e){e})
#as.character
as.character(x = )
result <- tryCatch({as.character(abc)},error =function(e){e})
#as.character.condition
as.character.condition(x = )
result <- tryCatch({as.character(condition)},error =function(e){e})
#as.character.Date 
as.character.Date(x = )
result <- tryCatch({as.character(Date(abc))},error =function(e){e})

#as.character.default
as.character.default(x = )
result <- tryCatch({as.character.default(abc)},error =function(e){e})
#as.character.error 
as.character.error(x = )
result <- tryCatch({as.character.error(abc)},error =function(e){e})
#as.character.factor 
as.character.factor(x = )
result <- tryCatch({as.character.factor(abc)},error =function(e){e})
#as.character.hexmode 
as.character.hexmode(x = )
result <- tryCatch({as.character.hexmode(abc)},error =function(e){e})
#as.character.numeric_version
as.character.numeric_version(x = )
#as.character.octmode
as.character.octmode(x = )
#as.character.POSIXt
as.character.POSIXt(x = )
#as.character.srcref
as.character.srcref(x = ,useSource = ,to = )
#as.complex
as.complex(x = )
result <- tryCatch({as.complex(abc)},error =function(e){e})

#as.data.frame
as.data.frame(x = ,row.names = ,optional = )
list <- list(a = 1:3, b = 4:9)
result <- tryCatch({as.data.frame(
 abc
)},error = function(e){e})
#as.data.frame.array
as.data.frame.array(x = ,row.names = ,optional = )
#as.data.frame()  
as.data.frame(x = ,row.names = ,optional = )
result <- tryCatch({as.data.frame(arrayTest)},error = function(e){e})

#as.data.frame.AsIs 
as.data.frame.AsIs(x = ,row.names = ,optional = )
result <- tryCatch({as.data.frame(AsIs(abc))},error = function(e){e})
#as.data.frame.character 
as.data.frame.character(x = ,stringsAsFactors = )
result <- tryCatch({as.data.frame(character(abc))},error = function(e){e})
#as.data.frame.complex
as.data.frame.complex(x = ,row.names = ,optional = ,nm = )
result <- tryCatch({as.data.frame(complex(abc))},error = function(e){e})
#as.data.frame.data.frame 
as.data.frame.data.frame(x = ,row.names = )
result <- tryCatch({as.data.frame(data.frame(abc))},error = function(e){e})
#as.data.frame.Date 
as.data.frame.Date(x = ,row.names = ,optional = ,nm = )
result <- tryCatch({as.data.frame(Date(abc))},error = function(e){e})
#as.data.frame.default
as.data.frame.default(x = )
#as.data.frame.difftime
as.data.frame.difftime(x = ,row.names = ,optional = ,nm = )
#as.data.frame.factor
as.data.frame.factor(x = ,row.names = ,optional = ,nm = )
#as.data.frame.integer
as.data.frame.integer(x = ,row.names = ,optional = ,nm = )
#as.data.frame.list
as.data.frame.list(x = ,row.names = ,optional = ,cut.names = ,col.names = ,fix.empty.names = ,check.names = ,stringsAsFactors = )
#as.data.frame.logical
as.data.frame.logical(x = ,row.names = ,optional = ,nm = )
#as.data.frame.matrix
as.data.frame.matrix(x = ,row.names = ,optional = ,make.names = ,stringsAsFactors = )
#as.data.frame.model.matrix
as.data.frame.model.matrix(x = ,row.names = ,optional = ,make.names = )
#as.data.frame.noquote
as.data.frame.noquote(x = ,row.names = ,optional = ,nm = )
#as.data.frame.numeric
as.data.frame.numeric(x = ,row.names = ,optional = ,nm = )
#as.data.frame.numeric_version
as.data.frame.numeric_version(x = ,row.names = ,optional = ,nm = )
#as.data.frame.ordered
as.data.frame.ordered(x = ,row.names = ,optional = ,nm = )
#as.data.frame.POSIXct
as.data.frame.POSIXct(x = ,row.names = ,optional = ,nm = )
#as.data.frame.POSIXlt
as.data.frame.POSIXlt(x = ,row.names = ,optional = ,nm = )
#as.data.frame.raw
as.data.frame.raw(x = ,row.names = ,optional = ,nm = )
#as.data.frame.table
as.data.frame.table(x = ,row.names = ,optional = ,responseName = ,stringsAsFactors = )
#as.data.frame.ts
as.data.frame.ts(x = ,row.names = ,optional = ,nm = )
#as.data.frame.vector
as.data.frame.vector(x = ,row.names = ,optional = ,nm = )
#as.Date 
as.Date(x = ,format = ,tz = )
result <- tryCatch({as.Date(abc)},error = function(e){e})

#as.Date.character
as.Date.character(x = ,format = ,tz = )
#as.Date.default
as.Date.default(x = ,format = ,tz = )
#as.Date.factor
as.Date.factor(x = )
#as.Date.numeric
as.Date.numeric(x = ,origin = )
#as.Date.POSIXct
as.Date.POSIXct(x = ,tz = )

#as.Date.POSIXlt
as.Date.POSIXlt(x = ,tz = )
#as.difftime 
as.difftime(x = ,units = )
result <- tryCatch({as.difftime(abc)},error = function(e){e})
#as.double 
as.double(x = )
result <- tryCatch({as.double(abc)},error = function(e){e})

#as.double.difftime
as.double.difftime(x = ,units = )
#as.double.POSIXlt
as.double.POSIXlt(x = )
#as.environment 
as.environment(x = )
result <- tryCatch({as.environment(abc)},error = function(e){e})
#as.expression 
as.expression(x = )
result <- tryCatch({as.expression(abc)},error = function(e){e})
#as.expression.default
as.expression.default(x = )
#as.factor
as.factor(x = )
#as.function
as.function(x = )
#as.function.default
as.function.default(x = )
#as.hexmode
as.hexmode(x = )
#as.integer
as.integer(x = )
#as.list
as.list(x = )

#as.list.data.frame
as.list.data.frame(x = ,row.names = ,optional = )
#as.list.Date
as.list.Date(x = )
#as.list.default
as.list.default(x = )
#as.list.difftime
as.list.difftime(x = )
#as.list.environment
as.list.environment(x = )
#as.list.factor
as.list.factor(x = )
#as.list.function
as.list.function(x = )
#as.list.numeric_version
as.list.numeric_version(x = )

#as.list.POSIXct
as.list.POSIXct(x = )
#as.list.POSIXlt
as.list.POSIXlt(x = )
#as.logical
as.logical(x = )
#as.logical.factor
as.logical.factor(x = )
#as.matrix
as.matrix(x = ,rownames.force = )
#as.matrix.data.frame
as.matrix.data.frame(x = ,row.names = )
#as.matrix.default
as.matrix.default(x = )
#as.matrix.noquote
as.matrix.noquote(x = ,rownames.force = )
#as.matrix.POSIXlt
as.matrix.POSIXlt(x = )
#as.name
as.name(x = )
#as.null
as.null(x = )
#as.null.default
as.null.default(x = )
#as.numeric
as.numeric(x = )
#as.numeric_version
as.numeric_version(x = )
#as.octmode
as.octmode(x = )
#as.ordered
as.ordered(x = )
#as.package_version
as.package_version(x = )
#as.pairlist
as.pairlist(x = )
#as.POSIXct
as.POSIXct(x = ,tz = ,format = ,tryFormats = )
#as.POSIXct.Date
as.POSIXct.Date(x = ,tz = )
#as.POSIXct.default
as.POSIXct.default(x = ,tz = ,format = ,tryFormats = )
#as.POSIXct.numeric
as.POSIXct.numeric(x = ,tz = )
#as.POSIXct.POSIXlt
as.POSIXct.POSIXlt(x = ,tz = )
#as.POSIXlt
as.POSIXlt(x = ,tz = ,format = ,tryFormats = )
#as.POSIXlt.character
as.POSIXlt.character(x = ,tz = )
#as.POSIXlt.Date
as.POSIXlt.Date(x = ,tz = )

#as.POSIXlt.default
as.POSIXlt.default(x = ,tz = ,format = ,tryFormats = )
#as.POSIXlt.factor
as.POSIXlt.factor(x = ,tz = )
#as.POSIXlt.numeric
as.POSIXlt.numeric(x = ,tz = )
#as.POSIXlt.POSIXct
as.POSIXlt.POSIXct(x = ,tz = )
#as.qr
as.qr(x = ,pivot = )
#as.raw
as.raw(x = )
#as.single
as.single(x = )

#as.single.default
as.single.default(x = )
#as.symbol
as.symbol(x = )
#as.table
as.table(x = )
#as.table.default
as.table.default(x = )
#as.vector
as.vector(x = ,mode = )
#as.vector.factor
as.vector.factor(x = )
#asin
asin(x = )
#asinh
asinh(x = )
#asNamespace
asNamespace(ns = )
#asplit
asplit(x = ,dims = ,drop = )
#asS3
asS3(object = ,class = )
#asS4
asS4(object = ,class = )
#assign
assign(x = ,value = ,pos = ,envir = ,inherits = )
#atan
atan(x = )
#atan2
atan2(y = ,x = )
#atanh
atanh(x = )
#attach
attach(pos = ,name = ,warn.conflicts = ,pos = ,name = ,warn.conflicts = )
#attachNamespace
attachNamespace(package = ,pos = )
#attr
attr(x = ,which = ,exact = )
#attr.all.equal
attr.all.equal(x = ,y = ,tolerance = ,scale = ,check.attributes = )
#attr<-
`attr<-`()
#attributes
attributes(object = )
#attributes<-
`attributes<-`()
#autoload
autoload(pkgname = ,file = ,character.only = )
#autoloader
autoloader(pkgname = ,file = ,character.only = )
#backsolve
backsolve(r = ,x = ,k = ,upper.tri = )
#baseenv
baseenv()
#basename
basename(path = ,suffix = )
#besselI
besselI(x = ,nu = )
#besselJ
besselJ(x = ,nu = )
#besselK
besselK(x = ,nu = )
#besselY
besselY(x = ,nu = )
#beta
beta(x = ,y = )
#bindingIsActive
bindingIsActive(x = )
#bindingIsLocked
bindingIsLocked(x = )
#bindtextdomain
bindtextdomain(domain = ,dirname = )
#bitwAnd
bitwAnd(x = ,y = )
#bitwNot
bitwNot(x = )
#bitwOr
bitwOr(x = ,y = )
#bitwShiftL
bitwShiftL(x = ,n = )
#bitwShiftR
bitwShiftR(x = ,n = )
#bitwXor
bitwXor(a = ,b = )
#body 
body(x = )
#body<-
`body<-`()
#bquote
bquote(expr = )
#break
break()
#browser
browser(expr = )
#browserCondition
browserCondition(expr = )
#browserSetDebug
browserSetDebug(flag = )
#browserText
browserText(expr = )
#builtins
builtins()
#by
by(data = ,INDICES = ,FUN = ,...,simplify = )
#by.data.frame
by.data.frame(data = ,INDICES = ,FUN = ,...,simplify = )
#by.default
by.default(data = ,INDICES = ,FUN = ,...,simplify = )
#bzfile
bzfile(description = ,open = ,encoding = ,compression = ,close = )
#c
c(...)
#c.Date
c.Date(...)

#c.difftime
c.difftime(...)
#c.factor
c.factor(...)
#c.noquote
c.noquote(...)
#c.numeric_version
c.numeric_version(...)
#c.POSIXct
c.POSIXct(...)
#c.POSIXlt
c.POSIXlt(...)
#c.warnings
c.warnings(...)
#call
call(name = ,...)

#callCC
callCC(fun = )
#capabilities
capabilities()
#casefold
casefold(x = ,upper = )
#cat
cat(...,file = ,sep = ,fill = ,labels = ,append = )
#cbind
cbind(...)
#cbind.data.frame
cbind.data.frame(...)
#ceiling
ceiling(x = )
#char.expand
char.expand(x = )
#character
character(length = )
#charmatch
charmatch(x = ,table = ,nomatch = )
#charToRaw
charToRaw(x = )
#chartr
chartr(old = ,new = ,x = )
#check_tzones
check_tzones(tz1 = ,tz2 = )
#chkDots
chkDots(...)
#chol
chol(x = x,pivot = true,LINPACK = true,tol = x)
chol(x = test)
#chol.default
chol.default(x = x,pivot = true,LINPACK = true)

#chol2inv
chol2inv(x = x,size = NCOL(x),LINPACK = true)

#choose
choose(n =x,k = x)
#class
class(x = x)
#class<-
`class<-`(x = ,value = )
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
#conflicts
conflicts(pkg = ,mask.ok = ,exclude = )

#Conj
Conj(x = ,z = )

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
#droplevels
droplevels(x = )
#droplevels.data.frame
droplevels.data.frame(x = )
#droplevels.factor
droplevels.factor(x = )
#dump
dump(object = ,file = ,...)
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

#file.remove
file.remove(...)
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
#invokeRestart
invokeRestart(restart = ,...)
#invokeRestartInteractively
invokeRestartInteractively(restart = ,...)
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
#max.col
max.col(x = ,ties.method = )
#mean
mean(x = ,...)
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
#merge.data.frame
merge.data.frame(x = ,y = ,...)
#merge.default
merge.default(x = ,y = ,...)
#message
message(...)
#mget
mget(x = ,envir = ,inherits = ,mode = )
#min
min(...)
#missing
missing(x = )
#Mod
Mod(x = ,y = )
#mode
mode(x = )
#mode<-
`mode<-`(x = ,value = )
#month.abb
month.abb
#month.name
month.name
#months
months(x = ,abbreviate = )
#months.Date
months.Date(x = ,abbreviate = )
#months.POSIXt
months.POSIXt(x = ,abbreviate = )
#mostattributes<-
`mostattributes<-`(x = ,value = )
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
#next
next(n = )  
#NextMethod
NextMethod(...)
#ngettext
ngettext(msgid = ,msgid_plural = ,n = )
#nlevels
nlevels(x = )
#noquote
noquote(x = )
#norm
norm(x = ,type = ,...)
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
#Ops.numeric_version
Ops.numeric_version(e1 = ,e2 = )
#Ops.ordered
Ops.ordered(e1 = ,e2 = )
#Ops.POSIXt
Ops.POSIXt(e1 = ,e2 = )
#options
options(...)
#order
order(...)
#ordered
ordered(x = ,levels = ,labels = ,exclude = )
#outer
outer(X = ,Y = ,FUN = ,...)
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
#packBits
packBits(x = )
#pairlist
pairlist(...)
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
#paste
paste(...,sep = ,collapse = ,sep = ,collapse = )
#paste0
paste0(...,sep = ,collapse = ,sep = ,collapse = )
#path.expand
path.expand(path = )
#path.package
path.package(package = ,quiet = ,verbose = ,lib.loc = )
#pcre_config
pcre_config()

#pi
pi
#pipe
pipe(command = ,open = ,encoding = ,text = )
#plot
plot(x = ,y = ,...)
#pmatch
pmatch(x = ,table = ,nomatch = )
#pmax
pmax(...)
#pmax.int
pmax.int(...)
#pmin
pmin(...)
#pmin.int
pmin.int(...)
#polyroot
polyroot(x = )
#pos.to.env
pos.to.env(pos = ,envir = )
#Position
Position(f = ,x = )
#pretty
pretty(x = ,n = ,min.n = ,max.n = ,...)
#pretty.default
pretty.default(x = ,n = ,min.n = ,max.n = ,...)
#prettyNum
prettyNum(x = ,big.mark = ,big.interval = ,small.mark = ,small.interval = ,decimal.mark = ,scientific = ,drop0trailing = )
#print
print(x = ,...)
#print.AsIs
print.AsIs(x = ,...)
#print.by
print.by(x = ,...)
#print.condition
print.condition(x = ,...)
#print.connection
print.connection(x = ,...)
#print.data.frame
print.data.frame(x = ,...)
#print.Date
print.Date(x = ,...)
#print.default
print.default(x = ,...)
#print.difftime
print.difftime(x = ,...)
#print.Dlist
print.Dlist(x = ,...)
#print.DLLInfo
print.DLLInfo(x = ,...)
#print.DLLInfoList
print.DLLInfoList(x = ,...)
#print.DLLRegisteredRoutines
print.DLLRegisteredRoutines(x = ,...)
#print.eigen
print.eigen(x = ,...)
#print.factor
print.factor(x = ,...)
#print.function
print.function(x = ,...)
#print.hexmode
print.hexmode(x = ,...)
#print.libraryIQR
print.libraryIQR(x = ,...)
#print.listof
print.listof(x = ,...)    
#print.NativeRoutineList
print.NativeRoutineList(x = ,...)
#print.noquote
print.noquote(x = ,...)
#print.numeric_version
print.numeric_version(x = ,...)

#print.octmode
print.octmode(x = ,...) 
#print.packageInfo
print.packageInfo(x = ,...)
#print.POSIXct
print.POSIXct(x = ,...)
#print.POSIXlt
print.POSIXlt(x = ,...)
#print.proc_time
print.proc_time(x = ,...)
#print.restart
print.restart(x = ,...)
#print.rle
print.rle(x = ,...)
#print.simple.list
print.simple.list(x = ,...)
#print.srcfile
print.srcfile(x = ,...)
#print.srcref
print.srcref(x = ,...)
#print.summary.table
print.summary.table(x = ,...)
#print.summary.warnings
print.summary.warnings(x = ,...)
#print.summaryDefault
print.summaryDefault(x = ,...)
#print.table
print.table(x = ,...)
#print.warnings
print.warnings(x = ,...)
#prmatrix
prmatrix(x = ,quote = ,right = ,max = ,...)
#proc.time
proc.time()
#prod
prod(...)   
#prop.table
prop.table(x = ,margin = ,...)
#proportions
proportions(x = ,margin = ,...)
#provideDimnames
provideDimnames(x = ,value = )
#psigamma
psigamma(x = ,deriv = )
#pushBack
pushBack(x = ,value = )
#pushBackLength
pushBackLength(x = ,value = )
#q
q(...)
#qr
qr(x = ,...)
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
#quote
quote(...)
#R_system_version
R_system_version()
#R.home
R.home(component = )
#R.version
R.version()
#R.Version
R.Version()
#R.version.string
R.version.string()
#range
range(...)
#range.default
range.default(...)
#rank
rank(x = ,ties.method = ,...)
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
#rawToChar
rawToChar(x = ,multiple = )
#rbind
rbind(...)
#rbind.data.frame
rbind.data.frame(...)
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
#readLines
  readLines(con = ,n = ,ok = ,warn = ,encoding = ,skipNul = )
#readRDS
readRDS(file = )
#readRenviron
readRenviron(file = )
#Recall
Recall(...)
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
#remove
remove(...)
#removeTask

#rep
rep(x = ,...)
#rep_len
rep_len(x = ,length.out = )
#rep.Date
rep.Date(x = ,times = )
#rep.difftime
rep.difftime(x = ,times = )
#rep.factor
rep.factor(x = ,times = )
#rep.int
rep.int(x = ,times = )
#rep.numeric_version
rep.numeric_version(x = ,times = )
#rep.POSIXct
rep.POSIXct(x = ,times = )
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
#row.names<-.data.frame
`row.names<-.data.frame`(x = ,value = )
#row.names<-.default
`row.names<-.default`(x = ,value = )
#rowMeans
rowMeans(x = ,na.rm = )
#rownames
rownames(x = )
#rownames<-
`rownames<-`(x = ,value = )
#rowsum
rowsum(x = ,group = ,reorder = ,...)
#rowsum.data.frame
rowsum.data.frame(x = ,group = ,reorder = ,...)
#rowsum.default
rowsum.default(x = ,group = ,reorder = ,...)
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
#solve.default
solve.default(a = ,b = ,tol = ,LINPACK = ,...)
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
#split
split(x = ,f = ,drop = ,...)
#split.data.frame
split.data.frame(x = ,f = ,drop = ,...)
#split.Date
split.Date(x = ,f = ,drop = ,...)
#split.default
split.default(x = ,f = ,drop = ,...)
#split.POSIXct
split.POSIXct(x = ,f = ,drop = ,...)
#split<-
`split<-`(x = ,f = ,drop = ,...)
#split<-.data.frame
`split<-.data.frame`(x = ,f = ,drop = ,...)
#split<-.default
`split<-.default`(x = ,f = ,drop = ,...)
#sprintf
sprintf(fmt = ,...)
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
#stdin
stdin()
#stdout
stdout()
#stop
stop(...)
#stopifnot
stopifnot(...)
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
#substring
substring(text = ,first = ,last = )
#substring<-
`substring<-`(text = ,first = ,last = ,value = )
#sum
sum(...)
#summary
summary(object = ,...)
#summary.connection
summary.connection(con = ,...)
#summary.data.frame
summary.data.frame(object = ,...)
#Summary.data.frame
Summary.data.frame(object = ,...)
#summary.Date
summary.Date(object = ,...)
#Summary.Date
Summary.Date(object = ,...)
#summary.default
summary.default(object = ,...)
#Summary.difftime
Summary.difftime(object = ,...)
#summary.factor
summary.factor(object = ,...)
#Summary.factor
Summary.factor(object = ,...)
#summary.matrix
summary.matrix(object = ,...)
#Summary.numeric_version
Summary.numeric_version(object = ,...)
#Summary.ordered
Summary.ordered(object = ,...)
#summary.POSIXct
summary.POSIXct(object = ,...)
#Summary.POSIXct
Summary.POSIXct(object = ,...)
#summary.POSIXlt
summary.POSIXlt(object = ,...)
#Summary.POSIXlt
Summary.POSIXlt(object = ,...)
#summary.proc_time
summary.proc_time(object = ,...)

#summary.srcfile
summary.srcfile(object = ,...)

#summary.srcref
summary.srcref(object = ,...)
#summary.table
summary.table(object = ,...)

#summary.warnings
summary.warnings(object = ,...)

#suppressMessages
suppressMessages(expr = )
#suppressPackageStartupMessages
suppressPackageStartupMessages(expr = )
#suppressWarnings
suppressWarnings(expr = )
#suspendInterrupts
suspendInterrupts()
#svd
svd(x = ,nu = ,nv = ,LINPACK = ,...)
#sweep
sweep(x = ,MARGIN = ,STATS = ,FUN = ,...)

#switch
switch(expr = ,...)
#sys.call
sys.call(which = )
#sys.calls
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
#Sys.setFileTime
Sys.setFileTime(path = ,times = )
#Sys.setlocale
Sys.setlocale(category = ,locale = )
#Sys.sleep
Sys.sleep(secs = )
#sys.source
sys.source(file = ,envir = ,...)
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
#system.file
system.file(package = ,...)
#system.time
system.time(expr = )
#system2
system2(command = ,args = ,stdout = ,stderr = ,stdin = ,input = ,env = ,wait = ,show.output.on.console = ,minimized = ,invisible = ,...)
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
#tabulate
tabulate(x = ,nbins = )
#tan
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
#transform.data.frame
transform.data.frame(x = ,...)
#transform.default
transform.default(x = ,...)

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
#unique.array
unique.array(x = ,...)
#unique.data.frame
unique.data.frame(x = ,...)
#unique.default
unique.default(x = ,...)
#unique.matrix
unique.matrix(x = ,...)
#unique.numeric_version
unique.numeric_version(x = ,...)
#unique.POSIXlt
unique.POSIXlt(x = ,...)
#unique.warnings
unique.warnings(x = ,...)
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
#warningCondition
warningCondition(message = ,call = )
#warnings
warnings(...)
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
#while
while(test = ,expr = )
#with
with(data = ,expr = ,...)
#with.default
with.default(data = ,expr = ,...)
#withAutoprint
withAutoprint(expr = )
#withCallingHandlers
withCallingHandlers(expr = ,...)
#within
within(data = ,expr = ,...)
#within.data.frame
within.data.frame(data = ,expr = ,...)
#within.list
within.list(data = ,expr = ,...)
#withRestarts
withRestarts(expr = ,...)
#withVisible
withVisible(expr = )
#write
write(x = ,file = ,nmax = ,append = ,sep = ,na = ,...)
#write.dcf
write.dcf(x = ,file = ,...)
#writeBin
writeBin(object = ,con = ,size = ,...)
#writeChar
writeChar(object = ,con = ,eos = ,useBytes = )
#writeLines
writeLines(text = ,con = ,sep = ,useBytes = )
#xor
xor(x = ,y = )
#xpdrows.data.frame
xpdrows.data.frame(x = ,...)
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
#abline
abline(a = ,b = ,h = ,v = ,reg = ,coef = ,untf = )
#arrows
arrows(x0 = ,y0 = ,x1 = ,y1 = ,angle = ,code = ,length = ,...)
#assocplot
assocplot(x = ,...)
#axis
axis(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,hadj = ,padj = ,...)
#Axis
Axis(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,hadj = ,padj = ,...)
#axis.Date
axis.Date(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,format = ,digits = ,cex.axis = ,tcl = ,mgp = ,las = ,...)
#axis.POSIXct
axis.POSIXct(side = ,at = ,labels = ,tick = ,line = ,pos = ,outer = ,font = ,lty = ,lwd = ,lwd.ticks = ,col = ,col.ticks = ,format = ,digits = ,cex.axis = ,tcl = ,mgp = ,las = ,...)
#axTicks
axTicks(side = ,...)
#barplot
barplot(...)
#barplot.default
barplot.default(...)
#box
boxplot(x = ,...)
#boxplot
boxplot(...)
#boxplot.default
boxplot.default(...)
#boxplot.matrix
boxplot.matrix(x = ,...)
#bxp
bxp(list = ,horizontal = ,...)
#cdplot
cdplot(x = ,...)
#clip
clip(x = ,y = )
#close.screen
close.screen(which = )
#co.intervals
co.intervals(x = ,...)
#contour
contour(x = ,y = ,z = ,nlevels = ,levels = ,labels = ,xlim = ,ylim = ,zlim = ,zlab = ,main = ,sub = ,xlab = ,ylab = ,asp = ,axes = ,frame.plot = ,...)
#contour.default
contour.default(x = ,y = ,z = ,nlevels = ,levels = ,labels = ,xlim = ,ylim = ,zlim = ,zlab = ,main = ,sub = ,xlab = ,ylab = ,asp = ,axes = ,frame.plot = ,...)
#coplot
coplot(formula = ,data = ,...)
#curve
curve(expr = ,from = ,to = ,n = ,add = ,type = ,xname = ,xaxs = ,yaxs = ,...)
#dotchart
dotchart(x = ,labels = ,groups = ,gcolor = ,color = ,cex = ,pch = ,pt.cex = ,bg = ,main = ,xlab = ,ylab = ,...)
#erase.screen
erase.screen(which = )
#filled.contour
filled.contour(x = ,y = ,z = ,levels = ,color.palette = ,color = ,plot.title = ,plot.axes = ,key.title = ,key.axes = ,asp = ,xaxs = ,yaxs = ,...)
#fourfoldplot
fourfoldplot(x = ,color = ,conf.level = ,margin = ,main = ,...)
#frame
frame()
#grconvertX
grconvertX(x = ,from = ,to = )
#grconvertY
grconvertY(y = ,from = ,to = )
#grid
grid(nx = ,ny = ,col = ,lty = ,lwd = ,equilogs = ,...)
#hist
hist(x = ,breaks = ,freq = ,probability = ,include.lowest = ,right = ,density = ,angle = ,col = ,border = ,main = ,xlim = ,ylim = ,xlab = ,ylab = ,axes = ,plot = ,labels = ,nclass = ,warn.unused = ,offset = ,...)
#hist.default
hist.default(x = ,breaks = ,freq = ,probability = ,include.lowest = ,right = ,density = ,angle = ,col = ,border = ,main = ,xlim = ,ylim = ,xlab = ,ylab = ,axes = ,plot = ,labels = ,nclass = ,warn.unused = ,offset = ,...)
#identify
identify(x = ,n = ,plot = ,labels = ,offset = ,...)
#image
image(x = ,y = ,z = ,xlim = ,ylim = ,zlim = ,axes = ,xlab = ,ylab = ,breaks = ,col = ,col.axis = ,col.lab = ,...)
#image.default
image.default(x = ,y = ,z = ,xlim = ,ylim = ,zlim = ,axes = ,xlab = ,ylab = ,breaks = ,col = ,col.axis = ,col.lab = ,...)
#layout
layout(...)
#layout.show
layout.show(n = ,...)
#lcm
lcm(x = )
#legend
legend(x = ,y = ,legend = ,fill = ,col = ,border = ,lty = ,lwd = ,pch = ,angle = ,density = ,bty = ,bg = ,box.lwd = ,box.lty = ,box.col = ,pt.bg = ,cex = ,pt.cex = ,pt.lwd = ,xjust = ,yjust = ,x.intersp = ,y.intersp = ,adj = ,text.width = ,text.col = ,text.font = ,merge = ,trace = ,plot = ,ncol = ,horiz = ,title = ,inset = ,xpd = ,title.col = ,title.adj = ,seg.len = ,...)
#lines
lines(x = ,y = ,type = ,...)
#lines.default
lines.default(x = ,y = ,type = ,...)
#locator
locator(n = ,type = ,...)
#matlines
matlines(x = ,y = ,type = ,...)
#matplot
matplot(x = ,y = ,type = ,...)
#matpoints
matpoints(x = ,y = ,type = ,...)
#mosaicplot
mosaicplot(x = ,...)
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
#persp
persp(x = ,y = ,z = ,theta = ,phi = ,expand = ,col = ,border = ,ticktype = ,nticks = ,...)
#pie
pie(x = ,labels = ,edges = ,radius = ,clockwise = ,init.angle = ,density = ,angle = ,col = ,border = ,lty = ,main = ,...)
#plot
plot(x = ,y = ,type = ,...)
#plot.default
plot.default(x = ,y = ,type = ,...)
#plot.design
plot.design(x = ,...)
#plot.function
plot.function(x = ,from = ,to = ,add = ,...)
#plot.new
plot.new()
#plot.window
plot.window(xlim = ,ylim = ,log = ,asp = ,...)
#plot.xy
plot.xy(xy = ,type = ,...)
#points
points(x = ,y = ,...)
#points.default
points.default(x = ,y = ,...)
#polygon
polygon(x = ,y = ,...)
#polypath
polypath(x = ,y = ,...)
#rasterImage
rasterImage(x = ,y = ,image = ,col = ,...)
#rect
rect(xleft = ,ybottom = ,xright = ,ytop = ,...)
#rug
rug(x = ,side = ,ticksize = ,line.col = ,...)
#screen
screen(...)
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
#add1
add1(x = ,...)
#addmargins
addmargins(x = ,...)
#aggregate
aggregate(x = ,...)
#aggregate.data.frame
aggregate.data.frame(x = ,...)

#aggregate.ts
aggregate.ts(x = ,...)
#AIC
AIC(x = ,...)
#alias
alias(x = ,...)
#anova
anova(x = ,...)
#ansari.test
ansari.test(x = ,...)
#aov
aov(x = ,...)
#approx
approx(x = ,...)
#approxfun
approxfun(x = ,...)
#ar
ar(x = ,...)
#ar.burg
ar.burg(x = ,...)
#ar.mle
ar.mle(x = ,...)
#ar.ols
ar.ols(x = ,...)
#ar.yw
ar.yw(x = ,...)
#arima
arima(x = ,...)
#arima.sim
arima.sim(x = ,...)
#arima0
arima0(x = ,...)
#arima0.diag
arima0.diag(x = ,...)
#ARMAacf
ARMAacf(x = ,...)
#ARMAtoMA
ARMAtoMA(x = ,...)
#as.dendrogram
as.dendrogram(x = ,...)
#as.dist
as.dist(x = ,...)
#as.formula
as.formula(x = ,...)
#as.hclust
as.hclust(x = ,...)
#as.stepfun
as.stepfun(x = ,...)
#as.ts
as.ts(x = ,...)
#asOneSidedFormula
asOneSidedFormula(x = ,...)
#ave
ave(x = ,...)
#bandwidth.kernel
bandwidth.kernel(x = ,...)
#bartlett.test
bartlett.test(x = ,...)
#BIC
BIC(x = ,...)
#binom.test
binom.test(x = ,...)
#binomial
binomial(x = ,...)
#biplot
biplot(x = ,...)
#Box.test
Box.test(x = ,...)

#bw.bcv
bw.bcv(x = ,nb = ,lower = ,upper = ,tol = )
#bw.nrd
bw.nrd(x = ,...)
#bw.nrd0
bw.nrd0(x = ,...)
#bw.SJ
bw.SJ(x = ,...)
#bw.ucv
bw.ucv(x = ,...)
#C
C(x = ,...)
#cancor
cancor(x = ,...)
#case.names
case.names(x = ,...)
#ccf
ccf(x = ,...)
#chisq.test
chisq.test(x = ,...)
#cmdscale
cmdscale(x = ,...)
#coef
coef(x = ,...)
#coefficients
coefficients(x = ,...)
#complete.cases
complete.cases(x = ,...)
#confint
confint(x = ,...)
#confint.default
confint.default(x = ,...)
#confint.lm
confint.lm(x = ,...)
#constrOptim
constrOptim(x = ,...)
#contr.helmert
contr.helmert(x = ,...)
#contr.poly
contr.poly(x = ,...)
#contr.SAS
contr.SAS(x = ,...)
#contr.sum
contr.sum(x = ,...)
#contr.treatment
contr.treatment(x = ,...)
#contrasts
contrasts(x = ,...)
#contrasts<-
`contrasts<-`(x = ,...)
#convolve
convolve(x = ,...)
#cooks.distance
cooks.distance(x = ,...)
#cophenetic
cophenetic(x = ,...)
#cor
cor(x = ,...)
#cor.test
cor.test(x = ,...)
#cov
cov(x = ,...)
#cov.wt
cov.wt(x = ,...)
#cov2cor
cov2cor(x = ,...)

#covratio
covratio(x = ,...)
#cpgram
cpgram(x = ,...)

#cutree
cutree(x = ,...)
#cycle
cycle(x = ,...)
#D
D(x = ,...)
#dbeta
  dbeta(x = ,shape1 = ,shape2 = ,ncp = ,log = )
#dbinom
  dbinom(x = ,size = ,prob = ,log = )
#dcauchy
  dcauchy(x = ,location = ,scale = ,log = )
#dchisq
  dchisq(x = ,df = ,ncp = ,log = )
#decompose
  decompose(x = ,...)
#delete.response
  delete.response(x = ,...)
#deltat
  deltat(x = ,...)
#dendrapply
  dendrapply(x = ,...)
#density
  density(x = ,...)
#density.default
  density.default(x = ,...)
#deriv
  deriv(x = ,...)
#deriv3
  deriv3(x = ,...)
#deviance
  deviance(x = ,...)
#dexp
  dexp(x = ,rate = ,log = )
#df
  df(x = ,...)
#df.kernel
  df.kernel(x = ,...)
#df.residual
  df.residual(x = ,...)
#DF2formula
  DF2formula(x = ,...)
#dfbeta
  dfbeta(x = ,...)
#dfbetas
  dfbetas(x = ,...)
#dffits
  dffits(x = ,...)
#dgamma
  dgamma(x = ,shape = ,rate = ,log = )
#dgeom
  dgeom(x = ,prob = ,log = )
#dhyper
  dhyper(x = ,m = ,n = ,k = ,log = )
#diffinv
  diffinv(x = ,...)
#dist
  dist(x = ,...)
#dlnorm
  dlnorm(x = ,meanlog = ,sdlog = ,log = )
#dlogis
  dlogis(x = ,location = ,scale = ,log = )
#dmultinom
  dmultinom(x = ,size = ,prob = ,log = )  
#dnbinom
  dnbinom(x = ,size = ,prob = ,log = )
#dnorm
  dnorm(x = ,mean = ,sd = ,log = )
#dpois
  dpois(x = ,lambda = ,log = )
#drop.scope
  drop.scope(x = ,...)
#drop.terms
  drop.terms(x = ,...)
#drop1
  drop1(x = ,...)
#dsignrank
  dsignrank(x = ,n = ,log = ) 
#dt
  dt(x = ,df = ,ncp = ,log = )
#dummy.coef
  dummy.coef(x = ,...)
#dummy.coef.lm
  dummy.coef.lm(x = ,...)
#dunif
  dunif(x = ,min = ,max = ,log = )  
#dweibull
  dweibull(x = ,shape = ,scale = ,log = )
#dwilcox
  dwilcox(x = ,m = ,n = ,log = )
#ecdf
  ecdf(x = ,...)
#eff.aovlist
  eff.aovlist(x = ,...)
#effects
  effects(x = ,...)
#embed
  embed(x = ,...)
#end
  end(x = ,...)
#estVar
  estVar(x = ,...)
#expand.model.frame
  expand.model.frame(x = ,...)
#extractAIC
  extractAIC(x = ,...)
#factanal
  factanal(x = ,...)
#factor.scope
  factor.scope(x = ,...)
#family
  family(x = ,...)
#fft
  fft(x = ,...)
#filter
  filter(x = ,...)
#fisher.test
  fisher.test(x = ,...)
#fitted
  fitted(x = ,...)
#fitted.values
  fitted.values(x = ,...)
#fivenum
  fivenum(x = ,...)
#fligner.test
  fligner.test(x = ,...)
#formula
  formula(x = ,...)
#frequency
  frequency(x = ,...)
#friedman.test
  friedman.test(x = ,...)
#ftable
  ftable(x = ,...)
#Gamma
  Gamma(x = ,...)
#gaussian
  gaussian(x = ,...)
#get_all_vars
  get_all_vars(x = ,...)
#getCall
  getCall(x = ,...)
#getInitial
  getInitial(x = ,...)
#glm
  glm(x = ,...)
#glm.control
  glm.control(x = ,...)
#glm.fit
  glm.fit(x = ,...)
#hasTsp
  hasTsp(x = ,...)
#hat
  hat(x = ,...)
#hatvalues
  hatvalues(x = ,...)
#hclust
  hclust(x = ,...)
#heatmap
  heatmap(x = ,...)
#HoltWinters
  HoltWinters(x = ,...)
#influence
  influence(x = ,...)
#influence.measures
  influence.measures(x = ,...)
#integrate
  integrate(x = ,...)
#interaction.plot
  interaction.plot(x = ,...)
#inverse.gaussian
  inverse.gaussian(x = ,...)
#IQR
  IQR(x = ,...)
#is.empty.model
  is.empty.model(x = ,...)
#is.leaf
  is.leaf(x = ,...)
#is.mts
  is.mts(x = ,...)
#is.stepfun
  is.stepfun(x = ,...)
#is.ts
  is.ts(x = ,...)
#is.tskernel
  is.tskernel(x = ,...)
#isoreg
  isoreg(x = ,...)
#KalmanForecast
  KalmanForecast(x = ,...)
#KalmanLike
  KalmanLike(x = ,...)
#KalmanRun
  KalmanRun(x = ,...)
#KalmanSmooth
  KalmanSmooth(x = ,...)
#kernapply
  kernapply(x = ,...)
#kernel
  kernel(x = ,...)
#kmeans
  kmeans(x = ,...)
#knots
  knots(x = ,...)
#kruskal.test
  kruskal.test(x = ,...)
#ks.test
  ks.test(x = ,...)
#ksmooth
  ksmooth(x = ,...)
#lag
  lag(x = ,...)
#lag.plot
  lag.plot(x = ,...)
#line
  line(x = ,...)
#lm
  lm(x = ,...)
#lm.fit
  lm.fit(x = ,...)
#lm.influence
  lm.influence(x = ,...)

#lm.wfit
  lm.wfit(x = ,...)
#loadings
  loadings(x = ,...)

#loess
  loess(x = ,...)
#loess.control
  loess.control(x = ,...)

#loess.smooth
  loess.smooth(x = ,...)
#logLik
  logLik(x = ,...)
#loglin
  loglin(x = ,...)
#lowess
  lowess(x = ,...)
#ls.diag
  ls.diag(x = ,...)
#ls.print
  ls.print(x = ,...)
#lsfit
  lsfit(x = ,...)
#mad
  mad(x = ,...)
#mahalanobis
  mahalanobis(x = ,...)
#make.link
  make.link(x = ,...)
#makeARIMA
  makeARIMA(x = ,...)
#makepredictcall
  makepredictcall(x = ,...)
#manova
  manova(x = ,...)
#mantelhaen.test
  mantelhaen.test(x = ,...)
#mauchly.test
  mauchly.test(x = ,...)
#mcnemar.test
  mcnemar.test(x = ,...)
#median
  median(x = ,...)
#median.default
  median.default(x = ,...)
#medpolish
  medpolish(x = ,...)
#model.extract
  model.extract(x = ,...)
#model.frame
  model.frame(x = ,...) 
#model.frame.default
  model.frame.default(x = ,...)
#model.matrix
  model.matrix(x = ,...)
#model.matrix.default
  model.matrix.default(x = ,...)
#model.matrix.lm
  model.matrix.lm(x = ,...)
#model.offset
  model.offset(x = ,...)
#model.response
  model.response(x = ,...)  
#model.tables
  model.tables(x = ,...)
#model.weights
  model.weights(x = ,...) 
#monthplot
  monthplot(x = ,...)
#mood.test
  mood.test(x = ,...)
#mvfft
  mvfft(x = ,...)
#na.action
  na.action(x = ,...)
#na.contiguous
  na.contiguous(x = ,...)
#na.exclude
  na.exclude(x = ,...)
#na.fail
  na.fail(x = ,...)
#na.omit
  na.omit(x = ,...)
#na.pass
  na.pass(x = ,...)
#napredict
  napredict(x = ,...)
#naprint
  naprint(x = ,...)
#naresid
  naresid(x = ,...)
#nextn
  nextn(x = ,...)
#nlm
  nlm(x = ,...)
#nlminb
  nlminb(x = ,...)
#nls
  nls(x = ,...)
#nls.control
  nls.control(x = ,...)
#NLSstAsymptotic
  NLSstAsymptotic(x = ,...)
#NLSstClosestX
  NLSstClosestX(x = ,...)
#NLSstLfAsymptote
  NLSstLfAsymptote(x = ,...)
#NLSstRtAsymptote
  NLSstRtAsymptote(x = ,...)
#nobs
  nobs(x = ,...)
#numericDeriv
  numericDeriv(x = ,...)
#offset
  offset(x = ,...)
#oneway.test
  oneway.test(x = ,...)
#optim
  optim(x = ,...)
#optimHess
  optimHess(x = ,...)
#optimise
  optimise(x = ,...)
#optimize
  optimize(x = ,...)
#order.dendrogram
  order.dendrogram(x = ,...)
#p.adjust
  p.adjust(x = ,...)
#p.adjust.methods
  p.adjust.methods(x = ,...)
#pacf
  pacf(x = ,...)
#Pair
  Pair(x = ,...)    
#pairwise.prop.test
pairwise.prop.test(x = ,n = ,p.adjust.method = )
#pairwise.t.test
pairwise.t.test(x = ,g = ,p.adjust.method = )
#pairwise.table
pairwise.table(x = ,g = ,p.adjust.method = )
#pairwise.wilcox.test
pairwise.wilcox.test(x = ,g = ,p.adjust.method = )
#pbeta
  pbeta(x = ,shape1 = ,shape2 = ,ncp = ,log.p = ) 
#pbinom
  pbinom(x = ,size = ,prob = ,lower.tail = ,log.p = )
#pbirthday
  pbirthday(x = ,n = ,classes = ,lower.tail = ,log.p = )
#pcauchy
  pcauchy(x = ,location = ,scale = ,lower.tail = ,log.p = )
#pchisq
  pchisq(x = ,df = ,ncp = ,lower.tail = ,log.p = )
#pexp
  pexp(x = ,rate = ,lower.tail = ,log.p = )
#pf
  pf(x = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )
#pgamma
  pgamma(x = ,shape = ,rate = ,lower.tail = ,log.p = )
#pgeom
  pgeom(x = ,prob = ,lower.tail = ,log.p = )
#phyper
  phyper(x = ,m = ,n = ,k = ,lower.tail = ,log.p = )
#plclust
  plclust(x = ,...)
#plnorm
  plnorm(x = ,meanlog = ,sdlog = ,lower.tail = ,log.p = )
#plogis
  plogis(x = ,location = ,scale = ,lower.tail = ,log.p = )
#plot.ecdf
  plot.ecdf(x = ,...)
#plot.spec.coherency
  plot.spec.coherency(x = ,...)
#plot.spec.phase
  plot.spec.phase(x = ,...)
#plot.stepfun
  plot.stepfun(x = ,...)
#plot.ts
  plot.ts(x = ,...)
#pnbinom
  pnbinom(x = ,size = ,prob = ,lower.tail = ,log.p = )  
#pnorm
  pnorm(x = ,mean = ,sd = ,lower.tail = ,log.p = )
#poisson
  poisson(x = ,...)
#poisson.test
  poisson.test(x = ,...)
#poly
  poly(x = ,...)
#polym
  polym(x = ,...)
#power
  power(x = ,...)
#power.anova.test
  power.anova.test(x = ,...)
#power.prop.test
  power.prop.test(x = ,...)
#power.t.test
  power.t.test(x = ,...)
#PP.test
  PP.test(x = ,...)
#ppoints
  ppoints(x = ,...)
#ppois
  ppois(x = ,lambda = ,lower.tail = ,log.p = )
#ppr
  ppr(x = ,...)
#prcomp
  prcomp(x = ,...)
#predict
  predict(x = ,...)
#predict.glm
  predict.glm(x = ,...)
#predict.lm
  predict.lm(x = ,...)  
#preplot
  preplot(x = ,...)
#princomp
  princomp(x = ,...)
#printCoefmat
  printCoefmat(x = ,...)
#profile
  profile(x = ,...)
#proj
  proj(x = ,...)
#promax
  promax(x = ,...)
#prop.test
  prop.test(x = ,...)
#prop.trend.test
  prop.trend.test(x = ,...)
#psignrank
  psignrank(x = ,n = ,lower.tail = ,log.p = ) 
#pt
  pt(x = ,df = ,ncp = ,lower.tail = ,log.p = )
#ptukey
  ptukey(x = ,q = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )
#punif
  punif(x = ,min = ,max = ,lower.tail = ,log.p = )
#pweibull
  pweibull(x = ,shape = ,scale = ,lower.tail = ,log.p = )
#pwilcox
  pwilcox(x = ,m = ,n = ,lower.tail = ,log.p = )
#qbeta
  qbeta(x = ,shape1 = ,shape2 = ,ncp = ,lower.tail = ,log.p = )
#qbinom
  qbinom(x = ,size = ,prob = ,lower.tail = ,log.p = )
#qbirthday
  qbirthday(x = ,n = ,classes = ,lower.tail = ,log.p = )  
#qcauchy
  qcauchy(x = ,location = ,scale = ,lower.tail = ,log.p = )
#qchisq
  qchisq(x = ,df = ,ncp = ,lower.tail = ,log.p = )
#qexp
  qexp(x = ,rate = ,lower.tail = ,log.p = )
#qf
  qf(x = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )  
#qgamma
  qgamma(x = ,shape = ,rate = ,lower.tail = ,log.p = )
#qgeom
  qgeom(x = ,prob = ,lower.tail = ,log.p = )
#qhyper
  qhyper(x = ,m = ,n = ,k = ,lower.tail = ,log.p = )
#qlnorm
  qlnorm(x = ,meanlog = ,sdlog = ,lower.tail = ,log.p = )
#qlogis
  qlogis(x = ,location = ,scale = ,lower.tail = ,log.p = )  
#qnbinom
  qnbinom(x = ,size = ,prob = ,lower.tail = ,log.p = )
#qnorm
  qnorm(x = ,mean = ,sd = ,lower.tail = ,log.p = )
#qpois
  qpois(x = ,lambda = ,lower.tail = ,log.p = )
#qqline
  qqline(x = ,...)
#qqnorm
  qqnorm(x = ,...)
#qqplot
  qqplot(x = ,...)
#qsignrank
  qsignrank(x = ,n = ,lower.tail = ,log.p = )
#qt
  qt(x = ,df = ,ncp = ,lower.tail = ,log.p = )  
#qtukey
  qtukey(x = ,q = ,df1 = ,df2 = ,ncp = ,lower.tail = ,log.p = )
#quade.test
  quade.test(x = ,...)
#quantile
  quantile(x = ,...)
#quasi
  quasi(x = ,...)
#quasibinomial
  quasibinomial(x = ,...) 
#quasipoisson
  quasipoisson(x = ,...)
#qunif
  qunif(x = ,min = ,max = ,lower.tail = ,log.p = )
#qweibull
  qweibull(x = ,shape = ,scale = ,lower.tail = ,log.p = )
#qwilcox
  qwilcox(x = ,m = ,n = ,lower.tail = ,log.p = )
#r2dtable
  r2dtable(x = ,...)
#rbeta
  rbeta(x = ,shape1 = ,shape2 = ,ncp = )
#rbinom
  rbinom(x = ,size = ,prob = )
#rcauchy
  rcauchy(x = ,location = ,scale = )
#rchisq
  rchisq(x = ,df = ,ncp = )
#read.ftable
  read.ftable(x = ,...)
#rect.hclust
  rect.hclust(x = ,...)
#reformulate
  reformulate(x = ,...)
#relevel
  relevel(x = ,...)
#reorder
  reorder(x = ,...)
#replications
  replications(x = ,...)
#reshape
  reshape(x = ,...)
#resid
  resid(x = ,...)
#residuals
  residuals(x = ,...)
#residuals.glm
  residuals.glm(x = ,...)
#residuals.lm
  residuals.lm(x = ,...)
#rexp
  rexp(x = ,rate = )
#rf
  rf(x = ,df1 = ,df2 = ,ncp = ) 
#rgamma
  rgamma(x = ,shape = ,rate = )
#rgeom
  rgeom(x = ,prob = )
#rhyper
  rhyper(x = ,m = ,n = ,k = )
#rlnorm
  rlnorm(x = ,meanlog = ,sdlog = )
#rlogis
rlogis(n = ,location = ,scale = )
#rmultinom
  rmultinom(x = ,size = ,prob = )
#rnbinom
  rnbinom(x = ,size = ,prob = ) 
#rnorm
  rnorm(x = ,mean = ,sd = )
#rpois
  rpois(x = ,lambda = )
#rsignrank
  rsignrank(x = ,n = )
#rstandard
  rstandard(x = ,...) 
#rstudent
  rstudent(x = ,...)
#rt
  rt(x = ,df = ,ncp = ) 
#runif
  runif(x = ,min = ,max = )
#runmed
  runmed(x = ,...)
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
#SSfpl
  SSfpl(x = ,...)
#SSgompertz
  SSgompertz(x = ,...)
#SSlogis
  SSlogis(x = ,...)
#SSmicmen
  SSmicmen(x = ,...)
#SSweibull
  SSweibull(x = ,...)
#start
  start(x = ,...) 
#stat.anova
  stat.anova(x = ,...)
#step
  step(x = ,...)
#stepfun
  stepfun(x = ,...)
#stl
  stl(x = ,...)
#StructTS
  StructTS(x = ,...)
#summary.aov
  summary.aov(x = ,...)
#summary.glm
  summary.glm(x = ,...)
#summary.lm
summary.lm(object = ,correlation = ,symbolic.cor = )
#summary.manova
summary.manova(object = ,test = ,intercept = ,tol = )
#summary.stepfun
summary.stepfun(x = ,...)
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
