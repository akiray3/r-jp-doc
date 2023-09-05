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
chol(x = ,pivot = ,LINPACK = )
#chol.default
chol.default(x = ,pivot = ,LINPACK = )

#chol2inv
chol2inv(x = ,size = ,LINPACK = )
#choose
choose(n = ,k = )
#class
class(x = )
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
#La_version
#La.svd
#labels
#labels.default
#lapply
#lazyLoad
#lazyLoadDBexec
#lazyLoadDBfetch
#lbeta
#lchoose
#length
#length.POSIXlt
#length<-
#length<-.Date
#length<-.difftime
#length<-.factor
#length<-.POSIXct
#length<-.POSIXlt
#lengths
#letters
#LETTERS
#levels
#levels.default
#levels<-
#levels<-.factor
#lfactorial
#lgamma
#libcurlVersion
#library
#library.dynam
#library.dynam.unload
#licence
#license
#list
#list.dirs
#list.files
#list2DF
#list2env
#load
#loadedNamespaces
#loadingNamespaceInfo
#loadNamespace
#local
#lockBinding
#lockEnvironment
#log
#log10
#log1p
#log2
#logb
#logical
#lower.tri
#ls
#make.names
#make.unique
#makeActiveBinding
#Map
#mapply
#margin.table
#marginSums
#mat.or.vec
#match
#match.arg
#match.call
#match.fun
#Math.data.frame
#Math.Date
#Math.difftime
#Math.factor
#Math.POSIXt
#matrix
#max
#max.col
#mean
#mean.Date
#mean.default
#mean.difftime
#mean.POSIXct
#mean.POSIXlt
#mem.maxNSize
#mem.maxVSize
#memCompress
#memDecompress
#memory.profile
#merge
#merge.data.frame
#merge.default
#message
#mget
#min
#missing
#Mod
#mode
#mode<-
#month.abb
#month.name
#months
#months.Date
#months.POSIXt
#mostattributes<-
#names
#names.POSIXlt
#names<-
#names<-.POSIXlt
#namespaceExport
#namespaceImport
#namespaceImportClasses
#namespaceImportFrom
#namespaceImportMethods
#nargs
#nchar
#ncol
#NCOL
#Negate
#new.env
#next
#NextMethod
#ngettext
#nlevels
#noquote
#norm
#normalizePath
#nrow
#NROW
#nullfile
#numeric
#numeric_version
#numToBits
#numToInts
#nzchar
#objects
#oldClass
#oldClass<-
#OlsonNames
#on.exit
#open
#open.connection
#open.srcfile
#open.srcfilealias
#open.srcfilecopy
#Ops.data.frame
#Ops.Date
#Ops.difftime
#Ops.factor
#Ops.numeric_version
#Ops.ordered
#Ops.POSIXt
#options
#order
#ordered
#outer
#package_version
#packageEvent
#packageHasNamespace
#packageNotFoundError
#packageStartupMessage
#packBits
#pairlist
#parent.env
#parent.env<-
#parent.frame
#parse
#parseNamespaceFile
#paste
#paste0
#path.expand
#path.package
#pcre_config
#pi
#pipe
#plot
#pmatch
#pmax
#pmax.int
#pmin
#pmin.int
#polyroot
#pos.to.env
#Position
#pretty
#pretty.default
#prettyNum
#print
#print.AsIs
#print.by
#print.condition
#print.connection
#print.data.frame
#print.Date
#print.default
#print.difftime
#print.Dlist
#print.DLLInfo
#print.DLLInfoList
#print.DLLRegisteredRoutines
#print.eigen
#print.factor
#print.function
#print.hexmode
#print.libraryIQR
#print.listof
#print.NativeRoutineList
#print.noquote
#print.numeric_version
#print.octmode
#print.packageInfo
#print.POSIXct
#print.POSIXlt
#print.proc_time
#print.restart
#print.rle
#print.simple.list
#print.srcfile
#print.srcref
#print.summary.table
#print.summary.warnings
#print.summaryDefault
#print.table
#print.warnings
#prmatrix
#proc.time
#prod
#prop.table
#proportions
#provideDimnames
#psigamma
#pushBack
#pushBackLength
#q
#qr
#qr.coef
#qr.default
#qr.fitted
#qr.Q
#qr.qty
#qr.qy
#qr.R
#qr.resid
#qr.solve
#qr.X
#quarters
#quarters.Date
#quarters.POSIXt
#quit
#quote
#R_system_version
#R.home
#R.version
#R.Version
#R.version.string
#range
#range.default
#rank
#rapply
#raw
#rawConnection
#rawConnectionValue
#rawShift
#rawToBits
#rawToChar
#rbind
#rbind.data.frame
#rcond
#Re
#read.dcf
#readBin
#readChar
#readline
#readLines
#readRDS
#readRenviron
#Recall
#Reduce
#reg.finalizer
#regexec
#regexpr
#registerS3method
#registerS3methods
#regmatches
#regmatches<-
#remove
#removeTaskCallback
#rep
#rep_len
#rep.Date
#rep.difftime
#rep.factor
#rep.int
#rep.numeric_version
#rep.POSIXct
#rep.POSIXlt
#repeat
#replace
#replicate
#require
#requireNamespace
#restartDescription
#restartFormals
#retracemem
#return
#returnValue
#rev
#rev.default
#rle
#rm
#RNGkind
#RNGversion
#round
#round.Date
#round.POSIXt
#row
#row.names
#row.names.data.frame
#row.names.default
#row.names<-
#row.names<-.data.frame
#row.names<-.default
#rowMeans
#rownames
#rownames<-
#rowsum
#rowsum.data.frame
#rowsum.default
#rowSums
#sample
#sample.int
#sapply
#save
#save.image
#saveRDS
#scale
#scale.default
#scan
#search
#searchpaths
#seek
#seek.connection
#seq
#seq_along
#seq_len
#seq.Date
#seq.default
#seq.int
#seq.POSIXt
#sequence
#sequence.default
#serialize
#serverSocket
#set.seed
#setdiff
#setequal
#setHook
#setNamespaceInfo
#setSessionTimeLimit
#setTimeLimit
#setwd
#showConnections
#shQuote
#sign
#signalCondition
#signif
#simpleCondition
#simpleError
#simpleMessage
#simpleWarning
#simplify2array
#sin
#single
#sinh
#sink
#sink.number
#sinpi
#slice.index
#socketAccept
#socketConnection
#socketSelect
#socketTimeout
#solve
#solve.default
#solve.qr
#sort
#sort.default
#sort.int
#sort.list
#sort.POSIXlt
#source
#split
#split.data.frame
#split.Date
#split.default
#split.POSIXct
#split<-
#split<-.data.frame
#split<-.default
#sprintf
#sqrt
#sQuote
#srcfile
#srcfilealias
#srcfilecopy
#srcref
#standardGeneric
#startsWith
#stderr
#stdin
#stdout
#stop
#stopifnot
#storage.mode
#storage.mode<-
#str2expression
#str2lang
#strftime
#strptime
#strrep
#strsplit
#strtoi
#strtrim
#structure
#strwrap
#sub
#subset
#subset.data.frame
#subset.default
#subset.matrix
#substitute
#substr
#substr<-
#substring
#substring<-
#sum
#summary
#summary.connection
#summary.data.frame
#Summary.data.frame
#summary.Date
#Summary.Date
#summary.default
#Summary.difftime
#summary.factor
#Summary.factor
#summary.matrix
#Summary.numeric_version
#Summary.ordered
#summary.POSIXct
#Summary.POSIXct
#summary.POSIXlt
#Summary.POSIXlt
#summary.proc_time
#summary.srcfile
#summary.srcref
#summary.table
#summary.warnings
#suppressMessages
#suppressPackageStartupMessages
#suppressWarnings
#suspendInterrupts
#svd
#sweep
#switch
#sys.call
#sys.calls
#Sys.chmod
#Sys.Date
#sys.frame
#sys.frames
#sys.function
#Sys.getenv
#Sys.getlocale
#Sys.getpid
#Sys.glob
#Sys.info
#sys.load.image
#Sys.localeconv
#sys.nframe
#sys.on.exit
#sys.parent
#sys.parents
#Sys.readlink
#sys.save.image
#Sys.setenv
#Sys.setFileTime
#Sys.setlocale
#Sys.sleep
#sys.source
#sys.status
#Sys.time
#Sys.timezone
#Sys.umask
#Sys.unsetenv
#Sys.which
#system
#system.file
#system.time
#system2
#t
#T
#t.data.frame
#t.default
#table
#tabulate
#tan
#tanh
#tanpi
#tapply
#taskCallbackManager
#tcrossprod
#tempdir
#tempfile
#textConnection
#textConnectionValue
#tolower
#topenv
#toString
#toString.default
#toupper
#trace
#traceback
#tracemem
#tracingState
#transform
#transform.data.frame
#transform.default
#trigamma
#trimws
#trunc
#trunc.Date
#trunc.POSIXt
#truncate
#truncate.connection
#try
#tryCatch
#tryInvokeRestart
#typeof
#unclass
#undebug
#union
#unique
#unique.array
#unique.data.frame
#unique.default
#unique.matrix
#unique.numeric_version
#unique.POSIXlt
#unique.warnings
#units
#units.difftime
#units<-
#units<-.difftime
#unix.time
#unlink
#unlist
#unloadNamespace
#unlockBinding
#unname
#unserialize
#unsplit
#untrace
#untracemem
#unz
#upper.tri
#url
#UseMethod
#utf8ToInt
#validEnc
#validUTF8
#vapply
#vector
#Vectorize
#version
#warning
#warningCondition
#warnings
#weekdays
#weekdays.Date
#weekdays.POSIXt
#which
#which.max
#which.min
#while
#with
#with.default
#withAutoprint
#withCallingHandlers
#within
#within.data.frame
#within.list
#withRestarts
#withVisible
#write
#write.dcf
#writeBin
#writeChar
#writeLines
#xor
#xpdrows.data.frame
#xtfrm
#xtfrm.AsIs
#xtfrm.data.frame
#xtfrm.Date
#xtfrm.default
#xtfrm.difftime
#xtfrm.factor
#xtfrm.numeric_version
#xtfrm.POSIXct
#xtfrm.POSIXlt
#xzfile
#zapsmall
#abline
#arrows
#assocplot
#axis
#Axis
#axis.Date
#axis.POSIXct
#axTicks
#barplot
#barplot.default
#box
#boxplot
#boxplot.default
#boxplot.matrix
#bxp
#cdplot
#clip
#close.screen
#co.intervals
#contour
#contour.default
#coplot
#curve
#dotchart
#erase.screen
#filled.contour
#fourfoldplot
#frame
#grconvertX
#grconvertY
#grid
#hist
#hist.default
#identify
#image
#image.default
#layout
#layout.show
#lcm
#legend
#lines
#lines.default
#locator
#matlines
#matplot
#matpoints
#mosaicplot
#mtext
#pairs
#pairs.default
#panel.smooth
#par
#persp
#pie
#plot
#plot.default
#plot.design
#plot.function
#plot.new
#plot.window
#plot.xy
#points
#points.default
#polygon
#polypath
#rasterImage
#rect
#rug
#screen
#segments
#smoothScatter
#spineplot
#split.screen
#stars
#stem
#strheight
#stripchart
#strwidth
#sunflowerplot
#symbols
#text
#text.default
#title
#xinch
#xspline
#xyinch
#yinch
#%+%
#acs
#alpha
#alpha.ci
#anova.psych
#AUC
#autoR
#bassAckward
#bassAckward.diagram
#Bechtoldt
#Bechtoldt.1
#Bechtoldt.2
#bestItems
#bestScales
#bfi
#bfi.keys
#bi.bars
#bifactor
#bigCor
#biplot.psych
#biquartimin
#biserial
#block.random
#bock.table
#cattell
#char2numeric
#Chen
#chi2r
#circ.sim
#circ.sim.plot
#circ.simulation
#circ.tests
#circadian.cor
#circadian.F
#circadian.linear.cor
#circadian.mean
#circadian.phase
#circadian.reliability
#circadian.sd
#circadian.stats
#circular.cor
#circular.mean
#cluster.cor
#cluster.fit
#cluster.loadings
#cluster.plot
#cluster2keys
#cohen.d
#cohen.d.by
#cohen.d.ci
#cohen.kappa
#cohen.profile
#comorbidity
#con2cat
#congeneric.sim
#congruence
#cor.ci
#cor.plot
#cor.plot.upperLowerCi
#cor.smooth
#cor.smoother
#cor.wt
#cor2
#cor2cov
#cor2dist
#corCi
#corFiml
#corPlot
#corPlotUpperLowerCi
#corr.p
#corr.test
#correct.cor
#cortest
#cortest.bartlett
#cortest.jennrich
#cortest.mat
#cortest.normal
#cosinor
#cosinor.period
#cosinor.plot
#count.pairwise
#crossValidation
#cs
#cta
#cta.15
#d.ci
#d.robust
#d2CL
#d2OVL
#d2OVL2
#d2r
#d2t
#d2U3
#densityBy
#describe
#describe.by
#describeBy
#describeData
#describeFast
#dia.arrow
#dia.cone
#dia.curve
#dia.curved.arrow
#dia.ellipse
#dia.ellipse1
#dia.rect
#dia.self
#dia.shape
#dia.triangle
#diagram
#directSl
#distance
#draw.cor
#draw.tetra
#dummy.code
#Dwyer
#eigen.loadings
#ellipses
#equamax
#error.bars
#error.bars.by
#error.bars.tab
#error.crosses
#error.dots
#errorCircles
#esem
#esem.diagram
#extension.diagram
#fa
#fa.congruence
#fa.diagram
#fa.extend
#fa.extension
#fa.graph
#fa.lookup
#fa.multi
#fa.multi.diagram
#fa.organize
#fa.parallel
#fa.parallel.poly
#fa.plot
#fa.poly
#fa.pooled
#fa.random
#fa.rgraph
#fa.sapa
#fa.sort
#fa.stats
#fa2irt
#faBy
#fac
#faCor
#factor.congruence
#factor.fit
#factor.minres
#factor.model
#factor.pa
#factor.plot
#factor.residuals
#factor.rotate
#factor.scores
#factor.stats
#factor.wls
#factor2cluster
#faRotate
#faRotations
#fisherz
#fisherz2r
#fparse
#fromTo
#g2r
#Garcia
#geometric.mean
#glb
#glb.algebraic
#glb.fa
#Gleser
#Gorsuch
#guttman
#Harman.5
#Harman.8
#Harman.Burt
#Harman.Holzinger
#Harman.political
#harmonic.mean
#headtail
#headTail
#het.diagram
#histBy
#Holzinger
#Holzinger.9
#ICC
#iclust
#ICLUST
#ICLUST.cluster
#iclust.diagram
#ICLUST.graph
#ICLUST.rgraph
#iclust.sort
#ICLUST.sort
#interbattery
#interp.boxplot
#interp.median
#interp.q
#interp.qplot.by
#interp.quantiles
#interp.quart
#interp.quartiles
#interp.values
#irt.0p
#irt.1p
#irt.2p
#irt.discrim
#irt.fa
#irt.item.diff.rasch
#irt.person.rasch
#irt.responses
#irt.se
#irt.select
#irt.stats.like
#irt.tau
#isCorrelation
#isCovariance
#item.dichot
#item.lookup
#item.sim
#item.validity
#kaiser
#keys.lookup
#keys2list
#keysort
#KMO
#kurtosi
#lavaan.diagram
#levels2numeric
#logistic
#logistic.grm
#logit
#lookup
#lookupFromKeys
#lookupItems
#lowerCor
#lowerMat
#lowerUpper
#lsat6
#lsat7
#m2d
#m2t
#make.congeneric
#make.hierarchical
#make.irt.stats
#make.keys
#makePositiveKeys
#manhattan
#mardia
#mat.regress
#mat.sort
#matPlot
#matReg
#matSort
#mediate
#mediate.diagram
#minkowski
#mixed.cor
#mixedCor
#mlArrange
#mlPlot
#mlr
#moderate.diagram
#mssd
#multi.arrow
#multi.curved.arrow
#multi.hist
#multi.rect
#multi.self
#multilevel.reliability
#nchar2numeric
#nfactors
#omega
#omega.diagram
#omega.graph
#omegaDirect
#omegaFromSem
#omegah
#omegaSem
#outlier
#p.rep
#p.rep.f
#p.rep.r
#p.rep.t
#paired.r
#pairs.panels
#pairwiseCount
#pairwiseCountBig
#pairwiseDescribe
#pairwiseImpute
#pairwisePlot
#pairwiseReport
#pairwiseSample
#pairwiseZero
#parcels
#partial.r
#pca
#phi
#phi.demo
#phi.list
#phi2poly
#phi2poly.matrix
#phi2tetra
#Pinv
#plot.irt
#plot.poly
#plot.poly.parallel
#plot.psych
#plot.reliability
#plot.residuals
#polar
#poly.mat
#polychoric
#polydi
#polyserial
#predict.psych
#predicted.validity
#principal
#print.psych
#Procrustes
#progressBar
#Promax
#psych
#psych.misc
#quickView
#r.con
#r.test
#r2c
#r2chi
#r2d
#r2t
#radar
#rangeCorrection
#reflect
#Reise
#reliability
#rescale
#resid.psych
#residuals.psych
#response.frequencies
#responseFrequency
#reverse.code
#rmssd
#sat.act
#scaling.fits
#scatter.hist
#scatterHist
#schmid
#Schmid
#schmid.leiman
#score.alpha
#score.irt
#score.irt.2
#score.irt.poly
#score.items
#score.multiple.choice
#scoreBy
#scoreFast
#scoreIrt
#scoreIrt.1pl
#scoreIrt.2pl
#scoreItems
#scoreOverlap
#scoreVeryFast
#scoreWtd
#scree
#scrub
#SD
#selectFromKeys
#sem.diagram
#sem.graph
#set.cor
#setCor
#setCor.diagram
#setCorLookup
#shannon
#sim
#sim.anova
#sim.bonds
#sim.circ
#sim.congeneric
#sim.correlation
#sim.dichot
#sim.general
#sim.hierarchical
#sim.irt
#sim.item
#sim.minor
#sim.multi
#sim.multilevel
#sim.npl
#sim.npn
#sim.omega
#sim.parallel
#sim.poly
#sim.poly.ideal
#sim.poly.ideal.npl
#sim.poly.ideal.npn
#sim.poly.mat
#sim.poly.npl
#sim.poly.npn
#sim.rasch
#sim.simplex
#sim.spherical
#sim.structural
#sim.structure
#sim.VSS
#simCor
#simulation.circ
#skew
#smc
#spider
#splitHalf
#statsBy
#statsBy.boot
#statsBy.boot.summary
#structure.diagram
#structure.graph
#structure.list
#structure.sem
#summary.psych
#super.matrix
#superCor
#superMatrix
#t2d
#t2r
#table2df
#table2matrix
#tableF
#Tal_Or
#Tal.Or
#target.rot
#TargetQ
#TargetT
#tenberge
#test.all
#test.irt
#test.psych
#testRetest
#tetrachoric
#thurstone
#Thurstone
#Thurstone.33
#Thurstone.9
#topBottom
#tr
#Tucker
#unidim
#varimin
#vgQ.bimin
#vgQ.targetQ
#vgQ.varimin
#violin
#violinBy
#vss
#VSS
#VSS.parallel
#VSS.plot
#VSS.scree
#VSS.sim
#VSS.simulate
#West
#winsor
#winsor.mean
#winsor.means
#winsor.sd
#winsor.var
#withinBetween
#wkappa
#Yule
#Yule.inv
#Yule2phi
#Yule2phi.matrix
#Yule2poly
#Yule2poly.matrix
#Yule2tetra
#YuleBonett
#YuleCor
#acf
#acf2AR
#add.scope
#add1
#addmargins
#aggregate
#aggregate.data.frame
#aggregate.ts
#AIC
#alias
#anova
#ansari.test
#aov
#approx
#approxfun
#ar
#ar.burg
#ar.mle
#ar.ols
#ar.yw
#arima
#arima.sim
#arima0
#arima0.diag
#ARMAacf
#ARMAtoMA
#as.dendrogram
#as.dist
#as.formula
#as.hclust
#as.stepfun
#as.ts
#asOneSidedFormula
#ave
#bandwidth.kernel
#bartlett.test
#BIC
#binom.test
#binomial
#biplot
#Box.test
#bw.bcv
#bw.nrd
#bw.nrd0
#bw.SJ
#bw.ucv
#C
#cancor
#case.names
#ccf
#chisq.test
#cmdscale
#coef
#coefficients
#complete.cases
#confint
#confint.default
#confint.lm
#constrOptim
#contr.helmert
#contr.poly
#contr.SAS
#contr.sum
#contr.treatment
#contrasts
#contrasts<-
#convolve
#cooks.distance
#cophenetic
#cor
#cor.test
#cov
#cov.wt
#cov2cor
#covratio
#cpgram
#cutree
#cycle
#D
#dbeta
#dbinom
#dcauchy
#dchisq
#decompose
#delete.response
#deltat
#dendrapply
#density
#density.default
#deriv
#deriv3
#deviance
#dexp
#df
#df.kernel
#df.residual
#DF2formula
#dfbeta
#dfbetas
#dffits
#dgamma
#dgeom
#dhyper
#diffinv
#dist
#dlnorm
#dlogis
#dmultinom
#dnbinom
#dnorm
#dpois
#drop.scope
#drop.terms
#drop1
#dsignrank
#dt
#dummy.coef
#dummy.coef.lm
#dunif
#dweibull
#dwilcox
#ecdf
#eff.aovlist
#effects
#embed
#end
#estVar
#expand.model.frame
#extractAIC
#factanal
#factor.scope
#family
#fft
#filter
#fisher.test
#fitted
#fitted.values
#fivenum
#fligner.test
#formula
#frequency
#friedman.test
#ftable
#Gamma
#gaussian
#get_all_vars
#getCall
#getInitial
#glm
#glm.control
#glm.fit
#hasTsp
#hat
#hatvalues
#hclust
#heatmap
#HoltWinters
#influence
#influence.measures
#integrate
#interaction.plot
#inverse.gaussian
#IQR
#is.empty.model
#is.leaf
#is.mts
#is.stepfun
#is.ts
#is.tskernel
#isoreg
#KalmanForecast
#KalmanLike
#KalmanRun
#KalmanSmooth
#kernapply
#kernel
#kmeans
#knots
#kruskal.test
#ks.test
#ksmooth
#lag
#lag.plot
#line
#lm
#lm.fit
#lm.influence
#lm.wfit
#loadings
#loess
#loess.control
#loess.smooth
#logLik
#loglin
#lowess
#ls.diag
#ls.print
#lsfit
#mad
#mahalanobis
#make.link
#makeARIMA
#makepredictcall
#manova
#mantelhaen.test
#mauchly.test
#mcnemar.test
#median
#median.default
#medpolish
#model.extract
#model.frame
#model.frame.default
#model.matrix
#model.matrix.default
#model.matrix.lm
#model.offset
#model.response
#model.tables
#model.weights
#monthplot
#mood.test
#mvfft
#na.action
#na.contiguous
#na.exclude
#na.fail
#na.omit
#na.pass
#napredict
#naprint
#naresid
#nextn
#nlm
#nlminb
#nls
#nls.control
#NLSstAsymptotic
#NLSstClosestX
#NLSstLfAsymptote
#NLSstRtAsymptote
#nobs
#numericDeriv
#offset
#oneway.test
#optim
#optimHess
#optimise
#optimize
#order.dendrogram
#p.adjust
#p.adjust.methods
#pacf
#Pair
#pairwise.prop.test
#pairwise.t.test
#pairwise.table
#pairwise.wilcox.test
#pbeta
#pbinom
#pbirthday
#pcauchy
#pchisq
#pexp
#pf
#pgamma
#pgeom
#phyper
#plclust
#plnorm
#plogis
#plot.ecdf
#plot.spec.coherency
#plot.spec.phase
#plot.stepfun
#plot.ts
#pnbinom
#pnorm
#poisson
#poisson.test
#poly
#polym
#power
#power.anova.test
#power.prop.test
#power.t.test
#PP.test
#ppoints
#ppois
#ppr
#prcomp
#predict
#predict.glm
#predict.lm
#preplot
#princomp
#printCoefmat
#profile
#proj
#promax
#prop.test
#prop.trend.test
#psignrank
#pt
#ptukey
#punif
#pweibull
#pwilcox
#qbeta
#qbinom
#qbirthday
#qcauchy
#qchisq
#qexp
#qf
#qgamma
#qgeom
#qhyper
#qlnorm
#qlogis
#qnbinom
#qnorm
#qpois
#qqline
#qqnorm
#qqplot
#qsignrank
#qt
#qtukey
#quade.test
#quantile
#quasi
#quasibinomial
#quasipoisson
#qunif
#qweibull
#qwilcox
#r2dtable
#rbeta
#rbinom
#rcauchy
#rchisq
#read.ftable
#rect.hclust
#reformulate
#relevel
#reorder
#replications
#reshape
#resid
#residuals
#residuals.glm
#residuals.lm
#rexp
#rf
#rgamma
#rgeom
#rhyper
#rlnorm
#rlogis
#rmultinom
#rnbinom
#rnorm
#rpois
#rsignrank
#rstandard
#rstudent
#rt
#runif
#runmed
#rweibull
#rwilcox
#rWishart
#scatter.smooth
#screeplot
#sd
#se.contrast
#selfStart
#setNames
#shapiro.test
#sigma
#simulate
#smooth
#smooth.spline
#smoothEnds
#sortedXyData
#spec.ar
#spec.pgram
#spec.taper
#spectrum
#spline
#splinefun
#splinefunH
#SSasymp
#SSasympOff
#SSasympOrig
#SSbiexp
#SSD
#SSfol
#SSfpl
#SSgompertz
#SSlogis
#SSmicmen
#SSweibull
#start
#stat.anova
#step
#stepfun
#stl
#StructTS
#summary.aov
#summary.glm
#summary.lm
#summary.manova
#summary.stepfun
#supsmu
#symnum
#t.test
#termplot
#terms
#terms.formula
#time
#toeplitz
#ts
#ts.intersect
#ts.plot
#ts.union
#tsdiag
#tsp
#tsp<-
#tsSmooth
#TukeyHSD
#uniroot
#update
#update.default
#update.formula
#var
#var.test
#variable.names
#varimax
#vcov
#weighted.mean
#weighted.residuals
#weights
#wilcox.test
#window
#window<-
#write.ftable
#xtabs
