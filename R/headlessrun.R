#headlessRunEx
library(RNetLogo)

# path to NetLogo
path.to.NetLogo <- "~/NetLogo/app"

web1 <- "web"
NLStart(path.to.NetLogo, gui=FALSE, nl.obj=web1)

# start another NetLogo instance into variable with the name "my.obj2" use: 
web2 <- "web2"
NLStart(path.to.NetLogo, gui=FALSE, nl.obj=web2)

# relative path to a sample model
model.path <- "/models/sim.nlogo"


NLLoadModel(paste(path.to.NetLogo,model.path,sep=""),nl.obj=web1)

NLLoadModel(paste(path.to.NetLogo,model.path,sep=""),nl.obj=web2)

NLCommand("set web \"myobj1 test\"",nl.obj=web1)

NLCommand("set web2 \"myobj2 test\"",nl.obj=web2)

name.var1 <- NLReport("mov", nl.obj=web1)
name.var2 <- NLReport("mov2", nl.obj=web2)
print(name.var1)
print(name.var2)

NLQuit(my.obj1)
NLQuit(my.obj2)