library(foreign)
library(nFactors)
library(GPArotation)
library(lavaan)
library(psych)
library(semPlot)
library(kableExtra)
library(ggplot2)
library(semPlot)
data<-read.spss(
  file = "ПредобработаннаяСоцОжиданияВыборка.sav",
  use.value.labels = F,
  use.missings = T,
  to.data.frame = T)



procr$prlvl <- (procr$ProcrLevel)
procr$implslvl <- (procr$ImpulsLevel)
procr$prntex <- (procr$ParentsExpectactions)
procr$ttlex <- (procr$TotalExpectactions)


procrik <- sem('prlvl ~ implslvl + BudgetStructr + Age + prntex;
               implslvl ~ ttlex',
               
               data=procr
)


semPaths(procrik, whatLabels="est", layout = "spring" , label.cex = 1.5, 
         edge.label.cex = 1, edge.color = "darkgreen", style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "ellipse", nCharNodes=5)
summary(procrik, fit.measures=TRUE)


modindices(procrik, sort = T)

procrik1 <- sem('prlvl ~ implslvl + BudgetStructr + Age + prntex;
               implslvl ~ ttlex;
                prlvl ~~ implslvl',
                data=procr
)

summary(procrik1, fit.measures=TRUE)
modindices(procrik1, sort = T)

semPaths(procrik1, whatLabels="est", layout = "tree" , label.cex = 1.5, 
         edge.label.cex = 1, edge.color = "darkgreen", style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "ellipse", nCharNodes=5)


procrik2 <- sem('prlvl ~ implslvl + BudgetStructr + Age + prntex;
               implslvl ~ ttlex;
                prlvl ~~ implslvl;
                implslvl ~ BudgetStructr',
                data=procr
)

summary(procrik2, fit.measures=TRUE)
modindices(procrik2, sort = T)

semPaths(procrik2, whatLabels="est", layout = "tree" , label.cex = 1.5, 
         edge.label.cex = 1, edge.color = "darkgreen", style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "ellipse", nCharNodes=5)
