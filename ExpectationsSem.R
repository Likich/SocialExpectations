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
  file = "SocialExpData.sav",
  use.value.labels = F,
  use.missings = T,
  to.data.frame = T)

data




mymodel <- sem('
               SoxEXPstand ~ RodPoddStand + SobstvPoddStand1 + standwork;
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               rabotapodusha ~ SobstvPoddStand1 + standwork;
               vinuzhden ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               tyazhelopomosh ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               tyazheloprinimresh ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               schitaus ~ SobstvPoddStand1;
               pitayusborot ~ SobstvPoddStand1 + SoxEXPstand;
               stidno ~ SobstvPoddStand1 + SoxEXPstand;
               vina ~ RodPoddStand + SobstvPoddStand1 +  SoxEXPstand;
               risk ~ RodPoddStand;
               nevsyozavisit ~ RodPoddStand +  SoxEXPstand + SobstvPoddStand1 + standwork

              ',
               
               data=data)

summary(mymodel, fit.measures=TRUE)

semPaths(mymodel, whatLabels="std", layout = "tree" , label.cex = 1.8, 
         edge.label.cex = 1.5, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple", edge.width=1, node.width = 1.4, curve
         = 1.2)

mymodel1 <- sem('
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ SobstvPoddStand1 + SoxEXPstand;
               rabotapodusha ~ SobstvPoddStand1 + standwork;
               vinuzhden ~  SobstvPoddStand1 + SoxEXPstand;
               SoxEXPstand =~ RodPoddStand + SobstvPoddStand1 + standwork;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               tyazhelopomosh ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               tyazheloprinimresh ~ SobstvPoddStand1 + SoxEXPstand;

             
               schitaus ~ SobstvPoddStand1;
               pitayusborot ~ SobstvPoddStand1 + SoxEXPstand;
               stidno ~ SobstvPoddStand1 + SoxEXPstand;
               vina ~ SobstvPoddStand1 + SoxEXPstand;
               risk ~ RodPoddStand;
               nevsyozavisit ~ standwork;
               
              
               
               vsegdadobiv  ~     tyazhelopomosh;
               polagatsya  ~      skrivayutseli;
              
               SobstvPoddStand1 ~~          standwork;
               '          ,
               
               data=data)



semPaths(mymodel1, whatLabels="std", layout = "tree" , label.cex = 1.5, 
         edge.label.cex = 1, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple")

summary(mymodel1, fit.measures=TRUE)


modindices(mymodel1, sort = T)

mymodel12 <- sem('SoxEXPstand =~ RodPoddStand + SobstvPoddStand1 + standwork;
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ RodPoddStand + SobstvPoddStand1;
               rabotapodusha ~ SobstvPoddStand1 + standwork;
               vinuzhden ~ RodPoddStand + SobstvPoddStand1;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               tyazhelopomosh ~ RodPoddStand + SobstvPoddStand1;
               tyazheloprinimresh ~ RodPoddStand + SobstvPoddStand1;
               schitaus ~ SobstvPoddStand1;
               pitayusborot ~ SobstvPoddStand1;
               stidno ~ SobstvPoddStand1;
               vina ~ RodPoddStand + SobstvPoddStand1;
               risk ~ RodPoddStand;
               nevsyozavisit ~ RodPoddStand + SobstvPoddStand1 + standwork;
               standwork  ~  vinuzhden + rabotapodusha'
                
                ,
                
                data=data)


semPaths(mymodel12, whatLabels="est", layout = "tree2" , label.cex = 1.5, 
         edge.label.cex = 1, edge.color = "darkgreen", style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "ellipse", nCharNodes=5)
summary(mymodel12, fit.measures=TRUE)


modindices(mymodel1, sort = T)


mymodel2 <- sem('
               SoxEXPstand =~ RodPoddStand + SobstvPoddStand1 + standwork;
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ standwork;
               rabotapodusha ~ standwork;
               vinuzhden ~ standwork;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               schitaus ~ SobstvPoddStand1;
               pitayusborot ~ standwork;
               stidno ~ standwork;
               vina ~ standwork;
               risk ~ RodPoddStand;
               nevsyozavisit ~ standwork;
               tyazhelopomosh ~ standwork;
               tyazheloprinimresh ~ standwork;
               tyazhelopomosh  ~       RodPoddStand;
               vsegdadobiv  ~~    tyazhelopomosh;
               standwork  ~~      rabotapodusha
               
        
              ',
               
               data=data)

summary(mymodel2, fit.measures=TRUE)
modindices(mymodel2, sort = T)

semPaths(mymodel2, whatLabels="std", layout = "tree" , label.cex = 1.8, 
         edge.label.cex = 1.5, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple", edge.width=1, node.width = 1.4, curve
         = 1.2)



mymodel3 <- sem('
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ SobstvPoddStand1;
               rabotapodusha ~ SobstvPoddStand1 + standwork;
               vinuzhden ~  SobstvPoddStand1;
               SoxEXPstand =~ RodPoddStand + SobstvPoddStand1 + standwork;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               tyazhelopomosh ~ RodPoddStand + SobstvPoddStand1;
               tyazheloprinimresh ~ SobstvPoddStand1;

             
               schitaus ~ SobstvPoddStand1;
               pitayusborot ~ SobstvPoddStand1;
               stidno ~ SobstvPoddStand1;
               vina ~ SobstvPoddStand1;
               risk ~ RodPoddStand;
               nevsyozavisit ~ standwork;
              
              
               
               ' #?? ???????? ? ???? ??????? ?? ???????? ????
                
                
                
                ,
                
                data=data
)



semPaths(mymodel3, whatLabels="std", layout = "tree" , label.cex = 1.5, 
         edge.label.cex = 1, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple")

summary(mymodel3, fit.measures=TRUE)


modindices(mymodel3, sort = T)



#????? ???? ??????? ???? ?? ??????? (2) ????? ????????

mymodel4 <- sem('
               SoxEXPstand =~ RodPoddStand + SobstvPoddStand1 + standwork;
             
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ standwork;
               rabotapodusha ~ standwork;
               vinuzhden ~ standwork;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               schitaus ~ SobstvPoddStand1;
               pitayusborot ~ standwork;
               stidno ~ standwork;
               vina ~ standwork;
               risk ~ RodPoddStand;
               nevsyozavisit ~ standwork;
               tyazhelopomosh ~ standwork;
               tyazheloprinimresh ~ standwork;
               tyazhelopomosh  ~       RodPoddStand;
               vsegdadobiv  ~~    tyazhelopomosh;
               standwork  ~~      rabotapodusha;
               
               

              ',
                
                data=data
)

summary(mymodel4, fit.measures=TRUE)
modindices(mymodel4, sort = T)

semPaths(mymodel4, whatLabels="std", layout = "tree" , label.cex = 1.8, 
         edge.label.cex = 1.5, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple", edge.width=1, node.width = 1.4, curve
         = 1.2)



#???????? ???????

mymodel5 <- sem('
               SoxEXPstand =~ RodPoddStand + SobstvPoddStand1 + standwork;
             
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ standwork;
               rabotapodusha ~ standwork;
               vinuzhden ~ standwork;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               schitaus ~ S*SobstvPoddStand1;
               pitayusborot ~ standwork;
               stidno ~ standwork;
               vina ~ standwork;
              
               nevsyozavisit ~ standwork;
               tyazhelopomosh ~ standwork;
               tyazheloprinimresh ~ standwork;
               tyazhelopomosh  ~       RodPoddStand;
               vsegdadobiv  ~~    tyazhelopomosh;
               standwork  ~~      rabotapodusha;
               risk ~ RodPoddStand;
               SobstvPoddStand1 ~ AG*Age;
               indirect:= S*AG
               

              ',
                
                data=data
)

summary(mymodel5, fit.measures=TRUE)
modindices(mymodel4, sort = T)

semPaths(mymodel4, whatLabels="std", layout = "tree" , label.cex = 1.8, 
         edge.label.cex = 1.5, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple", edge.width=1, node.width = 1.4, curve
         = 1.2)


#?????? ????? ????


mymodel3 <- sem('
              
               thinkwhatothers ~ SoxEXPstand;
               polagatsya ~ SoxEXPstand;
               skrivayutseli ~ SoxEXPstand;
               rabotapodusha ~ SoxEXPstand;
               vinuzhden ~ SoxEXPstand;
               starayussled ~ SoxEXPstand;
               tyazhelopomosh ~ SoxEXPstand;
               tyazheloprinimresh ~ SoxEXPstand;
               chastosovet ~ SoxEXPstand;
               schitaus ~ SoxEXPstand;
               pitayusborot ~ SoxEXPstand;
               stidno ~ SoxEXPstand;
               vina ~ SoxEXPstand;
               risk ~ SoxEXPstand;
               nevsyozavisit ~ SoxEXPstand;
              ',
               
               data=data
)


summary(mymodel3, fit.measures=TRUE)
modindices(mymodel3, sort = T)

semPaths(mymodel3, whatLabels="std", layout = "tree" , label.cex = 1.8, 
         edge.label.cex = 1.5, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple", edge.width=1, node.width = 1.4, curve
         = 1.2)













mymodel <- sem('
               SoxEXPstand ~ RodPoddStand + SobstvPoddStand1 + standwork;
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               rabotapodusha ~ SobstvPoddStand1 + standwork;
               vinuzhden ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               tyazhelopomosh ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               tyazheloprinimresh ~ RodPoddStand + SobstvPoddStand1 + SoxEXPstand;
               schitaus ~ SobstvPoddStand1;
               pitayusborot ~ SobstvPoddStand1 + SoxEXPstand;
               stidno ~ SobstvPoddStand1 + SoxEXPstand;
               vina ~ RodPoddStand + SobstvPoddStand1 +  SoxEXPstand;
               risk ~ RodPoddStand;
               nevsyozavisit ~ RodPoddStand +  SoxEXPstand + SobstvPoddStand1 + standwork

              ',
               
               data=data)

summary(mymodel, fit.measures=TRUE)

semPaths(mymodel, whatLabels="std", layout = "tree" , label.cex = 1.8, 
         edge.label.cex = 1.5, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple", edge.width=1, node.width = 1.4, curve
         = 1.2)
#торая модель:
  mymodel2 <- sem('
               SoxEXPstand =~ RodPoddStand + SobstvPoddStand1 + standwork;
               thinkwhatothers ~ standwork;
               polagatsya ~ RodPoddStand;
               skrivayutseli ~ standwork;
               rabotapodusha ~ standwork;
               vinuzhden ~ standwork;
               vsegdadobiv ~ SobstvPoddStand1;
               starayussled ~ SobstvPoddStand1;
               schitaus ~ SobstvPoddStand1;
               pitayusborot ~ standwork;
               stidno ~ standwork;
               vina ~ standwork;
               risk ~ RodPoddStand;
               nevsyozavisit ~ standwork;
               tyazhelopomosh ~ standwork;
               tyazheloprinimresh ~ standwork;
               tyazhelopomosh  ~       RodPoddStand;
               vsegdadobiv  ~~    tyazhelopomosh;
               standwork  ~~      rabotapodusha
               
        
              ',
                  
                  data=data)

summary(mymodel2, fit.measures=TRUE)
modindices(mymodel2, sort = T)

semPaths(mymodel2, whatLabels="std", layout = "tree" , label.cex = 1.8, 
         edge.label.cex = 1.5, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple", edge.width=1, node.width = 1.4, curve
         = 1.2)
#Третья модель:
  mymodel3 <- sem('
              
               thinkwhatothers ~ SoxEXPstand;
               polagatsya ~ SoxEXPstand;
               skrivayutseli ~ SoxEXPstand;
               rabotapodusha ~ SoxEXPstand;
               vinuzhden ~ SoxEXPstand;
               starayussled ~ SoxEXPstand;
               tyazhelopomosh ~ SoxEXPstand;
               tyazheloprinimresh ~ SoxEXPstand;
               chastosovet ~ SoxEXPstand;
               schitaus ~ SoxEXPstand;
               pitayusborot ~ SoxEXPstand;
               stidno ~ SoxEXPstand;
               vina ~ SoxEXPstand;
               risk ~ SoxEXPstand;
               nevsyozavisit ~ SoxEXPstand;
              ',
                  
                  data=data
  )


summary(mymodel3, fit.measures=TRUE)
modindices(mymodel3, sort = T)

semPaths(mymodel3, whatLabels="std", layout = "tree" , label.cex = 1.8, 
         edge.label.cex = 1.5, style = "lisrel", 
         rotation=2, esize = 3, shapeLat = "circle", nCharNodes=4, what = 'std',
         edge.color = "purple", edge.width=1, node.width = 1.4, curve
         = 1.2)

