setwd("C:/Users/nvbil/Desktop/University/2017/Sci_Comm/SciCommDataTotal.csv")

library(readr)
SciCommDataTotal <- read_csv("C:/Users/nvbil/Desktop/University/2017/Sci_Comm/Data_WWOX.csv")
View(SciCommDataTotal)
attach(SciCommDataTotal)
library(ggplot2)
library(nortest)
library(lmtest)


ad.test(SciCommDataTotal$`Degree of cellular deformation`)
ad.test(SciCommDataTotal$`Diameter of Nucleus`)
ad.test(SciCommDataTotal$`% Dead cells`)

cor.test(SciCommDataTotal$`Degree of cellular deformation`,SciCommDataTotal$`Diameter of Nucleus`, method="spearman")
cor.test(SciCommDataTotal$`Degree of cellular deformation`,SciCommDataTotal$`Diameter of Nucleus`, method="spearman")
cor.test(SciCommDataTotal$`Degree of cellular deformation`,SciCommDataTotal$`Diameter of Nucleus`, method="spearman")





Diam_A_0 = `Diameter of Nucleus`[`Gene A (1=+)` == 0]
Diam_A_1 = `Diameter of Nucleus`[`Gene A (1=+)` == 1]
mean(Diam_A_1)
mean(Diam_A_0)
Defor_A_0 = `Degree of cellular deformation`[`Gene A (1=+)` == 0]
Defor_A_1 = `Degree of cellular deformation`[`Gene A (1=+)` == 1]
mean(Defor_A_1)
mean(Defor_A_0)
Deat_A_0 = `% Dead cells`[`Gene A (1=+)` == 0]
Deat_A_1 = `% Dead cells`[`Gene A (1=+)` == 1]
mean(Deat_A_1)
mean(Deat_A_0)

wilcox.test(SciCommData_WA$`Diameter of Nucleus`, SciCommData_WOA$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WX$`Diameter of Nucleus`, SciCommData_WOX$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WB$`Diameter of Nucleus`, SciCommData_WOB$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )

wilcox.test(SciCommData_WA$`Degree of cellular deformation`, SciCommData_WOA$`Degree of cellular deformation`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WX$`Degree of cellular deformation`, SciCommData_WOX$`Degree of cellular deformation`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WB$`Degree of cellular deformation`, SciCommData_WOB$`Degree of cellular deformation`,conf.int =TRUE, conf.level=0.95 )

wilcox.test(SciCommData_WA$`% Dead cells`, SciCommData_WOA$`% Dead cells`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WX$`% Dead cells`, SciCommData_WOX$`% Dead cells`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WB$`% Dead cells`, SciCommData_WOB$`% Dead cells`,conf.int =TRUE, conf.level=0.95 )




wilcox.test(SciCommData_WA$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOA$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WX$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOX$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WB$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOB$`Diameter of Nucleus`,conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WA$`Degree of cellular deformation`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOA$`Degree of cellular deformation`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WB$`Degree of cellular deformation`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOB$`Degree of cellular deformation`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WX$`Degree of cellular deformation`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOX$`Degree of cellular deformation`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WA$`% Dead cells`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOA$`% Dead cells`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WB$`% Dead cells`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOB$`% Dead cells`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WX$`% Dead cells`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOX$`% Dead cells`, conf.int =TRUE, conf.level=0.95 )


#building glm - Nucl Diam

Diam_GA = glm(`Diameter of Nucleus`~`Gene A (1=+)`,family = Gamma)
summary(Diam_GA)
Diam_GB = glm(`Diameter of Nucleus`~`Gene B (1=+)`,family = Gamma)
summary(Diam_GB)
Diam_PX = glm(`Diameter of Nucleus`~`Product X(1=+)`,family = Gamma)
summary(Diam_PX)
#nonnested -use AIC - B is lowest, thus keep B
Diam_GBGA = glm(`Diameter of Nucleus`~`Gene B (1=+)`+`Gene A (1=+)`,family = Gamma)
summary(Diam_GBGA)
Diam_GBPX = glm(`Diameter of Nucleus`~`Gene B (1=+)`+`Product X(1=+)`,family = Gamma)
summary(Diam_GBPX)

#lrtest
lrtest(Diam_GA)
lrtest(Diam_GB)
lrtest(Diam_PX)
lrtest(Diam_GB, Diam_GBGA)
lrtest(Diam_GB, Diam_GBPX)

#AIC lower for GA
Diam_GBGAPX = glm(`Diameter of Nucleus`~`Gene B (1=+)`+`Gene A (1=+)`+`Product X(1=+)`,family = Gamma)
summary(Diam_GBGAPX)
#Goes up - use GAGB
lrtest(Diam_GBGA,Diam_GBGAPX)
lrtest(Diam_GBGA,Diam_GBGA_Int)

#interaction:
GeneA_GeneB = `Gene B (1=+)`*`Gene A (1=+)`
Diam_GBGA_Int = glm(`Diameter of Nucleus`~`Gene B (1=+)`+`Gene A (1=+)`+ GeneA_GeneB,family = Gamma)
summary(Diam_GBGA_Int)
#AIC improves - final is A,B and A*B

#Check family:
Diam_GBGA_Int_Gamma = glm(`Diameter of Nucleus`~`Gene B (1=+)`+`Gene A (1=+)`,family = Gamma)
summary(Diam_GBGA_Int_Gamma)
Diam_GBGA_Int_InvG = glm(`Diameter of Nucleus`~`Gene B (1=+)`+`Gene A (1=+)`,family = inverse.gaussian)
summary(Diam_GBGA_Int_InvG)
#gamma best fit



#building glm - Death
Death_GA = glm(`% Dead cells`~`Gene A (1=+)`,family = Gamma)
summary(Death_GA)
Death_GB = glm(`% Dead cells`~`Gene B (1=+)`,family = Gamma)
summary(Death_GB)
Death_PX = glm(`% Dead cells`~`Product X(1=+)`,family = Gamma)
summary(Death_PX)
#nonnested -use AIC - X is lowest, thus keep X
lrtest(Death_GA)
lrtest(Death_GB)
lrtest(Death_PX)


Death_PXGA = glm(`% Dead cells`~`Product X(1=+)`+`Gene A (1=+)`,family = Gamma)
summary(Death_PXGA)
Death_PXGB = glm(`% Dead cells`~`Product X(1=+)`+`Gene B (1=+)`,family = Gamma)
summary(Death_PXGB)
lrtest(Death_PX,Death_PXGA)
lrtest(Death_PX,Death_PXGB)

#AIC lower for GB
Death_PXGBGA = glm(`% Dead cells`~`Gene B (1=+)`+`Gene A (1=+)`+`Product X(1=+)`,family = Gamma)
summary(Death_PXGBGA)
#Goes down - use PXGAGB
lrtest(Death_PXGB,Death_PXGBGA)

#interaction:
GeneA_GeneB = `Gene B (1=+)`*`Gene A (1=+)`
GeneA_PrdX = `Product X(1=+)`*`Gene A (1=+)`
GeneB_PrdX = `Product X(1=+)`*`Gene B (1=+)`
Death_PXGBGA_IntAB = glm(`% Dead cells`~`Gene B (1=+)`+`Gene A (1=+)`+`Product X(1=+)`+GeneA_GeneB,family = Gamma)
summary(Death_PXGBGA_IntAB)
#AIC improves -add A*B
Death_PXGBGA_IntAX = glm(`% Dead cells`~`Gene B (1=+)`+`Gene A (1=+)`+`Product X(1=+)`+GeneA_PrdX,family = Gamma)
summary(Death_PXGBGA_IntAX)
#not better - dont add
Death_PXGBGA_IntBX = glm(`% Dead cells`~`Gene B (1=+)`+`Gene A (1=+)`+`Product X(1=+)`+GeneB_PrdX,family = Gamma)
summary(Death_PXGBGA_IntBX)
#Improves - keep B*X
Death_PXGBGA_IntABBX = glm(`% Dead cells`~`Gene B (1=+)`+`Gene A (1=+)`+`Product X(1=+)`+GeneA_GeneB + GeneB_PrdX,family = Gamma)
summary(Death_PXGBGA_IntABBX)
#lowest AIC - best model
lrtest(Death_PXGBGA, Death_PXGBGA_IntAB)
lrtest(Death_PXGBGA, Death_PXGBGA_IntBX)
lrtest(Death_PXGBGA, Death_PXGBGA_IntAX)
lrtest(Death_PXGBGA, Death_PXGBGA_IntABBX)


#InvG wouldnt even fit data.

#building glm - Deformation
Defor_GA = glm(`Degree of cellular deformation`~`Gene A (1=+)`,family = Gamma)
summary(Defor_GA)
Defor_GB = glm(`Degree of cellular deformation`~`Gene B (1=+)`,family = Gamma)
summary(Defor_GB)
Defor_PX = glm(`Degree of cellular deformation`~`Product X(1=+)`,family = Gamma)
summary(Defor_PX)
#nonnested -use AIC - X is lowest, thus keep X
lrtest(Defor_GA)
lrtest(Defor_GB)
lrtest(Defor_PX)

Defor_PXGA = glm(`Degree of cellular deformation`~`Product X(1=+)`+`Gene A (1=+)`,family = Gamma)
summary(Defor_PXGA)
Defor_PXGB = glm(`Degree of cellular deformation`~`Product X(1=+)`+`Gene B (1=+)`,family = Gamma)
summary(Defor_PXGB)
#AIC not better - only use X
lrtest(Defor_PX,Defor_PXGA)
lrtest(Defor_PX,Defor_PXGB)

Defor_PX_Gamma = glm(`Degree of cellular deformation`~`Product X(1=+)`,family = Gamma)
summary(Defor_PX_Gamma)
Defor_PX_InvG = glm(`Degree of cellular deformation`~`Product X(1=+)`,family = inverse.gaussian)
summary(Defor_PX_InvG)
Defor_PX_G = glm(`Degree of cellular deformation`~`Product X(1=+)`,family = gaussian)
summary(Defor_PX_G)
Defor_PX_GammaL = glm(`Degree of cellular deformation`~`Product X(1=+)`,family = Gamma(link = "log"))
summary(Defor_PX_GammaL)
#Gamma lower AIC

#Final Diameter:
Diam_GBGA = glm(`Diameter of Nucleus`~`Gene B (1=+)`+`Gene A (1=+)`,family = Gamma(link="identity"))
summary(Diam_GBGA)
confint(Diam_GBGA)

Death_PXGBGA_IntABBX = glm(`% Dead cells`~`Gene B (1=+)`+`Gene A (1=+)`+`Product X(1=+)`+GeneA_GeneB + GeneB_PrdX,family = Gamma(link="identity"))
summary(Death_PXGBGA_IntABBX)
confint(Death_PXGBGA_IntABBX)

Defor_PX = glm(`Degree of cellular deformation`~`Product X(1=+)`,family = Gamma(link="identity"))
summary(Defor_PX)
confint(Defor_PX)

#Selecting for X
SciCommData_WX = SciCommDataTotal[`Product X(1=+)` == 1,]
SciCommData_WOX = SciCommDataTotal[`Product X(1=+)` == 0,]
par(mfrow=c(3,2))
hist(SciCommData_WOX$`Diameter of Nucleus`,main="Hist Diameter without X")
hist(SciCommData_WX$`Diameter of Nucleus`,main="Hist Diameter with X")
hist(SciCommData_WOX$`Degree of cellular deformation`,main="Hist Deformation without X")
hist(SciCommData_WX$`Degree of cellular deformation`,main="Hist Deformation with X")
hist(SciCommData_WOX$`% Dead cells`,main="Hist Death without X")
hist(SciCommData_WX$`% Dead cells`,main="Hist Death with X")

#Selecting for A
SciCommData_WA = SciCommDataTotal[`Gene A (1=+)` == 1,]
SciCommData_WOA = SciCommDataTotal[`Gene A (1=+)` == 0,]
par(mfrow=c(3,2))
hist(SciCommData_WOA$`Diameter of Nucleus`,main="Hist Diameter without A")
hist(SciCommData_WA$`Diameter of Nucleus`,main="Hist Diameter with A")
hist(SciCommData_WOA$`Degree of cellular deformation`,main="Hist Deformation without A")
hist(SciCommData_WA$`Degree of cellular deformation`,main="Hist Deformation with A")
hist(SciCommData_WOA$`% Dead cells`,main="Hist Death without A")
hist(SciCommData_WA$`% Dead cells`,main="Hist Death with A")

#Selecting for B
SciCommData_WB = SciCommDataTotal[`Gene B (1=+)` == 1,]
SciCommData_WOB = SciCommDataTotal[`Gene B (1=+)` == 0,]
par(mfrow=c(3,2))
hist(SciCommData_WOB$`Diameter of Nucleus`,main="Hist Diameter without B")
hist(SciCommData_WB$`Diameter of Nucleus`,main="Hist Diameter with B")
hist(SciCommData_WOB$`Degree of cellular deformation`,main="Hist Deformation without B")
hist(SciCommData_WB$`Degree of cellular deformation`,main="Hist Deformation with B")
hist(SciCommData_WOB$`% Dead cells`,main="Hist Death without B")
hist(SciCommData_WB$`% Dead cells`,main="Hist Death with B")

#Gene B -same as A?
SciCommData_WBWX = SciCommData_WB[`Product X(1=+)` == 1,]
SciCommData_WBWOX = SciCommData_WB[`Product X(1=+)` == 0,]
SciCommData_WOBWX = SciCommData_WOB[`Product X(1=+)` == 1,]
SciCommData_WOBWOX = SciCommData_WOB[`Product X(1=+)` == 0,]

par(mfrow = c(2,2))
hist(SciCommData_WBWX$`% Dead cells`,main="Histogram of % Death with B with X",xlab = "% Dead Cells" )
hist(SciCommData_WBWOX$`% Dead cells`,main="Histogram of % Death with B without X",xlab = "% Dead Cells" )
hist(SciCommData_WOBWX$`% Dead cells`,main="Histogram of % Death without B with X",xlab = "% Dead Cells" )
hist(SciCommData_WOBWOX$`% Dead cells`,main="Histogram of % Death without B without X",xlab = "% Dead Cells" )

wilcox.test(SciCommData_WBWX$`% Dead cells`,SciCommData_WBWOX$`% Dead cells`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOBWX$`% Dead cells`,SciCommData_WOBWOX$`% Dead cells`, conf.int =TRUE, conf.level=0.95 )

wilcox.test(SciCommData_WBWX$`Diameter of Nucleus`,SciCommData_WBWOX$`Diameter of Nucleus`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOBWX$`Diameter of Nucleus`,SciCommData_WOBWOX$`Diameter of Nucleus`, conf.int =TRUE, conf.level=0.95 )

wilcox.test(SciCommData_WBWX$`Degree of cellular deformation`,SciCommData_WBWOX$`Degree of cellular deformation`, conf.int =TRUE, conf.level=0.95 )
wilcox.test(SciCommData_WOBWX$`Degree of cellular deformation`,SciCommData_WOBWOX$`Degree of cellular deformation`, conf.int =TRUE, conf.level=0.95 )


par(mfrow = c(2,2))
hist(SciCommData_WBWX$`Diameter of Nucleus`,main="Histogram of Nuclear Diameter with B with X",xlab = "Nuclear diamete(um) " )
hist(SciCommData_WBWOX$`Diameter of Nucleus`,main="Histogram of Nuclear Diameter with B without X",xlab = "Nuclear diamete(um)" )
hist(SciCommData_WOBWX$`Diameter of Nucleus`,main="Histogram of Nuclear Diameter without B with X",xlab = "Nuclear diamete(um)" )
hist(SciCommData_WOBWOX$`Diameter of Nucleus`,main="Histogram of Nuclear Diameter without B without X",xlab = "Nuclear diamete(um)" )


####

SciCommDataTotal$`Product X(1=+)` <- as.factor(SciCommDataTotal$`Product X(1=+)`)
SciCommData_WOX$`Product X(1=+)` <- as.factor(SciCommData_WOX$`Product X(1=+)`)
SciCommData_WX$`Product X(1=+)` <- as.factor(SciCommData_WX$`Product X(1=+)`)

SciCommDataTotal$`Gene B (1=+)` <- as.factor(SciCommDataTotal$`Gene B (1=+)`)
SciCommData_WOX$`Gene B (1=+)` <- as.factor(SciCommData_WOX$`Gene B (1=+)`)
SciCommData_WX$`Gene B (1=+)` <- as.factor(SciCommData_WX$`Gene B (1=+)`)

SciCommDataTotal$`Gene A (1=+)` <- as.factor(SciCommDataTotal$`Gene A (1=+)`)
SciCommData_WOX$`Gene A (1=+)` <- as.factor(SciCommData_WOX$`Gene A (1=+)`)
SciCommData_WX$`Gene A (1=+)` <- as.factor(SciCommData_WX$`Gene A (1=+)`)


#ggplot(SciCommDataTotal, aes(x= `Degree of cellular deformation` ,y = `Diameter of Nucleus`)) + geom_point(aes(color=`Gene A (1=+)`, shape=`Gene B (1=+)`))+ ggtitle("Diameter of Nucleus With Product X")


#With X
DiamSort = SciCommData_WX[order(SciCommData_WX$`Diameter of Nucleus`),]
DiamOrd = 1:132
ggplot(DiamSort, aes(x= `DiamOrd` ,y = `Diameter of Nucleus`)) + geom_point(aes(color=`Gene A (1=+)`, shape=`Gene B (1=+)`))+ ggtitle("Diameter of Nucleus With Product X")

DeathSort = SciCommData_WX[order(SciCommData_WX$`% Dead cells`),]
DeathOrd = 1:132
ggplot(DeathSort, aes(x= `DeathOrd` ,y = `% Dead cells`)) + geom_point(aes(color=`Gene A (1=+)`,shape=`Gene B (1=+)`))+ ggtitle("% Cell Death With Product X")

DeforSort = SciCommData_WX[order(SciCommData_WX$`Degree of cellular deformation`),]
DeforOrd = 1:132
ggplot(DeforSort, aes(x= `DeforOrd` ,y = `Degree of cellular deformation`)) + geom_point(aes(color=`Gene A (1=+)`,shape=`Gene B (1=+)`))+ ggtitle("Degree of cellular deformation With Product X")

#Without X
par(mfrow=c(1,3))
DiamSort = SciCommData_WOX[order(SciCommData_WOX$`Diameter of Nucleus`),]
DiamOrd = 1:82
ggplot(DiamSort, aes(x= `DiamOrd` ,y = `Diameter of Nucleus`)) + geom_point(aes(color=`Gene A (1=+)`,shape=`Gene B (1=+)`)) + ggtitle("Diameter of Nucleus Without Product X")

DeathSort = SciCommData_WOX[order(SciCommData_WOX$`% Dead cells`),]
DeathOrd = 1:82
ggplot(DeathSort, aes(x= `DeathOrd` ,y = `% Dead cells`)) + geom_point(aes(color=`Gene A (1=+)`,shape=`Gene B (1=+)`))+ ggtitle("% Cell Death Without Product X")

DeforSort = SciCommData_WOX[order(SciCommData_WOX$`Degree of cellular deformation`),]
DeforOrd = 1:82
ggplot(DeforSort, aes(x= `DeforOrd` ,y = `Degree of cellular deformation`)) + geom_point(aes(color=`Gene A (1=+)`,shape=`Gene B (1=+)`))+ ggtitle("Degree of cellular deformation Without Product X")


DeathSortAll = SciCommDataTotal[order(SciCommDataTotal$`% Dead cells`),]
DeathOrdA = 1:214
#ggplot(DeathSortAll, aes(x= `DeathOrdA` ,y = `% Dead cells`)) + geom_point(aes(color=`Product X(1=+)`,shape=`Gene B (1=+)`, fill=ifelse(`Gene B (1=+)`, NA,`Gene A (1=+)`))) + ggtitle("% Cell Death") + scale_fill_discrete(na.value=NA, guide="none")+ scale_shape_manual(values=c(21,22))+ scale_alpha_manual(values=c("1"=0, "0"=1))

ggplot(DeathSortAll,aes(x=`DeathOrdA`,y=`% Dead cells`, color=factor(`Product X(1=+)`), shape=factor(`Gene B (1=+)`))) +
  geom_point(size=2, aes(fill=factor(`Product X(1=+)`), alpha=as.character(`Gene A (1=+)`))) +
  geom_point(size=2) +      
  scale_shape_manual(values=c(21,22,23)) +
  scale_alpha_manual(values=c("1"=0, "0"=1))

DiamSortAll = SciCommDataTotal[order(SciCommDataTotal$`Diameter of Nucleus`),]
DiamOrdA = 1:214

ggplot(DiamSortAll,aes(x=`DiamOrdA`,y=`Diameter of Nucleus`, color=factor(`Product X(1=+)`), shape=factor(`Gene B (1=+)`))) +
  geom_point(size=2, aes(fill=factor(`Product X(1=+)`), alpha=as.character(`Gene A (1=+)`))) +
  geom_point(size=2) +      
  scale_shape_manual(values=c(21,22,23)) +
  scale_alpha_manual(values=c("1"=0, "0"=1))

DeforSortAll = SciCommDataTotal[order(SciCommDataTotal$`Degree of cellular deformation`),]
DeforOrdA = 1:214

ggplot(DeforSortAll,aes(x=`DeforOrdA`,y=`Degree of cellular deformation`, color=factor(`Product X(1=+)`), shape=factor(`Gene B (1=+)`))) +
  geom_point(size=2, aes(fill=factor(`Product X(1=+)`), alpha=as.character(`Gene A (1=+)`))) +
  geom_point(size=2) +      
  scale_shape_manual(values=c(21,22,23)) +
  scale_alpha_manual(values=c("1"=0, "0"=1))


#ggplot(SciCommDataTotal, aes(x= `Degree of cellular deformation` ,y = `Diameter of Nucleus`)) + geom_point(aes(color=`Gene A (1=+)`, shape=`Gene B (1=+)`))+ ggtitle("Diameter of Nucleus With Product X")
ggplot(SciCommDataTotal,aes(x=`% Dead cells`,y=`Degree of cellular deformation`, color=factor(`Product X(1=+)`), shape=factor(`Gene B (1=+)`))) +
  geom_point(size=2, aes(fill=factor(`Product X(1=+)`), alpha=as.character(`Gene A (1=+)`))) +
  geom_point(size=2) +      
  scale_shape_manual(values=c(21,22,23), name="Gene B",labels=c("Negative","Positive")) +
  scale_alpha_manual(values=c("1"=0, "0"=1), name="Gene A",labels=c("Negative","Positive")) +
  scale_colour_discrete(name="PrdX",labels=c("Negative","Positive"))

ggplot(SciCommDataTotal,aes(x=`Diameter of Nucleus`,y=`Degree of cellular deformation`, color=factor(`Product X(1=+)`), shape=factor(`Gene B (1=+)`))) +
  geom_point(size=2, aes(fill=factor(`Product X(1=+)`), alpha=as.character(`Gene A (1=+)`))) +
  geom_point(size=2) +      
  scale_shape_manual(values=c(21,22,23), name="Gene B",labels=c("Negative","Positive")) +
  scale_alpha_manual(values=c("1"=0, "0"=1), name="Gene A",labels=c("Negative","Positive")) +
  scale_colour_discrete(name="PrdX",labels=c("Negative","Positive"))

ggplot(SciCommDataTotal,aes(x=`% Dead cells`,y=`Diameter of Nucleus`, color=factor(`Product X(1=+)`), shape=factor(`Gene B (1=+)`))) +
  geom_point(size=2, aes(fill=factor(`Product X(1=+)`), alpha=as.character(`Gene A (1=+)`))) +
  geom_point(size=2) +      
  scale_shape_manual(values=c(21,22,23), name="Gene B",labels=c("Negative","Positive")) +
  scale_alpha_manual(values=c("1"=0, "0"=1), name="Gene A",labels=c("Negative","Positive")) +
  scale_colour_discrete(name="PrdX",labels=c("Negative","Positive"))




#GLM
par(mfrow =c(1,3))
hist(SciCommDataTotal$`Diameter of Nucleus`) #lognormal
hist(SciCommDataTotal$`Degree of cellular deformation`) #gamma
hist(SciCommDataTotal$`% Dead cells`) #gamma



Death = glm(`% Dead cells`~ `Gene A (1=+)`+ `Gene B (1=+)`+ `Product X(1=+)`, family = Gamma(link="inverse"))
summary(Death)
coef(summary(Death))
confint(Death)

Deformation = glm(`Degree of cellular deformation`~ `Gene A (1=+)`+ `Gene B (1=+)`+ `Product X(1=+)`, family = Gamma(link="inverse"))
summary(Deformation)
coef(summary(Deformation))
confint(Deformation)

Diameter = glm(`Diameter of Nucleus`~ `Gene A (1=+)`+ `Gene B (1=+)`+ `Product X(1=+)`, family = Gamma(link="inverse"))
summary(Diameter)
coef(summary(Diameter))
confint(Diameter)

summary(SciCommData_WX)[, 2:4]
summary(SciCommData_WOX)[, 2:4]
summary(SciCommData_WA)[, 2:4]
summary(SciCommData_WOA)[, 2:4]
summary(SciCommData_WB)[, 2:4]
summary(SciCommData_WOB)[, 2:4]


MeanWX_Diam = mean(SciCommData_WX$`Diameter of Nucleus`)
MeanWOX_Diam = mean(SciCommData_WOX$`Diameter of Nucleus`)
MeanWX_Defor =mean(SciCommData_WX$`Degree of cellular deformation`)
MeanWOX_Defor =mean(SciCommData_WOX$`Degree of cellular deformation`)
MeanWX_Death =mean(SciCommData_WX$`% Dead cells`)
MeanWOX_Death=mean(SciCommData_WOX$`% Dead cells`)


MeanWA_Diam =mean(SciCommData_WA$`Diameter of Nucleus`)
MeanWOA_Diam =mean(SciCommData_WOA$`Diameter of Nucleus`)
MeanWA_Defor =mean(SciCommData_WA$`Degree of cellular deformation`)
MeanWOA_Defor =mean(SciCommData_WOA$`Degree of cellular deformation`)
MeanWA_Death =mean(SciCommData_WA$`% Dead cells`)
MeanWOA_Death =mean(SciCommData_WOA$`% Dead cells`)


MeanWB_Diam =mean(SciCommData_WB$`Diameter of Nucleus`)
MeanWOB_Diam =mean(SciCommData_WOB$`Diameter of Nucleus`)
MeanWB_Defor =mean(SciCommData_WB$`Degree of cellular deformation`)
MeanWOB_Defor =mean(SciCommData_WOB$`Degree of cellular deformation`)
MeanWB_Death =mean(SciCommData_WB$`% Dead cells`)
MeanWOB_Death =mean(SciCommData_WOB$`% Dead cells`)

Means_Diam = c(MeanWX_Diam, MeanWOX_Diam, MeanWA_Diam, MeanWOA_Diam, MeanWB_Diam, MeanWOB_Diam)
Means_Defor = c(MeanWX_Defor, MeanWOX_Defor, MeanWA_Defor, MeanWOA_Defor, MeanWB_Defor, MeanWOB_Defor)
Means_Death = c(MeanWX_Death, MeanWOX_Death, MeanWA_Death, MeanWOA_Death, MeanWB_Death, MeanWOB_Death)




plot(Means_Diam)

install.packages("scatterplot3d")
install.packages("scatterpie")
library(scatterplot3d) 
scatterplot3d(`Diameter of Nucleus`,`Degree of cellular deformation`,`% Dead cells`)



MeansAll           <- as.data.frame(cbind(Means_Death, Means_Defor,Means_Diam))
rownames(MeansAll) <-c("With X", "Without X", "With A", "Without A", "With B", "Without B")
#MeansAll      <- factor(MeansAll, labels = c("Mean Death", "Mean Deformation", "Mean Diameter"))

Mean_All = t(MeansAll)
barplot(as.matrix(Mean_All))


library(ggplot2)

ggplot(data = MeansAll, aes(x = MeansAll)) + 
  geom_bar(stat = "identity")

ggplot(Mean_All, aes(x=Mean_All, fill=row)) + 
  geom_bar(stat="identity") +
  xlab("\nType") +
  ylab("Time\n") +
  guides(fill=FALSE) +
  theme_bw()

#Treemap
install.packages("treemap")

library(readxl)
TreeMapData <- read_excel("C:/Users/nvbil/Desktop/University/2017/Sci_Comm/TreeMapData.xlsx", col_names = FALSE)
View(TreeMapData)

library(treemap)
treemap(TreeMapData, #Your data frame object
        index=c("X__1","X__2"),  #A list of your categorical variables
        vSize = "X__3",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Relative effects of changing cell treatments", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)


