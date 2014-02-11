## voir les sites suivants pour les calculs
## http://www.cbanque.com/credit/principe.php
## http://fr.wikipedia.org/wiki/Plan_de_remboursement

## Libraries
library(plyr)
library(ggplot2)

## Functions
TxPer <- function(TxAn, EchAn=12) {
	tx <- (1+TxAn)^(1/EchAn)-1
	return(tx)
}

TxAn <- function(TxPer,EchAn=12) {
	tx <- (1+TxPer)^EchAn-1
	return(tx)
}

echeance <- function(capital, TxAn, Duree, EchAn = 12) {
	NbEch <- Duree * EchAn
	tx <- TxPer(TxAn, EchAn)
	montant <- capital*tx/(1-(1+tx)^-NbEch)
	return(montant)
}

test.echeance <- function(tx,C,M,N) {
	M-C*tx/(1-(1+tx)^-N)
}

## Simulation Mensualite
# Pour une duree, un taux, quelle mensualite?
capital <- 285000
tx.12 <- 0.05

simulation <- expand.grid( duree = seq(15,35), tx = seq(5,0.5,-0.5)/100 )

simulation <- ddply(simulation,.(duree,tx),transform,mensualite = echeance(capital,tx,duree))
simulation$Annees <- factor(as.character(simulation$duree))

qplot(tx,mensualite,data=simulation,geom='line',group=duree,col=Annees)+
	geom_hline(y=1500,col=2,linetype=2)

qplot(duree,mensualite,data=simulation,geom="line",group=tx,# col=factor(100*tx),
	main=paste('Simulation de Credit pour un capital de',capital,'Euros') )+
	geom_hline(y=1500,col=4,linetype=2)+
	geom_hline(y=1700,col=2,linetype=2)+
	geom_text(aes(x=21,y=mensualite[duree==21],label=100*tx[duree==20]),data=simulation,fill=1)

## Simulation Taux
# pour une duree, une mensualite, quel taux
uniroot(test.echeance,TxPer(c(1,5)/100),C=capital,M=1500,N=30*12)

simulation.tx <- expand.grid(duree = seq(20,35,5),mensualite = seq(1500,2000,100))
simulation.tx$Annees <- factor(as.character(simulation.tx$duree))

simulation.tx <- ddply(simulation.tx,.(duree,mensualite),transform,
	taux.periodique=uniroot(test.echeance,TxPer(c(1,100)/100),C=capital,M=mensualite,N=duree*12)$root
)

simulation.tx$taux.annuel<-TxAn(simulation.tx$taux.periodique)

qplot(mensualite,taux.annuel,data=simulation.tx,geom='line',group=duree,col=Annees)+
	geom_hline(y=0.023,col=2,linetype=2)+
	geom_vline(x=1600,col=2,linetype=2)

## simulation capital
# pour un capital, un taux, une duree, quelle mensualite

simulation.cp <- expand.grid(capital = seq(250000,350000,25000), tx = seq(5,0.5,-0.5)/100, duree = seq(15,35,5) )
simulation.cp$Annees <- factor(as.character(simulation.cp$duree))
simulation.cp$Capital <- factor(paste(as.character(simulation.cp$capital/1000),'k',sep=''))

simulation.cp <- ddply(simulation.cp,.(capital,duree,tx),transform,mensualite = echeance(capital,tx,duree))

qplot(tx,mensualite,data=simulation.cp[simulation.cp$duree==25,],geom='line',group=capital,col=Capital,main='Simulation sur 25 Ans')+
	geom_hline(y=1500,col=2,linetype=2)+
	geom_vline(x=0.023,col=2,linetype=2)

qplot(tx,mensualite,data=simulation.cp[simulation.cp$duree==30,],geom='line',group=capital,col=Capital,main='Simulation sur 30 Ans')+
	geom_hline(y=1500,col=2,linetype=2)+
	geom_vline(x=0.023,col=2,linetype=2)

qplot(tx,mensualite,data=simulation.cp,geom='line',group=capital,col=Capital,facets=~duree)+
	geom_hline(y=1500,col=2,linetype=2)+
	geom_vline(x=0.023,col=2,linetype=2)



