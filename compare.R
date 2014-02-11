## voir les sites suivants pour les calculs
## http://www.cbanque.com/credit/principe.php
## http://fr.wikipedia.org/wiki/Plan_de_remboursement

setwd('c:/Temp/Simulateur Credit/')

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

realisePlans <- function(plan) {
	plan$echeance <- with(plan,echeance(Capital,TxAn,Duree) )
	plan$CoutCredit <- with(plan,echeance*Duree*12-Capital)
	return(plan)
}

makePlans <- function(plan) {
	df <- data.frame( Annee=as.numeric(format(Sys.time(), "%Y"))+seq(0,plan$Duree) )
	if(length(plan$echeance)>1) {
		df$Vmin <- c(0,cumsum(rep(min(plan$echeance)*12,plan$Duree)))
		df$Valeur <- c(0,cumsum(rep(plan$echeance[1]*12,plan$Duree)))
		df$Vmax <- c(0,cumsum(rep(max(plan$echeance)*12,plan$Duree)))
		df$Interet <- factor(df$Valeur>plan$CoutCredit[1])
		df$Taux <- 100*plan$TxAn[1]
		df$Interet <- factor(df$Valeur>plan$CoutCredit[1])
	} else {
		df$Vmax <- df$Vmin <- df$Valeur <- c(0,cumsum(rep(plan$echeance*12,plan$Duree)))
		df$Interet <- factor(df$Valeur>plan$CoutCredit)
		df$Taux <- 100*plan$TxAn
		df$Interet <- factor(df$Valeur>plan$CoutCredit)
	}
	
	levels(df$Interet) <- c('Credit','Maison')
	df$Banque <- plan$name
	df$Duree <- plan$Duree
	df$id <- paste(unique(df[,c('Banque','Duree','Taux')]),sep='',collapse='|')
	return(df)
}

## Compare
plans <- list()

plans$ca.1 <-list(name='Credit Agricole',
	Capital = 285000,
	TxAn = 2.95/100,
	Duree = 20
)

plans$bp.1 <-list(name='Banque Populaire',
	Capital = 285000,
	TxAn = c(1.98,1,1.98+1.5)/100,
	Duree = 15
)

plans$bp.2 <-list(name='Banque Populaire',
	Capital = 285000,
	TxAn = c(2.15,1,2.15+1.5)/100,
	Duree = 20
)

plans <- lapply(plans, realisePlans )
simulation <- lapply(plans,makePlans)
simulation <- do.call('rbind',simulation)
sum.simulation <- ddply(simulation,.(id),summarize,
	Banque=unique(Banque),
	Annee=max(Annee),
	Valeur=max(Valeur)
)

ggplot(data=simulation)+
	geom_smooth(aes(x=Annee,y=Valeur,ymax=Vmax,ymin=Vmin,group=id,color=Interet),stat='identity')+
	scale_colour_manual(value=c('red','blue'))+
	geom_text(aes(x=Annee,y=Valeur,label=Banque),data=sum.simulation,color='darkblue',hjust=1)


summary.table <- lapply(plans,function(plan) return(with(plan,c(name,Capital,100*TxAn[1],Duree,round(echeance[1],2),round(CoutCredit[1],2)))))
summary.table <- data.frame(do.call('rbind',summary.table))
names(summary.table) <- c('Banque','Capital','Taux','Duree','Echeance','Cout Total')
summary.table