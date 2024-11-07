library(readr)
library(lavaan)
library(dagitty)

ESS11 <- read_csv("data/ESS11.csv")

dag <- graphLayout(dagitty("dag {
	innate_desire_to_learn[latent]
	innate_desire_to_help[latent]
	desire_to_seek_validation[latent]
	agea[exposure]
	cntry[exposure]
	gndr[exposure]
	health[exposure]
	ipshabta[outcome]
	ipmodsta[outcome]
	ipsucesa[outcome]
	iprspota[outcome]
	impricha[outcome]
	stflife[outcome]
	ipcrtiva[outcome]
	iphlppla[outcome]
	hlthhmp[outcome]
	ipudrsta[outcome]
	lrscale[outcome]
	alcfreq[outcome]
	cgtsmok[outcome]
	slprl[outcome]

	agea     -> edulvlb
	alcfreq  <- edulvlb
	cgtsmok  <- edulvlb
	cntry    -> { edulvlb happy lrscale health }
	gndr     -> edulvlb
	happy    -> ipsucesa
	health   -> happy
	health   -> hlthhmp

	edulvlb  <- {
		innate_desire_to_learn
		innate_desire_to_help
		desire_to_seek_validation
	}

	edulvlb -> {
		slprl    
		stflife
		impricha
	}

	desire_to_seek_validation -> {
		ipshabta
		ipmodsta
		ipsucesa
		iprspota
		impricha
		stflife
	}

	innate_desire_to_learn -> {
		impricha
		ipcrtiva
		ipsucesa
		stflife
	}

	innate_desire_to_help -> {
		iphlppla
		hlthhmp  
		ipudrsta
		stflife
		lrscale
	}
}"))

plot(dag)

