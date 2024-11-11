library(readr)
library(lavaan)
library(dagitty)
library(bnlearn)
library(dplyr)

ESS11 <- read_csv("data/ESS11_clean.csv")

dag <- graphLayout(dagitty("dag{
agea [exposure]
alcfreq [outcome]
cgtsmok [outcome]
cntry [exposure]
desire_to_seek_validation [latent]
edulvlb 
gndr [exposure]
happy
health [exposure]
hinctnta [exposure]
hlthhmp [outcome]
impricha [outcome]
innate_desire_to_help [latent]
innate_desire_to_learn [latent]
ipcrtiva [outcome]
iphlppla [outcome]
ipmodsta [outcome]
iprspota [outcome]
ipshabta [outcome]
ipsucesa [outcome]
ipudrsta [outcome]
lrscale [outcome]
slprl [outcome]
stflife [outcome]
agea -> edulvlb
cntry -> edulvlb
cntry -> happy
cntry -> health
cntry -> lrscale
desire_to_seek_validation -> edulvlb
desire_to_seek_validation -> impricha
desire_to_seek_validation -> ipmodsta
desire_to_seek_validation -> iprspota
desire_to_seek_validation -> ipshabta
desire_to_seek_validation -> ipsucesa
desire_to_seek_validation -> stflife
edulvlb -> alcfreq
edulvlb -> cgtsmok
edulvlb -> impricha
edulvlb -> slprl
edulvlb -> stflife
gndr -> edulvlb
happy -> ipsucesa
health -> happy
health -> hlthhmp
hinctnta -> edulvlb
innate_desire_to_help -> edulvlb
innate_desire_to_help -> hlthhmp
innate_desire_to_help -> iphlppla
innate_desire_to_help -> ipudrsta
innate_desire_to_help -> lrscale
innate_desire_to_help -> stflife
innate_desire_to_learn -> edulvlb
innate_desire_to_learn -> impricha
innate_desire_to_learn -> ipcrtiva
innate_desire_to_learn -> ipsucesa
innate_desire_to_learn -> stflife
}"))

# Which factors positively affect external validation-seeking?

# How do factors such as age, gender, and socioeconomic status affect an individual’s educational attainment?
#   Exposure variables: gndr, agea, hinctnta
#   Outcome: edulvlb


plot(dag)

lvsem <- toString(dag, "lavaan")
lvsem.fit <- sem(lvsem, ESS11)

lvsem <-  "
U=~ l*agea
U=~ l*gndr
U=~ l*hinctnta
edulvlb~agea
edulvlb~gndr
edulvlb~hinctnta
"
cat(lvsem)

ESS11$U <- predict(lvsem.fit)


# Subset ESS11 to keep only the columns specified in the DAG
dag_vars <- names(dag)
ESS11 <- ESS11 %>% select(all_of(dag_vars))
ESS11 <- ESS11 %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(across(where(is.factor), as.numeric))
ESS11$U <- predict(lvsem.fit)


net <- model2network(toString(dag,"bnlearn"))
fit <- bn.fit(net, ESS11)
fit
