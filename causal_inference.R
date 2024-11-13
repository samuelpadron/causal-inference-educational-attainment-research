library(readr)
library(lavaan)
library(dagitty)
library(bnlearn)
library(dplyr)

ESS11 <- read_csv("data/ESS11_clean.csv")

# Pre-processing steps
d <- ESS11 %>%
  filter(
    edulvlb <= 800,
    agea < 100,
    lrscale <= 10,
    hinctnta <= 10,
    happy <= 10,
    stflife <= 10,
    alcfreq <= 7,
    cgtsmok <= 6,
    ipsucesa <= 6,
    iprspota <= 6,
    impricha <= 6,
    ipshabta <= 6,
    ipcrtiva <= 6,
    ipudrsta <= 6,
    ipmodsta <= 6,
    iphlppla <= 6,
    health <= 5,
    slprl <= 4,
    hlthhmp <= 3
  )
d$edulvlb <- cut(d$edulvlb, breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800))
d$edulvlb <- ordered(d$edulvlb)
d$gndr <- as.integer(d$gndr)
# d$agea <- as.integer(d$agea)
d$agea <- scale(d$agea)

d <- subset(d, select=setdiff(names(dag), latents(dag)))
d <- subset(d, select=-c(cntry))

dag_vars <- names(dag)
d <- d %>% select(all_of(dag_vars))
d <- d %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(across(where(is.factor), as.numeric))

dag <- graphLayout(dagitty("dag{
agea [exposure]
alcfreq [outcome]
cgtsmok [outcome]
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
edulvlb -> alcfreq
edulvlb -> cgtsmok
edulvlb -> impricha
edulvlb -> slprl
edulvlb -> stflife
gndr -> edulvlb
happy -> ipsucesa
health -> happy
health -> hlthhmp
hinctnta -> edulvlb}"))


# Which factors positively affect external validation-seeking?

# First step: Get factor values for the latent variables 'desire_to_seek_validation', 'innate_desire_to_help', 'innate_desire_to_learn'
lvsem <- toString(dag, "lavaan")
#cat(lvsem)
latent_variables <- 'desire_to_seek_validation =~ ipsucesa + ipshabta + ipmodsta + iprspota\ninnate_desire_to_help =~ ipudrsta + iphlppla + lrscale + hlthhmp\ninnate_desire_to_learn =~ ipsucesa + ipcrtiva + impricha'
full_model <- paste(lvsem, latent_variables, sep="")
#cat(full_model)

# Fit the CFA model and inspect latent variable covariance matrix (right now only using latent_variables, not complete DAG)
fit <- cfa(latent_variables, data=d)
lavInspect(fit, "cov.lv")

# Obtain latent factor scores and print them (these are the values we can use to add the three variables to the dataset)
latent_scores <- lavPredict(fit)
cat(latent_scores)

summary(fit)


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

net <- model2network(toString(dag,"bnlearn"))
fit <- bn.fit(net, ESS11)
fit
