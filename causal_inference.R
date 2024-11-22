library(readr)
library(lavaan)
library(dagitty)
library(bnlearn)
library(dplyr)
library(lavaanPlot) # https://lavaanplot.alexlishinski.com/articles/intro_to_lavaanplot

ESS11 <- read_csv("data/ESS11_raw.csv")
ESS11 <- ESS11 %>% select(
  gndr, edulvlb, agea, lrscale, hinctnta, happy,
  stflife, alcfreq, cgtsmok, ipsucesa, iprspota, impricha,
  ipshabta, ipcrtiva, ipudrsta, ipmodsta, iphlppla, health, slprl,
  hlthhmp, height, weighta, rlgdgr, weasoff, enjlf, wrhpp, sclmeet,
  netustm, ctrlife,
  hltprhc, hltprhb, hltprbp, hltpral, hltprbn, hltprpa,
  hltprpf, hltprsd, hltprsc, hltprsh, hltprdi, hltphhc, hltphhb,
  hltphbp, hltphal, hltphbn, hltphpa, hltphpf, hltphsd, hltphsc,
  hltphsh, hltphdi, hltprca,
  hltprref, hltprdk, hltprna, hltphnap,
  hltphref, hltphdk, hltphna
)

# %%
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
    hlthhmp <= 3,
    height <= 300,
    weighta <= 500,
    rlgdgr <= 10,
    weasoff <= 5,
    wrhpp <= 4,
    enjlf <= 4,
    sclmeet <= 7,
    netustm < 6666,
    ctrlife <= 10,
    hltprref == 0,
    hltprdk == 0,
    hltprna == 0,
    hltphnap == 0,
    hltphref == 0,
    hltphdk == 0,
    hltphna == 0,
  )

d$hlthpr_total <-
  d$hltprhc + d$hltprhb + d$hltprbp + d$hltpral + d$hltprbn + d$hltprpa +
  d$hltprpf + d$hltprsd + d$hltprsc + d$hltprsh + d$hltprdi + d$hltphhc +
  d$hltphhb + d$hltphbp + d$hltphal + d$hltphbn + d$hltphpa + d$hltphpf +
  d$hltphsd + d$hltphsc + d$hltphsh + d$hltphdi + d$hltprca
d$hlthpr_total <- scale(d$hlthpr_total)

d <- d %>% select(
  -hltprhc, -hltprhb, -hltprbp, -hltpral, -hltprbn, -hltprpa,
  -hltprpf, -hltprsd, -hltprsc, -hltprsh, -hltprdi, -hltphhc,
  -hltphhb, -hltphbp, -hltphal, -hltphbn, -hltphpa, -hltphpf,
  -hltphsd, -hltphsc, -hltphsh, -hltphdi, -hltprca,
  -hltprref, -hltprdk, -hltprna, -hltphnap,
  -hltphref, -hltphdk, -hltphna
)


d$edulvlb  <- round(d$edulvlb / 100)
d$edulvlb  <- ordered(d$edulvlb)
d$gndr     <- as.numeric(ordered(d$gndr, c("1", "2")))
d$stflife  <- ordered(d$stflife)
d$lrscale  <- ordered(d$lrscale)
d$health   <- ordered(-d$health)
d$hlthhmp  <- ordered(d$hlthhmp)
d$alcfreq  <- ordered(-d$alcfreq)
d$cgtsmok  <- ordered(-d$cgtsmok)
d$slprl    <- ordered(d$slprl)
d$happy    <- ordered(d$happy)
d$ipsucesa <- ordered(-d$ipsucesa)
d$iprspota <- ordered(-d$iprspota)
d$impricha <- ordered(-d$impricha)
d$ipshabta <- ordered(-d$ipshabta)
d$ipcrtiva <- ordered(-d$ipcrtiva)
d$ipudrsta <- ordered(-d$ipudrsta)
d$ipmodsta <- ordered(-d$ipmodsta)
d$iphlppla <- ordered(-d$iphlppla)
d$hinctnta <- ordered(d$hinctnta)
d$wrhpp    <- ordered(d$wrhpp)
d$enjlf    <- ordered(d$enjlf)
d$sclmeet  <- ordered(d$sclmeet)
d$rlgdgr   <- ordered(d$rlgdgr)
d$weasoff  <- ordered(d$weasoff)
d$ctrlife  <- ordered(d$ctrlife)

d$agea     <- scale(d$agea)
d$height   <- scale(d$height)
d$weighta  <- scale(d$weighta)
d$netustm  <- scale(d$netustm)


# %%
# Which factors positively affect external validation-seeking?
# First step: Get factor values for the latent variables
lat_lvsem <- "desire_to_seek_validation =~ impricha + iprspota + ipshabta + ipsucesa
  virtue =~ ipcrtiva + ipmodsta + iphlppla
  real_health =~ health + hlthhmp + hlthpr_total
  happiness =~ happy + stflife + enjlf + wrhpp"


lvsem.fit <- cfa(lat_lvsem, data = d)
summary(lvsem.fit)

# %%
# Inspect latent variable covariance matrix
lavInspect(lvsem.fit, "cov.lv")

# %%
# Obtain latent factor scores and print them (these are the values we can use to add the three variables to the dataset)
latent_scores <- lavPredict(lvsem.fit)
head(latent_scores)

# %%
# Append and delete variables used in latent variable definitions
dl <- cbind(d, as.data.frame(latent_scores))
dl <- dl %>% select(
  -impricha, -iprspota, -ipshabta, -ipsucesa, -ipcrtiva,
  -ipmodsta, -iphlppla, -health, -hlthhmp, -happy, -stflife,
  -enjlf, -wrhpp
)

# %%
dag <- graphLayout(dagitty(read_file("./dag_lat.txt")))
plot(dag)

# Fit SEM on DAG with latent variables now as observed
lvsem <- toString(dag, "lavaan")
cat(lvsem)
M <- lavCor(dl)
fit <- sem(lvsem, sample.cov = M, sample.nobs = nrow(dl))
r <- localTests(dag, sample.cov = M, sample.nobs = nrow(dl))
r[order(-r$p.value), ] # sorted by p.value

plotLocalTestResults(r)
lavaanPlot(model = fit, coefs = TRUE)
