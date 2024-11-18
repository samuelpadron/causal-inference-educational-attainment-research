library(readr)
library(lavaan)
library(dagitty)
library(bnlearn)
library(dplyr)

ESS11 <- read_csv("data/ESS11_clean.csv")
dag <- graphLayout(dagitty(read_file("./dag_lat.txt")))

plot(dag)

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

d$edulvlb <- round(d$edulvlb / 100)
d$edulvlb <- ordered(d$edulvlb)
# d$edulvlb <- as.integer(d$edulvlb)
# d$gndr <- as.integer(d$gndr)

d$gndr <- as.numeric( ordered(d$gndr, c("1", "2")))
d$stflife <- ordered(d$stflife)
d$lrscale <- ordered(d$lrscale)
d$health <- ordered(d$health)
d$hlthhmp <- ordered(d$hlthhmp)
d$alcfreq <- ordered(d$alcfreq)
d$cgtsmok <- ordered(d$cgtsmok)
d$slprl <- ordered(d$slprl)
d$happy <- ordered(d$happy)
d$ipsucesa <- ordered(d$ipsucesa)
d$iprspota <- ordered(d$iprspota)
d$impricha <- ordered(d$impricha)
d$ipshabta <- ordered(d$ipshabta)
d$ipcrtiva <- ordered(d$ipcrtiva)
d$ipudrsta <- ordered(d$ipudrsta)
d$ipmodsta <- ordered(d$ipmodsta)
d$iphlppla <- ordered(d$iphlppla)
d$hinctnta <- ordered(d$hinctnta)
d$agea <- scale(d$agea)

d <- subset(d, select=-c(cntry))

# d <- d %>% mutate(across(where(is.numeric), scale))

# Which factors positively affect external validation-seeking?
# First step: Get factor values for the latent variables

lvsem <- toString(dag, "lavaan")

lat_lvsem <- "desire_to_seek_validation =~ impricha + iprspota + ipshabta + ipsucesa + edulvlb
  virtuosity =~ ipcrtiva + ipshabta + ipmodsta + iphlppla
  real_health =~ health + hlthhmp
  happiness =~ happy + stflife"

cat(lvsem)

M <- lavCor(d)
r <- localTests(dag, sample.cov=M, sample.nobs=nrow(d))
plotLocalTestResults(r)
r

fit <- sem(lvsem, sample.cov=M, sample.nobs=nrow(d))
lvsem.fit <- cfa(lat_lvsem, data=d)
summary(fit)

# Inspect latent variable covariance matrix 
lavInspect(lvsem.fit, "cov.lv")

# Obtain latent factor scores and print them (these are the values we can use to add the three variables to the dataset)
latent_scores <- lavPredict(lvsem.fit)
head(latent_scores)

d <- cbind(d, as.data.frame(latent_scores))


summary(fit)

cg <- coordinates(dag)
fg <- lavaanToGraph(fit, digits=2)
coordinates(fg) <- cg
plot(fg, show.coefficients=TRUE)
