library(readr)
library(lavaan)
library(dagitty)
library(bnlearn)
library(dplyr)

ESS11 <- read_csv("data/ESS11.csv")

# Which factors positively affect external validation-seeking?

# How do factors such as age, gender, and socioeconomic status affect an individual’s educational attainment?
#   Exposure variables: gndr, agea, hinctnta
#   Outcome: edulvlb

dag <- graphLayout(dagitty("dag {
U [latent]
agea [exposure]
edulvlb [outcome]
gndr [exposure]
hinctnta [exposure]
U -> agea
U -> gndr
U -> hinctnta
agea -> edulvlb
gndr -> edulvlb
hinctnta -> edulvlb
}"))

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
