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