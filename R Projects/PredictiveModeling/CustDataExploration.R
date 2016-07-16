custdata <-
  read.table('../../Resources/Datasets/101s/Custdata.tsv',
             sep = '\t',
             header = T)
summary(custdata)
####  Age - Health insurance scatterplot
library(ggplot2)
ggplot(custdata, aes(age, health.ins)) +
  geom_point()
####
with(custdata, table(sex, health.ins))
with(custdata, table(is.employed, health.ins))
with(custdata, table(state.of.res, health.ins))
####
