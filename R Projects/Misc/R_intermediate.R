fy <- c(2010, 2011, 2012, 2010, 2011, 2012, 2010, 2011, 2012)
company <- c(
  "Apple",
  "Apple",
  "Apple",
  "Google",
  "Google",
  "Google",
  "Microsoft",
  "Microsoft",
  "Microsoft"
)
revenue <- c(65225, 108249, 156508, 29321, 37905, 50175, 62484,
             69943, 73723)
profit <- c(14013, 25922, 41733, 8505, 9737, 10737, 18760, 23150,
            16978)
companiesData <- data.frame(fy, company, revenue, profit)
companiesData$fy <- factor(companiesData$fy, ordered = T)
companiesData$margin <-
  round((companiesData$profit / companiesData$revenue) * 100, 1)
library(plyr)
library(dplyr)
companiesBestMargin <-
  ddply(companiesData, 'company', summarize, best = max(margin))
companiesBestMargin <-
  ddply(companiesData, 'company', transform, best = max(margin))
testResults <-
  companiesData %>% group_by(company) %>% summarise(best = max(margin))
testResults <-
  companiesData %>% group_by(company) %>% mutate(best = max(margin), worst = min(margin))
library(reshape2)
molten <-
  melt(
    companiesData,
    id.vars = c('fy', 'company'),
    measure.vars = c('revenue', 'profit', 'margin'),
    variable.name = 'category',
    value.name = 'amount'
  )
recast_data <-
  dcast(molten,
        formula = fy + company ~ category,
        value.var = 'amount')
########################
str(mtcars)
glimpse(mtcars)
