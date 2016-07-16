# install.packages('survival')
library(survival)
inData <- read.csv('?')
attach(inData)
###
## PLACEHOLDER: [Read features with their names]
Cust <- Customer_id
l_resp <- Last_Response_tag
Plat <- Platinum_flag
span <- Months
Resp <- Purchase_2k
X <- cbind(l_resp, Plat)
###
##  Non-paramteric sol
###
kmsurv <- survfit(Surv(span, Resp) ~ 1)
summary(kmsurv)
plot(kmsurv, xlab = "Span", ylab = "Survival probability")
#############
# 1. The curve starts from a point below 1, which means some of the observation/customer made an immediate purchase of $20,000 just after receiving the offer (in month 0)
#
# 2. After 6 months, around 62% of the population have survived. In other words, 38% of the population has made a purchase of more than $20,000
#
# 3. Around 38% of the population survives even after 12 months. This does not mean they will never make a purchase. But from a non-parametric solution we cannot extrapolate the solution for more than 12 months.
#####################
group1 <- l_resp

group2 <- Plat

kmsurv1 <- survfit(Surv(span, Resp) ~ group1)
summary(kmsurv1)
plot(kmsurv1,
     xlab =  'span',
     ylab = 'Survival Proabability')

#######################
##  Semi parametric sol
######################
coxph <- coxph(Surv(span,Resp)~X,method = 'breslow')

summary(coxph)