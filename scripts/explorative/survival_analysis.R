#survival analysis
library(survival)

fit_passage <- coxph(Surv(t_start, t_stop, is_passing) ~ 
                       ridge(Discharge, Waterlevel, fishway,
                             I((1-ff)*Overflow),
                             I((1-ff)*Underflow),
                             ff, theta = 10, scale = TRUE) +
                       frailty(individual, dist = "gauss"),
                     data = model_data1)
print(fit_passage)
betas_passage <- coef(fit_passage)
names(betas_passage) <- c("Discharge", "Waterlevel", "fishway", "Overflow", "Underflow", "ff")
var_passage <- fit_passage$history$frailty$theta
sigma_passage <- sqrt(var_passage)
bh_pass <- basehaz(fit_passage, 
                   centered = FALSE) 
get_H_pass <- approxfun(bh_pass$time, bh_pass$hazard, rule = 2)