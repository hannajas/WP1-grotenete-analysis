library(mgcv)

gam_y <- gamm(label_bin ~ s(Q_an) + s(photoperiod) + s(R), random = list(tag_serial_number = ~1),method = "REML", data = data_env, family = binomial(link = "logit"))
summary(gam_y$lme)
gam.check(gam_y)

