
load("data/processed/lmmdata.RData")

library(lme4)
library(performance)

# younger model ====
model_yg <- lmer(zhfa ~ as.factor(chsex) + 
                   time +
                   drwaterq_new + 
                   as.factor(chsex) * time + 
                   as.factor(chsex) * drwaterq_new + 
                   time * drwaterq_new + 
                   age_first_c + 
                   (1 | childid),
                 data = ethiopia_yg)

manual_eth_yg <- ethiopia_yg[complete.cases(ethiopia_yg),]

# older model ====

model_old <- lmer(zhfa ~ as.factor(chsex) + 
                    time +
                    drwaterq_new + 
                    as.factor(chsex) * time + 
                    as.factor(chsex) * drwaterq_new + 
                    time * drwaterq_new + 
                    age_first_c + 
                    (1 | childid),
                  data = ethiopia_old)

manual_eth_old <- ethiopia_old[complete.cases(ethiopia_old),]


# save to LaTeX ====
png("output/model_diagnostics_young_vs_old.png",
    width = 2400, height = 3000, res = 300)

layout(matrix(1:14, nrow = 7, byrow = TRUE))
par(mar = c(4, 4, 2, 1), oma = c(2, 2, 6, 2))

# younger cohort
res_yg <- resid(model_yg)
fit_yg <- fitted(model_yg)
re_yg  <- ranef(model_yg)$childid[,1]

# older cohort
res_old <- resid(model_old)
fit_old <- fitted(model_old)
re_old  <- ranef(model_old)$childid[,1]

# (A) Residuals vs fitted
plot(fit_yg, res_yg,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "(A) Younger: Residuals vs fitted")
abline(h = 0, col = "red")

# (B) Residuals vs fitted
plot(fit_old, res_old,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "(B) Older: Residuals vs fitted")
abline(h = 0, col = "red")


# (C) Q–Q plot of residuals
qqnorm(res_yg, main = "(C) Younger: Q–Q residuals")
qqline(res_yg, col = "red")

# (D) Q–Q plot of residuals
qqnorm(res_old, main = "(D) Older: Q–Q residuals")
qqline(res_old, col = "red")

# (E) Residuals vs follow-up time
plot(manual_eth_yg$time, res_yg,
     xlab = "Follow-up time",
     ylab = "Residuals",
     main = "(E) Younger: Residuals vs time")
abline(h = 0, col = "red")
lines(lowess(manual_eth_yg$time, res_yg), col = "blue", lwd = 2)

# (F) Residuals vs follow-up time
plot(manual_eth_old$time, res_old,
     xlab = "Follow-up time",
     ylab = "Residuals",
     main = "(F) Older: Residuals vs time")
abline(h = 0, col = "red")
lines(lowess(manual_eth_old$time, res_old), col = "blue", lwd = 2)

# (G) Residuals vs baseline age
plot(manual_eth_yg$age_first_c, res_yg,
     xlab = "Centered baseline age",
     ylab = "Residuals",
     main = "(G) Younger: Residuals vs baseline age")
abline(h = 0, col = "red")
lines(lowess(manual_eth_yg$age_first_c, res_yg), col = "blue", lwd = 2)

# (H) Residuals vs baseline age
plot(manual_eth_old$age_first_c, res_old,
     xlab = "Centered baseline age",
     ylab = "Residuals",
     main = "(H) Older: Residuals vs baseline age")
abline(h = 0, col = "red")
lines(lowess(manual_eth_old$age_first_c, res_old), col = "blue", lwd = 2)

# (I) Residuals by sex
boxplot(res_yg ~ manual_eth_yg$chsex,
        xlab = "Sex",
        ylab = "Residuals",
        main = "(I) Younger: Residuals by sex",
        xaxt = "n")
axis(1, at = c(1, 2), labels = c("Female", "Male"))
abline(h = 0, col = "red")

# (J) Residuals by sex
boxplot(res_old ~ manual_eth_old$chsex,
        xlab = "Sex",
        ylab = "Residuals",
        main = "(J) Older: Residuals by sex",
        xaxt = "n")
axis(1, at = c(1, 2), labels = c("Female", "Male"))
abline(h = 0, col = "red")

# (K) Residuals by water access
boxplot(res_yg ~ manual_eth_yg$drwaterq_new,
        xlab = "Water access",
        ylab = "Residuals",
        main = "(K) Younger: Residuals by water access",
        xaxt = "n")
axis(1, at = c(1, 2), labels = c("No", "Yes"))
abline(h = 0, col = "red")

# (L) Residuals by water access
boxplot(res_old ~ manual_eth_old$drwaterq_new,
        xlab = "Water access",
        ylab = "Residuals",
        main = "(L) Older: Residuals by water access",
        xaxt = "n")
axis(1, at = c(1, 2), labels = c("No", "Yes"))
abline(h = 0, col = "red")

# (M) Q–Q plot of random intercepts
qqnorm(re_yg, main = "(M) Younger: Q–Q random intercepts")
qqline(re_yg, col = "red")

# (N) Q–Q plot of random intercepts
qqnorm(re_old, main = "(N) Older: Q–Q random intercepts")
qqline(re_old, col = "red")


mtext("Figure A1. Diagnostic plots for cohort-specific linear mixed-effects models",
      outer = TRUE, cex = 1.2, font = 2, line = 2)

mtext("Left column: Younger cohort    Right column: Older cohort",
      outer = TRUE, cex = 0.9, line = 0)

dev.off()

