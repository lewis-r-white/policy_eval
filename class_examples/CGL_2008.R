
library(ggplot2)
library(stargazer)
library(estimatr)
library(here)

# IMPORT CSV DATA
CGL <- read.csv(here("data", "cgl_2008_data_extract.csv"))



# SUMMARY STATISTICS
stargazer(CGL, type="text", digits=2)


# EXAMINE BALANCE IN COVARIATES
# COVARIATE MEAN DIFFERENCES by DAPever
m1 <- lm(formula = LME ~ DAPever, data=CGL)
m2 <- lm(formula = genus ~ DAPever, data=CGL)
m3 <- lm(formula = species ~ DAPever, data=CGL)
se_models = starprep(m1, m2, m3, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(m1, m2, m3, se = se_models, type="text")

# BOXPLOTS TO EXAMINE BALANCE IN COVARIATES
ggplot(CGL, aes(x=as.factor(DAPever), y=LME)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Status")

ggplot(CGL, aes(x=as.factor(DAPever), y=genus)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Status")

ggplot(CGL, aes(x=as.factor(DAPever), y=species)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Status")


# BASIC OLS by DAPever -- THEN ADD INDICATORS FOR OTHER COVARIATES 
# NOTE DO NOT INCLUDE SPECIES IN MODELS TO KEEP RUNNING TIME FAST
mA <- lm(formula = collapse ~ DAPever, data=CGL)
mB <- lm(formula = collapse ~ DAPever + as.factor(LME), data=CGL)
mC <- lm(formula = collapse ~ DAPever + as.factor(LME) + as.factor(genus), data=CGL)
se_models = starprep(mA, mB, mC, stat = c("std.error"), se_type = "HC1", alpha = 0.05)
stargazer(mA, mB, mC, se = se_models, type="text", omit = "(LME)|(genus)|(species)")

#the standard error is about as big as the difference between the beta hat. So there isn't much of a difference when adding LME (and genus). Even though it doesn't move the needle much, less chance for confounding variables when they're included in the model. If it had made the DAPever statistically insignificant, would definitely want to report it. 

