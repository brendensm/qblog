
library(haven)
library(dplyr)
library(survival)
library(ggthemes)

FHS <- read_sav("posts/Survival Analysis/data/Cox Prop Hazard Regression Data.sav")

FHS_surv <- Surv(time = FHS$TimeDeathYears,
                 event = FHS$death)
summary(survfit(FHS_surv ~ 1))

plot(FHS_surv, 
     xlab = "Years Since Baseline",    # x-axis label
     ylab ="Survival Probability",   # y-axis label
     main = "Overall survival curve", # figure title
     ylim = c(0, 1),
     mark.time = TRUE,
     conf.int = FALSE
)

FHS_fit_db <- survfit(Surv(TimeDeathYears, death) ~ diabetes, data = FHS)

col_diabetes <- c("lightblue", "darkblue")

plot(
  FHS_fit_db,
  col = col_diabetes,
  xlab = "Years since baseline",
  ylab = "Survival Probability")
legend(
  "topright",
  legend = c("Not a diabetic","Diabetic"),
  col = col_diabetes,
  lty = 1,
  cex = .9,
  bty = "n")

survminer::ggsurvplot(
  FHS_fit_db, 
  data = FHS,          # again specify the data used to fit linelistsurv_fit_sex 
  conf.int = TRUE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 1,            # present the time axis with an increment of 10 days
  xlab = "Time Since baseline (Years)",
  ylab = "Survival Probability",
  pval = T,# print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "Diabetic Status",       # legend characteristics
  legend.labs = c("not diabetic","diabetic"),
  font.legend = 10, 
  palette = "Dark2",             # specify color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_few()       # simplify plot background
)

FHS_cox <-  coxph(
  Surv(TimeDeathYears, death) ~ diabetes + cursmoke + diabetes + 
    educ + prevchd + age + bmi + sex,
  data = FHS
)

summary(FHS_cox)

survminer::ggforest(FHS_cox, data = FHS)
