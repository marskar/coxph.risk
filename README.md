# `coxph.risk`

Absolute risk estimation with Cox proportional hazards models.


The goal of coxph.risk is to compute absolute risk for cohort data.

## Installation

You can install `coxph.risk` with `devtools`:

``` r
devtools::install_github("https://github.com/marskar/coxph.risk")
```

## Example of `coxph.risk()` use


``` r
data(mgus)

# Mayo Clinic 20-35 year follow-up of patients with 
# monoclonal gammopathy of undetermined significance (MGUS)

# Hazard models of multiple myeloma, death, other plasma malignancy
# Time scale is days from MGUS diagnosis

myeloma.model <- Surv(time, status)~age+factor(sex)+alb+hgb+mspike
competing.model <- Surv(time, status)~age*factor(sex)

cox1 <- coxph(myeloma.model,data=mgus2,subset=event=="myeloma")
cox2 <- coxph(competing.model,data=mgus2,subset=event=="death")
cox3 <- coxph(competing.model,data=mgus2,subset=event=="other")

# Absolute risk predictions for multiple myeloma in 5 years
predict.data <-  mgus2[mgus2$event=="death",]

# ONLY COMPLETE CASES
predict.data <- predict.data[complete.cases(predict.data),]
risk <- coxph.risk(0, 5*365.25, newdata = predict.data,
     			     cox1, cox2, cox3)

summary(risk)

# RISK BY AGE AND GENDER AT MGUS DIAGNOSIS
cols <- c("dodgerblue","darkorchid")

plot(risk*1000~age, data = predict.data, 
	       ylab = "multiple myeloma 5-yr absolute risk (per 1000)",
	       las = 1, col = cols[predict.data$sex])

legend("topright", bty="n", levels(predict.data$sex), col= cols, pch=1)
```

## Example of `survfit.risk()` use

```
data(mgus)

# Mayo Clinic 20-35 year follow-up of patients with 
# monoclonal gammopathy of undetermined significance (MGUS)

# Hazard models of multiple myeloma, death, other plasma malignancy
# Time scale is days from MGUS diagnosis

surv1 <- survfit(Surv(time, status)~1,data=mgus2,subset=event=="myeloma")
surv2 <- survfit(Surv(time, status)~1,data=mgus2,subset=event=="death")
surv3 <- survfit(Surv(time, status)~1,data=mgus2,subset=event=="other")

# Absolute risk predictions for multiple myeloma consecutive biyearly intervals
bytwo <- seq(0,9,by=2)
intervals <- cbind(bytwo, bytwo+2)*365.25
risk <- survfit.risk(intervals[,1],intervals[,2], surv1, surv2, surv3)
risk

# How much do competing risks of death and other plasma malignancy reduce net risk?
netrisk <- survfit.risk(intervals[,1], intervals[,2], surv1)
netrisk

names(risk) <- paste("[",bytwo,", ",bytwo+2,")",sep="")

dotchart(risk*100, xlim = range(c(risk, netrisk)*100), 
                    xlab = "risk of multiple myeloma progression (%)",
                    ylab = "projection interval")
                    
points(x=netrisk*100, y=1:length(risk), col="red")

legend("topleft",bty="n",c("Net Risk", "Abs. Risk"), pch=1,col = c("red", "black"))


# How does this compare with cumulative risk?
intervals <- cbind(rep(0, length(bytwo)), bytwo+2)*365.25
risk <- survfit.risk(intervals[,1], intervals[,2], surv1, surv2, surv3)
netrisk <- survfit.risk(intervals[,1], intervals[,2], surv1)

names(risk) <- paste("[0, ",bytwo+2,")",sep="")

dotchart(risk*100, xlim = range(c(risk, netrisk)*100), 
                    xlab = "risk of multiple myeloma progression (%)",
                    ylab = "projection interval")
                    
points(x=netrisk*100, y=1:length(risk), col="red")

legend("topleft",bty="n",c("Net Risk", "Abs. Risk"), pch=1,col = c("red", "black"))

### STRATIFIED BY GENDER

surv1 <- survfit(Surv(time, status)~1,data=mgus2,subset=event=="myeloma"&sex=="male")
surv2 <- survfit(Surv(time, status)~1,data=mgus2,subset=event=="death"&sex=="male")
surv3 <- survfit(Surv(time, status)~1,data=mgus2,subset=event=="other"&sex=="male")

# 5-year, 10-year RISK OF MULTIPLE MYELOMA
intervals <- cbind(c(0,0), c(5,10))*365.25
male.risk <- survfit.risk(intervals[,1], intervals[,2], surv1, surv2, surv3)

male.risk
```
