
options(stringsAsFactors = FALSE);

library(dplyr);



# ===============================================================================================
# load
# 

load("data/preprocessing.20221204.rdata");
load("data/subphenotypes.20221204.rdata");

n.famd <- 14;
n.sp   <- 4;
sp.hclust.famd <- relevel(sp.hclust.famd, "2");



# ===============================================================================================
# Survival data
# 

d.surv <- select(d.study.cm, admit_dt, fu.cvd, fu.cvd_dt, fu.ckd_i, fu.ckd_i_dt, fu.ckd_p, fu.ckd_p_dt, fu.esrd, fu.esrd_dt, fu.death, fu.death_dt);

d.surv <- mutate(d.surv,
    fu.cvd_tm   = as.numeric(difftime(fu.cvd_dt  , admit_dt, units="days"))/365.25,
    fu.ckd_i_tm = as.numeric(difftime(fu.ckd_i_dt, admit_dt, units="days"))/365.25,
    fu.ckd_p_tm = as.numeric(difftime(fu.ckd_p_dt, admit_dt, units="days"))/365.25,
    fu.esrd_tm  = as.numeric(difftime(fu.esrd_dt , admit_dt, units="days"))/365.25,
    fu.death_tm = as.numeric(difftime(fu.death_dt, admit_dt, units="days"))/365.25
);

d.surv <- mutate(d.surv,
    fu.kd     = ifelse(!is.na(fu.ckd_i), fu.ckd_i   , fu.ckd_p   ),
    fu.kd_tm  = ifelse(!is.na(fu.ckd_i), fu.ckd_i_tm, fu.ckd_p_tm),
    fu.cvd_tm = ifelse(!is.na(fu.cvd  ), fu.cvd_tm  , fu.esrd_tm ),
    fu.cvd    = ifelse(!is.na(fu.cvd  ), fu.cvd     , 0          )
);

d.surv <- mutate(d.surv,
    fu.cvd      = ifelse(fu.cvd_tm  >7 & fu.cvd==1  , 0, fu.cvd     ),
    fu.cvd_tm   = ifelse(fu.cvd_tm  >7              , 7, fu.cvd_tm  ),
    fu.kd       = ifelse(fu.kd_tm   >7 & fu.kd==1   , 0, fu.kd      ),
    fu.kd_tm    = ifelse(fu.kd_tm   >7              , 7, fu.kd_tm   ),
    fu.death    = ifelse(fu.death_tm>7 & fu.death==1, 0, fu.death   ),
    fu.death_tm = ifelse(fu.death_tm>7              , 7, fu.death_tm),
);

d.surv <- data.frame(
    select(d.surv, fu.cvd, fu.cvd_tm, fu.kd, fu.kd_tm, fu.death, fu.death_tm),
    sp = sp.hclust.famd, 
    select(d.study.cm, de.age, de.gender, de.race, de.smoker, hx.icu, hx.dm, hx.htn, hx.copd, hx.cvd, hx.sepsis, hx.ckd, pe.bmi, pe.gfr.v0, pe.gfr.bl, pe.cr.bl, pe.cr.v0, ua.prot, ua.acr, ua.pcr, in.crp)
);

d.surv.n <- data.frame(
    sp           = levels(d.surv$sp),
    de.age       = mean(d.surv$de.age),
    de.gender    = levels(d.surv$de.gender)[1],
    de.race      = levels(d.surv$de.race)[1],
    de.smoker    = levels(d.surv$de.smoker)[1],
    hx.dm        = levels(d.surv$hx.dm)[1],
    hx.copd      = levels(d.surv$hx.copd)[1],
    hx.cvd       = levels(d.surv$hx.cvd)[1],
    hx.ckd       = levels(d.surv$hx.ckd)[1],
    hx.sepsis    = levels(d.surv$hx.sepsis)[1],
    pe.bmi       = mean(d.surv$pe.bmi),
    pe.gfr.bl    = mean(d.surv$pe.gfr.bl),
    pe.gfr.v0    = mean(d.surv$pe.gfr.v0),
    pe.cr.v0     = mean(d.surv$pe.cr.v0),
    ua.acr       = mean(d.surv$ua.acr), 
    in.crp       = mean(d.surv$in.crp)
);



# -----------------------------------------------------------------------------------------------
# Survival analysis
# 

library(ggplot2);
library(survival);
library(survminer);

cox.mort.u <- coxph(
    Surv(fu.death_tm, fu.death) ~ sp, 
    data=d.surv
);

cox.mort <- coxph(
    Surv(fu.death_tm, fu.death) ~ sp + de.age + de.gender + hx.dm + hx.ckd + pe.bmi + ua.acr + pe.gfr.bl, 
    data=d.surv
);

cox.cvd.u <- coxph(
    Surv(fu.cvd_tm, fu.cvd) ~ sp,
    data=d.surv
);

cox.cvd <- coxph(
    Surv(fu.cvd_tm, fu.cvd) ~ sp + de.age + de.gender + hx.dm + hx.ckd + pe.bmi + ua.acr + pe.gfr.bl, 
    data=d.surv
);

cox.kd.u <- coxph(
    Surv(fu.kd_tm, fu.kd) ~ strata(hx.ckd) + sp,
    data=d.surv
);

cox.kd <- coxph(
    Surv(fu.kd_tm, fu.kd) ~ sp + de.age + de.gender + hx.dm + hx.ckd + pe.bmi + ua.acr + pe.gfr.bl, 
    data=d.surv
);



# -----------------------------------------------------------------------------------------------
# Save
# 

save(
    d.surv, d.surv.n,
    cox.mort.u, cox.mort, cox.cvd.u, cox.cvd, cox.kd.u, cox.kd, 
    file="data/survival.20221204.rdata"
)









