
options(stringsAsFactors = FALSE);

library(dplyr);
library(haven);



# -----------------------------------------------------------------------------------------------
# demographics
# 

d.demo <- read.csv(file="data.0/demographics.csv");

d.demo <- mutate(d.demo,
    center        = factor(x=center, levels=c(1:4), labels=c("Y", "V", "K", "W")),
    V0_AKIN_stage = factor(x=V0_AKIN_stage, levels=c(0:3)),
    gender        = factor(x=gender, levels=1:2, labels=c("M", "F")),
    race          = factor(x=race, levels=1:6, labels=c("A", "S", "B", "W", "P", "O")),
    ethnicity     = factor(x=ethnicity, levels=1:2, labels=c("H", "N")),
    smoker        = factor(x=smoker, levels=c(0, 1, 2, 98, 99), labels=c("N", "E", "C", "U", "R")),
    ICU           = factor(x=ICU, levels=c(0, 1), labels=c("N", "Y"))
);

d.demo <- mutate_at(
    d.demo, 
    c("V0_date", "V0_hosp_discharge_date", "V3M_date"),
    as.Date, format="%d%b%Y"
);

d.demo <- mutate_at(
    d.demo, 
    c("V0_CKD", "V0_AKI", "Hypertension", "diabetes", "CLD", "CVD", "CHF", "COPD", "sepsis", "lupus"),
    factor, levels=c(0, 1, 98), labels=c("N", "Y", "U")
);



# -----------------------------------------------------------------------------------------------
# adult_plasma_biomarkers
# 

d.inflamm <- read.csv(file="data.0/adult_plasma_biomarkers.csv");
d.inflamm <- mutate(d.inflamm,
    CRP   = ifelse(CRP_censored  =="No", CRP  , NA),
    FGF23 = ifelse(FGF23_censored=="No", FGF23, NA),
    GAL_3 = ifelse(GAL_3_censored=="No", GAL_3, NA)
);



# -----------------------------------------------------------------------------------------------
# adult_plasma_cardiac_biomarkers
# 

d.cardiac <- read.csv(file="data.0/adult_plasma_cardiac_biomarkers.csv");
d.cardiac <- mutate(d.cardiac, 
    PBNP_NT  = ifelse(PBNP_NT_censored =="No", PBNP_NT , NA),
    Troponin = ifelse(Troponin_censored=="No", Troponin, NA)
);



# -----------------------------------------------------------------------------------------------
# adult_plasma_proinfl_biomarkers
# 

d.proinfl <- read.csv(file="data.0/adult_plasma_proinfl_biomarkers.csv");
d.proinfl <- mutate(d.proinfl,
    IFNg    = ifelse(IFNg_censored   =="No", IFNg   , NA),
    IL10    = ifelse(IL10_censored   =="No", IL10   , NA),
    IL12p70 = ifelse(IL12p70_censored=="No", IL12p70, NA),
    IL13    = ifelse(IL13_censored   =="No", IL13   , NA),
    IL1b    = ifelse(IL1b_censored   =="No", IL1b   , NA),
    IL2     = ifelse(IL2_censored    =="No", IL2    , NA),
    IL4     = ifelse(IL4_censored    =="No", IL4    , NA),
    IL6     = ifelse(IL6_censored    =="No", IL6    , NA),
    IL8     = ifelse(IL8_censored    =="No", IL8    , NA),
    TNFa    = ifelse(TNFa_censored   =="No", TNFa   , NA),
    TNF_RI  = ifelse(TNF_RI_censored =="No", TNF_RI , NA),
    TNF_RII = ifelse(TNF_RII_censored=="No", TNF_RII, NA)
);



# -----------------------------------------------------------------------------------------------
# adult_urine_biomarkers
# 

d.urine <- read.csv(file="data.0/adult_urine_biomarkers.csv");
d.urine <- mutate(d.urine,
    uIL18      = ifelse(uIL18_censored     =="No", uIL18     , NA),
    uKIM1      = ifelse(uKIM1_censored     =="No", uKIM1     , NA),
    uMCP1      = ifelse(uMCP1_censored     =="No", uMCP1     , NA),
    uYKL40     = ifelse(uYKL40_censored    =="No", uYKL40    , NA),
    uNGAL      = ifelse(uNGAL_censored     =="No", uNGAL     , NA),
    uUMOD      = ifelse(uUMOD_censored     =="No", uUMOD     , NA),
    uCystatinC = ifelse(uCystatinC_censored=="No", uCystatinC, NA),
    uProtein   = ifelse(uProtein_censored  =="No", uProtein  , NA),
    uAlbumin   = ifelse(uAlbumin_censored  =="No", uAlbumin  , NA)
);



# -----------------------------------------------------------------------------------------------
# prev0_medications
# 

d.meds <- read.csv(file="data.0/prev0_medications.csv" );
d.meds <- mutate_at(
    d.meds, 
    c("drug_class_A", "drug_class_D", "drug_class_E", "drug_class_F", "drug_class_G", "drug_class_H", "drug_class_I", "drug_class_J", "drug_class_K", "drug_class_L", "drug_class_M", "drug_class_O"),
    factor, levels=c(0, 1, 98), labels=c("N", "Y", "U")
);



# -----------------------------------------------------------------------------------------------
# postv3m_renal_death
# 

d.fu.death <- read.csv(file="data.0/postv3m_renal_death.csv" );
d.fu.death <- dplyr::select(d.fu.death, subj, V0_date, V3M_date, Event_death, Event_death_date);
d.fu.death <- mutate_at(
    d.fu.death, 
    c("V0_date", "V3M_date", "Event_death_date"),
    as.Date, format="%d%b%Y"
);


d.fu.renal <- read.csv(file="data.0/postv3m_renal_death.csv" );
d.fu.renal <- dplyr::select(d.fu.renal, 
    subj, V0_date, V3M_date, 
    Event_CKD_incidence, Event_CKD_incidence_date, 
    Event_CKD_progression, Event_CKD_progression_date, 
    Event_ESRD, Event_ESRD_date,
    Event_ESRD_eGFRhalving, Event_ESRD_eGFRhalving_date
);
d.fu.renal <- mutate_at(
    d.fu.renal, 
    c("V0_date", "V3M_date", "Event_CKD_incidence_date", "Event_CKD_progression_date", "Event_ESRD_date", "Event_ESRD_eGFRhalving_date"),
    as.Date, format="%d%b%Y"
);



# -----------------------------------------------------------------------------------------------
# postv3m_recurrent_outcomes
# 

d.fu.outcomes <- read.csv(file="data.0/postv3m_recurrent_outcomes.csv");
d.fu.outcomes <- mutate_at(
    d.fu.outcomes, 
    c("V0_date", "V3M_date", "date"),
    as.Date, format="%d%b%Y"
);



# -----------------------------------------------------------------------------------------------
# postv3m_serum_urine
# 

d.fu.serum <- read.csv(file="data.0/postv3m_serum_urine.csv" );
d.fu.serum <- dplyr::select(d.fu.serum,
    subj, V0_date, V3M_date, visit_date, visit, 
    serum_creatinine, eGFR_creatinine, serum_CystatinC, eGFR_CystatinC, eGFR_creatinine_CystatinC
);
d.fu.serum <- mutate_at(
    d.fu.serum, 
    c("V0_date", "V3M_date", "visit_date"),
    as.Date, format="%d%b%Y"
);


d.fu.urine <- read.csv(file="data.0/postv3m_serum_urine.csv" );
d.fu.urine <- dplyr::select(d.fu.urine,
    subj, V0_date, V3M_date, visit_date, visit, 
    urine_creatinine, urine_albumin, urine_protein
);
d.fu.urine <- mutate_at(
    d.fu.urine, 
    c("V0_date", "V3M_date", "visit_date"),
    as.Date, format="%d%b%Y"
);



# -----------------------------------------------------------------------------------------------
# postv3m_lifestyle
# 

d.fu.life <- read.csv(file="data.0/postv3m_lifestyle.csv" );



# -----------------------------------------------------------------------------------------------
# inpatient_creatinine
# 

d.scr <- read_sas("../../data/assess_aki_2021/data.0/v0_inpatient_creatinine.sas7bdat");
d.scr <- as.data.frame(d.scr);

d.scr <- transmute(d.scr,
    subj     = subj,
    match_id = match_id, 
    center   = factor(x=center, levels=c(1:4), labels=c("Y", "V", "K", "W")),
    visit    = visit,
    scr      = V0_creatinine,
    scr_dt   = as.POSIXct(sprintf("%s %02d:%02d:00", V0_date, floor(V0_time), as.integer((V0_time%%1)*100) )),
    scr_i    = V0_collection_number
);



# -----------------------------------------------------------------------------------------------
# inpatient_creatinine
# 

d.meta <- read_sas("../../data/assess_aki_2021/data.0/p1_inpt_spec.sas7bdat")
d.meta <- as.data.frame(d.meta);





# -----------------------------------------------------------------------------------------------
# save
# 

save(
    d.demo, d.inflamm, d.cardiac, d.proinfl, d.urine, d.meds, 
    d.fu.death, d.fu.renal, d.fu.outcomes, d.fu.serum, d.fu.urine, d.fu.life, 
    d.scr, d.meta, 
    file="data/assess_aki.20221204.rdata"
);









