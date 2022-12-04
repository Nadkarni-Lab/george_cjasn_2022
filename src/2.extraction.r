
options(stringsAsFactors = FALSE);

library(dplyr);



# ===============================================================================================
# load
# 

load("data/assess_aki.20221204.rdata");





# ===============================================================================================
# construct preliminary data
# 
# 

# -----------------------------------------------------------------------------------------------
# rename
# 

# demo
d.demo.v0 <- select(d.demo, 
    sid          = subj, 
    mid          = match_id, 
    center       = center, 
    admit_dt     = V0_date, 
    disch_dt     = V0_hosp_discharge_date, 
    de.age       = age, 
    de.gender    = gender, 
    de.black     = black, 
    de.race      = race, 
    de.ethnicity = ethnicity, 
    de.smoker    = smoker, 
    hx.icu       = ICU, 
    hx.ckd       = V0_CKD, 
    hx.aki       = V0_AKI, 
    hx.aki_st    = V0_AKIN_stage,
    hx.htn       = Hypertension, 
    hx.dm        = diabetes, 
    hx.cld       = CLD, 
    hx.cvd       = CVD, 
    hx.chf       = CHF, 
    hx.copd      = COPD, 
    hx.sepsis    = sepsis, 
    hx.lupus     = lupus,
    pe.cr.bl     = baseline_creatinine, 
    pe.gfr.bl    = baseline_GFR,
    pe.cr.v0     = V0_creatinine, 
    pe.gfr.v0    = V0_GFR
);
d.demo.v0 <- arrange(d.demo.v0, sid);

# inflamm
d.inflamm.v0 <- filter(d.inflamm, visit=="V0");
d.inflamm.v0 <- select(d.inflamm.v0, 
    sid      = subj, 
    in.cysc  = CystatinC, 
    in.pth   = PTH, 
    in.po4   = Phos, 
    in.crp   = CRP,
    in.fgf23 = FGF23, 
    in.st2   = ST2, 
    in.gal3  = GAL_3
);

# cardiac
d.cardiac.v0 <- filter(d.cardiac, visit=="V0");
d.cardiac.v0 <- select(d.cardiac.v0, 
    sid          = subj, 
    ca.nt_probnt = PBNP_NT, 
    ca.tropt     = Troponin
);

# proinfl
d.proinfl.v0 <- filter(d.proinfl, visit=="V0");
d.proinfl.v0 <- select(d.proinfl.v0, 
    sid        = subj, 
    pi.ifng    = IFNg, 
    pi.il1b    = IL1b, 
    pi.il2     = IL2, 
    pi.il4     = IL4, 
    pi.il6     = IL6, 
    pi.il8     = IL8, 
    pi.il10    = IL10, 
    pi.il12p70 = IL12p70, 
    pi.il13    = IL13, 
    pi.tnfa    = TNFa, 
    pi.tnfr1   = TNF_RI, 
    pi.tnfr2   = TNF_RII
);

# urine
d.urine.v0 <- filter(d.urine, visit=="V0");
d.urine.v0 <- select(d.urine.v0, 
    sid      = subj, 
    ua.il18  = uIL18, 
    ua.kim1  = uKIM1, 
    ua.mcp1  = uMCP1, 
    ua.ykl40 = uYKL40,
    ua.ngal  = uNGAL, 
    ua.umod  = uUMOD, 
    ua.cysc  = uCystatinC, 
    ua.osm   = uOsmolality, 
    ua.cr    = uCreatinine, 
    ua.prot  = uProtein, 
    ua.alb   = uAlbumin
);

# death
d.fu.death.v0 <- select(d.fu.death, 
    sid         = subj, 
    fu.death    = Event_death, 
    fu.death_dt = Event_death_date
);

# cvd
d.fu.cvd.v0 <- select(d.fu.outcomes,
    sid       = subj, 
    fu.mi     = Event_B_adjudicated,
    fu.hf     = Event_C_adjudicated,
    fu.tia    = Event_D_adjudicated,
    fu.ich    = Event_E_adjudicated,
    fu.pvd    = Event_F,
    fu.cabg   = Event_G,
    fu.pci    = Event_H,
    fu.par    = Event_I,
    fu.car    = Event_J,
    fu.cvd_dt = date
);

# gfr
d.fu.gfr.v0 <- select(d.fu.serum, 
    sid       = subj, 
    visit     = visit,
    fu.gfr    = eGFR_creatinine, 
    fu.gfr_dt = visit_date
);
d.fu.gfr.v0 <- filter(d.fu.gfr.v0, !is.na(fu.gfr));

# ckd - incidence
d.fu.ckd_i.v0 <- select(d.fu.renal, 
    sid         = subj, 
    fu.ckd_i    = Event_CKD_incidence, 
    fu.ckd_i_dt = Event_CKD_incidence_date
);
d.fu.ckd_i.v0 <- filter(d.fu.ckd_i.v0, !is.na(fu.ckd_i));

# ckd - progression
d.fu.ckd_p.v0 <- select(d.fu.renal, 
    sid         = subj, 
    fu.ckd_p    = Event_CKD_progression, 
    fu.ckd_p_dt = Event_CKD_progression_date
);
d.fu.ckd_p.v0 <- filter(d.fu.ckd_p.v0, !is.na(fu.ckd_p));

# esrd
d.fu.esrd.v0 <- select(d.fu.renal, 
    sid        = subj, 
    fu.esrd    = Event_ESRD, 
    fu.esrd_dt = Event_ESRD_date
);

# kidney transplant
d.fu.kt.v0 <- select(d.fu.outcomes, 
    sid      = subj, 
    fu.kt    = Event_N, 
    fu.kt_dt = date
);
d.fu.kt.v0 <- filter(d.fu.kt.v0, !is.na(fu.kt));

# dialysis
d.fu.dialysis.v0 <- select(d.fu.outcomes, 
    sid            = subj, 
    fu.dialysis    = Event_O, 
    fu.dialysis_dt = date
);

d.fu.bmi.v0 <- select(d.fu.life,
    sid    = subj,
    visit  = visit,
    pe.bmi = BMI
);



# -----------------------------------------------------------------------------------------------
# minor adjustment
# 

# * pull data from follow-up *
d.bmi.v0 <- filter(d.fu.bmi.v0, visit==1);
d.bmi.v0 <- select(d.bmi.v0, -visit);


# * drop some levels *

# demo
d.demo.v0$de.race  [!(d.demo.v0$de.race  %in%c("W", "B", "O"))] <- "O";
d.demo.v0$de.smoker[!(d.demo.v0$de.smoker%in%c("N", "E", "C"))] <- "N";
d.demo.v0$hx.ckd   [!(d.demo.v0$hx.ckd   %in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.aki   [!(d.demo.v0$hx.aki   %in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.htn   [!(d.demo.v0$hx.htn   %in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.dm    [!(d.demo.v0$hx.dm    %in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.cld   [!(d.demo.v0$hx.cld   %in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.cvd   [!(d.demo.v0$hx.cvd   %in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.chf   [!(d.demo.v0$hx.chf   %in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.copd  [!(d.demo.v0$hx.copd  %in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.sepsis[!(d.demo.v0$hx.sepsis%in%c("N", "Y"     ))] <- "N";
d.demo.v0$hx.lupus [!(d.demo.v0$hx.lupus %in%c("N", "Y"     ))] <- "N";

d.demo.v0 <- mutate(d.demo.v0,
    de.race   = factor(x=de.race  , levels=c("W", "B", "O")),
    de.smoker = factor(x=de.smoker, levels=c("N", "E", "C")),
    hx.ckd    = factor(x=hx.ckd   , levels=c("N", "Y"     )),
    hx.aki    = factor(x=hx.aki   , levels=c("N", "Y"     )),
    hx.htn    = factor(x=hx.htn   , levels=c("N", "Y"     )),
    hx.dm     = factor(x=hx.dm    , levels=c("N", "Y"     )),
    hx.cld    = factor(x=hx.cld   , levels=c("N", "Y"     )),
    hx.cvd    = factor(x=hx.cvd   , levels=c("N", "Y"     )),
    hx.chf    = factor(x=hx.chf   , levels=c("N", "Y"     )),
    hx.copd   = factor(x=hx.copd  , levels=c("N", "Y"     )),
    hx.sepsis = factor(x=hx.sepsis, levels=c("N", "Y"     )),
    hx.lupus  = factor(x=hx.lupus , levels=c("N", "Y"     ))
);


# * composite variables *

# cvd
d.fu.cvd.v0 <- rowwise(d.fu.cvd.v0);
d.fu.cvd.v0 <- mutate(d.fu.cvd.v0, fu.cvd = any(c_across(fu.mi:fu.car),na.rm=T)*1L);
d.fu.cvd.v0 <- as.data.frame(d.fu.cvd.v0);
d.fu.cvd.v0 <- relocate(d.fu.cvd.v0, fu.cvd_dt, .after=last_col());

# * survival outcomes *

# cvd
d.fu.cvd.v0 <- group_by(d.fu.cvd.v0, sid);
d.fu.cvd.v0 <- summarise(d.fu.cvd.v0, 
    fu.cvd_dt = if ( max(fu.cvd)==1 ) { head(fu.cvd_dt[which(fu.cvd==1)], 1) } else { tail(fu.cvd_dt[which(fu.cvd==0)], 1) },
    fu.cvd    = max(fu.cvd)
);
d.fu.cvd.v0 <- relocate(d.fu.cvd.v0, fu.cvd_dt, .after=last_col());
d.fu.cvd.v0 <- as.data.frame(d.fu.cvd.v0);

# ckd - incidence
d.fu.ckd_i.v0 <- group_by(d.fu.ckd_i.v0, sid);
d.fu.ckd_i.v0 <- summarise(d.fu.ckd_i.v0, 
    fu.ckd_i_dt = if ( max(fu.ckd_i)==1 ) { head(fu.ckd_i_dt[which(fu.ckd_i==1)], 1) } else { tail(fu.ckd_i_dt[which(fu.ckd_i==0)], 1) },
    fu.ckd_i    = max(fu.ckd_i)
);
d.fu.ckd_i.v0 <- relocate(d.fu.ckd_i.v0, fu.ckd_i_dt, .after=last_col());
d.fu.ckd_i.v0 <- as.data.frame(d.fu.ckd_i.v0);

# ckd - progression
d.fu.ckd_p.v0 <- group_by(d.fu.ckd_p.v0, sid);
d.fu.ckd_p.v0 <- summarise(d.fu.ckd_p.v0, 
    fu.ckd_p_dt = if ( max(fu.ckd_p)==1 ) { head(fu.ckd_p_dt[which(fu.ckd_p==1)], 1) } else { tail(fu.ckd_p_dt[which(fu.ckd_p==0)], 1) },
    fu.ckd_p    = max(fu.ckd_p)
);
d.fu.ckd_p.v0 <- relocate(d.fu.ckd_p.v0, fu.ckd_p_dt, .after=last_col());
d.fu.ckd_p.v0 <- as.data.frame(d.fu.ckd_p.v0);

# esrd
d.fu.esrd.v0 <- group_by(d.fu.esrd.v0, sid);
d.fu.esrd.v0 <- summarise(d.fu.esrd.v0, 
    fu.esrd_dt = if ( max(fu.esrd)==1 ) { head(fu.esrd_dt[which(fu.esrd==1)], 1) } else { tail(fu.esrd_dt[which(fu.esrd==0)], 1) },
    fu.esrd    = max(fu.esrd)
);
d.fu.esrd.v0 <- relocate(d.fu.esrd.v0, fu.esrd_dt, .after=last_col());
d.fu.esrd.v0 <- as.data.frame(d.fu.esrd.v0);

# 3 month gfr
d.fu.gfr.v0 <- filter(d.fu.gfr.v0, visit==1);
d.fu.gfr.v0 <- select(d.fu.gfr.v0, -visit);


# * drop values w/ negative numbers *

# inflamm
l.inflamm <- colnames(d.inflamm.v0);
l.inflamm <- l.inflamm[ grepl(pattern="^in[.]", l.inflamm) ];
d.inflamm.v0 <- mutate_at(d.inflamm.v0, l.inflamm, function(x) { ifelse(test=x>=0, yes=x, no=NA)});

# cardiac
l.cardiac <- colnames(d.cardiac.v0);
l.cardiac <- l.cardiac[ grepl(pattern="^ca[.]", l.cardiac) ];
d.cardiac.v0 <- mutate_at(d.cardiac.v0, l.cardiac, function(x) { ifelse(test=x>=0, yes=x, no=NA)});

# proinfl
l.proinfl <- colnames(d.proinfl.v0);
l.proinfl <- l.proinfl[ grepl(pattern="^pi[.]", l.proinfl) ];
d.proinfl.v0 <- mutate_at(d.proinfl.v0, l.proinfl, function(x) { ifelse(test=x>=0, yes=x, no=NA)});

# urine
l.urine <- colnames(d.urine.v0);
l.urine <- l.urine[ grepl(pattern="^ua[.]", l.urine) ];
d.urine.v0 <- mutate_at(d.urine.v0, l.urine, function(x) { ifelse(test=x>=0, yes=x, no=NA)});



# -----------------------------------------------------------------------------------------------
# merge
# 

d.ext <- left_join(x=d.demo.v0, y=d.bmi.v0     , by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.fu.death.v0, by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.fu.cvd.v0  , by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.fu.ckd_i.v0, by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.fu.ckd_p.v0, by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.fu.esrd.v0 , by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.fu.gfr.v0  , by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.cardiac.v0 , by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.inflamm.v0 , by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.proinfl.v0 , by=c("sid"));
d.ext <- left_join(x=d.ext  , y=d.urine.v0   , by=c("sid"));

d.ext <- select(d.ext,
    sid, mid, center, admit_dt, disch_dt, 
    de.age, de.gender, de.black, de.race, de.ethnicity, de.smoker, 
    hx.icu, hx.ckd, hx.aki, hx.aki_st, hx.htn, hx.dm, hx.cld, hx.cvd, hx.chf, hx.copd, hx.sepsis, hx.lupus, 
    pe.bmi, pe.cr.bl, pe.gfr.bl, pe.cr.v0, pe.gfr.v0, 
    ca.nt_probnt, ca.tropt, 
    in.cysc, in.pth, in.po4, in.crp, in.fgf23, in.st2, in.gal3, 
    pi.ifng, pi.il1b, pi.il2, pi.il4, pi.il6, pi.il8, pi.il10, pi.il12p70, pi.il13, pi.tnfa, pi.tnfr1, pi.tnfr2, 
    ua.il18, ua.kim1, ua.mcp1, ua.ykl40, ua.ngal, ua.umod, ua.cysc, ua.osm, ua.cr, ua.prot, ua.alb,
    fu.death, fu.death_dt, fu.cvd, fu.cvd_dt, fu.ckd_i, fu.ckd_i_dt, fu.ckd_p, fu.ckd_p_dt, fu.esrd, fu.esrd_dt, fu.gfr, fu.gfr_dt
);





# ===============================================================================================
# save
# 

save(
    d.demo.v0, d.inflamm.v0, d.cardiac.v0, d.proinfl.v0, d.urine.v0, d.bmi.v0, 
    d.fu.death.v0, d.fu.cvd.v0, d.fu.ckd_i.v0, d.fu.ckd_p.v0, d.fu.esrd.v0,
    d.ext, d.scr, d.meta, 
    file="data/extraction.20221204.rdata"
);









