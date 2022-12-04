
options(stringsAsFactors = FALSE);

library(dplyr);



# -----------------------------------------------------------------------------------------------
# load
# 

load("data/preprocessing.20221204.rdata");
load("data/subphenotypes.20221204.rdata");
load("data/survival.20221204.rdata");

n.famd <- 14;
n.sp   <- 4;



# -----------------------------------------------------------------------------------------------
# statistics - table
# 

# center
stat.center <- arsenal::tableby(
    center ~ de.age + de.gender + de.race + de.smoker + 
    hx.icu + hx.ckd + hx.aki + hx.aki_st + hx.htn + hx.dm + hx.cld + hx.cvd + hx.chf + hx.copd + hx.sepsis + hx.lupus + 
    pe.bmi + pe.cr.bl + pe.gfr.bl + pe.cr.v0 + pe.gfr.v0 + 
    ca.nt_probnt + ca.tropt + 
    in.cysc + in.pth + in.po4 + in.crp + in.fgf23 + in.st2 + in.gal3 + 
    pi.ifng + pi.il1b + pi.il2 + pi.il6 + pi.il8 + pi.il10 + pi.il12p70 + pi.il13 + pi.tnfa + pi.tnfr1 + pi.tnfr2 + 
    ua.il18 + ua.kim1 + ua.mcp1 + ua.ykl40 + ua.ngal + ua.umod + ua.cysc + ua.osm + ua.cr + ua.prot + ua.alb + ua.acr + ua.pcr +
    fu.death + fu.cvd + fu.ckd_i + fu.ckd_p + fu.esrd, 
    data=d.study.c,
    control = arsenal::tableby.control(
        numeric.stats = c("Nmiss", "meansd", "meanCI", "medianq1q3", "range")
    )
);
arsenal::write2word(stat.center, file=paste(getwd(), "/doc/stat.center.doc", sep=""));


# hclust
d.sp <- data.frame(
    sp = sp.hclust.famd,
    d.study.c
);

stat.hclust <- arsenal::tableby(
    sp ~ de.age + de.gender + de.race + de.smoker + 
    hx.icu + hx.ckd + hx.aki + hx.aki_st + hx.htn + hx.dm + hx.cld + hx.cvd + hx.chf + hx.copd + hx.sepsis + hx.lupus + 
    pe.bmi + pe.cr.bl + pe.gfr.bl + pe.cr.v0 + pe.gfr.v0 + 
    ca.nt_probnt + ca.tropt + 
    in.cysc + in.pth + in.po4 + in.crp + in.fgf23 + in.st2 + in.gal3 + 
    pi.ifng + pi.il1b + pi.il2 + pi.il6 + pi.il8 + pi.il10 + pi.il12p70 + pi.il13 + pi.tnfa + pi.tnfr1 + pi.tnfr2 + 
    ua.il18 + ua.kim1 + ua.mcp1 + ua.ykl40 + ua.ngal + ua.umod + ua.cysc + ua.osm + ua.cr + ua.prot + ua.alb + ua.acr + ua.pcr +
    fu.death + fu.cvd + fu.ckd_i + fu.ckd_p + fu.esrd, 
    d.sp
);
arsenal::write2word(stat.hclust, file=paste(getwd(), "/doc/stat.hclust.doc", sep=""));


# hclust
d.sp <- data.frame(
    sp = sp.hclust.famd,
    d.study
);

stat.hclust <- arsenal::tableby(
    sp ~ 
    pe.bmi + pe.cr.bl + pe.gfr.bl + pe.cr.v0 + pe.gfr.v0 + 
    ca.nt_probnt + ca.tropt + 
    in.cysc + in.pth + in.po4 + in.crp + in.fgf23 + in.st2 + in.gal3 + 
    pi.ifng + pi.il1b + pi.il2 + pi.il6 + pi.il8 + pi.il10 + pi.il12p70 + pi.il13 + pi.tnfa + pi.tnfr1 + pi.tnfr2 + 
    ua.il18 + ua.kim1 + ua.mcp1 + ua.ykl40 + ua.ngal + ua.umod + ua.cysc + ua.osm + ua.cr + ua.prot + ua.alb + ua.acr + ua.pcr,
    d.sp,
    control = arsenal::tableby.control(
        numeric.stats = c("Nmiss", "meansd", "meanCI", "median", "medianq1q3", "iqr", "range"),
        digits=2
    )
);
arsenal::write2word(stat.hclust, file=paste(getwd(), "/doc/stat.hclust2.doc", sep=""));


# hclust
vs <- c("ca.nt_probnt", "ca.tropt", "in.cysc", "in.pth", "in.po4", "in.crp", "in.fgf23", "in.st2", "in.gal3", "pi.ifng", "pi.il1b", "pi.il2", "pi.il6", "pi.il8", "pi.il10", "pi.il12p70", "pi.il13", "pi.tnfa", "pi.tnfr1", "pi.tnfr2", "ua.il18", "ua.kim1", "ua.mcp1", "ua.ykl40", "ua.ngal", "ua.umod", "ua.cysc", "ua.osm", "ua.cr", "ua.prot", "ua.alb", "ua.acr", "ua.pcr");
d.sp2 <- data.frame(
    sp = sp.hclust.famd,
    d.study.cl[,vs]
)
for (v in vs) {
    d.sp2[, v] <- cut(x=d.sp2[, v], breaks=quantile(x=d.sp2[, v], c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm=T), labels=c("Q1", "Q2", "Q3", "Q4"), include.lowest=T)
}
stat.hclust <- arsenal::tableby(
    sp ~ 
    ca.nt_probnt + ca.tropt + 
    in.cysc + in.pth + in.po4 + in.crp + in.fgf23 + in.st2 + in.gal3 + 
    pi.ifng + pi.il1b + pi.il2 + pi.il6 + pi.il8 + pi.il10 + pi.il12p70 + pi.il13 + pi.tnfa + pi.tnfr1 + pi.tnfr2 + 
    ua.il18 + ua.kim1 + ua.mcp1 + ua.ykl40 + ua.ngal + ua.umod + ua.cysc + ua.osm + ua.cr + ua.prot + ua.alb + ua.acr + ua.pcr, 
    data = d.sp2,
);
arsenal::write2word(stat.hclust, file=paste(getwd(), "/doc/stat.hclust3.doc", sep=""));


# hclust
ls <- levels(sp.hclust.famd);
vs <- c("ca.nt_probnt", "ca.tropt", "in.cysc", "in.pth", "in.po4", "in.crp", "in.fgf23", "in.st2", "in.gal3", "pi.ifng", "pi.il1b", "pi.il2", "pi.il6", "pi.il8", "pi.il10", "pi.il12p70", "pi.il13", "pi.tnfa", "pi.tnfr1", "pi.tnfr2", "ua.il18", "ua.kim1", "ua.mcp1", "ua.ykl40", "ua.ngal", "ua.umod", "ua.cysc", "ua.osm", "ua.cr", "ua.prot", "ua.alb", "ua.acr", "ua.pcr");

dat <- d.study.cl[, vs[1]];
cut(x=dat, breaks=quantile(x=dat, c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm=T), include.lowest=T)

for(v in vs) {
    dat <- d.study.cl[, v];
    res1 <- sapply(ls, 
        function(l, ls, x) {
            x <- x[ls==l];
            qs <- cut(x=x, breaks=quantile(x=x, c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm=T), include.lowest=T);
            aggregate(x, list(qs), FUN=mean)[, 2];
    }, sp.hclust.famd, x=dat)
    res2 <- {
        qs <- cut(x=dat, breaks=quantile(x=dat, c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm=T), include.lowest=T);
        aggregate(dat, list(qs), FUN=mean)[, 2];
    }
    res <- cbind(res1, tot=res2);
    print(res);
}
stat.hclust <- arsenal::tableby(
    sp ~ de.age + de.gender + de.race + de.smoker + 
    hx.icu + hx.ckd + hx.aki + hx.aki_st + hx.htn + hx.dm + hx.cld + hx.cvd + hx.chf + hx.copd + hx.sepsis + hx.lupus + 
    pe.bmi + pe.cr.bl + pe.gfr.bl + pe.cr.v0 + pe.gfr.v0 + 
    ca.nt_probnt + ca.tropt + 
    in.cysc + in.pth + in.po4 + in.crp + in.fgf23 + in.st2 + in.gal3 + 
    pi.ifng + pi.il1b + pi.il2 + pi.il6 + pi.il8 + pi.il10 + pi.il12p70 + pi.il13 + pi.tnfa + pi.tnfr1 + pi.tnfr2 + 
    ua.il18 + ua.kim1 + ua.mcp1 + ua.ykl40 + ua.ngal + ua.umod + ua.cysc + ua.osm + ua.cr + ua.prot + ua.alb + ua.acr + ua.pcr +
    fu.death + fu.cvd + fu.ckd_i + fu.ckd_p + fu.esrd, 
    data = d.sp,
    control = arsenal::tableby.control(
        numeric.stats = c("Nmiss", "meansd", "meanCI", "medianq1q3", "range")
    )
);
arsenal::write2word(stat.hclust, file=paste(getwd(), "/doc/stat.hclust4.doc", sep=""));

# gender
d.sp <- data.frame(
    sp = sp.hclust.famd,
    d.study
);

stat.gender <- arsenal::tableby(
    de.gender ~ 
    pe.bmi + pe.cr.bl + pe.gfr.bl + pe.cr.v0 + pe.gfr.v0 + 
    ca.nt_probnt + ca.tropt + 
    in.cysc + in.pth + in.po4 + in.crp + in.fgf23 + in.st2 + in.gal3 + 
    pi.ifng + pi.il1b + pi.il2 + pi.il6 + pi.il8 + pi.il10 + pi.il12p70 + pi.il13 + pi.tnfa + pi.tnfr1 + pi.tnfr2 + 
    ua.il18 + ua.kim1 + ua.mcp1 + ua.ykl40 + ua.ngal + ua.umod + ua.cysc + ua.osm + ua.cr + ua.prot + ua.alb + ua.acr + ua.pcr,
    d.sp,
    control = arsenal::tableby.control(
        numeric.stats = c("meanCI", "Nmiss"),
        digits=2
    )
);
arsenal::write2word(stat.gender, file=paste(getwd(), "/doc/stat.gender.doc", sep=""));






