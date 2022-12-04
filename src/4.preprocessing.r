
options(stringsAsFactors = FALSE);

library(dplyr);



# -----------------------------------------------------------------------------------------------
# load
# 

load("data/cohort.20221204.rdata");



# -----------------------------------------------------------------------------------------------
# derived variables
# 

d.study <- mutate(d.study,
    ua.acr = ua.alb / ua.cr,
    ua.pcr = ua.prot / ua.cr
);



# -----------------------------------------------------------------------------------------------
# 95% winsorization
# 

d.study.c <- d.study

l.cap <- colnames(d.study.c);
l.cap <- l.cap[ grepl(x=l.cap, pattern="^(pe|in|ca|pi|ua)[.]") ];
d.study.c <- mutate_at(d.study.c, l.cap, function(x) { ifelse(test=is.na(x) | x < quantile(x, .975, na.rm=T), yes=x, no=quantile(x, .975, na.rm=T)) });
d.study.c <- mutate_at(d.study.c, l.cap, function(x) { ifelse(test=is.na(x) | x > quantile(x, .025, na.rm=T), yes=x, no=quantile(x, .01 , na.rm=T)) });



# -----------------------------------------------------------------------------------------------
# skewed distribution
# 

# log transformation
l.log <- c(
    # measurements from demo
    "pe.bmi", "pe.cr.v0",
    # cardiac
    "ca.nt_probnt", "ca.tropt", 
    # inflamm
    "in.cysc", "in.pth", "in.crp", "in.fgf23", "in.st2", "in.gal3", 
    # proinfl
    "pi.ifng", "pi.il1b", "pi.il2", "pi.il6", "pi.il8", "pi.il10", 
    "pi.il12p70", "pi.il13", "pi.tnfa", "pi.tnfr1", "pi.tnfr2", 
    # urine
    "ua.il18", "ua.kim1", "ua.mcp1", "ua.ykl40", "ua.ngal", "ua.umod", 
    "ua.cysc", "ua.osm", "ua.prot", "ua.alb", "ua.acr", "ua.pcr"
);
d.study.cl <- d.study.c;
for(l in l.log) {
    d.study.cl[, l] <- log(d.study.cl[, l]);
    attr(d.study.cl[, l], "scaled:log") <- T;
};



# -----------------------------------------------------------------------------------------------
# scale
# 

l.scale <- colnames(d.study.cl);
l.scale <- l.scale[ grepl(x=l.scale, pattern="^(de|pe|in|ca|pi|ua)[.]") ];
l.scale <- setdiff(x=l.scale, y=c("de.gender", "de.black", "de.race", "de.ethnicity", "de.smoker"));

# normalization - robust scaler
robustscaler <- function(x, ...) {
    center <- median(x, ...);
    scale <- (quantile(x, .75, ...)-quantile(x, .25, ...));
    names(center) <- NULL;
    names(scale) <- NULL;
    mm <- (x-center) / scale;
    
    attr(mm, "scaled:center") <- center;
    attr(mm, "scaled:scale") <- scale;
    
    return( mm );
}
d.study.cln <- d.study.cl;
for (l in l.scale) {
    d.study.cln[, l] <- robustscaler(d.study.cln[, l], na.rm=T);
}



# -----------------------------------------------------------------------------------------------
# missing data imputation
# 

l.missing <- colnames(d.study.cl);
l.missing <- l.missing[ grepl(x=l.missing, pattern="^(de|hx|pe|in|ca|pi|ua)[.]") ];
l.missing <- setdiff(x=l.missing, "pe.bmi");

# normalization
d.study.clnm <- d.study.cln;
d.study.clnm[, l.missing] <- mice::complete(mice::mice(d.study.cln[, l.missing], m=5, maxit=50, meth='pmm',seed=500));
d.study.clnm$pe.bmi[ is.na(d.study.clnm$pe.bmi) ] <- mean(d.study.clnm$pe.bmi, na.rm=T);



# -----------------------------------------------------------------------------------------------
# 95% winsorization w/ missing data imputation
# 

d.study.cm <- d.study.clnm;
for (i in 1:ncol(d.study.cm)) {
    if ( !is.null(attr(d.study.cm[, i], "scaled:scale")) ) {
        d.study.cm[, i] <- d.study.cm[, i] * attr(d.study.cm[, i], "scaled:scale");
    }
    if ( !is.null(attr(d.study.cm[, i], "scaled:center")) ) {
        d.study.cm[, i] <- d.study.cm[, i] + attr(d.study.cm[, i], "scaled:center");
    }
    if ( !is.null(attr(d.study.cm[, i], "scaled:log")) ) {
        d.study.cm[, i] <- exp(d.study.cm[, i]);
    }
}



# -----------------------------------------------------------------------------------------------
# Dimensionality reduction
# 

l.famd <- colnames(d.study.clnm);
l.famd <- l.famd[ grepl(x=l.famd, pattern="^(de|hx|pe|in|ca|pi|ua)[.]") ];
l.famd <- setdiff(x=l.famd, y=c("de.black", "hx.aki", "pe.bmi"));

fit.famd <- FactoMineR::FAMD(d.study.clnm[, l.famd], ncp=length(l.famd), graph=F);
factoextra::get_eigenvalue(fit.famd)[, 1];
d.famd <- fit.famd$ind$coord;
colnames(d.famd) <- sprintf("PC%02d", 1:ncol(d.famd));



# -----------------------------------------------------------------------------------------------
# save
# 

save(
    d.study, d.study.c, d.study.cl, d.study.cln, d.study.clnm, d.study.cm, 
    fit.famd, d.famd, l.famd, 
    file="data/preprocessing.20221204.rdata"
);









