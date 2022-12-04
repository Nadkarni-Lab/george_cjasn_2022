
options(stringsAsFactors = FALSE);

library(dplyr);



# ===============================================================================================
# load
# 

load("data/extraction.20221204.rdata");





# ===============================================================================================
# study design
# - Inclusion                                  + 1603 = 1603
# - Exclusion
#   No match_id                                -   65 = 1538
#   Non-AKI patients                           -  769 =  769
#   More than 25% of missing lab measurements  -   21 =  748

# initial
d.study <- d.ext;
dim(d.study);
## [1] 1603   72

# exclude pe.il4
d.study <- select(d.study, -pi.il4);
dim(d.study);
## [1] 1603   71

# select patients w/ match_id
d.study <- filter(d.study, mid!="");
dim(d.study);
## [1] 1538   71

# select aki patients
d.study <- filter(d.study, hx.aki=="Y");
dim(d.study);
## [1] 769  71

# select patients w/ less than 25% of missing lab measurements
l.study <- colnames(d.study);
l.labs <- l.study[ grepl(x=l.study, pattern="^(pe|in|ca|pi|ua)[.]") ];

sel <- rowSums(is.na(d.study[, l.labs])) < (length(l.labs)*0.25);
d.study <- filter(d.study, sel);
dim(d.study);
## [1] 748  71





# ===============================================================================================
# save
# 

save(
    d.study, 
    file="data/cohort.20221204.rdata"
);









