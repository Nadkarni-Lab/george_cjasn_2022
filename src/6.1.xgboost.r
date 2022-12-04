
options(stringsAsFactors = FALSE);

library(dplyr);



# ===============================================================================================
# load
# 

load("data/preprocessing.20221204.rdata");
load("data/subphenotypes.20221204.rdata");

n.famd <- 14;
n.sp   <- 4;





# ===============================================================================================
# shap diagram
# 

m.x.xgboost <- d.study.cm[, l.famd]
m.x.xgboost <- model.matrix(~ ., m.x.xgboost)[, -1];

d.xgboost <- data.frame(
    sp = sp.hclust.famd,
    m.x.xgboost
);

write.csv(d.xgboost, file="data/xgboost.20221204.csv", row.names=F);









