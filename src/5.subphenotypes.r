
options(stringsAsFactors = FALSE);

library(dplyr);



# ===============================================================================================
# load
# 

load("data/preprocessing.20221204.rdata");





# ===============================================================================================
# clustering
# 

n.famd <- 14;
n.sp   <- 4;



# -----------------------------------------------------------------------------------------------
# hclust
# 

# optimal number of clusters
set.seed(0);
nbclust.hclust.famd <- NbClust::NbClust(
    data=d.famd[, 1:n.famd], 
    method="ward.D", 
    distance="euclidean", 
    index="all"
);
dev.off();

# construct hierarchical clustering
dist.famd <- dist(d.famd[, 1:n.famd]);
set.seed(0);
hclust.famd <- hclust(dist.famd, method="ward.D");

# extract clusters
sp.hclust.famd <- cutree(tree=hclust.famd, k=n.sp);
sp.hclust.famd <- factor(x=sp.hclust.famd, levels=c(2, 1, 3, 4), labels=1:n.sp);

# centers
centers.hclust.famd <- data.frame(
    clust=sp.hclust.famd,
    d.famd[, 1:n.famd]
);
centers.hclust.famd <- group_by(centers.hclust.famd, clust);
centers.hclust.famd <- summarise_all(centers.hclust.famd, mean);
centers.hclust.famd <- as.data.frame(centers.hclust.famd);



# -----------------------------------------------------------------------------------------------
# hclust - cc
# 

B <- 1000;
hclust.famd.cc <- array(NA, dim=c(nrow(d.famd), B, 1, 1));
for (b in 1:B) {
    set.seed(b);
    sel.cc.b <- sample(x=1:nrow(d.famd), size=round(nrow(d.famd)*0.9, digits=0), replace=F);
    dist.famd.cc.b <- dist(d.famd[sel.cc.b, 1:n.famd]);
    set.seed(b);
    hclust.famd.cc.b <- hclust(dist.famd.cc.b, method="ward.D");
    hclust.famd.cc[sel.cc.b, b, 1, 1] <- cutree(tree=hclust.famd.cc.b, k=n.sp);
}
dimnames(hclust.famd.cc)[[2]] <- paste("R", 1:B, sep="");
dimnames(hclust.famd.cc)[[3]] <- "HC_Euclidean";
dimnames(hclust.famd.cc)[[4]] <- "4";

sp.hclust.famd.cc <- as.vector(diceR::consensus_combine(hclust.famd.cc, element = "class")[[1]]);
sp.hclust.famd.cc <- factor(x=sp.hclust.famd.cc, levels=1:n.sp);

# centers
centers.hclust.famd.cc <- data.frame(
    clust=sp.hclust.famd.cc,
    d.famd[, 1:n.famd]
);
centers.hclust.famd.cc <- group_by(centers.hclust.famd.cc, clust);
centers.hclust.famd.cc <- summarise_all(centers.hclust.famd.cc, mean);
centers.hclust.famd.cc <- as.data.frame(centers.hclust.famd.cc);



# -----------------------------------------------------------------------------------------------
# kmeans
# 

# optimal number of clusters
set.seed(0);
nbclust.kmeans.famd <- NbClust::NbClust(
    data=d.famd[, 1:n.famd], 
    method="kmeans", 
    distance="euclidean", 
    index="all"
);
dev.off();

# construct kmeans clustering
# - check: I'm sure giving centroids from hierarchical clustering is acceptable.
set.seed(0);
kmeans.famd <- kmeans(x=d.famd[, 1:n.famd], centers=n.sp);

# extract clusters
sp.kmeans.famd <- kmeans.famd$cluster;
sp.kmeans.famd <- factor(x=sp.kmeans.famd, levels=c(4, 2, 3, 1), labels=1:n.sp);
names(sp.kmeans.famd) <- NULL;



# -----------------------------------------------------------------------------------------------
# kmeans
# 

B <- 1000;
kmeans.famd.cc <- array(NA, dim=c(nrow(d.famd), B, 1, 1));
for (b in 1:B) {
    cat(".");
    set.seed(b);
    sel.cc.b <- sample(x=1:nrow(d.famd), size=round(nrow(d.famd)*0.9, digits=0), replace=F);
    set.seed(b);
    kmeans.famd.cc.b <- kmeans(d.famd[sel.cc.b, 1:n.famd], centers=n.sp, nstart = 100);
    kmeans.famd.cc[sel.cc.b, b, 1, 1] <- kmeans.famd.cc.b$cluster;
}
dimnames(kmeans.famd.cc)[[2]] <- paste("R", 1:B, sep="");
dimnames(kmeans.famd.cc)[[3]] <- "KM";
dimnames(kmeans.famd.cc)[[4]] <- "4";

sp.kmeans.famd.cc <- as.vector(diceR::consensus_combine(kmeans.famd.cc, element = "class")[[1]]);
sp.kmeans.famd.cc <- factor(x=sp.kmeans.famd.cc, levels=1:n.sp);





# ===============================================================================================
# save
# 

save(
    hclust.famd, sp.hclust.famd, 
    kmeans.famd, sp.kmeans.famd, 
    hclust.famd.cc, sp.hclust.famd.cc, 
    kmeans.famd.cc, sp.kmeans.famd.cc, 
    file="data/subphenotypes.20221204.rdata"
);









