
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
# Visualization
# 

# -----------------------------------------------------------------------------------------------
# Dendrogram
# 

dend.famd <- as.dendrogram(hclust.famd);
dend.famd <- dendextend::color_branches(dend.famd, k=n.sp, col=ggsci::pal_jama()(n.sp), groupLabels=T);

dend.famd <- dendextend::set(
    dend  = dend.famd, 
    what  = "labels", 
    value = rep("", nrow(d.famd))
);

pdf(file="fig/dendrogram.hclust.pdf", width=5, height=5);
o.mar <- par(mar=c(0.25, 4.10, 0.25, 0.25));
plot(dend.famd, horiz=FALSE, ylab="Height");
par(o.mar);
dev.off();
png(file="fig/dendrogram.hclust.png", width=5, height=5, units="in", res=600);
o.mar <- par(mar=c(0.25, 4.10, 0.25, 0.25));
plot(dend.famd, horiz=FALSE, ylab="Height");
par(o.mar);
dev.off();



# -----------------------------------------------------------------------------------------------
# Consensus clustering
# 

pdf(file="fig/heatmap.consensus.hclust.pdf", width=5, height=5);
diceR::graph_heatmap(hclust.famd.cc);
dev.off();

png(file="fig/heatmap.consensus.hclust.png", width=5, height=5, units="in", res=600);
diceR::graph_heatmap(hclust.famd.cc);
dev.off();

pdf(file="fig/heatmap.consensus.kmeans.pdf", width=5, height=5);
diceR::graph_heatmap(kmeans.famd.cc)
dev.off()

png(file="fig/heatmap.consensus.kmeans.png", width=5, height=5, units="in", res=600);
diceR::graph_heatmap(kmeans.famd.cc)
dev.off()



# -----------------------------------------------------------------------------------------------
# umap
# 

library(ggplot2);
library(ggsci);

# hclust
set.seed(0);
umap.hclust.famd <- umap::umap(d.famd[, 1:n.famd])
umap.hclust.famd <- umap.hclust.famd$layout
umap.hclust.famd <- data.frame(
    sp = sp.hclust.famd,
    umap.hclust.famd
);
colnames(umap.hclust.famd) <- c("sp", "umap1", "umap2");

g.umap.hclust.famd <- ggplot(data=umap.hclust.famd, mapping=aes(x=umap2, y=umap1, colour=sp)) +
    geom_point(size=1) +
    scale_color_jama() +
    labs(x="UMAP 2", y="UMAP 1", colour="Subphenotype") +
    theme_bw() + 
    theme(legend.position="bottom");

ggsave(filename="fig/umap.hclust.pdf", device="pdf", plot=g.umap.hclust.famd, width=5, height=5, units="in", dpi=600);
ggsave(filename="fig/umap.hclust.png", device="png", plot=g.umap.hclust.famd, width=5, height=5, units="in", dpi=600);


# kmeans
set.seed(0);
umap.kmeans.famd <- umap::umap(d.famd[, 1:n.famd])
umap.kmeans.famd <- umap.kmeans.famd$layout
umap.kmeans.famd <- data.frame(
    sp = sp.kmeans.famd,
    umap.kmeans.famd
);
colnames(umap.kmeans.famd) <- c("sp", "umap1", "umap2");

g.umap.kmeans.famd <- ggplot(data=umap.kmeans.famd, mapping=aes(x=umap2, y=umap1, colour=sp)) +
    geom_point(size=1) +
    scale_color_jama() +
    labs(x="UMAP 2", y="UMAP 1", colour="Subphenotype") +
    theme_bw() + 
    theme(legend.position="bottom");

ggsave(filename="fig/umap.kmeans.pdf", device="pdf", plot=g.umap.hclust.famd, width=5, height=5, units="in", dpi=600);
ggsave(filename="fig/umap.kmeans.png", device="png", plot=g.umap.hclust.famd, width=5, height=5, units="in", dpi=600);



# -----------------------------------------------------------------------------------------------
# distribution - subphenotypes
# 

library(ggplot2);
library(gridExtra);
library(ggsci);

l.fname <- read.csv("data.0/fname.csv");

# continuous variable - boxplot
l.box <- colnames(d.study.c);
l.box <- l.box[ grepl(x=l.box, pattern="^(de|pe|in|ca|pi|ua)[.]") ];
l.box <- setdiff(x=l.box, y=c("de.gender", "de.black", "de.race", "de.ethnicity", "de.smoker"));

g.summary.box.list <- lapply(1:length(l.box), 
    function(i, lables, fnames, sp, df) {
        fname <- fnames$label[ fnames$variable==lables[i] ];
        d.box <- data.frame(sp=sp, value = df[, lables[i]]);
        g.box <- ggplot(data=d.box, mapping=aes(x=sp, y=value, colour=sp)) +
            geom_boxplot() + 
            scale_colour_jama() +
            labs(x="Subphenotype", y=fname, colour="Subphenotype") +
            theme_bw() + 
            theme(legend.position="none");
        return( g.box );
}, lables=l.box, fnames=l.fname, sp=sp.hclust.famd, df=d.study.c);
g.summary.boxs <- grid.arrange(grobs=g.summary.box.list, ncol=8);
ggsave(filename="fig/subphenotypes.summary.boxs.pdf", device="pdf", plot=g.summary.boxs, width=24, height=15, units="in", dpi=600);
ggsave(filename="fig/subphenotypes.summary.boxs.png", device="png", plot=g.summary.boxs, width=24, height=15, units="in", dpi=600);

# continuous variable - boxplot - log & normalized
l.box <- colnames(d.study.cln);
l.box <- l.box[ grepl(x=l.box, pattern="^(de|pe|in|ca|pi|ua)[.]") ];
l.box <- setdiff(x=l.box, y=c("de.gender", "de.black", "de.race", "de.ethnicity", "de.smoker"));

g.summary.box.ln.list <- lapply(1:length(l.box), 
    function(i, lables, fnames, sp, df) {
        fname <- fnames$label[ fnames$variable==lables[i] ];
        d.box <- data.frame(sp=sp, value = df[, lables[i]]);
        g.box <- ggplot(data=d.box, mapping=aes(x=sp, y=value, colour=sp)) +
            geom_boxplot() + 
            scale_colour_jama() +
            labs(x="Subphenotype", y=fname, colour="Subphenotype") +
            theme_bw() + 
            theme(legend.position="none");
        return( g.box );
}, lables=l.box, fnames=l.fname, sp=sp.hclust.famd, df=d.study.cln);
g.summary.boxs.ln <- grid.arrange(grobs=g.summary.box.ln.list, ncol=8);
ggsave(filename="fig/subphenotypes.summary.boxs.ln.pdf", device="pdf", plot=g.summary.boxs.ln, width=24, height=15, units="in", dpi=600);
ggsave(filename="fig/subphenotypes.summary.boxs.ln.png", device="png", plot=g.summary.boxs.ln, width=24, height=15, units="in", dpi=600);


# continuous variable - density plot
l.density <- colnames(d.study.c);
l.density <- l.density[ grepl(x=l.density, pattern="^(de|pe|in|ca|pi|ua)[.]") ];
l.density <- setdiff(x=l.density, y=c("de.gender", "de.black", "de.race", "de.ethnicity", "de.smoker"));

g.legend.sp <- ggplot(data=data.frame(sp=sp.hclust.famd, value=d.study.c$de.age), mapping=aes(x=value, colour=sp)) +
    geom_density() + 
    scale_colour_jama() +
    labs(x="Age", y="Density", colour="Subphenotype") +
    theme_bw() + 
    theme(legend.position="bottom");
g.legend.sp <- ggpubr::get_legend(g.legend.sp);
g.legend.sp <- ggpubr::as_ggplot(g.legend.sp);
g.summary.density.list <- lapply(1:length(l.density), 
    function(i, lables, fnames, sp, df) {
        fname <- fnames$label[ fnames$variable==lables[i] ];
        d.density <- data.frame(sp=sp, value = df[, lables[i]]);
        g.density <- ggplot(data=d.density, mapping=aes(x=value, colour=sp)) +
            geom_density() + 
            scale_colour_jama() +
            labs(x=fname, y="Density", colour="Subphenotype") +
            theme_bw() + 
            theme(legend.position="none");
        return( g.density );
}, lables=l.density, fnames=l.fname, sp=sp.hclust.famd, df=d.study.c);
g.summary.density.list[[length(g.summary.density.list)+1]] <- g.legend.sp;
g.summary.densities <- grid.arrange(grobs=g.summary.density.list, ncol=8);
ggsave(filename="fig/subphenotypes.summary.densities.pdf", device="pdf", plot=g.summary.densities, width=24, height=15, units="in", dpi=600);
ggsave(filename="fig/subphenotypes.summary.densities.png", device="png", plot=g.summary.densities, width=24, height=15, units="in", dpi=600);


# norminal variable - barplot
l.bar <- colnames(d.study.c);
l.bar <- l.bar[ grepl(x=l.bar, pattern="^(de|hx)[.]") ];
l.bar <- setdiff(x=l.bar, y=c("de.age", "de.black", "de.ethnicity", "hx.aki"));

g.summary.bar.list <- lapply(1:length(l.bar), 
    function(i, lables, fnames, sp, df) {
        fname <- fnames$label[ fnames$variable==lables[i] ];
        d.bar <- data.frame(sp=sp, value = df[, lables[i]]);
        d.bar <- dplyr::group_by(d.bar, sp, value);
        d.bar <- dplyr::summarise(d.bar, n=dplyr::n());
        d.bar <- as.data.frame(d.bar);
        g.bar <- ggplot(data=d.bar, mapping=aes(x=sp, y=n, fill=value)) +
            geom_bar(position=position_fill(reverse=TRUE), stat="identity") + 
            scale_y_continuous(labels=scales::percent) +
            scale_fill_nejm() +
            labs(x="Subphenotype", y=NULL, fill=fname) +
            theme_bw() + 
            theme(legend.position="bottom");
        return( g.bar );
}, lables=l.bar, fnames=l.fname, sp=sp.hclust.famd, df=d.study.c);
g.summary.bars <- grid.arrange(grobs=g.summary.bar.list, ncol=8);
ggsave(filename="fig/subphenotypes.summary.bars2.pdf", device="pdf", plot=g.summary.bars, width=24, height=6, units="in", dpi=600);
ggsave(filename="fig/subphenotypes.summary.bars.png", device="png", plot=g.summary.bars, width=24, height=6, units="in", dpi=600);



# -----------------------------------------------------------------------------------------------
# survival analysis - subphenotypes
# 

library(ggplot2);
library(survival);
library(survminer);

# KM - death
km.mort <- survfit(cox.mort, newdata=d.surv.n)
pdf(file="fig/km.mort.pdf", width=5, height=5);
ggsurvplot(
    fit          = km.mort,
    data         = d.surv.n,
    xlim         = c(0, 7),
    xlab         = "Follow-up time, year",
    conf.int     = TRUE, # Add confidence interval
    censor       = FALSE,
    palette      = "jama",
    ggtheme      = theme_bw(), # Change ggplot2 theme
    legend       = "bottom",
    legend.title = "Subphenotype",
    legend.labs  = 1:4
);
dev.off();
png(file="fig/km.mort.png", width=5, height=5, units="in", res=600);
ggsurvplot(
    fit          = km.mort,
    data         = d.surv.n,
    xlim         = c(0, 7),
    xlab         = "Follow-up time, year",
    conf.int     = TRUE, # Add confidence interval
    censor       = FALSE,
    palette      = "jama",
    ggtheme      = theme_bw(), # Change ggplot2 theme
    legend       = "bottom",
    legend.title = "Subphenotype",
    legend.labs  = 1:4
);
dev.off();


# KM - CVD
km.cvd <- survfit(cox.cvd, newdata=d.surv.n)
pdf(file="fig/km.cvd.pdf", width=5, height=5);
ggsurvplot(
    fit          = km.cvd,
    data         = d.surv.n,
    xlim         = c(0, 7),
    xlab         = "Follow-up time, year",
    conf.int     = TRUE, # Add confidence interval
    censor       = FALSE,
    palette      = "jama",
    ggtheme      = theme_bw(), # Change ggplot2 theme
    legend       = "bottom",
    legend.title = "Subphenotype",
    legend.labs  = 1:4
);
dev.off();
png(file="fig/km.cvd.png", width=5, height=5, units="in", res=600);
ggsurvplot(
    fit          = km.cvd,
    data         = d.surv.n,
    xlim         = c(0, 7),
    xlab         = "Follow-up time, year",
    conf.int     = TRUE, # Add confidence interval
    censor       = FALSE,
    palette      = "jama",
    ggtheme      = theme_bw(), # Change ggplot2 theme
    legend       = "bottom",
    legend.title = "Subphenotype",
    legend.labs  = 1:4
);
dev.off();


# KM - KD
km.kd <- survfit(cox.kd, newdata=d.surv.n)
pdf(file="fig/km.kd.pdf", width=5, height=5);
ggsurvplot(
    fit          = km.kd,
    data         = d.surv.n,
    xlim         = c(0, 7),
    xlab         = "Follow-up time, year",
    conf.int     = TRUE, # Add confidence interval
    censor       = FALSE,
    palette      = "jama",
    ggtheme      = theme_bw(), # Change ggplot2 theme
    legend       = "bottom",
    legend.title = "Subphenotype",
    legend.labs  = 1:4
);
dev.off();
png(file="fig/km.kd.png", width=5, height=5, units="in", res=600);
ggsurvplot(
    fit          = km.kd,
    data         = d.surv.n,
    xlim         = c(0, 7),
    xlab         = "Follow-up time, year",
    conf.int     = TRUE, # Add confidence interval
    censor       = FALSE,
    palette      = "jama",
    ggtheme      = theme_bw(), # Change ggplot2 theme
    legend       = "bottom",
    legend.title = "Subphenotype",
    legend.labs  = 1:4
);
dev.off();


# forest plot
library(forestplot);

get.fp.data <- function(object, categ, comp, n) {
    label <- data.frame(
        categ = categ,
        comp  = comp
    );
    data <- data.frame(
        exp(coef(object))[1:n],
        exp(confint(object))[1:n, , drop=F]
    );
    colnames(data) <- c("mean", "lower", "upper")
    rownames(data) <- NULL;
    
    fp <- data.frame(
        label,
        data,
        summary=F
    );
    
    return( fp );
}

fp.mort <- get.fp.data(
    object = cox.mort, 
    categ  = c("Mortality", NA, NA),
    comp = c("SP 1 vs. 2", "SP 3 vs. 2", "SP 4 vs. 2"),
    n = 3
);

fp.cvd <- get.fp.data(
    object = cox.cvd, 
    categ  = c("CVD events", NA, NA),
    comp = c("SP 1 vs. 2", "SP 3 vs. 2", "SP 4 vs. 2"),
    n = 3
);

fp.kd <- get.fp.data(
    object = cox.kd, 
    categ  = c("CKD outcomes", NA, NA),
    comp = c("SP 1 vs. 2", "SP 3 vs. 2", "SP 4 vs. 2"),
    n = 3
);

fp.a <- rbind(
    fp.mort,
    fp.cvd,
    fp.kd
)

png(filename="fig/fp_a.png", width=5, height=5, units="in", res=600);
forestplot(
    x = fp.a, 
    labeltext = c(categ, comp),
    hrzl_lines = list(
        "4" = gpar(lty=2, col="gray"),
        "7" = gpar(lty=2, col="gray")
        ),
    zero = 1,
    clip = c(0.01, 55),
    boxsize = 0.1,
    xlab = "HR",
    xlog=T
);
dev.off();

fp.mort.u <- get.fp.data(
    object = cox.mort.u, 
    categ  = c("Mortality", NA, NA),
    comp = c("SP 1 vs. 2", "SP 3 vs. 2", "SP 4 vs. 2"),
    n = 3
);

fp.cvd.u <- get.fp.data(
    object = cox.cvd.u, 
    categ  = c("CVD events", NA, NA),
    comp = c("SP 1 vs. 2", "SP 3 vs. 2", "SP 4 vs. 2"),
    n = 3
);

fp.kd.u <- get.fp.data(
    object = cox.kd.u, 
    categ  = c("CKD outcomes", NA, NA),
    comp = c("SP 1 vs. 2", "SP 3 vs. 2", "SP 4 vs. 2"),
    n = 3
);

fp.u <- rbind(
    fp.mort.u,
    fp.cvd.u,
    fp.kd.u
)

png(filename="fig/fp_u.png", width=5, height=5, units="in", res=600);
forestplot(
    x = fp.u, 
    labeltext = c(categ, comp),
    hrzl_lines = list(
        "4" = gpar(lty=2, col="gray"),
        "7" = gpar(lty=2, col="gray")
        ),
    zero = 1,
    clip = c(0.01, 55),
    boxsize = 0.1,
    xlab = "HR",
    xlog=T
);
dev.off();









