####################################################################
################# Workspace preparation ############################
####################################################################

#load packages
library(phytools)
library(psych)
library(GPArotation)
library(corpcor)
library(fields)
library(rstan)
library(brms)

#stan settings
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#these files can be downloaded at
# https://github.com/Jordan-Scott-Martin/SCCS_alloparental-care

#load data
setwd("")
allo_data<-read.csv("Martinetal_Dataset S1.csv")

#load phylogeny
phylo<-readRDS("SCCSphylo.RDS")

####################################################################
###################### Phylo PCA ###################################
####################################################################
# Climate PCA
{
climate <- #select and standardize variables for PCA
scale(subset(allo_data, select = temp.mean:precip.pred))
  
#organize for phylo PCA
climate<-as.matrix(climate)
rownames(climate)<-allo_data$socname

#phylo R package object
phylo_pca<-as.phylo(phylo)

#initial phylogenetic PCA
phyclim<-phyl.pca(phylo_pca, climate, mode="cov", method="lambda")
phyclim

#how many PCs to keep? (2)
pa<-fa.parallel(climate, fa="pc", n.iter=1000)

#rotate PCA loadings to enhance interpretation
rot<-GPForth(phyclim$L[,1:2], method="quartimax")
rot #loadings

#proportion of variance explained
#total
sum(apply(rot$loadings, 2, function(x) sum(x^2)/nrow(rot$loadings) ) )
#PC1
sum(rot$loadings[,1]^2)/nrow(rot$loadings)
#PC2
sum(rot$loadings[,2]^2)/nrow(rot$loadings)

#calculate ancestral means
C <- vcv.phylo(phylo)[rownames(climate), rownames(climate)]
temp <- phyl.vcv(climate, C, phyclim$lambda)
a <- t(temp$alpha)

#calculate rotated pPC scores (ancestral mean centering)
B<-t(pseudoinverse(rot$loadings))
scores <- data.frame((scale(climate, center=a, scale=FALSE) %*% B))

#organize scores
climatescore<-data.frame(socname=allo_data$socname,PC1=NA,PC2=NA)
rownames(climatescore)<-allo_data$socname
climatescore[rownames(climatescore) %in% rownames(phyclim$S),"PC1"]<-scores[,1]
climatescore[rownames(climatescore) %in% rownames(phyclim$S),"PC2"]<-scores[,2] 
}

#Starvation PCA (#safe to ignore warnings from parallel analysis)
{
starve <- #select and standardize variables for PCA
  scale(subset(allo_data, select = endstarve:seasonstarve),
          scale= apply(subset(allo_data, select = endstarve:seasonstarve), 2,
                       function(x) sd(x,na.rm=TRUE)))

rownames(starve)<-allo_data$socname

#remove missing rows
starve<-na.omit(starve)

#phylo PCA
phystarve<-phyl.pca(phylo_pca, starve, mode="cov", method="lambda")
phystarve #loadings (sign changed by *-1 so that higher scores = higher starvation)

#how many PCs to keep?
pa<-fa.parallel(starve, fa="pc", n.iter=1000) #warnings can be safely ignored

#proportion of variance explained
sum(phystarve$L[,1]^2)/3

#organize scores
starvescore<-data.frame(socname=allo_data$socname, PC3=NA)
rownames(starvescore)<-allo_data$socname

#add non-missing scores
#score*(-1) so that higher scores = higher starvation
starvescore[rownames(starvescore) %in% rownames(phystarve$S),"PC3"]<-phystarve$S[,1]*(-1)
}

#combine data
finaldata<-cbind(allo_data, PC1=climatescore$PC1, PC2=climatescore$PC2, PC3=starvescore$PC3)

####################################################################
######################### Prep variables ###########################
####################################################################

#standardize continous predictor for regression
finaldata$pathogen <- scale(finaldata$pathogen)

#standardize PC3, keep ancestral mean centering
#note that rotated PCs are already standardized
finaldata$PC3 <- finaldata$PC3/sd(finaldata$PC3, na.rm = TRUE)

#bin allocare to 'low' (0) and 'high' (1) rates
finaldata$bincare<-ifelse(finaldata$allomaternal<3,0,1)

#bin food storage to 'none' (1), 'present' (1)
finaldata$binstore<-as.factor(ifelse(finaldata$storage==1,"none","present"))

#phylo matrix
phylo_vcv<-vcv.phylo(phylo, cor=TRUE)

####################################################################
########################## Full models #############################
####################################################################

#Model set

#without random slopes
{
#main effects model
{
  #model structure
  
  #imputation model
  starve.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(fixity) + binstore + pathogen + 
                  PC1 + PC2 + (1|biome) + (1|subsistence), decomp = "QR") + gaussian()
  #main model
  main.m<-bf(bincare ~ mo(fathercare) + mo(fixity) + binstore + pathogen +
               mi(PC3) + PC1 + PC2 + (1|biome) + (1|subsistence) + (1|socname), decomp = "QR") +
    bernoulli()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="bincare"),
      prior(normal(0,1), class=b,resp="bincare"),
      prior(exponential(3), class=sd, resp="bincare"))
  
  #estimate model
  allo.m1 <-
    brm(formula= main.m + starve.mi  + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=99,
        control=list(adapt_delta=0.99, max_treedepth=10))

  summary(allo.m1, prob= 0.90)
  
  saveRDS(allo.m1,"allo_m1.RDS")
} 

#interaction  model
{
  #model structure
  
  #imputation model
  starve.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(fixity) + binstore + pathogen + 
                  PC1*PC2 + (1|biome) + (1|subsistence), decomp = "QR") + gaussian()
  #main model
  main.m<-bf(bincare ~ mo(fathercare) + mo(fixity) + binstore + pathogen +
               mi(PC3) + PC1*PC2 + (1|biome) + (1|subsistence) + (1|socname), decomp = "QR") +
    bernoulli()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="bincare"),
      prior(normal(0,1), class=b,resp="bincare"),
      prior(exponential(3), class=sd, resp="bincare"))
  
  #estimate model
  allo.m2 <-
    brm(formula= main.m + starve.mi  + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=99,
        control=list(adapt_delta=0.99, max_treedepth=10))

  summary(allo.m2, prob = 0.90)
  
  saveRDS(allo.m2,"allo_m2.RDS")
}

#response surface model
{
  #model structure
  
  #imputation model
  starve.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(fixity) + binstore + pathogen + 
                  PC1*PC2 + I(PC1^2) + I(PC2^2) + 
                  (1|biome) + (1|subsistence), decomp = "QR") + gaussian()
  #main model
  main.m<-bf(bincare ~ mo(fathercare) + mo(fixity) + binstore + pathogen +
               mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2)  +
               (1|biome) + (1|subsistence) + (1|socname), decomp = "QR") +
    bernoulli()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="bincare"),
      prior(normal(0,1), class=b,resp="bincare"),
      prior(exponential(3), class=sd, resp="bincare"))
  
  #estimate model
  allo.m3 <-
    brm(formula= main.m + starve.mi  + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=99,
        control=list(adapt_delta=0.99, max_treedepth=10))

  summary(allo.m3, prob = 0.9)
  
  saveRDS(allo.m3,"allo_m3.RDS")
}

#reduced model (check for overfitting)  
{
  #model structure
  
  #imputation model
  starve.mi<-bf(PC3|mi() ~ bincare + PC1*PC2 + I(PC1^2) + I(PC2^2), decomp = "QR") + gaussian()
  #main model
  main.m<-bf(bincare ~ mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2) + (1|socname), decomp = "QR") +
    bernoulli()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="bincare"),
      prior(normal(0,1), class=b,resp="bincare"),
      prior(exponential(3), class=sd, resp="bincare"))
  
  #estimate model
  allo.m4_of <-
    brm(formula= main.m + starve.mi  + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=99,
        control=list(adapt_delta=0.99, max_treedepth=10))

  summary(allo.m4_of, prob = 0.9)
  
  saveRDS(allo.m4_of,"allo_m4_of.RDS")
  
  #results
  fixef(allo.m4_of, robust=TRUE, probs=c(0.05, 0.95)) #robust fixed effect estimates
  hypothesis(allo.m4_of, "bincare_PC1  < 0", class="b")
  hypothesis(allo.m4_of, "bincare_PC2  > 0", class="b")
  hypothesis(allo.m4_of, "bincare_IPC1E2  < 0", class="b")
  hypothesis(allo.m4_of, "bincare_IPC2E2  < 0", class="b")
  hypothesis(allo.m4_of, "bincare_PC1:PC2  > 0", class="b")
  hypothesis(allo.m4_of, "bincare_miPC3  < 0", class="bsp")
  }
  
    
}  

#with random slopes
{
#main effects model
{
  #model structure
  
  #imputation model
  starve.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(fixity) + binstore + pathogen + 
                  PC1 + PC2 + (1|biome) + (PC1 + PC2||subsistence), decomp = "QR") + gaussian()
  #main model
  main.m<-bf(bincare ~ mo(fathercare) + mo(fixity) + binstore + pathogen +
               mi(PC3) + PC1 + PC2 + (1|biome) + (mi(PC3) + PC1 + PC2||subsistence) + (1|socname), decomp = "QR") +
    bernoulli()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="bincare"),
      prior(normal(0,1), class=b,resp="bincare"),
      prior(exponential(3), class=sd, resp="bincare"))
  
  #estimate model
  allo.m1_rs <-
    brm(formula= main.m + starve.mi  + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=99,
        control=list(adapt_delta=0.99, max_treedepth=10))
  
  saveRDS(allo.m1_rs,"allo_m1_rs.RDS")
} 

#interaction  model
{
  #model structure
  
  #imputation model
  starve.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(fixity) + binstore + pathogen + 
                  PC1*PC2 + (1|biome) + (PC1*PC2||subsistence), decomp = "QR") + gaussian()
  #main model
  main.m<-bf(bincare ~ mo(fathercare) + mo(fixity) + binstore + pathogen +
               mi(PC3) + PC1*PC2 + (1|biome) + (mi(PC3) + PC1*PC2||subsistence) +
               (1|socname), decomp = "QR") +
    bernoulli()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="bincare"),
      prior(normal(0,1), class=b,resp="bincare"),
      prior(exponential(3), class=sd, resp="bincare"))
  
  #estimate model
  allo.m2_rs <-
    brm(formula= main.m + starve.mi  + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=99,
        control=list(adapt_delta=0.99, max_treedepth=10))
  
  saveRDS(allo.m2_rs,"allo_m2_rs.RDS")
}

#response surface model
{
  #model structure
  
  #imputation model
  starve.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(fixity) + binstore + pathogen + 
                  PC1*PC2 + I(PC1^2) + I(PC2^2) + 
                  (1|biome) + (PC1*PC2 + I(PC1^2) + I(PC2^2)||subsistence), decomp = "QR") + gaussian()
  #main model
  main.m<-bf(bincare ~ mo(fathercare) + mo(fixity) + binstore + pathogen +
                mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2)  +
               (1|biome) + (mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2)||subsistence) + (1|socname), decomp = "QR") +
                bernoulli()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="bincare"),
      prior(normal(0,1), class=b,resp="bincare"),
      prior(exponential(3), class=sd, resp="bincare"))
  
  #estimate model
  allo.m3_rs <-
    brm(formula= main.m + starve.mi  + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=99,
        control=list(adapt_delta=0.99, max_treedepth=10))
  
  summary(allo.m3_rs, prob = 0.9)
  
  saveRDS(allo.m3_rs,"allo_m3_rs.RDS")
}
    
}

#load models
allo.m1 <- readRDS("allo_m1.RDS")
allo.m2 <- readRDS("allo_m2.RDS")
allo.m3 <- readRDS("allo_m3.RDS")
allo.m4_of <- readRDS("allo_m4_rs.RDS")
allo.m1_rs <- readRDS("allo_m1_rs.RDS")
allo.m2_rs <- readRDS("allo_m2_rs.RDS")
allo.m3_rs <- readRDS("allo_m3_rs.RDS")

#model comparison (note that values may differ trivially due to stochastic sampling)
compare_ic(WAIC(allo.m1,resp="bincare"), WAIC(allo.m2,resp="bincare"),
           WAIC(allo.m3,resp="bincare"), WAIC(allo.m4_of,resp="bincare"),
           WAIC(allo.m1_rs,resp="bincare"), WAIC(allo.m2_rs,resp="bincare"),
           WAIC(allo.m3_rs,resp="bincare"))

####################################################################
################# Final model results ##############################
####################################################################

#please note throughout that values may differ trivially from those reported
#in the main text or supplement due to stochastic sampling

#inspect m3
summary(allo.m3_rs, prob=0.90) #look for good R_hat and ESS
fixef(allo.m3_rs, robust=TRUE, probs=c(0.05, 0.95)) #robust fixed effect estimates

  #posterior probabilities
hypothesis(allo.m3_rs, "bincare_PC1  < 0", class="b")
hypothesis(allo.m3_rs, "bincare_PC2  > 0", class="b")
hypothesis(allo.m3_rs, "bincare_IPC1E2  < 0", class="b")
hypothesis(allo.m3_rs, "bincare_IPC2E2  > 0", class="b")
hypothesis(allo.m3_rs, "bincare_PC1:PC2  > 0", class="b")
hypothesis(allo.m3_rs, "bincare_miPC3  < 0", class="bsp")

hypothesis(allo.m3_rs, "bincare_binstorepresent  > 0", class="b")
hypothesis(allo.m3_rs, "bincare_mofixity  < 0", class="bsp")
hypothesis(allo.m3_rs, "bincare_pathogen  > 0", class="b")
hypothesis(allo.m3_rs, "bincare_mofathercare  > 0", class="bsp")

#get posterior samples
post <- posterior_samples(allo.m3_rs)

#robust random effect estimates
ref <- vars<-subset(post, select = c(sd_biome__bincare_Intercept:sd_subsistence__bincare_miPC3))

apply(ref, 2, median)
apply(ref, 2, mad)
apply(ref, 2, quantile, probs=c(0.05,0.95))


#Cohen's d
{logitsd<-pi/sqrt(3)
  
d_df<-data.frame(
    effect=c("PC1","PC2", "PC1^2",  "PC2^2",  "PC1*PC2",  "PC3",
             "binstore", "fixity", "pathogen", "fathercare"), d_median=NA, d_mad=NA)
  
vars<-subset(post, select = c(b_bincare_PC1:`b_bincare_PC1:PC2`,bsp_bincare_miPC3,b_bincare_binstorepresent,
                          bsp_bincare_mofixity,b_bincare_pathogen, bsp_bincare_mofathercare))
  
d_df$d_median<-apply(vars, 2, function(x) median(x/logitsd))
d_df$d_mad<-apply(vars, 2, function(x) mad(x/logitsd))

print(d_df) }

#predict probability difference between high, average, and low PC1 & PC2
#at average starvation, average covariates
#minimum monotonic category, marginalized over random effects
{
f <- function(x,y){
    
    #calculate posterior linear predictor
    post$b_bincare_Intercept +
    post$b_bincare_PC1*x +
    post$b_bincare_PC2*y +
    post$b_bincare_IPC1E2*x^2 +
    post$b_bincare_IPC2E2*y^2 +
    post$`b_bincare_PC1:PC2`*(x*y) } 
  
high <- logistic(f(1, 1)) #posterior probability for high PC1 & PC2
avg <- logistic(f(0,0)) #average
low  <- logistic(f(-1,-1)) #low
lowt_highp  <- logistic(f(-1,1)) #low PC1, high PC2
diff1<-low-avg #difference between these conditions
diff2<-low-high #difference between these conditions
diff3 <- low - lowt_highp

result <-  data.frame(condition=c("high", "average", "low", "lowt_highp",
                                  "low - average", "low - high","low - lowt_highp" ),
             prob_median=c(median(high), median(avg), median(low),median(lowt_highp),
                           median(diff1), median(diff2), median(diff3)),
             prob_mad=c(mad(high), mad(avg), mad(low),mad(lowt_highp),
                        mad(diff1), mad(diff2), mad(diff3)),
             diff_prob=c("-","-", "-","-",
                         sum(diff1>0)/length(diff1),sum(diff2>0)/length(diff2),
                         sum(diff3>0)/length(diff3)) )
print(result)
}

####################################################################
########################## Plots ###################################
####################################################################

library(reshape2)
library(ggplot2)
library(viridis)
library(cowplot)
library(tidybayes)
library(dplyr)
library(grid)
library(gridExtra) 

#Fig 1
{
#basic map
  mp <- NULL
  mapWorld <- borders("world", colour="grey85", fill="grey85") # create a layer of borders
  mp <- ggplot(data=finaldata) + mapWorld
  
  #add bincare
  allo_map<-
    mp + geom_point(aes(x=longitude, y=latitude,color=as.factor(bincare)), size=1.5)+
    scale_color_manual(values=c("#5215a1","#ffde24"))+
    theme_bw(base_size=18)+
    xlab("Longitude") +
    ylab("Latitude") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background=element_rect(fill="white"),
          plot.margin = unit(c(2,1,1,0.5), "cm"),
          axis.title.x = element_text(size=12,face="bold"),
          axis.title.y = element_text(size=12,face="bold"))+
    guides(color=FALSE)

#histogram
  allo_hist<-
    ggplot(finaldata, aes(x=allomaternal,fill=as.factor(bincare)))+geom_histogram(stat="count")+
    scale_fill_manual(values=c("#5215a1","#ffde24"))+
    theme_bw(base_size=18)+
    ylab("Frequency of Societies\n") +
    scale_y_continuous(expand=c(0,0), limits=c(0,80), labels=c("", "20", "40", "60", ""))+
    scale_x_continuous(expand=c(0,0),breaks=c(1,2,3,4,5,6),
                       labels=c("Almost exclusively mother",
                                "Principally mother, others minor roles",
                                "Principally mother, others important roles",
                                "Mother < 1/2 care",
                                "Mother minor but significant",
                                "Mother minimal except for nursing"))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background=element_rect(fill="white"),
          plot.margin = unit(c(2,0.5,1,0.5), "cm"),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_text(size=12,face="bold"),
          axis.text.x = element_text(size=10, angle=45,vjust=1,hjust=1))+
    guides(fill=FALSE)
  
  #combine
  library(cowplot)
  
  p.grid1<-plot_grid(allo_hist ,allo_map,ncol=2,
                     align="v",labels=c("A","B"),rel_widths=c(1,1.4))
  
  #save
  save_plot("Figure 1.tiff", p.grid1, compression="lzw",
            base_width = 10,base_height=5,
            dpi=600)
  #save
  save_plot("Figure 1.png", p.grid1,
            base_width = 10,base_height=5,
            dpi=600)
  
}

#Fig 2A
{
post <- posterior_samples(allo.m3_rs)
  
######################################################################
#low precipitation  
{
#raw data  
df.raw <- data.frame(socname=finaldata$socname, bincare = finaldata$bincare,
                     PC1 = finaldata$PC1, PC2 = finaldata$PC2)
df.raw$precip <- factor(ifelse(df.raw$PC2>0, "high","low"), levels=c("low","high"))
df.raw_low <- df.raw[df.raw$precip=="low",]
df.raw_high <- df.raw[df.raw$precip=="high",]

#model predicted slopes
#predict values (low precipitation)
seq<-seq(-2.6,1, by=0.02)
nsample=8000
  
newdata<-data.frame(PC1=seq, PC2=-1, PC3 = 0,
                      bincare=0,fathercare=1,fixity=1,
                      binstore="present", pathogen=0)
  
fitin<-fitted(allo.m3_rs, newdata=newdata,resp="bincare",
                nsamples=nsample, allo_new_levels=TRUE, re_formula=NA, summary=FALSE)
##
fitp<-data.frame(
    probability=c(fitin),
    temp=rep(seq, each=nsample),
    precip="low")

#low plot
levels=6

lowp.plot<-
  ggplot(fitp, aes(x = temp, y = probability, color=precip,fill=precip))+
  stat_lineribbon(size=2, .width = ppoints(levels), alpha=1/levels)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.1,1.1), breaks=c(-0.2,0,0.25,0.5,0.75,1))+
  scale_color_manual(values=c("#465b70"))+
  xlab("\nTemperature (pPC1)\n\n")+
  ylab("Probability of\n High Alloparental Care\n")+
  ggtitle("\nLow Precipitation (pPC2 -1)")+
  theme(plot.title =element_text(size=12,face="bold", hjust=0.5),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_text(size=12,face="bold"),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.line = element_line(size = 1),
        panel.border=element_rect(fill=NA,color="black", size=1, 
                                  linetype="solid"),
        panel.background= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1,0.5,0.5,0.5), "cm"))+
        guides(fill=FALSE, color=FALSE)+
        scale_fill_manual(values=c("#5215a1","#ffde24","#90b7de"))+
        geom_point(data=df.raw_low, aes(x=PC1,y = bincare + runif(length(df.raw_low$PC1),-0.05,0.05),
                                   fill=as.factor(bincare)),inherit.aes = FALSE, 
                   pch=21, size=3, alpha=0.6, stroke=1)
}
  
######################################################################
#high precipitation
{
#predict values (high precipitation)
seq<-seq(-4.2,1, by=0.02)
nsample=8000
  
newdata<-data.frame(PC1=seq, PC2=1, PC3 = 0,
                      bincare=0, fathercare=1,fixity=1,
                      binstore="present", pathogen=0)

fitin<-fitted(allo.m3_rs, newdata=newdata,resp="bincare",
                nsamples=nsample, allo_new_levels=TRUE, re_formula=NA, summary=FALSE)
##
fitp2<-data.frame(
    probability=c(fitin),
    temp=rep(seq, each=nsample),
    precip="high")

#low plot
levels=6

highp.plot<-
  ggplot(fitp2, aes(x = temp, y = probability, color=precip,fill=precip))+
  stat_lineribbon(size=2, .width = ppoints(levels), alpha=1/levels)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.1,1.1), breaks=c(0,0.25,0.5,0.75,1))+
  scale_color_manual(values=c("#465b70"))+
  xlab("\nTemperature (pPC1)\n\n")+
  ggtitle("\nHigh Precipitation (pPC2 +1)")+
  ylab("Probability of\n High Alloparental Care\n")+
  theme(plot.title =element_text(size=12, face="bold",hjust=0.5),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_text(size=12,face="bold"),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.line = element_line(size = 1),
        panel.border=element_rect(fill=NA,color="black", size=1, 
                                  linetype="solid"),
        panel.background= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1,0.5,0.5,0.5), "cm"))+
        guides(fill=FALSE, color=FALSE)+
        scale_fill_manual(values=c("#5215a1","#ffde24","#90b7de"))+
        geom_point(data=df.raw_high, aes(x=PC1,y = bincare + runif(length(df.raw_high$PC1),-0.05,0.05),
                                   fill=as.factor(bincare)),inherit.aes = FALSE, 
                   pch=21, size=3, alpha=0.6, stroke=1)
}

######################################################################
#starvation
{
#dataframe of raw values for plots
df.raw2 <- data.frame(bincare = finaldata$bincare, PC3 = finaldata$PC3)
df.raw2 <- na.omit(df.raw2)
  
#predict values (average climate)
seq<-seq(-2.1,2.1, by=0.02)
nsample=8000
  
newdata<-data.frame(PC1=0, PC2=0, PC3 = seq,
                      bincare=0,fathercare=1,fixity=1,
                      binstore="present", pathogen=0)
  
fitin<-fitted(allo.m3_rs, newdata=newdata,resp="bincare",
                nsamples=nsample, allo_new_levels=TRUE, re_formula=NA, summary=FALSE)
##
fitp3<-data.frame(
    probability=c(fitin),
    starve=rep(seq, each=nsample),
    color="col")

#low plot
levels=6

starve.plot<-
  ggplot(fitp3, aes(x = starve, y = probability, color=color, fill=color))+
  stat_lineribbon(size=2, .width = ppoints(levels), alpha=1/levels)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.1,1.1), breaks=c(0,0.25,0.5,0.75,1))+
  scale_color_manual(values=c("#465b70"))+
  xlab("\nStarvation (pPC3)\n\n")+
  ggtitle("\n")+
  ylab("Probability of\n High Alloparental Care\n")+
  theme(plot.title =element_text(size=12, face="bold",hjust=0.5),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_text(size=12,face="bold"),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.line = element_line(size = 1),
        panel.border=element_rect(fill=NA,color="black", size=1, 
                                  linetype="solid"),
        panel.background= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1,0.5,0.5,0.5), "cm"))+
        guides(fill=FALSE, color=FALSE)+
        scale_fill_manual(values=c("#5215a1","#ffde24","#90b7de"))+
        geom_point(data=df.raw2, aes(x=PC3,
                                     y = bincare + runif(length(df.raw2$PC3),-0.05,0.05),
                                     fill=as.factor(bincare)),
                   position=position_jitter(width=0.15,height=0),
                   inherit.aes = FALSE, pch=21, size=3, alpha=0.6, stroke=1)
}

######################################################################
#combine
{  
p.grid<-plot_grid(lowp.plot, highp.plot, starve.plot, ncol=3, nrow=1,  align="h")

#y axis
y.grob <- textGrob("\nProbability of\n High Alloparental Care", rot=90, hjust=0.4,
                     gp=gpar(fontface="bold", fontsize=12))
 
#add to plot
gridplot <- grid.arrange(arrangeGrob(p.grid, left=y.grob))

#save
save_plot("Fig 2A.tiff", gridplot, compression="lzw",
              dpi=600, base_width=13, base_height=4)
  
save_plot("Fig 2A.png", gridplot,
            dpi=600, base_width=13, base_height=4)

}

}
#Fig 2B
{
post <- posterior_samples(allo.m3_rs)

#low starvation
{
  #median linear predictor to probability scale
  #conditional on average covariates, minimum monotonic category
  #marginalized over random effects
  
  f <- function(pc1,pc2){ z <- logistic( 
    median(post$b_bincare_Intercept) +
      median(post$b_bincare_PC1)*pc1 +
      median(post$b_bincare_PC2)*pc2 +
      median(post$b_bincare_IPC1E2)*pc1^2 +
      median(post$b_bincare_IPC2E2)*pc2^2 +
      median(post$`b_bincare_PC1:PC2`)*(pc1*pc2) +
      median(post$bsp_bincare_miPC3*-1)
  ) }

  #predictions from from low (-1,-1) to high (1,1) scores
  pc1<- seq(from=-1,to=1,0.01)
  pc2<- seq(from=-1,to=1,0.01)
  
  #predict median probability of high allopaternal care
  z <- outer(pc1,pc2,f)
  df<- melt(z)
  
  #convert to original data scale
  temp<-as.factor(df$Var1)
  df$PC1<-as.vector(pc1[temp])
  df$PC2<-rep(pc2,each=length(pc2))

  plot2a<-  
    ggplot(df, aes(x=PC1, y=PC2, fill=value))+geom_raster()+
    scale_fill_viridis("Probability of \nHigh Alloparental Care", limits=c(0,1))+
    scale_x_continuous(expand=c(0,0),breaks=c(-1,0,1), labels=c("   ","0   ","1   "))+
    scale_y_continuous(expand=c(0,0),breaks=c(-1,0,1), labels=c("-1   ","0   ","1   "))+
    xlab("\nTemperature (pPC1)")+
    ylab("Precipitation (pPC2)\n")+
    ggtitle("Low (-1)\n")+
    theme(plot.title =element_text(size=12, hjust=0.5),
          legend.text =element_text(size=9),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=9),
          axis.text.y=element_text(size=9),
          plot.margin = unit(c(0.1,0.5,0.5,0.5), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))+
    guides(fill=FALSE, color=FALSE)
}

#average starvation
{
  #median linear predictor to probability scale
  f <- function(pc1,pc2){ z <- logistic( 
    median(post$b_bincare_Intercept) +
      median(post$b_bincare_PC1)*pc1 +
      median(post$b_bincare_PC2)*pc2 +
      median(post$b_bincare_IPC1E2)*pc1^2 +
      median(post$b_bincare_IPC2E2)*pc2^2 +
      median(post$`b_bincare_PC1:PC2`)*(pc1*pc2) +
      median(post$bsp_bincare_miPC3*0)
  ) }
  
  #predictions from from low (-1,-1) to high (1,1)
  pc1<- seq(from=-1,to=1,0.01)
  pc2<- seq(from=-1,to=1,0.01)
  
  #predict median probability of high allopaternal care
  z <- outer(pc1,pc2,f)
  df<- melt(z)
  
  #convert to original data scale
  temp<-as.factor(df$Var1)
  df$PC1<-as.vector(pc1[temp])
  df$PC2<-rep(pc2,each=length(pc2))

  plot2b<-  
    ggplot(df, aes(x=PC1, y=PC2, fill=value))+geom_raster()+
    scale_fill_viridis("Probability of \nHigh Alloparental Care", limits=c(0,1))+
    scale_x_continuous(expand=c(0,0),breaks=c(-1,0,1), labels=c("   ","0   ","1   "))+
    scale_y_continuous(expand=c(0,0),breaks=c(-1,0,1), labels=c("-1   ","0   ","1   "))+
    xlab("\nTemperature (pPC1)")+
    ylab("Precipitation (pPC2)\n")+
    ggtitle("Average (0)\n")+
    theme(plot.title =element_text(size=12, hjust=0.5),
          legend.text =element_text(size=9),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_text(size=12, face="bold", hjust=0.45),
          axis.text.x=element_text(size=9),
          axis.text.y=element_text(size=9),
          plot.margin = unit(c(0.1,0.5,0.5,0.5), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))+
    guides(fill=FALSE, color=FALSE)
}

#high starvation
{
  #median linear predictor to probability scale
  f <- function(pc1,pc2){ z <- logistic( 
    median(post$b_bincare_Intercept) +
      median(post$b_bincare_PC1)*pc1 +
      median(post$b_bincare_PC2)*pc2 +
      median(post$b_bincare_IPC1E2)*pc1^2 +
      median(post$b_bincare_IPC2E2)*pc2^2 +
      median(post$`b_bincare_PC1:PC2`)*(pc1*pc2) +
      median(post$bsp_bincare_miPC3*1)
  ) }
  
  #predictions from from low (-1,-1) to high (1,1)
  pc1<- seq(from=-1,to=1,0.01)
  pc2<- seq(from=-1,to=1,0.01)
  
  #predict median probability of high allopaternal care
  z <- outer(pc1,pc2,f)
  df<- melt(z)
  
    #convert to original data scale
  temp<-as.factor(df$Var1)
  df$PC1<-as.vector(pc1[temp])
  df$PC2<-rep(pc2,each=length(pc2))
  
  #with legend
  temp<-  
    ggplot(df, aes(x=PC1, y=PC2, fill=value))+geom_raster()+
    scale_fill_viridis("Probability of \nHigh Alloparental Care\n", limits=c(0,1))+
    scale_x_continuous(expand=c(0,0),breaks=c(-1,0,1), labels=c("   ","0   ","1   "))+
    scale_y_continuous(expand=c(0,0),breaks=c(-1,0,1), labels=c("-1   ","0   ","1   "))+
    xlab("\nTemperature (pPC1)")+
    ylab("Precipitation (pPC2)\n")+
    ggtitle("High (+1)\n")+
    theme(plot.title =element_text(size=12, hjust=0.5),
          legend.text=element_text(size=9),
          legend.title=element_text(size=12, face="bold"), 
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=9),
          axis.text.y=element_text(size=9),
          plot.margin = unit(c(0.1,0.5,0.5,0.5), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  library(cowplot)
  
  #grab legend
  legend <- get_legend(temp + theme(legend.box.margin = margin(0, 15, 0, 15)))
  
  
  #without legend
  plot2c<-  
    ggplot(df, aes(x=PC1, y=PC2, fill=value))+geom_raster()+
    scale_fill_viridis("Probability of \nHigh Alloparental Care", limits=c(0,1))+
    scale_x_continuous(expand=c(0,0),breaks=c(-1,0,1), labels=c("   ","0   ","1   "))+
    scale_y_continuous(expand=c(0,0),breaks=c(-1,0,1), labels=c("-1   ","0   ","1   "))+
    xlab("\nTemperature (pPC1)")+
    ylab("Precipitation (pPC2)\n")+
    ggtitle("High (+1)\n")+
    theme(plot.title =element_text(size=12, hjust=0.5),
          legend.text =element_text(size=9),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=9),
          axis.text.y=element_text(size=9),
          plot.margin = unit(c(0.1,0.5,0.5,0.5), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))+
    guides(fill=FALSE, color=FALSE)
}

#Combine Fig 2B plots
{
  
  #combine
  p.grid2<-plot_grid(plot2a, plot2b, plot2c, ncol=3, nrow=1,  align="h")
  
  x.grob <- textGrob("       Starvation (pPC3)\n",
                     gp=gpar(fontface="bold", fontsize=12))
  
  y.grob2 <- textGrob("\nPrecipitation (pPC2)\n", rot=90, hjust=0.7,
                     gp=gpar(fontface="bold", fontsize=12))
  
  gridplot2<-grid.arrange(arrangeGrob(p.grid2, top=x.grob))
  gridplot2<- grid.arrange(arrangeGrob(gridplot2, left=y.grob2))

  #save
  save_plot("Fig 2B.tiff", gridplot2,compression="lzw",
              dpi=600, base_width=13, base_height=5)
  
  save_plot("Fig 2B.png", gridplot2,
            dpi=300, base_width=13, base_height=5)
  }
} 
#combine Figure 2 panels
{
gp3<-plot_grid(gridplot, gridplot2,labels = c('   A', '   B'),ncol=1, nrow=2, align="hv")
temp <- plot_grid(NULL,legend, ncol=1, nrow=2)
gridplot3<-plot_grid(gp3, temp, rel_widths=c(0.85,0.15))

  #save
  save_plot("Fig 2.tiff", gridplot3,compression="lzw",
              dpi=600, base_width=15, base_height=8)
  
  save_plot("Fig 2.png", gridplot3,
            dpi=300, base_width=15, base_height=8)
}

#Fig S1
{
#get posterior median imputed  missing starvation scores
post <- posterior_samples(allo.m3_rs)
df <-finaldata
df[is.na(df$PC3),"PC3"]<-
  apply(post[ , grepl( "Ymi" , colnames( post ) ) ], 2, median)
  
#posterior median high allocare probability per society
pred<-data.frame(fitted(allo.m3_rs, newdata=df, resp="bincare", summary=TRUE, robust=TRUE))
pred<-setNames(pred$Estimate, df$socname)
  
#plot
library(phytools)
 
#observed allomaternal care plot
bincare <- factor(ifelse(finaldata$bincare==0, "Low allomaternal care", "High allomaternal care"),
                  levels=c("Low allomaternal care", "High allomaternal care"))
names(bincare) <- finaldata$socname
cols<-setNames(c("purple4","yellow"), c("Low allomaternal care", "High allomaternal care"))

 
tiff(file="Fig S1.tiff", res=600, width=8, height=10, units="in", compression="lzw")
{
dotTree(phylo, bincare, length=10, lwd=4, fsize=0.4, ftype="reg", colors=cols,
        mar=c(2.5,1,1,1), res=600, legend=FALSE)

#contour map of predicted alloparental care
obj<-contMap(phylo, pred, plot=FALSE, res=600, length=10, lwd=4, fsize=0.4, ftype="reg")
obj<-setMap(obj,c("purple4", "cyan", "chartreuse", "yellow"))

#overlay continous mapping
plot(obj$tree, colors=obj$cols, add=TRUE, ftype="off", lwd=4, mar=c(2.5,1,1,1),
     xlim=get("last_plot.phylo",envir=.PlotPhyloEnv)$x.lim,
     ylim=get("last_plot.phylo",envir=.PlotPhyloEnv)$y.lim)

add.color.bar(100, obj$cols,title="Posterior Probability of\n High Allomaternal Care",
              lims=obj$lims,digits=3,prompt=FALSE,x=0,
              y=135, lwd=4,fsize=1,subtitle="")
axis(1, pos= -5)
title(xlab="Thousands of years from the root", line = 1)
}
dev.off()
  
png(file="Fig S1.png", res=600, width=8, height=10, units="in")
{
  dotTree(phylo, bincare, length=10, lwd=4, fsize=0.4, ftype="reg", colors=cols,
          mar=c(2.5,1,1,1), res=600, legend=FALSE)
  
  #contour map of predicted alloparental care
  obj<-contMap(phylo, pred, plot=FALSE, res=600, length=10, lwd=4, fsize=0.4, ftype="reg")
  obj<-setMap(obj,c("purple4", "cyan", "chartreuse", "yellow"))
  
  #overlay continous mapping
  plot(obj$tree, colors=obj$cols, add=TRUE, ftype="off", lwd=4, mar=c(2.5,1,1,1),
       xlim=get("last_plot.phylo",envir=.PlotPhyloEnv)$x.lim,
       ylim=get("last_plot.phylo",envir=.PlotPhyloEnv)$y.lim)
  
  add.color.bar(100, obj$cols,title="Posterior Probability of\n High Allomaternal Care",
                lims=obj$lims,digits=3,prompt=FALSE,x=0,
                y=135, lwd=4,fsize=1,subtitle="")
  axis(1, pos= -5)
  title(xlab="Thousands of years from the root", line = 1)
}
dev.off()

}

#Fig S2
{
  library(ggplot2)
  library(tidyr)
  
  #low starvation
  {
  #data for predictions
  df <- data.frame(PC1 = rep(c(-1,-1,-1,0,0,0,1,1,1),7),
                   PC2 = rep(c(-1,-1,-1,0,0,0,1,1,1),7),
                   PC3= -1,
                   fathercare=1,
                   bincare=0.5, #arbitrary value for fitted function (has no effect on predictions)
                   fixity=1,
                   pathogen=0,
                   binstore="none",
                   subsistence=rep(as.character(unique(finaldata$subsistence)),each=9))
  
  #model predicted values
  pred <-
  data.frame(fitted(allo.m3_rs, newdata=df, resp="bincare",nsamples=3000,
         re_formula = ~ (mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2)||subsistence),
         scale="linear", summary=FALSE))
  
  #wide to long
  pred<- gather(pred, value="Log_Odds")
  pred$PC1 <- rep(c(-1,-1,-1,0,0,0,1,1,1),each=3000,times=7)
  pred$PC2 <- rep(c(-1,-1,-1,0,0,0,1,1,1),each=3000,times=7)
  pred$PC3 <- -1
  pred$subsistence <- rep(as.character(unique(finaldata$subsistence)),each=3000,times=9)
  pred$condition <- factor( ifelse(pred$PC1== -1 & pred$PC2 == -1, "-1 pPC1, -1 pPC2", 
                           ifelse(pred$PC1== 0 & pred$PC2 == 0, "0 pPC1, 0 pPC2", 
                           "1 pPC1, 1 pPC2")), levels=c("-1 pPC1, -1 pPC2", 
                                                        "0 pPC1, 0 pPC2",
                                                        "1 pPC1, 1 pPC2"))
  }

  #high starvation
  {
    #data for predictions
    df <- data.frame(PC1 = rep(c(-1,-1,-1,0,0,0,1,1,1),7),
                     PC2 = rep(c(-1,-1,-1,0,0,0,1,1,1),7),
                     PC3= 1,
                     fathercare=1,
                     bincare=0.5, #arbitrary value for fitted function (has no effect on predictions)
                     fixity=1,
                     pathogen=0,
                     binstore="none",
                     subsistence=rep(as.character(unique(finaldata$subsistence)),each=9))
    
    #model predicted values
    pred3 <-
      data.frame(fitted(allo.m3_rs, newdata=df, resp="bincare",nsamples=3000,
                        re_formula = ~ (mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2)||subsistence),
                        scale="linear", summary=FALSE))
    
    #wide to long
    pred3<- gather(pred3, value="Log_Odds")
    pred3$PC1 <- rep(c(-1,-1,-1,0,0,0,1,1,1),each=3000,times=7)
    pred3$PC2 <- rep(c(-1,-1,-1,0,0,0,1,1,1),each=3000,times=7)
    pred3$PC3 <- 1
    pred3$subsistence <- rep(as.character(unique(finaldata$subsistence)),each=3000,times=9)
    pred3$condition <- factor( ifelse(pred3$PC1== -1 & pred3$PC2 == -1, "-1 pPC1, -1 pPC2", 
                                     ifelse(pred3$PC1== 0 & pred3$PC2 == 0, "0 pPC1, 0 pPC2", 
                                            "1 pPC1, 1 pPC2")), levels=c("-1 pPC1, -1 pPC2", 
                                                                         "0 pPC1, 0 pPC2",
                                                                         "1 pPC1, 1 pPC2"))
  }
  
  #combine data
  pred.df <- rbind(pred, pred3)
  pred.df$PC3 <- factor(ifelse(pred.df$PC3== -1, "Low (-1)", "High (+1)"),
                        levels=c("Low (-1)", "High (+1)"))
  
  subplot<-
  ggplot(pred.df, aes(x = Log_Odds, fill = PC3, group = PC3)) +
        geom_density(aes(y=..scaled..), alpha=0.4, size=0.75)+ facet_grid(subsistence ~ condition) + 
    geom_vline(xintercept=c(0), linetype="longdash", size=0.5)+
    geom_vline(xintercept=c(-5, 5), linetype="longdash", size=0.5)+
    scale_fill_manual(values=c("#057ae8", "#e8052b"))+
    coord_cartesian(xlim=c(-20, 20))+
    xlab("\nLog Odds of High Alloparental Care")+
    ylab("Posterior density\n")+
    labs(fill="pPC3")+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    theme(plot.title =element_text(size=12, hjust=0.5),
          legend.text =element_text(size=10),
          legend.title=element_text(size=12),
          axis.ticks.y=element_blank(),
          axis.title.y=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_text(size=12),
          axis.text.x=element_text(size=9),
          axis.text.y=element_text(size=9),
          plot.margin = unit(c(1,0.5,0.5,0.5), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          strip.text=element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.text.y.right = element_text(angle=0))+
    guides()

png(file="Fig S2.png", res=600, width=14, height=12, units="in")  
plot(subplot)
dev.off()

tiff(file="Fig S2.tiff", res=600, width=14, height=12, units="in", compression="lzw")  
plot(subplot)
dev.off()
  
}

#Fig S3
{
library(ggplot2)
library(cowplot)
library(tidyr)

temp <- finaldata[,c("PC1","PC2","biome","bincare")]
long<-gather(temp,col,value,PC1,PC2, factor_key=TRUE)
long$biome<-factor(long$biome,
            levels=c("Deserts & Xeric Shrublands",
                     "Temperate Grasslands, Savannas & Shrublands",
                     "Mediterranean Forests, Woodlands & Scrub",
                     unique(levels(long$biome))[-c(2,7,11)]))
long$col<-ifelse(long$col=="PC1", "pPC1", "pPC2")

#PC plot
biome.plotA<-
ggplot(long, aes(x = as.factor(biome), y = value, color=as.factor(col)))+
 stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "pointrange", size=1.1)+
  scale_x_discrete(limits = rev(levels(long$biome)))+
  geom_hline(yintercept=0)+
  labs(color="Principal Component", x="Biome", y = "Score")+
  theme(plot.title =element_text(size=12, hjust=0.5),
          legend.text =element_text(size=12),
          legend.key = element_rect(fill = NA),
          legend.title=element_text(size=12, face="bold"),
          axis.ticks.y=element_blank(),
          axis.title.y=element_text(size=12, face="bold"),
          axis.ticks.x=element_blank(),
          axis.title.x=element_text(size=12, face="bold"),
          axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          plot.margin = unit(c(1,0.5,0.5,0.5), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          strip.text=element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.text.y.right = element_text(angle=0))+
  coord_flip()

 
png(file="Fig S3.png", res=600, width=12, height=8, units="in")  
plot(biome.plotA)
dev.off()
  
  }


####################################################################
##################### Robustness checks ############################
####################################################################

#original ordinal scale model
{
  #model structure
  
  #imputation model
  starve.mi<-bf(PC3|mi() ~ mo(allomaternal) + mo(fathercare) + mo(fixity) +
                  binstore + pathogen + PC1*PC2 + I(PC1^2) + I(PC2^2) + 
                  (1|biome) + (PC1*PC2 + I(PC1^2) + I(PC2^2)||subsistence), decomp = "QR") + gaussian()
  #main model
  main.m<-bf(allomaternal ~ mo(fathercare) + mo(fixity) + binstore + pathogen +
               mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2)  +
               (1|biome) + (mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2)||subsistence) + (1|socname), decomp = "QR") +
    cumulative()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="allomaternal"),
      prior(normal(0,1), class=b,resp="allomaternal"),
      prior(exponential(3), class=sd, resp="allomaternal"))
  
  #estimate model
  allo.m3_ord_rs <-
    brm(formula= main.m + starve.mi  + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=9,
        control=list(adapt_delta=0.99, max_treedepth=10))
  
  summary(allo.m3_ord_rs, prob=0.9)
  
  saveRDS(allo.m3_ord_rs,"allo_m3_ord_rs.RDS")
}

#load model
allo.m3_ord_rs <- readRDS("allo_m3_ord_rs.RDS")

#inspect m3
summary(allo.m3_ord_rs, prob=0.90) #check for convergence and appropriate sampling
fixef(allo.m3_ord_rs, robust=TRUE, probs=c(0.05, 0.95)) #robust fixed effect estimates

#posterior probabilities for central effects
hypothesis(allo.m3_ord_rs, "allomaternal_PC1  < 0", class="b")
hypothesis(allo.m3_ord_rs, "allomaternal_PC2  > 0", class="b")
hypothesis(allo.m3_ord_rs, "allomaternal_IPC1E2  < 0", class="b")
hypothesis(allo.m3_ord_rs, "allomaternal_IPC2E2  < 0", class="b")
hypothesis(allo.m3_ord_rs, "allomaternal_PC1:PC2  > 0", class="b")
hypothesis(allo.m3_ord_rs, "allomaternal_miPC3  < 0", class="bsp")

####################################################################
#starvation effect without imputation
#estimate model
{
  #model structure

  #main model
  main.m<-bf(bincare ~ mo(fathercare) + mo(fixity) + binstore + pathogen +
               PC3 + PC1 + PC2 + I(PC1^2) + I(PC2^2) + PC1*PC2 + 
               (1|biome) + (1|subsistence) + (1|socname), decomp = "QR") + bernoulli()
  
  #priors
  priormv<-
    c(prior(normal(0,1), class=Intercept),
      prior(normal(0,1), class=b),
      prior(exponential(3), class=sd))
  
  #estimate model
  allo.mcc<-
    brm(formula= main.m, data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv, warmup=1000, iter=3000, chains=4, seed=9,
        control=list(adapt_delta=0.99, max_treedepth=10))

  summary(allo.mcc, prob=0.9)
  
  saveRDS(allo.mcc,"allo_mcc.RDS")
} 

#load model
allo.mcc <- readRDS("allo_mcc.RDS")

#PC3 effect without imputation
fixef(allo.mcc, robust=TRUE, probs=c(0.05,0.95))["PC3",]

#posterior probability for PC3 effect
hypothesis(allo.mcc, "PC3  < 0", class="b")

####################################################################
############ Supplementary father care analysis ####################
####################################################################

#bin patrilineal inheritance
finaldata$inheritbin<-ifelse(finaldata$inheritance<6,0,1)

#main effects model w/o random slopes
{
  #imputation
  starvef.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(polygny) +  mo(fixity) +
                   binstore + pathogen + inheritbin + PC1 + PC2 + (1|biome) + (1|subsistence)) + gaussian()
  
  #estimate model
  mainf.m<-bf(fathercare ~ bincare + mo(polygny) +  mo(fixity) + binstore + pathogen + inheritbin +
                mi(PC3) + PC1 + PC2 + (1|biome) + (1|socname) + (1|subsistence)) + cumulative()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="fathercare"),
      prior(normal(0,1), class=b,resp="fathercare"),
      prior(exponential(3), class=sd, resp="fathercare"))
  
  #estimate model
  father.m1 <-
    brm(formula= mainf.m + starvef.mi + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv,
        warmup=1000, iter=3000, chains=4, seed=9,
        control=list(adapt_delta=0.99, max_treedepth=10))
  
  saveRDS(father.m1,"father_m1.RDS")
  
}

#main effects model w/ random slopes
{
  #imputation
  starvef.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(polygny) +  mo(fixity) +
                   binstore + pathogen + inheritbin + PC1 + PC2 + (1|biome) + (PC1 + PC2||subsistence)) + gaussian()
  
  #estimate model
  mainf.m<-bf(fathercare ~ bincare + mo(polygny) +  mo(fixity) + binstore + pathogen + inheritbin +
                mi(PC3) + PC1 + PC2 + (1|biome) + (1|socname) + (mi(PC3) + PC1 + PC2 ||subsistence)) + cumulative()
  
  #priors
  priormv<-
    c(
      prior(normal(0,1), class=Intercept,resp="PC3"),
      prior(normal(0,1), class=b,resp="PC3"),
      prior(exponential(3), class=sd, resp="PC3"),
      prior(exponential(3), class=sigma, resp="PC3"),
      prior(normal(0,1), class=Intercept,resp="fathercare"),
      prior(normal(0,1), class=b,resp="fathercare"),
      prior(exponential(3), class=sd, resp="fathercare"))
  
  #estimate model
  father.m1_rs <-
    brm(formula= mainf.m + starvef.mi + set_rescor(FALSE),
        data = finaldata, cov_ranef=list(socname=phylo_vcv),
        prior=priormv,
        warmup=1000, iter=3000, chains=4, seed=9,
        control=list(adapt_delta=0.99, max_treedepth=10))
  
  saveRDS(father.m1_rs,"father_m1_rs.RDS")
  
}

#response surface model w/ random slopes
{
#imputation
starvef.mi<-bf(PC3|mi() ~ bincare + mo(fathercare) + mo(polygny) +  mo(fixity) +
                binstore + pathogen + inheritbin + PC1*PC2 + I(PC1^2) + I(PC2^2) + 
                (1|biome) + (PC1*PC2 + I(PC1^2) + I(PC2^2)||subsistence)) + gaussian()

#estimate model
mainf.m<-bf(fathercare ~ bincare + mo(polygny) +  mo(fixity) + binstore + pathogen + inheritbin +
             mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2) +
             (1|biome) + (1|socname) + (mi(PC3) + PC1*PC2 + I(PC1^2) + I(PC2^2)||subsistence)) + cumulative()

#priors
priormv<-
  c(
    prior(normal(0,1), class=Intercept,resp="PC3"),
    prior(normal(0,1), class=b,resp="PC3"),
    prior(exponential(3), class=sd, resp="PC3"),
    prior(exponential(3), class=sigma, resp="PC3"),
    prior(normal(0,1), class=Intercept,resp="fathercare"),
    prior(normal(0,1), class=b,resp="fathercare"),
    prior(exponential(3), class=sd, resp="fathercare"))

#estimate model
father.m2_rs <-
  brm(formula= mainf.m + starvef.mi + set_rescor(FALSE),
      data = finaldata, cov_ranef=list(socname=phylo_vcv),
      prior=priormv,
      warmup=1000, iter=3000, chains=4, seed=9,
      control=list(adapt_delta=0.99, max_treedepth=10))

saveRDS(father.m2_rs,"father_m2_rs.RDS")

}

#load models
father.m1 <- readRDS("father_m1.RDS")
father.m1_rs <- readRDS("father_m1_rs.RDS")
father.m2_rs <- readRDS("father_m2_rs.RDS")


#model comparison
compare_ic(WAIC(father.m1,resp="fathercare"),
           WAIC(father.m1_rs,resp="fathercare"),
           WAIC(father.m2_rs,resp="fathercare"))

####################################################################

#inspect model
summary(father.m1_rs, prob=0.90) #check for convergence and sampling
fixef(father.m1_rs, robust=TRUE, probs=c(0.05, 0.95)) #robust estimates

#posterior probabilities
hypothesis(father.m1_rs, "fathercare_PC1  > 0", class="b")
hypothesis(father.m1_rs, "fathercare_PC2  > 0", class="b")
hypothesis(father.m1_rs, "fathercare_miPC3  < 0", class="bsp")
hypothesis(father.m1_rs, "fathercare_binstorepresent  < 0", class="b")
hypothesis(father.m1_rs, "fathercare_mofixity  < 0", class="bsp")
hypothesis(father.m1_rs, "fathercare_inheritbin  < 0", class="b")
hypothesis(father.m1_rs, "fathercare_mopolygny  < 0", class="bsp")
hypothesis(father.m1_rs, "fathercare_pathogen  < 0", class="b")

#get posterior samples
post <- posterior_samples(father.m1_rs)

#Cohen's d
{logitsd<-pi/sqrt(3)
  
  d_df<-data.frame(
    effect=c("PC1","PC2", "PC3", "inheritbin", "polygny",
             "binstore", "fixity", "pathogen"), d_median=NA, d_mad=NA)
  
  vars<-subset(post, select = c(b_fathercare_PC1, b_fathercare_PC2, bsp_fathercare_miPC3,
                                b_fathercare_inheritbin, bsp_fathercare_mopolygny,
                                b_fathercare_binstorepresent, bsp_fathercare_mofixity, b_fathercare_pathogen))
  
  d_df$d_median<-apply(vars, 2, function(x) median(x/logitsd))
  d_df$d_mad<-apply(vars, 2, function(x) mad(x/logitsd))
  
  print(d_df) }

###########################################################

#If you have any questions or concerns please contact
#Jordan Scott Martin at jordan.martin@uzh.ch

