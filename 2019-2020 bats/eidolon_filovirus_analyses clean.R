## eidolon helvum: filovirus analyses 2019-2020 data
## maya juman
## updated 02/09/24

## clean up space
rm(list=ls()) 
graphics.off()

## set up

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(readxl)
library(GGally)
library(dplyr)
library(factoextra)
library(lme4)
library(lmerTest)
library(mgcv)
library(Hmisc)
library(corrplot)

## set up data

MFI.data <- read_excel("Filovirus PREEMPT 2019-2020 Data.xlsx",sheet="Data", guess_max = 5000) %>% 
  mutate(Site = factor(Site)) %>%
  dplyr::rename(Weight = `Weight (g)`,
                Forearm = `Forearm (mm)`,
                Age = `Age Category`,
                Repro = `Reproductive Status`)

#defining body condition as weight/forearm length (Plowright et al. 2008)
MFI.data$BodyCondition <- MFI.data$Weight/MFI.data$Forearm

bead.col <- which(str_starts(names(MFI.data),"Analyte"))
bead.names <- names(MFI.data)[bead.col]
virus.names <- str_split_fixed(bead.names," ",3)[,3]

MFI.data.long <- MFI.data %>% 
  pivot_longer(starts_with("Analyte"), names_to="Analyte", values_to="MFI") %>%
  mutate(Analyte = str_split_fixed(Analyte," ",3)[,3],
         log.MFI = log(MFI))

bead.key <- read_excel("Filovirus PREEMPT 2019-2020 Data.xlsx",sheet="Key")
controls <- read_excel("Filovirus PREEMPT 2019-2020 Data.xlsx",sheet="Controls",na = "NaN")

mAb.controls <- controls %>% 
  filter(str_starts(Sample,"mAb")) %>%
  separate(Sample, into=c("mAb","Dilution"),sep=" 1:", extra="drop") %>%
  separate(Dilution, into=c("Dilution"),sep=" ", extra="drop") %>%
  mutate(Dilution = as.integer(Dilution))

mAb.controls.long <- mAb.controls %>%
  pivot_longer(starts_with("Analyte"), names_to="Analyte", values_to="MFI") %>%
  mutate(Analyte = str_split_fixed(Analyte," ",3)[,3],
         log.MFI = log(MFI),
         Control = sapply(Analyte, function(x){with(bead.key, Control_mAB[which(Abbreviation==x)])}),
         Match = (mAb==Control)
  )

#generate corrected log MFI values by subtracting background (mock) MFI

MFI.mock.corrected <- MFI.data.long %>% 
  group_by(Sample) %>%
  mutate(log.MFI.mc = log.MFI - log.MFI[Analyte=="MOCK"]) %>%
  ungroup() %>%
  filter(Analyte != "MOCK")

MFI.mock.corrected$Site <- factor(MFI.mock.corrected$Site, 
                                  levels = c("Accra urban roost","Captive colony",
                                             "Akosombo rural roost","Kumasi urban roost"))

MFI.mock.corrected.wide <- MFI.mock.corrected %>% dplyr::select(-MFI, -log.MFI) %>% 
  pivot_wider(names_from = Analyte, values_from = log.MFI.mc)

#monoclonal antibodies as positive controls

bead.key %>% filter(!is.na(Control_mAB)) %>% dplyr::select(c(4,8))

#mock correction for mAb
mAb.controls.mc <- mAb.controls.long %>% 
  group_by(`Luminex Date`, mAb, Dilution, Analyte) %>%
  dplyr::summarise(log.MFI = mean(log.MFI), .groups = "keep") %>%
  group_by(`Luminex Date`, mAb, Dilution) %>%
  mutate(log.MFI.mc = log.MFI - log.MFI[Analyte=="MOCK"],
         Control = sapply(Analyte, function(x){with(bead.key, Control_mAB[which(Abbreviation==x)])}),
         Match = (mAb==Control)
  )

#monoclonal antibody cutoffs for all possible viruses
#Inflection point: x = e/b, y = (d-c)/2

bead.key %>% filter(!is.na(Control_mAB)) %>% dplyr::select(c(4,8))

mAb.controls.mc$`Luminex Date` <- as.factor(mAb.controls.mc$`Luminex Date`)

#EBOV
sig.EBOV.control <- nls(log.MFI.mc ~ c+(d-c)/(1+(exp(b*log10(Dilution)-e))), 
                       data = mAb.controls.mc %>% filter(Analyte=="EBOV" & Match),  
                       start = list(b=1, c=0, d=2, e=5))

EBOV.ctr.par <- sig.EBOV.control$m$getPars()
EBOVcut <- (EBOV.ctr.par['d']-EBOV.ctr.par['c'])/2

EBOVsero <- MFI.mock.corrected %>% 
  filter(Analyte=="EBOV") %>%
  mutate(Positive = (log.MFI.mc>EBOVcut)) %>%
  group_by(Site, `Sampling month`, Analyte) %>% 
  dplyr::summarise(Prevalence = mean(as.integer(Positive)))

mAb.controls.mc %>% filter(Analyte=="EBOV" & Match) %>% 
  ggplot(aes(x=log10(Dilution), y=log.MFI.mc)) +
  geom_line(aes(col=factor(`Luminex Date`))) +
  geom_hline(yintercept=EBOVcut, linetype="dashed", col="red") + 
  theme(legend.position = "none") + ylab("log corrected MFI value")

#MARV
sig.MARV.control <- nls(log.MFI.mc ~ c+(d-c)/(1+(exp(b*log10(Dilution)-e))), 
                        data = mAb.controls.mc %>% filter(Analyte=="MARV" & Match),  
                        start = list(b=1, c=0, d=2, e=5))

MARV.ctr.par <- sig.MARV.control$m$getPars()
MARVcut <- (MARV.ctr.par['d']-MARV.ctr.par['c'])/2

MARVsero <- MFI.mock.corrected %>% 
  filter(Analyte=="MARV") %>%
  mutate(Positive = (log.MFI.mc>MARVcut)) %>%
  group_by(Site, `Sampling month`, Analyte) %>% 
  dplyr::summarise(Prevalence = mean(as.integer(Positive)))

mAb.controls.mc %>% filter(Analyte=="MARV" & Match) %>% 
  ggplot(aes(x=log10(Dilution), y=log.MFI.mc)) +
  geom_line(aes(col=factor(`Luminex Date`))) +
  geom_hline(yintercept=MARVcut, linetype="dashed", col="red") + 
  theme(legend.position = "none") + ylab("log corrected MFI value")

#seroprevalences <- bind_rows(EBOVsero, MARVsero)

### Multivariate analyses

###all MFI values

#histogram
ggplot(MFI.mock.corrected) + 
  geom_histogram(aes(x=log.MFI.mc, fill=Site, col=Site), alpha=0.4) + 
  geom_vline(xintercept = 0) + facet_wrap(vars(Analyte),scales = "free", nrow=2) + 
  xlab("Mock-corrected lnMFI values") + ylab("Count") +
  theme_minimal()
ggsave("allfilos_hist.jpg",width=14,height=6)

#density plot
ggplot(MFI.mock.corrected) + 
  geom_density(aes(x=log.MFI.mc, fill=Site, col=Site), alpha=0.4) + 
  geom_vline(xintercept = 0) + facet_wrap(vars(Analyte),scales = "free") + 
  xlab("Log mock-corrected MFI values")
#ggsave("allfilos.png",width=12,height=6)

#correlation plot

corrplot(cor(MFI.mock.corrected.wide[,12:21]))

cors <- cor(MFI.mock.corrected.wide[,12:21])

#pca

PCA <- MFI.mock.corrected.wide[,12:21] %>% prcomp(scale. = TRUE)
#fviz_pca_ind(paramyxo.PCA,habillage=MFI.mock.corrected.wide$Site, addEllipses=TRUE, ellipse.level=0.95, label="none")
fviz_pca_biplot(PCA,habillage=MFI.mock.corrected.wide$Site, 
                addEllipses=TRUE, ellipse.level=0.95, 
                label="var")

####clusters?

noiseless.filo2 <- MFI.mock.corrected.wide %>% 
  filter(!if_all(c(LLOV,MLAV,EBOV,BDBV,BOMV,SUDV,TAFV,RESTV,MARV,RAVV), ~ .x < 0))

res.km <- kmeans(scale(noiseless.filo2[,12:21]), 5, nstart = 25)
fviz_cluster(res.km, data = noiseless.filo2[,12:21],
             geom = "text",
             ellipse.type = "convex"
)

noiseless.filo2[c(456,1103,1104,1105,1106),12:21]
noiseless.filo2[c(1449,342,511,1487,1222,28),12:21]
noiseless.filo2[c(497,1370,502,604,745,718),12:21]

fviz_nbclust(MFI.mock.corrected.wide[,12:21], # data  
             kmeans, # clustering algorithm 
             nstart = 25, # if centers is a number, how many random sets should be chosen?(default is 25)
             iter.max = 200, # the maximum number of iterations allowed.
             method = "wss") # elbow method

res.km2 <- kmeans(scale(MFI.mock.corrected.wide[,12:21]), 5, nstart = 25)
fviz_cluster(res.km2, data = MFI.mock.corrected.wide[,12:21],
             geom = "text",
             ellipse.type = "convex"
)

MFI.mock.corrected.wide[c(1620,706,1622,1621,1623),12:21]
MFI.mock.corrected.wide[c(2015,560,1773,2053,779,43),12:21]
MFI.mock.corrected.wide[c(760,1933, 1162),12:21]

###which individuals are consistently positive?

filo.names <- bead.key %>% filter(Family=="Filoviridae") %>% pull(Abbreviation)
filo.columns <- which(names(MFI.mock.corrected.wide) %in% filo.names)

#cutting EBOV, BDBV, and BOMV here?
noiseless.filo <- MFI.mock.corrected.wide %>% 
  filter(!if_all(c(LLOV,MLAV,EBOV,SUDV,BOMV,TAFV,BDBV,RESTV,MARV,RAVV), ~ .x < 0))

PCAnoiseless <- noiseless.filo[,12:21] %>% prcomp(scale. = TRUE)
#fviz_pca_ind(paramyxo.PCA,habillage=MFI.mock.corrected.wide$Site, addEllipses=TRUE, ellipse.level=0.95, label="none")
fviz_pca_biplot(PCAnoiseless,habillage=noiseless.filo$Site, 
                addEllipses=TRUE, ellipse.level=0.95, 
                label="var")

colSums(noiseless.filo[,12:21] > 0)
colSums(noiseless.filo[which(noiseless.filo$BOMV > 0),]>0)[12:21]

####linear modeling?

MFI.mock.corrected.wide$`Sampling month` <- as.POSIXct(MFI.mock.corrected.wide$`Sampling month`,origin = "2020-01-01")
MFI.mock.corrected.wide$time <- (as.numeric(MFI.mock.corrected.wide$`Sampling month`)/86400)-18262
MFI.mock.corrected.wide$timesq <- (MFI.mock.corrected.wide$time*MFI.mock.corrected.wide$time)

MFI.mock.corrected.wide$Repro[which(is.na(MFI.mock.corrected.wide$Repro))] <- "-"

###EBOV

#gaussian GLM using all individuals + MFI values, wild roosts only
summary(glm(EBOV ~ time + Sex + timesq, 
            data=MFI.mock.corrected.wide %>% filter(Site != "Captive colony"), family = gaussian))

#LMEM of three wild roosts
summary(lmer(EBOV ~ Age + Sex + BodyCondition + Site + (1|`Sampling month`), 
             data=MFI.mock.corrected.wide %>% filter(Site != "Captive colony")))

#LMEM of captive col
summary(lmer(EBOV ~ Age + Sex + BodyCondition + Repro + (1|`Bat ID`), 
             data=MFI.mock.corrected.wide %>% filter(Site == "Captive colony")))

###BOMV

#gaussian GLM using all individuals + MFI values, wild roosts only
summary(glm(BOMV ~ time + Sex + timesq, 
            data=MFI.mock.corrected.wide %>% filter(Site != "Captive colony"), family = gaussian))

#LMEM of three wild roosts
summary(lmer(BOMV ~ Age + Sex + BodyCondition + Site + (1|`Sampling month`), 
             data=MFI.mock.corrected.wide %>% filter(Site != "Captive colony")))

#LMEM of captive col
summary(lmer(BOMV ~ Age + Sex + BodyCondition + Repro + (1|`Bat ID`), 
             data=MFI.mock.corrected.wide %>% filter(Site == "Captive colony")))

###BDBV

#gaussian GLM using all individuals + MFI values, wild roosts only
summary(glm(BDBV ~ time + Sex + timesq, 
            data=MFI.mock.corrected.wide %>% filter(Site != "Captive colony"), family = gaussian))

#LMEM of three wild roosts
summary(lmer(BDBV ~ Age + Sex + BodyCondition + Site + (1|`Sampling month`), 
             data=MFI.mock.corrected.wide %>% filter(Site != "Captive colony")))

#LMEM of captive col
summary(lmer(BDBV ~ Age + Sex + BodyCondition + Repro + (1|`Bat ID`), 
             data=MFI.mock.corrected.wide %>% filter(Site == "Captive colony")))


####antigen patterns

EBOVsex <- MFI.mock.corrected %>% drop_na(Sex) %>%
  filter(Analyte=="EBOV") %>%
  mutate(Positive = (log.MFI.mc>EBOVcut)) %>%
  group_by(Site, `Sampling month`, Analyte, Sex) %>% 
  dplyr::summarise(Prevalence = mean(as.integer(Positive)),
                   Positives = sum(as.integer(Positive)),
                   n=n())

EBOVsex$`Sampling month` <- as.Date(EBOVsex$`Sampling month`)

conf <- binconf(EBOVsex$Positives, EBOVsex$n, alpha=0.05)[,2:3]
EBOVsex <- cbind(EBOVsex, conf)

EBOVsex %>%
  ggplot(aes(x=`Sampling month`, y=Prevalence, col=Sex)) +
  theme_bw() + facet_wrap(vars(Site)) +
  geom_line() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Sex), 
              alpha=0.1,
              color=NA) +
  geom_vline(aes(xintercept = as.Date("2019-07-01")), 
             data = subset(EBOVsex, Site == "Captive colony"),
             colour="black", linetype = "longdash") +
  geom_vline(aes(xintercept = as.Date("2020-07-01")), 
             data = subset(EBOVsex, Site == "Captive colony"),
             colour="black", linetype = "longdash") +
  ylab("EBOV seroprevalence")

####age boxplots

AZallage <- MFI.mock.corrected.wide %>% 
  filter(Site=="Captive colony") %>% drop_na(`Age in Years`) %>% 
  dplyr::rename(age = `Age in Years`)

AZallage <- AZallage %>% mutate(Agebin = case_when(age == 0 ~ "0-1",
                                                   age == 1 ~ "0-1",
                                                   age == 2 ~ "2-3",
                                                   age == 3 ~ "2-3",
                                                   age == 4 ~ "4-5",
                                                   age == 5 ~ "4-5",
                                                   age == 6 ~ "6-7",
                                                   age == 7 ~ "6-7",
                                                   age == 8 ~ "8-9",
                                                   age == 9 ~ "8-9",
                                                   age == 10 ~ "10-12",
                                                   age == 11 ~ "10-12",
                                                   age == 12 ~ "10-12"))

AZallage$Agebin <- factor(AZallage$Agebin, levels = 
                            c("0-1","2-3","4-5","6-7","8-9","10-12"))

ggplot(AZallage, aes(x=Agebin, y=EBOV)) + 
  theme_minimal() +
  geom_boxplot(aes(fill=Sex), alpha=0.15, outlier.shape=NA) + #varwidth = TRUE
  geom_point(aes(x=Agebin, colour=Sex), size=0.6, alpha=1, 
             position = position_dodge(width=0.75)) +
  ylab("EBOV log MFI values") +
  xlab("Age") +
  theme(legend.position = "none", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = EBOVcut, colour="black", linetype = "longdash")
ggsave("EBOVages.jpg", width=5, height=5)

ggplot(AZallage, aes(x=Agebin, y=EBOV)) + 
  geom_violin(trim=FALSE, aes(x=Agebin,fill=Sex)) +
  geom_hline(yintercept = EBOVcut, colour="black", linetype = "longdash")
ggsave("EBOVviolin.jpg", width=8, height=2)

ggplot(AZallage, aes(x=Agebin, y=BOMV)) + 
  theme_minimal() +
  geom_boxplot(aes(fill=Sex), alpha=0.15, outlier.shape=NA) + #varwidth = TRUE
  geom_point(aes(x=Agebin, colour=Sex), size=0.6, alpha=1,
             position = position_dodge(width=0.75)) +
  ylab("BOMV log MFI values") +
  xlab("Age") +
  theme(legend.position = "none", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("BOMVages.jpg", width=5, height=5)

ggplot(AZallage, aes(x=Agebin, y=BOMV)) + 
  geom_violin(trim=FALSE, aes(x=Agebin,fill=Sex))
ggsave("BOMVviolin.jpg", width=8, height=2)

ggplot(AZallage, aes(x=Agebin, y=BDBV)) + 
  theme_minimal() +
  geom_boxplot(aes(fill=Sex), alpha=0.15, outlier.shape=NA) + #varwidth = TRUE
  geom_point(aes(x=Agebin, colour=Sex), size=0.6, alpha=1, 
             position = position_dodge(width=0.75)) +
  ylab("BDBV log MFI values") +
  xlab("Age") +
  theme(legend.position = "none", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("BDBVages.jpg", width=5, height=5)

ggplot(AZallage, aes(x=Agebin, y=BDBV)) + 
  geom_violin(trim=FALSE, aes(x=Agebin,fill=Sex))
ggsave("BDBVviolin.jpg", width=8, height=2)

####seroprevalence plots

seroprevs <- MFI.mock.corrected %>% filter(Analyte %in% c("BOMV","EBOV","BDBV")) %>%
  drop_na(Sex) %>%
  mutate(Positive = case_when(Analyte=="EBOV" ~ log.MFI.mc>EBOVcut, 
                              Analyte=="BOMV" ~ log.MFI.mc>0.0264834,
                              Analyte=="BDBV" ~ log.MFI.mc>0.4466668)) %>%
  group_by(Site, `Sampling month`, Analyte, Sex) %>% 
  dplyr::summarise(Prevalence = mean(as.integer(Positive)),
                   Positives = sum(as.integer(Positive)),
                   n=n())
seroprevs <- cbind(seroprevs, 
                       binconf(seroprevs$Positives,
                               seroprevs$n, alpha=0.05)[,2:3])

seroprevs$`Sampling month` <- as.Date(seroprevs$`Sampling month`)

seroprevs %>% filter(Analyte == "EBOV") %>%
  ggplot(aes(x=`Sampling month`, y=Prevalence, group=Sex, col=Sex)) +
  theme_bw() + facet_wrap(vars(Site)) +
  geom_line() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Sex), 
              alpha=0.1,
              color=NA) +
  geom_vline(aes(xintercept = as.Date("2019-07-01")), 
             data = subset(seroprevs, Site == "Captive colony"),
             colour="black", linetype = "longdash") +
  geom_vline(aes(xintercept = as.Date("2020-07-01")), 
             data = subset(seroprevs, Site == "Captive colony"),
             colour="black", linetype = "longdash") +
  ylab("EBOV seroprevalence") + ylim(c(0,0.4))

boxplotMFI <- MFI.mock.corrected.wide %>% 
  mutate(`Sampling month` = 
           case_when((Site == "Captive colony" & `Sampling month` == "2019-07-01") 
                     ~ as.Date("2019-08-01"), TRUE ~ `Sampling month`))

MFI.mock.corrected.wide$date <- MFI.mock.corrected.wide(boxplotMFI$`Sampling month`)
MFI.mock.corrected.wide$Sex <- as.factor(MFI.mock.corrected.wide$Sex)

ggplot(MFI.mock.corrected.wide %>% drop_na(Sex), aes(x=factor(date), y=EBOV, fill=factor(Sex))) + 
  facet_wrap(vars(Site)) +
  geom_boxplot(outlier.shape=NA) +
  ylab("EBOV log MFI values") +
  xlab("Sampling month") +
  theme(legend.position = "none") + ylim(c(-0.5,1))

seroprevs %>% filter(Analyte == "BOMV") %>%
  ggplot(aes(x=`Sampling month`, y=Prevalence, group=Sex, col=Sex)) +
  theme_bw() + facet_wrap(vars(Site)) +
  geom_line() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Sex), 
              alpha=0.1,
              color=NA) +
  geom_vline(aes(xintercept = as.Date("2019-07-01")), 
             data = subset(seroprevs, Site == "Captive colony"),
             colour="black", linetype = "longdash") +
  geom_vline(aes(xintercept = as.Date("2020-07-01")), 
             data = subset(seroprevs, Site == "Captive colony"),
             colour="black", linetype = "longdash") +
  ylab("BOMV seroprevalence") + ylim(c(0,1))

ggplot(MFI.mock.corrected.wide %>% drop_na(Sex), aes(x=factor(date), y=BOMV, fill=factor(Sex))) + 
  facet_wrap(vars(Site)) +
  geom_boxplot(outlier.shape=NA) +
  ylab("BOMV log MFI values") +
  xlab("Sampling month") +
  theme(legend.position = "none") + ylim(c(-0.6,0.6))

seroprevs %>% filter(Analyte == "BDBV") %>%
  ggplot(aes(x=`Sampling month`, y=Prevalence, group=Sex, col=Sex)) +
  theme_bw() + facet_wrap(vars(Site)) +
  geom_line() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Sex), 
              alpha=0.1,
              color=NA) +
  geom_vline(aes(xintercept = as.Date("2019-07-01")), 
             data = subset(seroprevs, Site == "Captive colony"),
             colour="black", linetype = "longdash") +
  geom_vline(aes(xintercept = as.Date("2020-07-01")), 
             data = subset(seroprevs, Site == "Captive colony"),
             colour="black", linetype = "longdash") +
  ylab("BDBV seroprevalence") + ylim(c(0,0.75))

ggplot(MFI.mock.corrected.wide %>% drop_na(Sex), aes(x=factor(date), y=BDBV, fill=factor(Sex))) + 
  facet_wrap(vars(Site)) +
  geom_boxplot(outlier.shape=NA) +
  ylab("BDBV log MFI values") +
  xlab("Sampling month") +
  theme(legend.position = "none") + ylim(c(-1,0.75))

####try mixture model cutoffs on BOMV and BDBV

bomv_clust <- Mclust(MFI.mock.corrected.wide$BOMV, G = 1:3, verbose = FALSE)
plot(bomv_clust, what = "BIC", xlab = "BOMV MFI rescaled") #3 cluster

#cutoff - mean of first cluster + 3x standard deviation
bomv_cutoff <- as.numeric(bomv_clust$parameters$mean[2]) + 3*sqrt(bomv_clust$parameters$variance$sigmasq[2])

bomv_bootClust <- MclustBootstrap(bomv_clust)
bomv_cutoff_low <- summary(bomv_bootClust, what = "ci")$mean[3] + (3*sqrt(summary(bomv_bootClust, what = "ci")$variance[3]))
bomv_cutoff_hi <- summary(bomv_bootClust, what = "ci")$mean[4] + (3*sqrt(summary(bomv_bootClust, what = "ci")$variance[4]))

bomv_plot <- ggplot(MFI.mock.corrected.wide) +
  geom_histogram(aes(x=BOMV), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  xlim(c(-1,1)) +
  theme_classic() + xlab("Corrected BOMV lnMFI") + ylab("Count") +
  geom_vline(xintercept = bomv_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = bomv_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = bomv_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(bomv_clust$parameters$mean[1]),
                                        sd = sqrt(bomv_clust$parameters$variance$sigmasq[1]))*
                  bomv_clust$parameters$pro[1]*2090*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(bomv_clust$parameters$mean[2]), 
                                        sd = sqrt(bomv_clust$parameters$variance$sigmasq[2]))*
                  bomv_clust$parameters$pro[2]*2090*0.05,
                color = "black", size = 0.5) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(bomv_clust$parameters$mean[3]), 
                                        sd = sqrt(bomv_clust$parameters$variance$sigmasq[3]))*
                  bomv_clust$parameters$pro[3]*2090*0.05,
                color = "black", size = 0.5)


bdbv_clust <- Mclust(MFI.mock.corrected.wide$BDBV, G = 1:3, verbose = FALSE)
plot(bdbv_clust, what = "BIC", xlab = "BDBV MFI rescaled") #3 cluster

#cutoff - mean of first cluster + 3x standard deviation
bdbv_cutoff <- as.numeric(bdbv_clust$parameters$mean[2]) + 3*sqrt(bdbv_clust$parameters$variance$sigmasq[2])

bdbv_bootClust <- MclustBootstrap(bdbv_clust)
bdbv_cutoff_low <- summary(bdbv_bootClust, what = "ci")$mean[3] + (3*sqrt(summary(bdbv_bootClust, what = "ci")$variance[3]))
bdbv_cutoff_hi <- summary(bdbv_bootClust, what = "ci")$mean[4] + (3*sqrt(summary(bdbv_bootClust, what = "ci")$variance[4]))

bdbv_plot <- ggplot(MFI.mock.corrected.wide) +
  geom_histogram(aes(x=BDBV), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  xlim(c(-1.2,1.8)) +
  theme_classic() + xlab("Corrected BDBV lnMFI") + ylab("Count") +
  geom_vline(xintercept = bdbv_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = bdbv_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = bdbv_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(bdbv_clust$parameters$mean[1]),
                                        sd = sqrt(bdbv_clust$parameters$variance$sigmasq[1]))*
                  bdbv_clust$parameters$pro[1]*2090*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(bdbv_clust$parameters$mean[2]), 
                                        sd = sqrt(bdbv_clust$parameters$variance$sigmasq[2]))*
                  bdbv_clust$parameters$pro[2]*2090*0.05,
                color = "black", size = 0.5) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(bdbv_clust$parameters$mean[3]), 
                                        sd = sqrt(bdbv_clust$parameters$variance$sigmasq[3]))*
                  bdbv_clust$parameters$pro[3]*2090*0.05,
                color = "black", size = 0.5)


ebov_clust <- Mclust(MFI.mock.corrected.wide$EBOV, G = 1:3, verbose = FALSE)
plot(bdbv_clust, what = "BIC", xlab = "EBOV MFI rescaled") #3 cluster

#cutoff - mean of first cluster + 3x standard deviation
ebov_cutoff <- as.numeric(ebov_clust$parameters$mean[2]) + 3*sqrt(ebov_clust$parameters$variance$sigmasq[2])

ebov_bootClust <- MclustBootstrap(ebov_clust)
ebov_cutoff_low <- summary(ebov_bootClust, what = "ci")$mean[3] + (3*sqrt(summary(ebov_bootClust, what = "ci")$variance[3]))
ebov_cutoff_hi <- summary(ebov_bootClust, what = "ci")$mean[4] + (3*sqrt(summary(ebov_bootClust, what = "ci")$variance[4]))

ebov_plot <- ggplot(MFI.mock.corrected.wide) +
  geom_histogram(aes(x=EBOV), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  xlim(c(-0.7,2.2)) +
  theme_classic() + xlab("Corrected EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = bdbv_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = bdbv_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = bdbv_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ebov_clust$parameters$mean[1]),
                                        sd = sqrt(ebov_clust$parameters$variance$sigmasq[1]))*
                  ebov_clust$parameters$pro[1]*2090*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ebov_clust$parameters$mean[2]), 
                                        sd = sqrt(ebov_clust$parameters$variance$sigmasq[2]))*
                  ebov_clust$parameters$pro[2]*2090*0.05,
                color = "black", size = 0.5) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ebov_clust$parameters$mean[3]), 
                                        sd = sqrt(ebov_clust$parameters$variance$sigmasq[3]))*
                  ebov_clust$parameters$pro[3]*2090*0.05,
                color = "black", size = 0.5)

bdbv_bomv_ebov <- ggarrange(bdbv_plot, bomv_plot, ebov_plot, nrow=3,
                               labels=c("A","B","C"))
ggsave("bdbv_bomv_ebov.png", bdbv_bomv_ebov, height=8, width=4)


MFI.mock.corrected.wide %>%
  mutate(BDBVpos = (BDBV>bdbv_cutoff),
         BDBVhi = (BDBV>bdbv_cutoff_hi),
         BDBVlow = (BDBV>bdbv_cutoff_low),
         BOMVpos = (BOMV>bomv_cutoff),
         BOMVhi = (BOMV>bomv_cutoff_hi),
         BOMVlow = (BOMV>bomv_cutoff_low),
         EBOVpos = (EBOV>ebov_cutoff),
         EBOVhi = (EBOV>ebov_cutoff_hi),
         EBOVlow = (EBOV>ebov_cutoff_low)) %>%
  dplyr::summarise(BDBVpos = mean(as.integer(BDBVpos))*100,
                   BDBVhi = mean(as.integer(BDBVhi))*100,
                   BDBVlow = mean(as.integer(BDBVlow))*100,
                   BOMVpos = mean(as.integer(BOMVpos))*100,
                   BOMVhi = mean(as.integer(BOMVhi))*100,
                   BOMVlow = mean(as.integer(BOMVlow))*100,
                   EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVhi = mean(as.integer(EBOVhi))*100,
                   EBOVlow = mean(as.integer(EBOVlow))*100)

BOMV <- MFI.mock.corrected.wide[which(MFI.mock.corrected.wide$BOMV > bomv_cutoff),c(1:11, 16)]
BDBV <- MFI.mock.corrected.wide[which(MFI.mock.corrected.wide$BDBV > bdbv_cutoff),c(1:11, 18)]

