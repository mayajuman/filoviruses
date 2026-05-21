######filovirus human data compilation
###maya juman
#december 18 2025

## clean up space
rm(list=ls())
graphics.off()

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(ggpubr)

allhuman <- read.csv("human data processed.csv")

allhuman$Note <- NA
allhuman$Note[which(allhuman$Site == "Bonn")] <- "Bonn remeasures"

#sites in Bonn dataset
allhuman$Site[grepl("^(?i)KB", allhuman$Sample)] <- "Kwamang"
allhuman$Site[grepl("^(?i)BB", allhuman$Sample)] <- "Buoyem"
allhuman$Site[grepl("^(?i)FB", allhuman$Sample)] <- "Forikrom"

#labeling exposures
allhuman$Exposure <- NA
allhuman$Exposure[which(allhuman$Site == "Accra")] <- "Indirect/none"
allhuman$Exposure[which(allhuman$Site == "37 Hospital")] <- "Indirect/none"
allhuman$Exposure[which(allhuman$Site == "Akonkonti")] <- "Cave exposure"
allhuman$Exposure[which(allhuman$Site == "Amangoase")] <- "Cave exposure"
allhuman$Exposure[which(allhuman$Site == "Kwamang")] <- "Cave exposure"
allhuman$Exposure[which(allhuman$Site == "Buoyem")] <- "Cave exposure"
allhuman$Exposure[which(allhuman$Site == "Forikrom")] <- "Cave exposure"
allhuman$Exposure[which(allhuman$Site == "Bonya")] <- "Cave exposure"
allhuman$Exposure[which(allhuman$Site == "Buoyem (other)")] <- "Cave exposure"
allhuman$Exposure[which(allhuman$Site == "Ve-Golokuati")] <- "Hunting exposure"
allhuman$Exposure[which(allhuman$Site == "Volta")] <- "Hunting exposure"
allhuman$Exposure[which(allhuman$Site == "Tanoboase")] <- "Hunting exposure"
allhuman$Exposure[which(allhuman$Site == "Noguchi")] <- "Unknown (hospital)"

ggplot(allhuman) + 
  geom_density(aes(x=EBOV, fill=Site, col=Site), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  ggtitle("EBOV affinities by site")

###ebov rescaled
ggplot(allhuman) + 
  geom_density(aes(x=logEBOVrs, fill=Site, col=Site), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1, col="red") + 
  ggtitle("log rescaled EBOV affinities by site") + xlim(c(-0.5,1.5))

ggplot(allhuman) + 
  geom_density(aes(x=MARV, fill=Site, col=Site), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  ggtitle("MARV affinities by site")

###marv rescaled
ggplot(allhuman) + 
  geom_density(aes(x=logMARVrs, fill=Site, col=Site), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1, col="red") + 
  ggtitle("log rescaled MARV affinities by site") + xlim(c(-0.5,1.5))

####by exposure

###ebov rescaled
humanfiga <- ggplot(allhuman) + 
  geom_density(aes(x=logEBOVrs, fill=Exposure, color=Exposure), alpha=0.3, size=0.8) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1, col="red") + xlim(c(-0.5,1.5)) +
  theme_classic() + scale_fill_manual(values=c("darkred","darkblue","darkgreen","grey30")) +
  scale_colour_manual(values=c("darkred","darkblue","darkgreen","grey30"), guide='none') +
  theme(legend.position = c(0.87,0.8)) + xlab("log EBOV MFI, rescaled") +
  guides(fill=guide_legend(title="Bat contact")) + ylab("Density")

###marv rescaled
humanfigb <- ggplot(allhuman) + 
  geom_density(aes(x=logMARVrs, fill=Exposure, color=Exposure), alpha=0.3, size=0.8) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1, col="red") + xlim(c(-0.5,1.5)) +
  theme_classic() + scale_fill_manual(values=c("darkred","darkblue","darkgreen","grey30")) +
  scale_colour_manual(values=c("darkred","darkblue","darkgreen","grey30"), guide='none') +
  theme(legend.position = c(0.87,0.8)) + xlab("log MARV MFI, rescaled") +
  guides(fill=guide_legend(title="Bat contact")) + ylab("Density")

humanfig <- ggarrange(humanfiga, humanfigb, nrow=2, labels=c("A","B"))
ggsave("humanfig1.png", humanfig, height=8, width=7)

ggplot(allhuman) +
  geom_boxplot(aes(x=logEBOVrs, fill=Exposure), alpha=0.4)

allhuman$Exposure <- as.factor(allhuman$Exposure)
allhuman$Exposure <- relevel(allhuman$Exposure, ref = "Indirect/none")

allhuman$Exposure <- as.factor(allhuman$Exposure)
allhuman$Exposure <- relevel(allhuman$Exposure, ref = "Indirect/none")

#linear models of exposure
summary(glm(logEBOVrs~Exposure, data=allhuman))
summary(glm(logMARVrs~Exposure, data=allhuman))


###sanity checks on controls

controls <- read_excel("pos controls.xlsx")

###before rescaling

##EBOV logged
ggplot(allhuman) + geom_density(aes(x=logEBOV), fill="grey") + geom_vline(xintercept = 0) +
  geom_vline(data=controls%>%filter(Antigen == "EBOV", Control != "NSC"), aes(xintercept = log(Value), colour=Control)) +
  geom_vline(data=controls%>%filter(Antigen == "EBOV", Control == "NSC"), aes(xintercept = log(Value)), col="black")

#EBOV unlogged
ggplot(allhuman) + geom_density(aes(x=EBOV), fill="grey") + geom_vline(xintercept = 0) +
  geom_vline(data=controls%>%filter(Antigen == "EBOV", Control != "NSC"), aes(xintercept = Value, colour=Control)) +
  geom_vline(data=controls%>%filter(Antigen == "EBOV", Control == "NSC"), aes(xintercept = (Value)), col="black")

##MARV logged
ggplot(allhuman) + geom_density(aes(x=logMARV), fill="grey") + geom_vline(xintercept = 0) +
  geom_vline(data=controls%>%filter(Antigen == "MARV", Control != "NSC"), aes(xintercept = log(Value), colour=Control)) +
  geom_vline(data=controls%>%filter(Antigen == "MARV", Control == "NSC"), aes(xintercept = log(Value)), col="black")

#MARV unlogged
ggplot(allhuman) + geom_density(aes(x=MARV), fill="grey") + geom_vline(xintercept = 0) +
  geom_vline(data=controls%>%filter(Antigen == "MARV", Control != "NSC"), aes(xintercept = Value, colour=Control)) +
  geom_vline(data=controls%>%filter(Antigen == "MARV", Control == "NSC"), aes(xintercept = (Value)), col="black")


####think about mixture models

boxplot(allhuman$EBOV, allhuman$MARV)
summary(allhuman$EBOV)
summary(allhuman$MARV)

ggplot(allhuman) + geom_density(aes(x=logEBOVrs), fill="grey") + 
  geom_vline(xintercept = 0) + ggtitle("rescaled EBOV affinities, aggregate")

ggplot(allhuman) + geom_density(aes(x=logMARVrs), fill="grey") + 
  geom_vline(xintercept = 0) + ggtitle("rescaled MARV affinities, aggregate")

##no clear cuts for mixture models but i'll try anyway

library(mclust)

###EBOV
ebov_clust <- Mclust(allhuman$logEBOVrs, G = 1:3, verbose = FALSE)
plot(ebov_clust, what = "BIC", xlab = "EBOV MFI rescaled")
plot(ebov_clust, what = "density", xlab = "EBOV MFI rescaled")

# Add lines for individual components of the model by extracting the mean,
# variance, and proportion parameters from the model
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01),         # generate a normal distribution
                                   ebov_clust$parameters$mean[1], # with mean of the first cluster
                                   sqrt(ebov_clust$parameters$variance$sigmasq[1]))* # and SD of the first cluster
        ebov_clust$parameters$pro[1],      # Times the proportion of values in the cluster
      col = "blue", lty = 2)              # colored blue and dotted
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), ebov_clust$parameters$mean[2], 
                                   sqrt(ebov_clust$parameters$variance$sigmasq[2]))*ebov_clust$parameters$pro[2], col = "red", lty = 2)
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), ebov_clust$parameters$mean[3], 
                                   sqrt(ebov_clust$parameters$variance$sigmasq[3]))*ebov_clust$parameters$pro[3], col = "green", lty = 2)
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), ebov_clust$parameters$mean[4], 
                                   sqrt(ebov_clust$parameters$variance$sigmasq[4]))*ebov_clust$parameters$pro[4], col = "orange", lty = 2)
abline(v = ebov_threshold_hi, lty = 3)

# Add empirical density of the data
lines(density(allhuman$logEBOVrs), col = "purple") 

plot(ebov_clust, what = "classification", xlab = "EBOV MFI rescaled")
plot(ebov_clust, what = "uncertainty", xlab = "EBOV MFI rescaled")

###not working because of extremely high uncertainty in cluster 2
ebov_threshold_low <- max(ebov_clust$data[ebov_clust$z[, 1] > 0.95])
ebov_threshold_hi <- min(ebov_clust$data[ebov_clust$z[, 2] > 0.95])
ebov_threshold_50 <- max(ebov_clust$data[ebov_clust$z[, 2] > 0.5])

###MARV
marv_clust <- Mclust(allhuman$logMARVrs, G = 1:4,  verbose = FALSE)
plot(marv_clust, what = "BIC", xlab = "MARV MFI rescaled")
plot(marv_clust, what = "density", xlab = "MARV MFI rescaled")

# Add lines for individual components of the model by extracting the mean,
# variance, and proportion parameters from the model
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01),         # generate a normal distribution
                                   marv_clust$parameters$mean[1], # with mean of the first cluster
                                   sqrt(marv_clust$parameters$variance$sigmasq[1]))* # and SD of the first cluster
        marv_clust$parameters$pro[1],      # Times the proportion of values in the cluster
      col = "blue", lty = 2)              # colored blue and dotted
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), marv_clust$parameters$mean[2], 
                                   sqrt(marv_clust$parameters$variance$sigmasq[2]))*marv_clust$parameters$pro[2], col = "red", lty = 2)
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), marv_clust$parameters$mean[3], 
                                   sqrt(marv_clust$parameters$variance$sigmasq[3]))*marv_clust$parameters$pro[3], col = "green", lty = 2)
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), marv_clust$parameters$mean[4], 
                                   sqrt(marv_clust$parameters$variance$sigmasq[4]))*marv_clust$parameters$pro[4], col = "orange", lty = 2)
abline(v = marv_threshold_50, lty = 3)

# Add empirical density of the data
lines(density(allhuman$logMARVrs), col = "purple") 

plot(marv_clust, what = "classification", xlab = "MARV MFI rescaled")
plot(marv_clust, what = "uncertainty", xlab = "MARV MFI rescaled")

###not working because only one cluster
marv_threshold_low <- max(marv_clust$data[marv_clust$z[, 1] > 0.95])
marv_threshold_hi <- min(marv_clust$data[marv_clust$z[, 2] > 0.95])
marv_threshold_50 <- max(marv_clust$data[marv_clust$z[, 2] > 0.5])



######repeat measurements from bonn

bonn <- allhuman %>%filter(Note == "Bonn remeasures")
bonn$Sample <- gsub('-', '.', bonn$Sample)
str_sub(bonn$Sample, 3, 3) <- ""

bonnrepeats <- bonn %>% group_by(Sample) %>% count() %>% filter(n > 1)
bonnrepeats <- bonn %>% filter(Sample %in% bonnrepeats$Sample)
bonnrepeats <- bonnrepeats %>% group_by(Sample) %>% 
  summarise(changeEBOV = logEBOVrs[Year == 2012] - logEBOVrs[Year == 2011],
            changeMARV = logMARVrs[Year == 2012] - logMARVrs[Year == 2011])

bonnrepeats$Site <- NA
bonnrepeats$Site[grepl("^(?i)KB", bonnrepeats$Sample)] <- "Kwamang"
bonnrepeats$Site[grepl("^(?i)BB", bonnrepeats$Sample)] <- "Buoyem"
bonnrepeats$Site[grepl("^(?i)FB", bonnrepeats$Sample)] <- "Forikrom"

visreg(glm(changeEBOV ~ Site, data=bonnrepeats))
visreg(glm(changeMARV ~ Site, data=bonnrepeats))

##two possible seroconversions here? also went up for MARV but not by that much

#rescaled
bonnfiga <- bonn %>% ggplot(aes(y=logEBOVrs, x=Year)) +
  geom_line(aes(group=factor(Sample), colour = Site), size=0.4, alpha=0.5) +
  #theme(legend.position = "none") +
  geom_point(size=0.5, aes(colour=Site)) +
  geom_hline(yintercept = 1, colour="black") +
  theme_minimal() + ylab("Corrected EBOV lnMFI") + xlab("Year (2011-2012)") +
  theme(axis.text.x = element_blank())

#rescaled
bonnfigb <-bonn %>% ggplot(aes(y=logMARVrs, x=Year)) +
  geom_line(aes(group=factor(Sample), colour = Site), size=0.4, alpha=0.5) +
  #theme(legend.position = "none") +
  geom_point(size=0.5, aes(colour=Site)) +
  geom_hline(yintercept = 1, colour="black") +
  theme_minimal() + ylab("Corrected MARV lnMFI") + xlab("Year (2011-2012)") +
  theme(axis.text.x = element_blank())

bonnfig <- ggarrange(bonnfiga, bonnfigb, nrow=2, labels=c("A","B"))
ggsave("bonnfig1.jpg", bonnfig, height=8, width=7)

ggplot(bonn) + 
  geom_line(aes(y=MARV, x=Year, group=Sample), size=0.05, color="red") +
  geom_line(aes(y=EBOV, x=Year, group=Sample), size=0.05, color="blue") +
  theme_classic()
#+ scale_x_continuous(name ="Year", limits=c("2011","2012"))

class(bonn$Year)

ggplot(bonn) + 
  geom_line(aes(y=logMARVrs, x=Year, group=Sample), size=0.1, color="red") +
  geom_line(aes(y=logEBOVrs, x=Year, group=Sample), size=0.1, color="blue") +
  ylab("rescaled log MFI") +
  theme_classic() #+ scale_x_continuous(name ="Year", limits=c("2011","2012"))

bonnsimple <- bonn %>% select(Site, Sample, logEBOVrs, logMARVrs, Year) %>%
  pivot_wider(names_from = Year, values_from = c(logEBOVrs, logMARVrs))

bonnsimple <- bonnsimple[complete.cases(bonnsimple),]
bonnsimple$marvchange <- bonnsimple$logMARVrs_2012 - bonnsimple$logMARVrs_2011
bonnsimple$ebovchange <- bonnsimple$logEBOVrs_2012 - bonnsimple$logEBOVrs_2011

bonnsimple$Site <- as.factor(bonnsimple$Site)
bonnsimple$Site <- relevel(bonnsimple$Site, ref = "Forikrom")

summary(glm(ebovchange ~ Site, data=bonnsimple))
summary(glm(marvchange ~ Site, data=bonnsimple))

###correlation between antigens

cor.test(allhuman$logEBOVrs, allhuman$logMARVrs)
