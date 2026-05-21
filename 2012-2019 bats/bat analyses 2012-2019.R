###2012-2019 bat data
## maya juman

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(ggpubr)
library(visreg)
library(mclust)

## clean up space
rm(list=ls())
graphics.off()

#read data in
bats <- read.csv("bats.csv")

##simplify sites
bats$SiteSimple <- bats$Site
bats$SiteSimple[which(bats$SiteSimple == "Bouyem (MQ)")] <- "Buoyem"
bats$SiteSimple[which(bats$SiteSimple == "Bouyem (DC)")] <- "Buoyem"

##remove captive eidolon
bats_wild <- bats[-which(bats$Site == "Accra Zoo"),]

###basic boxplots

#EBOV
ggplot(bats[!is.na(bats$Sex),], aes(Sex, logEBOVrs, fill=Sex)) + geom_boxplot() +
  ggtitle("EBOV by Sex")
ggplot(bats, aes(Site, logEBOVrs, fill=Site)) + geom_boxplot() +
  ggtitle("EBOV by Site") + theme(legend.position = "none")
ggplot(bats[!is.na(bats$Age),], aes(Age, logEBOVrs, fill=Age)) + geom_boxplot() +
  ggtitle("EBOV by Age")+ theme(legend.position = "none")
ggplot(bats[!is.na(bats$ReproSex),], aes(ReproSex, logEBOVrs, fill=ReproSex)) + geom_boxplot() +
  ggtitle("EBOV by Repro Status") + theme(legend.position = "none")
ggplot(bats[!is.na(bats$Species),], aes(Species, logEBOVrs, fill=Species)) + geom_boxplot()
ggplot(bats %>% filter(Species == "Eidolon helvum" | 
                         Species == "Epomophorus gambianus" | 
                         Species == "Rousettus aegyptiacus"), 
       aes(Species, logEBOVrs, fill=Species)) + geom_boxplot()

#MARV
ggplot(bats[!is.na(bats$Sex),], aes(Sex, logMARVrs, fill=Sex)) + geom_boxplot() +
  ggtitle("MARV by Sex")
ggplot(bats, aes(Site, logMARVrs, fill=Site)) + geom_boxplot() +
  ggtitle("MARV by Site") + theme(legend.position = "none")
ggplot(bats[!is.na(bats$Age),], aes(Age, logMARVrs, fill=Age)) + geom_boxplot() +
  ggtitle("MARV by Age")+ theme(legend.position = "none")
ggplot(bats[!is.na(bats$ReproSex),], aes(ReproSex, logMARVrs, fill=ReproSex)) + geom_boxplot() +
  ggtitle("MARV by Repro Status") + theme(legend.position = "none")
ggplot(bats %>% filter(Species == "Eidolon helvum" | 
                         Species == "Epomophorus gambianus" | 
                         Species == "Rousettus aegyptiacus"), 
       aes(Species, logMARVrs, fill=Species)) + geom_boxplot()

###split boxplots by species

eidolon <- bats_wild[which(bats_wild$Species == "Eidolon helvum"),]
epomophorus <- bats[which(bats$Species == "Epomophorus gambianus"),]
rousettus <- bats[which(bats$Species == "Rousettus aegyptiacus"),]

###eidolon

ggplot(eidolon[!is.na(eidolon$Sex),], aes(Sex, logEBOVrs, fill=Sex)) + geom_boxplot()
ggplot(eidolon, aes(Site, logEBOVrs, fill=Site)) + geom_boxplot()
ggplot(eidolon[!is.na(eidolon$Age),], aes(Age, logEBOVrs, fill=Age)) + geom_boxplot()
ggplot(eidolon[!is.na(eidolon$ReproSex),], aes(ReproSex, logEBOVrs, fill=ReproSex)) + geom_boxplot()

ggplot(eidolon[!is.na(eidolon$Sex),], aes(Sex, logMARVrs, fill=Sex)) + geom_boxplot()
ggplot(eidolon, aes(Site, logMARVrs, fill=Site)) + geom_boxplot()
ggplot(eidolon[!is.na(eidolon$Age),], aes(Age, logMARVrs, fill=Age)) + geom_boxplot()
ggplot(eidolon[!is.na(eidolon$ReproSex),], aes(ReproSex, logMARVrs, fill=ReproSex)) + geom_boxplot()

###epomophorus

ggplot(epomophorus[!is.na(epomophorus$Sex),], aes(Sex, logEBOVrs, fill=Sex)) + geom_boxplot()
ggplot(epomophorus, aes(Site, logEBOVrs, fill=Site)) + geom_boxplot()
ggplot(epomophorus[!is.na(epomophorus$Age),], aes(Age, logEBOVrs, fill=Age)) + geom_boxplot()
ggplot(epomophorus[!is.na(epomophorus$ReproSex),], aes(ReproSex, logEBOVrs, fill=ReproSex)) + 
  geom_boxplot() + ggtitle("EBOV by Repro Status in Epomophorus") +
  theme(legend.position = "none")

ggplot(epomophorus[!is.na(epomophorus$Sex),], aes(Sex, logMARVrs, fill=Sex)) + geom_boxplot()
ggplot(epomophorus, aes(Site, logMARVrs, fill=Site)) + geom_boxplot() + ggtitle("MARV by Site in Epomophorus")
ggplot(epomophorus[!is.na(epomophorus$Age),], aes(Age, logMARVrs, fill=Age)) + geom_boxplot()
ggplot(epomophorus[!is.na(epomophorus$ReproSex),], aes(ReproSex, logMARVrs, fill=ReproSex)) + geom_boxplot()

summary(glm(logMARVrs~Site, data=eidolon))

###rousettus

ggplot(rousettus[!is.na(rousettus$Sex),], aes(Sex, logEBOVrs, fill=Sex)) + geom_boxplot()
ggplot(rousettus, aes(Site, logEBOVrs, fill=Site)) + geom_boxplot()
ggplot(rousettus[!is.na(rousettus$Age),], aes(Age, logEBOVrs, fill=Age)) + geom_boxplot()
ggplot(rousettus[!is.na(rousettus$ReproSex),], aes(ReproSex, logEBOVrs, fill=ReproSex)) + geom_boxplot() +
  ggtitle("EBOV by Repro Status in Rousettus") + theme(legend.position = "none")

ggplot(rousettus[!is.na(rousettus$Sex),], aes(Sex, logMARVrs, fill=Sex)) + geom_boxplot()
ggplot(rousettus, aes(Site, logMARVrs, fill=Site)) + geom_boxplot()
ggplot(rousettus[!is.na(rousettus$Age),], aes(Age, logMARVrs, fill=Age)) + geom_boxplot() + facet_grid(cols=vars(Sex))
ggplot(rousettus[!is.na(rousettus$ReproSex),], aes(ReproSex, logMARVrs, fill=ReproSex)) + 
  geom_boxplot() + theme(legend.position = "none") + theme_classic() + xlab("Reproductive Status / Sex")

###temporal trends

ggplot(eidolon, aes(Sampling.Date, logEBOVrs)) + geom_boxplot()
ggplot(epomophorus, aes(Sampling.Date, logEBOVrs)) + geom_boxplot()
ggplot(rousettus, aes(Sampling.Date, logEBOVrs)) + geom_boxplot()

ggplot(eidolon, aes(Year, logEBOVrs)) + geom_boxplot()
ggplot(epomophorus, aes(Year, logEBOVrs)) + geom_boxplot()
ggplot(rousettus, aes(Year, logEBOVrs)) + geom_boxplot()

ggplot(eidolon, aes(Sampling.Date, logMARVrs)) + geom_boxplot()
ggplot(epomophorus, aes(Sampling.Date, logMARVrs)) + geom_boxplot()
ggplot(rousettus, aes(Sampling.Date, logMARVrs)) + geom_boxplot()

ggplot(eidolon, aes(Year, logMARVrs)) + geom_boxplot()
ggplot(epomophorus, aes(Year, logMARVrs)) + geom_boxplot()
ggplot(rousettus, aes(Year, logMARVrs)) + geom_boxplot()

bats$date <- as.numeric(as.Date(bats$Sampling.Date))
bats$Sex <- as.factor(bats$Sex)
bats$Age <- as.factor(bats$Age)
bats$Species <- as.factor(bats$Species)
bats$ReproSex <- as.factor(bats$ReproSex)

library(mgcv)
ebovgam <- (gamm(logEBOVrs~s(date, k=5, by=Species)+ReproSex,
             data=bats %>% filter(Species == "Eidolon helvum" | 
                                  Species == "Epomophorus gambianus" | 
                                  Species == "Rousettus aegyptiacus") %>%
               filter(Site != "Accra Zoo") %>% drop_na(Sex, Age, BodyCondition, date))$gam)
marvgam <- (gamm(logMARVrs~s(date, k=5, by=Species)+ReproSex,
             data=bats %>% filter(Species == "Eidolon helvum" | 
                                    Species == "Epomophorus gambianus" | 
                                    Species == "Rousettus aegyptiacus") %>% 
               filter(Site != "Accra Zoo") %>% drop_na(Sex, Age, BodyCondition, date))$gam)

summary(ebovgam)
plot(ebovgam)
summary(marvgam)
plot(marvgam)

bats$ReproSex <- relevel(bats$ReproSex, ref="Not pregnant F")
summary(gamm(logMARVrs~s(date, k=5, by=Species)+Repro.Status+Sex+BodyCondition,
                 data=bats %>% filter(Species == "Rousettus aegyptiacus") %>% 
               drop_na(Sex, Age, BodyCondition, date))$gam)

##attempt to plot accra zoo by repeats

#read in captive bat spreadsheet with IDs
az <- read.csv("Accra Zoo all samples.csv")

az$Sex[which(az$Sex == "?")] <- NA
az$Sex[which(az$Sex == "UNK")] <- NA

id <- az %>% group_by(Bat.ID) %>% count() %>% filter(n > 1)
az_recap <- az %>%  filter(Bat.ID %in% id$Bat.ID)

ccfiga <- az_recap[!is.na(az_recap$Sex),] %>% ggplot(aes(y=logEBOVrs, x=Sampling.Date)) +
  geom_line(aes(group=factor(Bat.ID), col=Sex), size=0.1) +
  theme(legend.position = "none") +
  geom_point(size=0.7, aes(col=Sex)) +
  theme_minimal() + ylab("Corrected EBOV lnMFI") +
  theme(axis.text.x = element_blank())

ccfigb <- az_recap[!is.na(az_recap$Sex),] %>% ggplot(aes(y=logMARVrs, x=Sampling.Date)) +
  geom_line(aes(group=factor(Bat.ID), col=Sex), size=0.1) +
  theme(legend.position = "none") +
  geom_point(size=0.7, aes(col=Sex)) +
  theme_minimal() + ylab("Corrected MARV lnMFI") +
  theme(axis.text.x = element_blank())

ccfig <- ggarrange(ccfiga, ccfigb, nrow=2, labels=c("A","B"))
ggsave("captive fig.jpg", ccfig, height=6, width=6)

az$date <- as.numeric(as.Date(az$Sampling.Date))
az$Sex <- as.factor(az$Sex)

az$ReproSex <- as.character(az$Sex)
az$ReproSex[which(az$Sex == "F")] <- az$Repro.Status[which(az$Sex == "F")]
az$ReproSex[which(az$ReproSex == "L")] <- "Pregnant/Lactating"
az$ReproSex[which(az$ReproSex == "Pregnant")] <- "Pregnant/Lactating"
az$ReproSex[which(az$ReproSex == "P")] <- "Pregnant/Lactating"
az$ReproSex[which(az$ReproSex == "pregnant")] <- "Pregnant/Lactating"
az$ReproSex[which(az$ReproSex == "NRS")] <- "Unknown"
az$ReproSex[which(az$ReproSex == "NP")] <- "Not pregnant F"
az$ReproSex[which(az$ReproSex == "?")] <- "Unknown"
az$ReproSex[which(az$Age == "J")] <- "Sexually immature"
az$ReproSex[which(az$Age == "Juv")] <- "Sexually immature"
az$ReproSex[which(az$Age == "JUV")] <- "Sexually immature"
az$ReproSex[which(az$Age == "SA")] <- "Sexually immature"
az$ReproSex[which(az$Age == "SI")] <- "Sexually immature"
az$ReproSex[which(az$Age == "SIM")] <- "Sexually immature"
az$ReproSex[which(az$Age == "UNK")] <- "Unknown"

az$ReproSex <- as.factor(az$ReproSex)
az$Bat.ID <- as.factor(az$Bat.ID)

##GAMMs in the captive colony
az_ebovgam<-((gamm(logEBOVrs~s(date, bs="cr", k=5, by=Sex)+ReproSex,
             data=az %>% filter(Age == "A"),random=list(Bat.ID=~1))))
az_marvgam<-((gamm(logMARVrs~s(date, bs="cr", k=5, by=Sex)+ReproSex,
                   data=az %>% filter(Age == "A"),random=list(Bat.ID=~1))))

summary(az_ebovgam$gam)
plot(az_ebovgam$gam)
summary(az_marvgam$gam)
plot(az_marvgam$gam)

ggplot(bats) + geom_density(aes(x=logEBOVrs), fill="grey") + 
  geom_vline(xintercept = 0) + ggtitle("rescaled EBOV affinities, aggregate")

ggplot(bats) + geom_density(aes(x=logMARVrs), fill="grey") + 
  geom_vline(xintercept = 0) + ggtitle("rescaled MARV affinities, aggregate")

####MFI distributions of three main species

###ebov rescaled
ggplot(bats %>% filter(Species %in% c("Eidolon helvum","Epomophorus gambianus","Rousettus aegyptiacus"))) + 
  geom_density(aes(x=logEBOVrs, fill=Species, col=Species), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1, col="red") + 
  ggtitle("log rescaled EBOV affinities by species") + xlim(c(-0.2,1.2))

ggplot(bats %>% filter(Species %in% c("Eidolon helvum","Epomophorus gambianus","Rousettus aegyptiacus"))) + 
  geom_density(aes(x=logEBOVrs, fill=Species), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  #geom_vline(xintercept = 1) + 
  xlim(c(-0.2,1.2)) +
  theme_classic() + xlab("Corrected EBOV lnMFI") + ylab("Density") +
  theme(legend.position = c(0.67,0.9)) +
  scale_fill_manual(values=c("darkgreen","blue","red"))

###marv rescaled
ggplot(bats %>% filter(Species %in% c("Eidolon helvum","Epomophorus gambianus","Rousettus aegyptiacus"))) + 
  geom_density(aes(x=logMARVrs, fill=Species, col=Species), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1, col="red") + 
  ggtitle("log rescaled MARV affinities by species") + xlim(c(-0.2,1.2))

###marv rescaled
ggplot(bats %>% drop_na(ReproSex)) + 
  geom_density(aes(x=logMARVrs, fill=ReproSex, col=ReproSex), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1) + xlim(c(-0.2,1.2)) +
  theme_classic() + xlab("log MARV MFI, rescaled") +
  theme(legend.position = c(0.67,0.9))

###########GLMs

summary(glm(logMARVrs~Site, data=(bats_wild %>% filter(Species == "Eidolon helvum"))))
summary(glm(logMARVrs~Site, data=(bats_wild %>% filter(!Species == "Rousettus aegyptiacus"))))

nonrous <- bats_wild %>% filter(!Species == "Rousettus aegyptiacus")

coroostmod_marv_epo <- (glm(logMARVrs~SiteSimple, data=epomophorus))
coroostmod_marv_eid <- (glm(logMARVrs~SiteSimple, data=eidolon))

coroostmod_ebov_epo <- (glm(logEBOVrs~SiteSimple, data=epomophorus))
coroostmod_ebov_eid <- (glm(logEBOVrs~SiteSimple, data=eidolon))

#report these
summary(coroostmod_marv_epo)
summary(coroostmod_ebov_epo)

summary(coroostmod_marv_eid)
summary(coroostmod_ebov_eid)

visreg(coroostmod_marv_epo,"SiteSimple",xlab="Site", ylab="MARV lnMFI")
visreg(coroostmod_ebov_epo,"SiteSimple",xlab="Site", ylab="EBOV lnMFI")

epomophorus$rousettus <- "N"
epomophorus$rousettus[which(epomophorus$SiteSimple == "Buoyem")] <- "Y"
epomophorus$rousettus[which(epomophorus$SiteSimple == "Ve-Golokwati")] <- "Y"

epo_EBOV <- ggplot(epomophorus, aes(SiteSimple, logEBOVrs, fill = rousettus)) + 
  geom_boxplot() + 
  theme_classic() + ylab("Corrected EBOV lnMFI") + xlab("Site") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("white","red"))

epo_MARV <- ggplot(epomophorus, aes(SiteSimple, logMARVrs, fill = rousettus)) + 
  geom_boxplot() + 
  theme_classic() + ylab("Corrected MARV lnMFI") + xlab("Site") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("white","red"))

figs2 <- ggarrange(epo_EBOV, epo_MARV, ncol=2, labels=c("A","B"))
ggsave("Fig S2.jpg", figs2, height=5, width=12)

##gamms

bats_wild$date <- as.numeric(as.Date(bats_wild$Sampling.Date))
bats_wild$Sex <- as.factor(bats_wild$Sex)
bats_wild$Age <- as.factor(bats_wild$Age)
bats_wild$Species <- as.factor(bats_wild$Species)
bats_wild$ReproSex <- as.factor(bats_wild$ReproSex)
bats_wild2 <- bats_wild[-which(bats_wild$ReproSex == "Unknown"),]

library(mgcv)
ebovgam <- (gamm(logEBOVrs~s(date, k=25, by=Species)+ReproSex+BodyCondition,
                 data=bats_wild2 %>% filter(Species == "Eidolon helvum" | 
                                        Species == "Epomophorus gambianus" | 
                                        Species == "Rousettus aegyptiacus") %>%
                  drop_na(Sex, Age, BodyCondition, date))$gam)
marvgam <- (gamm(logMARVrs~s(date, k=25, by=Species)+ReproSex+BodyCondition,
                 data=bats_wild2 %>% filter(Species == "Eidolon helvum" | 
                                        Species == "Epomophorus gambianus" | 
                                        Species == "Rousettus aegyptiacus") %>% 
                   drop_na(Sex, Age, BodyCondition, date))$gam)

summary(ebovgam)
summary(marvgam)
plot(ebovgam)
plot(marvgam)

k.check(ebovgam) #edf < k', and non-significant resids, so this is OK
k.check(marvgam) #edf < k', and non-significant resids, so this is OK

newdata <- data.frame(date=rep(15631:17650, 3))
newdata$Species[1:2020] <- "Eidolon helvum"
newdata$Species[2021:4040] <- "Epomophorus gambianus"
newdata$Species[4041:6060] <- "Rousettus aegyptiacus"
newdata$BodyCondition <- mean(bats_wild2$BodyCondition, na.rm=TRUE)
newdata$ReproSex <- "M" #arbitrary value - coefficients are so small that it doesn't matter

predEBOV <- bind_cols(newdata, as.data.frame(predict(ebovgam, newdata=newdata, type = "link", se.fit=TRUE)))
predEBOV$date <- as.POSIXct(as.Date(predEBOV$date, origin = "1970-01-01"))
predEBOV$upr <- predEBOV$fit + (1.96 * predEBOV$se.fit)
predEBOV$lwr <- predEBOV$fit - (1.96 * predEBOV$se.fit)

EBOVgamplot <- ggplot() + 
  geom_line(data=predEBOV, aes(x=date, y=fit, colour=Species), size=1) +
  geom_ribbon(data=predEBOV, aes(x=date, ymin=lwr, ymax=upr, fill=Species), alpha=0.2) +
  ylim(-2.5,2.6) + ylab("Corrected EBOV lnMFI values") + xlab("Date") +
  scale_color_manual(values=c("darkgreen","blue","red")) + 
  scale_fill_manual(values=c("green","blue","red")) + 
  theme_classic() + theme(legend.position = c(0.18,0.9), legend.title = element_blank())

predMARV <- bind_cols(newdata, as.data.frame(predict(marvgam, newdata=newdata, type = "link", se.fit=TRUE)))
predMARV$date <- as.POSIXct(as.Date(predMARV$date, origin = "1970-01-01"))
predMARV$upr <- predMARV$fit + (1.96 * predMARV$se.fit)
predMARV$lwr <- predMARV$fit - (1.96 * predMARV$se.fit)

MARVgamplot <- ggplot() + geom_line(data=predMARV, aes(x=date, y=fit, colour=Species), size=1) +
  geom_ribbon(data=predMARV, aes(x=date, ymin=lwr, ymax=upr, fill=Species), alpha=0.2) +
  ylim(-3.2,4.4) + ylab("Corrected MARV lnMFI values") + xlab("Date") +
  scale_color_manual(values=c("darkgreen","blue","red")) + 
  scale_fill_manual(values=c("green","blue","red")) + 
  theme_classic() + theme(legend.position = c(0.18,0.9), legend.title = element_blank())

gamplot <- ggarrange(EBOVgamplot, MARVgamplot, nrow=2, labels=c("A","B"))
ggsave("gamplot.png", gamplot, height=10, width=6)


rousettus$date <- as.numeric(as.Date(rousettus$Sampling.Date))
rousettus$Sex <- as.factor(rousettus$Sex)
rousettus$Age <- as.factor(rousettus$Age)
rousettus$Species <- as.factor(rousettus$Species)
rousettus$ReproSex <- as.factor(rousettus$ReproSex)
rousettus2 <- rousettus[-which(rousettus$ReproSex == "Unknown"),]

ebovgam_rous <- (gamm(logEBOVrs~s(date, k=5)+ReproSex+BodyCondition,
                 data=rousettus2 %>% drop_na(Sex, Age, BodyCondition, date))$gam)
marvgam_rous <- (gamm(logMARVrs~s(date, k=5)+ReproSex+BodyCondition,
                      data=rousettus2 %>% drop_na(Sex, Age, BodyCondition, date))$gam)

summary(ebovgam_rous)
summary(marvgam_rous)
plot(ebovgam_rous)
plot(marvgam_rous)

newdata2 <- data.frame(date=rep(15631:17650, 1))
newdata2$BodyCondition <- mean(bats_wild2$BodyCondition, na.rm=TRUE)
newdata2$ReproSex <- "M" #arbitrary value - coefficients are so small that it doesn't matter

predMARV <- bind_cols(newdata2, as.data.frame(predict(marvgam_rous, newdata=newdata2, type = "link", se.fit=TRUE)))
predMARV$date <- as.POSIXct(as.Date(predMARV$date, origin = "1970-01-01"))
predMARV$upr <- predMARV$fit + (1.96 * predMARV$se.fit)
predMARV$lwr <- predMARV$fit - (1.96 * predMARV$se.fit)

ggplot() + geom_line(data=predMARV, aes(x=date, y=fit), size=1, color="red") +
  geom_ribbon(data=predMARV, aes(x=date, ymin=lwr, ymax=upr), alpha=0.2) +
  ylim(0,1) + ylab("Corrected MARV lnMFI values") + xlab("Date") +
  theme_classic() + theme(legend.position = c(0.18,0.9), legend.title = element_blank())

#overall correlation bw EBOV and MARV
cor.test(bats_wild$logEBOVrs, bats_wild$logMARVrs)

scatterplot <- ggplot(bats_wild %>% filter(Species %in% c("Eidolon helvum","Epomophorus gambianus","Rousettus aegyptiacus")), 
       aes(x=logEBOVrs, y=logMARVrs, colour=Species)) +
  geom_point(size=3, alpha=0.4) +
  theme_classic() + ylab("Corrected MARV lnMFI") + xlab("Corrected EBOV lnMFI") +
  scale_fill_manual(values=c("darkgreen","blue","red")) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  theme(legend.text = element_text(face="italic"), legend.title = element_blank()) +
  xlim(c(-0.8,1.2)) + ylim(c(-0.8,1.2))

ggsave("scatterplot.png", scatterplot, height=6, width=8)

#######################CUTOFF DETERMINATIONS WITH MIXTURE MODELS

rousettus_marv_clust <- Mclust(rousettus$logMARVrs, G = 1:3, modelNames = "V", verbose = FALSE)
plot(rousettus_marv_clust, what = "BIC", xlab = "Rousettus MARV MFI rescaled")
plot(rousettus_marv_clust, what = "density", xlab = "Rousettus MARV MFI rescaled")

rous_threshold_low <- max(rousettus_marv_clust$data[rousettus_marv_clust$z[, 1] > 0.95])
rous_threshold_hi <- min(rousettus_marv_clust$data[rousettus_marv_clust$z[, 2] > 0.95])
rous_threshold_50 <- max(rousettus_marv_clust$data[rousettus_marv_clust$z[, 1] > 0.5])

# Add lines for individual components of the model by extracting the mean,
# variance, and proportion parameters from the model
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01),         # generate a normal distribution
                                   rousettus_marv_clust$parameters$mean[1], # with mean of the first cluster
                                   sqrt(rousettus_marv_clust$parameters$variance$sigmasq[1]))* # and SD of the first cluster
        rousettus_marv_clust$parameters$pro[1],      # Times the proportion of values in the cluster
      col = "blue", lty = 2)              # colored blue and dotted
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), rousettus_marv_clust$parameters$mean[2], 
                                   sqrt(rousettus_marv_clust$parameters$variance$sigmasq[2]))*rousettus_marv_clust$parameters$pro[2], col = "red", lty = 2)
abline(v = rous_threshold_50, lty = 3)

# Add empirical density of the data
lines(density(rousettus$logMARVrs), col = "purple") 

plot(rousettus_marv_clust, what = "classification", xlab = "Rousettus MARV MFI rescaled")
plot(rousettus_marv_clust, what = "uncertainty", xlab = "Rousettus MARV MFI rescaled")

###rousettus MARV

rousettus_marv_clust <- Mclust(rousettus$logMARVrs, G = 1:3, verbose = FALSE)
plot(rousettus_marv_clust, what = "BIC", xlab = "Rousettus MARV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
R_M_cutoff <- as.numeric(rousettus_marv_clust$parameters$mean[1]) + 3*sqrt(rousettus_marv_clust$parameters$variance$sigmasq[1])

rousettus_marv_bootClust <- MclustBootstrap(rousettus_marv_clust)
R_M_cutoff_low <- summary(rousettus_marv_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(rousettus_marv_bootClust, what = "ci")$variance[1]))
R_M_cutoff_hi <- summary(rousettus_marv_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(rousettus_marv_bootClust, what = "ci")$variance[2]))

R_M_plot <- ggplot(rousettus) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected Rousettus MARV lnMFI") + ylab("Count") +
  geom_vline(xintercept = R_M_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = R_M_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = R_M_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = sqrt(rousettus_marv_clust$parameters$variance$sigmasq[1]),
                                        sd = sqrt(rousettus_marv_clust$parameters$variance$sigmasq[1]))*
                  rousettus_marv_clust$parameters$pro[1]*418*0.05,
                color = "red", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(rousettus_marv_clust$parameters$mean[2]), 
                                        sd = sqrt(rousettus_marv_clust$parameters$variance$sigmasq[2]))*
                  rousettus_marv_clust$parameters$pro[2]*418*0.05,
                color = "red", size = 0.5) 

###rousettus EBOV

rousettus_ebov_clust <- Mclust(rousettus$logEBOVrs, G = 1:3, verbose = FALSE)
plot(rousettus_ebov_clust, what = "BIC", xlab = "Rousettus EBOV MFI rescaled") #1 cluster - no cutoff

R_E_plot <- ggplot(rousettus) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected Rousettus EBOV lnMFI") + ylab("Count") +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(rousettus_ebov_clust$parameters$mean[1]), 
                                        sd = sqrt(rousettus_ebov_clust$parameters$variance$sigmasq[1]))*
                  rousettus_ebov_clust$parameters$pro[1]*418*0.05,
                color = "red", size = 0.5)

###eidolon EBOV

eidolon_ebov_clust <- Mclust(eidolon$logEBOVrs, G = 1:3, verbose = FALSE)
plot(eidolon_ebov_clust, what = "BIC", xlab = "Eidolon EBOV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Ei_E_cutoff <- as.numeric(eidolon_ebov_clust$parameters$mean[1]) + 3*sqrt(eidolon_ebov_clust$parameters$variance$sigmasq[1])

eidolon_ebov_bootClust <- MclustBootstrap(eidolon_ebov_clust)
Ei_E_cutoff_low <- summary(eidolon_ebov_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(eidolon_ebov_bootClust, what = "ci")$variance[1]))
Ei_E_cutoff_hi <- summary(eidolon_ebov_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(eidolon_ebov_bootClust, what = "ci")$variance[2]))

Ei_E_plot <- ggplot(eidolon) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected Eidolon EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ei_E_cutoff, colour="green",size=1) + 
  geom_vline(xintercept = Ei_E_cutoff_low, colour="green", lty = "dashed", size=1) +
  geom_vline(xintercept = Ei_E_cutoff_hi, colour="green", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eidolon_ebov_clust$parameters$mean[1]),
                                        sd = sqrt(eidolon_ebov_clust$parameters$variance$sigmasq[1]))*
                  eidolon_ebov_clust$parameters$pro[1]*1050*0.05,
                color = "green", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eidolon_ebov_clust$parameters$mean[2]), 
                                        sd = sqrt(eidolon_ebov_clust$parameters$variance$sigmasq[2]))*
                  eidolon_ebov_clust$parameters$pro[2]*1050*0.05,
                color = "green", size = 0.5) 

###eidolon MARV

eidolon2 <- eidolon[-which(eidolon$logMARVrs > 0.2),]

eidolon_marv_clust <- Mclust(eidolon$logMARVrs, G = 1:3, verbose = FALSE)
plot(eidolon_marv_clust, what = "BIC", xlab = "Eidolon MARV MFI rescaled") #3 clusters
plot(eidolon_marv_clust, what = "density", xlab = "Eidolon MARV MFI rescaled") #3 clusters
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01),         # generate a normal distribution
                                   eidolon_marv_clust$parameters$mean[1], # with mean of the first cluster
                                   sqrt(eidolon_marv_clust$parameters$variance$sigmasq[1]))* # and SD of the first cluster
        eidolon_marv_clust$parameters$pro[1],      # Times the proportion of values in the cluster
      col = "blue", lty = 2)              # colored blue and dotted
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), eidolon_marv_clust$parameters$mean[2], 
                                   sqrt(eidolon_marv_clust$parameters$variance$sigmasq[2]))*eidolon_marv_clust$parameters$pro[2], col = "red", lty = 2)
lines(seq(-1, 6, by = 0.01), dnorm(seq(-1, 6, by = 0.01), eidolon_marv_clust$parameters$mean[3], 
                                   sqrt(eidolon_marv_clust$parameters$variance$sigmasq[3]))*eidolon_marv_clust$parameters$pro[3], col = "green", lty = 2)

# Add empirical density of the data
lines(density(eidolon$logMARVrs), col = "purple") 

#cutoff - mean of first cluster + 3x standard deviation
Ei_M_cutoff <- as.numeric(eidolon_marv_clust$parameters$mean[2]) + 3*sqrt(eidolon_marv_clust$parameters$variance$sigmasq[2])

eidolon_marv_bootClust <- MclustBootstrap(eidolon_marv_clust)
Ei_M_cutoff_low <- summary(eidolon_marv_bootClust, what = "ci")$mean[3] + (3*sqrt(summary(eidolon_marv_bootClust, what = "ci")$variance[3]))
Ei_M_cutoff_hi <- summary(eidolon_marv_bootClust, what = "ci")$mean[4] + (3*sqrt(summary(eidolon_marv_bootClust, what = "ci")$variance[4]))

Ei_M_plot <- ggplot(eidolon) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected Eidolon MARV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ei_M_cutoff, colour="green",size=1) + 
  geom_vline(xintercept = Ei_M_cutoff_low, colour="green", lty = "dashed", size=1) +
  geom_vline(xintercept = Ei_M_cutoff_hi, colour="green", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eidolon_marv_clust$parameters$mean[1]),
                                        sd = sqrt(eidolon_marv_clust$parameters$variance$sigmasq[1]))*
                  eidolon_marv_clust$parameters$pro[1]*1050*0.05,
                color = "green", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eidolon_marv_clust$parameters$mean[2]), 
                                        sd = sqrt(eidolon_marv_clust$parameters$variance$sigmasq[2]))*
                  eidolon_marv_clust$parameters$pro[2]*1050*0.05,
                color = "green", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eidolon_marv_clust$parameters$mean[3]), 
                                        sd = sqrt(eidolon_marv_clust$parameters$variance$sigmasq[3]))*
                  eidolon_marv_clust$parameters$pro[3]*1050*0.05,
                color = "green", size = 0.5)

###epomophorus EBOV

epo_ebov_clust <- Mclust(epomophorus$logEBOVrs, G = 1:3, verbose = FALSE)
plot(epo_ebov_clust, what = "BIC", xlab = "Epomophorus EBOV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Ep_E_cutoff <- as.numeric(epo_ebov_clust$parameters$mean[2]) + 3*sqrt(epo_ebov_clust$parameters$variance$sigmasq[2])

epo_ebov_bootClust <- MclustBootstrap(epo_ebov_clust)
Ep_E_cutoff_low <- summary(epo_ebov_bootClust, what = "ci")$mean[3] + (3*sqrt(summary(epo_ebov_bootClust, what = "ci")$variance[3]))
Ep_E_cutoff_hi <- summary(epo_ebov_bootClust, what = "ci")$mean[4] + (3*sqrt(summary(epo_ebov_bootClust, what = "ci")$variance[4]))

Ep_E_plot <- ggplot(epomophorus) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected Epomophorus EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ep_E_cutoff, colour="blue",size=1) + 
  geom_vline(xintercept = Ep_E_cutoff_low, colour="blue", lty = "dashed", size=1) +
  geom_vline(xintercept = Ep_E_cutoff_hi, colour="blue", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(epo_ebov_clust$parameters$mean[1]),
                                        sd = sqrt(epo_ebov_clust$parameters$variance$sigmasq[1]))*
                  epo_ebov_clust$parameters$pro[1]*1478*0.05,
                color = "blue", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(epo_ebov_clust$parameters$mean[2]), 
                                        sd = sqrt(epo_ebov_clust$parameters$variance$sigmasq[2]))*
                  epo_ebov_clust$parameters$pro[2]*1478*0.05,
                color = "blue", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(epo_ebov_clust$parameters$mean[3]), 
                                        sd = sqrt(epo_ebov_clust$parameters$variance$sigmasq[3]))*
                  epo_ebov_clust$parameters$pro[3]*1478*0.05,
                color = "blue", size = 0.5) 

###epomophorus MARV

epo_marv_clust <- Mclust(epomophorus$logMARVrs, G = 1:3, verbose = FALSE)
plot(epo_marv_clust, what = "BIC", xlab = "Eidolon MARV MFI rescaled") #3 clusters

#cutoff - mean of first cluster + 3x standard deviation
Ep_M_cutoff <- as.numeric(epo_marv_clust$parameters$mean[1]) + 3*sqrt(epo_marv_clust$parameters$variance$sigmasq[1])

epo_marv_bootClust <- MclustBootstrap(epo_marv_clust)
Ep_M_cutoff_low <- summary(epo_marv_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(epo_marv_bootClust, what = "ci")$variance[1]))
Ep_M_cutoff_hi <- summary(epo_marv_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(epo_marv_bootClust, what = "ci")$variance[2]))

Ep_M_plot <- ggplot(epomophorus) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected Epomophorus MARV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ep_M_cutoff, colour="blue",size=1) + 
  geom_vline(xintercept = Ep_M_cutoff_low, colour="blue", lty = "dashed", size=1) +
  geom_vline(xintercept = Ep_M_cutoff_hi, colour="blue", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(epo_marv_clust$parameters$mean[1]),
                                        sd = sqrt(epo_marv_clust$parameters$variance$sigmasq[1]))*
                  epo_marv_clust$parameters$pro[1]*1478*0.05,
                color = "blue", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(epo_marv_clust$parameters$mean[2]), 
                                        sd = sqrt(epo_marv_clust$parameters$variance$sigmasq[2]))*
                  epo_marv_clust$parameters$pro[2]*1478*0.05,
                color = "blue", size = 0.5)


cutoff_fig <- ggarrange(R_E_plot, R_M_plot, 
                        Ei_E_plot, Ei_M_plot,
                        Ep_E_plot, Ep_M_plot,
                        ncol=2, nrow=3,
                        labels=c("A","B","C","D","E","F"))
ggsave("cutoffs.png", cutoff_fig, height=8, width=8)


##########################CUTOFFS FOR OTHER SPECIES

bats_wild %>% filter(Species == "Micropteropus pusillus")

x <- Mclust((bats_wild %>% 
                            filter(Species == "Nanonycteris veldkampii"))$logEBOVrs, 
                         G = 1:3, modelNames = "V", verbose = FALSE)

plot(x, what = "BIC") #3 clusters


###micropteropus pusillus EBOV

mp_ebov_clust <- Mclust((bats_wild %>% filter(Species == "Micropteropus pusillus"))$logEBOVrs, G = 1:3, verbose = FALSE)
plot(mp_ebov_clust, what = "BIC", xlab = "Micropteropus EBOV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Mp_E_cutoff <- as.numeric(mp_ebov_clust$parameters$mean[1]) + 3*sqrt(mp_ebov_clust$parameters$variance$sigmasq[1])

mp_ebov_bootClust <- MclustBootstrap(mp_ebov_clust)
Mp_E_cutoff_low <- summary(mp_ebov_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(mp_ebov_bootClust, what = "ci")$variance[1]))
Mp_E_cutoff_hi <- summary(mp_ebov_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(mp_ebov_bootClust, what = "ci")$variance[2]))

Mp_E_plot <- ggplot(bats_wild %>% filter(Species == "Micropteropus pusillus")) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected E. pusillus EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Mp_E_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Mp_E_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Mp_E_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(mp_ebov_clust$parameters$mean[1]),
                                        sd = sqrt(mp_ebov_clust$parameters$variance$sigmasq[1]))*
                  mp_ebov_clust$parameters$pro[1]*49*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(mp_ebov_clust$parameters$mean[2]), 
                                        sd = sqrt(mp_ebov_clust$parameters$variance$sigmasq[2]))*
                  mp_ebov_clust$parameters$pro[2]*49*0.05,
                color = "black", size = 0.5)

###micropteropus pusillus MARV

mp_marv_clust <- Mclust((bats_wild %>% filter(Species == "Micropteropus pusillus"))$logMARVrs, G = 1:3, verbose = FALSE)
plot(mp_marv_clust, what = "BIC", xlab = "Micropteropus MARV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Mp_M_cutoff <- as.numeric(mp_marv_clust$parameters$mean[2]) + 3*sqrt(mp_marv_clust$parameters$variance$sigmasq)

mp_marv_bootClust <- MclustBootstrap(mp_marv_clust)
Mp_M_cutoff_low <- summary(mp_marv_bootClust, what = "ci")$mean[3] + (3*sqrt(summary(mp_marv_bootClust, what = "ci")$variance[3]))
Mp_M_cutoff_hi <- summary(mp_marv_bootClust, what = "ci")$mean[4] + (3*sqrt(summary(mp_marv_bootClust, what = "ci")$variance[4]))

Mp_M_plot <- ggplot(bats_wild %>% filter(Species == "Micropteropus pusillus")) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected E. pusillus MARV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Mp_M_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Mp_M_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Mp_M_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(mp_marv_clust$parameters$mean[1]),
                                        sd = sqrt(mp_marv_clust$parameters$variance$sigmasq[1]))*
                  mp_marv_clust$parameters$pro[1]*49*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(mp_marv_clust$parameters$mean[2]), 
                                        sd = sqrt(mp_marv_clust$parameters$variance$sigmasq))*
                  mp_marv_clust$parameters$pro[2]*49*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(mp_marv_clust$parameters$mean[3]), 
                                        sd = sqrt(mp_marv_clust$parameters$variance$sigmasq))*
                  mp_marv_clust$parameters$pro[3]*49*0.05,
                color = "black", size = 0.5)

###Epomops franqueti EBOV

ef_ebov_clust <- Mclust((bats_wild %>% filter(Species == "Epomops franqueti"))$logEBOVrs, G = 1:3, verbose = FALSE)
plot(ef_ebov_clust, what = "BIC", xlab = "Epomops EBOV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Ef_E_cutoff <- as.numeric(ef_ebov_clust$parameters$mean[1]) + 3*sqrt(ef_ebov_clust$parameters$variance$sigmasq[1])

ef_ebov_bootClust <- MclustBootstrap(ef_ebov_clust)
Ef_E_cutoff_low <- summary(ef_ebov_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(ef_ebov_bootClust, what = "ci")$variance[1]))
Ef_E_cutoff_hi <- summary(ef_ebov_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(ef_ebov_bootClust, what = "ci")$variance[2]))

Ef_E_plot <- ggplot(bats_wild %>% filter(Species == "Epomops franqueti")) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected E. franqueti EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ef_E_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Ef_E_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Ef_E_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ef_ebov_clust$parameters$mean[1]),
                                        sd = sqrt(ef_ebov_clust$parameters$variance$sigmasq[1]))*
                  ef_ebov_clust$parameters$pro[1]*42*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ef_ebov_clust$parameters$mean[2]), 
                                        sd = sqrt(ef_ebov_clust$parameters$variance$sigmasq[2]))*
                  ef_ebov_clust$parameters$pro[2]*42*0.05,
                color = "black", size = 0.5)

###Epomops franqueti MARV

ef_marv_clust <- Mclust((bats_wild %>% filter(Species == "Epomops franqueti"))$logMARVrs, G = 1:3, verbose = FALSE)
plot(ef_marv_clust, what = "BIC", xlab = "Epomops MARV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Ef_M_cutoff <- as.numeric(ef_marv_clust$parameters$mean[1]) + 3*sqrt(ef_marv_clust$parameters$variance$sigmasq[1])

ef_marv_bootClust <- MclustBootstrap(ef_marv_clust)
Ef_M_cutoff_low <- summary(ef_marv_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(ef_marv_bootClust, what = "ci")$variance[1]))
Ef_M_cutoff_hi <- summary(ef_marv_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(ef_marv_bootClust, what = "ci")$variance[2]))

Ef_M_plot <- ggplot(bats_wild %>% filter(Species == "Epomops franqueti")) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected E. franqueti MARV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ef_M_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Ef_M_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Ef_M_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ef_marv_clust$parameters$mean[1]),
                                        sd = sqrt(ef_marv_clust$parameters$variance$sigmasq[1]))*
                  ef_marv_clust$parameters$pro[1]*42*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ef_marv_clust$parameters$mean[2]), 
                                        sd = sqrt(ef_marv_clust$parameters$variance$sigmasq[2]))*
                  ef_marv_clust$parameters$pro[2]*42*0.05,
                color = "black", size = 0.5)

###Myonycteris angolensis EBOV

ma_ebov_clust <- Mclust((bats_wild %>% filter(Species == "Myonycteris angolensis"))$logEBOVrs, G = 1:3, verbose = FALSE)
plot(ma_ebov_clust, what = "BIC", xlab = "Myonycteris EBOV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Ma_E_cutoff <- as.numeric(ma_ebov_clust$parameters$mean[1]) + 3*sqrt(ma_ebov_clust$parameters$variance$sigmasq[1])

ma_ebov_bootClust <- MclustBootstrap(ma_ebov_clust)
Ma_E_cutoff_low <- summary(ma_ebov_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(ma_ebov_bootClust, what = "ci")$variance[1]))
Ma_E_cutoff_hi <- summary(ma_ebov_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(ma_ebov_bootClust, what = "ci")$variance[2]))

Ma_E_plot <- ggplot(bats_wild %>% filter(Species == "Myonycteris angolensis")) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected M. angolensis EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ma_E_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Ma_E_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Ma_E_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ma_ebov_clust$parameters$mean[1]),
                                        sd = sqrt(ma_ebov_clust$parameters$variance$sigmasq[1]))*
                  ma_ebov_clust$parameters$pro[1]*32*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ma_ebov_clust$parameters$mean[2]), 
                                        sd = sqrt(ma_ebov_clust$parameters$variance$sigmasq[2]))*
                  ma_ebov_clust$parameters$pro[2]*32*0.05,
                color = "black", size = 0.5)

###Myonycteris angolensis MARV

ma_marv_clust <- Mclust((bats_wild %>% filter(Species == "Myonycteris angolensis"))$logMARVrs, G = 1:3, verbose = FALSE)
plot(ma_marv_clust, what = "BIC", xlab = "Myonycteris MARV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Ma_M_cutoff <- as.numeric(ma_marv_clust$parameters$mean[1]) + 3*sqrt(ma_marv_clust$parameters$variance$sigmasq)

ma_marv_bootClust <- MclustBootstrap(ma_marv_clust)
Ma_M_cutoff_low <- summary(ma_marv_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(ma_marv_bootClust, what = "ci")$variance[1]))
Ma_M_cutoff_hi <- summary(ma_marv_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(ma_marv_bootClust, what = "ci")$variance[2]))

Ma_M_plot <- ggplot(bats_wild %>% filter(Species == "Myonycteris angolensis")) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected M. angolensis MARV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ma_M_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Ma_M_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Ma_M_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ma_marv_clust$parameters$mean[1]),
                                        sd = sqrt(ma_marv_clust$parameters$variance$sigmasq))*
                  ma_marv_clust$parameters$pro[1]*32*0.05,
                color = "black", size = 0.5) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(ma_marv_clust$parameters$mean[2]),
                                        sd = sqrt(ma_marv_clust$parameters$variance$sigmasq))*
                  ma_marv_clust$parameters$pro[2]*32*0.05,
                color = "black", size = 0.5)


###Epomops buettikoferi EBOV

eb_ebov_clust <- Mclust((bats_wild %>% filter(Species == "Epomops buettikoferi"))$logEBOVrs, G = 1:3, verbose = FALSE)
plot(eb_ebov_clust, what = "BIC", xlab = "Epomops EBOV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Eb_E_cutoff <- as.numeric(eb_ebov_clust$parameters$mean[1]) + 3*sqrt(eb_ebov_clust$parameters$variance$sigmasq[1])

eb_ebov_bootClust <- MclustBootstrap(eb_ebov_clust)
Eb_E_cutoff_low <- summary(eb_ebov_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(eb_ebov_bootClust, what = "ci")$variance[1]))
Eb_E_cutoff_hi <- summary(eb_ebov_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(eb_ebov_bootClust, what = "ci")$variance[2]))

Eb_E_plot <- ggplot(bats_wild %>% filter(Species == "Epomops buettikoferi")) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected E. buettikoferi EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Eb_E_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Eb_E_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Eb_E_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eb_ebov_clust$parameters$mean[1]),
                                        sd = sqrt(eb_ebov_clust$parameters$variance$sigmasq[1]))*
                  eb_ebov_clust$parameters$pro[1]*21*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eb_ebov_clust$parameters$mean[2]), 
                                        sd = sqrt(eb_ebov_clust$parameters$variance$sigmasq[2]))*
                  eb_ebov_clust$parameters$pro[2]*21*0.05,
                color = "black", size = 0.5)

###Epomops buettikoferi MARV

eb_marv_clust <- Mclust((bats_wild %>% filter(Species == "Epomops buettikoferi"))$logMARVrs, G = 1:3, verbose = FALSE)
plot(eb_marv_clust, what = "BIC", xlab = "Epomops MARV MFI rescaled") #2 clusters

#cutoff - mean of first cluster + 3x standard deviation
Eb_M_cutoff <- as.numeric(eb_marv_clust$parameters$mean[1]) + 3*sqrt(eb_marv_clust$parameters$variance$sigmasq[1])

eb_marv_bootClust <- MclustBootstrap(eb_marv_clust)
Eb_M_cutoff_low <- summary(eb_marv_bootClust, what = "ci")$mean[1] + (3*sqrt(summary(eb_marv_bootClust, what = "ci")$variance[1]))
Eb_M_cutoff_hi <- summary(eb_marv_bootClust, what = "ci")$mean[2] + (3*sqrt(summary(eb_marv_bootClust, what = "ci")$variance[2]))

Eb_M_plot <- ggplot(bats_wild %>% filter(Species == "Epomops buettikoferi")) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected E. buettikoferi MARV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Eb_M_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Eb_M_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Eb_M_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eb_marv_clust$parameters$mean[1]),
                                        sd = sqrt(eb_marv_clust$parameters$variance$sigmasq[1]))*
                  eb_marv_clust$parameters$pro[1]*21*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(eb_marv_clust$parameters$mean[2]), 
                                        sd = sqrt(eb_marv_clust$parameters$variance$sigmasq[2]))*
                  eb_marv_clust$parameters$pro[2]*21*0.05,
                color = "black", size = 0.5)


###Hypsignathus monstrosus EBOV

hm_ebov_clust <- Mclust((bats_wild %>% filter(Species == "Hypsignathus monstrosus"))$logEBOVrs, G = 1:3, verbose = FALSE)
plot(hm_ebov_clust, what = "BIC", xlab = "Hypsignathus EBOV MFI rescaled") #2 clusters

Hm_E_plot <- ggplot(bats_wild %>% filter(Species == "Hypsignathus monstrosus")) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected H. monstrosus EBOV lnMFI") + ylab("Count") +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(hm_ebov_clust$parameters$mean[1]),
                                        sd = sqrt(hm_ebov_clust$parameters$variance$sigmasq[1]))*
                  hm_ebov_clust$parameters$pro[1]*15*0.05,
                color = "black", size = 0.5)

###Hypsignathus monstrosus MARV

hm_marv_clust <- Mclust((bats_wild %>% filter(Species == "Hypsignathus monstrosus"))$logMARVrs, G = 1:3, verbose = FALSE)
plot(hm_marv_clust, what = "BIC", xlab = "Hypsignathus MARV MFI rescaled") #2 clusters

Hm_M_plot <- ggplot(bats_wild %>% filter(Species == "Hypsignathus monstrosus")) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  theme_classic() + xlab("Corrected H. monstrosus MARV lnMFI") + ylab("Count") +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(hm_marv_clust$parameters$mean[1]),
                                        sd = sqrt(hm_marv_clust$parameters$variance$sigmasq[1]))*
                  hm_marv_clust$parameters$pro[1]*15*0.05,
                color = "black", size = 0.5)


###Nanonycteris veldkampii EBOV

nv_ebov_clust <- Mclust((bats_wild %>% filter(Species == "Nanonycteris veldkampii"))$logEBOVrs, G = 1:3, verbose = FALSE)
plot(nv_ebov_clust, what = "BIC", xlab = "Nanonycteris EBOV MFI rescaled") #3 clusters

#cutoff - mean of first cluster + 3x standard deviation
Nv_E_cutoff <- as.numeric(nv_ebov_clust$parameters$mean[2]) + 3*sqrt(nv_ebov_clust$parameters$variance$sigmasq)

nv_ebov_bootClust <- MclustBootstrap(nv_ebov_clust)
Nv_E_cutoff_low <- summary(nv_ebov_bootClust, what = "ci")$mean[3] + (3*sqrt(summary(nv_ebov_bootClust, what = "ci")$variance[3]))
Nv_E_cutoff_hi <- summary(nv_ebov_bootClust, what = "ci")$mean[4] + (3*sqrt(summary(nv_ebov_bootClust, what = "ci")$variance[4]))

Nv_E_plot <- ggplot(bats_wild %>% filter(Species == "Nanonycteris veldkampii")) +
  geom_histogram(aes(x=logEBOVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  xlim(c(-0.2,0.3)) +
  theme_classic() + xlab("Corrected N. veldkampii EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Nv_E_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Nv_E_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Nv_E_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(nv_ebov_clust$parameters$mean[1]),
                                        sd = sqrt(nv_ebov_clust$parameters$variance$sigmasq))*
                  nv_ebov_clust$parameters$pro[1]*9*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(nv_ebov_clust$parameters$mean[2]), 
                                        sd = sqrt(nv_ebov_clust$parameters$variance$sigmasq))*
                  nv_ebov_clust$parameters$pro[2]*9*0.05,
                color = "black", size = 0.5) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(nv_ebov_clust$parameters$mean[3]), 
                                        sd = sqrt(nv_ebov_clust$parameters$variance$sigmasq))*
                  nv_ebov_clust$parameters$pro[3]*9*0.05,
                color = "black", size = 0.5)

###Nanonycteris veldkampii MARV

nv_marv_clust <- Mclust((bats_wild %>% filter(Species == "Nanonycteris veldkampii"))$logMARVrs, G = 1:3, verbose = FALSE)
plot(nv_marv_clust, what = "BIC", xlab = "Nanonycteris MARV MFI rescaled") #1 cluster

#cutoff - mean of first cluster + 3x standard deviation
Nv_M_cutoff <- as.numeric(nv_marv_clust$parameters$mean[2]) + 3*sqrt(nv_marv_clust$parameters$variance$sigmasq)

nv_marv_bootClust <- MclustBootstrap(nv_marv_clust)
Nv_M_cutoff_low <- summary(nv_marv_bootClust, what = "ci")$mean[3] + (3*sqrt(summary(nv_marv_bootClust, what = "ci")$variance[3]))
Nv_M_cutoff_hi <- summary(nv_marv_bootClust, what = "ci")$mean[4] + (3*sqrt(summary(nv_marv_bootClust, what = "ci")$variance[4]))

Nv_M_plot <- ggplot(bats_wild %>% filter(Species == "Nanonycteris veldkampii")) +
  geom_histogram(aes(x=logMARVrs), alpha=0.4, binwidth = 0.05) + 
  geom_vline(xintercept = 0) + 
  xlim(c(-0.4,0.25)) +
  theme_classic() + xlab("Corrected N. veldkampii MARV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Nv_M_cutoff, colour="red",size=1) + 
  geom_vline(xintercept = Nv_M_cutoff_low, colour="red", lty = "dashed", size=1) +
  geom_vline(xintercept = Nv_M_cutoff_hi, colour="red", lty = "dashed", size=1) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(nv_marv_clust$parameters$mean[1]),
                                        sd = sqrt(nv_marv_clust$parameters$variance$sigmasq))*
                  nv_marv_clust$parameters$pro[1]*9*0.05,
                color = "black", size = 0.5) + 
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(nv_marv_clust$parameters$mean[2]), 
                                        sd = sqrt(nv_marv_clust$parameters$variance$sigmasq))*
                  nv_marv_clust$parameters$pro[2]*9*0.05,
                color = "black", size = 0.5) +
  stat_function(fun = function(x) dnorm(x, 
                                        mean = as.numeric(nv_marv_clust$parameters$mean[3]), 
                                        sd = sqrt(nv_marv_clust$parameters$variance$sigmasq))*
                  nv_marv_clust$parameters$pro[3]*9*0.05,
                color = "black", size = 0.5)

cutoff_fig_extras <- ggarrange(Mp_E_plot, Mp_M_plot,
                               Ef_E_plot, Ef_M_plot,
                               Ma_E_plot, Ma_M_plot,
                               Eb_E_plot, Eb_M_plot,
                               Hm_E_plot, Hm_M_plot,
                               Nv_E_plot, Nv_M_plot,
                               ncol=2, nrow=6,
                               labels=c("A","B","C","D","E","F",
                                        "G","H","I","J","K","L"))
ggsave("cutoffs 2.png", cutoff_fig_extras, height=16, width=8)

####determining number of positives

bats_wild %>% filter(Species == "Rousettus aegyptiacus") %>%
  mutate(MARVpos = (logMARVrs>R_M_cutoff),
         MARVlow = (logMARVrs>R_M_cutoff_low),
         MARVhi = (logMARVrs>R_M_cutoff_hi)) %>%
  dplyr::summarise(MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100)

bats_wild %>% filter(Species == "Eidolon helvum") %>%
  mutate(EBOVpos = (logEBOVrs>Ei_E_cutoff),
         EBOVhi = (logEBOVrs>Ei_E_cutoff_hi),
         EBOVlow = (logEBOVrs>Ei_E_cutoff_low),
         MARVpos = (logMARVrs>Ei_M_cutoff),
         MARVhi = (logMARVrs>Ei_M_cutoff_hi),
         MARVlow = (logMARVrs>Ei_M_cutoff_low)) %>%
  dplyr::summarise(MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100,
                   EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVpos_hi = mean(as.integer(EBOVhi))*100,
                   EBOVpos_low = mean(as.integer(EBOVlow))*100)

bats_wild %>% filter(Species == "Epomophorus gambianus") %>%
  mutate(EBOVpos = (logEBOVrs>Ep_E_cutoff),
         MARVpos = (logMARVrs>Ep_M_cutoff),
         EBOVlow = (logEBOVrs>Ep_E_cutoff_low),
         EBOVhi = (logEBOVrs>Ep_E_cutoff_hi),
         MARVhi = (logMARVrs>Ep_M_cutoff_hi),
         MARVlow = (logMARVrs>Ep_M_cutoff_low)) %>%
  dplyr::summarise(MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100,
                   EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVpos_hi = mean(as.integer(EBOVhi))*100,
                   EBOVpos_low = mean(as.integer(EBOVlow))*100)

bats_wild %>% filter(Species == "Micropteropus pusillus") %>%
  mutate(EBOVpos = (logEBOVrs>Mp_E_cutoff),
         MARVpos = (logMARVrs>Mp_M_cutoff),
         EBOVlow = (logEBOVrs>Mp_E_cutoff_low),
         EBOVhi = (logEBOVrs>Mp_E_cutoff_hi),
         MARVhi = (logMARVrs>Mp_M_cutoff_hi),
         MARVlow = (logMARVrs>Mp_M_cutoff_low)) %>%
  dplyr::summarise(MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100,
                   EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVpos_hi = mean(as.integer(EBOVhi))*100,
                   EBOVpos_low = mean(as.integer(EBOVlow))*100)

bats_wild %>% filter(Species == "Epomops franqueti") %>%
  mutate(EBOVpos = (logEBOVrs>Ef_E_cutoff),
         MARVpos = (logMARVrs>Ef_M_cutoff),
         EBOVlow = (logEBOVrs>Ef_E_cutoff_low),
         EBOVhi = (logEBOVrs>Ef_E_cutoff_hi),
         MARVhi = (logMARVrs>Ef_M_cutoff_hi),
         MARVlow = (logMARVrs>Ef_M_cutoff_low)) %>%
  dplyr::summarise(MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100,
                   EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVpos_hi = mean(as.integer(EBOVhi))*100,
                   EBOVpos_low = mean(as.integer(EBOVlow))*100)

bats_wild %>% filter(Species == "Myonycteris angolensis") %>%
  mutate(EBOVpos = (logEBOVrs>Ma_E_cutoff),
         EBOVlow = (logEBOVrs>Ma_E_cutoff_low),
         EBOVhi = (logEBOVrs>Ma_E_cutoff_hi),
         MARVpos = (logMARVrs>Ma_M_cutoff),
         MARVlow = (logMARVrs>Ma_M_cutoff_low),
         MARVhi = (logMARVrs>Ma_M_cutoff_hi)) %>%
  dplyr::summarise(EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVpos_hi = mean(as.integer(EBOVhi))*100,
                   EBOVpos_low = mean(as.integer(EBOVlow))*100,
                   MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100)

bats_wild %>% filter(Species == "Epomops buettikoferi") %>%
  mutate(EBOVpos = (logEBOVrs>Eb_E_cutoff),
         MARVpos = (logMARVrs>Eb_M_cutoff),
         EBOVlow = (logEBOVrs>Eb_E_cutoff_low),
         EBOVhi = (logEBOVrs>Eb_E_cutoff_hi),
         MARVhi = (logMARVrs>Eb_M_cutoff_hi),
         MARVlow = (logMARVrs>Eb_M_cutoff_low)) %>%
  dplyr::summarise(MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100,
                   EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVpos_hi = mean(as.integer(EBOVhi))*100,
                   EBOVpos_low = mean(as.integer(EBOVlow))*100)

bats_wild %>% filter(Species == "Nanonycteris veldkampii") %>%
  mutate(EBOVpos = (logEBOVrs>Nv_E_cutoff),
         EBOVlow = (logEBOVrs>Nv_E_cutoff_low),
         EBOVhi = (logEBOVrs>Nv_E_cutoff_hi),
         MARVpos = (logMARVrs>Nv_M_cutoff),
         MARVlow = (logMARVrs>Nv_M_cutoff_low),
         MARVhi = (logMARVrs>Nv_M_cutoff_hi)) %>%
  dplyr::summarise(EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVpos_hi = mean(as.integer(EBOVhi))*100,
                   EBOVpos_low = mean(as.integer(EBOVlow))*100,
                   MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100)

az %>%
  mutate(EBOVpos = (logEBOVrs>Ei_E_cutoff),
         EBOVlow = (logEBOVrs>Ei_E_cutoff_low),
         EBOVhi = (logEBOVrs>Ei_E_cutoff_hi),
         MARVpos = (logMARVrs>Ei_M_cutoff),
         MARVlow = (logMARVrs>Ei_M_cutoff_low),
         MARVhi = (logMARVrs>Ei_M_cutoff_hi)) %>%
  dplyr::summarise(EBOVpos = mean(as.integer(EBOVpos))*100,
                   EBOVpos_hi = mean(as.integer(EBOVhi))*100,
                   EBOVpos_low = mean(as.integer(EBOVlow))*100,
                   MARVpos = mean(as.integer(MARVpos))*100,
                   MARVpos_hi = mean(as.integer(MARVhi))*100,
                   MARVpos_low = mean(as.integer(MARVlow))*100)

 ###########FIGURES

fig1a <- ggplot(bats_wild %>% filter(Species %in% c("Eidolon helvum","Epomophorus gambianus","Rousettus aegyptiacus"))) + 
  geom_density(aes(x=logEBOVrs, fill=Species), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  #geom_vline(xintercept = 1) + 
  xlim(c(-0.2,1.2)) +
  geom_vline(xintercept = Ei_E_cutoff, col="green") +
  geom_vline(xintercept = Ep_E_cutoff, col="blue") +
  theme_classic() + xlab("Corrected EBOV lnMFI") + ylab("Density") +
  theme(legend.position = c(0.72,0.83), legend.text = element_text(face="italic"), legend.title = element_blank()) +
  scale_fill_manual(values=c("darkgreen","blue","red"))

###marv rescaled
fig1b <- ggplot(bats_wild %>% filter(Species %in% c("Eidolon helvum","Epomophorus gambianus","Rousettus aegyptiacus"))) + 
  geom_density(aes(x=logMARVrs, fill=Species), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  #geom_vline(xintercept = 1) + 
  geom_vline(xintercept = R_M_cutoff, col="red") + 
  geom_vline(xintercept = Ei_M_cutoff, col="green") +
  geom_vline(xintercept = Ep_M_cutoff, col="blue") +
  xlim(c(-0.2,1.2)) +
  theme_classic() + xlab("Corrected MARV lnMFI") + ylab("Density") +
  scale_fill_manual(values=c("darkgreen","blue","red")) +
  theme(legend.position = c(0.72,0.83), legend.text = element_text(face="italic"), legend.title = element_blank())

fig1 <- ggarrange(fig1a, fig1b, ncol=2, labels=c("A","B"))
ggsave("fig1.png", fig1, height=5, width=11)

supp_fig1a <- ggplot(bats_wild %>% filter(Species %in% c("Eidolon helvum","Epomophorus gambianus","Rousettus aegyptiacus"))) + 
  geom_histogram(aes(x=logEBOVrs, fill=Species), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  xlim(c(-0.2,1.2)) +
  theme_classic() + xlab("Corrected EBOV lnMFI") + ylab("Count") +
  geom_vline(xintercept = Ei_E_cutoff, col="green") +
  geom_vline(xintercept = Ep_E_cutoff, col="blue") +
  theme(legend.position = c(0.72,0.83), legend.text = element_text(face="italic"), legend.title = element_blank()) +
  scale_fill_manual(values=c("darkgreen","blue","red"))

supp_fig1b <- ggplot(bats_wild %>% filter(Species %in% c("Eidolon helvum","Epomophorus gambianus","Rousettus aegyptiacus"))) + 
  geom_histogram(aes(x=logMARVrs, fill=Species), alpha=0.4) + 
  geom_vline(xintercept = 0) + 
  #geom_vline(xintercept = 1) + 
  geom_vline(xintercept = R_M_cutoff, col="red") + 
  geom_vline(xintercept = Ei_M_cutoff, col="green") +
  geom_vline(xintercept = Ep_M_cutoff, col="blue") +
  xlim(c(-0.2,1.2)) +
  theme_classic() + xlab("Corrected MARV lnMFI") + ylab("Count") +
  scale_fill_manual(values=c("darkgreen","blue","red")) +
  theme(legend.position = c(0.72,0.83), legend.text = element_text(face="italic"), legend.title = element_blank())

supp_fig1 <- ggarrange(supp_fig1a, supp_fig1b, ncol=2, labels=c("A","B"))
ggsave("figS1.png", supp_fig1, height=5, width=11)
