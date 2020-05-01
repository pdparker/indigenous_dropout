
#libraries ####
library(foreign)
library(haven)
library(Hmisc)
library(psych)
library(apaTables)
library(tidyverse)
library(ggeffects)
library(magrittr)
library(broom)
library(stringr)
library(foreign)
library(srvyr)
library(svyPVpack)
library(survey)
library(lme4)
library(patchwork)
library(ggthemes)
# Tips and Tricks: Don't Run ####
get_label(lsay2003$loc)
get_labels(lsay2003$loc)

# Data Prep ####
# Change here
lsay2009<- readit::readit("~/cloudstor/Databases/2020-03-12_LSAY/LSAY_2009.dta")
lsay2003<- readit::readit("~/cloudstor/Databases/LSAY_DATSETS/LSAY2003_2016.sav")

sjlabelled::get_labels(lsay2003$sex)
sjlabelled::get_labels(lsay2009$ST04Q01)
# 2003 
lsay2003short <- lsay2003 %>%
  setNames(toupper(names(.)))%>%
  filter(!is.na(WT2004) ) %>% #Not sure wether to use 2004 weights or 2007
  select(WT2 = WT2007, starts_with("PV"), INDIG, GENDER = sex, GEO = LOC,
         SCH10 = LBWDV01, SCH11 = LCWDV01, SCH12 = LDWDV01, SCH13 = LEWDV01,
         STATEID, GRADE = ST01Q01,
         SC = LAA027, WT = WT2004, ESCS, starts_with("W_FSTR")) %>%
  mutate(ESCS = replace(ESCS, ESCS > 900, NA),
         INDIG = replace(INDIG, INDIG == 8,NA),
         GRADE = replace(GRADE, GRADE > 90, NA),
         GENDER = case_when(
           GENDER == 1 ~ 1, #1 is male
           GENDER == 2 ~ 0  #0 is female
         ),
         DROPOUT = case_when(
           SCH10 == 1 ~ 1,
           SCH11 == 1 ~ 1,
           SCH12 == 1 ~ 1,
           SCH13 == 1 ~ 1,
           SCH10 != 1 ~ 0,
           SCH11 != 1 ~ 0,
           SCH12 != 1 ~ 0,
           SCH13 != 1 ~ 0,
           TRUE ~ NA_real_
         ),
         GEO = case_when(
           GEO == 1 ~ 1,
           TRUE ~ 0
         ),
         ACH1PV = principal(.[,c("PV1READ","PV1SCIE","PV1MATH")],nfactors = 1)$scores,
         ACH2PV = principal(.[,c("PV2READ","PV2SCIE","PV2MATH")],nfactors = 1)$scores,
         ACH3PV = principal(.[,c("PV3READ","PV3SCIE","PV3MATH")],nfactors = 1)$scores,
         ACH4PV = principal(.[,c("PV4READ","PV4SCIE","PV4MATH")],nfactors = 1)$scores,
         ACH5PV = principal(.[,c("PV5READ","PV5SCIE","PV5MATH")],nfactors = 1)$scores
  ) %>%
  as_survey_rep(type = "Fay", rho = .05, repweights = starts_with("W_FSTR"), weights = WT)

# LSAY 2009
lsay2009short <-lsay2009 %>% 
  filter(!is.na(WT2010)) %>% #Not sure wether to use 2010 weights or 2013
  select(WT2 = WT2013,starts_with("PV"), INDIG, GENDER = ST04Q01, 
         SCH10 = LBWDV01, SCH11 = LCWDV01, SCH12 = LDWDV01, SCH13 = LEWDV01,
         STATEID = STATE, GRADE = ST01Q01, GEO = GEOLOC,
         SC = ST62N04, WT = WT2010, ESCS, starts_with("W_FSTR")) %>%
  select(-matches("READ[1-9]+")) %>%
  mutate(ESCS = replace(ESCS, ESCS > 900, NA),
         INDIG = replace(INDIG, INDIG == 8,NA),
         GRADE = replace(GRADE, GRADE > 90, NA),
         #hisced = replace(HISCED, HISCED > 8, NA),
         GENDER = case_when(
           GENDER == 2 ~ 1, #1 is male
           GENDER == 1 ~ 0, #0 is female
           TRUE ~ NA_real_
         ),
         DROPOUT = case_when(
           SCH10 == 1 ~ 1,
           SCH11 == 1 ~ 1,
           SCH12 == 1 ~ 1,
           SCH13 == 1 ~ 1,
           SCH10 != 1 ~ 0,
           SCH11 != 1 ~ 0,
           SCH12 != 1 ~ 0,
           SCH13 != 1 ~ 0,
           TRUE ~ NA_real_
         ),
         GEO = case_when(
           GEO == 1 ~ 1, # 1 is Urban
           TRUE ~ 0 # 0 is Rural
         ),
         ACH1PV = principal(.[,c("PV1READ","PV1SCIE","PV1MATH")],nfactors = 1)$scores,
         ACH2PV = principal(.[,c("PV2READ","PV2SCIE","PV2MATH")],nfactors = 1)$scores,
         ACH3PV = principal(.[,c("PV3READ","PV3SCIE","PV3MATH")],nfactors = 1)$scores,
         ACH4PV = principal(.[,c("PV4READ","PV4SCIE","PV4MATH")],nfactors = 1)$scores,
         ACH5PV = principal(.[,c("PV5READ","PV5SCIE","PV5MATH")],nfactors = 1)$scores
  ) %>%
  as_survey_rep(type = "Fay", rho = .05, repweights = starts_with("W_FSTR"), weights = WT)


#Combined Cohorts ####
#Z-score function as the scale function does not work with the survey package for some reasons
z <- function(z) {(z-mean(z, na.rm=TRUE))/sd(z, na.rm=TRUE)}
# Combine the cohorts
# #Warnings relate to survey not liking labelled data. These can be safely ignored
lsay <- bind_rows(lsay2003short$variables, lsay2009short$variables,.id = "COHORT") %>%
  mutate(STATEID = factor(STATEID),
         SC = z(SC)*-1,
         STATE2 = case_when(
           STATEID %in% 1:3 ~ 1,
           TRUE ~ 0
         ),
         FLAG_MISS = ifelse(is.na(WT2), 1,0),
         DROPOUT_10 = ifelse(SCH10 == 1, 1,0)
         )

table(lsay$DROPOUT, useNA = "always")
table(lsay$GENDER, lsay$COHORT, useNA = "always")
table(lsay$INDIG, lsay$COHORT, useNA = "always")
lsay %>% select(-starts_with("PV"), -starts_with("W_")) %>% summary
lsay %>% mutate(sample_size = is.na(WT2)) %>% group_by(COHORT, sample_size) %>% summarise(n = n())
# Using old school survey package here in order for ggeffects to work
lsay <- svrepdesign(data=lsay, weights=~WT, repweights = "W_FSTR[1-9]+", type="Fay", rho = .5, )
# Descriptives ####
svytable(~INDIG+COHORT, lsay)
svyby(~ACH1PV, ~INDIG,lsay,FUN = svymean)
svyby(~ACH1PV, ~INDIG,lsay,FUN = svyvar)^.5
# Models ####
# Hypothesis 1 ####
# H1a: Indigenous Australian Children have higher dropout rates
H1 <- svyglm(DROPOUT ~ INDIG+COHORT+GRADE+STATE2+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
tidy(H1)
#Marginal Effects H1b
ggeffects::ggpredict(H1, terms = c("INDIG"))
# Is the gap the same for boys and girls
H1b <- svyglm(DROPOUT ~ INDIG*GENDER+COHORT+GRADE+STATE2+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
tidy(H1b)
# Is the gap the same for urban and rural
H1c <- svyglm(DROPOUT ~ INDIG*GEO+COHORT+GRADE+STATE2+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
tidy(H1c)
#Marginal Effects H1b
geo_h1 <- ggeffects::ggpredict(H1b, terms = c("INDIG","GEO"))

geo_h1_out <- data.frame(prob = geo_h1$predicted, ci.low = geo_h1$conf.low, ci.high = geo_h1$conf.high,
                         indig = rep(c("non-Indigenous","Indigenous"),each=2), geo = rep(c("Provincial/Rural","Urban"), 2))

geo_plot <- geo_h1_out %>%
  ggplot(aes(x=geo, y=prob, ymin=ci.low, ymax=ci.high)) +
  geom_pointrange() + 
  #geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Indigenous") + ylab("Probability") +
  facet_wrap(~indig) +
  theme_stata()
# Is the gap the same for Rich and poor and rural
H1d <- svyglm(DROPOUT ~ INDIG*ESCS+COHORT+GRADE+STATE2+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
tidy(H1d)
ses_h1 <- ggeffects::ggpredict(H1d, terms = c("INDIG","ESCS [-2,-1,0,1,2]"))
ses_h1_out <- data.frame(prob = ses_h1$predicted, ci.low = ses_h1$conf.low, ci.high = ses_h1$conf.high,
                         indig = rep(c("non-Indigenous","Indigenous"),each=5), ses = rep(-2:2, 2))


ses_dist <- ggplot() +
  geom_density(alpha = .2, aes(x=lsay$variables[lsay$variables$INDIG == 0,"ESCS"], fill = "grey", color = "grey", alpha = 0.7)) + 
  geom_density(alpha = .2, aes(x=lsay$variables[lsay$variables$INDIG == 1,"ESCS"], fill = "black", color = "black", alpha = 0.7)) + 
  theme_stata() + theme(legend.position = "none")

ses_plot <- ses_h1_out %>%
  ggplot(aes(x=ses, y=prob, ymin=ci.low, ymax=ci.high)) +
  geom_pointrange() + 
  #geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Indigenous") + ylab("Probability") +
  facet_wrap(~indig) +
  theme_stata()

ses_dist + {geo_plot + ses_plot + plot_layout(ncol=2)} + plot_layout(ncol=1)


H1e <- svyglm(DROPOUT ~ INDIG*ESCS+INDIG*GEO+INDIG*GENDER+COHORT+GRADE+STATE2+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
tidy(H1e)

# Hypothesis 2 ####
# H2b: Indigenous disadvantage still present when comparing equally advantaged and equally achieving Indigenous and non-Indigenous Youth
svyPVglm(DROPOUT ~ INDIG+COHORT+STATE2+GENDER+GRADE+GEO+ACH..PV+ESCS+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
h2 <- svyglm(DROPOUT ~ INDIG+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+ESCS+FLAG_MISS,design = lsay,family = quasibinomial())


ach_h2 <- ggeffects::ggpredict(h2, terms = c("INDIG","ACH1PV [-2,-1.9,-1.8,-1.7,-1.6,-1.5,-1.4,-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2]"))
ach_h1_out <- data.frame(prob = ach_h1$predicted, ci.low = ach_h1$conf.low, ci.high = ses_h1$conf.high,
                         indig = rep(c("non-Indigenous","Indigenous"),each=5), ses = rep(-2:2, 2))

# Model for marginal effects
# H2 <- svyglm(DROPOUT ~ INDIG*COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+ESCS+FLAG_MISS,design = lsay,family = quasibinomial())
# ggeffects::ggpredict(H2, terms = c("COHORT","INDIG"))

# Hypothesis 3: Heterogenity in Indigenous Effect ###
# H3a: Is Indigenous gap the same for high/low achieving Indigenous kids
svyPVglm(DROPOUT ~ INDIG*ACH..PV+COHORT+ESCS+STATE2+GENDER+GRADE+GEO+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
#H3b: Is Indigenous gap the same for high/low SES
svyPVglm(DROPOUT ~ INDIG*ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH..PV+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
#H3c: Is Indigenous gap the same for urban/Rural
svyPVglm(DROPOUT ~ INDIG*GEO+COHORT+ESCS+STATE2+GENDER+GRADE+GEO+ACH..PV+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
#H3d:Is Indigenous gap the same for boys/girls
svyPVglm(DROPOUT ~ INDIG*GENDER+COHORT+ESCS+STATE2+GENDER+GRADE+GEO+ACH..PV+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
#H3f:Does the Indigenous gap respond to changes in legislation
svyPVglm(DROPOUT ~ INDIG*COHORT+SC+GENDER+ESCS+STATE2+GENDER+GRADE+GEO+ACH..PV+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)

# All 2-way interaction
svyPVglm(DROPOUT ~ INDIG*ACH..PV+INDIG*ESCS+INDIG*GEO+INDIG*COHORT+INDIG*GENDER+ESCS+COHORT+STATE2+GENDER+GRADE+GEO+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)


# To get Marginal Effects
H3a <- svyglm(DROPOUT ~ INDIG*ACH1PV+ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+SC+FLAG_MISS,design = lsay,family = quasibinomial())
ggeffects::ggpredict(H3a, terms = c("INDIG", "ACH1PV [-2,-1,0,1,2]"))

H3b <- svyglm(DROPOUT ~ INDIG*ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+SC+FLAG_MISS,design = lsay,family = quasibinomial())
ggeffects::ggpredict(H3b, terms = c("INDIG", "ESCS [-2,-1,0,1,2]"))

H3c <- svyglm(DROPOUT ~ INDIG*GEO+ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+SC+FLAG_MISS,design = lsay,family = quasibinomial())
ggeffects::ggpredict(H3c, terms = c("INDIG", "GEO"))

# Only significant in model with 1 interaction at a time
H3c <- svyglm(DROPOUT ~ INDIG*GENDER+ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+SC+FLAG_MISS,design = lsay,family = quasibinomial())
ggeffects::ggpredict(H3c, terms = c("INDIG", "GENDER"))
H3c <- svyglm(DROPOUT ~ INDIG*GEO+ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+SC+FLAG_MISS,design = lsay,family = quasibinomial())
ggeffects::ggpredict(H3c, terms = c("INDIG", "GEO"))


