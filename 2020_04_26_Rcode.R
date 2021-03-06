
#libraries ####
library(foreign)
library(haven)
library(Hmisc)
library(psych)
library(apaTables)
library(tidyverse)
library(ggtext)
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
library(flextable)
library(officer)
library(oxbase)
library(sparkline)
# Tips and Tricks: Don't Run ####
#get_label(lsay2003$loc)
#get_labels(lsay2003$loc)

# Data Prep ####
# Change here
lsay2009<- readit::readit("~/cloudstor/Databases/2020-03-12_LSAY/LSAY_2009.dta")
lsay2003<- readit::readit("~/cloudstor/Databases/LSAY_DATSETS/LSAY2003_2016.sav")
lsay2015<- readit::readit("~/cloudstor/Databases/2020-03-12_LSAY/LSAY_2015.dta")

meta <- read_csv("names_metadata.csv")

#sjlabelled::get_labels(lsay2003$sex)
#sjlabelled::get_labels(lsay2009$ST04Q01)
# 2003 
lsay2003short <- lsay2003 %>%
  setNames(toupper(names(.)))%>%
  filter(!is.na(WT2004) ) %>% #Not sure wether to use 2004 weights or 2007
  select(WT2 = WT2007, starts_with("PV"), INDIG, GENDER = SEX, GEO = LOC,
         SCH10 = LBWDV01, SCH11 = LCWDV01, SCH12 = LDWDV01, 
         STATEID, GRADE = ST01Q01,
         WT = WT2004, ESCS, starts_with("W_FSTR")) %>%
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
           SCH10 != 1 ~ 0,
           SCH11 != 1 ~ 0,
           SCH12 != 1 ~ 0,
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
         SCH10 = LBWDV01, SCH11 = LCWDV01, SCH12 = LDWDV01, 
         STATEID = STATE, GRADE = ST01Q01, GEO = GEOLOC,
         WT = WT2010, ESCS, starts_with("W_FSTR")) %>%
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
           SCH10 != 1 ~ 0,
           SCH11 != 1 ~ 0,
           SCH12 != 1 ~ 0,
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

#LSAY 2015
lsay2015short <- lsay2015 %>%
  setNames(toupper(names(.)))%>%
  filter(!is.na(WT16GENP) ) %>% #Not sure wether to use 2004 weights or 2007
  select(WT2 = WT18GENP,starts_with("PV"), INDIG, GENDER = ST004Q01TA, 
         SCH10 = LBLA003, SCH11 = LCWDV01, SCH12 = LDWDV01,
         STATEID = STATE, GRADE = GRADE, GEO = GEOLOC_3,
         WT = WT16GENP, ESCS, starts_with("W_FSTURWT") ) %>%
  select(-matches("READ[1-9]+")) %>%
  mutate(
         INDIG = replace(INDIG, INDIG == 9,NA) %>% as.numeric(),
         GRADE = replace(GRADE, GRADE > 90, NA),
         GENDER = case_when(
           GENDER == 2 ~ 1, #1 is male
           GENDER == 1 ~ 0, #0 is female
           TRUE ~ NA_real_
         ),
         DROPOUT = case_when(
           SCH10 == 2 ~ 1,
           SCH11 == 1 ~ 1,
           SCH12 == 1 ~ 1,
           SCH10 != 2 ~ 0,
           SCH11 != 1 ~ 0,
           SCH12 != 1 ~ 0,
           SCH10 == 98 ~ NA_real_,
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
  ) 
  
names(lsay2015short)[122:201] <- paste0("W_FSTR", 1:80)

lsay2015short<- lsay2015short%>%
  as_survey_rep(type = "Fay", rho = .05, repweights = starts_with("W_FSTR"), weights = WT)

#Combined Cohorts ####
#Z-score function as the scale function does not work with the survey package for some reasons
z <- function(z) {(z-mean(z, na.rm=TRUE))/sd(z, na.rm=TRUE)}
# Combine the cohorts
# #Warnings relate to survey not liking labelled data. These can be safely ignored
lsay <- bind_rows(lsay2003short$variables, lsay2009short$variables,.id = "COHORT") %>%
  bind_rows(., lsay2015short$variables, .id = "COHORT2")%>%
  mutate(STATEID = factor(STATEID),
         STATE2 = case_when(
           STATEID %in% 1:3 ~ 1,
           TRUE ~ 0
         ),
         FLAG_MISS = ifelse(is.na(WT2), 1,0),
         COHORT = case_when(
           COHORT2 == 2 ~ "C2015",
           COHORT == 2 ~ "C2009",
           TRUE ~ "C2003"
         )
         )

table(lsay$DROPOUT, lsay$COHORT, useNA = "always") %>% prop.table(margin=1)
table(lsay$GENDER, lsay$COHORT, useNA = "always")
table(lsay$INDIG, lsay$COHORT, useNA = "always")
lsay %>% select(-starts_with("PV"), -starts_with("W_")) %>% summary
lsay %>% mutate(sample_size = is.na(WT2)) %>% group_by(COHORT, sample_size) %>% summarise(n = n())
# Using old school survey package here in order for ggeffects to work
lsay <- svrepdesign(data=lsay, weights=~WT, repweights = "W_FSTR[1-9]+", type="Fay", rho = .5, )
# Descriptives ####
svyby(~ACH1PV, ~INDIG,lsay,FUN = svymean)
svyby(~ACH1PV, ~INDIG,lsay,FUN = svyvar)^.5
# Descriptives
descriptive <- matrix(NA, ncol = 3, nrow = 8)
dropout <- svytable(~INDIG+DROPOUT, design=lsay) %>% prop.table(., margin = 1)*100 
cohort <- svytable(~INDIG+COHORT, lsay) %>% prop.table(., margin = 2)*100 
gender <- svytable(~INDIG+GENDER, lsay) %>% prop.table(., margin = 1)*100 
urban <- svytable(~INDIG+GEO, lsay) %>% prop.table(., margin = 1)*100 
grade <- svytable(~INDIG+I(GRADE>=10), lsay) %>% prop.table(., margin = 1)*100 
achievement <- svyby(~ACH1PV, ~INDIG,design = lsay,FUN = svymean, vartype = "ci")
ses <- svyby(~ESCS, ~INDIG,design = lsay,FUN = svymean, na.rm=TRUE, vartype = "ci")

descriptive[1,] <- c("Dropout %", sprintf("%.2f%%",dropout[2,1]), sprintf("%.2f%%",dropout[2,2]))
descriptive[2,] <- c("Cohort 2003", sprintf("%.2f%%",cohort[1,1]), sprintf("%.2f%%",cohort[2,1]))
descriptive[3,] <- c("Cohort 2003", sprintf("%.2f%%",cohort[1,2]), sprintf("%.2f%%",cohort[2,2]))
descriptive[4,] <- c("Girls %", sprintf("%.2f%%",gender[1,1]), sprintf("%.2f%%",gender[2,1]))
descriptive[5,] <- c("Urban %", sprintf("%.2f%%",urban[2,1]), sprintf("%.2f%%",urban[2,2]))
descriptive[6,] <- c("Year 10 or Higher %", sprintf("%.2f%%",grade[1,1]), sprintf("%.2f%%",grade[2,2]) )
descriptive[7,] <- c("Achievement Index (Mean)", sprintf("%.2f [%.2f, %.2f]",achievement[1,2],achievement[1,3],achievement[1,4]), sprintf("%.2f [%.2f, %.2f]",achievement[2,2],achievement[2,3],achievement[2,4]) )
descriptive[8,] <- c("Socioeconomic Status Index (Mean)", sprintf("%.2f [%.2f, %.2f]",ses[1,2],ses[1,3],ses[1,4]), sprintf("%.2f [%.2f, %.2f]",ses[2,2],ses[2,3],ses[2,4]) )
descriptive <- data.frame(descriptive)
names(descriptive) <- c("Variable", "non-Indigenous", "Indigenous")

flextable(descriptive) %>%
  hline_bottom(x=.,part="header", border = fp_border(width=1)) %>%
  hline_top(x=.,part="header", border = fp_border(width=2)) %>%
  autofit() %>%
  align(align = "left", part = "all") %>%
  footnote(i = 7:8, j = 1,
           value = as_paragraph("95% confidence intervals in square brackets"),
           ref_symbols = c("a")) %>%
  set_caption(caption = "Table 1. Descriptives") %>%
  save_as_image(path = "img/descriptives.png")

# Models ####
# Hypothesis 1 ####
# H1a: Indigenous Australian Children have higher dropout rates
H1 <- svyglm(DROPOUT ~ INDIG+COHORT+GRADE+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
(H1_out <- tidy(H1, conf.int=TRUE))
#Marginal Effects H1b
ggeffects::ggpredict(H1, terms = c("INDIG"))
# Is the gap the same for boys and girls
H1b <- svyglm(DROPOUT ~ INDIG*GENDER+COHORT+GRADE+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
(H1b_out <- tidy(H1b, conf.int=TRUE) )
# Is the gap the same for urban and rural
H1c <- svyglm(DROPOUT ~ INDIG*GEO+COHORT+GRADE+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
(H1c_out <- tidy(H1c, conf.int=TRUE))
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
  theme_stata() + xlab("") + ylab("Probability of not completing high-school") +
  ggtitle("Geography by Indigenous Status")

# Is the gap the same for Rich and poor and rural
H1d <- svyglm(DROPOUT ~ INDIG*ESCS+COHORT+GRADE+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
(H1d_out <- tidy(H1d, conf.int=TRUE))
ses_h1 <- ggeffects::ggpredict(H1d, terms = c("INDIG","ESCS [-2,-1,0,1,2]"))
ses_h1_out <- data.frame(prob = ses_h1$predicted, ci.low = ses_h1$conf.low, ci.high = ses_h1$conf.high,
                         indig = rep(c("non-Indigenous","Indigenous"),each=5), ses = rep(-2:2, 2))


library(ggtext)


ses_dist <- ggplot() +
  geom_density(alpha = .2, aes(x=lsay$variables[lsay$variables$INDIG == 0,"ESCS"], fill = "grey", color = "grey", alpha = 0.7)) + 
  geom_density(alpha = .2, aes(x=lsay$variables[lsay$variables$INDIG == 1,"ESCS"], fill = "black", color = "black", alpha = 0.7)) + 
  theme_stata() + theme(legend.position = "none") + xlab("Socioeconomic status") + ylab("")  +
  labs(
    title = 
    "<span>Distribution of Socioeconomic Status for</span>
    <span style='color:#F8766D;'>Indigenous</span> 
    and 
    <span style='color:#00BFC4;'>non-Indigenous</span>
    </span> Youth"
  ) +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
  )

ses_plot <- ses_h1_out %>%
  ggplot(aes(x=ses, y=prob, ymin=ci.low, ymax=ci.high)) +
  geom_pointrange() + 
  #geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Indigenous") + ylab("Probability") +
  facet_wrap(~indig) +
  theme_stata() + xlab("Socioeconomic Status") + ylab("Probability of not completing high-school") +
  ggtitle("Socioeconomic Status by Indigenous Status")

ses_dist + {geo_plot + ses_plot + plot_layout(ncol=2)} + plot_layout(ncol=1)


H1e <- svyglm(DROPOUT ~ INDIG*ESCS+INDIG*GEO+INDIG*GENDER+INDIG*COHORT+GRADE+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
H1e_out <- tidy(H1e, conf.int=TRUE)

# Has the gap closed
H1f <- svyglm(DROPOUT ~ INDIG*COHORT+ESCS+COHORT+GRADE+GEO+GENDER+FLAG_MISS+ESCS,design = lsay,family = quasibinomial())
# Tidy Output
(H1f_out <- tidy(H1f, conf.int=TRUE))

hypothesis_1 <- matrix(NA, ncol=3, nrow=5)

hypothesis_1[1,] <- c("Main Indigenous effect",sprintf("%.2f [%.2f, %.2f] p = %.3f", H1_out$estimate[2],H1_out$conf.low[2],H1_out$conf.high[2],H1_out$p.value[2])," ")
hypothesis_1[2,] <- c("Indigenous by gender",sprintf("%.2f [%.2f, %.2f] p = %.3f", H1b_out$estimate[9],H1b_out$conf.low[9],H1b_out$conf.high[9],H1b_out$p.value[9]),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H1e_out$estimate[11],H1e_out$conf.low[11],H1e_out$conf.high[11],H1e_out$p.value[11]))
hypothesis_1[3,] <- c("Indigenous by urban",sprintf("%.2f [%.2f, %.2f] p = %.3f", H1c_out$estimate[9],H1c_out$conf.low[9],H1c_out$conf.high[9],H1c_out$p.value[9]),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H1e_out$estimate[10],H1e_out$conf.low[10],H1e_out$conf.high[10],H1e_out$p.value[10]))
hypothesis_1[4,] <- c("Indigenous by socioeconomic status",sprintf("%.2f [%.2f, %.2f] p = %.3f", H1d_out$estimate[9],H1d_out$conf.low[9],H1d_out$conf.high[9],H1d_out$p.value[9]),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H1e_out$estimate[9],H1e_out$conf.low[9],H1e_out$conf.high[9],H1e_out$p.value[9]))
hypothesis_1[5,] <- c("Indigenous by cohort",sprintf("%.2f [%.2f, %.2f] p = %.3f", H1f_out$estimate[9],H1f_out$conf.low[9],H1f_out$conf.high[9],H1f_out$p.value[9]),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H1e_out$estimate[12],H1e_out$conf.low[12],H1e_out$conf.high[12],H1e_out$p.value[12]))

hypothesis_1 <- data.frame(hypothesis_1)
names(hypothesis_1) <- c("Log-odds", "Univariate", "Multivariate")

flextable(hypothesis_1) %>%
  hline_bottom(x=.,part="header", border = fp_border(width=1)) %>%
  hline_top(x=.,part="header", border = fp_border(width=2)) %>%
  autofit() %>%
  align(align = "left", part = "all") %>%
  footnote(i = c(1), j = 2:3, part = "header",
           value = as_paragraph(c("Interaction when entered one at a time",
                                "Interactions when all interactions entered into the model")
           ),
           ref_symbols = c("a", "b")) %>%
  color(i = c(2,5),
        j = 2,
        color="grey") %>%
  color(i = c(2,3),
        j = 3,
        color="grey") %>%
  set_caption("Table 2. Hypothesis 1 Results") %>%
  save_as_image(path = "img/hypothesis1.png")
# Hypothesis 2 ####
# H2b: Indigenous disadvantage still present when comparing equally advantaged and equally achieving Indigenous and non-Indigenous Youth
H2 <- svyPVglm(DROPOUT ~ INDIG+COHORT+GENDER+GRADE+GEO+ACH..PV+ESCS+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)

H2_out <- H2$coef %>% 
  rownames_to_column(var = "Parameter") %>%
  as_tibble() %>%
  mutate(conf.low = mean - 2*se,
         conf.high = mean + 2*se)
  


h2 <- svyglm(DROPOUT ~ INDIG+COHORT+GENDER+GRADE+GEO+ACH1PV+ESCS+FLAG_MISS,design = lsay,family = quasibinomial())

ach_h2 <- ggeffects::ggpredict(h2, terms = c("INDIG","ACH1PV [-2,-1.9,-1.8,-1.7,-1.6,-1.5,-1.4,-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2]"))
ach_h2_out <- data.frame(prob = ach_h2$predicted, ci.low = ach_h2$conf.low, ci.high = ach_h2$conf.high,
                         indig = rep(c("non-Indigenous","Indigenous"),each=41), ses = rep(seq(-2,2,.1), 2))

ach_dist <- ggplot() +
  geom_density(alpha = .2, aes(x=lsay$variables[lsay$variables$INDIG == 0,"ACH1PV"], fill = "grey", color = "grey", alpha = 0.7)) + 
  geom_density(alpha = .2, aes(x=lsay$variables[lsay$variables$INDIG == 1,"ACH1PV"], fill = "black", color = "black", alpha = 0.7)) + 
  theme_stata() + theme(legend.position = "none") + xlab("Achievement Index") + ylab("")  +
  labs(
    title = 
      "<span>Distribution of Achievement for</span>
    <span style='color:#F8766D;'>Indigenous</span> 
    and 
    <span style='color:#00BFC4;'>non-Indigenous</span>
    </span> youth"
  ) +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
  )

ach_effect <- ach_h2_out %>%
  ggplot(aes(x=ses, y=prob, group=indig, color=indig)) +
  geom_line() + 
  geom_ribbon(aes(ymin=ci.low,ymax=ci.high, fill=indig), alpha=.2, linetype=0) +
  theme_stata() + theme(legend.position = "none") + xlab("Achievement Index") + ylab("Probability of not completing high-school")  +
  labs(
    title = 
      "<span>Probability of dropout for equally able</span>
    <span style='color:#F8766D;'>Indigenous</span> 
    and 
    <span style='color:#00BFC4;'>non-Indigenous</span>
    </span> youth"
  ) +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
  )

ach_effect /ach_dist


# Hypothesis 3: Heterogenity in Indigenous Effect ###

#H3b: Is Indigenous gap the same for high/low SES
H2_b <- svyPVglm(DROPOUT ~ INDIG*ESCS+COHORT+GENDER+GRADE+GEO+ACH..PV+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
H2b_out <- H2_b$coef %>% 
  rownames_to_column(var = "Parameter") %>%
  as_tibble() %>%
  mutate(conf.low = mean - 2*se,
         conf.high = mean + 2*se)
#H3c: Is Indigenous gap the same for urban/Rural
H2_c <- svyPVglm(DROPOUT ~ INDIG*GEO+COHORT+ESCS+GENDER+GRADE+GEO+ACH..PV+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
H2c_out <- H2_c$coef %>% 
  rownames_to_column(var = "Parameter") %>%
  as_tibble() %>%
  mutate(conf.low = mean - 2*se,
         conf.high = mean + 2*se)
#H3d:Is Indigenous gap the same for boys/girls
H2_d <- svyPVglm(DROPOUT ~ INDIG*GENDER+COHORT+ESCS+GENDER+GRADE+GEO+ACH..PV+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
H2d_out <- H2_d$coef %>% 
  rownames_to_column(var = "Parameter") %>%
  as_tibble() %>%
  mutate(conf.low = mean - 2*se,
         conf.high = mean + 2*se)
# H3a: Is Indigenous gap the same for high/low achieving Indigenous kids
H2_e <- svyPVglm(DROPOUT ~ INDIG*ACH..PV+COHORT+ESCS+GENDER+GRADE+GEO+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
H2e_out <- H2_e$coef %>% 
  rownames_to_column(var = "Parameter") %>%
  as_tibble() %>%
  mutate(conf.low = mean - 2*se,
         conf.high = mean + 2*se)
#H3f:Does the Indigenous gap respond to changes in legislation
H2_f <- svyPVglm(DROPOUT ~ INDIG*COHORT+GENDER+ESCS+GENDER+GRADE+GEO+ACH..PV+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
H2f_out <- H2_f$coef %>% 
  rownames_to_column(var = "Parameter") %>%
  as_tibble() %>%
  mutate(conf.low = mean - 2*se,
         conf.high = mean + 2*se)
# All 2-way interaction
H2_g <- svyPVglm(DROPOUT ~ INDIG*ACH..PV+INDIG*ESCS+INDIG*GEO+INDIG*COHORT+INDIG*GENDER+ESCS+COHORT+GENDER+GRADE+GEO+FLAG_MISS,design = lsay,family = quasibinomial(), placeholder = 1:5)
H2g_out <- H2_g$coef %>% 
  rownames_to_column(var = "Parameter") %>%
  as_tibble() %>%
  mutate(conf.low = mean - 2*se,
         conf.high = mean + 2*se)

hypothesis_2 <- matrix(NA, ncol=3, nrow=6)
hypothesis_2[1,] <- c("Main Indigenous effect", sprintf("%.2f [%.2f, %.2f] p = %.3f", H2_out$mean[2],H2_out$conf.low[2],H2_out$conf.high[2],H2_out$Pr.t[2] %>% as.numeric())," ")
hypothesis_2[2,] <- c("Indigenous by gender", sprintf("%.2f [%.2f, %.2f] p = %.3f", H2d_out$mean[10],H2d_out$conf.low[10],H2d_out$conf.high[10],H2d_out$Pr.t[10]%>% as.numeric()),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H2g_out$mean[14],H2g_out$conf.low[14],H2g_out$conf.high[14],H2g_out$Pr.t[14]%>% as.numeric()) )

hypothesis_2[3,] <- c("Indigenous by urban",sprintf("%.2f [%.2f, %.2f] p = %.3f", H2c_out$mean[10],H2c_out$conf.low[10],H2c_out$conf.high[10],H2c_out$Pr.t[10]%>% as.numeric()),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H2g_out$mean[12],H2g_out$conf.low[12],H2g_out$conf.high[12],H2g_out$Pr.t[12]%>% as.numeric()))

hypothesis_2[4,] <- c("Indigenous by socioeconomic status",sprintf("%.2f [%.2f, %.2f] p = %.3f", H2b_out$mean[10],H2b_out$conf.low[10],H2b_out$conf.high[10],H2b_out$Pr.t[10]%>% as.numeric()),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H2g_out$mean[11],H2g_out$conf.low[11],H2g_out$conf.high[11],H2g_out$Pr.t[11]%>% as.numeric()))

hypothesis_2[5,] <- c("Indigenous by cohort",sprintf("%.2f [%.2f, %.2f] p = %.3f", H2f_out$mean[10],H2f_out$conf.low[10],H2f_out$conf.high[10],H2f_out$Pr.t[10]%>% as.numeric()),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H2g_out$mean[13],H2g_out$conf.low[13],H2g_out$conf.high[13],H2g_out$Pr.t[13]%>% as.numeric()))

hypothesis_2[6,] <- c("Indigenous by achievement",sprintf("%.2f [%.2f, %.2f] p = %.3f", H2e_out$mean[10],H2e_out$conf.low[10],H2e_out$conf.high[10],H2e_out$Pr.t[10]%>% as.numeric()),
                      sprintf("%.2f [%.2f, %.2f] p = %.3f", H2g_out$mean[10],H2g_out$conf.low[10],H2g_out$conf.high[10],H2g_out$Pr.t[10]%>% as.numeric()))

hypothesis_2 <- data.frame(hypothesis_2)
names(hypothesis_2) <- c("Log-odds", "Univariate", "Multivariate")

flextable(hypothesis_2) %>%
  hline_bottom(x=.,part="header", border = fp_border(width=1)) %>%
  hline_top(x=.,part="header", border = fp_border(width=2)) %>%
  autofit() %>%
  align(align = "left", part = "all") %>%
  footnote(i = 1, j = 2:3, part = "header",
           value = as_paragraph(c("Interaction when entered one at a time",
                                  "Interactions when all interactions entered into the model")
           ),
           ref_symbols = c("a", "b")) %>%
  color(i = c(1:2,5),
        j = 2,
        color="grey") %>%
  color(i = c(2,5),
        j = 3,
        color="dark grey") %>%
  set_caption("Table 3. Hypothesis 2 Results") %>%
  save_as_image(path = "img/hypothesis2.png")

# To get Marginal Effects

H3a <- svyglm(DROPOUT ~ INDIG*ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+SC+FLAG_MISS,design = lsay,family = quasibinomial())
ses_h3 <- ggeffects::ggpredict(H3a, terms = c("INDIG", "ESCS [-2,-1.9,-1.8,-1.7,-1.6,-1.5,-1.4,-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2]"))
ses_h3_out <- data.frame(prob = ses_h3$predicted, ci.low = ses_h3$conf.low, ci.high = ses_h3$conf.high,
                         indig = rep(c("non-Indigenous","Indigenous"),each=41), ses = rep(seq(-2,2,.1), 2))

ses_effect_h3 <- ses_h3_out %>%
  ggplot(aes(x=ses, y=prob, group=indig, color=indig)) +
  geom_line() + 
  geom_ribbon(aes(ymin=ci.low,ymax=ci.high, fill=indig), alpha=.2, linetype=0) +
  theme_stata() + theme(legend.position = "none") + xlab("Socioeconomic Status") + ylab("Probability of not completing high-school")  +
  labs(
    title = 
      "<span>Probability of dropout for </span>
    <span style='color:#F8766D;'>Indigenous</span> 
    and 
    <span style='color:#00BFC4;'>non-Indigenous</span>
    </span> youth"
  ) +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
  )

H3b <- svyglm(DROPOUT ~ INDIG*GEO+ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+SC+FLAG_MISS,design = lsay,family = quasibinomial())
geo_h3 <- ggeffects::ggpredict(H3b, terms = c("INDIG", "GEO"))
geo_h3_out <- data.frame(prob = geo_h3$predicted, ci.low = geo_h3$conf.low, ci.high = geo_h3$conf.high,
                         indig = rep(c("non-Indigenous","Indigenous"),each=2), geo = rep(c("Provincial/Rural","Urban"), 2))
geo_plot_h3 <- geo_h3_out %>%
  ggplot(aes(x=geo, y=prob, ymin=ci.low, ymax=ci.high)) +
  geom_pointrange() + 
  #geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Indigenous") + ylab("Probability") +
  facet_wrap(~indig) +
  theme_stata() + xlab("") + ylab("Probability of not completing high-school") +
  ggtitle("Geography by Indigenous Status")


H3c <- svyglm(DROPOUT ~ INDIG*ACH1PV+ESCS+COHORT+STATE2+GENDER+GRADE+GEO+ACH1PV+SC+FLAG_MISS,design = lsay,family = quasibinomial())
ach_h3 <- ggeffects::ggpredict(H3c, terms = c("INDIG", "ACH1PV [-2,-1.9,-1.8,-1.7,-1.6,-1.5,-1.4,-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2]"))
ach_h3_out <- data.frame(prob = ach_h3$predicted, ci.low = ach_h3$conf.low, ci.high = ach_h3$conf.high,
                         indig = rep(c("non-Indigenous","Indigenous"),each=41), ses = rep(seq(-2,2,.1), 2))
ach_effect_h3 <- ach_h3_out %>%
  ggplot(aes(x=ses, y=prob, group=indig, color=indig)) +
  geom_line() + 
  geom_ribbon(aes(ymin=ci.low,ymax=ci.high, fill=indig), alpha=.2, linetype=0) +
  theme_stata() + theme(legend.position = "none") + xlab("Achievement Index") + ylab("Probability of not completing high-school")  +
  labs(
    title = 
      "<span>Probability of dropout for </span>
    <span style='color:#F8766D;'>Indigenous</span> 
    and 
    <span style='color:#00BFC4;'>non-Indigenous</span>
    </span> youth"
  ) +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
  )

ach_effect_h3 + ses_effect_h3 +plot_layout(ncol=1)
geo_plot_h3
