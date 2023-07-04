library(dplyr)
library(haven)
library(data.table)
library(tidyverse)
library(geepack)

set_na <- function(x, na_codes = 99){ x[x %in% na_codes] <- NA; x }
## >> Read datasets  ###############

## Read in the file 

datDHS_per_orig <- as.data.frame(readRDS("DHS_perp_recode_july2_clean.rds"))
datPHIA_per_orig <- as.data.frame(readRDS( "PHIA_perp_recode_jul3_clean.rds"))
dat_perp <- rbind( datDHS_per_orig,datPHIA_per_orig)
saveRDS(dat_perp, "dat_perp_july3.rds") 
dat_perp <- readRDS("dat_perp_july3.rds")

## Set variables in proper format 
rec <- c("recent_viol")
for (var in rec){
  set(dat_perp, i=which(dat_perp[[var]] %in% c("Yes")), j=var, value=1)
  set(dat_perp, i=which(dat_perp[[var]] %in% c("No")), j=var, value=0)
}
dat_perp$recent_viol <- as.numeric(dat_perp$recent_viol )

dat_perp$hivstatus_m <- as.character(dat_perp$hivstatus_m)
rec <- c("hivstatus_m")
for (var in rec){
  set(dat_perp, i=which(dat_perp[[var]] %in% c("Positive")), j=var, value=1)
  set(dat_perp, i=which(dat_perp[[var]] %in% c("Negative")), j=var, value=0)
}
dat_perp$hivstatus_m <- as.numeric(dat_perp$hivstatus_m )

dat_perp$hivstatus_w <- as.character(dat_perp$hivstatus_w)
rec <- c("hivstatus_w")
for (var in rec){
  set(dat_perp, i=which(dat_perp[[var]] %in% c("Positive")), j=var, value=1)
  set(dat_perp, i=which(dat_perp[[var]] %in% c("Negative")), j=var, value=0)
}
dat_perp$hivstatus_w <- as.numeric(dat_perp$hivstatus_w )


dat_perp$arv_m <- as.character(dat_perp$arv_m)
rec <- c("arv_m")
for (var in rec){
  set(dat_perp, i=which(dat_perp[[var]] %in% c("Yes")), j=var, value=1)
  set(dat_perp, i=which(dat_perp[[var]] %in% c("No")), j=var, value=0)
}
dat_perp$arv_m <- as.numeric(dat_perp$arv_m )

dat_perp$country<-as.factor(dat_perp$country)
dat_perp$region<-as.factor(dat_perp$region)
dat_perp$psu_u<-as.factor(dat_perp$psu_u)
dat_perp <- dat_perp %>%
  group_by(psu_u) %>%
  mutate(group_id = cur_group_id())
dat_perp$surveyid<-as.factor(dat_perp$surveyid)


dat_perp$lifegrw<- cut(dat_perp$life_w, breaks = c( - Inf, 2, 3, Inf), labels = c( "0-1", "2", "3+"), include.lowest = TRUE, right = FALSE)
dat_perp$lifegrm<- cut(dat_perp$life_m, breaks = c( - Inf, 2, 3, Inf), labels = c( "0-1", "2", "3+"), include.lowest = TRUE, right = FALSE)
dat_perp$recpart_w<- cut(dat_perp$partners_rec_w, breaks = c(-Inf, 2, 95), labels = c( "0-1", "2+"), include.lowest = TRUE, right = FALSE)
dat_perp$recpart_m<- cut(dat_perp$partners_rec_m, breaks = c(-Inf, 2, 95), labels = c( "0-1", "2+"), include.lowest = TRUE, right = FALSE)
dat_perp$recpart_m<- as.character(dat_perp$recpart_m)

rec <- c("recpart_m")
for (var in rec){
  set(dat_perp, i=which(dat_perp[[var]] %in% c("2+")), j=var, value=1)
  set(dat_perp, i=which(dat_perp[[var]] %in% c("0-1")), j=var, value=0)
}
dat_perp$recpart_m <- as.numeric(dat_perp$recpart_m)


## >> Create vectors I will reference later ##########
cov_exp = dat_perp[,c ('justify_m', "earn_disp", 'decision',"head", "agedisp", 'poly', 'part_alcoholFRQ')]
cov_exp_sa = dat_perp[,c ( 'decision',"head", "agedisp", 'poly', 'part_alcoholFRQ')]
cov_adj= dat_perp[,c ( "agegrm",  "schoolm", "wealthm", "restype", "surveyid")]


## >> Crude and adjusted association between variables and IPV ########## 

runGEE_poi = function(outcome, exposures, df, adjustment) {
  iloop=list()
  
  for (i in seq_along(exposures)) {
    print(i)

    df1<-df
    ## Adjusted 
    df1 <- df1[, c(outcome, colnames(exposures[i]), colnames(adjustment),  "psu_u", "surveyid", "region")]
    df1 <- df1[complete.cases(df1),]
    df1$region <- droplevels(df1$region)
    df1$surveyid <- droplevels(df1$surveyid)
    df1$agegrm<- droplevels(df1$agegrm)
    
    f_intx =   as.formula(paste(outcome, "~", colnames(exposures)[i], "+", paste(colnames(adjustment), collapse="+")))
    m_intx = geeglm(data=df1, formula=f_intx,  id=psu_u, corstr="exchangeable", family = poisson, scale.fix = TRUE)
    
    coef_intx = as.data.frame(coefficients(summary(m_intx))) #extracting relevent coefficients 
    
    if ( colnames(exposures)[i] %in% "part_alcoholFRQ" ) { 
      coef_intx  = coef_intx[2:3,] 
      coef_intx$levels <- rownames(coef_intx) 
      } else if ( colnames(exposures)[i] %in% "earn_disp" )
        {  coef_intx  = coef_intx[2:4,] 
        coef_intx$levels <- rownames(coef_intx) 
        } else {   coef_intx  = coef_intx[2,] }
  
    coef_intx$levels <- rownames(coef_intx)
    coef_intx$outcome = outcome
    coef_intx$nsurv <- length(unique(df1$surveyid)) # N surveys in the dataset 
    coef_intx$N <- nrow(df1)
    coef_intx$N <- format(round(as.numeric(coef_intx$N), 1), nsmall=0, big.mark=",")
    
    coef_intx$adj_estimate =  round(exp(coef_intx$Estimate),2)
    
    coef_intx$adj_perc_robust95ci = paste(" (", ## have pretty CIS together 
                                         round(exp(coef_intx$Estimate-1.96*coef_intx$Std.err),2),
                                         ", ",
                                         round(exp(coef_intx$Estimate+1.96*coef_intx$Std.err),2),
                                         ")", sep="")
    coef_intx$lwr = round(exp(coef_intx$Estimate-1.96*coef_intx$Std.err),2) ## have separate upper and power intervals 
    coef_intx$upr = round(exp(coef_intx$Estimate+1.96*coef_intx$Std.err),2)
    coef_intx$tot = paste(coef_intx$adj_estimate, ## have pretty CIS together 
                          coef_intx$adj_perc_robust95ci,
                          sep="")
    
    coef_intx = coef_intx[, c("outcome", "levels", "adj_estimate", "adj_perc_robust95ci", "lwr", "upr", "nsurv", "N", 'tot')]
    
    
    ## Crude 
    
    df2 <- df[, c(outcome, colnames(exposures)[i], "psu_u", "surveyid")]
    df2 <- df2[complete.cases(df2), ]
    df2$surveyid <- droplevels(df2$surveyid)
  
    f_intx1 =   as.formula(paste(outcome, "~", colnames(exposures)[i]))
    m_intx1 = geeglm(data=df2, formula=f_intx1,  id=psu_u, corstr="exchangeable", family = poisson, scale.fix = TRUE)
    
    coef_intx1 = as.data.frame(coefficients(summary(m_intx1))) #extracting relevent coefficients 
    if ( colnames(exposures)[i] %in% "part_alcoholFRQ" ) { 
      coef_intx1  = coef_intx1[2:3,] 
      coef_intx1$levels <- rownames(coef_intx1) 
      } else if ( colnames(exposures)[i] %in% "earn_disp" )
        
        {  coef_intx1  = coef_intx1[2:4,] 
        coef_intx1$levels <- rownames(coef_intx1) 
        } else {   coef_intx1  = coef_intx1[2,] }


    coef_intx1$levels <- rownames(coef_intx1)
    rownames(coef_intx1) = c()
    coef_intx1$outcome = outcome
    coef_intx1$nsurv <- length(unique(df2$surveyid)) # N surveys in the dataset 
    coef_intx1$N <- nrow(df2)
    coef_intx1$N <- format(round(as.numeric(coef_intx1$N), 1), nsmall=0, big.mark=",")
    
    coef_intx1$cr_estimate =  round(exp(coef_intx1$Estimate),2)
    
    coef_intx1$cr_perc_robust95ci = paste(" (", ## have pretty CIS together 
                                         round(exp(coef_intx1$Estimate-1.96*coef_intx1$Std.err),2),
                                         ", ",
                                         round(exp(coef_intx1$Estimate+1.96*coef_intx1$Std.err),2),
                                         ")", sep="")
    coef_intx1$lwr = round(exp(coef_intx1$Estimate-1.96*coef_intx1$Std.err),2) ## have separate upper and power intervals 
    coef_intx1$upr = round(exp(coef_intx1$Estimate+1.96*coef_intx1$Std.err),2)
    coef_intx1$tot = paste(coef_intx1$cr_estimate, ## have pretty CIS together 
                          coef_intx1$cr_perc_robust95ci,
                          sep="")
    
    coef_intx1 = coef_intx1[, c("outcome", "levels", "cr_estimate", "cr_perc_robust95ci", "lwr", "upr", "nsurv", "N", 'tot')]
    rownames(coef_intx1) = c()
 
    
    coef_intx<- cbind( coef_intx,  coef_intx1)
    
    
    iloop[i] = list(coef_intx)
  }
  ibound = do.call("rbind", iloop)
  
  return(ibound)
  
}


adj_gee <- runGEE_poi(outcome= "recent_viol", exposures =cov_exp, df= dat_perp, adjustment = cov_adj)
gee_cent<- runGEE_poi(outcome= "recent_viol", exposures =cov_exp, adjustment = cov_adj, df= dat_perp[dat_perp$region %in% c("Central Africa"), ] )
gee_east<- runGEE_poi(outcome= "recent_viol", exposures =cov_exp,  adjustment = cov_adj, df= dat_perp[dat_perp$region %in% c("Eastern Africa"), ] )
gee_south<- runGEE_poi(outcome= "recent_viol", exposures =cov_exp_sa,  adjustment = cov_adj, df= dat_perp[dat_perp$region %in% c("Southern Africa"), ] )
gee_west<- runGEE_poi(outcome= "recent_viol", exposures =cov_exp,  adjustment = cov_adj, df= dat_perp[dat_perp$region %in% c("Western Africa"), ] )


## >> Create datasets for  biomarker analysis association between recent IPV and HIV positive  status , HIV incidence, ART and VLS and sexual risk behaviors ########## 
'%ni%' <- Negate('%in%')

dat_perp_hivpos_m<- dat_perp %>% 
  group_by (surveyid) %>% 
  filter (!all(is.na(arv_m)) & surveyid %ni% c("ZM2013DHS") & hivstatus_m %in% 1 ) %>% 
  ungroup()


## Remove Zambia from HIV analysis and all surveys without HIV data
dat_perp_hivp<- dat_perp %>% group_by (surveyid) %>% filter (!all(is.na(hivstatus_m)) & surveyid %ni% c("ZM2013DHS"))
sex_vars = c ("conLast_m", "pay_rec", "recpart_m", "conc_m") ## sexual risk behavior variables. 
adjustment_sex = c('agegrm', 'wealthm', 'schoolm',"restype", 'surveyid') ## adjust for sexual risk behavior
adjustment_bmr = c('agegrm', 'wealthm', 'schoolm',"restype", 'surveyid', "part_alcoholFRQ") ## adjust for ART use and VLS*****
adjustment_hiv = c('agegrm', 'wealthm', 'schoolm', "restype", 'lifegrm',  'surveyid') ##  adjust for HIV status - lifetime # sex parts


## >> Function for male HIV risk behaviors ########## 
recentadjGEE = function(outcome, covariates, adjustments, df) {
  iloop=list()
  
  for (i in seq_along(outcome)) {
    print(i)
 
    variable <- outcome[i]
    if (variable %in% c("conc_m")) { df <- df[df$partners_rec_m %ni% c(0,1) & !is.na(df$partners_rec_m), ] }
    
    ## Adjusted 
    df1<-df
    df1 <- df1[, c(covariates, variable, adjustments, "psu_u", "surveyid",  "group_id")]
    df1 <- df1[complete.cases(df1), ]
  
    df1$surveyid <- droplevels(df1$surveyid)
    df1$agegrm<- droplevels(  df1$agegrm)
 
    f_intx =   as.formula(paste(variable, "~", paste(covariates, collapse = "+"), "+" , paste(adjustments, collapse = "+")))
    

    if ( variable %in% c("partners_rec_m") ) {
      m_intx = geeglm(data=df1, formula=f_intx,  id=psu_u, corstr="exchangeable", family = gaussian, scale.fix = TRUE)
    }  else { 
      m_intx = geeglm(data=df1, formula=f_intx,  id=psu_u, corstr="exchangeable", family = poisson, scale.fix = TRUE)
      
      }
  if ( variable %in% c("partners_rec_m") ) {
     
       coef_intx = as.data.frame(coefficients(summary(m_intx))) #extracting relevent coefficients 
      
      coef_intx  = coef_intx[2,] 
      coef_intx$adj_estimate =  round((coef_intx$Estimate),2)
      
      coef_intx$adj_perc_robust95ci = paste(" (", ## have pretty CIS together 
                                            round((coef_intx$Estimate-1.96*coef_intx$Std.err),2),
                                            ", ",
                                            round((coef_intx$Estimate+1.96*coef_intx$Std.err),2),
                                            ")", sep="")
      coef_intx$lwr = round((coef_intx$Estimate-1.96*coef_intx$Std.err),2) 
      coef_intx$upr = round((coef_intx$Estimate+1.96*coef_intx$Std.err),2)
      coef_intx$outcome = variable
      
     } else { 
      coef_intx = as.data.frame(coefficients(summary(m_intx))) 
      
      coef_intx  = coef_intx[2,] 
      coef_intx$adj_estimate =  round(exp(coef_intx$Estimate),2)
      
      coef_intx$adj_perc_robust95ci = paste(" (", ## have pretty CIS together 
                                            round(exp(coef_intx$Estimate-1.96*coef_intx$Std.err),2),
                                            ", ",
                                            round(exp(coef_intx$Estimate+1.96*coef_intx$Std.err),2),
                                            ")", sep="")
      coef_intx$lwr = round(exp(coef_intx$Estimate-1.96*coef_intx$Std.err),2) ## have separate upper and power intervals 
      coef_intx$upr = round(exp(coef_intx$Estimate+1.96*coef_intx$Std.err),2)
      coef_intx$outcome = variable
      
      }
    coef_intx$nsurv <- length(unique(df1$surveyid)) # N surveys in the dataset 
    
    coef_intx$N <- nrow(df1)
    coef_intx$N <- format(round(as.numeric(coef_intx$N), 1), nsmall=0, big.mark=",")
 
    
    coef_intx = coef_intx[, c("outcome", "adj_estimate", "adj_perc_robust95ci", "lwr", "upr", "nsurv", "N")]
    
    rownames(coef_intx) = c()
    
    ## Crude 

    df2 <- df[, c(covariates, variable, "psu_u", "surveyid",  "group_id")]
    df2 <- df2[complete.cases(df2), ]
    df2$surveyid <- droplevels(df2$surveyid)
    f_intx1 =   as.formula(paste(variable, "~", paste(covariates, collapse = "+")))
   
    if ( variable %in% c("partners_rec_m") ) {
      m_intx1 = geeglm(data=df2, formula=f_intx1,  id=psu_u, corstr="exchangeable", family = gaussian, scale.fix = TRUE)
      } else {
      m_intx1 = geeglm(data=df2, formula=f_intx1,  id=psu_u, corstr="exchangeable", family = poisson, scale.fix = TRUE)
    }
    
    
  if ( variable %in% c("partners_rec_m") ) {
    
    coef_intx1 = as.data.frame(coefficients(summary(m_intx1))) #extracting relevent coefficients 
    
    coef_intx1  = coef_intx1[2,] 
    
    coef_intx1$cr_estimate =  round((coef_intx1$Estimate),2)
    
    coef_intx1$cr_perc_robust95ci = paste(" (", ## have pretty CIS together 
                                          round((coef_intx1$Estimate-1.96*coef_intx1$Std.err),2),
                                          ", ",
                                          round((coef_intx1$Estimate+1.96*coef_intx1$Std.err),2),
                                          ")", sep="")
    coef_intx1$lwr = round((coef_intx1$Estimate-1.96*coef_intx1$Std.err),2) ## have separate upper and power intervals 
    coef_intx1$upr = round((coef_intx1$Estimate+1.96*coef_intx1$Std.err),2)
    coef_intx1$outcome = variable
    
    
    } else { 
        
        coef_intx1 = as.data.frame(coefficients(summary(m_intx1))) #extracting relevent coefficients 
        
        coef_intx1  = coef_intx1[2,] 
        
        coef_intx1$cr_estimate =  round(exp(coef_intx1$Estimate),2)
        
        coef_intx1$cr_perc_robust95ci = paste(" (", ## have pretty CIS together 
                                              round(exp(coef_intx1$Estimate-1.96*coef_intx1$Std.err),2),
                                              ", ",
                                              round(exp(coef_intx1$Estimate+1.96*coef_intx1$Std.err),2),
                                              ")", sep="")
        coef_intx1$lwr = round(exp(coef_intx1$Estimate-1.96*coef_intx1$Std.err),2) ## have separate upper and power intervals 
        coef_intx1$upr = round(exp(coef_intx1$Estimate+1.96*coef_intx1$Std.err),2)
        coef_intx1$outcome = variable
        }

  
    coef_intx1$nsurv <- length(unique(df2$surveyid)) # N surveys in the dataset 
    coef_intx1$N <- nrow(df2)
    coef_intx1$N <- format(round(as.numeric(coef_intx1$N), 1), nsmall=0, big.mark=",")
    
    coef_intx1 = coef_intx1[, c("outcome",  "cr_estimate", "cr_perc_robust95ci", "lwr", "upr", "nsurv", "N")]
    
    rownames(coef_intx1) = c()
   
    coef_intx<- cbind( coef_intx1,  coef_intx)
    
    iloop[i] = list(coef_intx)
  }
  ibound = do.call("rbind", iloop)
  
  return( ibound)
  
}

sexbeh <- recentadjGEE (outcome= sex_vars, covariates="recent_viol", adjustments =  adjustment_sex,  df = dat_perp )
hivst <- recentadjGEE(outcome= "hivstatus_m", covariates="recent_viol", adjustments =  adjustment_hiv,  df = dat_perp_hivp) 
hiv_sex <- rbind(sexbeh, hivst)

## Summarize ART and VLS 
prop_bios <- function(df, outcome) { 
  formula = as.formula(paste( "~", paste("recent_viol","+", outcome )))
  cnt <- as.data.frame(xtabs(formula = formula, data = df, addNA = TRUE ))
  prp <- as.data.frame(prop.table(xtabs(formula = formula, data = df, addNA = TRUE ), 1))
  prp$Freq_1 = round (prp$Freq * 100, 1)
  names(prp)[names(prp) == 'Freq'] <- 'Pct'
  tot <- cbind(cnt, prp)[-c(4:5)]
  tot$Freq <- format(round(as.numeric( tot$Freq), 1), nsmall=0, big.mark=",")
  tot$Pct <- format(round(as.numeric( tot$Pct * 100), 1)) 
  tot$Freq_Pct <- paste( tot$Freq, " (",  tot$Pct, " %)", sep="")
  
  return (tot)
  
}
prop_arv <- prop_bios(df =dat_perp_hivpos_m, outcome = "arv_m" )
prop_vls <- prop_bios(df =dat_perp_hivpos_m, outcome = "vlsup_m" )


prop_arv[prop_arv$recent_viol %ni% NA, ][, c("recent_viol", "arv_m", "Freq_Pct")]
prop_vls[prop_vls$recent_viol %ni% NA  , ][ , c("recent_viol", "vlsup_m", "Freq_Pct")]


arv_m <- recentadjGEE(outcome= "arv_m", covariates='recent_viol', adjustment = adjustment_bmr,  df= dat_perp_hivpos_m )
vlsup_m <- recentadjGEE(outcome= "vlsup_m", covariates='recent_viol', adjustment = adjustment_bmr,  df= dat_perp_hivpos_m)


#>> Interaction between IPV and past year violence ##########

## What % of WLHIV have partners living with HIV, by IPV status?
prop_stat <- function(df) { 
formula = as.formula(paste( "~", paste("hivstatus_w","+", "hivstatus_m" )))
cnt <- as.data.frame(xtabs(formula = formula, data = df, addNA = TRUE ))
cnt <- subset(cnt, hivstatus_w %ni% NA & hivstatus_w %ni% 0)
prp <- as.data.frame(prop.table(xtabs(formula = formula, data = df, addNA = TRUE ), 1))
prp <- subset(prp, hivstatus_w %ni% NA & hivstatus_w %ni% 0)
prp$Freq_1 = round (prp$Freq * 100, 1)
names(prp)[names(prp) == 'Freq'] <- 'Pct'
tot <- cbind(cnt, prp)[-c(4:5)]
tot$Freq <- format(round(as.numeric( tot$Freq), 1), nsmall=0, big.mark=",")
tot$Pct <- format(round(as.numeric( tot$Pct * 100), 1)) 
tot$Freq_Pct <- paste( tot$Freq, " (",  tot$Pct, " %)", sep="")

return (tot)

}

tot_prophiv <- prop_stat (df =dat_perp_hivp[dat_perp_hivp$agew < 25, ] ); tot_prophiv$var <- "all"
ipv_prophiv <- prop_stat (df =dat_perp_hivp[dat_perp_hivp$agew < 25 & dat_perp_hivp$recent_viol==1, ] ); ipv_prophiv$var <- "ipv"
noipv_prophiv <-prop_stat (df =dat_perp_hivp[dat_perp_hivp$agew < 25 & dat_perp_hivp$recent_viol==0, ] ); noipv_prophiv$var <- "noipv"

all_prophiv <- cbind (tot_prophiv,ipv_prophiv , noipv_prophiv)


## Create a dummy variable for interaction 
dat_perp_hivp$ipvHIV<- factor(paste0(dat_perp_hivp$hivstatus_m, as.numeric(dat_perp_hivp$recent_viol==1)))
dat_perp_hivp <-dat_perp_hivp[!grepl("NA", dat_perp_hivp$ipvHIV),] 
dat_perp_hivp$ipvHIV <- droplevels(dat_perp_hivp$ipvHIV)
dat_perp_hivp$surveyid <- as.factor(dat_perp_hivp$surveyid )


table(dat_perp_hivp[dat_perp_hivp$agew < 25, ]$ipvHIV)
nrow(dat_perp_hivp[dat_perp_hivp$agew < 25, ])

table(dat_perp_hivp$ipvHIV)
nrow(dat_perp_hivp)


adjustment_inter = c('agew' , 'wealthw', 'schoolw', 'restype', 'lifegrw', 'surveyid') 
adjustment_inter1 = c('agegrw' , 'wealthw', 'schoolw', 'restype', 'lifegrw', 'surveyid') ## Categorical age for all women
exp_names = c("Neither",  "Only partner HIV status", "Only recent violence", "Both", 'RERI', "p00", 'p10', 'p01', 'p11') 

## Adjusted interaction function for GEE ######
set.seed(678) ## 
msm_Int= function(outcome, adjustment, df) {
  mloop=matrix(ncol = 9, nrow=500)
  colnames(mloop) <- c("baseline", "RD10_only HIV", "RD_only IPV", "RD both", "reri", "p00", "p10", "p01", "p11")
  for (m in 1:500) {
    print(m)
   # m=1
    clusters <- names(table(df$psu_u))

    df2 <- df[sample(1:length(clusters), length(clusters), replace = T), ]
    df2 <- df2[, c(outcome, "ipvHIV",  adjustment, "psu_u")]
    
    df2 <- df2[complete.cases(df2), ]
    f_intx =   as.formula(paste(outcome, "~", "ipvHIV", "+", paste(adjustment, collapse="+")))
    df2$surveyid <- droplevels(df2$surveyid)
    
    if (all(adjustment == c(adjustment_inter1))) { df2$agegrw<- droplevels(df2$agegrw) }
    m_intx = geeglm(data=df2, formula=f_intx,  id=psu_u, corstr="exchangeable", family = poisson, scale.fix = TRUE)
    
    da00<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da1<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da10<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da2<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da01<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da3<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da11<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da4<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    
    
    da00$ipvHIV <- as.factor("00") ## dataset with all exposure = 00
    da1$ipvHIV <- as.factor("00")## dataset with all exposure = 00
    
    da10$ipvHIV <- as.factor("10") ## dataset with all exposure = 10 (only male HIV status) 
    da2$ipvHIV <- as.factor("00")## dataset with all exposure = 00
    
    da01$ipvHIV <- as.factor("01") ## dataset with all exposure = 01 (only IPV)
    da3$ipvHIV <- as.factor("00")## dataset with all exposure = 00
    
    da11$ipvHIV <- as.factor("11") ## dataset with all exposure = 11 (both)
    da4$ipvHIV<- as.factor("00")## dataset with all exposure = 00
    
    
    da00$pred1<-predict( m_intx , da00, type = "response") 
    da1$pred0<-predict( m_intx , da1, type = "response")
    
    da10$pred1<-predict( m_intx , da10, type = "response") 
    da2$pred0<-predict( m_intx , da2, type = "response") 
    
    
    da01$pred1<-predict( m_intx , da01, type = "response") 
    da3$pred0<-predict( m_intx , da3, type = "response") 
    
    da11$pred1<-predict( m_intx , da11, type = "response") 
    da4$pred0<-predict( m_intx , da4, type = "response") 

    coef_intx_00 <- mean(da00$pred1) - mean(da1$pred0) 
    
    coef_intx_10 <- mean(da10$pred1) - mean(da2$pred0) 
    
    coef_intx_01 <- mean(da01$pred1) - mean(da3$pred0)  
    
    coef_intx_11 <- mean(da11$pred1) - mean(da4$pred0)
    
  
    p00 <- mean(da00$pred1)
    p10 <- mean(da10$pred1)
    p01 <- mean(da01$pred1)
    p11 <- mean(da11$pred1)
  

    rm(df2, da00, da1,  da10, da2, da01, da3, da11, da4)
    
    
    mloop[m,1] <- coef_intx_00
    mloop[m,2] <- coef_intx_10 
    mloop[m,3] <- coef_intx_01 
    mloop[m,4] <- coef_intx_11
 
    mloop[m,5] = coef_intx_11 -coef_intx_10   -coef_intx_01 +1
    mloop[m,6] <- p00 
    mloop[m,7] <- p10 
    mloop[m,8] <- p01 
    mloop[m,9] <- p11
    
    mloop <- as.data.frame(mloop)

  }
  return(mloop)
} 

interaction_boot_clust = as.data.frame(msm_Int(outcome="hivstatus_w", 
                                        adjustment = adjustment_inter, df = dat_perp_hivp[dat_perp_hivp$agew < 25, ])) ## younger than 25

interaction_boot_f_clust= as.data.frame(msm_Int(outcome="hivstatus_w", 
                                          adjustment = adjustment_inter1, df = dat_perp_hivp)) ## full dataset


## Crude interaction function for GEE  ######
set.seed(589)
msm_Int_crude= function(outcome,  df) {
 
  mloop=matrix(ncol = 8, nrow=500)
  colnames(mloop) <- c("baseline", "RD10_only HIV", "RD_only IPV", "RD both", "p00", "p10", "p01", "p11")
  for (m in 1:500) {
  print(m)


    clusters <- names(table(df$psu_u))
    df2 <- df[sample(1:length(clusters), length(clusters), replace = T), ]
 
    df2 <- df2[, c(outcome, "ipvHIV",  "psu_u", "surveyid")]
    
    df2 <- df2[complete.cases(df2), ] 
    if (nrow(df2)==0) next 
    f_intx =   as.formula(paste(outcome, "~", "ipvHIV"))
    
    df2$surveyid <- droplevels(df2$surveyid)
    m_intx = geeglm(data=df2, formula=f_intx,  id=psu_u, corstr="exchangeable", family = poisson, scale.fix = TRUE)
    
    
    da00<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da1<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da10<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da2<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da01<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da3<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da11<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    da4<-  df2[ , -which(names(df2) %in% c("hivstatus_w", "psu_u"))]
    
    
    
    da00$ipvHIV <- as.factor("00") ## dataset with all exposure = 00
    da1$ipvHIV <- as.factor("00")## dataset with all exposure = 00
    
    da10$ipvHIV <- as.factor("10") ## dataset with all exposure = 10 (only male HIV status) 
    da2$ipvHIV <- as.factor("00")## dataset with all exposure = 00
    
    da01$ipvHIV <- as.factor("01") ## dataset with all exposure = 01 (only IPV)
    da3$ipvHIV <- as.factor("00")## dataset with all exposure = 00
    
    da11$ipvHIV <- as.factor("11") ## dataset with all exposure = 11 (both)
    da4$ipvHIV<- as.factor("00")## dataset with all exposure = 00
    
    
    da00$pred1<-predict( m_intx , da00, type = "response") 
    da1$pred0<-predict( m_intx , da1, type = "response")
    
    da10$pred1<-predict( m_intx , da10, type = "response") 
    da2$pred0<-predict( m_intx , da2, type = "response") 
    
    
    da01$pred1<-predict( m_intx , da01, type = "response") 
    da3$pred0<-predict( m_intx , da3, type = "response") 
    
    da11$pred1<-predict( m_intx , da11, type = "response") 
    da4$pred0<-predict( m_intx , da4, type = "response") 
    
    coef_intx_00 <- mean(da00$pred1)-mean(da1$pred0) 
    
    coef_intx_10 <- mean(da10$pred1)-mean(da2$pred0, na.rm= TRUE) 
    
    coef_intx_01<-  mean(da01$pred1)-mean(da3$pred0)
    
    coef_intx_11 <- mean(da11$pred1)-mean(da4$pred0) #
   

    p00 <- mean(da00$pred1)
    p10 <- mean(da10$pred1)
    p01 <- mean(da01$pred1)
    p11 <- mean(da11$pred1)
    
    rm(df2, da00, da1,  da10, da2, da01, da3, da11, da4)
    
    
    mloop[m,1] <- coef_intx_00
    mloop[m,2] <- coef_intx_10
    mloop[m,3] <- coef_intx_01 
    mloop[m,4] <- coef_intx_11
    mloop[m,5] <- p00
    mloop[m,6] <- p10
    mloop[m,7] <- p01 
    mloop[m,8] <- p11
    
    
    mloop <- as.data.frame(mloop)
    
  }
  return(mloop)
} 


interaction_boot_crude_clust= as.data.frame(msm_Int_crude(outcome="hivstatus_w", 
                                                    df = dat_perp_hivp[dat_perp_hivp$agew < 25, ]))

interaction_boot_crude_f_clust= as.data.frame(msm_Int_crude(outcome="hivstatus_w", 
                                                      df = dat_perp_hivp))

## Save the large boot file and read it back in 
interaction_boot_clust = read.csv("~/")
interaction_boot_f_clust = read.csv("~/")

interaction_boot_crude_clust = read.csv("~/")
interaction_boot_crude_f_clust = read.csv("~/")


interaction_boot_clust_sensitivity = read.csv("~/")
interaction_boot_clust_crude_sensitivity = read.csv("~/")

## put everything in a nice format 
bts_d <- function(df, outcome) {
  dlp=list()
  for (i in 1:ncol(df)) {
    #i = 1
    df1<- df[,i]
    
    PRD<-round(mean( df1)*100, 1)
    
    CI_lw<-round(quantile(df1, c(0.025, 0.975))[1]*100,1)
    CI_up<-round(quantile(df1, c(0.025, 0.975))[2]*100,1)
    
    d<-as.data.frame(cbind( PRD, CI_lw, CI_up))
    d$CI<- paste(" (",d$ CI_lw,   ", ",d$ CI_up, ")", sep="")
    
    d$Exposures= exp_names[i]
    
    d= d[, c("Exposures", "PRD", "CI",    'CI_lw',    'CI_up' )]
    
    dlp[i] = list(d)
  }
  dlp = do.call("rbind", dlp)
  return (dlp)
} 

interaction_dt_25 <-bts_d(outcome="hivstatus_w", df = interaction_boot_clust[2:10]) # 
interaction_dt_25cr <-bts_d(outcome="hivstatus_w", df = interaction_boot_crude_clust[2:9]) # 

interaction_dt_25sens <-bts_d(outcome="hivstatus_w", df = interaction_boot_clust_sensitivity[2:5]) # 
interaction_dt_25crsens <-bts_d(outcome="hivstatus_w", df = interaction_boot_clust_crude_sensitivity[2:5]) # 

interaction_dt_full <-bts_d(outcome="hivstatus_w", df = interaction_boot_f_clust[2:5]) # 
interaction_dt_fullcr <-bts_d(outcome="hivstatus_w", df = interaction_boot_crude_f_clust[2:5]) # 


