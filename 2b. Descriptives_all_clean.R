library(dplyr)
library(data.table)
library(demogsurv)
library(jtools)
library(tidyverse)
library(geepack)
library(openxlsx)
library(sjmisc)
library(survey)
'%ni%' <- Negate('%in%')

set_na <- function(x, na_codes = 99){ x[x %in% na_codes] <- NA; x }

## > Read datasets   ##############################################
datDHS_per_orig <- as.data.frame(readRDS("DHS_perp_recode_july2_clean.rds"))
datPHIA_per_orig <- as.data.frame(readRDS( "PHIA_perp_recode_jul3_clean.rds"))

dat_perp <- rbind( datDHS_per_orig,datPHIA_per_orig)
saveRDS(dat_perp, "dat_perp_july3.rds") ##
dat_perp <- readRDS("dat_perp_july3.rds")

## Proper format 
dat_perp$surveyid <- as.factor(dat_perp$surveyid)
dat_perp$country<-as.factor(dat_perp$country)
dat_perp$psu_u<-as.factor(dat_perp$psu_u)
dat_perp$lifegrw<- cut(dat_perp$life_w, breaks = c( - Inf, 2, 3, Inf), labels = c( "0-1", "2", "3+"), include.lowest = TRUE, right = FALSE)
dat_perp$lifegrm<- cut(dat_perp$life_m, breaks = c( - Inf, 2, 3, Inf), labels = c( "0-1", "2", "3+"), include.lowest = TRUE, right = FALSE)
dat_perp$recpart_w<- cut(dat_perp$partners_rec_w, breaks = c(-Inf, 2, 95), labels = c( "0-1", "2+"), include.lowest = TRUE, right = FALSE)
dat_perp$recpart_m<- cut(dat_perp$partners_rec_m, breaks = c(-Inf, 2, 95), labels = c( "0-1", "2+"), include.lowest = TRUE, right = FALSE)


## > Vectors I will use later #############
vars_viol =dat_perp[c("recent_viol", "ever_phys", "ever_sex_vio", "any_viol" )]
variables_cat1 <- dat_perp[c("period",  "region", "agegrm", "agegrw", "schoolm", "schoolw", "employmentm","employmentw", "wealthm", "wealthw", "restype", "earn_disp", 'decision',
                            'justify_m',   'justify_w', "head", 'poly', 'part_alcoholFRQ')]
variables_cont1 <- dat_perp[c("agedisp" )]


# Categorical T1  ###################
wt_cat <- function(df, varlist) { 
  t1 <- list ()
  d1 <- list()
  for (i in seq_along(varlist)) { 

   print(i)
#i  = 1
    df <- dat_perp
    
    d1[[i]] <-as.data.frame( df %>% group_by(surveyid) %>% filter(!all(is.na(!! rlang::sym(colnames(varlist[i]))))))
    
     d1[[i]]$surveyid <- droplevels(d1[[i]]$surveyid)
    df <- d1[[i]]
    df[,colnames(df)==colnames(varlist[i])] <-  addNA( df[,colnames(df)==colnames(varlist[i])], ifany = TRUE)
    
    des <- svydesign(ids= ~0 , data=df, strata= NULL, weights= ~ 1, nest=TRUE)
  
    formula = as.formula(paste( "~", paste(colnames(varlist[i]),"+", "recent_viol" )))
    cnt <- as.data.frame(xtabs(formula = formula, data = df, addNA = TRUE ))
    cnt <- subset(cnt, recent_viol %ni% NA)

    prp <- as.data.frame(prop.table(svytable(formula, design = des), margin = 2) )
    prp$Freq_1 = round (prp$Freq * 100, 1)
    names(prp)[names(prp) == 'Freq'] <- 'Pct'
    tot <- cbind(cnt, prp)[-c(4:5)]
    tot$Freq <- format(round(as.numeric( tot$Freq), 1), nsmall=0, big.mark=",")
    tot$Pct <- format(round(as.numeric( tot$Pct * 100), 1)) 
    
    tot$Freq_Pct <- paste( tot$Freq, " (",  tot$Pct, " %)", sep="")

    tot <- reshape(tot, idvar = colnames(varlist[i]), timevar = "recent_viol", direction = "wide")
    tot$ys_denom <- aggregate(cnt$Freq, by=list(recent_viol=cnt$recent_viol), FUN=sum)[2,2] ##Yes
    tot$no_denom <- aggregate(cnt$Freq, by=list(recent_viol=cnt$recent_viol), FUN=sum)[1,2] ##no
    colnames(tot)[1] <- "level"
    tot$variable <- colnames(varlist[i])
    tot$ns <- length(unique(df$surveyid))

t1[i] = list(tot)
out <- as.data.frame(do.call("rbind", t1))

  }
  
  return( out)
}

wt_catdt <- wt_cat(df = dat_perp, varlist = variables_cat1)


# Continuous T1  ###################
wt_cont <- function( varlist) { 
  t1 <- list ()
  d1 <- list ()
  for (i in seq_along(varlist)) { 
     
    print(i)
   df <- dat_perp

    d1[[i]] <-as.data.frame( df %>% group_by(surveyid) %>% filter(!all(is.na(!! rlang::sym(colnames(varlist[i])))))) 
    d1[[i]]$surveyid <- droplevels(d1[[i]]$surveyid)
    df <- d1[[i]]

    df_recy <- df[df$recent_viol %in% c("Yes"),]
    df_recn <- df[df$recent_viol %in% c("No"),]
    
    
    formula1 = as.formula(paste(  paste(colnames(varlist[i])), "~", "recent_viol"))
    formula = as.formula(paste( "~", paste(colnames(varlist[i]))))  

    
    desn <- svydesign(ids=~ 0, data= df_recn, strata= NULL, weights= ~ 1, nest=TRUE)
    sdn <- svysd(formula,  by = ~ recent_viol, design = desn,  na.rm = TRUE)
    desy <- svydesign(ids=~0, data= df_recy, strata= NULL, weights= ~ 1, nest=TRUE)
    sdy <- svysd( formula,  by = ~ recent_viol, design = desy,  na.rm = TRUE)
    std <- rbind( sdn, sdy)
    mean1 <-  aggregate(formula1, data = df, FUN = mean) # 
    

    std_mean <- cbind(mean1,std)
    colnames(std_mean) <- c("recent_viol", "mean", "std")

    std_mean[2:3] <- round(std_mean[2:3], 2)

    std_mean$mean_se <-  paste(  std_mean$mean, " (",  std_mean$std, ")", sep="")
    std_mean$variable <- colnames(varlist[i])
    std_mean$ns <- length(unique(df$surveyid))
    
    t1[i] = list(std_mean)
    out <- as.data.frame(do.call("rbind", t1))
    out <- reshape(out, idvar = "variable", timevar = "recent_viol", direction = "wide")
  }
  
  return( out)
}

wt_contdt <- wt_cont( varlist = variables_cont1)

 # Prevalence of IPV by Survey ID  ###################
df <- dat_perp

rec <- c("recent_viol")
for (var in rec){
  set(df, i=which(df[[var]] %in% c("Yes")), j=var, value=1)
  set(df, i=which(df[[var]] %in% c("No")), j=var, value=0)
}
df$recent_viol <- as.numeric(df$recent_viol )
df$recent_viol <- addNA(df$recent_viol)
des <- svydesign(ids=~0, data=df, strata = NULL, weights= ~ 1, nest = TRUE )

cnt_tot <- as.data.frame(xtabs( ~ surveyid, data = df, addNA = TRUE )) ## Total sample
cnt <- as.data.frame(xtabs( ~ recent_viol +  surveyid, data = df, addNA = TRUE )) #
prp <- as.data.frame(prop.table(svytable( ~ surveyid + recent_viol, design = des), margin = 1) )
prp <- reshape(prp, idvar = "surveyid", timevar = "recent_viol", direction = "wide")
cnt <- reshape(cnt, idvar = "surveyid", timevar = "recent_viol", direction = "wide")
cnt <- cbind(cnt_tot, cnt,prp )[-c(3,6,7,8,10)]
cnt[2:4] <- format(round( cnt[2:4], 1), nsmall=0, big.mark=",")
cnt$Freq.1.1 <- round(cnt$Freq.1.1 * 100,1)
colnames(cnt) <-  c("surveyid", "cnt_tot", "cnt_no", "cnt_yes", "prp_yes"); tot_s <- cnt
tot_s$freq_prop <-  paste( tot_s$cnt_yes, " (",  tot_s$prp_yes, "%)", sep="")
regexp <- "[[:digit:]]+"
tot_s$year <- stringr::str_extract(tot_s$surveyid, regexp)
for (i in 1:nrow(tot_s)) {
  
if (tot_s$surveyid[i] %in% c( "BF2010DHS", "CI2012DHS", "GM2013DHS",  "GM2019DHS", "GH2008DHS", "LB2007DHS",  "LB2019DHS","ML2006DHS", "ML2012DHS", "ML2018DHS", "NG2008DHS", "NG2013DHS", "NG2018DHS", "SL2019DHS", "TG2013DHS")) {
  tot_s$region[i] <- 'Western Africa' 
} else if (tot_s$surveyid[i] %in% c("SZ2016PHIA", "ZA2016DHS")) {
  tot_s$region[i] <- 'Southern Africa'

 } else if (tot_s$surveyid[i] %in% c("AO2015DHS", "CM2018DHS", "TD2014DHS", "GA2012DHS", "ST2008DHS")) {
  tot_s$region[i] <- 'Central Africa'
  } else { tot_s$region[i] <- 'Eastern Africa' }
}

# Prevalence of IPV by Region   ###################

cnt_tot <- as.data.frame(xtabs( ~ region, data = df, addNA = TRUE )) ## Total sample
cnt <- as.data.frame(xtabs( ~ recent_viol +  region, data = df, addNA = TRUE )) #
prp <- as.data.frame(prop.table(svytable( ~ region + recent_viol, design = des), margin = 1) )

prp <- reshape(prp, idvar = "region", timevar = "recent_viol", direction = "wide")
cnt <- reshape(cnt, idvar = "region", timevar = "recent_viol", direction = "wide")
cnt <- cbind(cnt_tot, cnt,prp )[-c(3,6,7,8,10)]
cnt[2:4] <- format(round( cnt[2:4], 1), nsmall=0, big.mark=",")
cnt$Freq.1.1 <- (round(cnt$Freq.1.1 * 100,1))
colnames(cnt) <-  c("surveyid", "cnt_tot", "cnt_no", "cnt_yes", "prp_yes"); tot_r <- cnt
tot_r$freq_prop <-  paste( tot_r$cnt_yes, " (",  tot_r$prp_yes, "%)", sep="")

svyciprop( ~I(recent_viol==1), design =  des) ## Overall prevalence 


## >> Subset the datasets for each summary analysis for VLS, incidence, HIV status  #############

des <- NULL

wt_biom <- function(df, varlist) { 

df <-as.data.frame( df %>% group_by(surveyid) %>% filter(!all(is.na(!! rlang::sym(varlist)))))
 df$surveyid <- droplevels(df$surveyid)

 
 if  (varlist %ni% c( "partners_rec_m", "partners_rec_w" )) { 
  df[,colnames(df)==varlist] <-  addNA( df[,colnames(df)==varlist], ifany = TRUE) 
 }

  
    if (varlist %in% c("conLast_m",  "conLast_w", "pay_rec",  "part_alcoholFRQ", "partners_rec_m", "partners_rec_w", 'recpart_m', 'recpart_w', "conc_m", "conc_w" )) { 
      
      formula = as.formula(paste( "~", paste(varlist,"+", "recent_viol" )))
      cnt <- as.data.frame(xtabs(formula = formula, data = df,  addNA = TRUE))
      cnt <- subset(cnt, recent_viol %ni% NA)
    
      
      des <- svydesign(ids=~0, data=df, strata= NULL, weights= ~ 1, nest=TRUE) 
    
      } else if (varlist %in% c("incidence_m", "hivstatus_m", "arv_m", "vlsup_m", "hiv_std_m" )) { 
       
         formula = as.formula(paste( "~", paste(varlist,"+", "recent_viol" )))
        cnt <- as.data.frame(xtabs(formula = formula, data = df,  addNA = TRUE))
        cnt <- subset(cnt, recent_viol %ni% NA)
      
        des <- svydesign(ids=~0, data=df, strata= NULL, weights= ~ 1, nest=TRUE) 
        } else if (varlist %in% c("incidence_w", "hivstatus_w", "arv_w", "vlsup_w", "hiv_std_w" ))
        { 
          
          formula = as.formula(paste( "~", paste(varlist,"+", "recent_viol" )))
          
  
          cnt <- as.data.frame(xtabs(formula = formula, data = df,  addNA = TRUE))
          cnt <- subset(cnt, recent_viol %ni% NA)
        
          des <- svydesign(ids=~0, data=df, strata= NULL, weights= ~ 1, nest=TRUE)  }
    
    if (varlist %in% c("partners_rec_m", "partners_rec_w")) { 
       
      formula = as.formula(paste( "~", paste(varlist)))
      avg <- svyby ( formula, by = ~ recent_viol, design = des,  svymean, na.rm = TRUE)
    
      avg[2:3] <- round(avg[2:3], 2)
    
      names(avg)[names(avg) == varlist] <- 'est'
      names(avg)[names(avg) == "se"] <- 'pct'
      avg$est_pct <-  paste( avg$est, " (", avg$pct, "%)", sep="")
    
      avg$variable <- varlist
      avg$level <- NA
      avg$ns <- length(unique(df$surveyid))
    } else { 
      
        prp <- as.data.frame(prop.table(svytable(formula, design = des), margin = 2))
  
        tot <- cbind(cnt, prp)[-c(4:5)]
        names(tot)[names(tot) == 'Freq.1'] <- 'pct'
        names(tot)[names(tot) == 'Freq'] <- 'est'
        tot$est <- format(round(tot$est, 1), nsmall=0, big.mark=",")
        tot$pct <- format(round(tot$pct * 100,1))
    

        tot$est_pct <- paste( tot$est, " (", tot$pct, "%)", sep="")
        tot$variable <- varlist
        tot$ys_denom <- aggregate(cnt$Freq, by=list(recent_viol=cnt$recent_viol), FUN=sum)[2,2] ##Yes
        tot$no_denom <- aggregate(cnt$Freq, by=list(recent_viol=cnt$recent_viol), FUN=sum)[1,2] ##no
        names(tot)[names(tot) == varlist] <- c('level')
        tot$ns <- length(unique(df$surveyid))
        avg <- tot 
    }
  
  return( avg)
  
}


con_m <- wt_biom ( df = dat_perp, varlist = "conLast_m")
con_w <- wt_biom ( df = dat_perp, varlist = "conLast_w")
pay <- wt_biom ( df = dat_perp, varlist = "pay_rec")
hiv_m <- wt_biom ( df = dat_perp[dat_perp$surveyid %ni% c("ZM2013DHS"),], varlist = "hivstatus_m")
hiv_w <- wt_biom ( df = dat_perp[dat_perp$surveyid %ni% c("ZM2013DHS"),], varlist = "hivstatus_w")

par_m <- wt_biom ( df = dat_perp, varlist = "recpart_m")
par_w <- wt_biom ( df = dat_perp, varlist = "recpart_w")
concur_m <- wt_biom ( df = dat_perp[dat_perp$partners_rec_m %ni% c(0,1) & !is.na(dat_perp$partners_rec_m), ], varlist = "conc_m")
concur_w <- wt_biom ( df = dat_perp[dat_perp$partners_rec_w %ni% c(0,1) & !is.na(dat_perp$partners_rec_w), ], varlist = "conc_w")

total_bio <- rbind(con_m, con_w, pay,  hiv_m, hiv_w,  par_m, par_w, concur_m, concur_w)
total_bio <- cbind(total_bio[total_bio$recent_viol %in% 1,], total_bio[total_bio$recent_viol %in% 0 ,])

names(total_bio)[2] = "recent_viol.1"
names(total_bio)[5] = "est_pct.1"
names(total_bio)[11] = "recent_viol.0"
names(total_bio)[14] = "est_pct.0"
total_bio1 <- total_bio[,c("recent_viol.1", "level",  "est_pct.1",  "recent_viol.0", "est_pct.0","variable", 'ys_denom', 'no_denom', "ns")]


