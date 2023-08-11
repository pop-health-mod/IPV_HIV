
rm(list=ls()) #clears workspace
rdhs::set_rdhs_config(data_frame = "data.table::as.data.table",
                      email = toString("xxxxxxx"),
                      project = "Measuring HIV diagnostic coverage in sub-Saharan Africa",
                      config_path = "rdhs.json",
                      global = FALSE)

library(rdhs)
library(dplyr)
library(survey)
library(data.table)
library(demogsurv)
library(geepack)

id_folder <- 'sk' 

if (id_folder == 'sk') { setwd("~/xxxxxxx")}

#' Identify country codes for desired countries
countries <- c("Angola", "Benin", "Benin", "Burkina Faso", "Burundi", "Cameroon",
               "Chad", "Comoros", "Congo", "Congo Democratic Republic", 
               "Cote d'Ivoire", "Ethiopia", "Gabon", "Gambia", "Ghana", 
               "Guinea", "Kenya", "Malawi", "Liberia", "Mali", "Mozambique", 
               "Namibia", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Sierra Leone",
               "South Africa", "Sudan", "Tanzania","Togo", "Uganda", "Zambia", "Zimbabwe")

cc <- dhs_countries()[CountryName %in% countries]$DHS_CountryCode


#' Identify HIV behaviour characteristics that related to HIV
survchar <- dhs_survey_characteristics()
survchar[grepl("HIV", SurveyCharacteristicName)][order(SurveyCharacteristicID)]

#' Before downloadind the individual level data, we can query the API
data_api <- dhs_data(indicatorIds = c("DV_SPV1_W_PHS",
                                       "DV_SPV1_W_SEX",
                                       "DV_SPV1_W_EMT",
                                       "DV_SPV1_W_PAS", 
                                       "DV_SPV1_W_ALL", 
                                       "DV_SPV1_W_POS", "DV_SPV1_W_ANY"),
                     breakdown = "national", surveyYearStart = 2000)
head(data_api); dim(data_api)
length(unique(data_api$SurveyId))


#' Identify surveys with HIV testing questions from desired countries (N=88)
surveys <- dhs_surveys(surveyCharacteristicIds = c(24),
                       surveyYearStart = 2000, 
                       countryIds = cc,
                       surveyType = c("DHS", "AIS", "MIS"))


#' Ever tested
dat <- dhs_data(indicatorIds = c("HA_HIVP_B_HIV", "HA_HIVP_B_HVE", "HA_HIVP_B_HVR", "HA_HIVP_B_HVL", 
                                 "HA_HIVP_B_HVU", "HA_HIVY_B_HIV", "HA_HIVY_B_HVE", "HA_HIVY_B_HVR", "HA_HIVY_B_HVL", "HA_HIVY_B_HVU", 
                                 "HA_CPHT_W_T1R", 
                                 "HA_CPHT_W_ETR", 
                                 "HA_CATH_W_NUM", 
                                 "HA_CPHT_W_TOT"), 
                                 countryIds = cc, surveyYearStart = 2000) 

setdiff(surveys$SurveyId, dat$SurveyId)

survids <- union(dat$SurveyId, surveys$SurveyId)

crd <- dhs_datasets(fileType = "CR", fileFormat = "FL")[SurveyId %in% survids] # couple's
ard1 <- dhs_datasets(fileType = "AR", fileFormat = "FL")[SurveyId %in% survids] # HIV Test Results Recode
hrd <- dhs_datasets(fileType = "HR", fileFormat = "FL")[SurveyId %in% survids] # Household  Recode


#' Get local path to dataset 

crd$path <- unlist(get_datasets(crd, clear_cache = TRUE))
ard1$path <- unlist(get_datasets(ard1, clear_cache = TRUE))
hrd$path <- unlist(get_datasets(hrd, clear_cache = TRUE))


cr <- readRDS(tail(crd$path, 1))

cvars <- c("v000",   # Alphabetic country code to identify the survey.
            "v001",  # Cluster number.
            "mv001",
            "v002",  # Household number.
            "mv002", # Household number.
            "v003",  # Respondent's line number in the household schedule.
            "mv003", 
            "v005",  # Individual sample weight (to use divide by 1e6).
            "v008a", # date of the interview
            "mv008a",
            "v012",  # Current age of respondent.
            "mv012", # Current age of respondent.
            "v022",  # Stratum 1
            "v023",  # Stratum 2
            "v044",  # Selected for the DV module
            "v101",  # Region of residence (same as v024). #
            "mv101",
            "v102",  # Rural/urban (same of v025). #
            "mv102",  
            "v190",  # Wealth index ranking *
            "mv190",  
            'mv130', # Religion
            "v191",  # Wealth index factor score
            "mv191",
  
            "v501",  # Woman's current marital or union status
            "mv501", 
            "v502",  # Currently/formerly/nxfever in union
            "mv502",
            "v503",  # >1 union
            "v505",  # number of other wives
            "mv505",
            "v511",  # Age at first marriage
            "mv511", 
            "v525",  # Ever had sex. #
            "mv525", 
            "v531",  # Age at first sex 
            "mv531", 
            "v532",  # Flag variable for inconsistencies in v525. #
            "mv532",  
            "v535",  # Ever been married
            "v729",  # Partner education (summary)
            "v106",  # Highest level of school attended 
            "mv106", 
            "mv731", # Employment in the past 12 months
            "v731",
            "v744a", # Beating justified if wife argues with husband (women)
            "v744b", # Beating justified if wife neglects the children (women)
            "v744c", # Beating justified if wife goes out without telling husband
            "v744d", # Beating justified if wife refuses to have sex with husband
            "v744e", # Beating justified if wife burns the food
            "mv744a", # Beating justified if wife argues with husband (women)
            "mv744b", # Beating justified if wife neglects the children (women)
            "mv744c", # Beating justified if wife goes out without telling husband
            "mv744d", # Beating justified if wife refuses to have sex with husband
            "mv744e", # Beating justified if wife burns the food
            "v731", # Women worked in the past 12 months 
            "v741", # Type of earnings from respondent's work 
            "v746", # Couple earning disparity
            "v739", # Final say on women's spending 
            "v743a", # Final say on women's healthcare
            "v743b", # Final say on large household purchases
            "v743d", # FInal say on visit to family and relatives
            "v751",  # Ever heard about HIV/AIDS
            "mv751",  
            "v781",  # Ever tested for HIV.
            "mv781",  
            "v826a", # Months ago most recent HIV test
            "mv826a", # Months ago most recent HIV test
           "v826",  # When was the last time they tested for HIV
           "mv826", # When was the last time they tested for HI
           "s1311", # Have you been tested? (MW2010)
           "ms1311",# Have you been tested? (MW2010)
           "s1312", # Months was the last time they tested for HIV (Malawi 2010)
           "ms1312",# Months was the last time they tested for HIV (Malawi 2010)
           "v840",  # Tested for HIV as part of ANC visit
           "s816e", # Tested for HIV as part of ANC visit (CM2004)
           "s819",  # test for ANC as part of ANC visit (RW2005)
           "v840a", # Tested for HIV between the time went for delivery and before baby was born
           "v841",  # Got results of HIV test as part of ANC visit (general)
           "s816g", # Got results of HIV test as part of ANC visit (CM2004)
           "s820",  # Got results of HIV test as part of ANC visit (CRW2005)
           "v843",  # Tested since ANC 
           "v841a", # Got results of HIV test when tested before baby was born ( mz2015AIS)
           "v828",  # Received result from last HIV test
           "v761",  # Condom use at last sex ( with most recent partner) (past 12 months)
           "mv761",  
           'mv761b', # Condom use at last sex with 2nd most recent partner 
           'mv761c', # Condom use at last sex with 3rd most recent partner 
           'mv833a', # Condom use every time they had sex with their most recent partner (past 12 months)
           "v836",   # Lifetime number of sexual partners 
           "mv836", 
           "s539",   # Lifetime number of sex partners (RW2005DHS)
           "sm438",  # Lifetime number of sex partners (RW2005DHS)
           "s523a",  # Lifetime number of partners (CM2004DHS)
           "ms523a", # Lifetime number of partners (CM2004DHS)
           "mv766b", # Number of sex partners (last 12 months)
           "v766b",  # Number of sex partners (last 12 months)
           "mv852a", # How long ago first sex with the most recent partner 
           "mv852b", # How long ago first sex with 2nd the most recent partner 
           "mv852c", # How long ago first sex with 3rd the most recent partner 
           
           "mv527",  # How long ago last sex with the most recent partner 
           "mv832b", # How long ago last  sex with 2nd the most recent partner 
           "mv832c", # How long ago last  sex with 3rd the most recent partner 
          
           
           "v852a", # How long ago first sex with the most recent partner 
           "v852b", # How long ago first sex with 2nd the most recent partner 
           "v852c", # How long ago first sex with 3rd the most recent partner 
           
           "v527",  # How long ago last sex with the most recent partner 
           "v832b", # How long ago last sex with 2nd the most recent partner 
           "v832c", # How long ago last sex with 3rd the most recent partner 
           
           
           "d105a", # Pushed, thrown at
           "d105b", # Slapped
           "d105c", # Punched
           "d105d", # Kicked/dragged
           "d105e", # Chocked/burned
           "d105f", # Threaten/attacked with knife or other weapon (not for all surveys)
           "d105g", # CS physical violence OR attack with a knife or other weapon  (not for all surveys)
           "d105j", # Arm twisted or hair pulled (not for all surveys) 
           "d105h", # Physically forced into sex 
           "d105i", # Emotionally forced into sex
           "d105k", # Physically forced to perform acts 
           "d105an", # Pushed, shook, or threw something, past 12 month
           "d105bn", # Times slapped in last 12 months
           "d105cn", # Times husband punched or hit with object
           "d105dn", # Times kicked or dragged in past 12 months
           "d105en", # Times husband tried to strangle or burn, past 12 months
           "d105fn", # Times husband trheatned with a knife or run 
           "d105gn", # Times huband attacked w/ knife of gun or other weapon 
           "d105hn", # Times husband physically foced sex in the last 12 months 
           "d105in", # Times Times spouse  forced other sex acts
           "d105kn", # Times Times spouse physically forced to perform sexual acts respondent didn't want to
           "d101a",  # Angry to talking to another men
           "d101b", # Accused of being unfaithful 
           "d101c", # Permit to meet other friends 
           "d101d", # Limit contact w/ family
           "d101e", # Knowing where at all times 
           "d101f", # Trust you with any money?
           "d005",  # Weights for DV module
           
           "d113",  # Partner alcohol consumption (Yes/No)
           "d114",  # Frequency partner gets drunk (in general)

           "mv791", #Payment for sex (ever)
           "mv793", #Payment for sex (past 12 months)

           "sviral", ## vls
           "slagrecn", #Incidence
           
           "sbioarv", 
           # Male ARV
           "sm518",  # Have ever taken ARV (mozambique)
           "sm519",  # Are currently taking ARV (mozambique)) [ self-reported]

           "sm718", # Was told to take arvs (Angola)
           "sm719", # Currently taking ARVs (Angola) )  [ self-reported]
   
           "sm908", # Taking ARVS daily (malawi) [self-reported ]
           "sm909", # Ever taken ARVs daily (malawi) 
           
           #Female  ARV
           "s718",  # Have ever taken ARV (mozambique)
           "s719",  # Are currently taking ARV (mozambique)) [ self-reported]
           
           "s1028", # Was told to take arvs (angola)
           "s1029", # Currently taking ARVs (Angola) )  [ self-reported]
           
           "s1318", # Taking ARVS daily (malawi) [self-reported ]
           "s1319"  # Ever taken ARVs daily (malawi) [
           ) 
arvars <- c("hivclust", "hivnumb", "hivline", "hiv03", "hiv05", "slagrecn", "sbioarv", "sviral") #03-blood T result; 05- HIV weight
hvars <- c("hv219", "hv001", "hv002")
allvars <- c("SurveyId", "CountryName", "SurveyYear", cvars, hvars, arvars)


vars <- c("m_hiv03", "m_hiv05", "w_hiv03", "w_hiv05", "slagrecn_m","slagrecn_w", "sbioarv_m", "sbioarv_w", "sviral_m", "sviral_w" ) 
allvarsT <- c("SurveyId", "CountryName", "SurveyYear", cvars, hvars, vars)



##>> Load and merge datasets  ############

datlstc <- list()
for(survid in crd$SurveyId){
  
  print(survid)
  cr <- readRDS(crd[SurveyId == survid]$path)
  cr <- cr[, intersect(cvars, names(cr))] 
  
  dat<-cr
  if(nrow(ard1[SurveyId==survid])){
    ar<-readRDS(ard1[SurveyId == survid]$path)
    ar[setdiff(arvars, names(ar))] <- NA 
    ar <- as.data.frame(ar[, arvars]) 
    ar[, c("hivclust", "hivnumb", "hivline")] <- lapply(ar[, c("hivclust", "hivnumb", "hivline")], as.integer)
    dat <- dplyr::left_join(dat, ar, by=c("v001" = "hivclust", "v002" = "hivnumb", "v003" = "hivline")) ## women's HIV status 
    names(dat)[names(dat) == "hiv03"] <- "w_hiv03"
    names(dat)[names(dat) == "hiv05"] <- "w_hiv05"
    names(dat)[names(dat) == "slagrecn"] <- "slagrecn_w"
    names(dat)[names(dat) == "sviral"] <- "sviral_w"
    names(dat)[names(dat) == "sbioarv"] <- "sbioarv_w"
 

    dat <- dplyr::left_join(dat, ar, by=c("mv001" = "hivclust", "mv002" = "hivnumb", "mv003" = "hivline")) ## men's HIV status 
    names(dat)[names(dat) == "hiv03"] <- "m_hiv03"
    names(dat)[names(dat) == "hiv05"] <- "m_hiv05"
    names(dat)[names(dat) == "slagrecn"] <- "slagrecn_m"
    names(dat)[names(dat) == "sviral"] <- "sviral_m"
    names(dat)[names(dat) == "sbioarv"] <- "sbioarv_m"
  }
  
  
  if(nrow(hrd[SurveyId==survid])){
    hr<-readRDS(hrd[SurveyId == survid]$path)
    hr[setdiff(hvars, names(hr))] <- NA 
    hr <- as.data.frame(hr[, hvars]) 
    hr[, c("hv001", "hv002")] <- lapply(hr[, c("hv001", "hv002")], as.integer)
    dat <- dplyr::left_join(dat, hr, by=c("v001" = "hv001", "v002" = "hv002"))

  }
  
  dat[setdiff(allvarsT, names(dat))] <- NA
  dat$SurveyId <- survid
  dat$CountryName <- crd[SurveyId == survid]$CountryName
  dat$SurveyYear <- crd[SurveyId == survid]$SurveyYear
  
  datlstc[[survid]] <- dat[allvarsT]
}


set_na <- function(x, na_codes = 9){ x[x %in% na_codes] <- NA; x }
'%ni%' <- Negate('%in%')


##>> Recode function  ################################################

recode_dhs_c <- function(dat) {
  
 
  name <- dat$SurveyId[1]
  print(name)
  
  dat$country <- dat$CountryName
  dat$surveyid <- dat$SurveyId
  dat$survyear <- dat$SurveyYear
  dat$psu <- as.numeric(as.character(dat$v001))
  
  dat$psu_u <- as.character(paste0(dat$psu,sep= "_",dat$surveyid))
  dat$stratum <- as.character(paste0(dat$v101,sep= "_",dat$v102)) 
  dat$stratum_dv <- as.character(dat$stratum) 
  
  dat$household <- as.numeric(dat$v002)
  dat$linew <- as.numeric(dat$v003) ## Women
  dat$linem <- as.numeric(dat$mv003) ## Men

  dat$region <- as.factor(dat$v101)
  dat$indweight_w <- dat$v005 / 1e6

  dat$hivweight_w <- dat$w_hiv05 / 1e6
  dat$hivweight_m <- dat$m_hiv05 / 1e6
  dat$dvweight<-dat$d005/1e6
  
  #Add region 
  
  if (all(dat$SurveyId %in% c("BJ2012DHS", "BJ2017DHS", "BF2010DHS", "CI2012DHS", "GM2013DHS", "GH2003DHS","GH2008DHS", "GH2014DHS", "GN2005DHS", "GN2012DHS", 
                          "GN2018DHS", "GM2019DHS", "LB2007DHS", "LB2013DHS", "LB2019DHS","ML2006DHS", "ML2012DHS", "ML2018DHS", "NI2012DHS", "NG2003DHS","NG2008DHS", "NG2013DHS", "NG2018DHS", "SN2005DHS", "SN2010DHS", "SN2014DHS", "SN2015DHS", "SN2016DHS", "SN2017DHS", "SL2013DHS", "SL2019DHS", "TG2013DHS"))) {
    dat$region <- 'Western Africa' }
  else if (all(dat$SurveyId  %in% c("SZ2016PHIA", "LS2004DHS", "LS2009DHS", "LS2014DHS", 
                                "NM2000DHS", "NM2013DHS", "ZA2016DHS", "ZA2002SABSSM", "ZA2017SABSSM"))) {
    dat$region <- 'Southern Africa'}
  else if (all(dat$SurveyId %in%  c("BU2010DHS", "BU2016DHS", "KM2012DHS", "ET2005DHS", "ET2011DHS", "ET2016DHS", 
                                "ET2000DHS", "KE2003DHS", "KE2008DHS", "KE2012AIS", "KE2014DHS", "MD2004DHS", "MW2000DHS", "MW2004DHS", "MW2010DHS", "MW2015DHS", "MW2015PHIA", "MZ2003DHS", "MZ2011DHS", "MZ2015AIS", "RW2000DHS", "RW2005DHS", "RW2010DHS", "RW2015DHS", "RW2019DHS", "TZ2004DHS", "TZ2010DHS", "TZ2012AIS", "TZ2015DHS", "UG2000DHS","UG2006DHS" , "UG2011DHS", "UG2016DHS", "ZM2002DHS", "ZM2007DHS","ZM2013DHS", "ZM2016PHIA", "ZM2018DHS", "ZW2005DHS", "ZW2010DHS", "ZW2015DHS"))) {
    dat$region <- 'Eastern Africa' }
  else if (all(dat$SurveyId %in% c("AO2015DHS", "CD2007DHS", "CM2004DHS", "CM2011DHS", "CM2018DHS", "TD2004DHS", "TD2014DHS", "CG2011DHS", "CD2013DHS", "GA2012DHS", "GA2000DHS", "ST2008DHS"))) {
    dat$region <- 'Central Africa'}
  
  # ad period of study 
  if (all(dat$SurveyId %in% c("CM2004DHS", "KE2003DHS", "MW2004DHS"))) {
    dat$period <- '2000-2004' } 
  else if (all(dat$SurveyId  %in% c("RW2005DHS", "UG2006DHS", "ML2006DHS", "CD2007DHS", "LB2007DHS", "GH2008DHS", "NG2008DHS", "ZW2005DHS", "ZM2007DHS", "ST2008DHS", "KE2008DHS"))) {
    dat$period <- '2005-2009'} 
  else if (all(dat$SurveyId %in%  c("MW2010DHS", "RW2010DHS", "TZ2010DHS", "ZM2010DHS", "BF2010DHS", "MZ2011DHS", "CI2012DHS", "UG2011DHS","GA2012DHS", "KM2012DHS", "ML2012DHS",
                                "NM2013DHS", "GM2012DHS", "GM2013DHS", "NG2013DHS", "CD2013DHS", "ZM2013DHS", "TD2014DHS", "SL2013DHS", "TG2013DHS", "KE2014DHS", "ZW2010DHS", "ZW2013DHS"))) {
    dat$period <- '2010-2014' } 
  else if (all(dat$SurveyId %in% c("MW2015DHS", "RW2015DHS",  "MZ2015AIS", "AO2015DHS", "ZW2015DHS", "BU2016DHS", "ET2016DHS", "UG2016DHS", "SN2017DHS", "ZA2016DHS", "CM2018DHS", "ZM2018DHS", "ML2018DHS", "RW2019DHS", "NG2018DHS",  "TZ2015DHS",
                               "SL2019DHS", "LB2019DHS", "GM2019DHS"))) {
    dat$period <- '2015-2019'} 
  
  else {dat$period<- NA}
  
  
  # Age
  dat$agew <- as.numeric(dat$v012)
  dat$agem <- as.numeric(dat$mv012)
  
  # Couple age disparity
  dat$agedisp <- (dat$agem - dat$agew)
  
  # Age grouped 
  agegr = c('15-24', '25-34', '35-44', '45-64', "65+")
  dat$agegrw <- cut(dat$agew, breaks = c(15, 25, 35, 45, 65,  Inf), labels = agegr, TRUE, FALSE)
  dat$agegrm <- cut(dat$agem, breaks = c(15, 25, 35, 45, 65,  Inf), labels = agegr, TRUE, FALSE)
  
  
  # Residence type (urban or rural)
  dat$restype <- factor(set_na(dat$v102, 9) > 1, c(FALSE, TRUE), c("Urban", "Rural")) 
  dat$resprev <- as.integer(set_na(dat$v102, 9) > 1) #
  
  
  # Education - women & men 
  dat$schoolw<-factor(set_na(dat$v106, 9)) #codes as is +NA
  dat$schoolw<- factor(dat$schoolw, levels=c(0,1,2,3),labels=c("None","Primary", "Secondary", "Higher"))
  dat$schoolm<-factor(set_na(dat$mv106, 9)) #codes as is +NA
  dat$schoolm<- factor(dat$schoolm, levels=c(0,1,2,3),labels=c("None","Primary", "Secondary", "Higher"))
  
  
  # Recently or currently employed  - women & men 
  dat$employmentm <-  as.numeric(set_na(dat$mv731, 9) > 0) # 
  dat$employmentw <-  as.numeric(set_na(dat$v731, 9) > 0) # 
  

  # Wealth index - women & men 
  dat$wealthw <- factor(set_na(dat$v190, 9)) 
  dat$wealthm <- factor(set_na(dat$mv190, 9)) 

  # Polygamy- men 
  dat$poly<- as.numeric(set_na(dat$mv505, 99) > 1) 

  
  # Age at first sex - women & men 
  dat$age1s_w<- as.integer(set_na(dat$v531, c(0, 97:99)))
  dat$age1s_m<- as.integer(set_na(dat$mv531, c(0, 97:99)))
  

  ## Justification of wife-beating : women 
  dat$go_outw <- as.integer(set_na(dat$v744a, 8:9))
  dat$neglectw <- as.integer(set_na(dat$v744b, 8:9))
  dat$arguew <- as.integer(set_na(dat$v744c, 8:9))
  dat$ref_sexw <- as.integer(set_na(dat$v744d, 8:9))
  dat$burnw <- as.integer(set_na(dat$v744e, 8:9))
 
  dat$justify_w =apply(dat[ , c("go_outw", "neglectw", "arguew", "ref_sexw", "burnw")], 1, function(k) { ifelse(any(k== 1, na.rm = TRUE), 1,
                                                                                                          ifelse (all (k==0), 0, NA))})
 
 ## Justification of wife-beating : men 
   dat$go_outm <- as.integer(set_na(dat$mv744a, 8:9))
   dat$neglectm <- as.integer(set_na(dat$mv744b, 8:9))
   dat$arguem <- as.integer(set_na(dat$mv744c, 8:9))
   dat$ref_sexm <- as.integer(set_na(dat$mv744d, 8:9))
   dat$burnm <- as.integer(set_na(dat$mv744e, 8:9))
 
   dat$justify_m =apply(dat[ , c("go_outm", "neglectm", "arguem", "ref_sexm", "burnm")], 1, function(k) { ifelse(any(k== 1, na.rm = TRUE), "1",
                                                                                                          ifelse (all (k==0), 0, NA))})

 
 
 # Couple earning disparity - women 

 dat$v731 <- as.numeric(set_na(dat$v731, 9)) ## Employment 0 - not working 
 dat$v741 <- as.numeric(set_na(dat$v741, 9)) ## Payed how? 
 
 dat$earn <- as.numeric(set_na(dat$v746, 4:9))
 dat$earn[ dat$v731  %in%  c(0)] <- c("Not paid") 
 dat$earn[ dat$v741  %in%  c(0) | dat$v741  %in%  c(3) ] <- c("Not paid") 
 dat$earn_disp <- factor(dat$earn, levels=c(1,2,3, "Not paid"),labels=c("More than him", "Less than him", "Same", "Not paid"))
 dat$earn_disp <- factor (dat$earn_dis, levels = c("Less than him", "Same", "More than him", "Not paid"))


 # Women makes decision alone or jointly with her husband 
 # Final say on women's spending  (this is asked to only women who are getting paid in cash or kind)
 dat$v739 <- as.numeric(set_na(dat$v739, 9)) ##
 dat$spend <- as.integer(set_na(dat$v739, 9) < 3) 

 # Final say on women's healthcare 
 dat$v743a <- as.numeric(set_na(dat$v743a, 9)) ##
 dat$health <- as.integer(set_na(dat$v743a, 9) < 3) 

 
 # Final say on large household purchases 
 dat$v743b <- as.numeric(set_na(dat$v743b, 9)) ##
 dat$purchase <- as.integer(set_na(dat$v743b, 9) < 3)  
 
 # Final say on visit to family and relatives 
 dat$v743d <- as.numeric(set_na(dat$v743d, 9)) #
 dat$visit <- as.integer(set_na(dat$v743d, 9) < 3)  #
 

## Make all 3 decisions on their own or with their partners. 
## Health  MBD (missing by design) in Liberia
 if(all(dat$SurveyId %in% c("LB2007DHS")))  {
 dat$decision =apply(dat[ , c("purchase", "visit")], 1, function(k) { ifelse(all(k== 1), 1,
                                                                                                ifelse (any (is.na(k)), NA,
                                                                                                               ifelse (any (k==0, na.rm = TRUE), 0, NA)))}) 
 } else { 
   dat$decision =apply(dat[ , c("health", "purchase", "visit")], 1, function(k) { ifelse(all(k== 1), 1,
                                                                                                  ifelse (any (is.na(k)), NA,
                                                                                                          ifelse (any (k==0, na.rm = TRUE), 0, NA)))}) 
   }
 

 # HIV biomarker outcomes: Women & men 
 dat$hivstatus_w <- factor(set_na(dat$w_hiv03, 4:9) > 0, c(FALSE, TRUE), c("Negative", "Positive"))
 dat$hivstatus_m <- factor(set_na(dat$m_hiv03, 4:9) > 0, c(FALSE, TRUE), c("Negative", "Positive")) 


 # Condom use at last sex: Women & men 
 dat$conLast_w <-as.integer(set_na(dat$v761, 8:9))
 dat$conLast_m <-as.integer(set_na(dat$mv761, 8:9))
 

 # Lifetime number of sexual partners: Women 
 if(all(dat$SurveyId %in% c("RW2005DHS"))) {
   dat$life_w <- set_na(dat$s539, 96:99)
 } else {
   if(all(dat$SurveyId %in% c("CM2004DHS"))) {
     dat$life_w <- set_na(dat$s523a, c(0,95:99)) #if never had sex, code as NA 
   } else {
     dat$life_w <- set_na(dat$v836, 96:99)
   }
 }
 
 dat$life_w<-as.numeric(dat$life_w)


 # Lifetime number of sexual partners: men 
 if(all(dat$SurveyId %in% c("RW2005DHS"))) {
   dat$life_m <- set_na(dat$sm438, 96:99)
 } else {
   if(all(dat$SurveyId %in% c("CM2004DHS"))) {
     dat$life_m <- set_na(dat$ms523a, c(0,95:99)) #if never had sex, code as NA 
   } else {
     dat$life_m <- set_na(dat$mv836, 96:99)
   }
 }
 
 dat$life_m<-as.numeric(dat$life_m)
 
 # Number of sex partners in the past 12 months: men & women 

 dat$partners_rec_m <- as.integer(set_na(dat$mv766b, 98:99))
 dat$partners_rec_w <- as.integer(set_na(dat$v766b, 98:99))

  ## Concurency: 
  # Primary indicator: Point prevalence of concurrent partnerships, defined as the proportion of
  # women and men age 15-49 with more than one ongoing sexual partnership at the point in time six
  # months before the interview. 

 
 dat$first_recm <- as.numeric(set_na(dat$mv852a, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$first_2recm <- as.numeric(set_na(dat$mv852b, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$first_3recm <- as.numeric(set_na(dat$mv852c, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$last_recm <- as.numeric(set_na(dat$mv527, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$last_2recm <- as.numeric(set_na(dat$mv832b, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$last_3recm <- as.numeric(set_na(dat$mv832c, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 
 
 dat$first_recw <- as.numeric(set_na(dat$v852a, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$first_2recw <- as.numeric(set_na(dat$v852b, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$first_3recw <- as.numeric(set_na(dat$v852c, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$last_recw <- as.numeric(set_na(dat$v527, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$last_2recw <- as.numeric(set_na(dat$v832b, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 dat$last_3recw <- as.numeric(set_na(dat$v832c, c(198, 199, 299, 398, 399, 498, 499, 995, 996, 997, 998, 999))) 
 
 ## Convert to days since last sex 
 dat <- dat%>%
   mutate(across(c(first_recm, first_2recm, first_3recm,  first_recw,  first_2recw,  first_3recw , last_recm, last_2recm, last_3recm,  last_recw,  last_2recw,  last_3recw), ~ case_when(
     .x > 100  &  .x< 200 ~ (.x - 100), 
     .x > 200  &  .x< 300 ~ (.x - 200)*7, 
     .x > 300  &  .x< 400 ~ (.x - 300)*30, 
     .x > 400  &  .x< 500 ~ (.x - 400)*365, 
   ), .names = "{.col}_n")
   )
 
 dat$conc_cut <- as.numeric(182)

  # First sex was  before the six-month cutoff and last sex was after  the six-month cutoff 
 dat$had_mostrecm <- ifelse( dat$first_recm_n > 182.5 &  dat$last_recm_n < 182.5, 1, 0)
 dat$had_2ndrecm <- ifelse ( dat$first_2recm_n > 182.5 & dat$last_2recm_n < 182.5, 1, 0)
 dat$had_3rdrecm <- ifelse(dat$first_3recm_n > 182.5 & dat$last_3recm_n < 182.5, 1, 0)
 
 dat$had_mostrecw <- ifelse( dat$first_recw_n > 182.5 &  dat$last_recw_n < 182.5, 1, 0)
 dat$had_2ndrecw <- ifelse ( dat$first_2recw_n > 182.5 & dat$last_2recw_n < 182.5, 1, 0)
 dat$had_3rdrecw <- ifelse(dat$first_3recw_n > 182.5 & dat$last_3recw_n < 182.5, 1, 0)
 
 
 fun1 <- function(x) { 
   x$conc_m= ifelse(rowSums(x[,1:3], na.rm=TRUE) >= 2, 1, ## who had two or three sexual partnerships in the past 12 months that began before the cutoff and continued after the cutoff 
                    ifelse (rowSums(x[,1:3], na.rm=TRUE) <= 1 & x[,4] > 1, 0, #who had multiple sexual partnerships in the past 12 months but who had no sexual partnerships or only one sexual partnership that met the  cutoff
                            ifelse (x[,4] <=1, NA, NA)))  #Respondents who had no sexual partners in the past 12 months or who had exactly one sexual partner 
   return (x) 
 }
 
 fun2 <- function(x) { 
   x$conc_w= ifelse(rowSums(x[,1:3], na.rm=TRUE) >= 2, 1, 
                    ifelse (rowSums(x[,1:3], na.rm=TRUE) <= 1 & x[,4] > 1, 0, 
                            ifelse (x[,4] <=1, NA, NA)))  
   
   return (x) 
 }
 
 
dat1 <- fun1(x = dat[ , c("had_mostrecm","had_2ndrecm","had_3rdrecm", 'partners_rec_m')])
dat2 <- fun2(x = dat[ , c("had_mostrecw","had_2ndrecw","had_3rdrecw", 'partners_rec_w')])
 
 dat <- cbind (dat,dat1)
 dat <- cbind (dat,dat2)
 
 # Selected for DV module
 dat$DVSel <- as.integer(ifelse(dat$v044==1,1,0))
 
 
 # Payment for sex 
 dat$pay <- as.numeric(set_na(dat$mv791, 9))
 dat$pay_rec <- as.numeric(set_na(dat$mv793, 9))
 
 # Partner alcohol consumption frequency
 dat$part_alcohol <- as.numeric(set_na(dat$d113, 8:9))
 dat$part_alcoholFRQ <- factor(set_na(dat$d114, 9))
 dat$part_alcoholFRQ<- factor(dat$part_alcoholFRQ, levels=c(0,1,2),labels=c("Never","Often", "Sometimes"))
 dat$part_alcoholFRQ[dat$part_alcohol %in% (0)] <- c("Never")
 dat$part_alcoholFRQ<- factor(dat$part_alcoholFRQ, levels=c("Never", "Sometimes", "Often"))
 
 
 # women ever married or cohabitating 
 dat$evermarried <- as.integer(set_na(dat$v502, 9) > 0) #
 dat$currMarried <- as.integer(set_na(dat$v502, 9) == 1) #
 
 
 # Female-led household decision-making
 dat$head <- factor(set_na(dat$hv219, 9))
 dat$head<- factor(dat$head, levels=c(1,2),labels=c("Male", "Female"))
 
 
 # Recode physical violence
 dat$push<-as.integer(set_na(dat$d105a,9) >0) 
 dat$slap<-as.integer(set_na(dat$d105b,9) >0)
 dat$punch<-as.integer(set_na(dat$d105c,9) >0)
 dat$kick<-as.integer(set_na(dat$d105d,9) >0)
 dat$choke <-as.integer(set_na(dat$d105e,9) >0)
 dat$threatAttack<-as.integer(set_na(dat$d105f,9) >0)
 dat$armtw<-as.integer(set_na(dat$d105j,9) >0)
 dat$attack<-as.integer(set_na(dat$d105g,9) >0) # some surveys have attack separate and treat instead of threat/attack
 
 
 # Recode sexual violence 
 dat$ph_sex<-as.integer(set_na(dat$d105h,9) >0)
 dat$em_sex<-as.integer(set_na(dat$d105i,9) >0)
 dat$forced_act<-as.integer(set_na(dat$d105k,9) >0)
 
 
 # Categorical frequency indicator ( 1/2 often/sometimes; 3 not in the last 12 months, 0 - never)
 dat$fPHS1<-ifelse (dat$d105h==4|dat$d105h==9, NA,
                    ifelse (dat$d105h==0 | dat$d105h==3, 0, 
                            ifelse (dat$d105h==1 | dat$d105h==2, 1, NA))) 
 dat$fEMS1<-ifelse (dat$d105i==4|dat$d105i==9, NA,
                    ifelse (dat$d105i==0 | dat$d105i==3, 0,
                            ifelse (dat$d105i==1 | dat$d105i==2, 1, NA))) 
 
 dat$fForc1<-ifelse (dat$d105k==4|dat$d105k==9, NA,
                     ifelse (dat$d105k==0 | dat$d105k==3, 0, 
                             ifelse (dat$d105k==1 | dat$d105k==2, 1, NA))) 
 
 dat$fPush1<-ifelse (dat$d105a==4|dat$d105a==9, NA,
                     ifelse (dat$d105a==0 | dat$d105a==3, 0, # if 0, has not experianced violence <12
                             ifelse (dat$d105a==1 | dat$d105a==2, 1, NA))) #if 1, has experianced violence <12 
 
 dat$fSlap1<-ifelse (dat$d105b==4|dat$d105b==9, NA,
                     ifelse (dat$d105b==0 | dat$d105b==3, 0, 
                             ifelse (dat$d105b==1 | dat$d105b==2, 1, NA))) 
 dat$fPunch1<-ifelse (dat$d105c==4|dat$d105c==9, NA,
                      ifelse (dat$d105c==0 | dat$d105c==3, 0, 
                              ifelse (dat$d105c==1 | dat$d105c==2, 1, NA))) 
 dat$fKick1<-ifelse (dat$d105d==4|dat$d105d==9, NA,
                     ifelse (dat$d105d==0 | dat$d105d==3, 0, 
                             ifelse (dat$d105d==1 | dat$d105d==2, 1, NA))) 
 dat$fChoke1<-ifelse (dat$d105e==4|dat$d105e==9, NA,
                      ifelse (dat$d105e==0 | dat$d105e==3, 0, 
                              ifelse (dat$d105e==1 | dat$d105e==2, 1, NA))) 
 
 dat$fTreat1<-ifelse (dat$d105f==4|dat$d105f==9, NA,
                      ifelse (dat$d105f==0 | dat$d105f==3, 0, 
                              ifelse (dat$d105f==1 | dat$d105f==2, 1, NA))) 
 
 dat$fArmtw1<-ifelse (dat$d105j==4| dat$d105j==9, NA,
                      ifelse (dat$d105j==0 | dat$d105j==3, 0, 
                              ifelse (dat$d105j==1 | dat$d105j==2, 1, NA))) 
 
 dat$fAttack1<-ifelse (dat$d105g==4|dat$d105g==9, NA,    
                       ifelse (dat$d105g==0 | dat$d105g==3, 0, 
                               ifelse (dat$d105g==1 | dat$d105g==2, 1, NA)))

 
 # Continuous frequency indicator
 dat$fPush<-as.integer(set_na(dat$d105an, 94:99))
 dat$fSlap<-as.integer(set_na(dat$d105bn, 94:99)) 
 dat$fPunch<-as.integer(set_na(dat$d105cn, 94:99)) 
 dat$fKick<-as.integer(set_na(dat$d105dn, 94:99)) 
 dat$fStrangle<-as.integer(set_na(dat$d105en, 94:99)) 
 dat$fThreatKnife<-as.integer(set_na(dat$d105fn, 94:99)) 
 dat$fAttack<-as.integer(set_na(dat$d105gn, 94:99)) 
 dat$fForceSex<-as.integer(set_na(dat$d105hn, 94:99)) 
 dat$fEmoSex<-as.integer(set_na(dat$d105in, 94:99)) 
 
 ##### Create Physical Violence Indicators #######
 ## Important to ensure that 'missing by design is accounted for [ any NA is coded as NA, unless its NA by design]
 # Attack is collected in these 8 surveys. 
 
 if (all(dat$SurveyId %in% c("CM2004DHS","CD2007DHS", "KE2003DHS","MW2004DHS", "RW2005DHS","BF2010DHS","CI2012DHS","ML2006DHS"))) {
   dat$ever_phys=apply(dat[ , c("push","slap","punch","kick","choke","threatAttack","attack")], 1, function(y) { ifelse(any(y== 1, na.rm = TRUE), "Yes",
                                                                                                                        ifelse (all (y==0), "No", NA))})
 }
 ## Arm twist collected in all surveys except for these 10 surveys
 else if (all(dat$SurveyId %ni% c("CM2004DHS","CD2007DHS", "KE2003DHS","MW2004DHS", "RW2005DHS", "ZW2005DHS","BF2010DHS","CI2012DHS","ML2006DHS", "ZA2016DHS", "ZW2005DHS", "KE2008DHS", 
                                  "LB2007DHS", "TZ2010DHS", "ZM2007DHS"))) { 
   dat$ever_phys=apply(dat[ , c("push","slap","punch","kick","choke","threatAttack")], 1, function(y) { ifelse(any(y== 1, na.rm = TRUE), "Yes",
                                                                                                               ifelse (all (y==0), "No", NA))}) 
 }
  ## SA collects only these 4 compoenents 
 else if (all(dat$SurveyId %in% c("ZA2016DHS"))) {
   dat$ever_phys=apply(dat[ , c("push","kick","choke","threatAttack")], 1, function(y) { ifelse(any(y== 1, na.rm = TRUE), "Yes",
                                                                                                ifelse (all (y==0), "No", NA))}) 
 }
 
 else if(all(dat$SurveyId %in% c("ZW2005DHS"))) { 
   dat$ever_phys=apply(dat[ , c("push","slap","punch","choke","threatAttack", "attack")], 1, function(k) { ifelse(any(k== 1, na.rm = TRUE), "Yes",
                                                                                                                  ifelse (all (k==0), "No", NA))})
 }
 else if(all(dat$SurveyId %in% c("KE2008DHS", "LB2007DHS", "TZ2010DHS", "ZM2007DHS"))) { 
   dat$ever_phys=apply(dat[ , c("push","slap","punch","kick", "choke")], 1, function(k) { ifelse(any(k== 1, na.rm = TRUE), "Yes",
                                                                                                 ifelse (all (k==0), "No", NA))})
 }
 
 
 ### Create Sexual Violence Indicators ###
 # Account for missingness by design 
 if (all(is.na(dat$forced_act))) {
   dat$ever_sex_vio=apply(dat[,c("ph_sex", "em_sex")], 1, function (m) { ifelse(any(m== 1, na.rm = TRUE), "Yes",
                                                                                ifelse (all (m==0), "No", NA))})
   
 } else if (all(is.na(dat$em_sex))) {
   dat$ever_sex_vio=apply(dat[,c("ph_sex", "forced_act")], 1, function (m) { ifelse(any(m== 1, na.rm = TRUE), "Yes",
                                                                                    ifelse (all (m==0), "No", NA))}) 
   
 }
 else { dat$ever_sex_vio=apply(dat[,c("ph_sex", "em_sex", "forced_act")], 1, function (m) { ifelse(any(m== 1, na.rm = TRUE), "Yes",
                                                                                                   ifelse (all (m==0), "No", NA))}) }
 
 
 ##' Any violence 
 dat$any_viol = apply (dat[, c("ever_sex_vio", "ever_phys")], 1, function (m) { ifelse(any(m %in% "Yes", na.rm = TRUE), "Yes",
                                                                                       ifelse (all (m %in% "No"), "No", NA))})
 

 ## Replace all covariates with completely missing data (survey does not collect this information) with random value  so that they are not counted as "NA"
 if (all(is.na(dat$fSlap1))) { dat$fSlap1 <- 7} 
 if (all(is.na(dat$fPush1))) { dat$fPush1 <- 7} 
 if (all(is.na(dat$fPunch1))) { dat$fPunch1 <- 7} 
 if (all(is.na(dat$fKick1))) { dat$fKick1<- 7} 
 if (all(is.na(dat$fChoke1))) { dat$fChoke1<- 7} 
 if (all(is.na(dat$fTreat1))) { dat$fTreat1<- 7} 
 if (all(is.na(dat$fArmtw1))) { dat$fArmtw1<- 7} 
 if (all(is.na(dat$fPHS1))) { dat$fPHS1<- 7} 
 if (all(is.na(dat$fEMS1))) { dat$fEMS1<- 7} 
 if (all(is.na(dat$fForc1))) { dat$fForc1<- 7} 
 if (all(is.na(dat$fAttack1))) { dat$fAttack1 <- 7} 
 

 
 
 ##' Create frequency variable 
 ##' Continuous: If any violent acts happened >0 times in the past 12 months code 1. If all types are "None" code as No; if either is NA, code as NA; if all are NA, code as NA. 
 ##' Categorical: Now that we have replaced all missing variables with 7, the only remaining missing values are truly NA. 
 if(all(dat$SurveyId %in% c("CM2004DHS","MW2004DHS", "KE2003DHS", 
                        "RW2005DHS",  
                        "ML2006DHS"))){
   dat$recent_viol=apply(dat[, c("fPush","fSlap","fPunch","fKick","fStrangle","fThreatKnife", "fAttack", "fForceSex", "fEmoSex")],1,    function(x) { ifelse(any(x > 0, na.rm = TRUE), "Yes", 
                                                                                                                                                             ifelse (all (x==0), "No", NA))})
   dat$recent_viol<- ifelse (dat$any_viol=="No" & is.na(dat$recent_viol), "No", dat$recent_viol)
   
 }  else { dat$recent_viol=apply(dat[, c(  "fSlap1", "fPush1", "fPunch1", 
                                           "fKick1", "fChoke1", "fTreat1",
                                           "fPHS1", "fEMS1", "fForc1","fAttack1")],1, function(x) {ifelse(any(x == 1, na.rm = TRUE), "Yes",
                                                                                                          ifelse(all(x == 0 | x == 7), "No", NA))})
 
 }
 
 

 # ARV  intake:  self reported  for men 
 if(all(dat$SurveyId %in% c("MZ2015AIS"))) {
   dat$ever<- as.integer(set_na(dat$sm518, 8:9)) ## ever taking ARVs for mozambique
   dat$curr<- as.integer(set_na(dat$sm519, 9)) 
 } 
   else  {  dat$curr <-NA
   dat$ever <- NA
   }

 # ARV  intake:  biomarker-based  for men 
 if(all(dat$SurveyId %in% c("MZ2015AIS"))) {
   dat$arv_bio_m <- as.integer(set_na(dat$sbioarv_m, 9)) 
 } else  { dat$arv_bio_m<-NA }
 
 
 # Create self-reported/bio variable for men 
 if(all(dat$SurveyId %in% c("MZ2015AIS")))
 { dat$arv_self_m=apply(dat[, c("curr","ever")],1, function(y) { ifelse(any(y== 0, na.rm = TRUE), 0, 
                                                                       ifelse (all(is.na(y)), NA, 1 ))})      
 dat$arv_m=apply(dat[, c("arv_self_m","arv_bio_m")],1, function(y) { ifelse(any(y== 1, na.rm = TRUE), "Yes",  
                                                                       ifelse (all(is.na(y)), NA, "No" ))})  
 
 
 } else { dat$arv_m<- NA
 dat$arv_self_m<- NA}
 
 
 # ARV  intake:  self reported  for women 
 if(all(dat$SurveyId %in% c("MZ2015AIS"))) {
   dat$everw<- as.integer(set_na(dat$s718, 8:9)) ## ever taking ARVs for mozambique
   dat$currw<- as.integer(set_na(dat$s719, 9)) 
 } 
 else  {  dat$currw <-NA
 dat$everw <- NA
 }
 
 
 # ARV  intake:  biomarker-based  for women 
 if(all(dat$SurveyId %in% c("MZ2015AIS"))) {
   dat$arv_bio_w <- as.integer(set_na(dat$sbioarv_w, 9)) 
 } else  { dat$arv_bio_w<-NA }
 
 
 #  create self-reported/bio variable for women 
 if(all(dat$SurveyId %in% c("MZ2015AIS")))
 { dat$arv_self_w=apply(dat[, c("currw","everw")],1, function(y) { ifelse(any(y== 0, na.rm = TRUE), 0,
                                                                          ifelse (all(is.na(y)), NA, 1 ))})    
 dat$arv_w=apply(dat[, c("arv_self_w","arv_bio_w")],1, function(y) { ifelse(any(y== 1, na.rm = TRUE), "Yes",  
                                                                           ifelse (all(is.na(y)), NA, "No" ))})  
 
 
 } else { dat$arv_w<- NA
 dat$arv_self_w<- NA}
 

 ## Viral load - male
 if(all(dat$SurveyId %in% c("MZ2015AIS")))  {
   for(i in 1:nrow(dat)) {
     if (dat$sviral_m[i] %in% c(0:999)) {
       dat$vlsup_m[i] <- 1 }
     else if (dat$sviral_m[i] %in% c(9999999, 999994, 999993, 999992, 999991)) {
       dat$vlsup_m[i] <- NA }
     else if (dat$sviral_m[i] %in% c(3574851)) {
       dat$vlsup_m[i] <- 0 }
     else if (dat$sviral_m[i] %in% c(1000:999990)){
       dat$vlsup_m[i] <- 0}
     else if (dat$sviral_m[i] %in% c(NA)){
       dat$vlsup_m[i] <- NA }
   } 
 } else { dat$vlsup_m<- NA }
 
 
 dat$vlsup_m <- as.integer(dat$vlsup_m)
 
 
 ## Viral load - female
 if(all(dat$SurveyId %in% c("MZ2015AIS"))) {
   for(i in 1:nrow(dat)) {
     if (dat$sviral_w[i] %in% c(0:999)) {
       dat$vlsup_w[i] <- 1 }
     else if (dat$sviral_w[i] %in% c(9999999, 999994, 999993, 999992, 999991)) { 
       dat$vlsup_w[i] <- NA }
     else if (dat$sviral_w[i] %in% c(3574851)) { 
       dat$vlsup_w[i] <- 0 }
     else if (dat$sviral_w[i] %in% c(1000:999990)){
       dat$vlsup_w[i] <- 0}
     else if (dat$sviral_w[i] %in% c(NA)){
       dat$vlsup_w[i] <- NA }
   } 
 } else { dat$vlsup_w<- NA }
 
 
 dat$vlsup_w <- as.integer(dat$vlsup_w)
 
 
 # current marriage status so as to exclude widowed for certain surveys. 
 dat$wdw<-factor(set_na(dat$v501, 9))
 
 if(all(dat$SurveyId %in% c("CM2004DHS", "KE2003DHS", "MW2004DHS", 
                        "RW2005DHS", "ZW2005DHS", "LB2007DHS",
                        "ML2006DHS", "NG2008DHS"))) { 
   dat<-dat[which(dat$DVSel %in% 1 & dat$evermarried %in% 1 & dat$wdw %ni% 3), ]
 } 
 else { dat<-dat[which(dat$DVSel %in% 1 & dat$evermarried %in% 1), ] 
 }
 
 
 
 
 dat<-dat[ !all(is.na(dat$dvweight)), ]
 dat<-dat[ dat$SurveyId %ni% c("MZ2011DHS"), ] #Remove mozambique since only one individual responded to the DV module and was also in the dataet 
 
 

 dat <- dat[c( "country", "household", "linem", "linew", "surveyid", "survyear", "psu", "psu_u", "period", "stratum",  "stratum_dv", "region", "indweight_w", "hivweight_w", "hivweight_m",  "dvweight",
              "agem", "agew", "agegrw", "agegrm", "agedisp", "schoolw", "schoolm", "employmentm", "employmentw", "wealthw",  "wealthm",  "restype",  "age1s_w",  "age1s_m", 
              "go_outw",  "neglectw", "arguew", "ref_sexw", 'burnw',  'justify_w',  "go_outm", "neglectm", 'arguem', 'ref_sexm', 'burnm', 'justify_m',  'poly',
              "earn_disp", 'spend', "health", "purchase", "visit", 'decision',  'hivstatus_w', "hivstatus_m", 'conLast_w', 'conLast_m',   'ever_phys',
              "ever_sex_vio",  'any_viol',  'recent_viol', 'pay', 'pay_rec',   'part_alcoholFRQ', 'evermarried', 
              'life_w', 'life_m', 'partners_rec_w', 'partners_rec_m',  'DVSel',  "head", 'vlsup_m', 'vlsup_w', "arv_m","arv_w",  'conc_m', 'conc_w'
            )]
 
 return(dat)
}



datrcoup <- lapply(datlstc, recode_dhs_c) 
datcoup<-do.call("rbind", datrcoup)

datcoup <- datcoup%>% dplyr:: mutate(unique_ID = group_indices(., psu_u, household, linew)) ## Add a unique ID to each variable 

saveRDS(datcoup, "DHS_perp_recode_july2_clean.rds")



