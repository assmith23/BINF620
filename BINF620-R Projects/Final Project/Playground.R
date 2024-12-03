rawData <- read.csv("Final Project/NSCH_2022e_DRC.csv")

summary(rawData$SC_AGE_YEARS)

cols <- colnames(rawData)

columns_to_keep <- c(
  "SC_AGE_YEARS", "PLACESLIVED", "HHCOUNT", "FAMCOUNT", "K2Q40A", 
  "DIABETES", "K2Q42A", "HEADACHE", "HEADACHE_CURR", "K2Q33A", 
  "K2Q33B", "K2Q32A", "K2Q32B", "DOWNSYN", "FASD", "K2Q37A", 
  "K2Q35A", "K2Q31A", "K2Q31B", "K2Q31D", "ADDTREAT", "ENGAGE_FAST", 
  "ENGAGE_INTEREST", "ENGAGE_PICKY", "ENGAGE_BINGE", "ENGAGE_PURG", 
  "ENGAGE_PILLS", "ENGAGE_EXERCISE", "ENGAGE_NOEAT", "CUTHOURS", 
  "BORNUSA", "K8Q35", "EMOSUPSPO", "EMOSUPFAM", "EMOSUPSHCP", 
  "EMOSUPWOR", "EMOSUPFADV", "EMOSUPPEER", "EMOSUMHP", "EMOSUPOTH", 
  "ACE3", "ACE4", "ACE5", "ACE6", "ACE7", "ACE8", "ACE9", "ACE10", 
  "ACE12", "ACE11", "K10Q40_R", "K7Q02R_R", "K7Q04R_R", "PHYSACTIV", 
  "HOURSLEEP05", "HOURSLEEP", "OUTDOORSWKDAY", "OUTDOORSWKEND", 
  "SCREENTIME", "GRADES", "SC_ENGLISH", "FPL_I1", "FPL_I2", "FPL_I3", 
  "FPL_I4", "FPL_I5", "FPL_I6", "FWC", "HEIGHT", "WEIGHT", 
  "INQ_RESSEG", "INQ_EDU", "INQ_EMPLOY", "INQ_INCOME", "INQ_HOME", 
  "hrsareg", "age3_22", "age5_22", "sex_22", "race4_22", "raceASIA_22", 
  "race7_22", "PrntNativity_22", "HHLanguage_22", "hisplang_22", 
  "famstruct5_22", "povlev4_22", "povSCHIP_22", "AdultEduc_22", 
  "SugarDrink_22", "anxiety_22", "depress_22", "behavior_22", 
  "DevDelay_22", "ADHD_22", "ADHDSev_22", "MotherMH_22", "FatherMH_22", 
  "MotherHSt_22", "FatherHSt_22", "ScreenTime_22", "ACEdivorce_22", 
  "ACEdeath_22", "ACEjail_22", "ACEdomviol_22", "ACEneighviol_22", 
  "ACEmhealth_22", "ACEdrug_22", "ACEdiscrim_22", "ACESexDiscrim_22", 
  "ACEHealthDiscrim_22", "ACEct11_22", "PlacesLived_22"
)

# Create a new data frame with only the specified columns
filteredData <- rawData[, columns_to_keep]