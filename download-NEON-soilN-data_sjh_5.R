##############################################################
### CODE FOR DOWNLOADING SOIL N DATA FROM NEON DATA PORTAL ###
### S. Weintraub, Feb 2020                                 ###
##############################################################

#modified by Steven Hall, Aug 2021, to produce common inorganic N dataset with covariates for group discussion / use

# Clean up workspace
rm(list = ls())

# If you don't already have this package, install it - otherwise can skip
# install.packages("neonUtilities")

# Load packages
library(neonUtilities)
library(data.table)
# install package (might need to download devtools if you don't have it)
# library(devtools) 
# install_github("NEONScience/NEON-Nitrogen-Transformations/neonNTrans", dependencies=FALSE)  
library(neonNTrans) 
library(reshape)

# ## OPTION 1: DOWNLOAD DATA AND STORE IN FILE ON COMPUTER (only do this once, takes a long time!)

# # Download all data for a given product, then unzip and stack by table
# # setwd("/Users/sweintraub/Downloads") # change to desired location
# getwd() # make sure this works
# 
# #define token:
# 
# TOKEN_SJH<-"eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJzdGV2ZW5qaEBpYXN0YXRlLmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTc4NTE2MDY5MCwiaWF0IjoxNjI3NDgwNjkwLCJlbWFpbCI6InN0ZXZlbmpoQGlhc3RhdGUuZWR1In0.lnuPKQxO_MaBGoahJHRahtgLL31tAjsik_5yExTD_O4RvPXX4IX0nHGw0gYVzqomL1VKiv9_ZYumRwKLvjvjpg"
# 
# 
# # Example for Soil physical and chemical properties, periodic (DP1.10086.001)
# # Change DPID and filepath to download a different product
# # Note - delete older versions with same folder name, otherwise this won't work
# # Can choose 'basic' or 'expanded' for package type; sites = "all" or name specific ones
# 
  # zipsByProduct(
  #   dpID = "DP1.10086.001",
  #   site = "all",
  #   package = "basic",
  #   check.size = F,
  #   token=TOKEN_SJH)

  # stackByTable(
  #   filepath = paste0(getwd(), "/filesToStack10086"),
  #   folder = T,
  #   saveUnzippedFiles = F
  # )

# #micro biomass (PLFA)
# zipsByProduct(
#       dpID = "DP1.10104.001",
#       site = "all",
#       package = "basic",
#       check.size = F,
#       token=TOKEN_SJH)
# 
#     stackByTable(
#       filepath = paste0(getwd(), "/filesToStack10104"),
#       folder = T,
#       saveUnzippedFiles = F
#     )




##############################
## Load downloaded data into R 
##############################
  ntr_internalLabBlanks <- readTableNEON(
    dataFile='./filesToStack10086/stackedFiles/ntr_internalLabBlanks.csv',
    varFile='./filesToStack10086/stackedFiles/variables_10086.csv'
  )
  
  ntr_internalLab <- readTableNEON(
    dataFile='./filesToStack10086/stackedFiles/ntr_internalLab.csv',
    varFile='./filesToStack10086/stackedFiles/variables_10086.csv'
  )

    ntr_externalLab <- readTableNEON(
      dataFile='./filesToStack10086/stackedFiles/ntr_externalLab.csv',
      varFile='./filesToStack10086/stackedFiles/variables_10086.csv'
    )

    #here I use a moisture table with gapfilling for missing values, implemented in separate script
    sls_soilMoisture <- readTableNEON(
      dataFile='sls_soilMoisture_new.csv',
      varFile='./filesToStack10086/stackedFiles/variables_10086.csv'
    )  
    head(sls_soilMoisture)
    #bring in additional file with sample collection attribute data
    sls_soilCoreCollection<-readTableNEON(
      dataFile="./filesToStack10086/stackedFiles/sls_soilCoreCollection.csv",
      varFile='./filesToStack10086/stackedFiles/variables_10086.csv'
    )  
      
    #bring in other pH data for these samples
    sls_soilpH<-readTableNEON(
      dataFile="./filesToStack10086/stackedFiles/sls_soilpH.csv",
      varFile='./filesToStack10086/stackedFiles/variables_10086.csv'
    )  
  
    #soil Chem
    sls_soilChemistry<-readTableNEON(
      dataFile="./filesToStack10086/stackedFiles/sls_soilChemistry.csv",
      varFile='./filesToStack10086/stackedFiles/variables_10086.csv'
    )  
    
    #microbes
    sme_microbialBiomass<-readTableNEON(
      dataFile="./filesToStack10104/stackedFiles/sme_microbialBiomass.csv",
      varFile='./filesToStack10104/stackedFiles/variables_10104.csv'
    )  
    head(sme_microbialBiomass)
    
    #spreadsheet from SanClements paper;
    #converted "SanClements_Bioscience_siteSummaries.xlsx" to .csv
    
    siteSummaries<-read.csv("SanClements_Bioscience_siteSummaries.csv",header=TRUE)
    
    #bring in sample geographic data downloaded from geoNEON based on the named location list of the samples used here (did this separately, data are in .csv)
    
    IN_sample_geo<-read.csv("IN_sample_geo.csv",header=TRUE)
    
    
#make named list of inorganic N datasets for compatibility with Sam's function below
    soilData<-list(ntr_internalLabBlanks=ntr_internalLabBlanks,
                   ntr_internalLab=ntr_internalLab,
                   ntr_externalLab=ntr_externalLab,
                   sls_soilMoisture=sls_soilMoisture)



#first, make single dataframe with ancillary variables that we will later join with the N rate dataframe. We will subset some of these dataframes to remove columns that are either not very useful or that cause problems with merging, leading to duplicate names

#first subset sls_soilCoreCollection for the most useful columns 
sls_soilCoreCollection_red<-sls_soilCoreCollection[c(
  "domainID",
  "siteID",
  "plotID",
  "sampleID",
  "namedLocation",
  "plotType",
  "nlcdClass",
  "coreCoordinateX",
  "coreCoordinateY",
  "decimalLatitude",
  "decimalLongitude",
  "elevation",
  "sampleTiming",
  "standingWaterDepth",
  "horizon",
  "soilTemp",
  "sampleTopDepth",
  "sampleBottomDepth"
)]

#now merge with the site-level attributes (SanClements), to get MAT, MAP, some general plant/soil descriptive data
intersect(names(sls_soilCoreCollection_red),names(siteSummaries))

coredata_sitedata<-merge(sls_soilCoreCollection_red,siteSummaries, all.x=TRUE)

length(sls_soilCoreCollection_red$sampleID) #double-check that all sampleIDs are retained at each step
length(coredata_sitedata$sampleID)

#now merge with pH data, after making subsetted pH data frame (keep the most useful columns)
soilpH<-sls_soilpH[,c("sampleID",
                      "soilInWaterpH",
                      "soilInCaClpH")]

intersect(names(coredata_sitedata),names(soilpH))

coredata_sitedata_pH<-merge(coredata_sitedata, soilpH, all.x=TRUE)

length(coredata_sitedata_pH$sampleID)
length(coredata_sitedata$sampleID)

#merge with soil chem data
#make subsetted data frame with useful vars
sls_soilChemistry_red<-sls_soilChemistry[,c(
  "sampleID",
  "acidTreatment",
  "d15N",
  "organicd13C",
  "nitrogenPercent",
  "organicCPercent",
  "CNratio"
)]
#need to take means of analytical reps
sls_soilChemistry_red_melt<-melt(sls_soilChemistry_red, id.vars=1:2)
sls_soilChemistry_red_cast<-cast(sls_soilChemistry_red_melt,sampleID~variable, mean,na.rm=TRUE)
head(sls_soilChemistry_red_cast)
#redefine C:N ratio for samples with acid treatment
sls_soilChemistry_red_cast$CNratio<-sls_soilChemistry_red_cast$organicCPercent/sls_soilChemistry_red_cast$nitrogenPercent

# write.csv(sls_soilChemistry_red_cast,"sls_soilChemistry_red_cast.csv",row.names=FALSE)

coredata_sitedata_pH_chem<-merge(coredata_sitedata_pH, sls_soilChemistry_red_cast, all.x=TRUE)

length(coredata_sitedata_pH$sampleID)
length(coredata_sitedata_pH_chem$sampleID)


#merge with microbial biomass
#subset first; double-check to verify that the correct columns were retained
sme_microbialBiomass_red<-sme_microbialBiomass[,c(15,23:81)]

head(sme_microbialBiomass_red)
#find duplicate sample values
sme_microbialBiomass_red$sampleID[duplicated(sme_microbialBiomass_red$sampleID)]
#34 duplicated samples. 
#take means of analytical reps
sme_microbialBiomass_red_melt<-melt(sme_microbialBiomass_red, id.vars=1)
sme_microbialBiomass_cast<-cast(sme_microbialBiomass_red_melt,sampleID~variable, mean,na.rm=TRUE)
# write.csv(sme_microbialBiomass_cast,"sme_microbialBiomass_cast.csv", row.names=FALSE)
# 

coredata_sitedata_pH_chem_mic<-merge(coredata_sitedata_pH_chem, sme_microbialBiomass_cast, all.x=TRUE)       
length(coredata_sitedata_pH_chem_mic$sampleID)
length(coredata_sitedata_pH_chem$sampleID)

#merge with the geoNEON data; first make a subsetted DF with the useful columns
head(IN_sample_geo)
# unique(IN_sample_geo$locationType)
# unique(IN_sample_geo$plotDimensions)
# unique(IN_sample_geo$subtype)
# unique(IN_sample_geo$plotType)
#many of these columns are present in the existing DF
sample_geo_red<-IN_sample_geo[,c(
  "namedLocation",
  "easting",
  "northing",
  "utmZone",
  "maximumElevation",
  "minimumElevation",
  "plotType",
  "soilTypeOrder",
  "slopeAspect",
  "slopeGradient"
)]


coredata_sitedata_pH_chem_mic_geo<-merge(coredata_sitedata_pH_chem_mic, sample_geo_red, all.x=TRUE)

length(coredata_sitedata_pH_chem_mic_geo$namedLocation)
length(sample_geo_red$namedLocation)




###############################################
## CODE FOR turning inorganic N data into rates
###############################################
# See the user guide as well as this GitHub page: 
# https://github.com/NEONScience/NEON-Nitrogen-Transformations/tree/master/neonNTrans


# run function - this way assumes you loaded data directly to R (option 2 above) 
    #(or, that you manually loaded the relevant files and put them into a named list, as done above)
# see function documentation for more about the parameters/options [type ?def.calc.ntrans()]
    

####################################################
# we will prepare four different datasets: 
###############################################

#1) use Sam's function with default drop flags for NH4 and NO3; this is conservative with respect to removing lots of values that are slightly negative, but liberal insofar as samples with high blanks remain if the blank-corrected value is positive

# 2) Use no drop flags (all negative values go to zero); very liberal approach as all data are retained

# 3) Drop all samples with blanks that exceed some threshold; maybe 0.13 for NO3 and 0.4 for NH4. Samples with negative blank-corrected values are then retained (corrected to zero), regardless of how negative they are. This is quite conservative.

# 4) Correct for the systematic trend of increasingly negative blank-corrected values with increasing blank nitrate+nitrite concentration, under the assumption that a large fraction of the blank nitrate+nitrite was nitrite that was chemodenitrified in the soil extractions. This assumption is supported by a strong linear relationship between the lower quantiles of blank-corrected sample nitrate+nitrate and blank nitrate+nitrite, determined using quantile regression. For NH4, we will use a more liberal threshold for discarding negative values (-0.06), acknowledging  idiosyncratic analytical error

#for each dataset, I am reformatting the data to retain the NH4 and NO3 measurements of T0 (the field N availablity) as well as the net N mineralization/nitrification rates. The pools of inorganic N may be as useful as the fluxes.

#for each dataset, I am also manually recalculating the net N mineralization/nitrification rates after running Sam's function. The reason is that after applying the blank correction to the nitrate values for the 4th dataset, I had to recalculate the nitrate pools and net rates. To simplify downstream analyses, I then did this same calculation for the other datasets as well

#the files for subsequent analysis are Ntrans_etc_v1, Ntrans_etc_v2, Ntrans_etc_v3, Ntrans_etc_v4. Their columns are identical

#################################################################
# Dataset 1
#################################################################

ncalc_v1 <-
  def.calc.ntrans(
    kclInt = soilData$ntr_internalLab,
    kclIntBlank = soilData$ntr_internalLabBlanks,
    kclExt = soilData$ntr_externalLab,
    soilMoist = soilData$sls_soilMoisture,
    dropAmmoniumFlags = "blanks exceed sample value",
    dropNitrateFlags = "blanks exceed sample value"
  )

all_data_v1<-ncalc_v1$all_data
# write.csv(all_data_v1,"all_data_v1.csv",row.names=FALSE)
sum(!is.na(all_data_v1$soilInorganicNugPerGram))

head(all_data_v1)
#drop NA rows first
all_data_v1<-as.data.frame(all_data_v1[!is.na(all_data_v1$sampleID),])

#recalculate the net N rates manually. Convert long to wide based on the incubationPairIDs
#check for duplicate values for incubationPairID
all_data_v1$ID_check<-paste(all_data_v1$incubationPairID,all_data_v1$nTransBoutType)
all_data_v1$ID_check[duplicated(all_data_v1$ID_check)]
#remove second instance of duplicates
all_data_v1<-all_data_v1[!duplicated(all_data_v1$ID_check),]
head(all_data_v1)

#recalculate nitrate mass
all_data_v1$soilNitrateNitriteNugPerGram_rev<-all_data_v1$kclNitrateNitriteNBlankCor*all_data_v1$kclVolume/all_data_v1$soilDryMass

# #check calculation of ammonium mass
# all_data_v1$soilAmmoniumNugPerGram_check<-all_data_v1$kclAmmoniumNBlankCor*all_data_v1$kclVolume/all_data_v1$soilDryMass
# plot(soilAmmoniumNugPerGram_check~soilAmmoniumNugPerGram, all_data_v1)
# abline(0,1)

#subset the dataframe
all_data_v1_trans<-all_data_v1[,c("namedLocation",
                                  "incubationPairID",
                                  "nTransBoutType",
                                  "incubationLength",
                                  "kclAmmoniumNBlankCor",
                                  "kclNitrateNitriteNBlankCor",
                                  "soilAmmoniumNugPerGram",
                                  "soilNitrateNitriteNugPerGram_rev",
                                  "soilNitrateNitriteNugPerGram")]
all_data_v1_trans_melt<-melt(all_data_v1_trans,id.vars=1:3)
head(all_data_v1_trans_melt)

all_data_v1_trans_cast<-cast(all_data_v1_trans_melt, namedLocation+ incubationPairID ~ nTransBoutType+variable)

head(all_data_v1_trans_cast)

all_data_v1_trans_cast$netNminugPerGramPerDay<-((all_data_v1_trans_cast$tFinal_soilAmmoniumNugPerGram+all_data_v1_trans_cast$tFinal_soilNitrateNitriteNugPerGram_rev)-(all_data_v1_trans_cast$tInitial_soilAmmoniumNugPerGram+all_data_v1_trans_cast$tInitial_soilNitrateNitriteNugPerGram_rev))/all_data_v1_trans_cast$tFinal_incubationLength

all_data_v1_trans_cast$netNitugPerGramPerDay<-(all_data_v1_trans_cast$tFinal_soilNitrateNitriteNugPerGram_rev-all_data_v1_trans_cast$tInitial_soilNitrateNitriteNugPerGram_rev)/all_data_v1_trans_cast$tFinal_incubationLength

#merge with reduced version of all_data_v1 to get important covariates
#subset to remove some less useful columns
all_data_v1_red<-as.data.frame(all_data_v1[,c(
  "plotID",
  "sampleID",
  "incubationPairID",
  "collectDate",
  "soilMoisture",
  "dryMassFraction"
)])
intersect(names(all_data_v1_red),names(all_data_v1_trans_cast) )

#use data.table merge so that we keep only the attributes from the T0 sample
setDT(all_data_v1_trans_cast); setDT(all_data_v1_red) #

all_data_v1_trans_cast1<-as.data.frame(all_data_v1_red[all_data_v1_trans_cast, mult="first", on="incubationPairID",nomatch=0L])

# all_data_v1_trans_cast1<-merge(all_data_v1_trans_cast,all_data_v1_red )
head(all_data_v1_trans_cast1)


#then, merge with covariate data frame made above

Ntrans_etc_v1<-merge(all_data_v1_trans_cast1,coredata_sitedata_pH_chem_mic_geo,all.x=TRUE )
length(Ntrans_etc_v1$sampleID)
sum(!is.na(Ntrans_etc_v1$netNminugPerGramPerDay))
sum(!is.na(all_data_v1_trans_cast$netNminugPerGramPerDay))


write.csv(Ntrans_etc_v1,"Ntrans_etc_v1.csv",row.names=FALSE)


#################################################################
# Dataset 2
#################################################################


ncalc_v2 <-
  def.calc.ntrans(
    kclInt = soilData$ntr_internalLab,
    kclIntBlank = soilData$ntr_internalLabBlanks,
    kclExt = soilData$ntr_externalLab,
    soilMoist = soilData$sls_soilMoisture
  )

all_data_v2<-ncalc_v2$all_data
# write.csv(all_data_v2,"all_data_v2.csv",row.names=FALSE)
sum(!is.na(all_data_v2$soilInorganicNugPerGram))

head(all_data_v2)
#drop NA rows first
all_data_v2<-as.data.frame(all_data_v2[!is.na(all_data_v2$sampleID),])

#recalculate the net N rates manually. Convert long to wide based on the incubationPairIDs
#check for duplicate values for incubationPairID
all_data_v2$ID_check<-paste(all_data_v2$incubationPairID,all_data_v2$nTransBoutType)
all_data_v2$ID_check[duplicated(all_data_v2$ID_check)]
#remove second instance of duplicates
all_data_v2<-all_data_v2[!duplicated(all_data_v2$ID_check),]
head(all_data_v2)

#recalculate nitrate mass
all_data_v2$soilNitrateNitriteNugPerGram_rev<-all_data_v2$kclNitrateNitriteNBlankCor*all_data_v2$kclVolume/all_data_v2$soilDryMass

# #check calculation of ammonium mass
# all_data_v2$soilAmmoniumNugPerGram_check<-all_data_v2$kclAmmoniumNBlankCor*all_data_v2$kclVolume/all_data_v2$soilDryMass
# plot(soilAmmoniumNugPerGram_check~soilAmmoniumNugPerGram, all_data_v2)
# abline(0,1)

#subset the dataframe
all_data_v2_trans<-all_data_v2[,c("namedLocation",
                                  "incubationPairID",
                                  "nTransBoutType",
                                  "incubationLength",
                                  "kclAmmoniumNBlankCor",
                                  "kclNitrateNitriteNBlankCor",
                                  "soilAmmoniumNugPerGram",
                                  "soilNitrateNitriteNugPerGram_rev",
                                  "soilNitrateNitriteNugPerGram")]
all_data_v2_trans_melt<-melt(all_data_v2_trans,id.vars=1:3)
head(all_data_v2_trans_melt)

all_data_v2_trans_cast<-cast(all_data_v2_trans_melt, namedLocation+ incubationPairID ~ nTransBoutType+variable)

head(all_data_v2_trans_cast)

all_data_v2_trans_cast$netNminugPerGramPerDay<-((all_data_v2_trans_cast$tFinal_soilAmmoniumNugPerGram+all_data_v2_trans_cast$tFinal_soilNitrateNitriteNugPerGram_rev)-(all_data_v2_trans_cast$tInitial_soilAmmoniumNugPerGram+all_data_v2_trans_cast$tInitial_soilNitrateNitriteNugPerGram_rev))/all_data_v2_trans_cast$tFinal_incubationLength

all_data_v2_trans_cast$netNitugPerGramPerDay<-(all_data_v2_trans_cast$tFinal_soilNitrateNitriteNugPerGram_rev-all_data_v2_trans_cast$tInitial_soilNitrateNitriteNugPerGram_rev)/all_data_v2_trans_cast$tFinal_incubationLength

#merge with reduced version of all_data_v2 to get important covariates
#subset to remove some less useful columns
all_data_v2_red<-as.data.frame(all_data_v2[,c(
  "plotID",
  "sampleID",
  "incubationPairID",
  "collectDate",
  "soilMoisture",
  "dryMassFraction"
)])
intersect(names(all_data_v2_red),names(all_data_v2_trans_cast) )

#use data.table merge so that we keep only the attributes from the T0 sample
setDT(all_data_v2_trans_cast); setDT(all_data_v2_red) #

all_data_v2_trans_cast1<-as.data.frame(all_data_v2_red[all_data_v2_trans_cast, mult="first", on="incubationPairID",nomatch=0L])


# all_data_v2_trans_cast2<-merge(all_data_v2_trans_cast,all_data_v2_red )
head(all_data_v2_trans_cast1)

#then, merge with covariate data frame made above

Ntrans_etc_v2<-merge(all_data_v2_trans_cast1,coredata_sitedata_pH_chem_mic_geo,all.x=TRUE )
length(Ntrans_etc_v2$sampleID)
sum(!is.na(Ntrans_etc_v2$netNminugPerGramPerDay))

write.csv(Ntrans_etc_v2,"Ntrans_etc_v2.csv",row.names=FALSE)





#################################################################
# Dataset 3
#################################################################

#begin with the liberal Dataset 2 (no values thrown out).
#then, discard any samples with  with NH4 blank > 0.1 mgl, NO3 blank > 0.13 mgl

all_data_v3<- all_data_v2[all_data_v2$blankNH4mean< 0.1 & all_data_v2$blankNO3mean< 0.13,] 
  length(all_data_v3$sampleID)

  all_data_v3<-all_data_v3[!is.na(all_data_v3$sampleID),]
  
  # write.csv(all_data_v3,"all_data_v3.csv",row.names=FALSE)
  sum(!is.na(all_data_v3$soilInorganicNugPerGram))
  
  head(all_data_v3)
  #drop NA rows first
  all_data_v3<-as.data.frame(all_data_v3[!is.na(all_data_v3$sampleID),])
  
  #recalculate the net N rates manually. Convert long to wide based on the incubationPairIDs
  #check for duplicate values for incubationPairID
  all_data_v3$ID_check<-paste(all_data_v3$incubationPairID,all_data_v3$nTransBoutType)
  all_data_v3$ID_check[duplicated(all_data_v3$ID_check)]
  #remove second instance of duplicates
  all_data_v3<-all_data_v3[!duplicated(all_data_v3$ID_check),]
  head(all_data_v3)
  
  #recalculate nitrate mass
  all_data_v3$soilNitrateNitriteNugPerGram_rev<-all_data_v3$kclNitrateNitriteNBlankCor*all_data_v3$kclVolume/all_data_v3$soilDryMass
  
  # #check calculation of ammonium mass
  # all_data_v3$soilAmmoniumNugPerGram_check<-all_data_v3$kclAmmoniumNBlankCor*all_data_v3$kclVolume/all_data_v3$soilDryMass
  # plot(soilAmmoniumNugPerGram_check~soilAmmoniumNugPerGram, all_data_v3)
  # abline(0,1)
  
  #subset the dataframe
  all_data_v3_trans<-all_data_v3[,c("namedLocation",
                                    "incubationPairID",
                                    "nTransBoutType",
                                    "incubationLength",
                                    "kclAmmoniumNBlankCor",
                                    "kclNitrateNitriteNBlankCor",
                                    "soilAmmoniumNugPerGram",
                                    "soilNitrateNitriteNugPerGram_rev",
                                    "soilNitrateNitriteNugPerGram")]
  all_data_v3_trans_melt<-melt(all_data_v3_trans,id.vars=1:3)
  head(all_data_v3_trans_melt)
  
  all_data_v3_trans_cast<-cast(all_data_v3_trans_melt, namedLocation+ incubationPairID ~ nTransBoutType+variable)
  
  head(all_data_v3_trans_cast)
  
  all_data_v3_trans_cast$netNminugPerGramPerDay<-((all_data_v3_trans_cast$tFinal_soilAmmoniumNugPerGram+all_data_v3_trans_cast$tFinal_soilNitrateNitriteNugPerGram_rev)-(all_data_v3_trans_cast$tInitial_soilAmmoniumNugPerGram+all_data_v3_trans_cast$tInitial_soilNitrateNitriteNugPerGram_rev))/all_data_v3_trans_cast$tFinal_incubationLength
  
  all_data_v3_trans_cast$netNitugPerGramPerDay<-(all_data_v3_trans_cast$tFinal_soilNitrateNitriteNugPerGram_rev-all_data_v3_trans_cast$tInitial_soilNitrateNitriteNugPerGram_rev)/all_data_v3_trans_cast$tFinal_incubationLength
  
  #merge with reduced version of all_data_v3 to get important covariates
  #subset to remove some less useful columns
  all_data_v3_red<-as.data.frame(all_data_v3[,c(
    "plotID",
    "sampleID",
    "incubationPairID",
    "collectDate",
    "soilMoisture",
    "dryMassFraction"
  )])
  intersect(names(all_data_v3_red),names(all_data_v3_trans_cast) )
  
  #use data.table merge so that we keep only the attributes from the T0 sample
  setDT(all_data_v3_trans_cast); setDT(all_data_v3_red) #
  
  all_data_v3_trans_cast1<-as.data.frame(all_data_v3_red[all_data_v3_trans_cast, mult="first", on="incubationPairID",nomatch=0L])
  
  
  # all_data_v3_trans_cast1<-merge(all_data_v3_trans_cast,all_data_v3_red )
  head(all_data_v3_trans_cast1)
  
  #then, merge with covariate data frame made above
  
  Ntrans_etc_v3<-merge(all_data_v3_trans_cast1,coredata_sitedata_pH_chem_mic_geo,all.x=TRUE )
  length(Ntrans_etc_v3$sampleID)
  sum(!is.na(Ntrans_etc_v3$netNminugPerGramPerDay))
  
  write.csv(Ntrans_etc_v3,"Ntrans_etc_v3.csv",row.names=FALSE)
  
  
  
  
  
  
  #################################################################
  # Dataset 4
  #################################################################
  #begin with the liberal Dataset 2 (no values thrown out).
  ## explore distribution of negative blank-corrected values
  
  all_data_v4<-ncalc_v2$all_data
  #drop NA rows first
  all_data_v4<-as.data.frame(all_data_v4[!is.na(all_data_v4$sampleID),])
  
  #how do blank-corrected sample NH4 vary with blank NH4? (note truncated y axis)
  plot((kclAmmoniumNConc-blankNH4mean)~blankNH4mean, all_data_v4, col=rgb(0,0,0,alpha=0.1),
       ylim=c(-0.5,1)) #note that the negative blank-corrected NH4 do not  always occur at higher blank NH4
  plot((kclAmmoniumNConc-blankNH4mean)~blankNH4mean, all_data_v4, col=rgb(0,0,0,alpha=0.3),
       ylim=c(-0.25,-0.025)
  ) 
  
  #drop values with blank-corrected NH4 < -0.06
 low_NH4_v4<-all_data_v4[which(all_data_v4$kclAmmoniumNConc-all_data_v4$blankNH4mean< -0.06),]
  
  all_data_v4<-all_data_v4[which(all_data_v4$kclAmmoniumNConc-all_data_v4$blankNH4mean>= -0.06),]
  
  plot((kclNitrateNitriteNConc-blankNO3mean)~blankNO3mean, all_data_v4, col=rgb(0,0,0,alpha=0.1),
       ylim=c(-0.5,1)) #note that the negative blank-corrected NH4 do not  always occur at higher blank NH4
  plot((kclNitrateNitriteNConc-blankNO3mean)~blankNO3mean, all_data_v4, col=rgb(0,0,0,alpha=0.3),
       ylim=c(-0.25,0)
  ) 
  #the blank-corrected values show a strong negative linear trend with increasing blank NO3. Quantile regression allows us to characterize the edge of that relationship. This is consistent with the idea that blanks are mainly nitrite which is chemodenitrified in the sample. So we could use quantile regression to correct these values by adding back an amount of nitrate that depends on the measured blank value.
  library(quantreg)
  mod1<-rq((kclNitrateNitriteNConc-blankNO3mean)~blankNO3mean, data=all_data_v4,
           tau=0.01) # tau is the quantile chosen for the regression
  abline(mod1)
  mod2<-rq((kclNitrateNitriteNConc-blankNO3mean)~blankNO3mean, data=all_data_v4,
           tau=0)
  abline(mod2)
  mod3<-rq((kclNitrateNitriteNConc-blankNO3mean)~blankNO3mean, data=all_data_v4,
           tau=0.025)
  abline(mod3)
  #chose the 1% quantile because it seems a reasonable lower bound, and intercept is very close to zero
  
  # add 75% of the blank value back to the blank-corrected nitrate N value (based on the slope of the quantile regression at the 0.01 quantile)
  
  #define new variable, 
  all_data_v4$kclNitrateNitriteNBlankCor_real<-all_data_v4$kclNitrateNitriteNConc-all_data_v4$blankNO3mean#real blank corrected value, negative values allowed
  hist(all_data_v4$kclNitrateNitriteNBlankCor_real)
  #now add back 75% of the blank
  all_data_v4$kclNitrateNitriteNBlankCor_rev<-all_data_v4$kclNitrateNitriteNBlankCor_real+0.75*all_data_v4$blankNO3mean
  plot(kclNitrateNitriteNBlankCor_rev~kclNitrateNitriteNBlankCor_real, all_data_v4,
       xlim=c(-0.1,0.5),ylim=c(-0.1,0.5),col=rgb(0,0,0,alpha=0.1))
  abline(a=0,b=1)
  plot(kclNitrateNitriteNBlankCor_rev~blankNO3mean, all_data_v4,
       xlim=c(-0.1,0.5),ylim=c(-0.1,0.5),col=rgb(0,0,0,alpha=0.1))
  hist(all_data_v4$kclNitrateNitriteNBlankCor_rev[all_data_v4$kclNitrateNitriteNBlankCor_rev<0.1])
  #now all of the blank-corrected values are > -0.02...
  #assign those negative values to 0.
  all_data_v4$kclNitrateNitriteNBlankCor_rev[all_data_v4$kclNitrateNitriteNBlankCor_rev<0]<-0
  
  #recalculate the net N rates manually. Convert long to wide based on the incubationPairIDs
  #check for duplicate values for incubationPairID
  all_data_v4$ID_check<-paste(all_data_v4$incubationPairID,all_data_v4$nTransBoutType)
  all_data_v4$ID_check[duplicated(all_data_v4$ID_check)]
  #remove second instance of duplicates
  all_data_v4<-all_data_v4[!duplicated(all_data_v4$ID_check),]
  all_data_v4<-as.data.frame(all_data_v4)
  head(all_data_v4)
  
  #recalculate nitrate mass
  all_data_v4$soilNitrateNitriteNugPerGram_rev<-all_data_v4$kclNitrateNitriteNBlankCor_rev*all_data_v4$kclVolume/all_data_v4$soilDryMass
  
  # #check calcualtion of ammonium mass
  # all_data_v4$soilAmmoniumNugPerGram_check<-all_data_v4$kclAmmoniumNBlankCor*all_data_v4$kclVolume/all_data_v4$soilDryMass
  # plot(soilAmmoniumNugPerGram_check~soilAmmoniumNugPerGram, all_data_v4)
  # abline(0,1)
  
  all_data_v4_trans<-all_data_v4[,c("namedLocation",
                                    "incubationPairID",
                                    "nTransBoutType",
                                    "incubationLength",
                                    "kclAmmoniumNBlankCor",
                                    "kclNitrateNitriteNBlankCor_rev",
                                    "soilAmmoniumNugPerGram",
                                    "soilNitrateNitriteNugPerGram_rev")]
  all_data_v4_trans_melt<-melt(all_data_v4_trans,id.vars=1:3)
  head(all_data_v4_trans_melt)
  
  all_data_v4_trans_cast<-cast(all_data_v4_trans_melt, namedLocation+ incubationPairID ~ nTransBoutType+variable)
  
  head(all_data_v4_trans_cast)
  
  all_data_v4_trans_cast$netNminugPerGramPerDay<-((all_data_v4_trans_cast$tFinal_soilAmmoniumNugPerGram+all_data_v4_trans_cast$tFinal_soilNitrateNitriteNugPerGram_rev)-(all_data_v4_trans_cast$tInitial_soilAmmoniumNugPerGram+all_data_v4_trans_cast$tInitial_soilNitrateNitriteNugPerGram_rev))/all_data_v4_trans_cast$tFinal_incubationLength
  
  all_data_v4_trans_cast$netNitugPerGramPerDay<-(all_data_v4_trans_cast$tFinal_soilNitrateNitriteNugPerGram_rev-all_data_v4_trans_cast$tInitial_soilNitrateNitriteNugPerGram_rev)/all_data_v4_trans_cast$tFinal_incubationLength
  
  
  #merge with reduced version of all_data_v3 to get important covariates
  #subset to remove some less useful columns
  all_data_v4_red<-as.data.frame(all_data_v4[,c(
    "plotID",
    "sampleID",
    "incubationPairID",
    "collectDate",
    "soilMoisture",
    "dryMassFraction"
  )])
  intersect(names(all_data_v4_red),names(all_data_v4_trans_cast) )
  
  #use data.table merge so that we keep only the attributes from the T0 sample
  setDT(all_data_v4_trans_cast); setDT(all_data_v4_red) #
  
  all_data_v4_trans_cast1<-as.data.frame(all_data_v4_red[all_data_v4_trans_cast, mult="first", on="incubationPairID",nomatch=0L])
  
  
  # all_data_v4_trans_cast1<-merge(all_data_v4_trans_cast,all_data_v4_red )
  head(all_data_v4_trans_cast1)
  

#then, merge with covariate data frame made above

Ntrans_etc_v4<-merge(all_data_v4_trans_cast1,coredata_sitedata_pH_chem_mic_geo,all.x=TRUE )
length(Ntrans_etc_v4$sampleID)
sum(!is.na(Ntrans_etc_v4$netNminugPerGramPerDay))

write.csv(Ntrans_etc_v4,"Ntrans_etc_v4.csv",row.names=FALSE)

