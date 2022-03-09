
# Setup -------------
rm(list=ls())

library(tidyverse)
library(ggResidpanel)
library(car)
library(MuMIn)
library(lme4)
library(sjPlot)

options(stringsAsFactors = FALSE)

# Add path for figs
# path <- "/Users/sweintraub/Box/TWGs/TerrestrialBGC/soil N paper/figs"


# Add data from google drive -------------
# static fileID
df_id <- "1coCVkdgNYkWEzXDyODnxRoy1Icrq8UDn" # v4 dataset, quantile regression to correct nitrate data
# read csv and add subplotID
df <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", df_id), header = T) %>%
  mutate(subplotID = ifelse(coreCoordinateX >= 0 & coreCoordinateX < 20 & coreCoordinateY >= 0 & coreCoordinateY < 20, "21", 
                            ifelse(coreCoordinateX >= 20 & coreCoordinateY >= 0 & coreCoordinateY < 20, "23", 
                                   ifelse(coreCoordinateX >= 0 & coreCoordinateX < 20 & coreCoordinateY >= 20, "39", "41"))))
# checks
sum(is.na(df$subplotID)) # good, all have a subplot
head(df)


# Data distributions ------------
# Somewhat right-skewed but not terrible, numbers are neg and pos
ggplot(df, aes(x = netNminugPerGramPerDay)) + 
  geom_histogram() + 
  scale_x_continuous(trans = scales::pseudo_log_trans(), 
                     breaks = c(0,1,2,5,20,40,100)) +
  xlab(expression(paste('soil ammonuim N (ug N ', g^-1, ')')))
sum(df$netNminugPerGramPerDay == 0, na.rm = T) # No values are exactly 0

####
unique(df$subplotID) #need to get unique subplotID for each plot

head(df)
df$subplotID_plotID<-paste(df$subplotID,df$plotID,sep="_")

# df$plotID_nlcdClass<-paste(df$plotID,df$nlcdClass,sep="_")

#do plots ever have non-unique nlcdClass?

test<-as.data.frame(table(df$plotID,df$nlcdClass))

test<-table(df$plotID,df$nlcdClass)
test1<-test
test1[test1!=0]<-1
rowSums(test1) #plots all have unique veg types, don't need to look for plot/veg interaction

#remove NAs for netNminugPerGramPerDay
dfred<-df[!is.na(df$netNminugPerGramPerDay),]
length(dfred$netNminugPerGramPerDay)
length(df$netNminugPerGramPerDay)
# dfred1<-dfred[!is.na(dfred$CNratio),]
# length(dfred1$netNminugPerGramPerDay)

#fit the model

mod1<-lmer(netNminugPerGramPerDay~horizon+sampleTiming+nlcdClass+(1|subplotID_plotID)+(1|plotID)+(1|siteID)+(1|domainID), data=dfred)

plot(mod1)
#strong residual pattern
#try cube root transformation?
# df$netNminugPerGramPerDay_cubert<-df$netNminugPerGramPerDay^1/3
# 
# mod2<-lmer(netNminugPerGramPerDay_cubert~horizon+sampleTiming+(1|subplotID_plotID)+(1|plotID)+
#              (1|nlcdClass), data=dfred)

# plot(mod2)
# resid_panel(mod2)

#try neglog transformation (10.1111/j.1467-9876.2005.00520.x) Whittaker and Somers 2005

dfred$netNminugPerGramPerDay_neglog<-dfred$netNminugPerGramPerDay
hist(dfred$netNminugPerGramPerDay_neglog)
#replace negative values as follows:
dfred$netNminugPerGramPerDay_neglog[dfred$netNminugPerGramPerDay_neglog<=0]<- -log(-dfred$netNminugPerGramPerDay_neglog[dfred$netNminugPerGramPerDay_neglog<=0]+1)
#replace positive values with log(x+1)
dfred$netNminugPerGramPerDay_neglog[dfred$netNminugPerGramPerDay_neglog>0]<- log(dfred$netNminugPerGramPerDay_neglog[dfred$netNminugPerGramPerDay_neglog>0]+1)

hist(dfred$netNminugPerGramPerDay)
hist(dfred$netNminugPerGramPerDay_neglog) #data are less skewed, sign difference is retained


#scale/center
dfred$netNminugPerGramPerDay_neglog_scale<-scale(dfred$netNminugPerGramPerDay_neglog)

dfred$sampleTiming<-as.character(dfred$sampleTiming)
str(dfred$sampleTiming) #defaults to character, want to define as factor. interesting, will not converge if we use "peakGreenness" as the reference level
#for easier interpretation, redefine sample timing as ramp up / ramp down/ peak

dfred$Timing<-dfred$sampleTiming
dfred$Timing[dfred$Timing=="dryWetTransition"]<-"rampUp"
dfred$Timing[dfred$Timing=="wetDryTransition"]<-"rampDown"
dfred$Timing[dfred$Timing=="fallWinterTransition"]<-"rampDown"
dfred$Timing[dfred$Timing=="winterSpringTransition"]<-"rampUp"
unique(dfred$Timing)
dfred$Timing<-factor(dfred$Timing)
levels(dfred$Timing)
# dfred$sampleTiming<-factor(dfred$sampleTiming,levels=c("peakGreenness",
#                                                       "dryWetTransition",
#                                                         "wetDryTransition",
#                                                         "fallWinterTransition",
#                                                         "winterSpringTransition" ))
#                                                        
# dfred$sampleTiming<-factor(dfred$sampleTiming,levels=c("dryWetTransition",
#                                                        "wetDryTransition",
#                                                        "fallWinterTransition",
#                                                        "winterSpringTransition",
#                                                        "peakGreenness"
# ))
# 
# #ramp up / ramp down, change factor names
# dfred$sampleTiming<-factor(dfred$sampleTiming,levels=c("dryWetTransition",
#                                                        "wetDryTransition",
#                                                        "fallWinterTransition",
#                                                        "winterSpringTransition",
#                                                        "peakGreenness"
# ))
# 
# dfred$sampleTiming<-factor(dfred$sampleTiming)
# levels(dfred$sampleTiming)

mod2<-lmer(netNminugPerGramPerDay_neglog~horizon+Timing+
  (1|nlcdClass)+(1|subplotID_plotID)+(1|plotID)+(1|siteID)+(1|domainID), 
  data=dfred)
resid_panel(mod2) # residuals are still not great. 
r.squaredGLMM(mod2)

#with horizon*timing interaction
mod2.1<-lmer(netNminugPerGramPerDay_neglog~horizon*Timing+
      (1|nlcdClass)+(1|subplotID_plotID)+(1|plotID)+(1|siteID)+(1|domainID), 
             data=dfred)
resid_panel(mod2.1) # residuals are still not great.
Anova(mod2.1)

r.squaredGLMM(mod2.1)
summary(mod2.1)

quartz(,4,4)
plot_model(mod2.1, title="", colors='black')+theme_bw()

plot_model(mod2.1, type="re")

mod2.1_tidy<-tidy(mod2.1)
mod2.1_tidy$model<-"N min"
# mod2.1_tidy$model[mod2.1_tidy$model=="fixed"]<-0
# mod2.1_tidy$model[mod2.1_tidy$model=="ran_pars"]<-1

mod2.1_tidy<-mod2.1_tidy %>% subset(mod2.1_tidy$model==0)

quartz(,4,4)
dwplot(mod2.1_tidy)%>%
  relabel_predictors(
    c("horizonO" = "O horizon",
      "TimingrampDown" = "ramp down",
      "TimingrampUp" = "ramp up",
      "horizonO:TimingrampDown" = "O horizon:ramp down",
      "horizonO:TimingrampUp" = "O horizon:ramp up")
    )+
  theme_bw()


# library(remotes)
# remotes::install_github("glmmTMB/glmmTMB",subdir="glmmTMB")
# 
# dir.create('~/.R')
# file.create('~/.R/Makevars')


unique(dfred$nlcdClass)

Anova(mod2)

summary(mod2)

mod3<-lmer(netNminugPerGramPerDay_neglog~horizon+sampleTiming+MAT_C+MAP_mm+(1|subplotID_plotID)+(1|plotID)+(1|nlcdClass)+(1|domainID), data=dfred)
resid_panel(mod3)
Anova(mod3) #strikingly, no relationship with MAT or MAP!

# mod4<-lmer(netNminugPerGramPerDay_neglog~horizon+sampleTiming+MAT_C+MAP_mm+soilMoisture+(1|subplotID_plotID)+(1|plotID)+(1|nlcdClass)+(1|domainID), data=dfred) 
# 
# mod4<-lmer(netNminugPerGramPerDay_neglog~horizon+sampleTiming+MAT_C+MAP_mm+soilMoisture+(1|subplotID_plotID)+(1|plotID)+(1|nlcdClass), data=dfred) #had to drop domainID becuase of singular fit
# resid_panel(mod4) # the residual plot looks better!
# #hist(dfred$standingWaterDepth); not a good predictor
#soilInWaterpH not a good predictor
#soilTemp not a good predictor

# #try CNratio, remove sampleTiming
# mod4<-lmer(netNminugPerGramPerDay_neglog~horizon+CNratio+(1|subplotID_plotID)+(1|plotID)+(1|nlcdClass), data=dfred) 
# #have to remove domainID because of singular fit
# 
# mod4<-lmer(netNminugPerGramPerDay_neglog~horizon+sampleTiming+MAT_C+MAP_mm+(1|subplotID_plotID)+(1|plotID)+(1|nlcdClass)+(1|domainID), data=dfred) 

# plot(netNminugPerGramPerDay_neglog~soilMoisture,dfred)
# mod3<-lmer(netNminugPerGramPerDay_neglog~horizon+sampleTiming+(1|subplotID_plotID)+(1|plotID)+(1|nlcdClass)+(1|domainID), data=dfred)
# resid_panel(mod3)
# sum(is.na(dfred$soilMoisture))

summary(mod4)
unique(dfred$sampleTiming)
head(dfred)
Anova(mod3)
Anova(mod4)

r.squaredGLMM(mod3)
r.squaredGLMM(mod4)

# sum(is.na(dfred$CNratio))
# sum(!is.na(dfred$CNratio))


#make models with reduced dataset with other biogeo vars
length(dfred$netNminugPerGramPerDay_neglog)

dfred_biogeo<-dfred[!is.na(dfred$soilInCaClpH),]
length(dfred_biogeo$netNminugPerGramPerDay_neglog)
dfred_biogeo<-dfred[!is.na(dfred_biogeo$nitrogenPercent),]
length(dfred_biogeo$netNminugPerGramPerDay_neglog)

#try model with pH, soil N
mod4<-lmer(netNminugPerGramPerDay_neglog~horizon+(1|nlcdClass)+(1|domainID)+nitrogenPercent+CNratio, data=dfred_biogeo)
resid_panel(mod4)
Anova(mod4)

r.squaredGLMM(mod4)



colSums(matrix(table(df$plotID,df$nlcdClass))!=0)
if('plotID' %in% rand_var_list_mod){
  change_me <- which(grepl('subplot',rand_var_list_mod))
  rand_var_list_mod[change_me] <- paste(rand_var_list_mod[change_me], 'plotID', sep = ':')}

if('nlcdClass' %in% rand_var_list_mod){
  change_me <- which(grepl('plotID',rand_var_list_mod)) #catches both plotID and subplotID, which should both be nested in nlcdClass
  rand_var_list_mod[change_me] <- paste(rand_var_list_mod[change_me], 'nlcdClass', sep = ':')}

# Q: Is this random slope and intercept ? -------------

if(length(rand_var_list_mod) > 0){
  form_rand <- paste0('(1|',rand_var_list_mod,')', collapse = ' + ')
}else{
  form_rand <- character(0)
}

form_right_side <- paste(c(1, form_fixed, form_rand), collapse = ' + ')

paste0(y_name, ' ~ ', form_right_side) %>% as.character() %>%  return()
}











# Function to make formulas ---------------
# Handles nesting 

get_formula <- function(
  x, 
  fixed_var_list = c("horizon",'sampleTiming'), # arguably sampleTiming should be a fixed effect
  rand_var_list = c('plotID','subplotID', 'nlcdClass'),
  y_name = 'netNminugPerGramPerDay'){
  
  df_x <- as.data.frame(x)
  
  all_vars <- c(fixed_var_list, rand_var_list)
  all_var_counts <- df_x[,names(df_x) %in% all_vars] %>% summarize_all(function(x)length(unique(x)))
  df_x %>% select(one_of(all_vars)) %>% names()
  
  fixed_var_counts <- all_var_counts[fixed_var_list]
  fixed_var_list_mod <- names(fixed_var_counts[which(fixed_var_counts > 1)])
  if(length(fixed_var_list_mod) > 0 ){
    form_fixed <- paste0(fixed_var_list_mod, collapse = ' + ')
  }else{
    form_fixed <- character(0)
  }
  
  rand_var_counts <- all_var_counts[rand_var_list]
  rand_var_list_mod <- names(rand_var_counts[which(rand_var_counts > 1)])
  
# Q: Why adding colons, nesting ? -------------
  
  if('plotID' %in% rand_var_list_mod){
    change_me <- which(grepl('subplot',rand_var_list_mod))
    rand_var_list_mod[change_me] <- paste(rand_var_list_mod[change_me], 'plotID', sep = ':')}
  
  if('nlcdClass' %in% rand_var_list_mod){
    change_me <- which(grepl('plotID',rand_var_list_mod)) #catches both plotID and subplotID, which should both be nested in nlcdClass
    rand_var_list_mod[change_me] <- paste(rand_var_list_mod[change_me], 'nlcdClass', sep = ':')}

# Q: Is this random slope and intercept ? -------------
  
  if(length(rand_var_list_mod) > 0){
    form_rand <- paste0('(1|',rand_var_list_mod,')', collapse = ' + ')
  }else{
    form_rand <- character(0)
  }
  
  form_right_side <- paste(c(1, form_fixed, form_rand), collapse = ' + ')
  
  paste0(y_name, ' ~ ', form_right_side) %>% as.character() %>%  return()
}

# Create a nested DF ------------

data_nested <- df %>% 
  group_by(siteID, domainID) %>% 
  nest()


# Get numbers for variables of interest and create formulas ----------
data_nested <- data_nested %>% 
  mutate(
    #n_year = map_int(data, ~length(unique(.$year))),
    n_sampleTiming = map_int(data, ~length(unique(.$sampleTiming))),
    n_nlcdClass = map_int(data, ~length(unique(.$nlcdClass))),
    n_samples = map_int(data, ~length(.$sampleID)),
    form = map(data, get_formula))


# Troubleshooting model ----------------

# i_rec <- 2
# data_nested$form[i_rec]
# data_nested$data[i_rec]
# 
# data_x <- data_nested$data[i_rec]
# form_x <- data_nested$form[i_rec]
# # mod <- glmmTMB(as.formula(form_x[[1]]), data = as.data.frame(data_x), ziformula = ~ 1, family = tweedie)
# # mod <- glmmTMB(as.formula(form_x[[1]]), data = as.data.frame(data_x), ziformula = ~ 1, family = poisson)
# mod <- lmer(as.formula(form_x[[1]]), data = as.data.frame(data_x))
# 
# mod_summary <- summary(mod)
# mod_summary$AICtab['deviance']


# Model fitting function ----------------
fn_fit_models <- function(data_x, form_x) {
    tryCatch(
    {
      mod <- lmer(
        formula = as.formula(form_x),
        data = as.data.frame(data_x))
      
      return(mod)
    },
    error = function(cond) NA
  )
}


# Parallel set up ---------------

#detect cores, use ~ not quite all available cores
n.cores <- 2

#initiate cluster
my_cl <- parallel::makeCluster(n.cores)

parallel::clusterEvalQ(
  my_cl,
  {
    options(stringsAsFactors = FALSE)
    library(tidyverse)
    # library(glmmTMB)
    library(lme4)
  }
)

# fit models using parallel processing
data_mods_nested <- data_nested %>%
  mutate(
    mods = parallel::clusterMap(
      cl = my_cl,
      fun = fn_fit_models,
      data_x = data,
      form_x = form))


# This doesn't work, keeping code just in case ------------

# # Do a loop instead of parallel 
# data_mods_nested <- data_nested
# for(i in 1:nrow(data_nested)){
#   tmp_result <- fn_fit_models(data_x = data_nested$data[[i]],
#                               form_x = data_nested$form[[i]])
#   tmp_result_list <- list(mod = tmp_result)
#   
#   data_mods_nested[i,'mods'] <- list(tmp_result_list)
#   
#   print(paste(i ,'/',nrow(data_nested)))
#   
# }

# look at one of the model outputs
data_mods_nested$mods[1]

# stop parallel clusters
parallel::stopCluster(my_cl)

# check out variance components for 1 model
mod <- data_mods_nested$mods[1][[1]]
lme4::VarCorr(mod) # includes residual- why does LMER have this but glmmTMB approach does not?

# get var comps
data_mods_nested <- data_mods_nested %>%
  mutate(
    vars = map(mods, function(x) {
      if (is.na(x)) {
        return(c(plotID = NA))
      } else {
        return(c(vapply(lme4::VarCorr(x), function(y) y[1, 1], pi), 
                 residual = sigma(x)^2))      }
    })
  )

# get deviance from each model, deviance will be NA if model did not converge, should probably also throw out models with negative deviance
data_mods_nested$deviance <- data_mods_nested$mods %>% sapply(
  function(mod_x){
    if(class(mod_x) == 'glmmTMB'){
      summary_x <- summary(mod_x)
      return(summary_x$AICtab['deviance'])
    }else if(class(mod_x) == 'lmerMod'){
      return(lme4::REMLcrit(mod_x))
    }else if(is.na(mod_x)){
      return(NA)
    }})

# get n_obs used in each model
data_mods_nested$n_obs <- data_mods_nested$mods %>% sapply(
  function(mod_x){
    if(class(mod_x) %in% c('glmmTMB','lmerMod')){
      nobs(mod_x)
    }else if(is.na(mod_x)){
      return(NA)
    }}) %>% unlist()

# extract variance components
data_mods_nested <- data_mods_nested %>% 
  mutate(
    vars = map(vars, ~ c(., total = sum(.))),
    comps = map(vars, ~ names(.))
  )

# un-nest data table
data_mods <- data_mods_nested %>%
  unnest(c(vars, comps))

# rename plots and subplots
data_mods$comps[grepl('subplot',data_mods$comps)] <- 'subplot'
data_mods$comps[grepl('plotID',data_mods$comps)] <- 'plot'

# calculate proportions for var comps
data_var_comps <- data_mods %>%
  select(-data, -mods) %>%
  pivot_wider(names_from = comps, 
              values_from = vars) %>%
  mutate(
    nlcdClassProp = ifelse(!is.na(nlcdClass), round(nlcdClass / total, 3), NA),
    plotProp = ifelse(!is.na(plot), round(plot / total, 3), NA),
    subplotProp = ifelse(!is.na(subplot), round(subplot / total, 3), NA),
    sampleTimingProp = ifelse(!is.na(sampleTiming), round(sampleTiming / total, 3), NA),
    residualProp = ifelse(!is.na(residual), round(residual/ total, 3), NA)
  ) %>%
  arrange(siteID) %>%
  ungroup() %>%
  dplyr::mutate_all(.funs = unlist) 



# Plot results ------------

# filter results from models that did not converge, or were otherwise problematic
var_comps <- data_var_comps %>% filter(!is.na(deviance) & deviance > 0) # lose a lot of sites with negative deviance, ask for stats help

df_plot <- var_comps %>%
  group_by(siteID) %>%
  ungroup() %>%
  gather(type, proportion, c(nlcdClass, plot, subplot, sampleTiming, residual)) %>%
  mutate(`Total Variance` = total,
         domainSite = paste(domainID, siteID, sep = "."))

df_plot <- df_plot%>%
  mutate(`Variance comp.` = type)

df_plot$`Variance comp.` <-  recode_factor(df_plot$`Variance comp.`, 
                                           residualProp = "residual",
                                           nlcdClassProp = "nlcdClass", 
                                           plotProp = 'plot',
                                           subplotProp = 'subplot',
                                           sampleTimingProp = 'sampleTiming')
levels(df_plot$`Variance comp.`)
# Fig 2 -- boxplot -- variance components by site

# ggplot
my_pal <- RColorBrewer::brewer.pal(4, name = 'Set1')
legend_color_values <- c("grey", my_pal)
legend_color_labels <- c('residual','nlcdClass','plot','subplot','sampleTiming')

my_colors <- scale_color_manual(
  values = legend_color_values,
  labels = legend_color_labels)

my_fills <- scale_fill_manual(
  values = legend_color_values,
  labels = legend_color_labels)

df_plot %>%
  ggplot(aes(domainSite, proportion, fill = `Variance comp.`)) +
  # geom_violin() +
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=.5) +
  geom_bar(position = "fill", stat = "identity") +
  coord_cartesian(ylim = c(0, 1)) +
  ylab('Proportion Variance') +
  xlab('Site ID') +
  theme_light() +
  my_colors + 
  my_fills + 
  ggtitle("Variance Partitioning Net Nit, dataset v4") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, vjust = 0.6))

ggsave(filename = "Net N Min var comps by site_v4.pdf", path = path, width = 9, height = 6)

