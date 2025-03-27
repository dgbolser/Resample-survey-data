#######################################################################################################################################
#### resample survey data: all data
####
#######################################################################################################################################

####clear environment
rm(list=ls())

####set wds
wd = "C:/Users/Derek.Bolser/Documents/Resample_survey_data"
data = "C:/Users/Derek.Bolser/Documents/Resample_survey_data/code/Results"

arrowtooth <- file.path(data, "Arrowtooth_flounder")
bocaccio <- file.path(data, "Bocaccio")
canary <- file.path(data, "Canary_rockfish")
darkblotched <- file.path(data, "Darkblotched_rockfish")
dover <- file.path(data, "Dover_sole")
lingcod_n <- file.path(data, "Lingcod_north")
lingcod_s <- file.path(data, "Lingcod_south")
longnose <- file.path(data, "Longnose_skate")
pop <- file.path(data, "Pacific_ocean_perch")
dogfish <- file.path(data, "Pacific_spiny_dogfish")
petrale <- file.path(data, "Petrale_sole")
rex <- file.path(data, "Rex_sole")
sablefish <- file.path(data, "Sablefish")
shortspine <- file.path(data, "Shortspine_thornyhead")
widow <- file.path(data, "Widow_rockfish")
yellowtail <- file.path(data, "Yellowtail_rockfish")
figures <- file.path(wd, "Figures")

####load packages
library(tidyverse)

##### read in data #########################################################################################################################
#arrowtooth
setwd(arrowtooth)
arrowtooth_df<- read.csv("arrowtooth_indices_df.csv")
arrowtooth_df$species<- "Arrowtooth flounder"

#bocaccio
setwd(bocaccio)
bocaccio_df<- read.csv("bocaccio_indices_df.csv")
bocaccio_df$species<- "Bocaccio"

#canary
setwd(canary)
canary_df<- read.csv("canary_indices_df.csv")
canary_df$species<- "Canary rockfish"

#darkblotched
setwd(darkblotched)
darkblotched_df<- read.csv("darkblotched_indices_df.csv")
darkblotched_df$species<- "Darkblotched rockfish"

#dover
setwd(dover)
dover_df<- read.csv("dover_indices_df.csv")
dover_df$species<- "Dover sole"

#lingcod_n
setwd(lingcod_n)
lingcod_n_df<- read.csv("lingcod_n_indices_df.csv")
lingcod_n_df$species<- "Lingcod (North)"

#lingcod_s
setwd(lingcod_s)
lingcod_s_df<- read.csv("lingcod_s_indices_df.csv")
lingcod_s_df$species<- "Lingcod (South)"

#longnose
setwd(longnose)
longnose_df<- read.csv("longnose_indices_df.csv")
longnose_df$species<- "Longnose skate"

#pop
setwd(pop)
pop_df<- read.csv("pop_indices_df.csv")
pop_df$species<- "Pacific ocean perch"

#dogfish
setwd(dogfish)
dogfish_df<- read.csv("dogfish_indices_df.csv")
dogfish_df$species<- "Pacific spiny dogfish"

#petrale
setwd(petrale)
petrale_df<- read.csv("petrale_indices_df.csv")
petrale_df$species<- "Petrale sole"

#rex
setwd(rex)
rex_df<- read.csv("rex_indices_df.csv")
rex_df$species<- "Rex sole"

#sablefish
setwd(sablefish)
sablefish_df<- read.csv("sablefish_indices_df.csv")
sablefish_df$species<- "Sablefish"

#shortspine
setwd(shortspine)
shortspine_df<- read.csv("shortspine_indices_df.csv")
shortspine_df$species<- "Shortspine rockfish"

#widow
setwd(widow)
widow_df<- read.csv("widow_indices_df.csv")
widow_df$species<- "Widow rockfish"

#yellowtail
setwd(yellowtail)
yellowtail_df<- read.csv("yellowtail_indices_df.csv")
yellowtail_df$species<- "Yellowtail rockfish"

####plot results ################################################################################################################
all_indices<-rbind(arrowtooth_df,bocaccio_df,canary_df,darkblotched_df,dogfish_df,dover_df,lingcod_n_df,lingcod_s_df, longnose_df, petrale_df, pop_df,
                   rex_df, sablefish_df, shortspine_df, shortspine_df, widow_df, yellowtail_df)


#log biomass estimates
ggplot(all_indices, aes(x = as.factor(effort), y = log_est)) +
  geom_boxplot() +
  facet_wrap(~ species) +
  labs(x = "Proprotion of effort",
       y = "Log biomass estimate") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'all_indices_boxplot_log_biomass.png',plot = last_plot() , path = figures, width = 8, height = 8, device = 'png', dpi = 300)

#log(?) SE
ggplot(all_indices, aes(x = as.factor(effort), y = se)) +
  geom_boxplot() +
  facet_wrap(~ species) +
  labs(x = "Proprotion of effort",
       y = "Standard error of log biomass estimate") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'all_indices_boxplot_log_biomass_SE.png',plot = last_plot() , path = figures, width = 8, height = 8, device = 'png', dpi = 300)

#biomass estimates
ggplot(all_indices, aes(x = as.factor(effort), y = est)) +
  geom_boxplot() +
  facet_wrap(~ species) +
  labs(x = "Proprotion of effort",
       y = "Biomass estimate") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'all_indices_boxplot_biomass.png',plot = last_plot() , path = figures, width = 8, height = 8, device = 'png', dpi = 300)