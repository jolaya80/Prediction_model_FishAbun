library(dplyr)  # For data manipulation and joining
library(tidyverse)
library(vegan)
library(kableExtra)
library(tidyr) # to pivot tables
library(ggplot2)

############### FLORIDA ################

# design data frames for analysis


###### PERCENTAGE OF BENTHIC COVER
# From the benthic cover database, the percentage of cover for the 
# different substrate cover categories calculated for each primary sampling unit

# substrate cover data base year 2022
# from BenthicCover data frames NCRMP
Benthic_DRTO <- read.csv("Datos_Florida/NCRMP_DRTO2022_Benthic_Data01_BenthicCover.csv")
Benthic_fkey <- read.csv("Datos_Florida/NCRMP_FKEYS2022_Benthic_Data01_BenthicCover.csv")
Benthic_SEFCRI <- read.csv("Datos_Florida/NCRMP_SEFCRI2022_Benthic_Data01_BenthicCover.csv")

## combine dataframes and select desired columns
# Check if column names are identical
if (all(sapply(list(Benthic_DRTO, Benthic_fkey, Benthic_SEFCRI), colnames) == colnames(Benthic_DRTO))) {
  # Combine the data frames
  combined_df <- rbind(Benthic_DRTO, Benthic_fkey, Benthic_SEFCRI)
  head(combined_df)
} else {
  # Handle potential column name mismatches or different data types
  # ... (e.g., use `dplyr::bind_rows()` for more flexibility)
}

#### sum the number of points with substrate cover into a single value per category
#### and put it in a new columns
combined_df <- combined_df %>%
  mutate(Cover = HARDBOTTOM_P + SOFTBOTTOM_P + RUBBLE_P)

## Select the desired columns
Benthic_florida <- combined_df %>%
  group_by(REGION,
           SUB_REGION_NR,
           PRIMARY_SAMPLE_UNIT,
           YEAR,
           HABITAT_CD,
           MEAN_RUG,
           LAT_DEGREES,
           LON_DEGREES,
           PROT,
           MIN_DEPTH,
           MAX_DEPTH,
           CATEGORY) %>%
  summarize(
    per_cover = sum(Cover)
  )

## transform the CATEGORY column into individual columns and using the corresponding per_cover values as the cell values.
# Pivot the data
Benthic_florida <- Benthic_florida %>%
  pivot_wider(
    names_from = CATEGORY,
    values_from = per_cover,
    values_fill = 0
  )

Benthic_florida <- Benthic_florida %>%
  rename(Bare_Substrate = `Bare Substrate`)

# Convert columns to factors
columns_to_factor <- c("REGION", "SUB_REGION_NR", "PRIMARY_SAMPLE_UNIT", "HABITAT_CD", "PROT")
Benthic_florida[, columns_to_factor] <- lapply(Benthic_florida[, columns_to_factor], as.factor)

## make a boxplot to visualize tendencies
df_long <- pivot_longer(Benthic_florida, cols = c(Bare_Substrate, CORAL, MACROALGAE, SPONGES, Gorgonians, Millepora, Palythoa, CYANOBACTERIA, CCA),
                        names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = SUB_REGION_NR, y = value, fill = REGION)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = NULL, y = "Percentage cover") +
  theme_minimal()

ggplot(df_long, aes(x = value, y = variable)) +
  geom_boxplot() +
  labs(x = "Percentage cover", y = NULL) +
  theme_minimal()

### because some Primary sample units ID are repeated among Regions, we concatenate the two columns to make a unique ID for each PSU
Benthic_florida <- Benthic_florida %>% 
  unite("PSU", REGION, PRIMARY_SAMPLE_UNIT, sep = "_")

#### CORAL DIVERSITY AND RICHNESS BY PSU
### From database Benthic_Data02_CoralDemographies make estimations of 
### richness, diversity

Demographics_DRTO <- read.csv("Datos_Florida/NCRMP_DRTO2022_Benthic_Data02_CoralDemographics.csv")
Demographics_FKEYS <- read.csv("Datos_Florida/NCRMP_FKEYS2022_Benthic_Data02_CoralDemographics.csv")
Demographics_SEFCRI <- read.csv("Datos_Florida/NCRMP_SEFCRI2022_Benthic_Data02_CoralDemographics.csv")

# make a list with the dataframes
lista_dataframes <- c("Demographics_DRTO", "Demographics_FKEYS", "Demographics_SEFCRI")

# Agrupar los datos por especie y realizar los cálculos
Demografic_Florida <- lapply(lista_dataframes, function(x) {
  get(x) %>%
    group_by(REGION,
             PRIMARY_SAMPLE_UNIT, 
             SPECIES_CD) %>%
    summarise(
      total_colonias = sum(N)
      )
}) %>%
  bind_rows()

### beacuse some Primary sample units ID are repeted among Regions, we concatenate the two columns to make a unique ID for each PSU
Demografic_Florida <- Demografic_Florida %>% 
  unite("PSU", REGION, PRIMARY_SAMPLE_UNIT, sep = "_")

library(vegan) ### biodiversity calculations

# columns "PRIMARY_SAMPLE_UNIT" and "SPECIES_CD" as factors
Demografic_Florida$PSU <- as.factor(Demografic_Florida$PSU)
Demografic_Florida$SPECIES_CD <- as.factor(Demografic_Florida$SPECIES_CD)

# create an abundance matrix
matriz_abundance <- reshape2::dcast(Demografic_Florida, 
                                     PSU ~ SPECIES_CD, 
                                     value.var = "total_colonias", 
                                     fun.aggregate = sum)

# Convert the data to a matrix
abundance_matrix <- as.matrix(matriz_abundance[, 2:ncol(matriz_abundance)])

# calculate richness
riqueza <- specnumber(abundance_matrix)
# Calculate Shannon index by PSU
diversidad_shannon <- diversity(abundance_matrix, index = "shannon")
# Calculate evenness (equitability, Pielou’s) by PSU
diversidad_evenness <- diversidad_shannon/log(riqueza)

# create a dataframe with the results
results <- data.frame(PSU = unique(Demografic_Florida$PSU),
                         Coral_Richness = riqueza,
                         Coral_Shannon = diversidad_shannon,
                         Coral_evenness = diversidad_evenness)

###
### merge "results" with "Benthic_florida" to obtain the final dataframe with all variables for each PSU
###
Coral_variables <- left_join(Benthic_florida, 
                         results, 
                         by = "PSU")

summary(Coral_variables)

#### INVERTEBRATES
# from Benthic_Data03_InvertebratesESAcorals data frames NCRMP
Invertebrates_DRTO <- read.csv("Datos_Florida/NCRMP_DRTO2022_Benthic_Data03_InvertebratesESAcorals.csv")
Invertebrates_fkey <- read.csv("Datos_Florida/NCRMP_FKEYS2022_Benthic_Data03_InvertebratesESAcorals.csv")
Invertebrates_SEFCRI <- read.csv("Datos_Florida/NCRMP_SEFCRI2022_Benthic_Data03_InvertebrateESAcorals.csv")

# make a list with the dataframes
lista_inver_dataframes <- c("Invertebrates_DRTO", "Invertebrates_DRTO", "Invertebrates_SEFCRI")

# Agrupar los datos por especie y realizar los cálculos
Invertebrates_Florida <- lapply(lista_inver_dataframes, function(x) {
  get(x) %>%
    group_by(REGION,
             PRIMARY_SAMPLE_UNIT) %>%
    summarise(
      lobster = sum(LOBSTER_NUM),
      conch = sum(CONCH_NUM),
      diadema = sum(DIADEMA_NUM)
    )
}) %>%
  bind_rows()

### beacuse some Primary sample units ID are repeted among Regions, we concatenate the two columns to make a unique ID for each PSU
Invertebrates_Florida <- Invertebrates_Florida %>% 
  unite("PSU", REGION, PRIMARY_SAMPLE_UNIT, sep = "_")

###
Coral_variables_all <- left_join(Coral_variables, 
                                 Invertebrates_Florida, 
                             by = "PSU")

#####################################################
######### FISH DATA
#####################################################
## 
## load and merge the three dataframes from the NCRMP
## 
### From database Benthic_Data02_CoralDemographies make estimations of 
### richness, diversity

Fish_DRTO <- read.csv("Datos_Florida/NCRMP_DRTO2022_Fish_Data01_AnalysisReady.csv")
Fish_DRTO$REGION <- gsub("DRY TORT", "Tortugas", Fish_DRTO$REGION)
## select the desired columns to use in the analysis
Fish_DRTO <- Fish_DRTO %>%
  select(REGION, 
         PRIMARY_SAMPLE_UNIT, 
         YEAR, LAT_DEGREES,	
         LON_DEGREES, 
         SUBREGION_NR, 
         SPECIES_CD,
         LEN,
         NUM
         )

Fish_FKEYS <- read.csv("Datos_Florida/NCRMP_FKEYS2022_Fish_Data01_AnalysisReady.csv")
Fish_FKEYS$REGION <- gsub("FLA KEYS", "FLK", Fish_FKEYS$REGION)
Fish_FKEYS <- Fish_FKEYS %>%
  select(REGION, 
         PRIMARY_SAMPLE_UNIT, 
         YEAR, LAT_DEGREES,	
         LON_DEGREES, 
         SUBREGION_NR, 
         SPECIES_CD,
         LEN,
         NUM
  )

Fish_SEFCRI <- read.csv("Datos_Florida/NCRMP_SEFCRI2022_Fish_Data01_AnalysisReady.csv")
Fish_SEFCRI <- Fish_SEFCRI %>%
  select(REGION, 
         PRIMARY_SAMPLE_UNIT, 
         YEAR, LAT_DEGREES,	
         LON_DEGREES, 
         SUBREGION_NR, 
         SPECIES_CD,
         LEN,
         NUM
  )


# make a list with the dataframes
lista_Fish_dataframes <- c("Fish_DRTO", "Fish_DRTO", "Fish_SEFCRI")

# group data by species
Fish_Florida <- lapply(lista_Fish_dataframes, function(x) {
  get(x) %>%
    group_by(REGION,
             PRIMARY_SAMPLE_UNIT,
             YEAR,
             LAT_DEGREES,
             LON_DEGREES,
             SUBREGION_NR,
             SPECIES_CD,
             LEN) %>%
    summarise(
      num_fish = sum(NUM)
    )
}) %>%
  bind_rows()

### beacuse some Primary sample units ID could repeted between Regions, and to standarize with coral benthic dataframe, we concatenate the two columns to make a unique ID for each PSU
Fish_Florida <- Fish_Florida %>% 
  unite("PSU", REGION, PRIMARY_SAMPLE_UNIT, sep = "_")

Fish_taxonomic <- read.csv("Datos_Florida/Fish_taxonomic.csv")

# Merge the dataframes based on the 'SPECIES_CD' column
merged_fish <- left_join(Fish_Florida, 
                         Fish_taxonomic, 
                         by = "SPECIES_CD")

## upload a and b parameters to estimate biomass from length
## data obtained from fishbase
require(rfishbase)
## Get length-weight parameters from fishbase needed to estimate weight
Fish_sp <- Fish_taxonomic$SCINAME
lw = length_weight(Fish_sp)
write_csv(lw, "lw.csv")

## Fishbase provide several values a and b for each specie coming from different studies and localitites
## Select a single value for a and b per species (manual action)
## Choose the closest values to Florida (or the analysis location) when possible
## Input the database after selecting a and b
lw_FL <- read.csv("Datos_Florida/lw_FL.csv")

lw_FL <- lw_FL %>%
  select(Species, Sex, a, b)

# merge data.frames to combine all aresults and have a unique data frame
Fish_Biomass <- merge(merged_fish, 
                     lw_FL, 
                     by.x = "SCINAME", 
                     by.y = "Species" )


################################################################
############ ATENCION !!! ######################################
################################################################
# Calculate individual weight
# TENER EN CUENTA QUE LEN DEBE MULTIPLICARSE POR EL total length to fork length conversion factor
Fish_Biomass$weight <- Fish_Biomass$a * Fish_Biomass$LEN^Fish_Biomass$b

# Calculate total biomass for each size category
Fish_Biomass$total_biomass <- Fish_Biomass$weight * Fish_Biomass$num_fish

# Summations are then used to calculate the total biomass of a species, group, or family. 
# SUMATION AQUI

# These summations are then normalized by dividing by the transect area and multiplying by 100 to produce biomass in grams per 100m2
# NORMALIZAR AQUI




## MAKE ANALYSIS (MODEL) BY FAMILY, because our survey as fishers by family of fin fish
### group the data by family
Fish_Biomass_family <- Fish_Biomass %>%
  group_by(PSU,
           YEAR,
           LAT_DEGREES,
           LON_DEGREES,
           SUBREGION_NR,
           FAMILY) %>%
  summarize(num_fish_total = sum(num_fish),
            total_biomass = sum(total_biomass),
            .groups = 'keep')


#########################################################
##### merge coral variables and fish biomass
# Unir los dataframes por la columna "PSU", incluyendo todas las filas
Variables_model <- full_join(Fish_Biomass_family, 
                             Coral_variables_all, 
                             by = "PSU")

Variables_model$Coral_Richness <- as.numeric(Variables_model$Coral_Richness)
Variables_model$lobster <- as.numeric(Variables_model$lobster)
Variables_model$conch <- as.numeric(Variables_model$conch)
Variables_model$diadema <- as.numeric(Variables_model$diadema)

### select values for selected families
snappers <- Variables_model %>% 
  filter(FAMILY == "Lutjanidae") %>%
  select(MEAN_RUG,
         MIN_DEPTH,
         MAX_DEPTH,
         Bare_Substrate,
         CORAL,
         MACROALGAE,
         SPONGES,
         Gorgonians,
         Millepora,
         Palythoa,
         CYANOBACTERIA,
         CCA,
         Coral_Richness,
         Coral_Shannon,
         Coral_evenness,
         num_fish_total, 
         total_biomass,
         lobster,
         conch,
         diadema
  )

write.csv(snappers, "Datos_Florida/snappers.csv")

groupers <- Variables_model %>% 
  filter(FAMILY == "Serranidae") %>%
  select(MEAN_RUG,
         MIN_DEPTH,
         MAX_DEPTH,
         Bare_Substrate,
         CORAL,
         MACROALGAE,
         SPONGES,
         Gorgonians,
         Millepora,
         Palythoa,
         CYANOBACTERIA,
         CCA,
         Coral_Richness,
         Coral_Shannon,
         Coral_evenness,
         num_fish_total, 
         total_biomass,
         lobster,
         conch,
         diadema
  )

write.csv(groupers, "Datos_Florida/groupers.csv")

barracuda <- Variables_model %>% 
  filter(FAMILY == "Sphyraenidae") %>%
  select(MEAN_RUG,
         MIN_DEPTH,
         MAX_DEPTH,
         Bare_Substrate,
         CORAL,
         MACROALGAE,
         SPONGES,
         Gorgonians,
         Millepora,
         Palythoa,
         CYANOBACTERIA,
         CCA,
         Coral_Richness,
         Coral_Shannon,
         Coral_evenness,
         num_fish_total, 
         total_biomass,
         lobster,
         conch,
         diadema
  )

write.csv(barracuda, "Datos_Florida/barracuda.csv")

jacks <- Variables_model %>% 
  filter(FAMILY == "Carangidae") %>%
  select(MEAN_RUG,
         MIN_DEPTH,
         MAX_DEPTH,
         Bare_Substrate,
         CORAL,
         MACROALGAE,
         SPONGES,
         Gorgonians,
         Millepora,
         Palythoa,
         CYANOBACTERIA,
         CCA,
         Coral_Richness,
         Coral_Shannon,
         Coral_evenness,
         num_fish_total, 
         total_biomass,
         lobster,
         conch,
         diadema
  )

write.csv(jacks, "Datos_Florida/jacks.csv")
