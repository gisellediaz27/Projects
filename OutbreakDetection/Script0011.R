## Food Outbreak
## Proyecto por Giselle Diaz

library(reshape2)

getwd()
## En este proyecto blabla
rm(list = ls())

# Importamos el data set
dataset <- read.csv("Projects/Datasets/outbreak.csv")

# Reemplazamos los valores faltantes por NA
dataset[dataset == "NA"] <- NA

# Eliminamos las filas donde falten valores en las hospitalizaciones y muertes ya que es informacion critica
dataset <- dataset[!(is.na(dataset$Hospitalizations) | is.na(dataset$Deaths) | dataset$Hospitalizations == "" | dataset$Deaths == ""), ]
outbreak_na <- read.csv("Projects/Datasets/OutbreakNA.csv")

# Asignamos la variable de unknown parar el resto de las variables sin informacion
dataset$Location.of.Preparation <- ifelse(is.na(dataset$Location.of.Preparation) | dataset$Location.of.Preparation == "", "Unknown", dataset$Location.of.Preparation)
dataset$Genus.Species[dataset$Genus.Species == ""] <- "Unknown"
dataset$Serotype.or.Genotype[dataset$Serotype.or.Genotype == ""] <- "Unknown"
dataset$Etiology.Status[dataset$Etiology.Status == ""] <- "Suspected"
dataset$Food.Vehicle[dataset$Food.Vehicle == ""] <- "Unknown"
dataset$Contaminated.Ingredient[dataset$Contaminated.Ingredient == ""] <- "Unknown"


write.csv(dataset, file = "Projects/Datasets/OutbreakNA.csv", row.names = FALSE)
outbreak_na <- read.csv("Projects/Datasets/OutbreakNA.csv")

# verificamos que no haya valores sin definir
str(outbreak_na)
cat("Number of missing values in the cleaned dataset:\n")
sapply(outbreak_na, function(x) sum(x == ""))
head(outbreak_na)

#validamos el formato de las columnas en nuestras varia bles
validate_columns <- function(data) {
  numeric_columns <- c("Month", "Year", "Illnesses", "Hospitalizations", "Deaths")
  character_columns <- c("State", "Genus.Species", "Serotype.or.Genotype", "Etiology.Status", "Location.of.Preparation", "Food.Vehicle", "Contaminated.Ingredient")
  cat("Numeric Columns Validation:\n", sapply(data[, numeric_columns], is.numeric), "\n\n")
  cat("Character Columns Validation:\n", sapply(data[, character_columns], is.character), "\n")
}

validate_columns(outbreak_na)


outbreak_na$Grouped.Pathogen <- outbreak_na$Genus.Species
library(dplyr)

# Group similar pathogens using dplyr
outbreak_na <- outbreak_na %>%
  mutate(Grouped.Pathogen = case_when(
    Genus.Species %in% c("Escherichia coli, Shiga toxin-producing", 
                         "Escherichia coli, Shiga toxin-producing; Escherichia coli, Shiga toxin-producing", 
                         "Escherichia coli, Enteropathogenic", 
                         "Escherichia coli, Enteroaggregative", 
                         "Escherichia coli, Enterotoxigenic", 
                         "Escherichia coli, Other",
                         "Escherichia coli, Shiga toxin-producing; Escherichia coli, Shiga toxin-producing; Escherichia coli, Shiga toxin-producing") ~ "Escherichia coli",
    Genus.Species %in% c("Norovirus Genogroup II", 
                         "Norovirus", "Norovirus other",
                         "Norovirus Genogroup I", 
                         "Norovirus unknown", 
                         "Norovirus Genogroup II; Norovirus Genogroup II", 
                         "Norovirus; Other", 
                         "Norovirus unknown; Norovirus Genogroup II", 
                         "Norovirus; Norovirus Genogroup II", 
                         "Norovirus Genogroup II; Norovirus",
                         "Norovirus Genogroup IV; Norovirus Genogroup II", 
                         "Norovirus; Norovirus Genogroup II", 
                         "Norovirus Genogroup II; Norovirus", 
                         "Norovirus Genogroup II; Norovirus", 
                         "Norovirus; Norovirus Genogroup II", 
                         "Norovirus Genogroup II; Norovirus", 
                         "Norovirus Genogroup II; Norovirus Genogroup II", 
                         "Norovirus Genogroup I; Norovirus Genogroup II", 
                         "Norovirus Genogroup II; Norovirus", 
                         "Norovirus; Norovirus Genogroup II") ~ "Norovirus",
    Genus.Species %in% c("Salmonella enterica", 
                         "Salmonella enterica; Salmonella enterica", 
                         "Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica", 
                         "Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica", 
                         "Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica","Salmonella", 
                         "Salmonella other", 
                         "Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica" ,
                         "Salmonella unknown",
                         "Salmonella enterica; Salmonella enterica; Salmonella enterica", 
                         "Salmonella enterica; Salmonella unknown") ~ "Salmonella",
    Genus.Species %in% c("Clostridium perfringens", 
                         "Clostridium other", 
                         "Clostridium botulinum",
                         "Clostridium perfringens; Clostridium perfringens"
    ) ~ "Clostridium",
    Genus.Species %in% c("Staphylococcus aureus", 
                         "Staphylococcus other") ~ "Staphylococcus",
    Genus.Species %in% c("Bacillus cereus", 
                         "Bacillus unknown", "Bacillus cereus; Bacillus other", "Bacillus",
                         "Bacillus cereus; Clostridium perfringens", 
                         "Bacillus; Clostridium perfringens", 
                         "Bacillus; Clostridium perfringens; Other - Bacterium", 
                         "Bacillus other",
                         "Bacillus cereus; Clostridium perfringens; Other - Bacterium") ~ "Bacillus",
    Genus.Species %in% c("Campylobacter jejuni", 
                         "Campylobacter unknown", 
                         "Campylobacter coli", 
                         "Campylobacter" ,"Campylobacter jejuni; Campylobacter coli","Campylobacter other",   
                         "Campylobacter coli; Campylobacter jejuni","Campylobacter unknown; Campylobacter jejuni", "Campylobacter jejuni; Campylobacter unknown", "Campylobacter fetus"                                                                                                                                                                                           
    ) ~ "Campylobacter",
    Genus.Species %in% c("Hepatitis A") ~ "Hepatitis A",
    Genus.Species %in% c("Shigella boydii","Shigella boydii; Shigella sonnei"  , "Shigella",
                         "Shigella dysenteriae", "Shigella unknown","Shigella flexneri","Shigella sonnei") ~ "Shigella",
    Genus.Species %in% c("Histamine", 
                         "Mycotoxins", 
                         "Pesticides", 
                         "Rotavirus", 
                         "Paralytic shellfish poison") ~ "Contaminants",
    Genus.Species %in% c("Vibrio parahaemolyticus; Vibrio other","Vibrio vulnificus",
                         "Vibrio unknown","Vibrio cholerae",
                         "Vibrio other","Vibrio parahaemolyticus") ~ "Vibrio",
    Genus.Species %in% c("Yersinia enterocolitica", "Yersinia") ~ "Yersinia",
    Genus.Species %in% c("Trichinella unknown", "Trichinella spiralis", "Trichinella") ~ "Trichinella",
    Genus.Species %in% c("Streptococcus Group A") ~ "Streptococcus",
    Genus.Species %in% c("Staphylococcus unknown", "Staphylococcus") ~ "Staphylococcus",
    Genus.Species %in% c("Scombroid toxin") ~ "Scombroid toxin",
    Genus.Species %in% c("Sapovirus unknown", "Sapovirus Genogroup I", "Sapovirus") ~ "Sapovirus",
    Genus.Species %in% c("Salmonella enterica", "Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica; Salmonella enterica") ~ "Salmonella",
    Genus.Species %in% c("Puffer fish tetrodotoxin", "Plant/Herbal toxins", "Other - Chemical/Toxin", "Other - Parasite", "Other - Virus", "Other - Bacterium", "Other") ~ "Other",
    Genus.Species %in% c("Norovirus Genogroup II; Norovirus Genogroup I", 
                         "Norovirus Genogroup II; Norovirus Genogroup II; Norovirus Genogroup II; Norovirus other; Neurotoxic shellfish poison; Monosodium glutamate (MSG)") ~ "Norovirus",
    Genus.Species %in% c("Listeria monocytogenes", "Listeria unknown") ~ "Listeria",
    Genus.Species %in% c("Giardia intestinalis") ~ "Giardia",
    Genus.Species %in% c("Escherichia coli, Enteroaggregative; Escherichia coli, Enteropathogenic") ~ "Escherichia coli",
    Genus.Species %in% c("Enterococcus faecalis") ~ "Enterococcus",
    Genus.Species %in% c("Cyclospora cayatenensis", "Cyclospora") ~ "Cyclospora",
    Genus.Species %in% c("Cryptosporidium unknown", "Cryptosporidium parvum", "Cryptosporidium parvum; Cryptosporidium unknown","Cryptosporidium", "Cryptosporidium hominis; Cryptosporidium parvum; Cryptosporidium") ~ "Cryptosporidium",
    Genus.Species %in% c("Cleaning agents") ~ "Cleaning agents",
    Genus.Species %in% c("Ciguatoxin") ~ "Ciguatoxin",
   Genus.Species %in% c("Brucella unknown") ~ "Brucella",
    Genus.Species %in% c("Astrovirus") ~ "Astrovirus",
    Genus.Species %in% c("Anisakis") ~ "Anisakis",
   Genus.Species %in% c("Unknown") ~ "Unknown",
   Genus.Species %in% c("Amnesic shellfish poison","Neurotoxic shellfish poison") ~ "Shellfish poison",
   Genus.Species %in% c("Heavy metals") ~ "Heavy metals",
   Genus.Species %in% c("Monosodium glutamate (MSG)") ~ "Monosodium glutamate (MSG)",
    TRUE ~ "Mixed"  # Assign "Mixed" when a pathogen falls into multiple categories
  )) %>%
  # Reorder columns
  select(Year, Month, State, Grouped.Pathogen, Genus.Species,  everything())

outbreak_na$Grouped.Pathogen <- as.character(outbreak_na$Grouped.Pathogen)

# Check unique values in Grouped.Pathogen
unique_values <- unique(outbreak_na$Grouped.Pathogen)

# Print the unique values
print(unique_values)


unique_values <- unique(outbreak_na$Location.of.Preparation)

# Print the unique values
print(unique_values)






###
# outbreak_na
library(ggplot2)

# For Deaths by State (Top 20)
outbreak_na %>%
  filter(State != "Multistate") %>%
  group_by(State) %>%
  summarise(TotalDeaths = sum(Deaths)) %>%
  arrange(desc(TotalDeaths)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(State, TotalDeaths), y = TotalDeaths, fill = State)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = TotalDeaths), vjust = -0.5, color = "black", size = 3) +  # Add this line for text labels
  labs(title = "Top 10 States by Total Deaths (Excluding Multistate)", x = "State", y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# For Top 5 Food Vehicles (Excluding Unknown)
outbreak_na %>%
  filter(Food.Vehicle != "Unknown") %>%
  group_by(Food.Vehicle) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(5) %>%
  ggplot(aes(x = reorder(Food.Vehicle, Count), y = Count, fill = Food.Vehicle)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 3) +  # Add this line for text labels
  labs(title = "Top 5 Food Vehicles (Excluding Unknown)", x = "Food Vehicle", y = "Number of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

