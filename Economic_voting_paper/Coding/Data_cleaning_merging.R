# Load necessary libraries
install.packages("janitor")

library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(purrr)

#-------------------------------------------------------------
# ADDING AVERAGE SALARY
#-------------------------------------------------------------

# Uploading data from Excel file containing average salary information
average_salary_raw <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/WYNA_2497_XTAB_20250329140152.xlsx", sheet = 2)

# Set the first row as column names
average_salary_raw <- row_to_names(average_salary_raw, row_number = 1)

# Rename first two columns for clarity
colnames(average_salary_raw)[1:2] <- c("Code", "County")

# Select the relevant years for analysis (2005, 2007, 2011, 2015, 2019, 2023)
selected_years <- c("Code", "County", "2005", "2007", "2011", "2015", "2019", "2023")

# Filter the data to include only selected years
filtered_data_salary <- average_salary_raw %>%
  select(all_of(selected_years))

# Reshape data to long format with year as a variable
panel_data_salary <- filtered_data_salary %>%
  pivot_longer(cols = -c(Code, County),   # All years go into one column
               names_to = "year",
               values_to = "average_salary") %>%
  mutate(year = as.numeric(year))  # Convert 'year' to numeric

# Convert to panel data structure
panel_data_salary <- pdata.frame(panel_data_salary, index = c("Code", "year"))


#-------------------------------------------------------------
# ADDING POPULATION SIZE
#-------------------------------------------------------------

# Uploading population size data
population_size_raw <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/LUDN_2137_XTAB_20250329142524.xlsx", sheet = 2)

# Set the second row as column names
population_size_raw <- row_to_names(population_size_raw, row_number = 2)

# Rename first two columns for clarity
colnames(population_size_raw)[1:2] <- c("Code", "County")

# Filter data to only include relevant years
filtered_data_population_size <- population_size_raw %>%
  select(all_of(selected_years))

# Reshape the data to long format
panel_data_population_size <- filtered_data_population_size %>%
  pivot_longer(cols = -c(Code, County),  
               names_to = "year",
               values_to = "population_size") %>%
  mutate(year = as.numeric(year))  # Convert 'year' to numeric

# Convert to panel data structure
panel_data_population_size <- pdata.frame(panel_data_population_size, index = c("Code", "year"))


#-------------------------------------------------------------
# ADDING UNEMPLOYMENT RATE
#-------------------------------------------------------------

# Uploading unemployment rate data
unemployment_rate_raw <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/RYNE_2392_XTAB_20250329140926Bezrobocie.xlsx", sheet = 2)

# Set the first row as column names
unemployment_rate_raw <- row_to_names(unemployment_rate_raw, row_number = 1)

# Rename first two columns for clarity
colnames(unemployment_rate_raw)[1:2] <- c("Code", "County")

# Filter data to include only relevant years
filtered_data_unemployed <- unemployment_rate_raw %>%
  select(all_of(selected_years))

# Reshape the data to long format
panel_data_unemployed <- filtered_data_unemployed %>%
  pivot_longer(cols = -c(Code, County),   
               names_to = "year",
               values_to = "unemployment_rate") %>%
  mutate(year = as.numeric(year))  # Convert 'year' to numeric

# Convert to panel data structure
panel_data_unemployed <- pdata.frame(panel_data_unemployed, index = c("Code", "year"))


#-------------------------------------------------------------
# Convert 'year' to character for all datasets
#-------------------------------------------------------------

# Ensure 'year' is in character format for all panel data
panel_data_salary$year <- as.character(panel_data_salary$year)
panel_data_population_size$year <- as.character(panel_data_population_size$year)
panel_data_unemployed$year <- as.character(panel_data_unemployed$year)


#-------------------------------------------------------------
# Creating a list of data frames and converting all columns to character
#-------------------------------------------------------------

# Create a list of the three datasets (salary, population size, unemployment rate)
data_frames <- list(panel_data_salary, panel_data_population_size, panel_data_unemployed)

# Convert all columns in each dataset to character type
data_frames <- lapply(data_frames, function(df) {
  df %>% mutate(across(everything(), ~ type.convert(as.character(.), as.is = TRUE)))
})

#-------------------------------------------------------------
# Merging the socioeconomic data into one panel dataset
#-------------------------------------------------------------

# Start with the first dataset
socioeconomic_panel_data <- data_frames[[1]]

# Merge all datasets using a left join
for (i in 2:length(data_frames)) {
  socioeconomic_panel_data <- left_join(socioeconomic_panel_data, data_frames[[i]], by = c("Code", "year"))
}

#-------------------------------------------------------------
# Clean the 'County' column by removing unwanted prefixes
#-------------------------------------------------------------

socioeconomic_panel_data <- socioeconomic_panel_data %>%
  mutate(County = str_replace_all(County, c("Powiat " = "", "m. " = "", "st. " = "")))

# Remove duplicate 'County' columns from previous merges
socioeconomic_panel_data <- socioeconomic_panel_data %>%
  select(-County.x, -County.y)


#-------------------------------------------------------------
# ADDING VOTING DATA
#-------------------------------------------------------------

#ADDING VOTING DATA 2005. The data has been donwloaded from the National Electoral Commission website (https://danewyborcze.kbw.gov.pl/indexc4fa.html?title=Strona_g%C5%82%C3%B3wna)

# Load the data from the Excel file
elections_2005 <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/1456225675_36797.xlsx")
# Add 'year' column
elections_2005 <- elections_2005 %>%
  mutate(year = 2005)

# Select relevant columns
elections_2005_clean <- elections_2005 %>%
  select(TERYT, Powiat, year, `6 - Prawo i Sprawiedliwość`, `8 - Platforma Obywatelska`, `Głosy ważne`)

# Rename columns for clarity
elections_2005_clean <- elections_2005_clean %>%
  rename(Code = TERYT,
         County = Powiat,
         `Prawo i Sprawiedliwosc` = `6 - Prawo i Sprawiedliwość`,
         `Platforma Obywatelska` = `8 - Platforma Obywatelska`,
         `Valid ballot papers` = `Głosy ważne`)


elections_2005_clean$Code <- as.character(elections_2005_clean$Code)
elections_2005_clean$year <- as.character(elections_2005_clean$year)


#ADDING VOTING DATA 2007. The data has been donwloaded from the National Electoral Commission website (https://danewyborcze.kbw.gov.pl/indexc4fa.html?title=Strona_g%C5%82%C3%B3wna)

# Load the data for 2007 elections
elections_2007 <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/sejm2007-pow-listy.xlsx")

# Add 'year' column
elections_2007 <- elections_2007 %>%
  mutate(year = 2007)

# Select relevant columns
elections_2007_clean <- elections_2007 %>%
  select(`Kod pow.`, Powiat, year, `6 - Prawo i Sprawiedliwość`, 
         `8 - Platforma Obywatelska`, `10 - Polskie Stronnictwo Ludowe`, Ważne)

# Combine PSL votes into PO votes
elections_2007_clean <- elections_2007_clean %>%
  mutate(`8 - Platforma Obywatelska` = `8 - Platforma Obywatelska` + `10 - Polskie Stronnictwo Ludowe`)

# Rename columns for clarity
elections_2007_clean <- elections_2007_clean %>%
  rename(Code = `Kod pow.`,
         County = Powiat,
         `Prawo i Sprawiedliwosc` = `6 - Prawo i Sprawiedliwość`,
         `Platforma Obywatelska` = `8 - Platforma Obywatelska`,
         `Valid ballot papers` = Ważne)

# ADDING VOTING DATA 2011
# Load the data for 2011 elections
elections_2011 <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/2011-sejm-pow-listy.xlsx")

# Add 'year' column
elections_2011 <- elections_2011 %>%
  mutate(year = 2011)

# Select relevant columns
elections_2011_clean <- elections_2011 %>%
  select(TERYT, Powiat, year, `Komitet Wyborczy Prawo i Sprawiedliwość`,
         `Komitet Wyborczy Platforma Obywatelska RP`, 
         `Komitet Wyborczy Polskie Stronnictwo Ludowe`, `Głosy ważne`)

# Combine PSL votes into PO votes
elections_2011_clean <- elections_2011_clean %>%
  mutate(`Komitet Wyborczy Platforma Obywatelska RP` = `Komitet Wyborczy Platforma Obywatelska RP` + `Komitet Wyborczy Polskie Stronnictwo Ludowe`)

# Rename columns
elections_2011_clean <- elections_2011_clean %>%
  rename(Code = TERYT,
         County = Powiat,
         `Prawo i Sprawiedliwosc` = `Komitet Wyborczy Prawo i Sprawiedliwość`,
         `Platforma Obywatelska` = `Komitet Wyborczy Platforma Obywatelska RP`,
         `Valid ballot papers` = `Głosy ważne`)

# ADDING VOTING DATA 2015
# Load the data for 2015 elections
elections_2015 <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/2015-gl-lis-pow.xlsx")

# Add 'year' column
elections_2015 <- elections_2015 %>%
  mutate(year = 2015)

# Select relevant columns
elections_2015_clean <- elections_2015 %>%
  select(TERYT, Powiat, year, 
         `1 - Komitet Wyborczy Prawo i Sprawiedliwość`, 
         `2 - Komitet Wyborczy Platforma Obywatelska RP`,
         `8 - Komitet Wyborczy Nowoczesna Ryszarda Petru`, 
         `5 - Komitet Wyborczy Polskie Stronnictwo Ludowe`, 
         `Głosy ważne`)

# Combine votes from Nowoczesna and PSL into PO votes
elections_2015_clean <- elections_2015_clean %>%
  mutate(`2 - Komitet Wyborczy Platforma Obywatelska RP` = 
           `2 - Komitet Wyborczy Platforma Obywatelska RP` + 
           `8 - Komitet Wyborczy Nowoczesna Ryszarda Petru` + 
           `5 - Komitet Wyborczy Polskie Stronnictwo Ludowe`)

# Rename columns
elections_2015_clean <- elections_2015_clean %>%
  rename(Code = TERYT,
         County = Powiat,
         `Prawo i Sprawiedliwosc` = `1 - Komitet Wyborczy Prawo i Sprawiedliwość`,
         `Platforma Obywatelska` = `2 - Komitet Wyborczy Platforma Obywatelska RP`,
         `Valid ballot papers` = `Głosy ważne`)

# ADDING VOTING DATA 2019
# Load the data for 2019 elections
elections_2019 <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/wyniki_gl_na_listy_po_powiatach_sejm.xlsx")

# Add 'year' column
elections_2019 <- elections_2019 %>%
  mutate(year = 2019)

# Clean and combine coalition votes (Platforma Obywatelska + PSL)
elections_2019_clean <- elections_2019 %>%
  mutate(
    `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19` = 
      as.numeric(`KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19`) + 
      as.numeric(`KOMITET WYBORCZY POLSKIE STRONNICTWO LUDOWE - ZPOW-601-19/19`)
  )




# Select relevant columns
elections_2019_clean <- elections_2019 %>%
  select(`Kod TERYT`, Powiat, year, 
         `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ - ZPOW-601-9/19`, 
         `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19`, 
         `KOMITET WYBORCZY POLSKIE STRONNICTWO LUDOWE - ZPOW-601-19/19`, 
         `Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów`)

# Add the total votes column (KOALICJA OBYWATELSKA + PSL)
elections_2019_clean <- elections_2019_clean %>%
  mutate(
    total_votes = `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19` + 
      `KOMITET WYBORCZY POLSKIE STRONNICTWO LUDOWE - ZPOW-601-19/19`
  )



# Rename columns for clarity
elections_2019_clean <- elections_2019_clean %>%
  rename(Code = `Kod TERYT`,
         County = Powiat,
         `Prawo i Sprawiedliwosc` = `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ - ZPOW-601-9/19`,
         `Platforma Obywatelska` = `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19`,
         `Valid ballot papers` = `Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów`)

# ADDING VOTING DATA 2023
# Load the data for 2023 elections

elections_2023 <- read_excel("C:/Users/Marian/Documents/GitHub/Economic_Voting_PL/Data/wyniki_gl_na_listy_po_powiatach_sejm_utf8.xlsx")

# Add 'year' column
elections_2023 <- elections_2023 %>%
  mutate(year = 2023)


# Select relevant columns for the 2023 dataset
elections_2023_clean <- elections_2023 %>%
  select(`TERYT Powiatu`, Powiat, year, 
         `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ`, 
         `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI`, 
         `KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE`,
         `KOMITET WYBORCZY NOWA LEWICA`,
         `Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów`)

# Combine votes from Platforma Obywatelska, Nowa Lewica, and Trzecia Droga Polska
elections_2023_clean <- elections_2023_clean %>%
  mutate(`KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI` = 
           `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI` + 
           `KOMITET WYBORCZY NOWA LEWICA` + `KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE`)

# Rename columns for clarity
elections_2023_clean <- elections_2023_clean %>%
  rename(Code = `TERYT Powiatu`,
         County = Powiat,
         `Prawo i Sprawiedliwosc` = `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ`,
         `Platforma Obywatelska` = `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI`,
         `Valid ballot papers` = `Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów`)




#-------------------------------------------------------------
# Final Merge
#-------------------------------------------------------------

# List of election data frames
election_dfs <- list(
  elections_2005_clean, 
  elections_2007_clean, 
  elections_2011_clean, 
  elections_2015_clean, 
  elections_2019_clean, 
  elections_2023_clean
)

# Convert columns where applicable to numeric
election_dfs <- lapply(election_dfs, function(df) {
  df %>% mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))
})

# Combine all election data frames
elections_combined <- bind_rows(election_dfs)

# Merge socioeconomic and election data
final_merge <- left_join(elections_combined, socioeconomic_panel_data, by = c("Code", "year"))

# Select relevant columns for final dataset
final_merge_selected <- final_merge %>%
  select(Code, year, 
         `Prawo i Sprawiedliwosc`, 
         `Platforma Obywatelska`, 
         `Valid ballot papers`, 
         average_salary, 
         population_size, 
         unemployment_rate)

#-------------------------------------------------------------
# Remove rows with NA values
#-------------------------------------------------------------

final_merge_selected_clean <- final_merge_selected %>%
  drop_na()

# Final cleaned dataset
print(final_merge_selected_clean)

