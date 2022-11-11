#### PSCI 338
## Final Project: Cleaning the Data
## Dylan Radley

library(rio)
library(dplyr)
library(tidyr)
library(stargazer)
library(stringr)

rm(list = ls())

setwd("~/Desktop/GitHub/psci-338-project/Data/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.

### Census Data --------------------
# First the 3 major sources of ACS Data + Population density. I will clean and merge them.

censussoc <- import("ACSData/2018USSocialCharacteristicsACS5Yr.csv")
popdensity <- import("popdensitysocialexplorer.csv")
censusdemhou <- import("ACSData/2018USDemo&HousingCharacteristicsACS5Yr..csv")
censusecon <- import("ACSData/2018USEconCharacteristicsACS5Yr..csv")

# Adjust the column names of the ACS Data

names(censussoc) <- censussoc[1, ] ; censussoc <- censussoc[-1, ]
names(censusdemhou) <- censusdemhou[1, ] ; censusdemhou <- censusdemhou[-1, ]
names(censusecon) <- censusecon[1, ] ; censusecon <- censusecon[-1, ]

# Drop all of the extra rows in the population density dataset, I just need density and variable to merge with.

names(popdensity)
popdensity <- popdensity[, c(1, 2, 57)]
names(popdensity)[3] <- "popdensity"

# Merge the ACS data together!

censusfull <- merge(censusdemhou, censusecon, by = "id")
censusfull <- merge(censusfull, censussoc, by = "id")

# We need to create an identifier to merge population density and ACS data

names(popdensity)[1] <- "FIPS"
censusfull$FIPS <- sub(".*US", "", censusfull$id) # this pulls everything after US in the id column.

# add leading zeroes to four digit fips in pop density, since leading zeroes are in the ACS dataset

popdensity$FIPS[nchar(popdensity$FIPS) == 4] <- paste0("0", popdensity$FIPS[nchar(popdensity$FIPS) == 4])


censusfull <- merge(censusfull, popdensity, by = "FIPS")
censusfull <- censusfull[, -1] # remove the extra FIPS column

# Looking through the columns, the below are the ones we would like to keep!

census <- censusfull[, c(1, 2, 8, 26, 30, 162, 182, 224, 394, 456, 566, 570, 574, 578, 582, 586, 590, 
                         594, 598, 602, 604, 608, 708, 834, 870, 1143, 1147, 1151, 1155, 1159, 1163, 1167, 1171, 
                         1175, 1255, 1275, 1518)]

# Turn the second column into a more standardized county---state id.

names(census)[2] <- "countystate"
census$countystate <- gsub(" County, ", "---", census$countystate)

# Save original colnames, but then try to make them more legible

orignames <- names(census)
names(census) <- gsub("Estimate", "", names(census))
names(census) <- gsub("Percent", "pct", names(census))
names(census) <- gsub(" ", "", names(census))
names(census) <- gsub("!!.*!!", "", names(census))

# Fix some manually

names(census)[24] <- "pctFamiliesIncomeBelowPoverty"
names(census)[25] <- "pctPeopleIncomeBelowPoverty"

# Make the data numeric so math can be done on it.

census[, c(3:37)] <- census[, c(3:37)] %>% mutate_if(is.character,as.numeric)

# Modify the county-state idenitifer for Louisiana, which has parishes instead.

census$countystate[str_detect(census$countystate, "Louisiana")] <- gsub(" Parish, ", "---", 
                                                                        census$countystate[str_detect(census$countystate, "Louisiana")])


### Load and Clean Polling Place Data -------

setwd("PollingPlaces2018/") # set the working directory to the 2018 polling place files

states <- dir() # will loop over this
out_state <- list() # and output into this

for(state in 1:length(states)){
  sta <- import(states[state]) # pull in the list of polling places
  sta$num <- 1 # this will be summed to calculate polling places per county
  countysta <- summarize(group_by(sta, county_name),
                         pollingplaces = sum(num)) # calculate the number of polling places
  names(countysta)[1] <- "countystate" # match the identifier
  # modify the identifier further to get it to match
  countysta$countystate <- paste0(countysta$countystate, sep = "---", sub("\\_.*", "", basename(states[state])))
  countysta$countystate <- str_to_title(countysta$countystate)
  
  out_state[[length(out_state) + 1]] <- countysta # add this to our list of states
  print(states[state]) # shows any place that the loop may be breaking
}

# Combine all of the different polling places that were cleaned

pollingplacesnat <- bind_rows(out_state)

# Fine Counties that do not merge between census and polling places

test <- anti_join(pollingplacesnat, census, by = "countystate", all.x = TRUE)

# Make several manual fixes!

pollingplacesnat$countystate[pollingplacesnat$countystate == "Dewitt---Illinois"] <- "De Witt---Illinois"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Leflore---Oklahoma"] <- "Le Flore---Oklahoma"
pollingplacesnat$countystate[pollingplacesnat$countystate =="Jodaviess---Illinois"] <- "Jo Daviess---Illinois"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Lewis & Clark---Montana"] <- "Lewis and Clark---Montana"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Boxbutte---Nebraska"] <- "Box Butte---Nebraska"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Grant_ne---Nebraska"] <- "Grant---Nebraska"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Keyapaha---Nebraska"] <- "Keya Paha---Nebraska"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Red_willow---Nebraska"] <- "Red Willow---Nebraska"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Dona Ana---New Mexico"] <- "Doña Ana---New Mexico"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Vanwert---Ohio"] <- "Van Wert---Ohio"
pollingplacesnat$countystate[pollingplacesnat$countystate == "Scottsbluff---Nebraska"] <- "Scotts Bluff---Nebraska"

# make both uppercase to simplify
pollingplacesnat$countystate <- toupper(pollingplacesnat$countystate)
census$countystate <- toupper(census$countystate)

pollingplacesnat$countystate <- gsub("Mcd", "McD", pollingplacesnat$countystate)

# Merge!

ppsfull <- merge(pollingplacesnat, census, by = "countystate", all.x = TRUE)

setwd("~/Desktop/GitHub/psci-338-project/Data/") # shift the working directory back to all data

# Now to shorten all of the state names to their abbreviations with a crosswalk

ppsfull <- separate(ppsfull, countystate,
                    c("county", "state"),
                    sep = "---")

abbrevs <- import("state_abbreviations.csv") # crosswalk file
abbrevs$State <- toupper(abbrevs$State)

ppsfull <- merge(ppsfull, abbrevs[, c(1, 3)], by.x = "state", by.y = "State", all.x = TRUE) # merge to pull in abbreviations

ppsfull$state <- ppsfull$Code # make the state column the abbreviation, then drop the extra column
ppsfull <- ppsfull[, -40] 

# Calculate people per polling place!

ppsfull$peopleppp <- ppsfull$`Citizen,18andoverpopulation` / ppsfull$pollingplaces
ppsfull <- ppsfull[, c(1:3, 40, 5:39, 4)] # reorganize columns for ease of viewing

### Loading 2016 Results and Merging ------

prez16 <- import("2016Elect/president-wide-1.csv")

# Calculate partisan lean of each county.

prez16$plean <- (prez16$dem.two.party * 100) - 50

# Prepare to merge, first by isolating the variables of interest and then making an identifier

elect <- prez16[, c(2:3, 12)]
elect <- elect %>% unite("countystate", 2:1, sep = "---", remove = TRUE)

# Recreate the Identifier in ppsfull

ppsfull <- ppsfull %>% unite("countystate", 2:1, sep = "---", remove = TRUE)

# Find the places that will not merge

test <- anti_join(ppsfull, elect, by = "countystate", all.x = TRUE)

# Make modifications to rectify the differences. 

elect$countystate <- toupper(elect$countystate)
ppsfull$countystate <- toupper(ppsfull$countystate)

elect$countystate[elect$countystate == "DEWITT---IL"] <- "DE WITT---IL"
elect$countystate[elect$countystate == "JODAVIESS---IL"] <- "JO DAVIESS---IL"
elect$countystate[elect$countystate == "POTTAWATTAMIF---IA"] <- "POTTAWATTAMIE---IA"
elect$countystate[elect$countystate == "LEWIS & CLARK---MT"] <- "LEWIS AND CLARK---MT"
elect$countystate[elect$countystate == "DONA ANA---NM"] <- "DOÑA ANA---NM"
elect$countystate[elect$countystate == "LEFLORE---OK"] <- "LE FLORE---OK"

# Merge!

ppsfull <- merge(ppsfull, elect, by = "countystate", all.x = TRUE)

# Bring plean to the front, and separate countystate

ppsfull <- ppsfull[, c(1:3, 40, 4:39)]

ppsfull <- separate(ppsfull, countystate,
                    c("county", "state"),
                    sep = "---")

# Add a region variable

ppsfull$region <- NA
ppsfull$region[ppsfull$state %in% c("PA")] <- "Northeast"
ppsfull$region[ppsfull$state %in% c("MT", "NM")] <- "West"
ppsfull$region[ppsfull$state %in% c("DE", "GA", "LA", "NC", "OK", "SC", "WV")] <- "South"
ppsfull$region[ppsfull$state %in% c("IL", "IA", "MN", "NE", "ND", "OH", "SD", "WI")] <- "Midwest"

# Add the polling places per-capita

ppsfull$pppcapita <- ppsfull$pollingplaces / ppsfull$`Citizen,18andoverpopulation`
ppsfull <- ppsfull[, c(1:3, 43, 4:42)]

# Export all of the data!

save(ppsfull, file = "~/Desktop/GitHub/psci-338-project/Data/CleanData/2018pollingplacesRich.RData")

write.csv(ppsfull, "~/Desktop/GitHub/psci-338-project/Data/CleanData/2018pollingplacesRich.csv")

# That's all!