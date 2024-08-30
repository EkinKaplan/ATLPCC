### Cleaning Script ###
rm(list = ls())
graphics.off()

library(writexl)
library(readxl)
library(dplyr)

## Load the Masterfile
dataset1 <- read_excel("C:/Users/kaplane/Desktop/Literature Review/Data/MasterFile_v2_05_06_24.xlsx",
                       sheet = "Dataset")
#dataset1= dataset1[1:739,]


### Clean Accepted Papers Column
dataset1$Paper_Included= 
  gsub("Later", "No", dataset1$Paper_Included)

dataset1$Paper_Included <- 
  ifelse(is.na(dataset1$Paper_Included), "No",
         dataset1$Paper_Included)

# Check the column for errors
unique(dataset1$Paper_Included)



### Clean NA's from Conservation Column
dataset1$Conservation <- 
  ifelse(is.na(dataset1$Conservation), "No",
         dataset1$Conservation)

# Check for errors
unique(dataset1$Conservation)


### Clean Permanent Plots


#NA values to No's
dataset1$Permanent_Plots <- 
  ifelse(is.na(dataset1$Permanent_Plots), "No",
         dataset1$Permanent_Plots)

#Check
unique(dataset1$Permanent_Plots)


### Clean Experimental Plots

dataset1$Experimental_Plots <- 
  ifelse(is.na(dataset1$Experimental_Plots), "No",
         dataset1$Experimental_Plots)

#Check
unique(dataset1$Experimental_Plots)


### Clean Species List

dataset1$Species_List_Available <- 
  ifelse(is.na(dataset1$Species_List_Available), "No",
         dataset1$Species_List_Available)
unique(dataset1$Species_List_Available)





### Clean and structure Study System Drivers

#Create a new object
df1= dataset1


#Now clean the discrepencies

df1$SSD= gsub("\\;", "\\,", df1$SSD)
df1$SSD= gsub("\\,P", ", P", df1$SSD)
df1$SSD= gsub("\\,  ", ", ", df1$SSD)
df1$SSD= gsub("land-use cessation", "Land-use Cessation", df1$SSD)
df1$SSD= gsub("land-use \\(Ongoing\\)", "Land-use (Ongoing)", df1$SSD)
df1$SSD= gsub("Land-use\\(Ongoing\\)", "Land-use (Ongoing)", df1$SSD)
df1$SSD= gsub("NCP", "Natural Continious Pressure", df1$SSD)
df1$SSD= gsub("Land-use cessation", "Land-use Cessation", df1$SSD)
df1$SSD= gsub("Land-use change", "Land-use Change", df1$SSD)

#check for unique drivers
unique(df1$SSD)
unique(df1$SSD_comments)


split_column <- strsplit(df1$SSD, ", ")
unique(split_column)

char_vector <- unlist(split_column)

# Find the unique values
unique(char_vector)



# HABITAT
#check for unique drivers
unique(df1$Habitat_I)

split_column2 <- strsplit(df1$Habitat_I, ", ")
unique(split_column2)

char_vector2 <- unlist(split_column2)

# Find the unique values
unique(char_vector2)










#===========================================#
# Resurvey Europe - Literature Review Ekin  #
# Script: Bernd Lenzner                     #
# bernd.lenzner@univie.ac.at                #
# November 2023                             #
#===========================================#


# Load relevant packages ----
### Package names
packages <- c("tidyverse", "janitor", "cowplot", "sjPlot") 

### Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

### Load packages
invisible(lapply(packages, library, character.only = TRUE))


# Directory ----
### Select drive
drive <- "C"
### Sting to project directory
path.dir <- paste0(drive, ":/Publication_Projects/2023_ReSurveyEurope-Review-Ekin")
### String to data folder
path.data <- paste0(path.dir, "/Data")
### String to figures folder
path.fig <- paste0(path.dir, "/Figures")


# Load relevant data ----
dat <- read_excel(paste0(path.data, "/MasterFile_v2_05_06_24.xlsx"), sheet = "Dataset")
### Clean and simplify column names
#dat <- clean_names(dat)

#dat <- read_excel("Data/MasterFile_v2_05_06_24.xlsx",
#                  sheet = "Dataset")



dat$SSD= gsub("\\;", "\\,", dat$SSD)
dat$SSD= gsub("\\,P", ", P", dat$SSD)
dat$SSD= gsub("\\,  ", ", ", dat$SSD)
dat$SSD= gsub("land-use cessation", "Land-use Cessation", dat$SSD)
dat$SSD= gsub("land-use \\(Ongoing\\)", "Land-use (Ongoing)", dat$SSD)
dat$SSD= gsub("Land-use\\(Ongoing\\)", "Land-use (Ongoing)", dat$SSD)
dat$SSD= gsub("NCP", "Natural Continious Pressure", dat$SSD)
dat$SSD= gsub("Land-use cessation", "Land-use Cessation", dat$SSD)
dat$SSD= gsub("Land-use change", "Land-use Change", dat$SSD)
dat$SSD= gsub("Climate change", "Climate Change", dat$SSD)

dat$Habitat_I= gsub("Inland", "Barren", dat$Habitat_I)
dat$Habitat_I= gsub("Tundra", "Alpine Grassland", dat$Habitat_I)

dat$Conservation <- 
  ifelse(is.na(dat$Conservation), "No",
         dat$Conservation)

# Build relevant data subsets ----
## Overall ----
### Species richness change ----
dat.sr <- dat %>%
  select(SR_Change_Direction) %>%
  filter(SR_Change_Direction != "") %>%
  count(SR_Change_Direction) %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase")))

dat.sr

ggplot(dat.sr, aes(x = SR_Change_Direction, y = n)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Count of SR Change Direction", x = "SR Change Direction", y = "Count") +
  theme_minimal()


### Species diversity change ----
dat.div <- dat %>%
  select(`Diversity_Change_Direction (alpha)`) %>%
  filter(`Diversity_Change_Direction (alpha)` != "") %>%
  count(`Diversity_Change_Direction (alpha)`) %>%
  mutate(`Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase")))

dat.div

ggplot(dat.div, aes(x = `Diversity_Change_Direction (alpha)`, y = n)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Count of Diversity Change Direction (alpha)", x = "Diversity Change Direction (alpha)", y = "Count") +
  theme_minimal()

## Difference experimental vs. permanent plots ----
### Species richness change ----
dat.sr.exp <- dat %>%
  select(SR_Change_Direction, Permanent_Plots) %>%
  filter(SR_Change_Direction != "") %>%
  mutate(Permanent_Plots=replace(Permanent_Plots, 
                                 Permanent_Plots=="N",
                                 "No"),
         Permanent_Plots=replace(Permanent_Plots, 
                                 Permanent_Plots=="",
                                 "Yes")) %>%
  group_by(Permanent_Plots) %>%
  count(SR_Change_Direction) %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase")),
         Permanent_Plots = as_factor(Permanent_Plots))

dat.sr.exp

ggplot(dat.sr.exp, aes(x = SR_Change_Direction, y = n, fill = Permanent_Plots)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SR Change Direction by Permanent Plots", 
       x = "SR Change Direction", 
       y = "Count",
       fill = "Permanent Plots") +
  theme_minimal()

### Species diversity change ----
dat.div.exp <- dat %>%
  select(`Diversity_Change_Direction (alpha)`, Permanent_Plots) %>%
  filter(`Diversity_Change_Direction (alpha)` != "")  %>%
  mutate(Permanent_Plots=replace(Permanent_Plots, 
                                 Permanent_Plots=="N",
                                 "No"),
         Permanent_Plots=replace(Permanent_Plots, 
                                 Permanent_Plots=="",
                                 "Yes")) %>%
  group_by(Permanent_Plots) %>%
  count(`Diversity_Change_Direction (alpha)`) %>%
  mutate(`Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase")),
         Permanent_Plots = as_factor(Permanent_Plots))

dat.div.exp

ggplot(dat.div.exp, aes(x = `Diversity_Change_Direction (alpha)`, y = n, fill = Permanent_Plots)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Diversity Change Direction (alpha) by Permanent Plots", 
       x = "Diversity Change Direction (alpha)", 
       y = "Count",
       fill = "Permanent Plots") +
  theme_minimal()

## Change per habitat and driver type ----
### Species richness x habitat ----
dat.sr.habitat <- dat %>%
  select(SR_Change_Direction,
         Habitat_I) %>%
  filter(Habitat_I != "") %>%
  filter(SR_Change_Direction != "") %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase")))%>%
  group_by(Habitat_I) %>%
  count(SR_Change_Direction, .drop = F)

dat.sr.habitat

ggplot(dat.sr.habitat, aes(x = SR_Change_Direction, y = n, fill = Habitat_I)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SR Change Direction by Habitat Type", 
       x = "SR Change Direction", 
       y = "Count",
       fill = "Habitat Type") +
  theme_minimal()

dat.sr.habitat <- dat %>%
  select(SR_Change_Direction, Habitat_I) %>%
  filter(Habitat_I != "") %>%
  filter(SR_Change_Direction != "") %>%
  separate_rows(Habitat_I, sep = ", ") %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(Habitat_I) %>%
  count(SR_Change_Direction, .drop = FALSE)

# Plot the data
ggplot(dat.sr.habitat, aes(x = SR_Change_Direction, y = n, fill = Habitat_I)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SR Change Direction by Habitat Type", 
       x = "SR Change Direction", 
       y = "Count",
       fill = "Habitat Type") +
  theme_minimal()

dat.sr.habitat <- dat %>%
  select(SR_Change_Direction, Habitat_I) %>%
  filter(Habitat_I != "") %>%
  filter(SR_Change_Direction != "") %>%
  separate_rows(Habitat_I, sep = ", ") %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(Habitat_I) %>%
  count(SR_Change_Direction, .drop = FALSE)

# Plot the data with facets for each habitat type
ggplot(dat.sr.habitat, aes(x = SR_Change_Direction, y = n, fill = Habitat_I)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SR Change Direction by Habitat Type", 
       x = "SR Change Direction", 
       y = "Count",
       fill = "Habitat Type") +
  theme_minimal() +
  facet_wrap(~ Habitat_I, scales = "free_y")

### Diversity change x habitat ----
dat.div.habitat <- dat %>%
  select(`Diversity_Change_Direction (alpha)`,
         Habitat_I) %>%
  filter(Habitat_I != "") %>%
  filter(`Diversity_Change_Direction (alpha)` != "") %>%
  mutate(`Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(Habitat_I) %>%
  count(`Diversity_Change_Direction (alpha)`, .drop = F)

dat.div.habitat


dat.div.habitat <- dat %>%
  select(`Diversity_Change_Direction (alpha)`, Habitat_I) %>%
  filter(Habitat_I != "") %>%
  filter(`Diversity_Change_Direction (alpha)` != "") %>%
  separate_rows(Habitat_I, sep = ", ") %>%
  mutate(`Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(Habitat_I) %>%
  count(`Diversity_Change_Direction (alpha)`, .drop = FALSE)

# Plot the data with facets for each habitat type
ggplot(dat.div.habitat, aes(x = `Diversity_Change_Direction (alpha)`, y = n, fill = Habitat_I)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Diversity Change Direction (alpha) by Habitat Type", 
       x = "Diversity Change Direction (alpha)", 
       y = "Count",
       fill = "Habitat Type") +
  theme_minimal() +
  facet_wrap(~ Habitat_I, scales = "free_y")

### Drivers ----

### QUESTIONS
#### what is the difference between SSD & SSD_ii?
#### Are empty columns in SSD equal to "unknown"?
#### Why did you keep two columns and not merge them?

dat.driver <- dat %>%
  select(Ref_ID,
         SR_Change_Direction,
         `Diversity_Change_Direction (alpha)`,
         Habitat_I,
         SSD,
         Permanent_Plots,
         Experimental_Plots,
         Conservation) %>%
  separate_rows(SSD, sep = ",") %>%
  mutate(SSD = str_trim(SSD, side = "left")) # remove whitespace on left side of a string



#### Species richness x drivers ----
dat.sr.driver <- dat.driver %>%
  select(SR_Change_Direction,
         SSD) %>%
  filter(SSD != "") %>%
  filter(SR_Change_Direction != "") %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(SSD) %>%
  count(SR_Change_Direction, .drop = F)%>%
  filter(!SSD %in% c("NA", "Other", "Unknown"))

dat.sr.driver

ggplot(dat.sr.driver, aes(x = SR_Change_Direction, y = n, fill = SSD)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SR Change Direction by Drivers", 
       x = "SR Change Direction", 
       y = "Count",
       fill = "Drivers (SSD)") +
  theme_minimal() +
  facet_wrap(~ SSD, scales = "free_y")

#### Diversity x drivers ----
dat.div.driver <- dat.driver %>%
  select(`Diversity_Change_Direction (alpha)`,
         SSD) %>%
  filter(SSD != "") %>%
  filter(`Diversity_Change_Direction (alpha)` != "") %>%
  mutate(`Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(SSD) %>%
  count(`Diversity_Change_Direction (alpha)`, .drop = F)%>%
  filter(!SSD %in% c("NA", "Other", "Unknown"))

dat.div.driver


ggplot(dat.div.driver, aes(x = `Diversity_Change_Direction (alpha)`, y = n, fill = SSD)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Diversity Change Direction (alpha) by Drivers", 
       x = "Diversity Change Direction (alpha)", 
       y = "Count",
       fill = "Drivers (SSD)") +
  theme_minimal() +
  facet_wrap(~ SSD, scales = "free_y")


#### Species richness x Conservation ----
dat.sr.con <- dat %>%
  mutate(Conservation=replace(Conservation, 
                              Conservation=="", "No")) %>%
  select(SR_Change_Direction,
         Conservation) %>%
  filter(SR_Change_Direction != "") %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(Conservation) %>%
  count(SR_Change_Direction, .drop = F)

dat.sr.con


ggplot(dat.sr.con, aes(x = SR_Change_Direction, y = n, fill = Conservation)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SR Change Direction by Conservation Status", 
       x = "SR Change Direction", 
       y = "Count",
       fill = "Conservation Status") +
  theme_minimal() +
  facet_wrap(~ Conservation, scales = "free_y")

#### Species diversity x Conservation ----
dat.div.con <- dat %>%
  mutate(Conservation=replace(Conservation, 
                              Conservation=="", "No")) %>%
  select(`Diversity_Change_Direction (alpha)`,
         Conservation) %>%
  filter(`Diversity_Change_Direction (alpha)` != "") %>%
  mutate(`Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(Conservation) %>%
  count(`Diversity_Change_Direction (alpha)`, .drop = F)

dat.div.con


ggplot(dat.div.con, aes(x = `Diversity_Change_Direction (alpha)`, y = n, fill = Conservation)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Diversity Change Direction (alpha) by Conservation Status", 
       x = "Diversity Change Direction (alpha)", 
       y = "Count",
       fill = "Conservation Status") +
  theme_minimal() +
  facet_wrap(~ Conservation, scales = "free_y")

#### Species richness change x driver x habitat ----
dat.sr.driver.habitat <- dat.driver %>%
  select(SR_Change_Direction,
         SSD,
         Habitat_I) %>%
  filter(SSD != "") %>%
  filter(SR_Change_Direction != "") %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(SSD, Habitat_I) %>%
  count(SR_Change_Direction, .drop = F) %>%
  filter(!SSD %in% c("NA", "Other", "Unknown"))

dat.sr.driver.habitat

ggplot(dat.sr.driver.habitat, aes(x = SR_Change_Direction, y = n, fill = SSD)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SR Change Direction by Drivers and Habitat Type", 
       x = "SR Change Direction", 
       y = "Count",
       fill = "Drivers (SSD)") +
  theme_minimal() +
  facet_wrap(~ Habitat_I, scales = "free_y")


dat.sr.driver.habitat <- dat.driver %>%
  select(SR_Change_Direction, SSD, Habitat_I) %>%
  filter(SSD != "") %>%
  filter(SR_Change_Direction != "") %>%
  separate_rows(Habitat_I, sep = ", ") %>%
  mutate(SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(SSD, Habitat_I) %>%
  count(SR_Change_Direction, .drop = FALSE) %>%
  filter(!SSD %in% c("NA", "Other", "Unknown"))

# Plot the data with facets for each habitat type
ggplot(dat.sr.driver.habitat, aes(x = SR_Change_Direction, y = n, fill = SSD)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SR Change Direction by Drivers and Habitat Type", 
       x = "SR Change Direction", 
       y = "Count",
       fill = "Drivers (SSD)") +
  theme_minimal() +
  facet_wrap(~ Habitat_I, scales = "free_y")

#### Diversity change x driver x habitat ----
dat.div.driver.habitat <- dat.driver %>%
  select(`Diversity_Change_Direction (alpha)`,
         SSD,
         Habitat_I) %>%
  filter(SSD != "") %>%
  filter(`Diversity_Change_Direction (alpha)` != "") %>%
  mutate(`Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(SSD, Habitat_I) %>%
  count(`Diversity_Change_Direction (alpha)`, .drop = F) %>%
  filter(!SSD %in% c("NA", "Other", "Unknown"))

dat.div.driver.habitat


dat.div.driver.habitat <- dat.driver %>%
  select(`Diversity_Change_Direction (alpha)`, SSD, Habitat_I) %>%
  filter(SSD != "") %>%
  filter(`Diversity_Change_Direction (alpha)` != "") %>%
  separate_rows(Habitat_I, sep = ", ") %>%
  mutate(`Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(SSD, Habitat_I) %>%
  count(`Diversity_Change_Direction (alpha)`, .drop = FALSE) %>%
  filter(!SSD %in% c("NA", "Other", "Unknown"))

# Plot the data with facets for each habitat type
ggplot(dat.div.driver.habitat, aes(x = `Diversity_Change_Direction (alpha)`, y = n, fill = SSD)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Diversity Change Direction (alpha) by Drivers and Habitat Type", 
       x = "Diversity Change Direction (alpha)", 
       y = "Count",
       fill = "Drivers (SSD)") +
  theme_minimal() +
  facet_wrap(~ Habitat_I, scales = "free_y")

## Temporal trends analysis ----
### Species richness change ----
dat.sr.time <- dat %>%
  select(SR_Change_Direction, Start_year, End_year) %>%
  filter(SR_Change_Direction != "") %>%
  mutate(timespan = End_year - Start_year, 
         SR_Change_Direction = as_factor(SR_Change_Direction),
         SR_Change_Direction = factor(SR_Change_Direction, levels = c("Decrease", "No change", "Increase"))) %>%
  group_by(timespan) %>%
  count(SR_Change_Direction)

dat.sr.time


sr.mod.time.de <- glm(n ~ timespan, data = dat.sr.time %>% filter(SR_Change_Direction == "Decrease"), family = "poisson")
sr.mod.time.no <- glm(n ~ timespan, data = dat.sr.time %>% filter(SR_Change_Direction == "No change"), family = "poisson")
sr.mod.time.in <- glm(n ~ timespan, data = dat.sr.time %>% filter(SR_Change_Direction == "Increase"), family = "poisson")

summary(sr.mod.time.de)
summary(sr.mod.time.no)
summary(sr.mod.time.in)

### Species diversity change ----
dat.div.time <- dat %>%
  select(`Diversity_Change_Direction (alpha)`, Start_year, End_year) %>%
  filter(`Diversity_Change_Direction (alpha)` != "") %>%
  mutate(timespan = End_year - Start_year, 
         `Diversity_Change_Direction (alpha)` = as_factor(`Diversity_Change_Direction (alpha)`),
         `Diversity_Change_Direction (alpha)` = factor(`Diversity_Change_Direction (alpha)`, levels = c("Decrease", "No change", "Increase"))) %>%
  drop_na(timespan) %>%
  group_by(timespan) %>%
  count(`Diversity_Change_Direction (alpha)`)

dat.div.time

div.mod.time.de <- glm(n ~ timespan, data = dat.div.time %>% filter(`Diversity_Change_Direction (alpha)` == "Decrease"), family = "poisson")
div.mod.time.no <- glm(n ~ timespan, data = dat.div.time %>% filter(`Diversity_Change_Direction (alpha)` == "No change"), family = "poisson")
div.mod.time.in <- glm(n ~ timespan, data = dat.div.time %>% filter(`Diversity_Change_Direction (alpha)` == "Increase"), family = "poisson")

summary(div.mod.time.de)
summary(div.mod.time.no)
summary(div.mod.time.in)


#=======================================================================================#
# Visualization ----

datum <- Sys.Date()

bar.col <- c("#9B59B6", "#1ABC9C", "#F1C40F")
bar.col2 <- c("#9B59B6", "#F1C40F", "#1ABC9C")

### Overall data
p.sr <- ggplot(dat.sr, aes(x = SR_Change_Direction, y=n)) + 
  geom_bar(position="dodge", stat="identity", fill = bar.col) +
  coord_flip() +
  xlab("Trend") +
  ylab("") +
  ggtitle("Species richness change") + 
  ylim(0,400) +
  theme_bw()

p.div <- ggplot(dat.div, aes(x = `Diversity_Change_Direction (alpha)`, y=n)) + 
  geom_bar(position="dodge", stat="identity", fill = bar.col) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Diversity change") + 
  ylim(0,200) +
  theme_bw() +
  theme(axis.text.y=element_blank())

### Experimental vs. permanent
p.sr.exp <- ggplot(dat.sr.exp, aes(x = SR_Change_Direction, y=n)) + 
  geom_bar(position="dodge", stat="identity", fill = bar.col) +
  facet_wrap(~Permanent_Plots) +
  coord_flip() +
  xlab("Trend") +
  ylab("") +
  ggtitle("Species richness change") + 
  ylim(0,200) +
  theme_bw()

p.div.exp <- ggplot(dat.div, aes(x = `Diversity_Change_Direction (alpha)`, y=n)) + 
  geom_bar(position="dodge", stat="identity", fill = bar.col) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Diversity change") + 
  ylim(0,200) +
  theme_bw() +
  theme(axis.text.y=element_blank())


### Habitat
p.sr.habitat <- ggplot(dat.sr.habitat, aes(fill = SR_Change_Direction, y=n, x=Habitat_I)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = bar.col2) +
  coord_flip() +
  xlab("Habitat") +
  ylab("") +
  ylim(0,200) +
  theme_bw() +
  theme(legend.position = "none")

p.div.habitat <- ggplot(dat.div.habitat, aes(fill = `Diversity_Change_Direction (alpha)`, y=n, x=Habitat_I)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = bar.col2) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ylim(0,100) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y=element_blank())

### Driver
new_order = c("Climate Change", "IAS", "Pollution",
              "Land-use (Ongoing)", "Land-use Cessation",
              "Land-use Change", "Natural Continious Pressure", "Mid-disturbance",
               "Postdisturbance")
dat.sr.driver$SSD <- factor(dat.sr.driver$SSD, levels = new_order)

p.sr.driver <- ggplot(dat.sr.driver, aes(fill = SR_Change_Direction, y=n, x=SSD)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = "", values = bar.col2) +
  coord_flip() +
  xlab("Driver") +
  ylab("# publications") +
  ylim(0,200) +
  theme_bw() +
  theme(legend.position = "bottom")


dat.div.driver$SSD <- factor(dat.div.driver$SSD, levels = new_order)
p.div.driver <- ggplot(dat.div.driver, aes(fill = `Diversity_Change_Direction (alpha)`, y=n, x=SSD)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = "", values = bar.col2) +
  coord_flip() +
  xlab("") +
  ylab("# publications") +
  ylim(0,100) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_blank())


### Plot including overall trend, habitats, drivers
#x11()
fig2 <- cowplot::plot_grid(p.sr, p.div,p.sr.habitat, p.div.habitat,p.sr.driver, p.div.driver, ncol = 2, align = "v", rel_heights = c(1/13, 1/5, 1/5))

ggsave(filename = paste0(path.fig, "/", datum, "-Figure2-trends.pdf"),
       fig2,
       width = 10, height = 9, unit = "in")




## Conservation plot ----
e.dist.sr= data.frame(Cons= c("No","Partial","Yes"),
                      equal= c(127.333, 7, 96.66667))
p.sr.con <- ggplot(dat.sr.con, aes(fill = SR_Change_Direction, y=n, x=Conservation)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = "", values = bar.col2) +
  coord_flip() +
  xlab("Conservation") +
  ylab("") +
  ylim(0,250) +
  ggtitle("Species richness change") +
  theme_bw() +
  theme(legend.position = "bottom")

p.div.con <- ggplot(dat.div.con, aes(fill = `Diversity_Change_Direction (alpha)`, y=n, x=Conservation)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = "", values = bar.col2) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ylim(0,250) +
  ggtitle("Diversity change") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_blank())

#x11()
fig4 <- cowplot::plot_grid(p.sr.con, p.div.con, ncol = 2, align = "v", rel_heights = c(1/4, 1/4))

ggsave(filename = paste0(path.fig, "/", datum, "-Figure4-Conservation.pdf"),
       fig4,
       width = 10, height = 5, unit = "in")

## Composite/interaction plot ----
### All combinations ----
dat.sr.driver.habitat$SSD <- factor(dat.sr.driver.habitat$SSD, levels = new_order)
p.sr.driver.habitat <- ggplot(dat.sr.driver.habitat, aes(fill = SR_Change_Direction, y=n, x=SSD)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = "", values = bar.col2) +
  facet_wrap(~Habitat_I, dir = "v") +
  coord_flip() +
  xlab("") +
  ylab("# publications") +
  ggtitle("Species richness change")+
  ylim(0,50) +
  theme_bw()+
  theme(legend.position = "bottom")

dat.div.driver.habitat$SSD <- factor(dat.div.driver.habitat$SSD, levels = new_order)
p.div.driver.habitat <- ggplot(dat.div.driver.habitat, aes(fill = `Diversity_Change_Direction (alpha)`, y=n, x=SSD)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = "", values = bar.col2) +
  facet_wrap(~Habitat_I, dir = "v") +
  coord_flip() +
  xlab("") +
  ylab("# publications") +
  ggtitle("Diversity change")+
  ylim(0,50) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_blank())

#x11()
figS1 <- cowplot::plot_grid(p.sr.driver.habitat, p.div.driver.habitat, ncol = 2, align = "v", rel_heights = c(1/4, 1/4))

ggsave(filename = paste0(path.fig, "/", datum, "-FigureS1-interactions.png"),
       figS1,
       width = 13, height = 9, unit = "in")

ggsave("Fig_S2.pdf", p.sr.driver.habitat, width = 13, height = 9, unit = "in")
ggsave("Fig_S3.pdf", p.div.driver.habitat, width = 13, height = 9, unit = "in")

### Subset ----
dat.sr.driver.habitat2 <- dat.sr.driver.habitat %>%
  filter(Habitat_I %in% c("Forest", "Grassland"))
dat.div.driver.habitat2 <- dat.div.driver.habitat %>%
  filter(Habitat_I %in% c("Forest", "Grassland"))


dat.sr.driver.habitat2$SSD <- factor(dat.sr.driver.habitat2$SSD, levels = new_order)
p.sr.driver.habitat.sub <- ggplot(dat.sr.driver.habitat2, aes(fill = SR_Change_Direction, y=n, x=SSD)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = "", values = bar.col2) +
  facet_wrap(~Habitat_I, dir = "v") +
  coord_flip() +
  xlab("") +
  ylab("# publications") +
  ggtitle("Species richness change")+
  ylim(0,50) +
  theme_bw()+
  theme(legend.position = "bottom")


dat.div.driver.habitat2$SSD <- factor(dat.div.driver.habitat2$SSD, levels = new_order)
p.div.driver.habitat.sub <- ggplot(dat.div.driver.habitat2, aes(fill = `Diversity_Change_Direction (alpha)`, y=n, x=SSD)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = "", values = bar.col2) +
  facet_wrap(~Habitat_I, dir = "v") +
  coord_flip() +
  xlab("") +
  ylab("# publications") +
  ggtitle("Diversity change")+
  ylim(0,50) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_blank())


#x11()
fig3 <- cowplot::plot_grid(p.sr.driver.habitat.sub, p.div.driver.habitat.sub, ncol = 2, align = "v", rel_heights = c(1/4, 1/4))

ggsave(filename = paste0(path.fig, "/", datum, "-Figure3-interactions-sub.pdf"),
       fig3,
       width = 10, height = 7, unit = "in")


### Species richness change
p.sr.time <- ggplot(dat.sr.time, aes(x=timespan, y = n, shape = factor(SR_Change_Direction), fill = factor(SR_Change_Direction))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", se = T, method.args = list(family = "poisson"), color = "black") +
  scale_shape_manual(name = "", values = c("Increase" = 3, "Decrease" = 1, "No change" = 4)) +  # 3: +, 1: o, 4: x
  scale_fill_manual(name = "", values = bar.col2) +  # keep your color values
  ggtitle("Species richness change") + 
  ylab("# publications") +
  xlab("Years between first and last observation") +
  ylim(0,22) +
  theme_bw() +
  theme(legend.position = "bottom")

### Diversity change
p.div.time <- ggplot(dat.div.time, aes(x=timespan, y = n, shape = factor(`Diversity_Change_Direction (alpha)`), fill = factor(`Diversity_Change_Direction (alpha)`))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", se = T, method.args = list(family = "poisson"), color = "black") +
  scale_shape_manual(name = "", values = c("Increase" = 3, "Decrease" = 1, "No change" = 4)) +  # 3: +, 1: o, 4: x
  scale_fill_manual(name = "", values = bar.col2) +  # keep your color values
  ggtitle("Composition change") + 
  ylab("") +
  xlab("Years between first and last observation") +
  ylim(0,22) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank())


fig5 <- cowplot::plot_grid(p.sr.time, p.div.time, ncol = 2, align = "v", rel_heights = c(1/4, 1/4))
ggsave(filename = paste0(path.fig, "/", datum, "-Figure5-temporal-trend.pdf"),
       fig5,
       width = 10, height = 5, unit = "in")


#==============================================================================#
#==============================================================================#
#==============================================================================#
# Analyses ----
sr.rich <- dat.sr %>% pivot_wider(names_from = SR_Change_Direction, values_from = n) %>%
  select(Decrease, `No change`, Increase)
sr.prop <- sr.rich/sum(sr.rich)

div.rich <- dat.div %>% pivot_wider(names_from = `Diversity_Change_Direction (alpha)`, values_from = n) %>%
  select(Decrease, `No change`, Increase)
div.prop <- div.rich/sum(div.rich)


null.prop <- sr.prop
null.prop[1,] <- 1/3
#null.prop <- c(1/3, 1/3, 1/3)

#==============================================================================#
## H1: No trend in species richness/diversity change over time ----
res.rich <- chisq.test(sr.rich, p = c(1/3, 1/3, 1/3), simulate.p.value=F)
res.rich

res.div <- chisq.test(div.rich, p = c(1/3, 1/3, 1/3), simulate.p.value=F)
res.div


#==============================================================================#
## H2.1: Biodiversity change differently change across habitats ----
### Species richness change
sr.hab <- dat.sr.habitat %>% 
  pivot_wider(names_from = SR_Change_Direction, values_from = n) 

tab.sr.hab <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.hab)[1]){
  res <- chisq.test(as.numeric(sr.hab[i,-1]), 
                    p = as.numeric(sr.prop), simulate.p.value=T)
  tab.sr.hab[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.sr.hab <- cbind(sr.hab[,1], tab.sr.hab)
tab.sr.hab


#### NULL
tab.sr.hab.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.hab)[1]){
  res <- chisq.test(as.numeric(sr.hab[i,-1]), 
                    p = as.numeric(null.prop), simulate.p.value=T)
  tab.sr.hab.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}

tab.sr.hab.null <- cbind(sr.hab[,1], tab.sr.hab.null)
tab.sr.hab.null

### Species diversity change
div.hab <- dat.div.habitat %>% 
  pivot_wider(names_from = `Diversity_Change_Direction (alpha)`, values_from = n) 

tab.div.hab <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.hab)[1]){
  res <- chisq.test(as.numeric(div.hab[i,-1]), 
                    p = as.numeric(div.prop), simulate.p.value=T)
  tab.div.hab[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.div.hab <- cbind(div.hab[,1], tab.div.hab)
tab.div.hab


#### NULL
div.hab <- dat.div.habitat %>% 
  pivot_wider(names_from = `Diversity_Change_Direction (alpha)`, values_from = n) 

tab.div.hab.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.hab)[1]){
  res <- chisq.test(as.numeric(div.hab[i,-1]), 
                    p = as.numeric(null.prop), simulate.p.value=T)
  tab.div.hab.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.div.hab.null <- cbind(div.hab[,1], tab.div.hab.null)
tab.div.hab.null

#==============================================================================#
## H2.2: Biodiversity change differently change across drivers ----
### Species richness change
sr.driv <- dat.sr.driver %>% 
  pivot_wider(names_from = SR_Change_Direction, values_from = n) 

tab.sr.driv <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv)[1]){
  res <- chisq.test(as.numeric(sr.driv[i,-1]), 
                    p = as.numeric(sr.prop), simulate.p.value=T)
  tab.sr.driv[i,] <- c(res$statistic, res$parameter, res$p.value)
}


tab.sr.driv <- cbind(sr.driv[,1], tab.sr.driv)
tab.sr.driv

#### NULL
sr.driv <- dat.sr.driver %>% 
  pivot_wider(names_from = SR_Change_Direction, values_from = n) 

tab.sr.driv.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv)[1]){
  res <- chisq.test(as.numeric(sr.driv[i,-1]), 
                    p = as.numeric(null.prop), simulate.p.value=T)
  tab.sr.driv.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}


tab.sr.driv.null <- cbind(sr.driv[,1], tab.sr.driv.null)
tab.sr.driv.null


### Species diversity change
div.driv <- dat.div.driver %>% 
  pivot_wider(names_from = `Diversity_Change_Direction (alpha)`, values_from = n) 

tab.div.driv <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv)[1]){
  res <- chisq.test(as.numeric(div.driv[i,-1]), 
                    p = as.numeric(div.prop), simulate.p.value=T)
  tab.div.driv[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.div.driv <- cbind(div.driv[,1], tab.div.driv)
tab.div.driv


#### NULL
div.driv <- dat.div.driver %>% 
  pivot_wider(names_from = `Diversity_Change_Direction (alpha)`, values_from = n) 

tab.div.driv.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv)[1]){
  res <- chisq.test(as.numeric(div.driv[i,-1]), 
                    p = as.numeric(null.prop), simulate.p.value=T)
  tab.div.driv.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.div.driv.null <- cbind(div.driv[,1], tab.div.driv.null)
tab.div.driv.null

#==============================================================================#
## H3: Driver effect change across habitats (forest, grassland) ----
### Species richness change
sr.driv.hab <- dat.sr.driver.habitat %>% 
  pivot_wider(names_from = SR_Change_Direction, values_from = n) 

sr.forest <- sr.hab %>% 
  filter(Habitat_I == "Forest")
sr.forest.prop <- sr.forest[,-1]/rowSums(sr.forest[,-1])

sr.grass <- sr.hab %>% 
  filter(Habitat_I == "Grassland")
sr.grass.prop <- sr.grass[,-1]/rowSums(sr.grass[,-1])

#### Forest
sr.driv.forest <- sr.driv.hab %>%
  filter(Habitat_I == "Forest")

tab.driv.forest <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv.forest)[1]){
  res <- chisq.test(as.numeric(sr.driv.forest[i,-c(1,2)]), 
                    p = as.numeric(sr.forest.prop), simulate.p.value=T)
  tab.driv.forest[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.driv.forest <- cbind(sr.driv.forest[,c(1,2)], tab.driv.forest)
tab.driv.forest


#### NULL
tab.driv.forest.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv.forest)[1]){
  res <- chisq.test(as.numeric(sr.driv.forest[i,-c(1,2)]), 
                    p = as.numeric(null.prop), simulate.p.value=T)
  tab.driv.forest.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.driv.forest.null <- cbind(sr.driv.forest[,c(1,2)], tab.driv.forest.null)
tab.driv.forest.null


#### Grassland
sr.driv.grass <- sr.driv.hab %>%
  filter(Habitat_I == "Grassland")

tab.driv.grass <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv.grass)[1]){
  res <- chisq.test(as.numeric(sr.driv.grass[i,-c(1,2)]), 
                    p = as.numeric(sr.grass.prop), simulate.p.value=T)
  tab.driv.grass[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.driv.grass <- cbind(sr.driv.grass[,c(1,2)], tab.driv.grass)
tab.driv.grass


#### NULL
tab.driv.grass.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv.grass)[1]){
  res <- chisq.test(as.numeric(sr.driv.grass[i,-c(1,2)]), 
                    p = as.numeric(null.prop), simulate.p.value=T)
  tab.driv.grass.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.driv.grass.null <- cbind(sr.driv.grass[,c(1,2)], tab.driv.grass.null)
tab.driv.grass.null

### Species diversity change
div.driv.hab <- dat.div.driver.habitat %>% 
  pivot_wider(names_from = `Diversity_Change_Direction (alpha)`, values_from = n) 


div.forest <- div.hab %>% 
  filter(Habitat_I == "Forest")
div.forest.prop <- div.forest[,-1]/rowSums(div.forest[,-1])

div.grass <- sr.hab %>% 
  filter(Habitat_I == "Grassland")
div.grass.prop <- div.grass[,-1]/rowSums(div.grass[,-1])


#### Forest
div.driv.forest <- div.driv.hab %>%
  filter(Habitat_I == "Forest")

div.tab.driv.forest <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv.forest)[1]){
  res <- chisq.test(as.numeric(div.driv.forest[i,-1]), 
                    p = as.numeric(div.forest.prop), simulate.p.value=T)
  div.tab.driv.forest[i,] <- c(res$statistic, res$parameter, res$p.value)
}
div.tab.driv.forest <- cbind(div.driv.forest[,c(1,2)], div.tab.driv.forest)
div.tab.driv.forest

#### NULL

div.tab.driv.forest.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv.forest)[1]){
  res <- chisq.test(as.numeric(div.driv.forest[i,-c(1,2)]), 
                    p = as.numeric(null.prop), simulate.p.value=T)
  div.tab.driv.forest.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
div.tab.driv.forest.null <- cbind(div.driv.forest[,c(1,2)], div.tab.driv.forest.null)
div.tab.driv.forest.null

#### Grassland
div.driv.grass <- div.driv.hab %>%
  filter(Habitat_I == "Grassland")

div.tab.driv.grass <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv.grass)[1]){
  res <- chisq.test(as.numeric(div.driv.grass[i,-c(1,2)]), 
                    p = as.numeric(div.grass.prop), simulate.p.value=T)
  div.tab.driv.grass[i,] <- c(res$statistic, res$parameter, res$p.value)
}
div.tab.driv.grass <- cbind(div.driv.grass[,c(1,2)], div.tab.driv.grass)
div.tab.driv.grass

#### NULL
div.driv.grass <- div.driv.hab %>%
  filter(Habitat_I == "Grassland")

div.tab.driv.grass.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv.grass)[1]){
  res <- chisq.test(as.numeric(div.driv.grass[i,-c(1,2)]), 
                    p = as.numeric(null.prop), simulate.p.value=T)
  div.tab.driv.grass.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
div.tab.driv.grass.null <- cbind(div.driv.grass[,c(1,2)], div.tab.driv.grass.null)
div.tab.driv.grass.null


#==============================================================================#
## H4: Effect of Conservation areas on richness/diversity change ----
### Species richness change
sr.cons.yes <- as.numeric(t(dat.sr.con[7:9,"n"]))
sr.cons.no <- as.numeric(t(dat.sr.con[1:3,"n"]))

res.sr.cons.yes <- chisq.test(sr.cons.yes, p = as.numeric(sr.prop), simulate.p.value=T)
res.sr.cons.yes

res.sr.cons.no <- chisq.test(sr.cons.no, p = as.numeric(sr.prop), simulate.p.value=T)
res.sr.cons.no


#### NULL
res.sr.cons.yes.null <- chisq.test(sr.cons.yes, p = as.numeric(null.prop), simulate.p.value=T)
res.sr.cons.yes.null

res.sr.cons.no.null <- chisq.test(sr.cons.no, p = as.numeric(null.prop), simulate.p.value=T)
res.sr.cons.no.null


### Species diversity change
div.cons.yes <- as.numeric(t(dat.div.con[7:9,"n"]))
div.cons.no <- as.numeric(t(dat.div.con[1:3,"n"]))

res.div.cons.yes <- chisq.test(div.cons.yes, p = as.numeric(sr.prop), simulate.p.value=T)
res.div.cons.yes

res.div.cons.no <- chisq.test(div.cons.no, p = as.numeric(sr.prop), simulate.p.value=T)
res.div.cons.no

#### NULL
res.div.cons.yes.null <- chisq.test(div.cons.yes, p = as.numeric(null.prop), simulate.p.value=T)
res.div.cons.yes.null

res.div.cons.no.null <- chisq.test(div.cons.no, p = as.numeric(null.prop), simulate.p.value=T)
res.div.cons.no.null


#==============================================================================#
## H5: Effect of time on richness/diversity change ----
### Species richness change ----
sr.mod.time.de <- glm(n ~ timespan, data = dat.sr.time %>% filter(SR_Change_Direction == "Decrease"), family = "poisson")
sr.mod.time.no <- glm(n ~ timespan, data = dat.sr.time %>% filter(SR_Change_Direction == "No change"), family = "poisson")
sr.mod.time.in <- glm(n ~ timespan, data = dat.sr.time %>% filter(SR_Change_Direction == "Increase"), family = "poisson")

summary(sr.mod.time.de)
summary(sr.mod.time.no)
summary(sr.mod.time.in)

### Species diversity change ----
div.mod.time.de <- glm(n ~ timespan, data = dat.div.time %>% filter(`Diversity_Change_Direction (alpha)` == "Decrease"), family = "poisson")
div.mod.time.no <- glm(n ~ timespan, data = dat.div.time %>% filter(`Diversity_Change_Direction (alpha)` == "No change"), family = "poisson")
div.mod.time.in <- glm(n ~ timespan, data = dat.div.time %>% filter(`Diversity_Change_Direction (alpha)` == "Increase"), family = "poisson")

summary(div.mod.time.de)
summary(div.mod.time.no)
summary(div.mod.time.in)




## Pairwise calculations

#Richness and Diversity
chisq.test(sr.rich[,-2], p = c(1/2, 1/2), simulate.p.value=F)
chisq.test(div.rich[,-2], p = c(1/2, 1/2), simulate.p.value=F)

null.prop2= c(1/2, 1/2)

#Richness/Habitat
tab.sr.hab.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.hab)[1]){
  res <- chisq.test(as.numeric(sr.hab[i,-c(1,3)]), 
                    p = as.numeric(null.prop2), simulate.p.value=T)
  tab.sr.hab.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}

tab.sr.hab.null <- cbind(sr.hab[,1], tab.sr.hab.null)
tab.sr.hab.null



#Diversity/Habitat
div.hab <- dat.div.habitat %>% 
  pivot_wider(names_from = `Diversity_Change_Direction (alpha)`, values_from = n) 

tab.div.hab.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.hab)[1]){
  res <- chisq.test(as.numeric(div.hab[i,-c(1,3)]), 
                    p = as.numeric(null.prop2), simulate.p.value=T)
  tab.div.hab.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.div.hab.null <- cbind(div.hab[,1], tab.div.hab.null)
tab.div.hab.null



#Richness/Driver
sr.driv <- dat.sr.driver %>% 
  pivot_wider(names_from = SR_Change_Direction, values_from = n) 

tab.sr.driv.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv)[1]){
  res <- chisq.test(as.numeric(sr.driv[i,-c(1,3)]), 
                    p = as.numeric(null.prop2), simulate.p.value=T)
  tab.sr.driv.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}


tab.sr.driv.null <- cbind(sr.driv[,1], tab.sr.driv.null)
tab.sr.driv.null


#Diversity/Driver

div.driv <- dat.div.driver %>% 
  pivot_wider(names_from = `Diversity_Change_Direction (alpha)`, values_from = n) 

tab.div.driv.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv)[1]){
  res <- chisq.test(as.numeric(div.driv[i,-c(1,3)]), 
                    p = as.numeric(null.prop2), simulate.p.value=T)
  tab.div.driv.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.div.driv.null <- cbind(div.driv[,1], tab.div.driv.null)
tab.div.driv.null








###Forest and Grassland pairwise for drivers

##Forest.SR.DRIV
tab.driv.forest.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv.forest)[1]){
  res <- chisq.test(as.numeric(sr.driv.forest[i,-c(1,2,4)]), 
                    p = as.numeric(null.prop2), simulate.p.value=T)
  tab.driv.forest.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.driv.forest.null <- cbind(sr.driv.forest[,c(1,2)], tab.driv.forest.null)
tab.driv.forest.null




##Grassland.SR.DRIV
tab.driv.grass.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(sr.driv.grass)[1]){
  res <- chisq.test(as.numeric(sr.driv.grass[i,-c(1,2,4)]), 
                    p = as.numeric(null.prop2), simulate.p.value=T)
  tab.driv.grass.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
tab.driv.grass.null <- cbind(sr.driv.grass[,c(1,2)], tab.driv.grass.null)
tab.driv.grass.null


##Forest.DIV.DRIV

div.tab.driv.forest.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv.forest)[1]){
  res <- chisq.test(as.numeric(div.driv.forest[i,-c(1,2,4)]), 
                    p = as.numeric(null.prop2), simulate.p.value=T)
  div.tab.driv.forest.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
div.tab.driv.forest.null <- cbind(div.driv.forest[,c(1,2)], div.tab.driv.forest.null)
div.tab.driv.forest.null



##Grassland.DIV.DRIV

div.driv.grass <- div.driv.hab %>%
  filter(Habitat_I == "Grassland")

div.tab.driv.grass.null <- data.frame(X.squared = NA, df = NA, p.value = NA)
for(i in 1:dim(div.driv.grass)[1]){
  res <- chisq.test(as.numeric(div.driv.grass[i,-c(1,2,4)]), 
                    p = as.numeric(null.prop2), simulate.p.value=T)
  div.tab.driv.grass.null[i,] <- c(res$statistic, res$parameter, res$p.value)
}
div.tab.driv.grass.null <- cbind(div.driv.grass[,c(1,2)], div.tab.driv.grass.null)
div.tab.driv.grass.null



### Conservation pairwise

sr.cons.yes <- as.numeric(t(dat.sr.con[7:9,"n"]))
sr.cons.no <- as.numeric(t(dat.sr.con[1:3,"n"]))

#### SR
res.sr.cons.yes.null <- chisq.test(sr.cons.yes[-2], p = as.numeric(null.prop2), simulate.p.value=T)
res.sr.cons.yes.null

res.sr.cons.no.null <- chisq.test(sr.cons.no[-2], p = as.numeric(null.prop2), simulate.p.value=T)
res.sr.cons.no.null


### Species diversity change
div.cons.yes <- as.numeric(t(dat.div.con[7:9,"n"]))
div.cons.no <- as.numeric(t(dat.div.con[1:3,"n"]))


#### NULL
res.div.cons.yes.null <- chisq.test(div.cons.yes[-2], p = as.numeric(null.prop2),
                                    simulate.p.value=T)

res.div.cons.yes.null

res.div.cons.no.null <- chisq.test(div.cons.no[-2], p = as.numeric(null.prop2),
                                   simulate.p.value=T)

res.div.cons.no.null
