#===============================================================================
# "Visualisation and Exploratory Analysis - Coursework resit"
#===============================================================================
#-------------------------------------------------------------------------------
# Install and load the packages one by one.

#install.packages("ggplot2")
library(ggplot2)
#library("dplyr")
library(dplyr)
#install.packages("naniar")
library(naniar)
options(scipen=999)  # turn-off scientific notation like 1e+48

#-------------------------------------------------------------------------------
# Data Collection
data <- read.csv("world_bank_international_arrivals_islands.csv", header=TRUE)
head(data)

class(data)
dim(data)
str(data)
names(data)

#-------------------------------------------------------------------------------
# Data Cleaning		

colnames(data)[colnames(data) == "flights...WB"] = "flights"
names(data)	

data$pop <- as.numeric(data$pop)
data$areakm2 <- as.numeric(data$areakm2)
data$gdpnom <- as.numeric(data$gdpnom)
data$flights <- as.numeric(data$flights)
data$hotels <- as.numeric(data$hotels)
data$hotrooms <- as.numeric(data$hotrooms)
data$receipt <- as.numeric(data$receipt)
data$ovnarriv <- as.numeric(data$ovnarriv)
data$dayvisit <- as.numeric(data$dayvisit)
data$arram <- as.numeric(data$arram)
data$arreur <- as.numeric(data$arreur)
data$arraus <- as.numeric(data$arraus)

str(data)
head(data)

not_all_na <- function(x) any(!is.na(x))
data_with_nas <- data %>% select(where(not_all_na))
head(data_with_nas)

not_any_na <- function(x) all(!is.na(x))
data_with_no_na <- data %>% select(where(not_any_na))
head(data_with_no_na)

any(is.na(data)) 

cat("The country variable has", sum(is.na(data$country)), "Missing values.\n")
cat("The year variable has", sum(is.na(data$year)), "Missing values.\n")
cat("The pop variable has", sum(is.na(data$pop)), "Missing values.\n")
cat("The areakm2 variable has", sum(is.na(data$areakm2)), "Missing values.\n")
cat("The gdpnom variable has", sum(is.na(data$gdpnom)), "Missing values.\n\n")
cat("The flights variable has", sum(is.na(data$flights)), "Missing values.\n")
cat("The hotels variable has", sum(is.na(data$hotels)), "Missing values.\n")

cat("The hotrooms variable has", sum(is.na(data$hotrooms)), "Missing values.\n\n")
cat("The receipt variable has", sum(is.na(data$receipt)), "Missing values.\n")
cat("The ovnarriv variable has", sum(is.na(data$ovnarriv)), "Missing values.\n\n")
cat("The dayvisit variable has", sum(is.na(data$dayvisit)), "Missing values.\n")
cat("The arram variable has", sum(is.na(data$arram)), "Missing values.\n")
cat("The arreur variable has", sum(is.na(data$arreur)), "Missing values.\n")
cat("The arraus variable has", sum(is.na(data$arraus)), "Missing values.\n")


#-------------------------------------------------------------------------------
# Feature selection

data.1 <- subset(data, select=-c(flights,hotels,hotrooms,dayvisit,arram,arreur,arraus))
head(data.1)
dim(data.1)

#-------------------------------------------------------------------------------
# Data analysis
data.1 <- data.1 %>% group_by(country) %>% mutate(pop = ifelse(is.na(pop), mean(pop, na.rm = TRUE), pop), pop = as.numeric(format(round(pop , 0))))

data.1 <- data.1 %>% group_by(country) %>% mutate(gdpnom = ifelse(is.na(gdpnom), mean(gdpnom, na.rm = TRUE), gdpnom), gdpnom = as.numeric(format(round(gdpnom, 0))))

data.1 <- data.1 %>% group_by(country) %>% mutate(receipt = ifelse(is.na(receipt), mean(receipt, na.rm = TRUE), receipt), receipt = as.numeric(format(round(receipt, 0))))

data.1 <- data.1 %>% group_by(country) %>% mutate(ovnarriv = ifelse(is.na(ovnarriv), mean(ovnarriv, na.rm = TRUE), ovnarriv), ovnarriv = as.numeric(format(round(ovnarriv, 0))))

head(data.1)


cat("The country variable has", sum(is.na(data.1$country)), "Missing values.\n")
cat("The year variable has", sum(is.na(data.1$year)), "Missing values.\n")
cat("The pop variable has", sum(is.na(data.1$pop)), "Missing values.\n")
cat("The areakm2 variable has", sum(is.na(data.1$areakm2)), "Missing values.\n\n")
cat("The gdpnom variable has", sum(is.na(data.1$gdpnom)), "Missing values.\n")

cat("The receipt variable has", sum(is.na(data.1$receipt)), "Missing values.\n")

cat("The ovnarriv variable has", sum(is.na(data.1$ovnarriv)), "Missing values.\n")

names(data.1)

# create new dataset without missing data
#clean_data.1 <- na.omit(data.1)
#write.table(clean_data.1,file='clean_data.1_islands.csv',row.names=F,sep = ",")

# create new dataset without missing data
data.1 <- na.omit(data.1)
any(is.na(data.1)) 

data.2 <- data.1 %>%
  group_by(country) %>%
  summarize(pop = mean(pop),pop = as.numeric(format(round(pop , 0))),
            areakm2 = mean(areakm2),
            gdpnom = mean(gdpnom),gdpnom = as.numeric(format(round(gdpnom , 0))),
            receipt = mean(receipt),receipt = as.numeric(format(round(receipt , 0))),
            ovnarriv = mean(ovnarriv),ovnarriv  = as.numeric(format(round(ovnarriv  ,0))),
            pop_density  = pop/areakm2, pop_density =as.numeric(format(round(pop_density ,0))),
            GDP_per_capita = gdpnom/pop,GDP_per_capita =as.numeric(format(round(GDP_per_capita ,0))))

data.2$countrynames <- c('Mauritius','Seychelles','Antigua & Barbuda','Grenada','Bahrain','Barbados','Bermuda','Cape Verde','Comoros','Dominica','Kiribati','Maldives','Malta','Marshall Islands','Fed Micronesia','Samoa','Sao Tome & Principe','St Kitts and Nevis','St Lucia','St Vincent & Grenadines','Tonga','Singapore','Trinidad and Tobago','Solomon Islands')
head(data.2)

cat("The country variable has", sum(is.na(data.2$country)), "Missing values.\n")

cat("The pop variable has", sum(is.na(data.2$pop)), "Missing values.\n")
cat("The areakm2 variable has", sum(is.na(data.2$areakm2)), "Missing values.\n\n")
cat("The gdpnom variable has", sum(is.na(data.2$gdpnom)), "Missing values.\n")

cat("The receipt variable has", sum(is.na(data.2$receipt)), "Missing values.\n")

cat("The ovnarriv variable has", sum(is.na(data.2$ovnarriv)), "Missing values.\n")

cat("The pop_density variable has", sum(is.na(data.2$pop_density)), "Missing values.\n")

cat("The GDP_per_capita variable has", sum(is.na(data.2$GDP_per_capita)), "Missing values.\n")

# create new dataset without missing data
#clean_data.2 <- na.omit(data.2)
#write.table(clean_data.2,file='clean_data.2_islands.csv',row.names=F,sep = ",")

#-------------------------------------------------------------------------------
# Interpreting results

#-----------------plot 1
ggplot(data.2, aes(x=reorder(countrynames,  pop), y= pop)) +
  geom_bar(stat="identity", width=0.7,fill="black")+coord_flip()+
  geom_text(aes(label=pop), hjust=-0.2, size=3.5)+
  theme_minimal()+ylim(0, 5000000)+
  xlab("Country's names")+ylab("Population of a country")+
  ggtitle("Bar plot of countries Vs Population of a country")+
  theme_minimal()

#-----------------plot 2
ggplot(data.2, aes(x=reorder(countrynames,  areakm2 ), y= areakm2 )) +
  geom_bar(stat="identity", width=0.7,fill="blue")+coord_flip()+
  geom_text(aes(label=areakm2), hjust=-0.2, size=3.5)+
  theme_minimal()+ylim(0, 30000)+
  xlab("Country's names")+ylab("Area of a country in  km^2")+
  ggtitle("Bar plot of countries Vs their are in km^2")+
  theme_minimal()

#-----------------plot 3
ggplot(data.2, aes(x=reorder(countrynames,  pop_density), y= pop_density)) +
  geom_bar(stat="identity", width=0.7,fill="cyan")+coord_flip()+
  geom_text(aes(label=pop_density), hjust=-0.2, size=3.5)+
  theme_minimal()+ylim(0, 7000)+
  xlab("Country's names")+ylab("Population Density")+
  ggtitle("Bar plot of countries Vs Population Density")+
  theme_minimal()

#-------------------plot 4
ggplot(data.2, aes(x=reorder(countrynames, GDP_per_capita), y=GDP_per_capita)) +
  geom_bar(stat="identity", width=0.7, color="red", fill="red")+coord_flip()+
  geom_text(aes(label=GDP_per_capita), hjust=-.2, size=3.5)+
  theme_minimal()+ylim(0, 90000)+
  xlab("Country names")+ylab("GDP per capita")+
  ggtitle("Bar plot of countries Vs their GDP per capita")+theme_minimal()

#-----------------plot 5
ggplot(data.2, aes(x=reorder(countrynames, receipt), y= receipt)) +
  geom_bar(stat="identity", width=0.7,fill="yellow")+coord_flip()+
  geom_text(aes(label=receipt), hjust=-0.2, size=3.5)+
  theme_minimal()+ylim(0, 9600000000)+
  xlab("Country names")+ylab("Tourism receipts")+
  ggtitle("Bar plot of countries Vs Tourism receipts\n")+
  theme_minimal()

max(data.2$receipt)

#-----------------plot 6
ggplot(data.2, aes(x=reorder(countrynames,  ovnarriv), y= ovnarriv)) +
  geom_bar(stat="identity", width=0.7,fill="green")+coord_flip()+
  geom_text(aes(label=ovnarriv), hjust=-0.2, size=3.5)+
  theme_minimal()+ylim(0, 8000000)+
  xlab("Country's names")+ylab("Overnight arrival")+
  ggtitle("Bar plot of countries Vs Overnight arrival")+
  theme_minimal()

#===============================================================================
                             # The end
#===============================================================================