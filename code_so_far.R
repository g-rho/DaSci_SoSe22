set.seed(1313)
covid_spring <- read.table("https://raw.githubusercontent.com/g-rho/DaSci_SoSe22/master/Daten/covid_spring.csv", sep =";", header = TRUE, stringsAsFactors = TRUE)
covid_summer <- read.table("https://raw.githubusercontent.com/g-rho/DaSci_SoSe22/master/Daten/covid_summer.csv", sep =";", header = TRUE, stringsAsFactors = TRUE)


## Transform the data
covid_spring <- covid_spring[,c("Gender", "Age", "Cardiovascular.Diseases", "Diabetes",
                                "Neurological.Diseases", "Kidney.Diseases", "Cancer",
                                "Death")]

covid_summer <- covid_summer[,c("Gender", "Age", "Cardiovascular.Diseases", "Diabetes",
                                "Neurological.Diseases", "Kidney.Diseases", "Cancer",
                                "Death")]

