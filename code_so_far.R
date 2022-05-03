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

## Create a first model
cdc_risk <- function(x, base_risk = 0.00003) {
  multip <- rep(7900, nrow(x))
  multip[which(x$Age < 84.5)] <- 2800
  multip[which(x$Age < 74.5)] <- 1100
  multip[which(x$Age < 64.5)] <- 400
  multip[which(x$Age < 49.5)] <- 130
  multip[which(x$Age < 39.5)] <- 45
  multip[which(x$Age < 29.5)] <- 15
  multip[which(x$Age < 17.5)] <- 1
  multip[which(x$Age < 4.5)]  <- 2
  multip * base_risk
}

## Wrap the model using DALEX
library(DALEX)
model_cdc <-  DALEX::explain(cdc_risk,
                             predict_function = function(m, x) m(x),
                             data  = covid_summer,
                             y     = covid_summer$Death == "Yes",
                             type  = "classification",
                             label = "CDC")

# Performance analysis
mp_cdc <- model_performance(model_cdc, cutoff = 0.1)

# Create tree model
library(partykit)
tree <- ctree(Death ~., covid_spring, control = ctree_control(alpha = 0.0001))

# wrap tree
model_tree <-  DALEX::explain(tree,
                              predict_function = function(m, x) predict(m, x, type = "prob")[,2],   
                              data = covid_summer[,-8],
                              y = covid_summer$Death == "Yes",
                              type = "classification",
                              label = "Tree",
                              verbose = FALSE)

# tree performance
mp_tree <- model_performance(model_tree, cutoff = 0.1)

