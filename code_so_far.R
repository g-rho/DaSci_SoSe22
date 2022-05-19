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

## Performance analysis
mp_cdc <- model_performance(model_cdc, cutoff = 0.1)


## Create tree model
library(partykit)
tree <- ctree(Death ~., covid_spring, control = ctree_control(alpha = 0.0001))

## wrap tree
model_tree <-  DALEX::explain(tree,
                              predict_function = function(m, x) predict(m, x, type = "prob")[,2],   
                              data = covid_summer[,-8],
                              y = covid_summer$Death == "Yes",
                              type = "classification",
                              label = "Tree",
                              verbose = FALSE)

## tree performance
mp_tree <- model_performance(model_tree, cutoff = 0.1)


## Create random forest model using mlr3

## set up task
library("mlr3")
covid_task <- TaskClassif$new(id = "covid_spring", backend = covid_spring, target = "Death", positive = "Yes")

## set up learner
library("mlr3learners")
covid_ranger <- lrn("classif.ranger", predict_type = "prob", num.trees = 25)

## Train learner
covid_ranger$train(covid_task) # Training!  (..note: this is done on the spring data!)

# Wrap model 
model_ranger <-  explain(covid_ranger,
                         predict_function = function(m,x) predict(m, x, predict_type = "prob")[,1],
                         data = covid_summer[,-8],
                         y = covid_summer$Death == "Yes",
                         type = "classification",
                         label = "Ranger",
                         verbose = FALSE)

## forest performance
mp_ranger <- model_performance(model_ranger)


# hyperparameter tuning
library("mlr3tuning")
library("paradox")
# covid_ranger$param_set
search_space = ps(
  num.trees = p_int(lower = 50, upper = 500),
  max.depth = p_int(lower = 1, upper = 10),
  mtry = p_int(lower = 1, upper = 7),
  minprop = p_dbl(lower = 0.01, upper = 0.1),
  splitrule = p_fct(levels = c("gini", "extratrees"))
)

# Set up the tuner...
tuned_ranger = AutoTuner$new(
  learner    = covid_ranger,
  resampling = rsmp("cv", folds = 5),
  measure    = msr("classif.auc"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 10),
  tuner    = tnr("random_search")
)
# ... and tune parameters...
tuned_ranger$train(covid_task)
# tuned_ranger$tuning_result

#...finally, as always: wrap your model:
model_tuned <-  explain(tuned_ranger,
                        predict_function = function(m,x)
                          m$predict_newdata(newdata = x)$prob[,1],
                        data = covid_summer[,-8],
                        y = covid_summer$Death == "Yes",
                        type = "classification",
                        label = "AutoTune",
                        verbose = FALSE)

# performance of tuned forest 
mp_tuned <- model_performance(model_tuned)

# compare performance of all models
# align performances (un summer validation data) of all four models for comparison
rbind(cdc    = mp_cdc$measures,
      tree   = mp_tree$measures,
      ranger = mp_ranger$measures,
      tuned  = mp_tuned$measures)

plot(mp_ranger, mp_tree, mp_cdc, mp_tuned, geom = "roc")


