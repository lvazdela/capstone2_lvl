#____________________________________________________
# title: 'Edx Capstone: Classification model Project'
# author: "Luis G. Vazquez de Lara Cisneros."
# date: "02/12/2020"
#____________________________________________________


# DOWNLOAD AND/OR OPEN LIBRARIES ------------------------------------------

#Libraries needded:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org", dependencies = TRUE)


# DOWNLOAD THE NECESSARY FILES --------------------------------------------


# Download the files
urlcscovid <- 'https://raw.githubusercontent.com/lvazdela/capstone2_lvl/main/cscovid.csv'
dbcovid <- read.csv(urlcscovid)

urlengvar <- 'https://raw.githubusercontent.com/lvazdela/capstone2_lvl/main/other-files/engvar2.txt'
engvar <- read_lines(urlengvar)
urlcodunits <- read_lines('https://raw.githubusercontent.com/lvazdela/capstone2_lvl/main/other-files/engvar3utf8.txt')
codunits <- read_lines(urlcodunits)

urlengvarnum <- 'https://raw.githubusercontent.com/lvazdela/capstone2_lvl/main/other-files/engvarnum.txt'
engvarnum <- read_lines(urlengvarnum)

# DATA WRANGLING ----------------------------------------------------------

# dataframe of variables in dbcovid.
nomvar <- dbcovid %>%
  names
rawdb <- tibble(code = nomvar, Description = engvar, cod_units = codunits)

#Manage date columns
dbcovid <- dbcovid %>% mutate(across(starts_with('fecha'), dmy))

#Check errors in numeric variables
nomcadenas <- c('urea', 'creat', 'bun', 'plaq', 'ca', 'aat', 'alat')
patron <- '([^\\d\\.])|(\\.{2,})' #Anything but digits or one decimal point.

fpatron <- function(x){
  x[str_which(x, pattern = patron)]
}

errores <- apply(dbcovid[, nomcadenas],2,fpatron )
dberrores <- data.frame(mistakes = unlist(errores))

#Check errors in numeric variables
nomcadenas <- c('urea', 'creat', 'bun', 'plaq', 'ca', 'aat', 'alat')
patron <- '([^\\d\\.])|(\\.{2,})' #Anything but digits or one decimal point.

fpatron <- function(x){
  x[str_which(x, pattern = patron)]
}

errores <- apply(dbcovid[, nomcadenas],2,fpatron )

# Catch-all function to detect errors in numeric variables
farreglar <- function(x){
  x = str_trim(x)
  x = case_when(
    str_detect(x, '\\s') ~ str_replace_all(x, '\\s', ''),
    str_detect(x, '[:alpha:]') ~ str_replace_all(x, '[:alpha:]',''),
    str_detect(x, ',|\\.{2,}') ~ str_replace_all(x,',|\\.{2,}', '.'),
    TRUE ~ x
  )
}

#Fix errors in numeric variables
dbcovid <- dbcovid %>% mutate(across(all_of(nomcadenas), farreglar))
#Check if it worked
arregerrores <- apply(dbcovid[, nomcadenas],2,fpatron )
arregerrores # no mistakes

# Transform numeric variables to numeric type 
dbcovid <- dbcovid %>% mutate(across(all_of(nomcadenas), as.numeric))

#Create new variables with dates, and eliminate the date variables.
dbcovid <- dbcovid %>%
  mutate(bmi = peso/talla^2, 
         dayshosp = as.numeric(fechalta - fechahosp),
         duration = as.numeric(fechalta - fechainisint),
         daysdelay = as.numeric(fechahosp - fechainisint),
         obesity = ifelse(bmi >= 30, 1, 2)) %>%
  select(-starts_with('fecha'))

# Change codification of dichotomous categorical variables.
dicot <- c('motivoegre','has', 'tabaquismo', 'dm', 'renal', 'autoinmunidad',
           'ing_disnea', 'obesity')
dbcovid <- dbcovid %>%
  mutate(across(all_of(dicot), function(x) ifelse(x == 2, 0, 1)))

# Transform categorical variables to factors.
nomcateg <- c('motivoegre','sexo', 'ocupacion', 'nivsoc', dicot[-1])
dbcovid <- dbcovid %>%
  mutate(across(all_of(nomcateg), as.factor))

# DATA EXPLORATION --------------------------------------------------------

#Function to process categorical variables
creatortabcat <- function(nomvar, etiq){
  dbcovid %>%
    select(motivoegre, all_of(nomvar)) %>%
    mutate(across(all_of(nomvar), function(x) factor(x, labels = etiq))) %>%
    mutate(motivoegre = factor(motivoegre, levels = c(0,1), labels = c('Alive', 'Dead'))) %>%
    pivot_longer(all_of(nomvar), names_to = 'apps', values_to = 'valor') %>%
    group_by(motivoegre, apps, valor) %>%
    summarise(n = n()) %>%
    mutate(frec = round(n/sum(n)*100, 2),
           total = sum(n)) %>%
    pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
    mutate(total = n_Alive + n_Dead,
           pordef = round(n_Dead/total*100, 1),
           pormejo = round(n_Alive/total*100, 1),
           muertos = paste0(str_pad(n_Dead,4,side = 'right', pad = ' '), '(', pordef, ')'),
           vivos = paste0(str_pad(n_Alive,4,side = 'right'), '(', pormejo, ')')) %>% 
    select(apps, valor, total, muertos, vivos) %>%
    filter(!is.na(valor)) %>%
    rename(Variable = apps, Value = valor, Total = total, Dead = muertos, Alive = vivos)
}

#data frames of summaries of categorical variables:
dbsex <- creatortabcat('sexo', c('Female', 'Male'))

englaboccup <- c('Health care worker', 'Office job','Outdoor work',
                 'Work in public area', 'Work at home', 'Unemployed')
dboccup <- creatortabcat('ocupacion', englaboccup)

dbnivsoc <- creatortabcat('nivsoc', c('Low or medium-low', 'Medium-high or high'))

dbdicot <- creatortabcat(dicot[-1], c('No', 'Yes'))

#Join the dataframes:
dbvarcateg <- bind_rows(dbsex, dboccup, dbnivsoc, dbdicot)

#function to write the p value of chi-squared test in the database  
fchipavarcateg <- function(x){
  p <- round(chisq.test(table(x, dbcovid$motivoegre))$p.value, 3)
  if (!is.factor(x)){
    x <- as.factor(x)
  } 
  blancos <- length(levels(x)) -1
  if (p < 0.001){
    p <- '< 0.001'
  }
  cad <- c(p, rep('', blancos))
  return(cad)
}
varcateg <- unique(dbvarcateg$Variable)

listachis <- unlist(apply(dbcovid[,varcateg],2, fchipavarcateg))
numlistachis <- apply(dbcovid[,varcateg], 2, function(x){chisq.test(table(x, dbcovid$motivoegre))$p.value} )


#Add listachisfin to the database and translate variables to English
engvarord <- c('Autoimmunity', 'Diabetes', 'Hypertension', 'Short of breath', 'Socioeconomic level',
               'Obesity', 'Occupation', 'Renal disease', 'Gender', 'Smoking')
dbvarcateg <- dbvarcateg %>%
  ungroup %>%
  mutate(p = listachis,
         Variable = factor(Variable, labels = engvarord))

varnum <- dbcovid %>%
  select(where(is.numeric)) %>%
  names


dbtranslator <- data.frame(spa = varnum, eng = engvarnum) %>%
  arrange(spa)

listapesnum <- apply(dbcovid[, varnum[-1]], 2, function(x) t.test(x ~ dbcovid$motivoegre)$p.value)
umann <- wilcox.test(dbcovid$escolaridad ~ dbcovid$motivoegre)$p.value
listapesnum <- round(c(umann, listapesnum), 3)
listapesnum2 <- ifelse(listapesnum < 0.001, '< 0.001', listapesnum)
#list of numeric variables in the model

dftes2 <- data.frame(Variable = varnum, p = listapesnum2)

dbvarnum <- dbcovid %>%
  select(motivoegre, all_of(varnum)) %>%
  mutate(motivoegre = factor(motivoegre, labels = c('Alive', 'Dead'))) %>%
  group_by(motivoegre) %>%
  summarise(across(everything(), list(
    validos = ~ sum(!is.na(.x)),
    prom = ~ round(mean(.x, na.rm = TRUE),2),
    desvest = ~ round(sd(.x, na.rm = TRUE),2)
  )))%>%
  pivot_longer(cols = -motivoegre,
               names_sep = '_',
               names_to = c('Variable', '.value')) %>%
  left_join(dftes2, by = 'Variable')%>%
  unite(meansd, c(prom, desvest), sep = ' ± ') %>%
  select(Variable, validos, motivoegre, meansd, p  )%>%
  mutate(Variable = factor(Variable, labels = dbtranslator$eng)) %>%
  arrange(p, Variable)

# DATA PREPROCESSING ------------------------------------------------------

finalvarcateg <- varcateg[which(numlistachis < 0.051)]
finalvarnum <- varnum[which(listapesnum < 0.051)]
finalvarnum <- finalvarnum[1:24]
dbcovid <- dbcovid %>%
  select(all_of(c('motivoegre', finalvarcateg, finalvarnum))) %>%
  mutate(motivoegre = factor(motivoegre, labels = c('Alive', 'Dead')),
         sexo = factor(sexo, labels = c('Female', 'Male')),
         nivsoc = factor(nivsoc, labels = c('low', 'High')),
         dm = factor(dm, labels = c('No', 'Yes')),
         has = factor(has, labels = c('No', 'Yes')),
         tabaquismo = factor(tabaquismo, labels = c('No', 'Yes')),
         ing_disnea = factor(ing_disnea, labels = c('No', 'Yes')))

nearZeroVar(dbcovid)

#Impute with the median
preprocmd <- preProcess(dbcovid, method = 'medianImpute')
dbcovidmd <- predict(preprocmd, dbcovid )
sum(is.na(dbcovidmd)) # Show the missings in the categorical variables
dbcovidmd <- na.omit(dbcovidmd)
sum(is.na(dbcovidmd)) #check we have no missing data

#Imputation via bagging (if I leave the factor variables, it throws an error).
set.seed(271220)
preprocbag <- dbcovid %>%
  select(where(is.numeric)) %>%
  preProcess(., method = 'bagImpute')

dbcovidbag <- dbcovid %>%
  select(where(is.numeric)) %>%
  predict(preprocbag, .)

dbcovidbag <- dbcovid %>%
  select(where(is.factor)) %>%
  bind_cols(dbcovidbag) %>%
  na.omit
sum(is.na(dbcovidbag)) # check we have no missing data

# Check missingness in the database.
dbnas <- dbcovid %>%
  select(all_of(c('motivoegre', finalvarcateg, finalvarnum))) %>%
  summarise(across(everything(),  ~sum(is.na(.x)) )) %>%
  t(.) %>%
  as.data.frame(.) %>%
  rename(totalnas = V1) %>%
  mutate(porcentaje = round(totalnas/dim(dbcovid)[1]*100,1),
         len = dim(dbcovid)[1],
         variable = row.names(.),
         validos = len - totalnas) %>%
  select(variable, validos, totalnas, porcentaje, len)%>%
  arrange(desc(porcentaje))


# MODEL GENERATION --------------------------------------------------------

# Data partition
set.seed(271220)
indtrainmd <- createDataPartition(y = dbcovidmd$motivoegre, times = 1, p = 0.85, list = FALSE)
train_setmd <- dbcovidmd[indtrainmd,]
test_setmd <- dbcovidmd[-indtrainmd,]

set.seed(271220)
indtrainbag <- createDataPartition(y = dbcovidbag$motivoegre, times = 1, p = 0.85, list = FALSE)
train_setbag <- dbcovidbag[indtrainbag,]
test_setbag <- dbcovidbag[-indtrainbag,]

tr.control <- trainControl(method = 'boot',
                           number = 25)

#GMM 
#Data set imputed with the median
set.seed(271220)
fit_glm <- train(motivoegre ~ .,
                 method = 'glm',
                 data = train_setmd,
                 trControl = tr.control)

model_glm <- predict(fit_glm, newdata = test_setmd, type = 'raw')
cm <- confusionMatrix(data = model_glm, reference = test_setmd$motivoegre)
acc_glmmd <- cm$overall[["Accuracy"]]
acc_glmmdup <- cm$overall[["AccuracyUpper"]]
acc_glmmdlo <-  cm$overall[["AccuracyLower"]]

#Data set imputed with bagging
set.seed(271220)
fit_glm <- train(motivoegre ~ .,
                 method = 'glm',
                 data = train_setbag,
                 trControl = tr.control)

model_glm <- predict(fit_glm, newdata = test_setbag, type = 'raw')
cm <- confusionMatrix(data = model_glm, reference = test_setbag$motivoegre)
acc_glmbag <- cm$overall[["Accuracy"]]
acc_glmbagup <- cm$overall[["AccuracyUpper"]]
acc_glmbaglo <-  cm$overall[["AccuracyLower"]]

varimpglm <- varImp(fit_glm)[["importance"]]
nomvars <- row.names(varimpglm)
varimpglm <- varimpglm %>%
  mutate(varnameglm = nomvars) %>%
  rename(valueglm = Overall) %>%
  select(varnameglm, valueglm) %>%
  arrange(desc(valueglm))

# kNN

#Imputed with median
set.seed(271220)
fit_knn <- train(motivoegre ~ .,
                 method = 'knn',
                 tuneGrid = data.frame(k = seq(5,35, 1)),
                 trControl = tr.control,
                 data = train_setmd)

model_knn <- predict(fit_knn, newdata = test_setmd)
cm <- confusionMatrix(data = model_knn, reference = test_setmd$motivoegre)
acc_knnmd <- cm$overall[["Accuracy"]]
acc_knnmdup <- cm$overall[["AccuracyUpper"]]
acc_knnmdlo <-  cm$overall[["AccuracyLower"]]

#imputed with bagging
set.seed(271220)
fit_knn <- train(motivoegre ~ .,
                 method = 'knn',
                 tuneGrid = data.frame(k = seq(5,35, 1)),
                 trControl = tr.control,
                 data = train_setbag)

model_knn <- predict(fit_knn, newdata = test_setbag)

cm <- confusionMatrix(data = model_knn, reference = test_setbag$motivoegre)
acc_knnbag <- cm$overall[["Accuracy"]]
acc_knnbagup <- cm$overall[["AccuracyUpper"]]
acc_knnbaglo <-  cm$overall[["AccuracyLower"]]

btune_knn <- fit_knn$bestTune

varimpknn <- varImp(fit_knn)$importance
nomvars <- row.names(varimpknn)
varimpknn <- varimpknn %>%
  mutate(varnameknn = nomvars) %>%
  rename(valueknn = Dead) %>%
  select(-Alive) %>%
  select(varnameknn, valueknn) %>%
  arrange(desc(valueknn))

# CART

#Imputed with bagging
set.seed(271220)

fit_rpart <- train(motivoegre ~ ., method = 'rpart',
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                   trControl = tr.control,
                   data = train_setbag)


model_rpart <- predict(fit_rpart, newdata = test_setbag)
cm <- confusionMatrix(data = model_rpart, reference = test_setbag$motivoegre)
acc_rpartbag <- cm$overall[["Accuracy"]]
acc_rpartbagup <- cm$overall[["AccuracyUpper"]]
acc_rpartbaglo <-  cm$overall[["AccuracyLower"]]

#Imputed with the median
set.seed(271220)
fit_rpart <- train(motivoegre ~ ., method = 'rpart',
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                   trControl = tr.control,
                   data = train_setmd)


model_rpart <- predict(fit_rpart, newdata = test_setmd)
cm <- confusionMatrix(data = model_rpart, reference = test_setmd$motivoegre)
acc_rpartmd <- cm$overall[["Accuracy"]]
acc_rpartmdup <- cm$overall[["AccuracyUpper"]]
acc_rpartmdlo <-  cm$overall[["AccuracyLower"]]
btune_rpart <-  fit_rpart$bestTune

varimprpart <- varImp(fit_rpart)$importance
nomvars <- row.names(varimprpart)
varimprpart <- varimprpart %>%
  mutate(varnamerpart = nomvars) %>%
  rename(valuerpart = Overall) %>%
  select(varnamerpart, valuerpart) %>%
  arrange(desc(valuerpart))

control_rf <- trainControl(method = 'boot',
                           number = 25)
grid <- data.frame(mtry = c(1,5,10,25))

# RANDOM FORESTS

#Imputed with bagging
set.seed(271220)
fit_rf <- train(motivoegre ~ .,
                method = 'rf',
                ntree = 150,
                trControl = control_rf,
                tuneGrid = grid,
                data = train_setbag)

model_rf <- predict(fit_rf, newdata = test_setbag)
cm <- confusionMatrix(data = model_rf, reference = test_setbag$motivoegre)
acc_rfbag <- cm$overall[["Accuracy"]]
acc_rfbagup <- cm$overall[["AccuracyUpper"]]
acc_rfbaglo <-  cm$overall[["AccuracyLower"]]

#Imputed with the median
set.seed(271220)
fit_rf <- train(motivoegre ~ .,
                method = 'rf',
                ntree = 150,
                trControl = control_rf,
                tuneGrid = grid,
                data = train_setmd)

model_rf <- predict(fit_rf, newdata = test_setmd)
cm <- confusionMatrix(data = model_rf, reference = test_setmd$motivoegre)
acc_rfmd <- cm$overall[["Accuracy"]]
acc_rfmdup <- cm$overall[["AccuracyUpper"]]
acc_rfmdlo <-  cm$overall[["AccuracyLower"]]


varimprf <- varImp(fit_rf)$importance
nomvars <- row.names(varimprf)
varimprf <- varimprf %>%
  mutate(varnamerf = nomvars) %>%
  rename(valuerf = Overall) %>%
  select(varnamerf, valuerf) %>%
  arrange(desc(valuerf))
btune_rf <- fit_rf$bestTune %>% pull

# Data frame with the accuracies and confidence intervals
accsmd <- c(acc_glmmd, acc_knnmd, acc_rpartmd, acc_rfmd)
accsbag <- c(acc_glmbag,acc_knnbag, acc_rpartbag, acc_rfbag)
accslomd <- c(acc_glmmdlo, acc_knnmdlo, acc_rpartmdlo, acc_rfmdlo)
accslobag <- c(acc_glmbaglo, acc_knnbaglo, acc_rpartbaglo, acc_rfbaglo)
accsupmd <- c(acc_glmmdup, acc_knnmdup, acc_rpartmdup, acc_rfmdup)
accsupbag <- c(acc_glmbagup, acc_knnbagup, acc_rpartbagup, acc_rfbagup )
model <- (c('Generalized linear model', 'k nearest neighbors', 'CART', 'Random forests'))

dfresfin <- tibble(model = model,
                   accsmd = accsmd,
                   accslomd = accslomd,
                   accsupmd = accsupmd,
                   accsbag = accsbag,
                   accslobag = accslobag,
                   accsupbag = accsupbag) %>%
  mutate(across(where(is.numeric), ~ round(.,3))) %>%
  unite(confintmd, all_of(c('accslomd', 'accsupmd')), sep = '-') %>%
  unite(confintbag, all_of(c('accslobag', 'accsupbag')), sep = '-')

# Data frame showing the glm coefficients.
fileconn <- file('glmres.txt')
writeLines (capture.output(summary(fit_glm)), fileconn)
close(fileconn)

dbresglm <- read.table('glmres.txt', skip = 10, nrows = 36, header = FALSE, fill = TRUE) %>%
  select(1:5) %>%
  filter(V1 != '(Intercept)') %>%
  mutate(across(c(2,3,5), ~round(., 4))) %>%
  mutate(V1 =  str_replace_all(V1, '\\_', '\\\\_')) %>%
  arrange(V5) %>%
  slice(1:20)

# plot of k tuninng
plot(fit_knn, xlab = 'Number of neighbors')

#Plot of cp parameter
plot(fit_rpart)

#Plot of the classification tree
plot(fit_rpart$finalModel, margin = 0.1) 
text(fit_rpart$finalModel, cex = 0.75)

#Plots of the random forestes parameters
plot(fit_rf, xlab = 'Number of randomly selected predictors')
plot(fit_rf$finalModel, main = NULL)

# Data frame of the variable importance
dfvarimp <- bind_cols(varimpglm[1:20,], varimpknn[1:20,], varimprpart[1:20,], varimprf[1:20,]) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
