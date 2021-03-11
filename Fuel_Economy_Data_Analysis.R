library(tidyverse) 
library(data.table)
library(rstudioapi)
library(recipes)
library(caret)
library(skimr)
library(stringr)
library(purrr)
library(inspectdf)
library(mice)
library(dplyr)
library(graphics)
library(Hmisc)
library(glue)
library(highcharter)
library(plotly)
library(h2o) 


library(ggplot2)
dataset <- mpg

#Number 1
dataset %>% skim()

dataset %>% View()

dataset <- dataset %>% as.data.frame()

#Number 2
df.num <- dataset %>% select_if(is.numeric) %>% select(cty,everything()) 
df.num %>% names() %>% unique() %>% length()

df.chr <- dataset %>% select_if(is.character)
df.chr %>% names() %>% unique() %>% length()

dataset %>% names() %>% unique() %>% length()

#Outliers
num_vars <- df.num %>% select(-cty) %>% names()

for_vars <- c()
for (b in length(num_vars)) {
  Outvals <- boxplot(df.num[[num_vars[b]]],plot = F)$out
  if(length(Outvals)>0){
    for_vars[b] <- num_vars[b]
  }
    
}

for_vars <- for_vars %>% na.omit() %>% as.matrix() %>% .[1,] %>% as.character() 
#For_vars is equal to "hwy" then no need for a loop 

Outvals <- boxplot(df.num[["hwy"]],plot = F)$out
mean <- mean(df.num$hwy,na.rm=T)

boxplot(df.num[["hwy"]])
#All outliers are higher than mean according to boxplot
o <- ifelse(Outvals>mean,Outvals,NA) %>% na.omit()   
val <- quantile(df.num$hwy,0.75,na.rm = T) + 1.5*IQR(df.num$hwy,na.rm = T)  

df.num[which(df.num$hwy %in% o),"hwy"] <- val


#One Hote Encoding

df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
  predict(newdata = df.chr) %>% 
  as.data.frame()

names(df.chr) <- df.chr %>% names() %>%  gsub("\\)","",.) %>% gsub("\\(","_",.) %>% gsub(" ","_",.)
names(df.chr)

df <- cbind(df.chr,df.num) %>%
  select(cty,everything()) %>% as.data.frame()

#Multicolleniarity
target <- 'cty'
variables <- df %>% select(year,cyl,displ) %>% names()

f <- as.formula(paste(target,paste(variables,collapse = " + "),sep = " ~ "))
glm <- glm(f,data = df)

glm %>% summary()

glm %>% vif() %>% length()

#VIF
library(faraway)
while (glm %>% vif() %>% sort(decreasing = TRUE) %>%  .[1] >= 1.5) {
  afterVIF <- glm %>% vif %>% sort(decreasing = TRUE) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f,data = df)
  
}

glm %>% vif %>% length()
glm %>% summary()

variables <- glm %>% vif %>% sort(decreasing = TRUE) %>% names() 

#Standardize and normalize

df %>% glimpse()

df[,variables]
df[,variables] <- df[,variables] %>% scale() %>% as.data.frame()

df[,variables] %>% skim()


#Number 3
#Modeling
library(rJava)

Sys.setenv(JAVA_HOME= "C:\\Program Files\\Java\\jre1.8.0_271")
Sys.getenv("JAVA_HOME")



h2o.init(nthreads = -1, max_mem_size = '2g', ip = "127.0.0.1", port = 54321)

h2o_data <- df[,c("cty","year","cyl")] %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'cty'
variables <- df %>% select(year,cyl) %>% names()

#Fitting model
model <- h2o.glm(
  x = variables, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,  
  lambda = 0,compute_p_values = T
  
)

#model %>% View()

model@model$coefficients_table %>% 
  as.data.frame() %>% 
  select(names,p_value) %>% 
  mutate(p_value = round(p_value,3)) %>% 
  .[-1,] %>% 
  arrange(desc(p_value))


while(model@model$coefficients_table %>%
      as.data.frame() %>%
      select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] > 0.05) {
  model@model$coefficients_table %>%
    as.data.frame() %>%
    select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  variables <- variables[variables!=v]
  
  train_h2o <- train %>% as.data.frame() %>% select(target,variables) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>% select(target,variables) %>% as.h2o()
  
  model <- h2o.glm(
    x = variables, 
    y = target,
    training_frame = train,
    validation_frame = test,
    nfolds = 10, seed = 123,
    lambda = 0, compute_p_values = T)
}

model@model$coefficients_table %>%
  as.data.frame() %>%
  select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))


model@model$coefficients_table %>% select(names,coefficients,standardized_coefficients)



