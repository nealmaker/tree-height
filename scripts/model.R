library("tidyverse")
library("caret")
library("Rborist")

# load data from GitHub
temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia-with-ht.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia_with_ht %>%
  filter(!is.na(ht_s)) %>% 
  select(ht_s, spp, dbh_s, cr_s, ba_s, bal_s, 
         forest_type_s, lat, lon, slope, aspect, 
         crown_class_s, stocking_s, site_class,
         landscape)
  

# test set is 20% of full dataset
test_size <- .2

set.seed(10)
index <- createDataPartition(nf_fia$ht_s, times = 1, p = test_size, list = FALSE)

train <- nf_fia[-index,]
test <- nf_fia[index,]


#####################################################################
# Preprocess data
#####################################################################

preproc_ht <- preProcess(train[,-1], method = c("center", "scale", "YeoJohnson"))
  
train_tran <- predict(preproc_ht, train)
test_tran <- predict(preproc_ht, test)

x <- train_tran[,-1]
y <- train_tran[,1]


#####################################################################
# Train model
#####################################################################

# calculates RMSE:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


set.seed(1)
ht_model <- train(x, y,
                  method = "ranger",
                  num.trees = 200,
                  importance = 'impurity',
                  tuneGrid = data.frame(mtry = seq(8, 14, by = 2),
                                        splitrule = rep("variance", 4),
                                        min.node.size = rep(5, 4)))


#####################################################################
# Results 
#####################################################################

ht_model$results

plot(ht_model)

varImp(ht_model, scale = F)


#####################################################################
# Prediction 
#####################################################################

df <- data.frame(spp = factor(rep("white pine", 4), levels = levels(train$spp)),
                 dbh_s = rep(12, 4),
                 cr_s = seq(10, 70, 20),
                 ba_s = rep(150, 4),
                 bal_s = seq(225, 0, -75),
                 forest_type_s = factor(rep("White pine", 4),
                                        levels = levels(train$forest_type_s)),
                 lat = rep(44.7, 4),
                 lon = rep(-73.6, 4),
                 slope = rep(0, 4),
                 aspect = rep(0, 4),
                 crown_class_s = rep(2, 4),
                 stocking_s = rep(2, 4),
                 site_class = rep(5, 4),
                 landscape = factor(rep("rolling uplands", 4),
                                    levels = levels(train$landscape)))

df_trans <- predict(preproc_ht, newdata = df)

df_pred <- df %>% 
  mutate(y_hat = predict(ht_model, newdata = df_trans))


#####################################################################
# Save
#####################################################################

# STOP! Too big to fit on GitHub
# save(ht_model, file = "rda/ht_model.rda")