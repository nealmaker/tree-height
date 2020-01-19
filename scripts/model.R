library("tidyverse")
library("caret")
library("Rborist")

# load data from GitHub
temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(!is.na(ht_s)) %>% 
  select(ht_s, spp, dbh_s, cr_s, crown_class_s, tree_class_s,
         ba_s, bal_s, forest_type_s, stocking_s, landscape, 
         site_class, slope, aspect, lat, lon, elev, plot) %>% 
  rename(dbh = dbh_s, cr = cr_s, crown_class = crown_class_s,
         tree_class = tree_class_s, ba = ba_s, bal = bal_s,
         forest_type = forest_type_s, stocking = stocking_s)
  
unlink(temp)

# test set is 20% of full dataset
test_size <- .2

# define test set based on plots (to make it truely independent)
set.seed(10)
test_plots <- sample(unique(nf_fia$plot), 
                     size = round(test_size*length(unique(nf_fia$plot))), 
                     replace = FALSE)

index <- which(nf_fia$plot %in% test_plots)
train <- nf_fia[-index,]
test <- nf_fia[index,]

x <- select(train, -plot, -ht_s)
y <- train[,1]


#####################################################################
# Train model
#####################################################################

set.seed(1)
ht_model_full <- train(x, y,
                  method = "ranger",
                  preProcess = c("center", "scale", "YeoJohnson"),
                  num.trees = 200,
                  importance = 'impurity',
                  tuneGrid = data.frame(mtry = seq(2, 8, by = 2),
                                        splitrule = rep("variance", 4),
                                        min.node.size = rep(5, 4)))


#####################################################################
# Results 
#####################################################################

ht_model_full$results

plot(ht_model_full)

varImp(ht_model_full, scale = F)


#####################################################################
# Prediction 
#####################################################################

df <- data.frame(spp = factor(rep("white pine", 4), levels = levels(train$spp)),
                 dbh_s = rep(12, 4),
                 cr_s = seq(10, 70, 20),
                 crown_class_s = rep(2, 4),
                 tree_class_s = rep(2, 4),
                 ba_s = rep(150, 4),
                 bal_s = seq(225, 0, -75),
                 forest_type_s = factor(rep("White pine", 4),
                                        levels = levels(train$forest_type_s)),
                 stocking_s = rep(2, 4),
                 landscape = factor(rep("rolling uplands", 4),
                                    levels = levels(train$landscape)),
                 site_class = rep(5, 4),
                 slope = rep(0, 4),
                 aspect = rep(0, 4),
                 lat = rep(44.7, 4),
                 lon = rep(-73.6, 4),
                 elev = rep(1400, 4))

df_pred <- df %>% 
  mutate(y_hat = predict(ht_model_full, newdata = df))


#####################################################################
# Train operational model
#####################################################################

set.seed(1)
ht_model_op <- train(x[,c(1:3, 6:8, 11, 14:15)], y,
                     method = "ranger",
                     preProcess = c("center", "scale", "YeoJohnson"),
                     num.trees = 200,
                     importance = 'impurity',
                     tuneGrid = data.frame(mtry = seq(2, 8, by = 2),
                                           splitrule = rep("variance", 4),
                                           min.node.size = rep(5, 4)))


#####################################################################
# Results 
#####################################################################

ht_model_op$results

plot(ht_model_op)

varImp(ht_model_op, scale = F)


#####################################################################
# Prediction op
#####################################################################

df_op <- data.frame(spp = factor(c(rep("white pine", 4), rep("hemlock", 4), 
                                rep("soft maple", 4)),
                              levels = levels(train$spp)),
                 dbh_s = rep(12, 12),
                 cr_s = rep(seq(10, 70, 20), 3),
                 ba_s = rep(150, 12),
                 bal_s = rep(seq(225, 0, -75), 3),
                 forest_type_s = factor(rep("Northern hardwood", 12),
                                        levels = levels(train$forest_type_s)),
                 lat = rep(44.7, 12),
                 lon = rep(-73.6, 12),
                 site_class = rep(5, 12))

df_pred_op <- df %>% 
  mutate(y_hat = predict(ht_model_op, newdata = df_op))

#####################################################################
# Save
#####################################################################

# STOP! Too big to fit on GitHub
# save(ht_model_full, file = "../big-rdas/ht-model-full.rda")
# save(ht_model_op, file = "../big-rdas/ht-model-op.rda")

