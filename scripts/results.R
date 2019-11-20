library("pdp")
library("ICEbox")
library("gridExtra")

# calculates RMSE:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

pred <- test %>% 
  mutate(y_hat = predict(ht_model_op, newdata = test),
         err = y_hat - ht_s)

RMSE(pred$ht_s, pred$y_hat)

postResample(pred = pred$y_hat, obs = pred$ht_s)

# Errors by spp
pred %>% 
  group_by(spp) %>% 
  mutate(err_mean = mean(err)) %>% 
  ungroup() %>% 
  ggplot(aes(err)) +
  geom_density(fill = "dark gray") +
  geom_vline(xintercept = 0, col = "#386cb0", size = 1) +
  geom_vline(aes(xintercept = err_mean), col = "#bf5b17", size = 1) +
  facet_wrap(~ spp, ncol = 4) +
  theme(text = element_text(family = "Perpetua"))


# Fake partial dependence on cr and bal
pred %>% 
  filter(bal_s < 300) %>% 
  mutate(bal_bin = cut(bal_s, c(0,30, 60, 90, 300), include.lowest = T)) %>% 
  ggplot(aes(cr_s, ht_s, color = as.factor(bal_bin))) +
  geom_smooth() +
  labs(title = "Crown Ratio and Height",
       subtitle = "by BAL") +
  scale_x_continuous(name = "crown ratio (%)") +
  scale_y_continuous(name = "height (ft)") +
  scale_color_brewer(type = "div",
                     name = "BAL class",
                     direction = -1)


# Real partial dependence plots ----------
# *using transformed predictors!!!

# use subset of training data to save time
set.seed(1)
pred_grid <- train[sample(nrow(train), 500, replace = F), c(1:9, 14)]

# ICE plots to look for interactions ----------
# ICEbox can sample from training data to save time, but it's hard to make pretty graphs
# (have to learn new graphing engine?)


ice_lat <- ice(ht_model_op,
               X = pred_grid[,-1],
               y = pred_grid[,1],
               predictor = "lat",
               num_grid_pts = 30)

latICE <- plot(ice_lat, 
               centered = F, 
               rug_quantile = seq(from = 0, to = 1, by = 0.1))
latICE_c <- plot(ice_lat, 
                 centered = T, 
                 rug_quantile = seq(from = 0, to = 1, by = 0.1))


# ICE plots w/ pdp look nice, but slow b/c can't sample training data
# must use work around to sample after partial() for display

# ICE plots from pdp ICe objects
iceit <- function(pd){
  pd_sample <- pd %>% 
    filter(yhat.id %in% sample(unique(pd$yhat.id), 500)) %>% 
    arrange(.[[1]]) %>% 
    group_by(as.factor(yhat.id)) %>% 
    mutate(yhat.c = yhat - yhat[1])
  
  p1 <- pd_sample %>% 
    ggplot() +
    geom_line(alpha = .3,
              aes(x = pd_sample[[1L]], 
                  y = pd_sample[["yhat"]], 
                  group = as.factor(yhat.id))) +
    geom_smooth(aes(x = pd_sample[[1L]], 
                    y = pd_sample[["yhat"]]),
                size = 1.5) +
    xlab(names(pd_sample[1])) +
    ylab("yhat") 
  
  p2 <- pd_sample %>% 
    ggplot() +
    geom_line(alpha = .2,
              aes(x = pd_sample[[1L]], 
                  y = pd_sample[["yhat.c"]], 
                  group = as.factor(yhat.id))) +
    geom_smooth(aes(x = pd_sample[[1L]], 
                    y = pd_sample[["yhat.c"]]),
                size = 1.5) +
    xlab(names(pd_sample[1])) +
    ylab("yhat centered") 
  
  grid.arrange(p1, p2, ncol = 2)
}

pd_lat <- pdp::partial(ht_model_op, 
                       pred.var = "lat", 
                       grid.resolution = 30,
                       ice = T,
                       progress = "text")

pd_lon <- pdp::partial(ht_model_op, 
                       pred.var = "lon", 
                       grid.resolution = 30,
                       ice = T,
                       progress = "text")

pd_cr <- pdp::partial(ht_model_op, 
                       pred.var = "cr_s", 
                       grid.resolution = 30,
                       ice = T,
                       progress = "text")

pd_ba <- pdp::partial(ht_model_op, 
                      pred.var = "ba_s", 
                      grid.resolution = 30,
                      ice = T,
                      progress = "text")

pd_bal <- pdp::partial(ht_model_op, 
                      pred.var = "bal_s", 
                      grid.resolution = 30,
                      ice = T,
                      progress = "text")

pd_site <- pdp::partial(ht_model_op, 
                      pred.var = "site_class", 
                      grid.resolution = 5,
                      ice = T,
                      progress = "text")

pd_forest <- pdp::partial(ht_model_op, 
                        pred.var = "forest_type_s", 
                        ice = T,
                        progress = "text")

pd_spp <- pdp::partial(ht_model_op, 
                          pred.var = "spp", 
                          ice = T,
                          progress = "text")

pd_dbh <- pdp::partial(ht_model_op, 
                       pred.var = "dbh_s", 
                       grid.resolution = 30,
                       ice = T,
                       progress = "text")


# Then make pdp's to show actual relationships
pd_cr_bal <- pdp::partial(ht_model_op, 
                          pred.var = c("cr_s", "bal_s"), 
                          grid.resolution = 30,
                          chull = T,
                          progress = "text")

pd_ba_bal <- pdp::partial(ht_model_op, 
                          pred.var = c("ba_s", "bal_s"), 
                          grid.resolution = 30,
                          chull = T,
                          progress = "text")

pd_lon_lat <- pdp::partial(ht_model_op, 
                          pred.var = c("lon", "lat"), 
                          grid.resolution = 30,
                          chull = T,
                          progress = "text")

pd_dbh_cr <- pdp::partial(ht_model_op, 
                           pred.var = c("dbh_s", "cr_s"), 
                           grid.resolution = 30,
                           chull = T,
                           progress = "text")

pd_dbh_bal <- pdp::partial(ht_model_op, 
                          pred.var = c("dbh_s", "bal_s"), 
                          grid.resolution = 30,
                          chull = T,
                          progress = "text")

pd_spp_cr<- pdp::partial(ht_model_op, 
                           pred.var = c("spp", "cr_s"), 
                           grid.resolution = 30,
                           chull = T,
                           progress = "text")

pd_spp_forest<- pdp::partial(ht_model_op, 
                         pred.var = c("spp", "forest_type_s"),
                         progress = "text")


pdp::plotPartial(pd_ba_bal, levelplot = T, 
                 contour = T, zlab = "height",
                 xlab = "ba", ylab = "bal",
                 drape = T, colorkey = TRUE, chull = F,
                 rug = T, 
                 train = pred_grid[,c(5,6)])

pdp::plotPartial(pd_cr_bal, levelplot = T, 
                 contour = T, zlab = "height",
                 xlab = "crown ratio", ylab = "bal",
                 drape = T, colorkey = TRUE, chull = T,
                 rug = T, 
                 train = pred_grid[,c(4,6)])

pdp::plotPartial(pd_dbh_cr, levelplot = F, 
                 contour = T, zlab = "height",
                 xlab = "dbh", ylab = "crown ratio",
                 drape = T, colorkey = TRUE, chull = F,
                 rug = T, 
                 train = pred_grid[,c(3,4)])

pdp::plotPartial(pd_ba_bal, levelplot = F, 
                 contour = T, zlab = "height",
                 xlab = "ba", ylab = "bal",
                 drape = T, colorkey = TRUE, chull = F,
                 rug = T, 
                 train = pred_grid[,c(5,6)])

pdp::plotPartial(pd_dbh_bal, levelplot = T, 
                 contour = T, zlab = "height",
                 xlab = "dbh", ylab = "bal",
                 drape = T, colorkey = TRUE, chull = F,
                 rug = T, 
                 train = pred_grid[,c(3,6)])

pdp::plotPartial(pd_spp_cr, levelplot = T, 
                 contour = T, zlab = "height",
                 xlab = "cr", ylab = "height",
                 drape = T, colorkey = TRUE, chull = F,
                 rug = F)

pdp::plotPartial(pd_spp_forest, levelplot = T,
                 zlab = "z",
                 xlab = "spp", ylab = "height",
                 colorkey = TRUE)


  pdp::plotPartial(pd_lat_sample, center = F, rug = T, alpha = .1)
ice_lat_c <- pdp::plotPartial(pd_lat_sample, center = T, rug = T, alpha = .1)
grid.arrange(ice_lat, ice_lat_c, ncol = 2, main = "ICE plots for latitude")
