library("gridExtra")
library("maps")


# Height distributions
train %>% 
  ggplot(aes(ht_s)) +
  geom_density(fill = "dark gray", bw = 2) +
  scale_x_continuous(limits = c(0, 175))


# Is there height data accross species, dbh, etc?

View(train %>% group_by(spp) %>% 
       summarize(n = n(), mean = mean(ht_s)) %>% 
       arrange(-n))

View(train %>% group_by(forest_type_s) %>% 
       summarize(n = n(), mean = mean(ht_s)) %>% 
       arrange(-n))

train %>% ggplot(aes(lon, lat)) +
  geom_point() +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 60") +
  coord_fixed(xlim = c(-77, -67), ylim = c(42.4, 47.6), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Relationships ----------

# Categorical vars.
train %>%  
  mutate(spp = reorder(spp, -ht_s, FUN = median)) %>%
  ggplot(aes(spp, ht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

train %>%  
  mutate(forest_type_s = reorder(forest_type_s, -ht_s, FUN = median)) %>%
  ggplot(aes(forest_type_s, ht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

train %>%  
  mutate(landscape = reorder(landscape, -ht_s, FUN = median)) %>%
  ggplot(aes(landscape, ht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Continuous vars.
train %>% 
  ggplot(aes(dbh_s, ht_s)) +
  geom_point(alpha = .05) +
  geom_smooth()

train %>% 
  filter(spp %in% c("fir", "spruce", "soft maple",
                    "hard maple", "cedar", "hemlock",
                    "yellow birch", "white pine")) %>%
  mutate(spp = reorder(spp, -ht_s, FUN = median)) %>%
  ggplot(aes(dbh_s, ht_s, col = spp)) +
  geom_smooth() +
  scale_color_brewer(type = "qual",
                     name = "species")

train %>% 
  ggplot(aes(cr_s, ht_s)) +
  geom_smooth() +
  labs(title = "Crown Ratio and Height") +
  scale_x_continuous(name = "crown ratio (%)") +
  scale_y_continuous(name = "height (ft)")

train %>% 
  ggplot(aes(ba_s, ht_s)) +
  geom_smooth()

train %>% 
  ggplot(aes(bal_s, ht_s)) +
  geom_smooth()

train %>% 
  ggplot(aes(ba_s, ht_s)) +
  geom_smooth()

train %>% 
  ggplot(aes(slope, ht_s)) +
  geom_smooth()

train %>% 
  ggplot(aes(aspect, ht_s)) +
  geom_smooth()

train %>% 
  ggplot(aes(lat, ht_s)) +
  geom_smooth()

train %>% 
  ggplot(aes(lon, ht_s)) +
  geom_smooth()

# Discrete numeric vars.
train %>%  
  ggplot(aes(as.factor(crown_class_s), ht_s)) +
  geom_boxplot(fill = "dark gray") 

train %>%  
  ggplot(aes(as.factor(stocking_s), ht_s)) +
  geom_boxplot(fill = "dark gray") 

train %>%  
  ggplot(aes(as.factor(site_class), ht_s)) +
  geom_boxplot(fill = "dark gray")


# Map
cut2 <- function(x, breaks) {
  r <- range(x)
  b <- seq(r[1], r[2], length=2*breaks+1)
  brk <- b[0:breaks*2+1]
  mid <- b[1:breaks*2]
  brk[1] <- brk[1]-0.01
  k <- cut(x, breaks=brk, labels=FALSE)
  mid[k]
}

# 2d bin smoothed mapping of height
train %>%
  mutate(groupx = cut2(lon, 14), #binning lat & lon
         groupy = cut2(lat, 14)) %>%
  group_by(groupx, groupy) %>%
  summarize(height = mean(ht_s)) %>%
  ggplot(aes(groupx, groupy)) +
  geom_raster(aes(fill = height), interpolate = TRUE) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 25") +
  scale_fill_viridis_c(option = "plasma", 
                       name = 'mean height \n(feet)') +
  coord_fixed(xlim = c(-76.8, -67), ylim = c(42.4, 47.5), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Perpetua")) 


# 3-dimensional relationships ---------

train %>% 
  filter(dbh_s <= 36) %>% 
  mutate(dbh_bin = cut(dbh_s, c(0, 6, 12, 18, 36))) %>% 
  ggplot(aes(cr_s, ht_s, color = as.factor(dbh_bin))) +
  geom_smooth() +
  labs(title = "Crown Ratio and Height",
       subtitle = "by diameter class") +
  scale_x_continuous(name = "crown ratio (%)") +
  scale_y_continuous(name = "height (ft)") +
  scale_color_brewer(type = "div",
                     name = "dbh class",
                     direction = -1)

train %>% 
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

train %>% 
  ggplot(aes(cr_s, ht_s, color = as.factor(site_class))) +
  geom_smooth(method.args = list(gamma = 1.5)) +
  labs(title = "Crown Ratio and Height",
       subtitle = "by site class") +
  scale_x_continuous(name = "crown ratio (%)") +
  scale_y_continuous(name = "height (ft)") +
  scale_color_brewer(type = "div",
                     name = "site class",
                     direction = 1) 

train %>% 
  ggplot(aes(dbh_s, ht_s, color = as.factor(site_class))) +
  geom_smooth(method.args = list(gamma = 1.5)) +
  labs(title = "DBH and Height",
       subtitle = "by site class") +
  scale_x_continuous(name = "DBH (in)") +
  scale_y_continuous(name = "height (ft)") +
  scale_color_brewer(type = "div",
                     name = "site class",
                     direction = 1)

train %>% 
  ggplot(aes(bal_s, ht_s, color = as.factor(site_class))) +
  geom_smooth(method.args = list(gamma = 20)) +
  labs(title = "BAL and Height",
       subtitle = "by site class") +
  scale_x_continuous(name = "BAL (sq ft/ac)") +
  scale_y_continuous(name = "height (ft)") +
  scale_color_brewer(type = "div",
                     name = "site class",
                     direction = 1)
