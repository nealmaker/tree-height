library("gridExtra")
library("maps")


# Height distributions
nf_fia %>% 
  ggplot(aes(ht_s)) +
  geom_density(fill = "dark gray", bw = 2) +
  scale_x_continuous(limits = c(0, 175))


# Is there height data accross species, dbh, etc?

View(nf_fia %>% group_by(spp) %>% 
       summarize(n = n(), mean = mean(ht_s)) %>% 
       arrange(-n))

View(nf_fia %>% group_by(forest_type_s) %>% 
       summarize(n = n(), mean = mean(ht_s)) %>% 
       arrange(-n))

nf_fia %>% ggplot(aes(lon, lat)) +
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
nf_fia %>%  
  mutate(spp = reorder(spp, -ht_s, FUN = median)) %>%
  ggplot(aes(spp, ht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nf_fia %>%  
  mutate(forest_type_s = reorder(forest_type_s, -ht_s, FUN = median)) %>%
  ggplot(aes(forest_type_s, ht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nf_fia %>%  
  mutate(landscape = reorder(landscape, -ht_s, FUN = median)) %>%
  ggplot(aes(landscape, ht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Continuous vars.
nf_fia %>% 
  ggplot(aes(dbh_s, ht_s)) +
  geom_point(alpha = .05) +
  geom_smooth()

nf_fia %>% 
  filter(spp %in% c("fir", "spruce", "soft maple",
                    "hard maple", "cedar", "hemlock",
                    "yellow birch", "white pine")) %>%
  mutate(spp = reorder(spp, -ht_s, FUN = median)) %>%
  ggplot(aes(dbh_s, ht_s, col = spp)) +
  geom_smooth() +
  scale_color_brewer(type = "qual",
                     name = "species")

nf_fia %>% 
  ggplot(aes(cr_s, ht_s)) +
  geom_smooth()

nf_fia %>% 
  ggplot(aes(ba_s, ht_s)) +
  geom_smooth()

nf_fia %>% 
  ggplot(aes(bal_s, ht_s)) +
  geom_smooth()

nf_fia %>% 
  ggplot(aes(ba_s, ht_s)) +
  geom_smooth()

nf_fia %>% 
  ggplot(aes(slope, ht_s)) +
  geom_smooth()

nf_fia %>% 
  ggplot(aes(aspect, ht_s)) +
  geom_smooth()

nf_fia %>% 
  ggplot(aes(lat, ht_s)) +
  geom_smooth()

nf_fia %>% 
  ggplot(aes(lon, ht_s)) +
  geom_smooth()

# Discrete numeric vars.
nf_fia %>%  
  ggplot(aes(as.factor(crown_class_s), ht_s)) +
  geom_boxplot(fill = "dark gray") 

nf_fia %>%  
  ggplot(aes(as.factor(stocking_s), ht_s)) +
  geom_boxplot(fill = "dark gray") 

nf_fia %>%  
  ggplot(aes(as.factor(site_class), ht_s)) +
  geom_boxplot(fill = "dark gray")
