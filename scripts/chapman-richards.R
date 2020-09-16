library("tidyverse")
library("minpack.lm")

temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(!is.na(ht_s)) %>% 
  select(ht_s, spp, dbh_s, cr_s, 
         ba_s, bal_s, plot) %>% 
  rename(ht = ht_s, dbh = dbh_s, cr = cr_s, ba = ba_s, bal = bal_s)

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

# initialize coefficients using Weiskittel nomenclature & values from a model
# we made that didn't differentiate species.
start <- list(b10 = 30,
              b11 = .03,
              b12 = 1,
              b13 = -.1,
              b14 = -.08,
              b15 = -.1)


# gets coefficients for any data frame
get_coefs <- function(df) {
  mod <- nlsLM(ht ~ ((b10 * (1 - exp((-b11) * dbh)) ^       ######### nlsLM function is throwing error #################
                       (b12 + (b13 * log(ba + 1)) + (b14 * log(bal = 1)))) + 
                 b15 * cr),
               start = start, data = df)
  
  coef(mod)
}

# split into species-specific data frames and get coefficients
by_spp <- split(train, train$spp)

coefs_list <- by_spp %>% map(get_coefs)
ht_coef <- as.data.frame(do.call(rbind, coefs_list))

# save
save(ht_coef, file = "dbh-growth-coef.rda")
write.csv(ht_coef, file = "ht-coef.csv")