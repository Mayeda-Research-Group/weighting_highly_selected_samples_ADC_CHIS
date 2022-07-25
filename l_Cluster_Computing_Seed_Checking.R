#---- packages ----
if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("tidyverse")

#---- read in results ----
path_to_box <- paste0("/Users/crystalshaw/Library/CloudStorage/Box-Box/JZ_CS/", 
                      "KD_bootstrapping/")

results <- read_csv(paste0(path_to_box, "results/bootstrap_runs.csv")) %>% 
  #there was one row before we missing values
  na.omit()

#---- check seeds ----
seeds <- seq(1, 2000)

missing_seeds <- setdiff(seeds, results$seed)

