library(plyr)

# one for each distribution/detectability
f_names <- list.files(path="../simulation_study/",  pattern="*\\.RData", 
                      full.names=TRUE)

# lookup for model names to short descriptions
model_name_lookup <- c("HT",
                       "HTcovar",
                       "HTstrat",
                       "HTstratcovar",
                       "Duchon",
                       "TPTE",
                       "TP",
                       "TPSH",
                       "TPR",
                       "TPTER")

names(model_name_lookup) <- c("HT",
                              "HT_cov",
                              "HT_strat",
                              "HT_strat_cov",
                              "m_xy_ds",
                              "m_xy_te",
                              "m_xy_tp",
                              "m_xy_ts",
                              "m_xyr_tp",
                              "m_xyr_te")
model_name_lookup <- factor(model_name_lookup, levels=c("HT", "HTcovar", "HTstrat",
                                                        "HTstratcovar", "Duchon", "TPTE",
                                                        "TP", "TPSH", "TPR", "TPTER"))


huge_res <- c()

# now loop over the files
for(i in seq_along(f_names)){

  load(f_names[i])

  # print the name of this scenario as the header
  f_names[i] <- sub("../simulation_study//", "", f_names[i], fixed=TRUE)
  f_names[i] <- sub(".RData", "", f_names[i])
  big_res$scenario <- f_names[i]
  big_res$model <- as.character(big_res$model)
  big_res$model <- model_name_lookup[big_res$model]

  ## WARNING: magic numbers below!!
  # bias boxplot
  big_res$bias <- big_res$N-500

  # separate out the scenario definition
  big_res$density <- sub("([a-z]+)-[a-z]+-[a-z]+", "\\1", big_res$scenario[1])
  big_res$design <- sub("[a-z]+-([a-z]+)-[a-z]+", "\\1", big_res$scenario[1])

  if(big_res$design == "tb"){
	big_res$design <- "twozigzag"
  }

  if(!grepl("covar", big_res$scenario[1])){
    big_res$df <- sub("[a-z]+-[a-z]+-([a-z]+)", "\\1", big_res$scenario[1])
    big_res$covar <- FALSE
  }else{
    big_res$covar <- TRUE
	big_res$df <- "covar"
  }

  huge_res <- rbind(huge_res, big_res)
}

