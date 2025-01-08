
##Conference Paper
##Bang, J. (2024, November 15). The Effect of Mind wandering and Boredom on habitual/addictive shortform usage. Korean Association of AD & PR Fall Conference 2024. Seoul, Korea

library(dplyr)
library(semTools)
library(tibble)
library(semPlot)
library(seminr)
library(openxlsx)

save_loadings_to_excel <- function(loadings, file_name) {
  
  # loadings 객체가 데이터 프레임인지 확인 후 변환
  if (!is.data.frame(loadings)) {
    loadings_df <- as.data.frame(loadings)
  } else {
    loadings_df <- loadings
  }
  
  # 행 이름(인덱스)을 새로운 열로 추가
  loadings_df_with_index <- cbind(Index = rownames(loadings_df), loadings_df)
  
  # 엑셀 파일로 저장
  library(openxlsx)
  write.xlsx(loadings_df_with_index, file_name)
  
  # 저장 완료 메시지 출력
  cat("Data has been saved to", file_name, "\n")
}

plot_pls_model <- function(filename = "test.pdf",pls_model, structure_only = FALSE) {
  
  temp <- dot_graph(pls_model, structure_only = structure_only)
  temp_resized <- gsub("graph \\[", "graph [\n  nodesep=0.1,\n  ranksep=2,",temp)
  d <-  DiagrammeR::grViz(temp_resized)
  save_plot(filename = filename, plot = d)
  
}

setwd("./data")
data <- read.csv("./data_20240913_coded_pls_sem.csv")

data$Gender = dplyr::recode(data$Gender, `1` = 1, `2` = 0)

selected_data <- data %>% select("Gender":"Bored_E_6")

mm <- constructs(
  
  composite("Age", single_item("Age")),
  composite("Gender", single_item("Gender")), ## 0: 여자, 1: 남자
  
  composite("Sf_habbit", multi_items("Sf_habbit_", 1:3)),
  composite("Sf_addict", multi_items("Sf_addict_", 1:6)),
  composite("Sf_deped", multi_items("Sf_deped_", 1:3)),
  composite("Sf_deped", multi_items("Sf_deped_", 5:6)),
  
  composite("Mind_Wand_deli", multi_items("Mind_Wand_deli_", 1:4)),
  composite("Mind_Wand_spon", multi_items("Mind_Wand_spon_", 1:4)),
  
  composite("Bored_I", multi_items("Bored_I_",1:6)),
  composite("Bored_E", multi_items("Bored_E_", 1:6))
  
)

sm_MW <- relationships(
  
  paths(from = c("Mind_Wand_deli","Mind_Wand_spon","Bored_I","Bored_E","Gender", "Age"),
        to = c("Sf_habbit","Sf_addict")),
        
  paths(from = c("Sf_habbit"), 
        to =c("Sf_addict"))
)

pls_model_MW <- estimate_pls(
  data = selected_data,
  measurement_model = mm,
  structural_model = sm_MW,
  missing = mean_replacement,
  missing_value = "-99"
)

# Summarize the model results
summary_pls_MW <- summary(pls_model_MW)
summary_pls_MW


###################################################
############ Reliability & Validity #################
###################################################
summary_pls_MW$iterations

# Inspect the indicator loadings (should be over .708)
summary_pls_MW$loadings
save_loadings_to_excel(summary_pls_MW$loadings, "./MW/summary_pls_MW_loadings.xlsx")

# Inspect the indicator reliability (should be over .50)
summary_pls_MW$loadings^2

# Inspect the composite reliability
# AVE -> convergent validity
summary_pls_MW$reliability
save_loadings_to_excel(summary_pls_MW$reliability, "./MW/summary_pls_MW_reliability.xlsx")

# Plot the reliabilities of constructs
plot(summary_pls_MW$reliability)

# Table of the FL criteria
# Discriminant validity
# HL often fails to identifying (Radomir & Moisesu, 2019)
summary_pls_MW$validity$fl_criteria

# HTMT criterion threshold value should below 0.9
# 한 요인 안 문항들의 상관관계는 커야하고, 다른 요인의 문항들과의 상관관계는 작아야 함
summary_pls_MW$validity$htmt
save_loadings_to_excel(summary_pls_MW$validity$htmt, "./MW/summary_pls_MW_htmt.xlsx")

# HTMT값이 1과 다르다는 것을 확인하기 위해서는, 부트스트래핑을 진행해야 함함
# CI가 1을 포함해서는 안됨 
boot_MW <- bootstrap_model(seminr_model = pls_model_MW, nboot = 1000)
sum_MW <- summary(boot_MW, alpha = 0.10) #  obtain 90% two-sided bootstrap
sum_MW$bootstrapped_HTMT
save_loadings_to_excel(sum_MW$bootstrapped_HTMT, "./MW/summary_pls_MW_htmt_boot.xlsx")

###################################################

#########  Evaluating Structural Model  #########

#################################################

###Step1. Assess collinearity
## VIF < 3~5
summary_pls_MW$validity$vif_items
summary_pls_MW$vif_antecedents


##Step2 Bootstrapping
boot_model_MW <- bootstrap_model(
  seminr_model = pls_model_MW,
  nboot = 1000,
  cores = NULL,
  seed = 123
)
summary_boot_MW <- summary(boot_model_MW, alpha = 0.05)
summary_boot_MW$bootstrapped_paths
save_loadings_to_excel(summary_boot_MW$bootstrapped_paths, "./MW/summary_boot_MW_bootstrapped_paths_age_gender.xlsx")

paths <- summary_boot_MW$bootstrapped_paths[, "Original Est."]
tvalues <- summary_boot_MW$bootstrapped_paths[, "T Stat."]
df = nrow(data)
# calculate pvalues from tvalues and df; round to 3 decimal places
pvalues <- round( pt(tvalues, df, lower.tail = FALSE), 3)
# make a table of paths, tvalues, pvalues
data.frame(paths, tvalues, pvalues)

predict_model_MW <- predict_pls(
  model = pls_model_MW,
  technique = predict_DA,
  noFolds = 10,
  reps = 10
)
# 
summary_predict_MW <-  summary(predict_model_MW)
summary_predict_MW

# #######################################
# #########Mediation#####################
# #####################################


summary_pls_MW$total_indirect_effects
save_loadings_to_excel(summary_pls_MW$total_indirect_effects, "./MW/summary_MW_total_indirect_effect.xlsx")

##Inspect indirect effects
specific_effect_significance(boot_model_MW,
                             from="Mind_Wand_deli",
                             through = c("Sf_habbit"),
                             to="Sf_addict",
                             alpha = 0.05)

specific_effect_significance(boot_model_MW,
                             from="Mind_Wand_spon",
                             through = c("Sf_habbit"),
                             to="Sf_addict",
                             alpha = 0.05)
specific_effect_significance(boot_model_MW,
                             from="Bored_I",
                             through = c("Sf_habbit"),
                             to="Sf_addict",
                             alpha = 0.05)
specific_effect_significance(boot_model_MW,
                             from="Bored_E",
                             through = c("Sf_habbit"),
                             to="Sf_addict",
                             alpha = 0.05)
specific_effect_significance(boot_model_MW,
                             from="Gender",
                             through = c("Sf_habbit"),
                             to="Sf_addict",
                             alpha = 0.05)
specific_effect_significance(boot_model_MW,
                             from="Age",
                             through = c("Sf_habbit"),
                             to="Sf_addict",
                             alpha = 0.05)

plot_pls_model(pls_model = pls_model_MW, filename = "./MW/pls_sem_MW_plot.pdf", structure_only = TRUE)

