
# Programme:    Generate code to produce ARD for Out14-3-2-1
# Output:       Summary of TEAE by System Organ Class and Preferred Term
# Date created: 2025-03-05 17:20:52


  

# load libraries ----
library(tidyverse)
library(readxl)
library(splitstackshape)
library(readr)
  
# load ADaM ----
ADSL <- read_csv('dev/examples/ADSL.csv')
ADAE <- read_csv('dev/examples/ADAE.csv') %>%
  rename(TRT01A = TRTA)
ADVS <- read_csv('dev/examples/ADVS.csv') %>%
  rename(TRT01A = TRTA)
  

# Analysis An01_05_SAF_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An01_05_SAF_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---
df1_An01_05_SAF_Summ_ByTrt <- df_An01_05_SAF_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



#Apply Data Subset ---
df2_An01_05_SAF_Summ_ByTrt <- df1_An01_05_SAF_Summ_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Count_ByGrp_1_n
# Operation name:         Count by group for a categorical variable
# Operation description:  Count across groups for a categorical variable, based on subject occurrence

df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n <- df2_An01_05_SAF_Summ_ByTrt %>%
        dplyr::summarise(res = n()) %>%
        dplyr::mutate(AnalsysisId = 'An01_05_SAF_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Count_ByGrp',
               OperationId = 'Mth01_CatVar_Count_ByGrp_1_n',
               OutputId = 'Out14-3-2-1',
               pattern = '(N=XX)')

#Combine operation datasets: 
df3_An01_05_SAF_Summ_ByTrt <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An07_01_TEAE_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_01_TEAE_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_01_TEAE_Summ_ByTrt <- df_An07_01_TEAE_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events
df2_An07_01_TEAE_Summ_ByTrt <- df1_An07_01_TEAE_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_01_TEAE_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_01_TEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-2-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_01_TEAE_Summ_ByTrt <- dplyr::bind_rows(df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An07_01_TEAE_Comp_ByTrt_PlacLow----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_01_TEAE_Comp_ByTrt_PlacLow <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An07_01_TEAE_Comp_ByTrt_PlacLow <- df_An07_01_TEAE_Comp_ByTrt_PlacLow


# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events for Placebo and Low Active Dose
df2_An07_01_TEAE_Comp_ByTrt_PlacLow <- df1_An07_01_TEAE_Comp_ByTrt_PlacLow %>%
        dplyr::filter(TRTEMFL == 'Y' & TRT01A %in% 'c("Placebo", "Xanomeline Low Dose")')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_01_TEAE_Comp_ByTrt_PlacLow <- dplyr::bind_rows(df3_An07_01_TEAE_Comp_ByTrt_PlacLow_Mth03_CatVar_Comp_FishEx_1_pval)

# Analysis An07_01_TEAE_Comp_ByTrt_PlacHigh----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_01_TEAE_Comp_ByTrt_PlacHigh <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An07_01_TEAE_Comp_ByTrt_PlacHigh <- df_An07_01_TEAE_Comp_ByTrt_PlacHigh


# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events for Placebo and High Active Dose
df2_An07_01_TEAE_Comp_ByTrt_PlacHigh <- df1_An07_01_TEAE_Comp_ByTrt_PlacHigh %>%
        dplyr::filter(TRTEMFL == 'Y' & TRT01A %in% 'c("Placebo", "Xanomeline High Dose")')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_01_TEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_01_TEAE_Comp_ByTrt_PlacHigh <- dplyr::bind_rows(df3_An07_01_TEAE_Comp_ByTrt_PlacHigh_Mth03_CatVar_Comp_FishEx_1_pval)

# Analysis An07_09_Soc_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_09_Soc_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_09_Soc_Summ_ByTrt <- df_An07_09_Soc_Summ_ByTrt %>%
          dplyr::group_by(TRT01A, AESOC)



# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events
df2_An07_09_Soc_Summ_ByTrt <- df1_An07_09_Soc_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_09_Soc_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_09_Soc_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-2-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_09_Soc_Summ_ByTrt <- dplyr::bind_rows(df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A,
             Group2 = AESOC)


# Analysis An07_09_Soc_Comp_ByTrt_PlacLow----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_09_Soc_Comp_ByTrt_PlacLow <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An07_09_Soc_Comp_ByTrt_PlacLow <- df_An07_09_Soc_Comp_ByTrt_PlacLow


# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events for Placebo and Low Active Dose
df2_An07_09_Soc_Comp_ByTrt_PlacLow <- df1_An07_09_Soc_Comp_ByTrt_PlacLow %>%
        dplyr::filter(TRTEMFL == 'Y' & TRT01A %in% 'c("Placebo", "Xanomeline Low Dose")')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_09_Soc_Comp_ByTrt_PlacLow <- dplyr::bind_rows(df3_An07_09_Soc_Comp_ByTrt_PlacLow_Mth03_CatVar_Comp_FishEx_1_pval)

# Analysis An07_09_Soc_Comp_ByTrt_PlacHigh----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_09_Soc_Comp_ByTrt_PlacHigh <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An07_09_Soc_Comp_ByTrt_PlacHigh <- df_An07_09_Soc_Comp_ByTrt_PlacHigh


# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events for Placebo and High Active Dose
df2_An07_09_Soc_Comp_ByTrt_PlacHigh <- df1_An07_09_Soc_Comp_ByTrt_PlacHigh %>%
        dplyr::filter(TRTEMFL == 'Y' & TRT01A %in% 'c("Placebo", "Xanomeline High Dose")')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_09_Soc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_09_Soc_Comp_ByTrt_PlacHigh <- dplyr::bind_rows(df3_An07_09_Soc_Comp_ByTrt_PlacHigh_Mth03_CatVar_Comp_FishEx_1_pval)

# Analysis An07_10_SocPt_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_10_SocPt_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_10_SocPt_Summ_ByTrt <- df_An07_10_SocPt_Summ_ByTrt %>%
          dplyr::group_by(TRT01A, AESOC, AEDECOD)



# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events
df2_An07_10_SocPt_Summ_ByTrt <- df1_An07_10_SocPt_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_10_SocPt_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_10_SocPt_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-2-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_10_SocPt_Summ_ByTrt <- dplyr::bind_rows(df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A,
             Group2 = AESOC,
             Group3 = AEDECOD)


# Analysis An07_10_SocPt_Comp_ByTrt_PlacLow----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_10_SocPt_Comp_ByTrt_PlacLow <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An07_10_SocPt_Comp_ByTrt_PlacLow <- df_An07_10_SocPt_Comp_ByTrt_PlacLow


# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events for Placebo and Low Active Dose
df2_An07_10_SocPt_Comp_ByTrt_PlacLow <- df1_An07_10_SocPt_Comp_ByTrt_PlacLow %>%
        dplyr::filter(TRTEMFL == 'Y' & TRT01A %in% 'c("Placebo", "Xanomeline Low Dose")')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_10_SocPt_Comp_ByTrt_PlacLow <- dplyr::bind_rows(df3_An07_10_SocPt_Comp_ByTrt_PlacLow_Mth03_CatVar_Comp_FishEx_1_pval)

# Analysis An07_10_SocPt_Comp_ByTrt_PlacHigh----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_10_SocPt_Comp_ByTrt_PlacHigh <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An07_10_SocPt_Comp_ByTrt_PlacHigh <- df_An07_10_SocPt_Comp_ByTrt_PlacHigh


# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events for Placebo and High Active Dose
df2_An07_10_SocPt_Comp_ByTrt_PlacHigh <- df1_An07_10_SocPt_Comp_ByTrt_PlacHigh %>%
        dplyr::filter(TRTEMFL == 'Y' & TRT01A %in% 'c("Placebo", "Xanomeline High Dose")')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_10_SocPt_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_10_SocPt_Comp_ByTrt_PlacHigh <- dplyr::bind_rows(df3_An07_10_SocPt_Comp_ByTrt_PlacHigh_Mth03_CatVar_Comp_FishEx_1_pval)

# combine analyses to create ARD ----
df4 <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt, 
df3_An07_01_TEAE_Summ_ByTrt, 
df3_An07_01_TEAE_Comp_ByTrt_PlacLow, 
df3_An07_01_TEAE_Comp_ByTrt_PlacHigh, 
df3_An07_09_Soc_Summ_ByTrt, 
df3_An07_09_Soc_Comp_ByTrt_PlacLow, 
df3_An07_09_Soc_Comp_ByTrt_PlacHigh, 
df3_An07_10_SocPt_Summ_ByTrt, 
df3_An07_10_SocPt_Comp_ByTrt_PlacLow, 
df3_An07_10_SocPt_Comp_ByTrt_PlacHigh)

 #Apply pattern format:
ARD_Out14_3_2_1<- df4 %>%
      dplyr::mutate(dec = ifelse(grepl('X.X',
                                df4$pattern, ),
                          stringr::str_count(substr(df4$pattern,
                                          str_locate(df4$pattern,
                                                    'X.X')[, 1]+2,
                                          nchar(df4$pattern)), 'X'),
                          0)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rnd = round(res, dec)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(disp = ifelse(grepl('\\(N=', df4$pattern),
                           paste0('(N=', rnd, ')'),
                           ifelse(grepl('\\(', df4$pattern),
                                  paste0('(', rnd, ')'),
                                  as.character(rnd)))) %>%
                         dplyr::select(-rnd, -dec)
