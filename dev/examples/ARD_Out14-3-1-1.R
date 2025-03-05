
# Programme:    Generate code to produce ARD for Out14-3-1-1
# Output:       Overall Summary of Treatment-Emergent Adverse Events
# Date created: 2025-03-05 17:20:51


  

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
               OutputId = 'Out14-3-1-1',
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
               OutputId = 'Out14-3-1-1',
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


# Analysis An07_02_RelTEAE_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_02_RelTEAE_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_02_RelTEAE_Summ_ByTrt <- df_An07_02_RelTEAE_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



# Apply Data Subset ---
# Data subset: Related Treatment-Emergent Adverse Events
df2_An07_02_RelTEAE_Summ_ByTrt <- df1_An07_02_RelTEAE_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y' & AEREL %in% 'c("POSSIBLE", "PROBABLE")')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_02_RelTEAE_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_02_RelTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_02_RelTEAE_Summ_ByTrt <- dplyr::bind_rows(df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_02_RelTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An07_03_SerTEAE_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_03_SerTEAE_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_03_SerTEAE_Summ_ByTrt <- df_An07_03_SerTEAE_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



# Apply Data Subset ---
# Data subset: Serious Treatment-Emergent Adverse Events
df2_An07_03_SerTEAE_Summ_ByTrt <- df1_An07_03_SerTEAE_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y' & AESER == 'Y')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_03_SerTEAE_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_03_SerTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_03_SerTEAE_Summ_ByTrt <- dplyr::bind_rows(df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_03_SerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An07_04_RelSerTEAE_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_04_RelSerTEAE_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_04_RelSerTEAE_Summ_ByTrt <- df_An07_04_RelSerTEAE_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



# Apply Data Subset ---
# Data subset: Related Serious Treatment-Emergent Adverse Events
df2_An07_04_RelSerTEAE_Summ_ByTrt <- df1_An07_04_RelSerTEAE_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y' & AEREL %in% 'c("POSSIBLE", "PROBABLE")' & AESER == 'Y')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_04_RelSerTEAE_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_04_RelSerTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_04_RelSerTEAE_Summ_ByTrt <- dplyr::bind_rows(df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_04_RelSerTEAE_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An07_05_TEAELd2Dth_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_05_TEAELd2Dth_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_05_TEAELd2Dth_Summ_ByTrt <- df_An07_05_TEAELd2Dth_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Death
df2_An07_05_TEAELd2Dth_Summ_ByTrt <- df1_An07_05_TEAELd2Dth_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y' & AESDTH == 'Y')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_05_TEAELd2Dth_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_05_TEAELd2Dth_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_05_TEAELd2Dth_Summ_ByTrt <- dplyr::bind_rows(df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_05_TEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An07_06_RelTEAELd2Dth_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_06_RelTEAELd2Dth_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_06_RelTEAELd2Dth_Summ_ByTrt <- df_An07_06_RelTEAELd2Dth_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



# Apply Data Subset ---
# Data subset: Related Treatment-Emergent Adverse Events Leading to Death
df2_An07_06_RelTEAELd2Dth_Summ_ByTrt <- df1_An07_06_RelTEAELd2Dth_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y' & AESDTH == 'Y', AEREL == 'POSSIBLE' | AEREL == 'PROBABLE')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_06_RelTEAELd2Dth_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_06_RelTEAELd2Dth_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt <- dplyr::bind_rows(df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An07_07_TEAELd2DoseMod_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_07_TEAELd2DoseMod_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_07_TEAELd2DoseMod_Summ_ByTrt <- df_An07_07_TEAELd2DoseMod_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Dose Modification
df2_An07_07_TEAELd2DoseMod_Summ_ByTrt <- df1_An07_07_TEAELd2DoseMod_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y' & AEACN %in% 'c("DOSE REDUCED", "DRUG INTERRUPTED")')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_07_TEAELd2DoseMod_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_07_TEAELd2DoseMod_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt <- dplyr::bind_rows(df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An07_08_TEAELd2TrtDsc_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADAE,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- df_An07_08_TEAELd2TrtDsc_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Treatment Discontinuation
df2_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- df1_An07_08_TEAELd2TrtDsc_Summ_ByTrt %>%
        dplyr::filter(TRTEMFL == 'Y' & AEACN == 'DRUG WITHDRAWN')

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An07_08_TEAELd2TrtDsc_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An07_08_TEAELd2TrtDsc_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- dplyr::bind_rows(df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A)


# combine analyses to create ARD ----
df4 <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt, 
df3_An07_01_TEAE_Summ_ByTrt, 
df3_An07_02_RelTEAE_Summ_ByTrt, 
df3_An07_03_SerTEAE_Summ_ByTrt, 
df3_An07_04_RelSerTEAE_Summ_ByTrt, 
df3_An07_05_TEAELd2Dth_Summ_ByTrt, 
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt, 
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt, 
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt)

 #Apply pattern format:
ARD_Out14_3_1_1<- df4 %>%
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
