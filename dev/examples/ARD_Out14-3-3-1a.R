
# Programme:    Generate code to produce ARD for Out14-3-3-1a
# Output:       Summary of Observed and Change from Baseline by Scheduled Visits - Vital Signs (Horizontal)
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
               OutputId = 'Out14-3-3-1a',
               pattern = '(N=XX)')

#Combine operation datasets: 
df3_An01_05_SAF_Summ_ByTrt <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An08_01_Obs_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An08_01_Obs_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADVS,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An08_01_Obs_Summ_ByTrt <- df_An08_01_Obs_Summ_ByTrt %>%
          dplyr::group_by(TRT01A, PARAMCD, AVISIT)



# Apply Data Subset ---
# Data subset: Vital Signs Analysis Records
df2_An08_01_Obs_Summ_ByTrt <- df1_An08_01_Obs_Summ_ByTrt %>%
        dplyr::filter(ANL01FL == 'Y')

#Apply Operations --- 
 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_1_n <- df2_An08_01_Obs_Summ_ByTrt %>%
        dplyr::summarise(res = n()) %>%
        dplyr::mutate(AnalsysisId = 'An08_01_Obs_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_2_Mean
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_2_Mean <- df2_An08_01_Obs_Summ_ByTrt %>%
        dplyr::summarise(res = mean(AVAL)) %>%
        dplyr::mutate(AnalsysisId = 'An08_01_Obs_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_2_Mean',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_3_SD
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_3_SD <- df2_An08_01_Obs_Summ_ByTrt %>%
        dplyr::summarise(res = sd(AVAL)) %>%
        dplyr::mutate(AnalsysisId = 'An08_01_Obs_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_3_SD',
               OutputId = 'Out14-3-3-1a',
               pattern = '(XX.XX)')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_4_Median
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_4_Median <- df2_An08_01_Obs_Summ_ByTrt %>%
        dplyr::summarise(res = median(AVAL)) %>%
        dplyr::mutate(AnalsysisId = 'An08_01_Obs_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_4_Median',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_5_Q1
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_5_Q1 <- df2_An08_01_Obs_Summ_ByTrt %>%
        dplyr::summarise(res = quantile(AVAL, c(.25), na.rm = TRUE)) %>%
        dplyr::mutate(AnalsysisId = 'An08_01_Obs_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_5_Q1',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_6_Q3
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_6_Q3 <- df2_An08_01_Obs_Summ_ByTrt %>%
        dplyr::summarise(res = quantile(AVAL, c(.75), na.rm = TRUE)) %>%
        dplyr::mutate(AnalsysisId = 'An08_01_Obs_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_6_Q3',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_7_Min
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_7_Min <- df2_An08_01_Obs_Summ_ByTrt %>%
        dplyr::summarise(res = min(AVAL)) %>%
        dplyr::mutate(AnalsysisId = 'An08_01_Obs_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_7_Min',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_8_Max
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_8_Max <- df2_An08_01_Obs_Summ_ByTrt %>%
        dplyr::summarise(res = max(AVAL)) %>%
        dplyr::mutate(AnalsysisId = 'An08_01_Obs_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_8_Max',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX')

#Combine operation datasets: 
df3_An08_01_Obs_Summ_ByTrt <- dplyr::bind_rows(df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_1_n, 
df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_2_Mean, 
df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_3_SD, 
df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_4_Median, 
df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_5_Q1, 
df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_6_Q3, 
df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_7_Min, 
df3_An08_01_Obs_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_8_Max) %>%
      dplyr::rename(Group1 = TRT01A,
             Group2 = PARAMCD,
             Group3 = AVISIT)


# Analysis An08_02_ChgBl_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An08_02_ChgBl_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y') %>%
            dplyr::select(USUBJID) %>%
            merge(ADVS,
                  by = 'USUBJID')



#Apply Analysis Grouping ---
df1_An08_02_ChgBl_Summ_ByTrt <- df_An08_02_ChgBl_Summ_ByTrt %>%
          dplyr::group_by(TRT01A, PARAMCD, AVISIT)



# Apply Data Subset ---
# Data subset: Vital Signs Non-baseline Analysis Records
df2_An08_02_ChgBl_Summ_ByTrt <- df1_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::filter(ANL01FL == 'Y' & AVISIT != 'Baseline')

#Apply Operations --- 
 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_1_n <- df2_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::summarise(res = n()) %>%
        dplyr::mutate(AnalsysisId = 'An08_02_ChgBl_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_2_Mean
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_2_Mean <- df2_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::summarise(res = mean(CHG)) %>%
        dplyr::mutate(AnalsysisId = 'An08_02_ChgBl_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_2_Mean',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_3_SD
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_3_SD <- df2_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::summarise(res = sd(CHG)) %>%
        dplyr::mutate(AnalsysisId = 'An08_02_ChgBl_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_3_SD',
               OutputId = 'Out14-3-3-1a',
               pattern = '(XX.XX)')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_4_Median
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_4_Median <- df2_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::summarise(res = median(CHG)) %>%
        dplyr::mutate(AnalsysisId = 'An08_02_ChgBl_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_4_Median',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_5_Q1
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_5_Q1 <- df2_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::summarise(res = quantile(CHG, c(.25), na.rm = TRUE)) %>%
        dplyr::mutate(AnalsysisId = 'An08_02_ChgBl_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_5_Q1',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_6_Q3
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_6_Q3 <- df2_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::summarise(res = quantile(CHG, c(.75), na.rm = TRUE)) %>%
        dplyr::mutate(AnalsysisId = 'An08_02_ChgBl_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_6_Q3',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_7_Min
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_7_Min <- df2_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::summarise(res = min(CHG)) %>%
        dplyr::mutate(AnalsysisId = 'An08_02_ChgBl_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_7_Min',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_8_Max
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_8_Max <- df2_An08_02_ChgBl_Summ_ByTrt %>%
        dplyr::summarise(res = max(CHG)) %>%
        dplyr::mutate(AnalsysisId = 'An08_02_ChgBl_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_8_Max',
               OutputId = 'Out14-3-3-1a',
               pattern = 'XX')

#Combine operation datasets: 
df3_An08_02_ChgBl_Summ_ByTrt <- dplyr::bind_rows(df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_1_n, 
df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_2_Mean, 
df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_3_SD, 
df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_4_Median, 
df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_5_Q1, 
df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_6_Q3, 
df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_7_Min, 
df3_An08_02_ChgBl_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_8_Max) %>%
      dplyr::rename(Group1 = TRT01A,
             Group2 = PARAMCD,
             Group3 = AVISIT)


# combine analyses to create ARD ----
df4 <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt, 
df3_An08_01_Obs_Summ_ByTrt, 
df3_An08_02_ChgBl_Summ_ByTrt)

 #Apply pattern format:
ARD_Out14_3_3_1a<- df4 %>%
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
