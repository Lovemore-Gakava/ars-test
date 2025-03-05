
# Programme:    Generate code to produce ARD for Out14-1-1
# Output:       Summary of Demographics
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
               OutputId = 'Out14-1-1',
               pattern = '(N=XX)')

#Combine operation datasets: 
df3_An01_05_SAF_Summ_ByTrt <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An03_01_Age_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_01_Age_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---
df1_An03_01_Age_Summ_ByTrt <- df_An03_01_Age_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



#Apply Data Subset ---
df2_An03_01_Age_Summ_ByTrt <- df1_An03_01_Age_Summ_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_1_n <- df2_An03_01_Age_Summ_ByTrt %>%
        dplyr::summarise(res = n()) %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-1-1',
               pattern = 'XX')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_2_Mean
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_2_Mean <- df2_An03_01_Age_Summ_ByTrt %>%
        dplyr::summarise(res = mean(AGE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_2_Mean',
               OutputId = 'Out14-1-1',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_3_SD
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_3_SD <- df2_An03_01_Age_Summ_ByTrt %>%
        dplyr::summarise(res = sd(AGE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_3_SD',
               OutputId = 'Out14-1-1',
               pattern = '(XX.XX)')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_4_Median
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_4_Median <- df2_An03_01_Age_Summ_ByTrt %>%
        dplyr::summarise(res = median(AGE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_4_Median',
               OutputId = 'Out14-1-1',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_5_Q1
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_5_Q1 <- df2_An03_01_Age_Summ_ByTrt %>%
        dplyr::summarise(res = quantile(AGE, c(.25), na.rm = TRUE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_5_Q1',
               OutputId = 'Out14-1-1',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_6_Q3
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_6_Q3 <- df2_An03_01_Age_Summ_ByTrt %>%
        dplyr::summarise(res = quantile(AGE, c(.75), na.rm = TRUE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_6_Q3',
               OutputId = 'Out14-1-1',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_7_Min
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_7_Min <- df2_An03_01_Age_Summ_ByTrt %>%
        dplyr::summarise(res = min(AGE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_7_Min',
               OutputId = 'Out14-1-1',
               pattern = 'XX')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_8_Max
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_8_Max <- df2_An03_01_Age_Summ_ByTrt %>%
        dplyr::summarise(res = max(AGE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_8_Max',
               OutputId = 'Out14-1-1',
               pattern = 'XX')

#Combine operation datasets: 
df3_An03_01_Age_Summ_ByTrt <- dplyr::bind_rows(df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_1_n, 
df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_2_Mean, 
df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_3_SD, 
df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_4_Median, 
df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_5_Q1, 
df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_6_Q3, 
df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_7_Min, 
df3_An03_01_Age_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_8_Max) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An03_01_Age_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_01_Age_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An03_01_Age_Comp_ByTrt <- df_An03_01_Age_Comp_ByTrt


#Apply Data Subset ---
df2_An03_01_Age_Comp_ByTrt <- df1_An03_01_Age_Comp_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth04_ContVar_Comp_Anova_1_pval
# Operation name:         Analysis of variance group comparison for a continuous variable
# Operation description:  Comparison of groups by analysis of variance (ANOVA) for a continuous variable


fm <- stats::as.formula(paste('AGE', '~', 'TRT01A'))
  model <- stats::lm(fm, data = df2_An03_01_Age_Comp_ByTrt
  )

if (class(model) != 'lm') stop('Not an object of class lm ')
f <- summary(model)$fstatistic
p <- stats::pf(f[1],f[2],f[3],lower.tail=F)
attributes(p) <- NULL

df3_An03_01_Age_Comp_ByTrt_Mth04_ContVar_Comp_Anova_1_pval <- data.frame(res = p,
                  AnalsysisId = 'An03_01_Age_Comp_ByTrt',
                  MethodId = 'Mth04_ContVar_Comp_Anova',
                  OperationId = 'Mth04_ContVar_Comp_Anova_1_pval',
                  OutputId = 'Out14-1-1',
                  pattern = 'X.XXXX')
#Combine operation datasets: 
df3_An03_01_Age_Comp_ByTrt <- dplyr::bind_rows(df3_An03_01_Age_Comp_ByTrt_Mth04_ContVar_Comp_Anova_1_pval)

# Analysis An03_02_AgeGrp_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_02_AgeGrp_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---
df1_An03_02_AgeGrp_Summ_ByTrt <- df_An03_02_AgeGrp_Summ_ByTrt %>%
          dplyr::group_by(TRT01A, AGEGR1)



#Apply Data Subset ---
df2_An03_02_AgeGrp_Summ_ByTrt <- df1_An03_02_AgeGrp_Summ_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An03_02_AgeGrp_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An03_02_AgeGrp_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An03_02_AgeGrp_Summ_ByTrt <- dplyr::bind_rows(df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An03_02_AgeGrp_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A,
             Group2 = AGEGR1)


# Analysis An03_02_AgeGrp_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_02_AgeGrp_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An03_02_AgeGrp_Comp_ByTrt <- df_An03_02_AgeGrp_Comp_ByTrt


#Apply Data Subset ---
df2_An03_02_AgeGrp_Comp_ByTrt <- df1_An03_02_AgeGrp_Comp_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth03_CatVar_Comp_PChiSq_1_pval
# Operation name:         Pearson's chi-square test group comparison for a categorical variable
# Operation description:  Comparison of groups by Pearson's chi-square test for a categorical variable

tab <- table(ADSL[, c('TRT01A', 'AGEGR1')])
p <- chisq.test(tab)$p.value

df3_An03_02_AgeGrp_Comp_ByTrt_Mth03_CatVar_Comp_PChiSq_1_pval <- data.frame(res = p,
                  AnalsysisId = 'An03_02_AgeGrp_Comp_ByTrt',
                  MethodId = 'Mth03_CatVar_Comp_PChiSq',
                  OperationId = 'Mth03_CatVar_Comp_PChiSq_1_pval',
                  OutputId = 'Out14-1-1',
                  pattern = 'X.XXXX')

#Combine operation datasets: 
df3_An03_02_AgeGrp_Comp_ByTrt <- dplyr::bind_rows(df3_An03_02_AgeGrp_Comp_ByTrt_Mth03_CatVar_Comp_PChiSq_1_pval)

# Analysis An03_03_Sex_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_03_Sex_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---
df1_An03_03_Sex_Summ_ByTrt <- df_An03_03_Sex_Summ_ByTrt %>%
          dplyr::group_by(TRT01A, SEX)



#Apply Data Subset ---
df2_An03_03_Sex_Summ_ByTrt <- df1_An03_03_Sex_Summ_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An03_03_Sex_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An03_03_Sex_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An03_03_Sex_Summ_ByTrt <- dplyr::bind_rows(df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An03_03_Sex_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A,
             Group2 = SEX)


# Analysis An03_03_Sex_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_03_Sex_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An03_03_Sex_Comp_ByTrt <- df_An03_03_Sex_Comp_ByTrt


#Apply Data Subset ---
df2_An03_03_Sex_Comp_ByTrt <- df1_An03_03_Sex_Comp_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth03_CatVar_Comp_PChiSq_1_pval
# Operation name:         Pearson's chi-square test group comparison for a categorical variable
# Operation description:  Comparison of groups by Pearson's chi-square test for a categorical variable

tab <- table(ADSL[, c('TRT01A', 'SEX')])
p <- chisq.test(tab)$p.value

df3_An03_03_Sex_Comp_ByTrt_Mth03_CatVar_Comp_PChiSq_1_pval <- data.frame(res = p,
                  AnalsysisId = 'An03_03_Sex_Comp_ByTrt',
                  MethodId = 'Mth03_CatVar_Comp_PChiSq',
                  OperationId = 'Mth03_CatVar_Comp_PChiSq_1_pval',
                  OutputId = 'Out14-1-1',
                  pattern = 'X.XXXX')

#Combine operation datasets: 
df3_An03_03_Sex_Comp_ByTrt <- dplyr::bind_rows(df3_An03_03_Sex_Comp_ByTrt_Mth03_CatVar_Comp_PChiSq_1_pval)

# Analysis An03_04_Ethnic_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_04_Ethnic_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---
df1_An03_04_Ethnic_Summ_ByTrt <- df_An03_04_Ethnic_Summ_ByTrt %>%
          dplyr::group_by(TRT01A, ETHNIC)



#Apply Data Subset ---
df2_An03_04_Ethnic_Summ_ByTrt <- df1_An03_04_Ethnic_Summ_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An03_04_Ethnic_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An03_04_Ethnic_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An03_04_Ethnic_Summ_ByTrt <- dplyr::bind_rows(df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An03_04_Ethnic_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A,
             Group2 = ETHNIC)


# Analysis An03_04_Ethnic_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_04_Ethnic_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An03_04_Ethnic_Comp_ByTrt <- df_An03_04_Ethnic_Comp_ByTrt


#Apply Data Subset ---
df2_An03_04_Ethnic_Comp_ByTrt <- df1_An03_04_Ethnic_Comp_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth03_CatVar_Comp_PChiSq_1_pval
# Operation name:         Pearson's chi-square test group comparison for a categorical variable
# Operation description:  Comparison of groups by Pearson's chi-square test for a categorical variable

tab <- table(ADSL[, c('TRT01A', 'ETHNIC')])
p <- chisq.test(tab)$p.value

df3_An03_04_Ethnic_Comp_ByTrt_Mth03_CatVar_Comp_PChiSq_1_pval <- data.frame(res = p,
                  AnalsysisId = 'An03_04_Ethnic_Comp_ByTrt',
                  MethodId = 'Mth03_CatVar_Comp_PChiSq',
                  OperationId = 'Mth03_CatVar_Comp_PChiSq_1_pval',
                  OutputId = 'Out14-1-1',
                  pattern = 'X.XXXX')

#Combine operation datasets: 
df3_An03_04_Ethnic_Comp_ByTrt <- dplyr::bind_rows(df3_An03_04_Ethnic_Comp_ByTrt_Mth03_CatVar_Comp_PChiSq_1_pval)

# Analysis An03_05_Race_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_05_Race_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---
df1_An03_05_Race_Summ_ByTrt <- df_An03_05_Race_Summ_ByTrt %>%
          dplyr::group_by(TRT01A, RACE)



#Apply Data Subset ---
df2_An03_05_Race_Summ_ByTrt <- df1_An03_05_Race_Summ_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n <- df2_An03_05_Race_Summ_ByTrt %>%
        dplyr::summarise(res = n_distinct(USUBJID)) %>%
        dplyr::mutate(AnalsysisId = 'An03_05_Race_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OperationId = 'Mth01_CatVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-1-1',
               pattern = 'XXX')

 
# Operation ID:           Mth01_CatVar_Summ_ByGrp_2_pct
# Operation name:         Summary by group of a categorical variable
# Operation description:  Descriptive summary statistics across groups for a categorical variable, based on subject occurrence



df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num <- df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n %>%
          dplyr::rename(NUM = res)

df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den <- df3_An01_05_SAF_Summ_ByTrt_Mth01_CatVar_Count_ByGrp_1_n %>%
          dplyr::rename(DEN = res)

df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct <- merge(df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_num,
                                            df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct_den %>%
                                                  dplyr::select(TRT01A, DEN),
                                            by = c('TRT01A')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'Mth01_CatVar_Summ_ByGrp_2_pct',
                                                   pattern = '( XX.X)') %>%
                                            dplyr::select(-NUM, -DEN)

#Combine operation datasets: 
df3_An03_05_Race_Summ_ByTrt <- dplyr::bind_rows(df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_1_n, 
df3_An03_05_Race_Summ_ByTrt_Mth01_CatVar_Summ_ByGrp_2_pct) %>%
      dplyr::rename(Group1 = TRT01A,
             Group2 = RACE)


# Analysis An03_05_Race_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_05_Race_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An03_05_Race_Comp_ByTrt <- df_An03_05_Race_Comp_ByTrt


#Apply Data Subset ---
df2_An03_05_Race_Comp_ByTrt <- df1_An03_05_Race_Comp_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth03_CatVar_Comp_PChiSq_1_pval
# Operation name:         Pearson's chi-square test group comparison for a categorical variable
# Operation description:  Comparison of groups by Pearson's chi-square test for a categorical variable

tab <- table(ADSL[, c('TRT01A', 'RACE')])
p <- chisq.test(tab)$p.value

df3_An03_05_Race_Comp_ByTrt_Mth03_CatVar_Comp_PChiSq_1_pval <- data.frame(res = p,
                  AnalsysisId = 'An03_05_Race_Comp_ByTrt',
                  MethodId = 'Mth03_CatVar_Comp_PChiSq',
                  OperationId = 'Mth03_CatVar_Comp_PChiSq_1_pval',
                  OutputId = 'Out14-1-1',
                  pattern = 'X.XXXX')

#Combine operation datasets: 
df3_An03_05_Race_Comp_ByTrt <- dplyr::bind_rows(df3_An03_05_Race_Comp_ByTrt_Mth03_CatVar_Comp_PChiSq_1_pval)

# Analysis An03_06_Height_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_06_Height_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---
df1_An03_06_Height_Summ_ByTrt <- df_An03_06_Height_Summ_ByTrt %>%
          dplyr::group_by(TRT01A)



#Apply Data Subset ---
df2_An03_06_Height_Summ_ByTrt <- df1_An03_06_Height_Summ_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_1_n
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_1_n <- df2_An03_06_Height_Summ_ByTrt %>%
        dplyr::summarise(res = n()) %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_1_n',
               OutputId = 'Out14-1-1',
               pattern = 'XX')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_2_Mean
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_2_Mean <- df2_An03_06_Height_Summ_ByTrt %>%
        dplyr::summarise(res = mean(HEIGHTBL)) %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_2_Mean',
               OutputId = 'Out14-1-1',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_3_SD
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_3_SD <- df2_An03_06_Height_Summ_ByTrt %>%
        dplyr::summarise(res = sd(HEIGHTBL)) %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_3_SD',
               OutputId = 'Out14-1-1',
               pattern = '(XX.XX)')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_4_Median
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_4_Median <- df2_An03_06_Height_Summ_ByTrt %>%
        dplyr::summarise(res = median(HEIGHTBL)) %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_4_Median',
               OutputId = 'Out14-1-1',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_5_Q1
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_5_Q1 <- df2_An03_06_Height_Summ_ByTrt %>%
        dplyr::summarise(res = quantile(HEIGHTBL, c(.25), na.rm = TRUE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_5_Q1',
               OutputId = 'Out14-1-1',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_6_Q3
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_6_Q3 <- df2_An03_06_Height_Summ_ByTrt %>%
        dplyr::summarise(res = quantile(HEIGHTBL, c(.75), na.rm = TRUE)) %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_6_Q3',
               OutputId = 'Out14-1-1',
               pattern = 'XX.X')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_7_Min
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_7_Min <- df2_An03_06_Height_Summ_ByTrt %>%
        dplyr::summarise(res = min(HEIGHTBL)) %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_7_Min',
               OutputId = 'Out14-1-1',
               pattern = 'XX')

 
# Operation ID:           Mth02_ContVar_Summ_ByGrp_8_Max
# Operation name:         Summary by group of a continuous variable
# Operation description:  Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_8_Max <- df2_An03_06_Height_Summ_ByTrt %>%
        dplyr::summarise(res = max(HEIGHTBL)) %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OperationId = 'Mth02_ContVar_Summ_ByGrp_8_Max',
               OutputId = 'Out14-1-1',
               pattern = 'XX')

#Combine operation datasets: 
df3_An03_06_Height_Summ_ByTrt <- dplyr::bind_rows(df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_1_n, 
df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_2_Mean, 
df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_3_SD, 
df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_4_Median, 
df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_5_Q1, 
df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_6_Q3, 
df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_7_Min, 
df3_An03_06_Height_Summ_ByTrt_Mth02_ContVar_Summ_ByGrp_8_Max) %>%
      dplyr::rename(Group1 = TRT01A)


# Analysis An03_06_Height_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  SAF
df_An03_06_Height_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_An03_06_Height_Comp_ByTrt <- df_An03_06_Height_Comp_ByTrt


#Apply Data Subset ---
df2_An03_06_Height_Comp_ByTrt <- df1_An03_06_Height_Comp_ByTrt

#Apply Operations --- 
 
# Operation ID:           Mth04_ContVar_Comp_Anova_1_pval
# Operation name:         Analysis of variance group comparison for a continuous variable
# Operation description:  Comparison of groups by analysis of variance (ANOVA) for a continuous variable


fm <- stats::as.formula(paste('HEIGHTBL', '~', 'TRT01A'))
  model <- stats::lm(fm, data = df2_An03_06_Height_Comp_ByTrt
  )

if (class(model) != 'lm') stop('Not an object of class lm ')
f <- summary(model)$fstatistic
p <- stats::pf(f[1],f[2],f[3],lower.tail=F)
attributes(p) <- NULL

df3_An03_06_Height_Comp_ByTrt_Mth04_ContVar_Comp_Anova_1_pval <- data.frame(res = p,
                  AnalsysisId = 'An03_06_Height_Comp_ByTrt',
                  MethodId = 'Mth04_ContVar_Comp_Anova',
                  OperationId = 'Mth04_ContVar_Comp_Anova_1_pval',
                  OutputId = 'Out14-1-1',
                  pattern = 'X.XXXX')
#Combine operation datasets: 
df3_An03_06_Height_Comp_ByTrt <- dplyr::bind_rows(df3_An03_06_Height_Comp_ByTrt_Mth04_ContVar_Comp_Anova_1_pval)

# combine analyses to create ARD ----
df4 <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt, 
df3_An03_01_Age_Summ_ByTrt, 
df3_An03_01_Age_Comp_ByTrt, 
df3_An03_02_AgeGrp_Summ_ByTrt, 
df3_An03_02_AgeGrp_Comp_ByTrt, 
df3_An03_03_Sex_Summ_ByTrt, 
df3_An03_03_Sex_Comp_ByTrt, 
df3_An03_04_Ethnic_Summ_ByTrt, 
df3_An03_04_Ethnic_Comp_ByTrt, 
df3_An03_05_Race_Summ_ByTrt, 
df3_An03_05_Race_Comp_ByTrt, 
df3_An03_06_Height_Summ_ByTrt, 
df3_An03_06_Height_Comp_ByTrt)

 #Apply pattern format:
ARD_Out14_1_1<- df4 %>%
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
