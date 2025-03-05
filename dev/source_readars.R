readARS <-
function (JSON_ARS, output_path = tempdir(), adam_path = "")
{
  func_libraries <- function() {
    template <- "\n\n# load libraries ----\nlibrary(tidyverse)\nlibrary(readxl)\nlibrary(splitstackshape)\nlibrary(readr)\n  "
    code <- template
    return(code)
  }
  code_libraries <- func_libraries()
  json_from <- jsonlite::fromJSON(JSON_ARS)
  otherListsOfContents <- json_from$otherListsOfContents$contentsList$listItems[[1]]
  mainListOfContents <- json_from$mainListOfContents$contentsList$listItems
  Lopa <- data.frame()
  for (a in 1:nrow(otherListsOfContents)) {
    tmp_PO <- otherListsOfContents[a, ]
    tmp_json_Lopa <- mainListOfContents$sublist$listItems[[a]]
    anaIds <- tmp_json_Lopa$analysisId %>% tibble::as_tibble() %>%
      dplyr::mutate(outputId = tmp_PO$outputId) %>% dplyr::rename(analysisId = value) %>%
      dplyr::filter(!is.na(analysisId))
    Lopa <- rbind(Lopa, anaIds)
    if ("sublist" %in% names(tmp_json_Lopa)) {
      tmp_json_lopa_sub <- tmp_json_Lopa$sublist$listItems
      forend <- length(tmp_json_lopa_sub)
      subana_dset <- data.frame()
      for (b in 2:forend) {
        ana_ids <- tmp_json_lopa_sub[[b]]$analysisId %>%
          tibble::as_tibble() %>% dplyr::mutate(outputId = tmp_PO$outputId) %>%
          dplyr::rename(analysisId = value)
        subana_dset <- rbind(subana_dset, ana_ids)
      }
      Lopa <- rbind(Lopa, subana_dset)
    }
  }
  JSON_DataSubsets <- json_from$dataSubsets
  JSONDSL1 <- tibble::tibble(id = json_from$dataSubsets[["id"]],
                             name = json_from$dataSubsets[["name"]], order = json_from$dataSubsets[["order"]],
                             level = json_from$dataSubsets[["level"]], condition_dataset = json_from[["dataSubsets"]][["condition"]][["dataset"]],
                             condition_variable = json_from[["dataSubsets"]][["condition"]][["variable"]],
                             condition_comparator = json_from[["dataSubsets"]][["condition"]][["comparator"]],
                             condition_value = json_from[["dataSubsets"]][["condition"]][["value"]],
                             compoundExpression_logicalOperator = json_from[["dataSubsets"]][["compoundExpression"]][["logicalOperator"]])
  whereClauses <- JSON_DataSubsets[["compoundExpression"]][["whereClauses"]]
  JSONDSL2 <- data.frame()
  JSONDSL3 <- data.frame()
  for (c in 1:nrow(JSON_DataSubsets)) {
    tmp_DSID <- JSON_DataSubsets[c, "id"]
    tmp_DSname <- JSON_DataSubsets[c, "name"]
    tmp_DS_c <- JSON_DataSubsets[c, ]
    if (!is.null(whereClauses[[c]])) {
      tmp_DS <- tibble::tibble(level = whereClauses[[c]][["level"]],
                               order = whereClauses[[c]][["order"]], condition_dataset = whereClauses[[c]][["condition"]][["dataset"]],
                               condition_variable = whereClauses[[c]][["condition"]][["variable"]],
                               condition_comparator = whereClauses[[c]][["condition"]][["comparator"]],
                               condition_value = whereClauses[[c]][["condition"]][["value"]],
                               compoundExpression_logicalOperator = whereClauses[[c]]$compoundExpression$logicalOperator,
                               id = tmp_DSID, name = tmp_DSname)
      JSONDSL2 = dplyr::bind_rows(JSONDSL2, tmp_DS)
      whereClausesL2 <- whereClauses[[c]][["compoundExpression"]][["whereClauses"]]
      for (d in 1:nrow(tmp_DS)) {
        if (!is.null(whereClausesL2[[d]])) {
          tmp_DSL2 <- tibble::tibble(level = whereClausesL2[[d]][["level"]],
                                     order = whereClausesL2[[d]][["order"]], condition_dataset = whereClausesL2[[d]][["condition"]][["dataset"]],
                                     condition_variable = whereClausesL2[[d]][["condition"]][["variable"]],
                                     condition_comparator = whereClausesL2[[d]][["condition"]][["comparator"]],
                                     condition_value = whereClausesL2[[d]][["condition"]][["value"]],
                                     id = tmp_DSID, name = tmp_DSname)
          JSONDSL3 = dplyr::bind_rows(JSONDSL3, tmp_DSL2)
        }
      }
    }
  }
  DataSubsets <- dplyr::bind_rows(JSONDSL1, JSONDSL2, JSONDSL3) %>%
    dplyr::arrange(id, level, order)
  DataSubsets$condition_value[DataSubsets$condition_value ==
                                "NULL"] = NA
  AnalysisSets <- tibble::tibble(id = json_from$analysisSets$id,
                                 label = json_from$analysisSets$label, name = json_from$analysisSets$name,
                                 level = json_from$analysisSets$level, order = json_from$analysisSets$order,
                                 condition_dataset = json_from$analysisSets$condition[["dataset"]],
                                 condition_variable = json_from$analysisSets$condition[["variable"]],
                                 condition_comparator = json_from$analysisSets$condition[["comparator"]],
                                 condition_value = json_from$analysisSets$condition[["value"]])
  JSON_AnalysisGroupings <- json_from$analysisGroupings
  JSON_AG_1 <- tibble::tibble(id = json_from$analysisGroupings$id,
                              name = json_from$analysisGroupings$name, groupingDataset = json_from$analysisGroupings$groupingDataset,
                              groupingVariable = json_from$analysisGroupings$groupingVariable,
                              dataDriven = json_from$analysisGroupings$dataDriven)
  JSON_AG <- data.frame()
  for (e in 1:nrow(JSON_AG_1)) {
    AG_ID <- JSON_AG_1[e, "id"] %>% as.character()
    AG_name <- JSON_AG_1[e, "name"] %>% as.character()
    AG_groupingVariable <- JSON_AG_1[e, "groupingVariable"] %>%
      as.character()
    AG_dataDriven <- JSON_AG_1[e, "dataDriven"] %>% as.character()
    tmp_AG <- tibble::tibble(group_id = JSON_AnalysisGroupings[["groups"]][[e]]$id,
                             group_name = JSON_AnalysisGroupings[["groups"]][[e]]$name,
                             group_level = JSON_AnalysisGroupings[["groups"]][[e]]$level,
                             group_order = JSON_AnalysisGroupings[["groups"]][[e]]$order,
                             group_condition_dataset = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["dataset"]],
                             group_condition_variable = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["variable"]],
                             group_condition_comparator = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["comparator"]],
                             group_condition_value = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["value"]],
                             id = AG_ID, name = AG_name, groupingVariable = AG_groupingVariable,
                             dataDriven = AG_dataDriven)
    JSON_AG <- dplyr::bind_rows(JSON_AG, tmp_AG)
  }
  AnalysisGroupings <- dplyr::bind_rows(JSON_AG)
  JSON_AN <- json_from$analyses
  JSON_AnalysesL1 <- tibble::tibble(id = JSON_AN$id, name = JSON_AN$name,
                                    label = JSON_AN$label, version = JSON_AN$version, categoryIds = JSON_AN$categoryIds,
                                    method_id = JSON_AN$methodId, analysisSetId = JSON_AN$analysisSetId,
                                    dataset = JSON_AN$dataset, variable = JSON_AN$variable,
                                    dataSubsetId = JSON_AN$dataSubsetId)
  AN_groupings <- data.frame()
  for (g in 1:nrow(JSON_AnalysesL1)) {
    tmp_id <- JSON_AN[g, ]$id %>% as.character()
    tmp <- JSON_AN[["orderedGroupings"]][[g]] %>% tidyr::pivot_wider(names_from = order,
                                                                     values_from = c(resultsByGroup, groupingId), names_glue = "{.value}{order}") %>%
      dplyr::mutate(id = tmp_id)
    AN_groupings <- dplyr::bind_rows(AN_groupings, tmp)
  }
  AN_refs <- data.frame()
  for (h in 1:nrow(JSON_AnalysesL1)) {
    tmp_id <- JSON_AN[h, ]$id %>% as.character()
    if (!is.null(JSON_AN[["referencedAnalysisOperations"]][[h]])) {
      tmp_ref <- JSON_AN[["referencedAnalysisOperations"]][[h]] %>%
        dplyr::mutate(order = dplyr::row_number()) %>%
        tidyr::pivot_wider(names_from = order, values_from = c(referencedOperationRelationshipId,
                                                               analysisId), names_glue = "{'referencedAnalysisOperations_'}{.value}{order}") %>%
        dplyr::mutate(id = tmp_id)
      AN_refs <- dplyr::bind_rows(AN_refs, tmp_ref)
    }
  }
  colnames(AN_refs) <- gsub("Relationship", "", colnames(AN_refs))
  Analyses <- merge(JSON_AnalysesL1, AN_refs, by = "id", all.x = TRUE) %>%
    merge(AN_groupings, by = "id", all.x = TRUE)
  JSONAML1 <- tibble::tibble(id = json_from$methods$id, name = json_from$methods$name,
                             description = json_from$methods$description, label = json_from$methods$label)
  JSONAML2 <- data.frame()
  JSONAML3 <- data.frame()
  for (i in 1:nrow(JSONAML1)) {
    tmp_l2 <- tibble::tibble(operation_id = json_from$methods$operations[[i]]$id,
                             operation_name = json_from$methods$operations[[i]]$name,
                             operation_resultPattern = json_from$methods$operations[[i]]$resultPattern,
                             operation_label = json_from$methods$operations[[i]]$label,
                             operation_order = json_from$methods$operations[[i]]$order,
                             id = JSONAML1[i, ]$id %>% as.character())
    JSONAML2 <- dplyr::bind_rows(JSONAML2, tmp_l2)
    rOF <- json_from$methods$operations[[i]]$referencedOperationRelationships
    if (!is.null(rOF)) {
      lenrOF <- length(rOF)
      for (j in 1:lenrOF) {
        if (!is.null(rOF[[j]])) {
          tmp_l3 <- tibble::tibble(id = rOF[[j]]$id,
                                   operationId = rOF[[j]]$operationId, description = rOF[[j]]$description,
                                   referencedOperationRole = rOF[[j]]$referencedOperationRole$controlledTerm)
          tmp_l3_fin <- tmp_l3 %>% dplyr::mutate(order = dplyr::row_number()) %>%
            tidyr::pivot_wider(names_from = order, values_from = c(id,
                                                                   operationId, description, referencedOperationRole),
                               names_glue = "{'operation_referencedResultRelationships'}{order}{'_'}{.value}") %>%
            dplyr::mutate(operation_id = json_from[["methods"]][["operations"]][[i]][["id"]][[j]])
          JSONAML3 = dplyr::bind_rows(JSONAML3, tmp_l3_fin)
        }
      }
    }
  }
  AnalysisMethods <- merge(JSONAML1, JSONAML2, by = "id", all = TRUE) %>%
    merge(JSONAML3, by = "operation_id", all = TRUE)
  adam_loc <- adam_path
  func_ADaM <- function(adampath) {
    template <- "\n# load ADaM ----\nADSL <- read_csv('adampathhere/ADSL.csv')\nADAE <- read_csv('adampathhere/ADAE.csv') %>%\n  rename(TRT01A = TRTA)\nADVS <- read_csv('adampathhere/ADVS.csv') %>%\n  rename(TRT01A = TRTA)\n  "
    code <- gsub("adampathhere", adampath, template)
    return(code)
  }
  code_ADaM <- func_ADaM(adam_loc)
  Lopo <- otherListsOfContents
  for (i in 1:nrow(Lopo)) {
    Output = Lopo[i, ]$outputId
    OutputName = Lopo[i, ]$name
    Anas <- Lopa %>% dplyr::filter(outputId == Output)
    run_code <- ""
    combine_analysis_code <- ""
    timenow <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    func_header <- function(OutputId, OutputName, date) {
      template <- "\n# Programme:    Generate code to produce ARD for outputidhere\n# Output:       outputnamehere\n# Date created: datehere\n\n\n  "
      code <- gsub("outputidhere", OutputId, template)
      code <- gsub("outputnamehere", OutputName, code)
      code <- gsub("datehere", date, code)
      return(code)
    }
    code_header <- func_header(Output, OutputName, timenow)
    max_j = nrow(Anas)
    for (j in 1:max_j) {
      Anas_j <- Anas[j, ]$analysisId
      Anas_s <- Analyses %>% dplyr::filter(id == Anas_j)
      ana_adam <- Anas_s$dataset
      ana_setId <- Anas_s$analysisSetId
      ana_var <- Anas_s$variable
      groupid1 <- Anas_s$groupingId1
      resultsByGroup1 <- Anas_s$resultsByGroup1
      groupid2 <- Anas_s$groupingId2
      resultsByGroup2 <- Anas_s$resultsByGroup2
      groupid3 <- Anas_s$groupingId3
      resultsByGroup3 <- Anas_s$resultsByGroup3
      subsetid <- Anas_s$dataSubsetId
      methodid <- Anas_s$method_id
      temp_AnSet <- AnalysisSets %>% dplyr::filter(id ==
                                                     ana_setId)
      cond_adam <- temp_AnSet %>% dplyr::select(condition_dataset) %>%
        as.character()
      cond_var <- temp_AnSet %>% dplyr::select(condition_variable) %>%
        as.character()
      cond_oper <- temp_AnSet %>% dplyr::select(condition_comparator) %>%
        as.character()
      cond_val <- temp_AnSet %>% dplyr::select(condition_value) %>%
        unlist()
      anSetName <- temp_AnSet %>% dplyr::select(label) %>%
        as.character()
      print(cond_oper) # LG 20250303
      if (cond_oper == "EQ") {
        oper <- "=="
      }
      if (cond_adam == ana_adam) {
        func_AnalysisSet1 <- function(dataset, variable,
                                      oper, val, ASID, anSetName) {
          template <- "\n# Apply Analysis Set ---\n# Analysis set :  Analysissetnamehere\ndf_analysisidhere <- dplyr::filter(ADaM,\n            var operator 'value')\n\n"
          code <- gsub("ADaM", dataset, template)
          code <- gsub("var", variable, code)
          code <- gsub("operator", oper, code)
          code <- gsub("value", val, code)
          code <- gsub("analysisidhere", ASID, code)
          code <- gsub("Analysissetnamehere", anSetName,
                       code)
          return(code)
        }
        assign(paste0("code_AnalysisSet_", Anas_j), func_AnalysisSet1(cond_adam,
                                                                      cond_var, oper, cond_val, Anas_j, anSetName))
      }
      else {
        func_AnalysisSet2 <- function(dataset, variable,
                                      oper, val, ASID, anaADaM, anSetName) {
          template <- "\n# Apply Analysis Set ---\n# Analysis set :  Analysissetnamehere\ndf_analysisidhere <- dplyr::filter(ADaM,\n            var operator 'value') %>%\n            dplyr::select(USUBJID) %>%\n            merge(analysisADAMhere,\n                  by = 'USUBJID')\n\n"
          code <- gsub("ADaM", dataset, template)
          code <- gsub("var", variable, code)
          code <- gsub("operator", oper, code)
          code <- gsub("value", val, code)
          code <- gsub("analysisidhere", ASID, code)
          code <- gsub("analysisADAMhere", anaADaM, code)
          code <- gsub("Analysissetnamehere", anSetName,
                       code)
          return(code)
        }
        assign(paste0("code_AnalysisSet_", Anas_j), func_AnalysisSet2(cond_adam,
                                                                      cond_var, oper, cond_val, Anas_j, ana_adam,
                                                                      anSetName))
      }
      column_names <- colnames(Analyses)
      group_columns <- column_names[grep("^groupingId[0-9]+$",
                                         column_names)]
      group_numbers <- as.numeric(sub("groupingId", "",
                                      group_columns))
      max_group_number <- max(group_numbers)
      if (max_group_number >= 1) {
        AG_temp1 <- AnalysisGroupings %>% dplyr::filter(id ==
                                                          groupid1)
        AG_var1 <- AG_temp1 %>% dplyr::select(groupingVariable) %>%
          unique() %>% as.character()
        if (resultsByGroup1 == TRUE && !is.na(resultsByGroup1)) {
          num_grp <- 1
        }
        else num_grp = 0
        if (max_group_number >= 2) {
          AG_temp2 <- AnalysisGroupings %>% dplyr::filter(id ==
                                                            groupid2)
          AG_var2 <- AG_temp2 %>% dplyr::select(groupingVariable) %>%
            unique() %>% as.character()
          if (resultsByGroup1 == TRUE && !is.na(resultsByGroup1)) {
            num_grp <- 1
            if (resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
              num_grp <- 2
            }
          }
          else num_grp = 0
          if (max_group_number >= 3) {
            AG_temp3 <- AnalysisGroupings %>% dplyr::filter(id ==
                                                              groupid3)
            AG_var3 <- AG_temp3 %>% dplyr::select(groupingVariable) %>%
              unique() %>% as.character()
            if (resultsByGroup1 == TRUE && !is.na(resultsByGroup1)) {
              num_grp <- 1
              if (resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
                num_grp <- 2
                if (resultsByGroup3 == TRUE && !is.na(resultsByGroup3)) {
                  num_grp <- 3
                }
              }
            }
            else num_grp = 0
          }
        }
      }
      if (num_grp == 1) {
        func_AnalysisGrouping1 <- function(var1, ASID) {
          template <- "\n\n#Apply Analysis Grouping ---\ndf1_analysisidhere <- df_analysisidhere %>%\n          dplyr::group_by(var)\n\n"
          code <- gsub("var", var1, template)
          code <- gsub("analysisidhere", ASID, code)
        }
        code_AnalysisGrouping_0 <- func_AnalysisGrouping1(AG_var1,
                                                          Anas_j)
      }
      else if (num_grp == 2) {
        func_AnalysisGrouping2 <- function(var1, var2,
                                           ASID) {
          template <- "\n\n#Apply Analysis Grouping ---\ndf1_analysisidhere <- df_analysisidhere %>%\n          dplyr::group_by(var1, var2)\n\n"
          code <- gsub("var1", var1, template)
          code <- gsub("var2", var2, code)
          code <- gsub("analysisidhere", ASID, code)
          return(code)
        }
        code_AnalysisGrouping_0 <- func_AnalysisGrouping2(AG_var1,
                                                          AG_var2, Anas_j)
      }
      else if (num_grp == 3) {
        func_AnalysisGrouping3 <- function(var1, var2,
                                           var3, ASID) {
          template <- "\n\n#Apply Analysis Grouping ---\ndf1_analysisidhere <- df_analysisidhere %>%\n          dplyr::group_by(var1, var2, var3)\n\n"
          code <- gsub("var1", var1, template)
          code <- gsub("var2", var2, code)
          code <- gsub("var3", var3, code)
          code <- gsub("analysisidhere", ASID, code)
          return(code)
        }
        code_AnalysisGrouping_0 <- func_AnalysisGrouping3(AG_var1,
                                                          AG_var2, AG_var3, Anas_j)
      }
      else {
        func_AnalysisGrouping4 <- function(ASID) {
          template <- "\n\n#Apply Analysis Grouping ---\n\n# (No grouping applicable for this analysis)\ndf1_analysisidhere <- df_analysisidhere\n"
          code <- gsub("analysisidhere", ASID, template)
          return(code)
        }
        code_AnalysisGrouping_0 <- func_AnalysisGrouping4(Anas_j)
      }
      assign(paste0("code_AnalysisGrouping_", Anas_j),
             code_AnalysisGrouping_0)
      if (exists("DataSubsets")) {
        if (!is.na(subsetid)) {
          subsetrule <- DataSubsets %>% dplyr::filter(id ==
                                                        subsetid)
          DSname <- subsetrule %>% dplyr::select(name) %>%
            unique() %>% as.character()
          if (nrow(subsetrule) == 1) {
            var = subsetrule$condition_variable
            val1 = stringr::str_trim(subsetrule$condition_value)
            vac = subsetrule$condition_comparator
            if (vac == "EQ")
              rvac = "=="
            if (vac == "NE")
              rvac = "!="
            if (vac == "GT")
              rvac = ">"
            if (vac == "GE")
              rvac = ">="
            if (vac == "LT")
              rvac = "<"
            if (vac == "LE")
              rvac = "<="
            rFilt_final <- paste0(var, " ", rvac, " ",
                                  "'", val1, "'")
          }
          else {
            for (m in 1:(max(subsetrule$level) - 1)) {
              log_oper = subsetrule %>% dplyr::filter(level ==
                                                        m, !is.na(compoundExpression_logicalOperator)) %>%
                dplyr::select(compoundExpression_logicalOperator) %>%
                as.character()
              if (log_oper == "character(0)")
                log_oper = NA
              assign(paste("log_oper", m, sep = ""),
                     log_oper)
              if (!is.na(log_oper)) {
                if (log_oper == "AND")
                  rlog_oper = "&"
                else if (log_oper == "OR")
                  rlog_oper = "|"
                else rlog_oper = NA
              }
              lev = subsetrule %>% dplyr::filter(level ==
                                                   m + 1, is.na(compoundExpression_logicalOperator))
              rcode <- ""
              for (n in 1:nrow(lev)) {
                ord1_ <- lev[n, ]
                var = ord1_$condition_variable
                vac = ord1_$condition_comparator
                val1 = ord1_$condition_value
                if (vac == "IN") {
                  f_vac = "%in%"
                }
                else {
                  if (vac == "EQ")
                    f_vac = "=="
                  else f_vac = "!="
                }
                assign(paste("fexp", m, n, sep = "_"),
                       paste0(var, " ", f_vac, " ", "'", val1,
                              "'"))
                if (n > 1)
                  assign("rcode", paste0(rcode, " LOGOP ",
                                         var, " ", f_vac, " ", "'", val1,
                                         "'"))
                else assign("rcode", paste0(var, " ",
                                            f_vac, " ", "'", val1, "'"))
              }
              assign(paste("rFilt", m, sep = "_"), gsub("LOGOP",
                                                        rlog_oper, rcode))
            }
            if (exists("rFilt_2")) {
              rFilt_final <- paste(rFilt_1, rFilt_2,
                                   sep = ", ")
              rm(rFilt_2)
            }
            else rFilt_final <- rFilt_1
          }
          func_DataSubset1 <- function(filterVal, ASID,
                                       DSNAME) {
            template <- "\n\n# Apply Data Subset ---\n# Data subset: dsnamehere\ndf2_analysisidhere <- df1_analysisidhere %>%\n        dplyr::filter(dplyr::filtertext1)\n\n"
            code <- gsub("dplyr::filtertext1", filterVal,
                         template)
            code <- gsub("analysisidhere", ASID, code)
            code <- gsub("dsnamehere", DSNAME, code)
            return(code)
          }
          assign(paste0("code_DataSubset_", Anas_j),
                 func_DataSubset1(rFilt_final, Anas_j, DSname))
        }
        else {
          func_DataSubset2 <- function(ASID) {
            template <- "\n\n#Apply Data Subset ---\ndf2_analysisidhere <- df1_analysisidhere\n\n"
            code <- gsub("analysisidhere", ASID, template)
            return(code)
          }
          assign(paste0("code_DataSubset_", Anas_j),
                 func_DataSubset2(Anas_j))
        }
      }
      else {
        func_DataSubset3 <- function(ASID) {
          template <- "\n\n#Apply Data Subset ---\ndf2_analysisidhere <- df1_analysisidhere\n\n"
          code <- gsub("analysisidhere", ASID, template)
          return(code)
        }
        assign(paste0("code_DataSubset_", Anas_j), func_DataSubset3(Anas_j))
      }
      method <- AnalysisMethods %>% dplyr::filter(id ==
                                                    methodid)
      code_Operation_0 = ""
      code_combine = ""
      for (k in 1:nrow(method)) {
        operation = method[k, ]
        oper_id <- operation$operation_id
        oper_name <- operation$name
        oper_desc <- operation$description
        oper_pattern <- operation$operation_resultPattern
        if (oper_id == "Mth01_CatVar_Count_ByGrp_1_n") {
          func_OperationTmp1 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = n()) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp1(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  oper_pattern)
        }
        else if (operation$operation_id == "Mth02_ContVar_Summ_ByGrp_1_n") {
          func_OperationTmp2 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = n()) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp2(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  oper_pattern)
        }
        else if (operation$operation_id == "Mth02_ContVar_Summ_ByGrp_2_Mean") {
          func_OperationTmp3 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         analysisvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = mean(ana_varhere)) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp3(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  ana_var, oper_pattern)
        }
        else if (operation$operation_id == "Mth02_ContVar_Summ_ByGrp_3_SD") {
          func_OperationTmp4 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         analysisvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = sd(ana_varhere)) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp4(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  ana_var, oper_pattern)
        }
        else if (operation$operation_id == "Mth02_ContVar_Summ_ByGrp_4_Median") {
          func_OperationTmp5 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         analysisvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = median(ana_varhere)) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp5(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  ana_var, oper_pattern)
        }
        else if (operation$operation_id == "Mth02_ContVar_Summ_ByGrp_5_Q1") {
          func_OperationTmp6 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         analysisvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = quantile(ana_varhere, c(.25), na.rm = TRUE)) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp6(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  ana_var, oper_pattern)
        }
        else if (operation$operation_id == "Mth02_ContVar_Summ_ByGrp_6_Q3") {
          func_OperationTmp7 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         analysisvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = quantile(ana_varhere, c(.75), na.rm = TRUE)) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp7(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  ana_var, oper_pattern)
        }
        else if (operation$operation_id == "Mth02_ContVar_Summ_ByGrp_7_Min") {
          func_OperationTmp8 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         analysisvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = min(ana_varhere)) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp8(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  ana_var, oper_pattern)
        }
        else if (operation$operation_id == "Mth02_ContVar_Summ_ByGrp_8_Max") {
          func_OperationTmp9 <- function(operid, opername,
                                         operdesc, analysisid, methodid, outputid,
                                         analysisvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = max(ana_varhere)) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp9(oper_id,
                                                  oper_name, oper_desc, Anas_j, methodid, Output,
                                                  ana_var, oper_pattern)
        }
        else if (operation$operation_id == "Mth01_CatVar_Summ_ByGrp_1_n") {
          func_OperationTmp10 <- function(operid, opername,
                                          operdesc, analysisid, methodid, outputid,
                                          analysisvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ndf3_analysisidhere_operationidhere <- df2_analysisidhere %>%\n        dplyr::summarise(res = n_distinct(ana_varhere)) %>%\n        dplyr::mutate(AnalsysisId = 'analysisidhere',\n               MethodId = 'methodidhere',\n               OperationId = 'operationidhere',\n               OutputId = 'outputidhere',\n               pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp10(oper_id,
                                                   oper_name, oper_desc, Anas_j, methodid, Output,
                                                   ana_var, oper_pattern)
        }
        else if (operation$operation_id == "Mth01_CatVar_Summ_ByGrp_2_pct") {
          NUM_analysisid = Anas_s$referencedAnalysisOperations_analysisId1
          DEN_analysisid = Anas_s$referencedAnalysisOperations_analysisId2
          NUM_operationid = operation$operation_referencedResultRelationships1_operationId
          DEN_operationid = operation$operation_referencedResultRelationships2_operationId
          func_OperationTmp11 <- function(operid, opername,
                                          operdesc, analysisid, methodid, outputid,
                                          analysisvar, num_analysisid, den_analysisid,
                                          num_operationid, den_operationid, pattern,
                                          groupvar1) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\n\n\ndf3_analysisidhere_operationidhere_num <- df3_num_analysisIDhere_num_operationIDhere %>%\n          dplyr::rename(NUM = res)\n\ndf3_analysisidhere_operationidhere_den <- df3_den_analysisIDhere_den_operationIDhere %>%\n          dplyr::rename(DEN = res)\n\ndf3_analysisidhere_operationidhere <- merge(df3_analysisidhere_operationidhere_num,\n                                            df3_analysisidhere_operationidhere_den %>%\n                                                  dplyr::select(group1varhere, DEN),\n                                            by = c('group1varhere')) %>%\n                                            dplyr::mutate(res = NUM / DEN * 100,\n                                                   OperationId = 'operationidhere',\n                                                   pattern = 'patternhere') %>%\n                                            dplyr::select(-NUM, -DEN)\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("num_analysisIDhere", num_analysisid,
                         code)
            code <- gsub("den_analysisIDhere", den_analysisid,
                         code)
            code <- gsub("num_operationIDhere", num_operationid,
                         code)
            code <- gsub("den_operationIDhere", den_operationid,
                         code)
            code <- gsub("patternhere", pattern, code)
            code <- gsub("group1varhere", groupvar1,
                         code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp11(oper_id,
                                                   oper_name, oper_desc, Anas_j, methodid, Output,
                                                   ana_var, NUM_analysisid, DEN_analysisid,
                                                   NUM_operationid, DEN_operationid, oper_pattern,
                                                   AG_var1)
        }
        else if (operation$operation_id == "Mth04_ContVar_Comp_Anova_1_pval") {
          func_OperationTmp12 <- function(operid, opername,
                                          operdesc, analysisid, methodid, outputid,
                                          analysisvar, AGvar, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\n\nfm <- stats::as.formula(paste('ana_varhere', '~', 'ana_groupvarhere'))\n  model <- stats::lm(fm, data = df2_analysisidhere\n  )\n\nif (class(model) != 'lm') stop('Not an object of class lm ')\nf <- summary(model)$fstatistic\np <- stats::pf(f[1],f[2],f[3],lower.tail=F)\nattributes(p) <- NULL\n\ndf3_analysisidhere_operationidhere <- data.frame(res = p,\n                  AnalsysisId = 'analysisidhere',\n                  MethodId = 'methodidhere',\n                  OperationId = 'operationidhere',\n                  OutputId = 'outputidhere',\n                  pattern = 'patternhere')\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("ana_groupvarhere", AGvar, code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp12(oper_id,
                                                   oper_name, oper_desc, Anas_j, methodid, Output,
                                                   ana_var, AG_var1, oper_pattern)
        }
        else if (operation$operation_id == "Mth03_CatVar_Comp_PChiSq_1_pval") {
          func_OperationTmp13 <- function(operid, opername,
                                          operdesc, analysisid, methodid, outputid,
                                          analysisvar, AGvar1, AGvar2, ana_adam, pattern) {
            template <- "\n# Operation ID:           operationidhere\n# Operation name:         operationnamehere\n# Operation description:  operationdeschere\n\ntab <- table(adamhere[, c('ana_groupvar1here', 'ana_groupvar2here')])\np <- chisq.test(tab)$p.value\n\ndf3_analysisidhere_operationidhere <- data.frame(res = p,\n                  AnalsysisId = 'analysisidhere',\n                  MethodId = 'methodidhere',\n                  OperationId = 'operationidhere',\n                  OutputId = 'outputidhere',\n                  pattern = 'patternhere')\n\n"
            code <- gsub("operationidhere", operid, template)
            code <- gsub("operationnamehere", opername,
                         code)
            code <- gsub("operationdeschere", operdesc,
                         code)
            code <- gsub("analysisidhere", analysisid,
                         code)
            code <- gsub("methodidhere", methodid, code)
            code <- gsub("outputidhere", outputid, code)
            code <- gsub("ana_varhere", analysisvar,
                         code)
            code <- gsub("ana_groupvar1here", AGvar1,
                         code)
            code <- gsub("ana_groupvar2here", AGvar2,
                         code)
            code <- gsub("adamhere", ana_adam, code)
            code <- gsub("patternhere", pattern, code)
            return(code)
          }
          code_Operation_tmp = func_OperationTmp13(oper_id,
                                                   oper_name, oper_desc, Anas_j, methodid, Output,
                                                   ana_var, AG_var1, AG_var2, ana_adam, oper_pattern)
        }
        code_Operation_0 = paste(code_Operation_0, code_Operation_tmp)
        if (k < nrow(method)) {
          code_combine = paste0(code_combine, "df3_",
                                Anas_j, "_", oper_id, ", \n")
        }
        else {
          code_combine = paste0(code_combine, "df3_",
                                Anas_j, "_", oper_id)
        }
      }
      if (num_grp == 1) {
        func_rename1 <- function(groupvar1) {
          template <- " %>%\n      dplyr::rename(Group1 = groupvar1here)\n"
          code <- gsub("groupvar1here", groupvar1, template)
          return(code)
        }
        code_rename = func_rename1(AG_var1)
      }
      else if (num_grp == 2) {
        func_rename2 <- function(groupvar1, groupvar2) {
          template <- " %>%\n      dplyr::rename(Group1 = groupvar1here,\n             Group2 = groupvar2here)\n"
          code <- gsub("groupvar1here", groupvar1, template)
          code <- gsub("groupvar2here", groupvar2, code)
          return(code)
        }
        code_rename = func_rename2(AG_var1, AG_var2)
      }
      else if (num_grp == 3) {
        func_rename3 <- function(groupvar1, groupvar2,
                                 groupvar3) {
          template <- " %>%\n      dplyr::rename(Group1 = groupvar1here,\n             Group2 = groupvar2here,\n             Group3 = groupvar3here)\n"
          code <- gsub("groupvar1here", groupvar1, template)
          code <- gsub("groupvar2here", groupvar2, code)
          code <- gsub("groupvar3here", groupvar3, code)
          return(code)
        }
        code_rename = func_rename3(AG_var1, AG_var2,
                                   AG_var3)
      }
      else code_rename = ""
      assign(paste0("code_Operation_", Anas_j), paste0("#Apply Operations --- \n",
                                                       code_Operation_0, "#Combine operation datasets: \n",
                                                       "df3_", Anas_j, " <- dplyr::bind_rows(", code_combine,
                                                       ")", code_rename))
      assign(paste0("code_", Anas_j), paste0("\n\n# Analysis ",
                                             Anas_j, "----", get(paste0("code_AnalysisSet_",
                                                                        Anas_j)), get(paste0("code_AnalysisGrouping_",
                                                                                             Anas_j)), get(paste0("code_DataSubset_", Anas_j)),
                                             get(paste0("code_Operation_", Anas_j))))
      run_code <- paste0(run_code, get(paste0("code_",
                                              Anas_j)))
      if (j < max_j) {
        combine_analysis_code = paste0(combine_analysis_code,
                                       "df3_", Anas_j, ", \n")
      }
      else {
        combine_analysis_code = paste0(combine_analysis_code,
                                       "df3_", Anas_j)
      }
    }
    code_pattern <- paste0("ARD_", gsub("-", "_", Output),
                           "<- df4 %>%\n      dplyr::mutate(dec = ifelse(grepl('X.X',\n                                df4$pattern, ),\n                          stringr::str_count(substr(df4$pattern,\n                                          str_locate(df4$pattern,\n                                                    'X.X')[, 1]+2,\n                                          nchar(df4$pattern)), 'X'),\n                          0)) %>%\n      dplyr::rowwise() %>%\n      dplyr::mutate(rnd = round(res, dec)) %>%\n      tibble::as_tibble() %>%\n      dplyr::mutate(disp = ifelse(grepl('\\\\(N=', df4$pattern),\n                           paste0('(N=', rnd, ')'),\n                           ifelse(grepl('\\\\(', df4$pattern),\n                                  paste0('(', rnd, ')'),\n                                  as.character(rnd)))) %>%\n                         dplyr::select(-rnd, -dec)")
    assign(paste0("code_", Output), paste0(code_header, code_libraries,
                                           code_ADaM, run_code, "\n\n# combine analyses to create ARD ----\n",
                                           "df4 <- dplyr::bind_rows(", combine_analysis_code,
                                           ")\n\n #Apply pattern format:\n", code_pattern))
    writeLines(get(paste0("code_", Output)), paste0(output_path,
                                                    "/ARD_", Output, ".R"))
  }
}
