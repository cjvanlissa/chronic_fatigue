# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.

library(worcs)
df <- read.table('data/2026-05-20_PredictionData_selectionInclItems_export_20260408.csv', sep = ";", header = TRUE, stringsAsFactors = FALSE)
names(df) <- tolower(names(df))
#names(df) <- gsub("_recoded", "", names(df), fixed = TRUE)

table3 <- readxl::read_xlsx("data/Tables3_4_Updated3_Caspar.xlsx", 1)
table3$Variable <- tolower(table3$Variable)
table3 <- table3[table3$Variable %in% names(df), ]
#table3 <- table3[-which(table3$`Primary use` == "0"), ]
table3 <- table3[!duplicated(table3$Variable), ]
table3[table3$Variable == "sex", "Type"] <- "Factor"
df_tab3 <- df[, table3$Variable]
table(table3$Type)
ints <- table3$Variable[table3$Type %in% c("Binary", "Count")]
if(!all(sapply(df_tab3[ints], inherits, what = "integer"))) stop()
cnts <- table3$Variable[table3$Type %in% c("Continous")]
df_tab3[cnts] <- lapply(df_tab3[cnts], as.numeric)
if(!all(sapply(df_tab3[cnts], inherits, what = "numeric"))) stop()
dts <- table3$Variable[table3$Type %in% c("Date")]
df_tab3[dts] <- lapply(df_tab3[dts], as.Date, format = "%Y-%m-%d")
if(!all(sapply(df_tab3[dts], inherits, what = "Date"))) stop()
cats <- table3$Variable[table3$Type %in% c("Fac", "Factor")]
df_tab3[cats] <- lapply(df_tab3[cats], factor)
if(!all(sapply(df_tab3[cats], inherits, what = "factor"))) stop()
ord <- table3$Variable[table3$Type %in% c("Ordinal")]
df_tab3[ord] <- lapply(df_tab3[ord], ordered)
df_tab3$o_education_o_ord <- ordered(df_tab3$o_education_o_ord, levels = c("low", "middle", "high"))
if(!all(sapply(df_tab3[ord], inherits, what = "ordered"))) stop()

table4 <- readxl::read_xlsx("data/Tables3_4_Updated3_Caspar.xlsx", 2, skip = 1)
names(table4)[1] <- "Variable"
table4$Variable <- tolower(table4$Variable)

table4 <- table4[table4$Variable %in% names(df) & ! table4$Variable %in% names(df_tab3), ]

df_tab4 <- df[, table4$Variable[which(table4$Variable %in% names(df))]]
df_anal <- cbind(df_tab3, df_tab4)
if(any(duplicated(names(df_anal)))) stop()

# Descriptives
tab_desc <- descriptives(df_anal)
tab_desc$min_cat <- NA
tab_desc$min_cat[tab_desc$type %in% c("factor", "integer", "ordered, factor")] <- sapply(tab_desc$name[tab_desc$type %in% c("factor", "integer", "ordered, factor")], function(v){
  min(table(df_anal[[v]]))
})
tab_desc$min_cat <- tab_desc$min_cat/nrow(df_anal)
write.csv(tab_desc, "tab_desc.csv", row.names = FALSE)

# Handle missings in disact
# df_anal[, grep("^disact", names(df_anal))] <- lapply(df_anal[, grep("^disact", names(df_anal))], function(x){x[is.na(x)] <- 0; return(x)})

# Handle missings in diags
# Just delete the factors, they seem redundant with the bin ones
# vars <- grep("^diag_.+?fac$", names(df_anal), value = TRUE)
# df_anal[vars] <- NULL

# vars <- grep("^diag_.+?bin$", names(df_anal), value = TRUE)
# df_anal[vars][is.na(df_anal[vars])] <- 0

# Add NA as factor level to diagnoses
# vars <- grep("^diag_.+?fac$", names(df_anal), value = TRUE)
# # tmp <- df_anal[,vars]
# #df_anal[vars][is.na(df_anal[vars])] <- "NA"
# df_anal[vars] <- lapply(df_anal[vars], factor)

# Handle missings in disease activity (set to zero)
# vars <- grep("^disact_", names(df_anal), value = TRUE)
# df_anal[vars][is.na(df_anal[vars])] <- 0

# Fix times
df_anal$diff_time <- NA
for(id in unique(df_anal$patientid)){
  #id = df_anal$patientid[1]
  rws <- which(df_anal$patientid == id)
  df_anal$diff_time[rws] <- c(NA, diff(df_anal$date_proactive_mt[rws]))
}

df_anal[c("date_proactive_mt", "questionnairestart", "mt_next_days", "mt_next")] <- NULL

# Drop variables with fewer than 1% of cases
tab_desc <- descriptives(df_anal)
df_anal <- df_anal[, tab_desc$name[tab_desc$missing < .99]]

# Handle outliers
df_anal$disact_ibd_calprotectin_num <- log(df_anal$disact_ibd_calprotectin_num)
df_anal$disact_ibd_calprotectin_num[is.infinite(df_anal$disact_ibd_calprotectin_num)] <- 0

tab_desc <- descriptives(df_anal)
vars <- c(grep("_bin$", names(df_anal), value = TRUE), names(df_anal)[which(sapply(df_anal, inherits, what = "factor"))])

# Handle special cases
# df_anal$diag_ibd_fac[df_anal$diag_ibd_fac == "Unclassified"] <- "NA"
# df_anal$diag_ibd_fac <- droplevels(df_anal$diag_ibd_fac)
#
# levels(df_anal$diag_jia_fac) <- c("Artritis psoriatica", "enthesitis related artritis (ERA)",
#                                   "NA", "oligo", "poli", "systemic", "systemic")
# df_anal$diag_jia_fac <- droplevels(df_anal$diag_jia_fac)


# levels(df_anal$diag_nef_fac) <- c("Other", "Chronic_kidney_disease_or_Post.transplantation",
#                                   "NA", "Other", "Other", "Other")
# df_anal$diag_nef_fac <- droplevels(df_anal$diag_nef_fac)

# Too skewed
df_anal$int_disact_jia_ajc_num[which(df_anal$int_disact_jia_ajc_num > 0)] <- 1
df_anal$admission_2nights_min6mo_num[which(df_anal$admission_2nights_min6mo_num > 0)] <- 1


levels(df_anal$int_disact_cardio_chd_ord) <- c(0, 1,1,1)
levels(df_anal$int_disact_immunodef_ord) <- c(0, 1,1)
levels(df_anal$int_disact_auto.inflam_ord) <- c(0, 1,1)

for(v in vars){
  tb <- prop.table(table(df_anal[[v]]))
  if(any(tb < .01)){
    if(endsWith(v, "_bin")){
      df_anal[[v]] <- NULL # Remove binary variables with too few cases
    } else {
      browser()
    }
  }
}

#
# # Make scales list
# # k_cis4 let op: in de loop van de tijd zijn er verschillende versies vragenlijsten gebruikt. Iedereen heeft dezelfde uitkomst, maar de naam van de onderliggende items verschilt voor groepen.
# tmp = df[, c(c("k_cis8_deel1_1_recoded", "k_cis8_deel1_2_recoded", "k_cis8_deel1_3", "k_cis8_deel1_7_recoded"),c("k_pro_cis4_cis_4_1_recoded", "k_pro_cis4_cis_4_2_recoded", #"k_pro_cis4_cis_4_3",
#                                                                                                       "k_pro_cis4_cis_4_4_recoded"))]
# tmp <- df[, c(c("k_cis8_deel1_1_recoded", "k_cis8_deel1_2_recoded", "k_cis8_deel1_3", "k_cis8_deel1_7_recoded"),c("k_pro_cis4_cis_4_1_recoded", "k_pro_cis4_cis_4_2_recoded", "k_pro_cis4_cis_4_3", "k_pro_cis4_cis_4_4_recoded"))]
#
#
#
# scales_list <- grep("_\\d$", names(df), value = TRUE)
# scales_list <- split(scales_list, factor(gsub("k_(.+?)_\\d", "\\1", scales_list)))
#
# # Bijgevoegd de uitkomsten van de CIS4 (4 items) en pedsQL MFS (6 items). Beide kind gerapporteerd. De ID is niet meer terug te leiden naar de oorspronkelijke deelnemer, wel te gebruiken om binnen 1 persoon naar de uitkomsten te kijken, mocht je dat willen doen.
# # MT staat voor measurement time, en geeft dus de chronologie aan.
# #
# # De items van de CIS (1-7)zijn deels recoded; hiermee geldt bij deze allemaal: hoger is meer moe.
# # De items van de pedsql (1-4) zijn allemaal nog niet omgezet, waarmee hier ook geldt: hoger is meer moe.
# #
# #
# # Ouder gerapporteerde  MFS heb ik nu niet aangeleverd, aangezien we die alleen voor imputatie gaan gebruiken. Laat het weten als je die wel wilt hebben.
# # Dit is de beschikbare vermoeidheidsdata van de export uit najaar 2024; dus niet de volledige set te gebruiken voor de studie.
# #
# # Harstikke fijn dat je hier naar wilt kijken!
# #   Laat het weten als je meer nodig hebt, of vragen hebt.
#
#
# # In this file, write the R-code necessary to load your original data file
# # (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# # use the function open_data(your_data_frame) or closed_data(your_data_frame)
# # to store the data.
#
# library(tidySEM)
#
#
# # Psychometrics -----------------------------------------------------------
#
# desc <- tidySEM::descriptives(df)
#
# # Variables with < 10 unique values are treated as ordinal
# is_ordered <- desc$name[desc$unique < 10]
# df[is_ordered] <- lapply(df[is_ordered], ordered)
#
# scales_list[["combined"]] <- unlist(scales_list)
# # Make data long for multilevel CFA
# psychmet <- lapply(names(scales_list), function(scal){
#   #scal = names(scales_list)[1]
#   indicators <- scales_list[[scal]]
#   syntx <- paste0(scal, "=~", paste0(indicators,
#                                      collapse = " + "
#   ))
#
#   df_tmp <- df[, indicators]
#   df_num <- df_tmp
#   df_num[] <- lapply(df_num, as.numeric)
#   res_fa <- stats::prcomp(cor(df_num, use = "pairwise.complete.obs"))
#
#   res_par <- psych::fa.parallel(cor(df_num, use = "pairwise.complete.obs"), n.obs = nrow(df_num))
#
#
#   # Any ordered
#   is_ordr <- sapply(df_tmp, inherits, what = "ordered")
#   # CFA
#   res <- lavaan::cfa(
#     model = syntx,
#     data = df_tmp,
#     ordered = if(any(is_ordr)){names(df_tmp)[is_ordr]} else {NULL},
#     std.lv = TRUE,
#     auto.fix.first = FALSE
#   )
#  p <- graph_sem(res, angle = 179)
#  ggplot2::ggsave(paste0(scal, "_sem.svg"), p, device = "svg")
#
# fits <- try(tidySEM::table_fit(res)[, c("Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")], silent = TRUE)
#   if(inherits(fits, "try-error")){
#     fits <- structure(list(Parameters = NA, chisq = NA, df = NA,
#                            cfi = NA, tli = NA, rmsea = NA,
#                            srmr = NA), class = c("tidy_fit", "data.frame"
#                            ), row.names = c(NA, -1L))
#   }
#   tab <- data.frame(variable = scal,
#                     items = length(indicators),
#                     fits)
#
#   tab$comp_rel <- semTools::compRelSEM(res, ord.scale = any(is_ordr))
#   tab$kaiser <- sum(res_fa$sdev^2 > 1)
#   tab$par_factors <- res_par$nfact
#   tab$par_components <- res_par$ncomp
#   return(tab)
# })
#
# tab_psychometrics <- do.call(rbind, psychmet)
#
#
# syntx <- c(paste0("C =~", paste0(unlist(scales_list),
#                                    collapse = " + "
# )),
# paste0("CIS4 =~", paste0(scales_list$cis4,
#                       collapse = " + "
# )),
# paste0("SQL =~", paste0(scales_list$pedsql_fatigue_alg,
#                          collapse = " + "
# )), "CIS4 ~~ SQL", "CIS4 ~~ 0*C", "SQL ~~ 0*C")
#
# res <- lavaan::cfa(model = syntx,
#                    data = df,
#                    ordered = unlist(scales_list),
#                    std.lv = TRUE,
#                    auto.fix.first = FALSE)
#
# table_fit(res)[c("Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")]
# tab_res <- table_results(res, columns = NULL)
# saveRDS(res, "res_combined.RData")
# lo <- matrix(unlist(scales_list), nrow = 1)
# lo <- rbind(matrix(NA, ncol = ncol(lo)), lo, matrix(NA, ncol = ncol(lo)))
# lo[1, 11] <- "C"
# lo[3, c(4, 22-4)] <- c("CIS4", "SQL")
# graph_sem(res, angle = 179, layout = get_layout(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "C", NA,
#                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "k_cis4_1", "k_cis4_2",
#                                                 "k_cis4_3", "k_cis4_7", "k_cis4_4", "k_pedsql_fatigue_alg_1",
#                                                 "k_pedsql_fatigue_alg_2", "k_pedsql_fatigue_alg_3", "k_pedsql_fatigue_alg_4",
#                                                 "k_pedsql_fatigue_alg_5", "k_pedsql_fatigue_alg_6", "k_cis4_1",
#                                                 "k_cis4_2", "k_cis4_3", "k_cis4_7", "k_cis4_4", "k_pedsql_fatigue_alg_1",
#                                                 "k_pedsql_fatigue_alg_2", "k_pedsql_fatigue_alg_3", "k_pedsql_fatigue_alg_4",
#                                                 "k_pedsql_fatigue_alg_5", "k_pedsql_fatigue_alg_6", NA, NA, NA,
#                                                 "CIS4", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "SQL",
#                                                 NA, NA, NA, NA, rows = 3))
#
# scale_scores <- data.frame(do.call(cbind, lapply(psychmet, `[[`, 2)))
# names(scale_scores) <- tab_psychometrics$variable
# write.csv(tab_psychometrics, "tab_psychometrics.csv", row.names = F)
#
# if(!all(sapply(names(scale_scores), function(n) isTRUE(all(scale_scores[[n]] == df_full[[n]]))))){
#   stop("Bastian's scales are not the same as Caspar's")
# }
#
# # Drop scales if the following psychometrics are poor:
# drop_scales <- which(tab_psychometrics$comp_rel < 0) # change to .6 for real data
#
# if(length(drop_scales) > 0){
#   tab_psychometrics <- tab_psychometrics[-drop_scales, ]
#   scale_scores <- scale_scores[, -drop_scales]
# }
#
# df_anal <- data.frame(df_full[, c("id", yvar, setdiff(selected_variables$variable_name, names(scales_list)))],
#                       scale_scores)
# desc <- worcs::descriptives(df_anal)
# cors <- polycor::hetcor(df_anal), use = "pairwise.complete.obs")
# df <- df_anal
open_data(df, filename = "df.RData", save_expression = saveRDS(object = data, file = "df.RData"), load_expression = readRDS("df.RData"))
