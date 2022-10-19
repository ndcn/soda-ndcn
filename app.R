#setwd("c:/Users/yassene/mydevenv/Rscripts/soda/")

################################################################
# This software is licensed under AGPLv3. Copyrights ? 2017 by Yassene Mohammed
#
# This software tool is still in active development and we are happy to obtain
# feedback on how to improve it.
# The input CSV file must have first column as sample name and second column as group.
# The software tool is developed and tested using quantitative lipidomics data. The program
# is however of general use and not limited to lipidomics data.
# Please send any feedback or questions to Yassene or Rico: y.mohammed@lumc.nl, r.j.e.derks@lumc.nl
################################################################





credentials <- data.frame(user = c("user1", "user2"), password = c("1234", "1234"), comment = "Secure authentification mechanism for SODA",
                          stringsAsFactors = FALSE)
library(shinymanager)
library(googleVis)
library(glmnet)
library(d3heatmap)
library(shiny)
library(readxl)
library(xlsx)
library(reshape)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggExtra)
library(cowplot)
library(RColorBrewer)
library(pROC)
library(ggbiplot)
library(corrplot)
library(gplots)
library(tidyverse)
library(dendextend)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(plotly)

###############

read_xlxs_lipidomics_files = function(file, blank_thr = 2, blank_thr_nr_of_samples = 0.8, group_thr_nr_of_samples = 0.7,
                                      norm_to_lipid = T)
{
  excel_sheet_names = excel_sheets(file)
  d1_org = d1 = as.data.frame(read_xlsx(file, sheet = 1))
  meta = lapply(d1$SampleID, FUN = function(x) strsplit(x, "__")[[1]])
  ind_blanks = grep("blank", d1$SampleID, ignore.case = T)
  ind_qcs = grep("qc", d1$SampleID, ignore.case = T)
  ind_samples = c(1:length(d1$SampleID))[-c(ind_blanks, ind_qcs)]
  meta_s = NULL
  for (i in ind_samples) meta_s = rbind(meta_s, meta[[i]])
  colnames(meta_s) = c("Date_of_harvest", "Cell_type", "Genotype", "Cell_line", "Drug_treatment", "Days_in_culture",
                       "Cell_number_k", "Replicate", "Date_of_exp.")
  meta_s = as.data.frame(meta_s)
  for (i in c(1:length(meta_s[1, ]))) meta_s[, i] = factor(meta_s[, i])
  l1 = l1_org = as.data.frame(d1[, -1])
  start_col = 2
  l1[, start_col:length(l1[1, ])] = sapply(start_col:length(l1[1, ]), function(i) as.numeric(as.character(l1[, i])))
  l1[which(is.na(l1), arr.ind = T)] = 0
  l1_blanks = colMeans(l1[ind_blanks, -1, drop = F], na.rm = T)
  if (!exists("blank_thr"))
    blank_thr = 2
  if (!exists("blank_thr_nr_of_samples"))
    blank_thr_nr_of_samples = 0.8
  if (!exists("group_thr_nr_of_samples"))
    group_thr_nr_of_samples = 0.7
  ind_below_blank = which(t(t(l1[ind_samples, -1]) <= blank_thr * l1_blanks), arr.ind = T)
  ind_below_blank[, 2] = ind_below_blank[, 2] + 1
  table_ind_below_blank = table(ind_below_blank[, 2])
  ind_rem1 = which((table_ind_below_blank/length(ind_samples)) > blank_thr_nr_of_samples)
  ind_rem = names(table_ind_below_blank[ind_rem1])
  length(ind_rem)
  ind_rem_ind_rem = NULL
  for (i in c(1:length(ind_rem)))
  {
    tmp_l1_i = cbind(meta_s, l1[ind_samples, c(as.numeric(ind_rem[i]))])
    tmp_a = table(tmp_l1_i$Genotype[which(tmp_l1_i[, length(tmp_l1_i[1, ])] > (blank_thr * l1_blanks[as.numeric(ind_rem[i]) -
                                                                                                       1]))])
    tmp_b = table(tmp_l1_i$Genotype)
    if (any(tmp_a/tmp_b >= group_thr_nr_of_samples))
      ind_rem_ind_rem = c(ind_rem_ind_rem, i)
  }
  if (length(ind_rem_ind_rem) > 0)
    ind_rem = ind_rem[-ind_rem_ind_rem]
  length(ind_rem)
  if (length(ind_rem) > 0)
  {
    l2 = l1[, -as.numeric(ind_rem)]
  }
  else
  {
    l2 = l1
  }
  l2_nonorm = l2
  dim(l2)
  tmp_l2 = l2_nonorm[ind_samples, -1]
  l3 = l3_no_lip_norm = l2[ind_samples, ]
  l3_2 = l3_2_no_lip_norm = l2[ind_samples, ]
  l3 = l2[ind_samples, ]
  l3_2 = l2[ind_samples, ]
  tmp = sapply(as.character(names(l3[, -1])), function(x) strsplit(x, " ", fixed = T)[[1]][1])
  classes = tmp
  classes_unique = unique(classes)
  for (i in c(1:length(classes_unique)))
  {
    ind_c = which(classes == classes_unique[i]) + 1
    for (j in c(1:length(l3[, 1])))
    {
      l3[j, ind_c] = (l3[j, ind_c]/sum(l3[j, ind_c]))
    }
  }
  for (j in c(1:length(l3_2[, 1])))
  {
    l3_2[j, -1] = (l3_2[j, -1]/sum((l3_2[j, -1])))
  }
  l3_lip_norm = l3
  l3_2_lip_norm = l3_2
  if (!exists("norm_to_lipid"))
    norm_to_lipid = T
  if (norm_to_lipid == F)
  {
    l3 = l3_no_lip_norm
    l3_2 = l3_2_no_lip_norm
  }
  else
  {
    l3 = l3_lip_norm
    l3_2 = l3_2_lip_norm
  }
  l_for_melt = l3
  l_melt = melt(cbind(meta_s, l_for_melt[, -1]))
  l_melt$class = {
    tmp = sapply(as.character(l_melt$variable), function(x) strsplit(x, " ", fixed = T)[[1]][1])
    (tmp)
  }
  l4_2 = l3_2_lip_norm
  tmp = sapply(as.character(names(l4_2[, -1])), function(x) strsplit(x, " ", fixed = T)[[1]][1])
  l4_2_classes = tmp
  l4_2_classes_unique = unique(l4_2_classes)
  l4_2_c = data.frame(matrix(0, nrow = length(l4_2[, 1]), ncol = length(l4_2_classes_unique) + 1))
  colnames(l4_2_c) = c("Sample", l4_2_classes_unique)
  l4_2_c[, 1] = l4_2[, 1]
  for (i in c(2:length(l4_2_c[1, ])))
  {
    l4_2_c[, i] = rowSums(l4_2[, -1][, which(l4_2_classes == colnames(l4_2_c)[i]), drop = F])
  }
  l4_2_c_melt = melt(cbind(meta_s, l4_2_c[, -1]))
  rownames(l3) = l3[, 1]
  l3 = l3[, -1]
  rownames(l3_2) = l3_2[, 1]
  l3_2 = l3_2[, -1]
  rownames(l4_2_c) = l4_2_c[, 1]
  l4_2_c = l4_2_c[, -1]
  list(data_class_norm = l3, data_tot_lip_norm = l3_2, data_class_norm_melt = l_melt, class_data_tot_lip_norm = l4_2_c,
       class_data_tot_lip_norm_melt = l4_2_c_melt, meta = meta_s)
}
plot_classes = function(l3 = m_org$data_class_norm, meta_s = m_org$meta, fill1 = "Genotype")
{
  l_for_melt = l3
  l_melt = melt(cbind(meta_s, l_for_melt[, -1]))
  l_melt$class = {
    tmp = sapply(as.character(l_melt$variable), function(x) strsplit(x, " ", fixed = T)[[1]][1])
    (tmp)
  }
  
   formula1=as.formula(paste(fill1, "~ class"))

  cast_data1 = ((cast(l_melt, Genotype~class, sum)))
  cast_data = ((cast(l_melt, formula1, sum))/ceiling(rowSums(cast(l_melt, formula1,sum))))
  cast_data[,1]=cast_data1[,1]
  melt_cast_data=melt(cast_data)
  colnames(melt_cast_data)=c(fill1,"class","value")

  ggbar_lipid_class_per_genotype_per_cellnumber=ggplot(melt_cast_data,aes_string(x="class", y="value", fill=fill1)) +
    geom_col(position = "dodge") + ylab("") + theme_bw()
  sum(l_melt$value[which(l_melt$class == "PC")]/3)
  ggbar_lipid_class_per_genotype_per_cellnumber
}
plot_compare_classes = function(l4_2 = m_org$data_class_norm, meta_s = m_org$meta, compare1 = "Genotype")
{
  tmp = sapply(as.character(names(l4_2[, -1])), function(x) strsplit(x, " ", fixed = T)[[1]][1])
  l4_2_classes = tmp
  l4_2_classes_unique = unique(l4_2_classes)
  l4_2_c = data.frame(matrix(0, nrow = length(l4_2[, 1]), ncol = length(l4_2_classes_unique) + 1))
  colnames(l4_2_c) = c("Sample", l4_2_classes_unique)
  l4_2_c[, 1] = l4_2[, 1]
  i = 2
  dim(l4_2)
  for (i in c(1:length(l4_2_c[1, ])))
  {
    l4_2_c[, i] = rowSums(l4_2[, ][, which(l4_2_classes == colnames(l4_2_c)[i]), drop = F])
  }
  l4_2_c_melt = melt(cbind(meta_s, l4_2_c[, -1]))
  ggbar_class = ggplot(l4_2_c_melt, aes_string(x = compare1, y = "value", fill = compare1)) + geom_bar(position = "dodge",
                                                                                                       stat = "summary", fun.y = "mean") + geom_boxplot(width = 0.2) + geom_point() + facet_wrap(~variable, ncol = 5,
                                                                                                                                                                                                 scales = "free") + ylab("percentage of total lipid") + theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                                                                                                                                                                                                                                           axis.ticks.x = element_blank())
  gg1 = ggbar_class + theme(legend.key.size = unit(1, "cm"), legend.key.height = unit(1, "cm"), legend.key.width = unit(1,
                                                                                                                        "cm"), legend.title = element_text(size = 14), legend.text = element_text(size = 10))
  gg1 + theme(legend.position = "none")
}
generat_all_cls = function(l3, meta_s, ind_for_comp1, comp1)
{
  wilcox_pairwise_p_fc = wilcox_pairwise_p_fold_change_mouseko2 <- function(mat4, groups, ind_keep = "all", scale_mat = T,
                                                                            ctrl_label = "(null)", fc_methode = "median", ind_feat = "all")
  {
    if (ind_keep != "all")
    {
      if (ind_feat == "all")
      {
        mat5 = mat4[ind_keep, ]
        groups = groups[ind_keep]
      }
      else
      {
        mat5 = mat4[ind_keep, ind_feat]
        groups = groups[ind_keep]
      }
    }
    else
    {
      if (ind_feat == "all")
      {
        mat5 = mat4[, ]
      }
      else
      {
        mat5 = mat4[, ind_feat]
      }
    }
    mat5 = as.matrix(mat5)
    mode(mat5) = "numeric"
    mat6 = mat5
    if (scale_mat == T)
      mat6 = scale(mat5)
    cat("\ndim(mat6)=", dim(mat6))
    case_ = case = unique(groups)
    if (length(case) > 1)
      if (case[1] != ctrl_label)
      {
        case[1] = case_[2]
        case[2] = case_[1]
      }
    if (length(case) == 1)
    {
      case[1] = case_
      case[2] = case_
    }
    x = mat6[groups == case[1], ]
    y = mat6[groups == case[2], ]
    xx = mat5[groups == case[1], ]
    yy = mat5[groups == case[2], ]
    dim(x)
    dim(y)
    rem_ind_y = unique(which(is.na(y), arr.ind = T)[, 2])
    rem_ind_x = unique(which(is.na(x), arr.ind = T)[, 2])
    rem_ind = unique(c(rem_ind_y, rem_ind_x))
    results = NULL
    fchange = NULL
    for (i in c(1:length(x[1, ])))
    {
      if (!i %in% rem_ind)
      {
        results = c(results, wilcox.test(x[, i], y[, i])$p.value)
        if (fc_methode == "median")
        {
          fchange = c(fchange, median(yy[, i], na.rm = T)/median(xx[, i], na.rm = T))
        }
        else
        {
          fchange = c(fchange, mean(yy[, i], na.rm = T)/mean(xx[, i], na.rm = T))
        }
      }
      else
      {
        results = c(results, 1)
        fchange = c(fchange, 1)
      }
    }
    if (any(fchange == 0))
    {
      tmp_min_fchange = min(fchange[-which(fchange == 0)], 1/fchange[-which(fchange == 0)], na.rm = T)
      fchange[which(fchange == 0)] = 2^(log2(tmp_min_fchange) - 1)
    }
    if (any(is.infinite(fchange)))
    {
      tmp_max_fchange = max(fchange)
      fchange[which(is.infinite(fchange))] = 2^(log2(tmp_max_fchange) + 1)
    }
    if (any(is.nan(fchange)))
    {
      fchange[which(is.nan(fchange))] = 1
    }
    results1 = NULL
    results1$Bonferroni = p.adjust(results, method = "bonferroni")
    results1$BH = p.adjust(results, method = "BH")
    results1$Holm = p.adjust(results, method = "holm")
    results1$Hochberg = p.adjust(results, method = "hochberg")
    results1$Hommel = p.adjust(results, method = "hommel")
    results1$BY = p.adjust(results, method = "BY")
    names(results) = names(fchange) = names(results1$BH) = colnames(mat6)
    list(p_values = results, foldchange = fchange, BH_adj_p = results1$BH)
  }
  comp = comp1
  ind_for_comp = ind_for_comp1
  ind_keep_1 = which(meta_s[, ind_for_comp] %in% comp)
  name_ctrl_label = unique(meta_s[, ind_for_comp])[comp[1]]
  r1 = wilcox_pairwise_p_fc(mat4 = l3[, -1], groups = meta_s[, ind_for_comp], ind_feat = "all", ind_keep = ind_keep_1,
                            scale_mat = T, ctrl_label = name_ctrl_label, fc_methode = "median")
  r1_df = as.data.frame(r1)
  tmp = sapply(as.character(names(l3[, -1])), function(x) strsplit(x, " ", fixed = T)[[1]][1])
  classes = tmp
  classes_unique = unique(classes)
  rr1 = data.frame(logFC = log2(r1$foldchange), adj.P.Val = r1$BH_adj_p)
  all_cls = list()
  for (i in c(1:length(classes_unique)))
  {
    cl = rr1[(classes == classes_unique[i]), c("logFC", "adj.P.Val")]
    asyl_group = grep("/", row.names(cl), fixed = T)
    asyl_group = grep("/|_", row.names(cl))
    if (length(grep("TG", row.names(cl))) > 0)
    {
      TAG = cl
      TAG$FA.carbon = str_sub(row.names(TAG), -4, -3)
      TAG$FA.sat = str_sub(row.names(TAG), -1)
      TAG$total.carbon = str_sub(row.names(TAG), 4, 5)
      TAG$total.sat = factor(str_sub(row.names(TAG), 7, -8), levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), ordered = TRUE)
      TAG$logP = -log10(TAG$adj.P.Val)
      cl = TAG
      colnames(cl) = c("logFC", "adj.P.Val", "second.carbon", "second.sat", "first.carbon", "first.sat", "logP")
    }
    else if (length(asyl_group) == 0)
    {
      tmp1 = sapply(row.names(cl), function(x)
      {
        gsub("^.+?\\(", "", strsplit(x, ":", fixed = T)[[1]][1])
      })
      tmp1 = sapply(tmp1, function(x)
      {
        strsplit(x, " ", fixed = T)[[1]][2]
      })
      tmp2 = sapply(row.names(cl), function(x)
      {
        gsub("\\)", "", strsplit(x, ":", fixed = T)[[1]][2])
      })
      cl$carbon = tmp1
      cl$sat = tmp2
      cl$logP = -log10(cl$adj.P.Val)
    }
    else
    {
      cl = rr1[(classes == classes_unique[i]), c("logFC", "adj.P.Val")]
      tmp1 = sapply(row.names(cl), function(x)
      {
        gsub("^.+?\\(", "", strsplit(x, "/|_")[[1]][1])
      })
      tmp1_1 = sapply(tmp1, function(x)
      {
        strsplit(x, ":", fixed = T)[[1]][1]
      })
      tmp1_1 = as.numeric(gsub(".*?([0-9]+).*", "\\1", tmp1_1))
      tmp1_2 = sapply(tmp1, function(x)
      {
        strsplit(x, ":", fixed = T)[[1]][2]
      })
      tmp2 = sapply(row.names(cl), function(x)
      {
        gsub("\\)", "", strsplit(x, "/|_")[[1]][2])
      })
      tmp2_1 = sapply(tmp2, function(x)
      {
        strsplit(x, ":", fixed = T)[[1]][1]
      })
      tmp2_2 = sapply(tmp2, function(x)
      {
        strsplit(x, ":", fixed = T)[[1]][2]
      })
      cl$second.carbon = tmp2_1
      cl$second.sat = tmp2_2
      cl$first.carbon = tmp1_1
      cl$first.sat = factor(tmp1_2, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), ordered = TRUE)
      cl$logP = -log10(cl$adj.P.Val)
    }
    all_cls[[i]] = cl
  }
  names(all_cls) = classes_unique
  all_cls
}
##############

{
  ui <- fluidPage(
    h1("SODA - Simple Omics Data Analysis (v0.8)"),
    fluidRow(tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
    ),
    column(2,

           wellPanel(
             fileInput("file1", "Choose input File",
                       #accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
             ),

             checkboxInput("scale", "Scale and center",value = T,),
             textInput("blank_thr", "Nr of blanks threshold", value = "2", width = NULL, placeholder = NULL),
             textInput("blank_thr_nr_of_samples", "Proportion of samples above blank threshold", value = "0.8", width = NULL, placeholder = NULL),
             textInput("group_thr_nr_of_samples", "Proportion of samples from one group above blank threshold", value = "0.7", width = NULL, placeholder = NULL),

           ),
           wellPanel(

             selectInput("norm", "Normalization", c("Normalized to total class"=1,
                                                    "Normalized to total lipid"=2
             ),
             selected = 2,multiple = F)


           ),
           wellPanel(
             uiOutput("info"),
             uiOutput("comp2"),
             checkboxInput("analysis1", "Apply disc. and red. analysis"),
             sliderInput("alpha",
                         "Alpha:",ticks=T,
                         min = 0.5, max =0.95,
                         value = 0.8, step=0.01),
           ),

           wellPanel(
             htmlOutput("license")
           )
    ),
    column(width = 5,
           id = "column3",
           uiOutput("comp3"),
           plotlyOutput("plot3"),
           plotlyOutput("plot4"),
           uiOutput("comp4"),

    ),
    column(width = 4,
           id = "column2",
           uiOutput("comp1"),
           uiOutput("generate_hm"),
           htmlOutput("heatmap_1")

    )


    )
  )

  ui <- secure_app(ui)

  shiny_env = new.env()

  server <- function(input, output, session)
  {
    #############
    res_auth <- secure_server(check_credentials = check_credentials(credentials))
    output$auth_output <- renderPrint({
      reactiveValuesToList(res_auth)
    })
    #############

    m3_scl = reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      m_org = read_xlxs_lipidomics_files(inFile$datapath, blank_thr = as.numeric(input$blank_thr), blank_thr_nr_of_samples = as.numeric(input$blank_thr_nr_of_samples),
                                         group_thr_nr_of_samples = as.numeric(input$group_thr_nr_of_samples))
      m_org
    })

    mat = reactive({
      if (is.null(m3_scl()))
        return(NULL)
      meta_scl = m3_scl()[[6]]
      if (as.numeric(input$dataset) == 1 & as.numeric(input$norm) == 1)
      {
        m3_scl1 = m3_scl()[[1]]
      }
      else if (as.numeric(input$dataset) == 1 & as.numeric(input$norm) == 2)
      {
        m3_scl1 = m3_scl()[[2]]
      }
      else if (as.numeric(input$dataset) == 2 & as.numeric(input$norm) == 1)
      {
        m3_scl1 = m3_scl()[[4]]
      }
      else if (as.numeric(input$dataset) == 2 & as.numeric(input$norm) == 2)
      {
        m3_scl1 = m3_scl()[[4]]
      }
      tmp_rn = rownames(m3_scl1)
      m3_scl1 = if (input$scale)
      {
        scale(m3_scl1)
      }
      else
      {
        m3_scl1
      }
      rownames(m3_scl1) = tmp_rn
      if (!is.null(input$labeling))
      {
        lab_ind = which(colnames(meta_scl) %in% input$labeling)
        rownames(m3_scl1) = sapply(tmp_rn, FUN = function(x)
        {
          s = strsplit(x, split = "__", fixed = T)[[1]][lab_ind]
          paste(s, collapse = "_")
        })
      }
      cat("\ninput$dataset=", input$dataset, "\n")
      cat("\nlength(m3_scl())=", length(m3_scl()), "\n")
      cat("\ndim(m3_scl1)=", dim(m3_scl1), "\n")
      cat("\ninput$comparision=", input$comparision, "\n")
      comp_ind = which(colnames(meta_scl) %in% input$comparision)
      ind_coef = c(1:length(m3_scl1[1, ]))
      if (input$analysis1)
      {
        group_m3 = group_m3_ = sapply(tmp_rn, FUN = function(x)
        {
          s = strsplit(x, split = "__", fixed = T)[[1]][comp_ind]
          paste(s, collapse = "_")
        })
        g = unique(group_m3)
        for (i in c(1:length(g))) group_m3[which(group_m3 == g[i])] = i
        ind_rem_group = which(as.numeric(group_m3) %in% which(table(group_m3) < 3))
        if (length(ind_rem_group) > 0)
        {
          group_m3 = group_m3[-ind_rem_group]
        }
        if (length(unique(group_m3)) > 1)
        {
          if (length(ind_rem_group) > 0)
          {
            m3_scl1 = m3_scl1[-ind_rem_group, ]
          }
          m3_scl1[which(is.na(m3_scl1), arr.ind = T)] = 0
          set.seed(100)
          cvfit = cv.glmnet(m3_scl1, (group_m3), nlambda = 100, alpha = as.numeric(input$alpha), family = "multinomial",
                            type.multinomial = "grouped")
          coef = coef(cvfit, s = "lambda.min")
          tmp = as.matrix(coef$"1")
          tmp1 = tmp[which(tmp != 0)]
          coef_names = rownames(tmp)[which(tmp != 0)][-1]
          ind_coef = which(colnames(m3_scl1) %in% coef_names)
          m3_scl1[, ind_coef]
        }
      }
      tmp = m3_scl1[, ind_coef]
      cat("\ndim(m3_scl1)=", dim(m3_scl1), "\n")
      cat("\ndim(tmp)=", dim(tmp), "\n")
      mat = tmp
      mat
    })

    mat_classes = reactive({
      if (is.null(m3_scl()))
        return(NULL)
      meta_scl = m3_scl()[[6]]
      m3_scl1 = m3_scl()[[4]]
      tmp_rn = rownames(m3_scl1)
      m3_scl1 = if (input$scale)
      {
        scale(m3_scl1)
      }
      else
      {
        m3_scl1
      }
      rownames(m3_scl1) = tmp_rn
      if (!is.null(input$labeling))
      {
        lab_ind = which(colnames(meta_scl) %in% input$labeling)
        rownames(m3_scl1) = sapply(tmp_rn, FUN = function(x)
        {
          s = strsplit(x, split = "__", fixed = T)[[1]][lab_ind]
          paste(s, collapse = "_")
        })
      }
      mat_classes = m3_scl1
      mat_classes
    })

    output$comp1 = renderUI({
      if (is.null(input$file1))
        return()
      fluidRow(column(3, selectInput("labeling", "Select labeling", colnames(m3_scl()[[6]]), selected = c(1, 2,
                                                                                                          3, 4), multiple = T)), column(3, selectInput("dataset", "Data set", c(Species = 1, Classes = 2), selected = 2,
                                                                                                                                                       multiple = F)), )
    })

    output$comp2 = renderUI({
      if (is.null(input$file1))
        return()
      wellPanel(selectInput("comparision", "Select comparision", colnames(m3_scl()[[6]]), selected = 3, multiple = T))
    })

    output$comp3 = renderUI({
      if (is.null(input$file1))
        return()
      selectInput("comparision3", "Select category comparision", colnames(m3_scl()[[6]]), selected = 3, multiple = F)
    })

    output$generate_hm = renderUI({
      if (is.null(input$file1))
        return()
      actionButton("show_heatmap", "Generate_heatmap")
    })

    output$info = renderUI({
      if (is.null(input$file1))
        return()
      paste("Data matrix size: [", dim(mat())[1], ", ", dim(mat())[2], "]", sep = "")
    })

    output$plot3 <- renderPlotly({
      ggly = ggplotly(plot_classes(l3 = m3_scl()[[as.numeric(input$norm)]], meta_s = m3_scl()[[6]], fill1 = input$comparision3))
      ggly %>% config(displaylogo = F)
    })

    output$plot4 <- renderPlotly({
      ggly = ggplotly(plot_compare_classes(l4_2 = m3_scl()[[as.numeric(input$norm)]], meta_s = m3_scl()[[6]], compare1 = input$comparision3))
      ggly %>% config(displaylogo = F)
    })

    observeEvent(input$show_heatmap, {
      mat = mat()
      mat_classes = mat_classes()
      ht = ht2 = NULL
      if (!is.null(mat))
      {
        ht = Heatmap(t(mat))
        ht = draw(ht)
      }
      if (!is.null(ht))
      {
        InteractiveComplexHeatmapWidget(input, output, session, ht, output_id = "heatmap_1")
      }
      else
      {
        output$heatmap_1 = renderPlot({
          grid.newpage()
          grid.text("No row exists after filtering.")
        })
      }
    }, ignoreNULL = FALSE)

    output$license <- renderText({
      "This software tool is still in <b>active development</b>.<br><br>\n    For further information please contact Yassene: y.mohammed@lumc.nl.<br><br>\n"
    })
  }
}
shinyApp(ui, server)


