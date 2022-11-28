#' calculations
#'
#' @title Calculate everything for all classes
#'
#' @description Calculate everything for all classes.
#'
#' @param l3 data
#' @param meta_s meta data
#' @param ind_for_comp1 indexes for the comparisson
#' @param comp1 which comparison
#'
#' @return The return value, if any, from executing the function.
#'
#' @author Yassene Mohammed
#' @author Rico Derks
#'
#' @importFrom stringr str_sub
#' @importFrom stats median p.adjust
#'
#' @noRd
#'
generat_all_cls <- function(l3, meta_s, ind_for_comp1, comp1) {

  comp <- comp1
  ind_for_comp <- ind_for_comp1
  ind_keep_1 <- which(meta_s[, ind_for_comp] %in% comp)

  name_ctrl_label <- unique(meta_s[, ind_for_comp])[comp[1]]


  r1 <- wilcox_pairwise_p_fc(mat4 = l3,
                             groups = meta_s[, ind_for_comp],
                             ind_feat = "all",
                             ind_keep = ind_keep_1,
                             scale_mat = TRUE,
                             ctrl_label = name_ctrl_label,
                             fc_methode = "median")

  r1_df <- as.data.frame(r1)
  classes <- sapply(as.character(names(l3)), function(x) strsplit(x, " ", fixed = TRUE)[[1]][1])
  classes_unique <- unique(classes)

  rr1 <- data.frame(logFC = log2(r1$foldchange),
                    adj.P.Val = r1$BH_adj_p)

  all_cls <- list()
  for(i in c(1:length(classes_unique))) {
    cl  <-  rr1[(classes == classes_unique[i]), c("logFC", "adj.P.Val")]
    asyl_group <- grep("/", row.names(cl), fixed = TRUE)
    asyl_group <- grep("/|_", row.names(cl))

    if(length(grep("TG", row.names(cl))) > 0) {
      TAG  <-  cl

      TAG$FA.carbon <- str_sub(row.names(TAG), -4, -3)
      TAG$FA.sat <- str_sub(row.names(TAG), -1)
      TAG$total.carbon <- str_sub(row.names(TAG), 4, 5)
      TAG$total.sat <- factor(str_sub(row.names(TAG), 7, -8),
                              levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                              ordered = TRUE)
      TAG$logP <- -log10(TAG$adj.P.Val)
      cl <- TAG
      colnames(cl) <- c("logFC", "adj.P.Val", "second.carbon", "second.sat", "first.carbon", "first.sat", "logP")
    } else if (length(asyl_group) == 0) {
      tmp1 <- sapply(row.names(cl), function(x){
        gsub("^.+?\\(", "", strsplit(x, ":", fixed = TRUE)[[1]][1])
      })

      tmp1 <- sapply(tmp1, function(x){
        strsplit(x, " ", fixed = TRUE)[[1]][2]
      })
      tmp2 <- sapply(row.names(cl), function(x){
        gsub("\\)", "", strsplit(x, ":", fixed = TRUE)[[1]][2])
      })

      cl$carbon <- tmp1
      cl$sat <- tmp2
      cl$logP <- -log10(cl$adj.P.Val)
    } else {
      #i=11
      cl <- rr1[(classes == classes_unique[i]), c("logFC","adj.P.Val")]

      tmp1 <- sapply(row.names(cl), function(x){
        gsub("^.+?\\(", "", strsplit(x, "/|_" )[[1]][1])
      })

      tmp1_1 <- sapply(tmp1, function(x) {
        strsplit(x, ":", fixed = TRUE)[[1]][1]
      })

      tmp1_1 <- as.numeric(gsub(".*?([0-9]+).*", "\\1", tmp1_1))

      tmp1_2 <- sapply(tmp1, function(x) {
        strsplit(x, ":", fixed = TRUE)[[1]][2]
      })

      tmp2 <- sapply(row.names(cl), function(x){
        gsub("\\)", "", strsplit(x, "/|_")[[1]][2])
      })

      tmp2_1 <- sapply(tmp2, function(x) {
        strsplit(x, ":", fixed = TRUE)[[1]][1]
      })
      tmp2_2 <- sapply(tmp2, function(x) {
        strsplit(x, ":", fixed = TRUE)[[1]][2]
      })

      cl$second.carbon <- tmp2_1
      cl$second.sat <- tmp2_2
      cl$first.carbon <- tmp1_1
      cl$first.sat <- factor(tmp1_2,levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                             ordered = TRUE)
      cl$logP <- -log10(cl$adj.P.Val)

    }
    all_cls[[i]] <- cl
  }

  names(all_cls) <- classes_unique

  return(all_cls)
}



#' @title Calculate p-value and fold change
#'
#' @description Calculate p-value and fold change.
#'
#' @param mat4 data.
#' @param groups which group.
#' @param ind_keep keep indexes.
#' @param scale_mat is the matrix scaled.
#' @param ctr_label who is the control group.
#' @param fc_method fold change method, median or mean.
#' @param ind_feat index features.
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom stats wilcox.test
#'
#' @author Yassene Mohammed
#' @author Rico Derks
#'
#' @noRd
#'
wilcox_pairwise_p_fc <- function(mat4,
                                 groups,
                                 ind_keep = "all",
                                 scale_mat = TRUE,
                                 ctrl_label = "(null)",
                                 fc_methode = c("median", "mean"),
                                 ind_feat = "all") {
  if(ind_keep[1] != "all") {
    if(ind_feat == "all") {
      mat5 <- mat4[ind_keep,]
      groups <- groups[ind_keep]
    } else {
      mat5 <- mat4[ind_keep,ind_feat]
      groups <- groups[ind_keep]
    }
  } else {
    if(ind_feat=="all") {
      mat5 <- mat4[, ]
    } else {
      mat5 <- mat4[, ind_feat]
    }
  }

  mat5 <- as.matrix(mat5)
  mode(mat5) <- "numeric"

  mat6 <- mat5
  if(scale_mat == TRUE)
    mat6 <- scale(mat5)

  cat("\ndim(mat6)=", dim(mat6))

  # colnames(df_meta_5)
  case_ <- case <- unique(groups)
  #case[1];case[2]
  if(length(case) > 1)
    if(case[1] != ctrl_label){
      case[1] <- case_[2]
      case[2] <- case_[1]
    }
  if(length(case) == 1) {
    case[1] <- case_
    case[2] <- case_
  }

  x <- mat6[groups == case[1], ]
  y <- mat6[groups == case[2], ]
  xx <- mat5[groups == case[1], ]
  yy <- mat5[groups == case[2], ]

  rem_ind_y <- unique(which(is.na(y), arr.ind = TRUE)[, 2])
  rem_ind_x <- unique(which(is.na(x), arr.ind = TRUE)[, 2])
  rem_ind <- unique(c(rem_ind_y, rem_ind_x))

  # calculate fold change
  results <- NULL
  fchange <- NULL
  for ( i in c(1:length(x[1, ]))) {
    #i=1
    if(!i %in% rem_ind) {
      results <- c(results, wilcox.test(x[, i], y[, i])$p.value)
      if(fc_methode == "median") {
        fchange <- c(fchange, median(yy[, i], na.rm = TRUE) / median(xx[, i], na.rm = TRUE))
      } else {
        fchange <- c(fchange, mean(yy[, i], na.rm = TRUE) / mean(xx[, i], na.rm = TRUE))
      }
    } else {
      results <- c(results, 1)
      fchange <- c(fchange, 1)
    }
  }

  # impute fold change = 0
  if(any(fchange == 0)) {
    tmp_min_fchange <- min(fchange[-which(fchange == 0)], 1/fchange[-which(fchange == 0)], na.rm = TRUE)
    fchange[which(fchange == 0)] <- 2^(log2(tmp_min_fchange) - 1)
  }

  # impute fold change = infinite
  if(any(is.infinite(fchange))) {
    tmp_max_fchange <- max(fchange)
    fchange[which(is.infinite(fchange))] <- 2^(log2(tmp_max_fchange) + 1)
  }

  # impute if fold change = NA
  if(any(is.nan(fchange))) {
    fchange[which(is.nan(fchange))] <- 1
  }

  results1 <- NULL
  results1$Bonferroni <- p.adjust(results, method = "bonferroni")
  results1$BH <- p.adjust(results, method = "BH")
  results1$Holm <- p.adjust(results, method = "holm")
  results1$Hochberg <- p.adjust(results, method = "hochberg")
  results1$Hommel <- p.adjust(results, method = "hommel")
  results1$BY <- p.adjust(results, method = "BY")

  names(results) <- names(fchange) <- names(results1$BH) <- colnames(mat6)

  return(list(p_values = results,
              foldchange = fchange,
              BH_adj_p = results1$BH))
}
