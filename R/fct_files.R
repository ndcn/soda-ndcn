#' files
#'
#' @title Import lipid data

#' @description Import all lipid data.
#'
#' @param data data.frame, containing all data.
#' @param blank_thr integer, number of blank threshold.
#' @param blank_thr_nr_of_samples number, proportion of samples above blank
#'     threshold.
#' @param group_thr_nr_of_samples number, proportion of samples from one group
#'     above blank threshold.
#' @param norm_to_lipid boolean, total area normalization.
#'
#' @details The structure is slightly changed. Only the first sheet is read. The
#'     first column should be sampleID which contains several fields separated
#'     by '__'.
#'
#' @return list, with the following entries:
#'    data_class_norm: data.frame normalized lipid class
#'    data_tot_lip_norm: data.frame
#'    data_class_norm_melt: long format of data_class_norm
#'    class_data_tot_lip_norm: data.frame
#'    class_data_tot_lip_norm_melt: long format of class_data_tot_lipid_norm
#'    meta: data.frame with meta data
#'
#' @author Yassene Mohammed
#' @author Rico Derks
#'
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
#' @importFrom stats aggregate
#'
#' @export
#'
#' @noRd
#
import_lipid_data <- function(data,
                              blank_thr = 2,
                              blank_thr_nr_of_samples = 0.8,
                              group_thr_nr_of_samples = 0.7,
                              norm_to_lipid = TRUE) {
  # read and remove first column
  rawData <- data

  # extract the meta data from the sampleID
  meta <- lapply(
    rawData$SampleID,
    FUN = function(x)
      strsplit(x, "__")[[1]]
  )

  # get the indexes of samples, blanks, qcs
  ind_blanks <- grep(pattern = "blank",
                     x = rawData$SampleID,
                     ignore.case = TRUE)
  ind_qcs <- grep(pattern = "qc",
                  x = rawData$SampleID,
                  ignore.case = TRUE)
  ind_samples <- c(1:length(rawData$SampleID))[-c(ind_blanks, ind_qcs)]

  # extract  the meta data of the samples
  meta_s <- as.data.frame(do.call(rbind, meta[ind_samples]),
                          stringsAsFactors = TRUE)

  colnames(meta_s) <- c(
    "Date_of_harvest",
    "Cell_type",
    "Genotype",
    "Cell_line",
    "Drug_treatment",
    "Days_in_culture",
    "Cell_number_k",
    "Replicate",
    "Date_of_exp."
  )

  # remove weird characters and NA's
  l1 <- l1_org <- as.data.frame(rawData[, -1])
  ### not sure if this is still necessary
  start_col <- 2
  l1[, start_col:length(l1[1,])] <-
    sapply(start_col:length(l1[1,]), function(i)
      as.numeric(as.character(l1[, i])))
  ###
  l1[which(is.na(l1), arr.ind = TRUE)] <- 0


  ### Apply blank thresholds, lipid species will be removed here
  # some sanity checking
  if (!exists("blank_thr"))
    blank_thr <- 2
  if (!exists("blank_thr_nr_of_samples"))
    blank_thr_nr_of_samples <- 0.8
  if (!exists("group_thr_nr_of_samples"))
    group_thr_nr_of_samples <- 0.7

  # calculate the mean value for the blanks  for each lipid species
  l1_blanks <- colMeans(l1[ind_blanks, -1, drop = FALSE],
                        na.rm = TRUE)
  # get the index of the samples which are lower then the blank
  ind_below_blank = which(t(t(l1[ind_samples, -1]) <= blank_thr * l1_blanks),
                          arr.ind = TRUE)
  # correct the column because of sampleID column
  ind_below_blank[, 2] = ind_below_blank[, 2] + 1
  # how many samples are below the blank for each lipid species
  table_ind_below_blank <- table(ind_below_blank[, 2])

  # which proportion of samples is above the threshold
  ind_rem1 <- which((table_ind_below_blank / length(ind_samples)) > blank_thr_nr_of_samples)
  ind_rem <- names(table_ind_below_blank[ind_rem1])

  ind_rem_ind_rem <-  NULL
  # which proportion of samples from one group is above the threshold
  for (i in c(1:length(ind_rem))) {
    tmp_l1_i <- cbind(meta_s, l1[ind_samples, c(as.numeric(ind_rem[i]))])
    tmp_a <- table(tmp_l1_i$Genotype[which(tmp_l1_i[, length(tmp_l1_i[1, ])] >
                                             (blank_thr * l1_blanks[as.numeric(ind_rem[i]) - 1]))])

    tmp_b <- table(tmp_l1_i$Genotype)
    if (any(tmp_a / tmp_b >= group_thr_nr_of_samples))
      ind_rem_ind_rem <- c(ind_rem_ind_rem, i)
  }

  if (length(ind_rem_ind_rem) > 0)
    ind_rem <- ind_rem[-ind_rem_ind_rem]

  if (length(ind_rem) > 0) {
    l2 <- l1[, -as.numeric(ind_rem)]
  } else {
    l2 <- l1
  }

  ### normalisation total class area
  l3 <- l3_no_lip_norm <- l2[ind_samples, ]
  l3_2 <- l3_2_no_lip_norm <- l2[ind_samples, ]

  classes <- sapply(as.character(names(l3)), function(x)
    strsplit(x = x,
             split = " ",
             fixed = TRUE)[[1]][1])

  classes_unique <- unique(classes)

  for (i in c(1:length(classes_unique))) {
    # which column belongs to which class
    ind_c <- which(classes == classes_unique[i])

    # do the normalisation total area of a lipid class
    for (j in c(1:nrow(l3))) {
      l3[j, ind_c] <- l3[j, ind_c] / sum(l3[j, ind_c])
    }
  }

  # total area normalisation
  l3_2 <- l3_2 / rowSums(l3_2)


  ### make all data.frames long
  l3_long  <- tidyr::pivot_longer(data = cbind(meta_s, l3),
                                  cols = -colnames(meta_s),
                                  names_to = "variable",
                                  values_to = "value")

  l3_2_long  <- tidyr::pivot_longer(data = cbind(meta_s, l3_2),
                                    cols = -colnames(meta_s),
                                    names_to = "variable",
                                    values_to = "value")

  ### Make lipid class data.frame
  # total class area normalisaton
  # add sample names
  meta_sample <- meta_s
  meta_sample$sample_name <- paste0("Sample_", 1:nrow(meta_sample))

  l4 <- tidyr::pivot_longer(data = cbind(meta_sample, l3),
                            cols = -colnames(meta_sample),
                            names_to = "variable",
                            values_to = "value")
  l4$class <- sapply(strsplit(x = l3_long$variable, split = " "), "[[", 1)

  l4_long <- aggregate(x = l4$value,
                       by = list(l4$sample_name, l4$class),
                       FUN = function(x) sum(x, na.rm = TRUE),
                       drop = FALSE)
  colnames(l4_long) <- c("sample_name", "class", "value")

  l4_long <- merge(l4_long, meta_sample, by = "sample_name")


  l4 <- tidyr::pivot_wider(data = l4_long,
                           names_from = .data$class,
                           values_from = .data$value)

  ### Make lipid class data.frame
  # total lipid species area normalisation
  # add sample names
  meta_sample <- meta_s
  meta_sample$sample_name <- paste0("Sample_", 1:nrow(meta_sample))

  l4_2 <- tidyr::pivot_longer(data = cbind(meta_sample, l3_2),
                              cols = -colnames(meta_sample),
                              names_to = "variable",
                              values_to = "value")
  l4_2$class <- sapply(strsplit(x = l3_2_long$variable, split = " "), "[[", 1)

  l4_2_long <- stats::aggregate(x = l4_2$value,
                                by = list(l4_2$sample_name, l4_2$class),
                                FUN = function(x) sum(x, na.rm = TRUE),
                                drop = FALSE)
  colnames(l4_2_long) <- c("sample_name", "class", "value")

  l4_2_long <- merge(l4_2_long, meta_sample, by = "sample_name")


  l4_2 <- tidyr::pivot_wider(data = l4_2_long,
                             names_from = .data$class,
                             values_from = .data$value)


  ###
  # will be returned
  all_data <- list(
    data_classNorm = l3,
    data_totLipidNorm = l3_2,
    data_classNorm_melt = l3_long,
    data_totLipidNorm_melt = l3_2_long,
    classData_classNorm = l4,
    classData_classNorm_melt = l4_long,
    classData_totLipidNorm = l4_2,
    classData_totLipidNorm_melt = l4_2_long,
    meta = meta_s
  )

  return(all_data)
}
