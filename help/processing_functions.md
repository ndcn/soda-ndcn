Data Processing
=======================
---

## Blank and group filtering
**Blank multiplier:** for a given feature, number of times above the blank mean each sample must be to be considered a significative value.  
**Sample threshold:** for a given feautre, proportion of the total amount of samples that must be above the blank values x blank multiplier for that feature to be kept.  
**Group threshold:** secondary threshold for the main group of interest. Works like "Sample threshold" but instead of being in proportion to the total amount of samples, it will be in proportion to the number of samples in each group.  

Given the following values:  
- Blank multiplier = 2  
- Sample threshold = 0.8
- Group threshold = 0.8

This is a two-step filtering, first blank filtering based on all samples, then group filtering based on the samples from each group. The first filter marks many features for deletion, and the second is meant to save some of these features by by checking if they would pass the same filter based on a smaller portion of the dataset (groups). The blank samples are selected from the metadata text pattern recognition.  

**Blank filtering.** For each feature, the mean of all blanks is calculated and a threshold is calculated for each column / feature : blank_multiplier x blank_mean (i.e. 2 x the blank mean of each feature). If 80% of the total samples (sample threshold = 0.8) have values above 2 x blank_mean, the feature is kept. Otherwise the feature is marked for deletion.  
**Group filtering.** Operates on the features marked for deletion after blank filtering to save some of them. Processing is the same as above, but this time on groups (group column selected in data upload). If for any of the groups, 80% (group threshold = 0.8) of the samples are above 2 x blank mean, the feature is saved from deletion.  

Features marked for deletion are then removed from the feature table.  

## Lipidomics table filtered
Raw values with rows filtered (without blanks, QCs, pool samples or any sample filtered out by the user) and features filtered (blank and group filtering). 

## Lipidomics table total normalised
Values for each lipid in are normalised by dividing them by the total lipid concentration in each sample. NAs are imputed by 0.  

## Lipidomics table z-score normalised
Z-score normalisation of the *Lipidomics table filtered*. NA values are left as is (NA) and the mean values are calculated by omiting NA values.  

## Lipidomics class table
A class table is produced by getting all the lipid classes from the lipidomics total normalised table to total lipids to obtain a sample x lipid class table. For each sample, the class value is calculated by summing the values of the lipid species of that class in that sample. NA values are imputed by 0.  

## Lipidomics volcano table
Table used to produce the volcano plot. Takes as input the *lipidomics table filtered* and the *lipidomics table z-score normalised*. Both these tables contain NA values, and the z-scoring on the latter is done by omiting NAs.  
Two groups are selected by the user to compare their features by calculating their fold changes on the *lipidomics table filtered* (median(group_2) / median(group_1)) and their p-values on *lipidomics table z-score normalised* using a Wilcoxon test.  
The p-values are then adjusted using the Benjamini-Hochberg procedure (BH). A table is then created with the lipid species as rows, with for columns log2(fold change), -log10(BH adjusted p-value) and the lipid class associated to each row.  
