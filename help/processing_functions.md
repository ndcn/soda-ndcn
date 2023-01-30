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
