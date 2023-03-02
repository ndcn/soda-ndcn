Plot tables
=======================
---

Tables related to the plot data, stored in the Omics_data class.  
<details>
<summary> <b>Class distribution table</b> </summary>

```
Omics_data$tables$class_distribution_table
```
</details>

<details>
<summary> <b>Class comparison tables</b> </summary>

Truffles
</details>

<details>
<summary> <b>Volcano table</b> </summary>

Table used to produce the volcano plot. Takes as input the *lipidomics table filtered* and the *lipidomics table z-score normalised*. Both these tables contain NA values, and the z-scoring on the latter is done by omiting NAs.  
Two groups are selected by the user to compare their features by calculating their fold changes on the *lipidomics table filtered* (median(group_2) / median(group_1)) and their p-values on *lipidomics table z-score normalised* using a Wilcoxon test.  
The p-values are then adjusted using the Benjamini-Hochberg procedure (BH). A table is then created with the lipid species as rows, with for columns log2(fold change), -log10(BH adjusted p-value) and the lipid class associated to each row.  

**If groups contain only NAs:** this usually happens as a result of filtering too much data out, out just having too few data to begin with, resulting in comparing two groups, at least one of which has only NAs for a given feature. In this case, NAs must be imputed by some other value:  
- If both groups contain only NAs, fold change and p-value are set to 1.  
- If one of the groups has only NAs and is the denominator (first group / group 1): p-value will be set to 0.99 x min(p-value) and fold change to 1.01 x max(fold change) (i.e. slightly above the maximum fold change in the comparison).  
- If one of the groups has only NAs and is the nominator (second group / group 2): p-value will be set to 0.99 x min(p-value) and fold change to 0.99 x min(fold change) (i.e. slightly below the minimum fold change in the comparison).  

```
Omics_data$tables$volcano_table
```
</details>

<details>
<summary> <b>Heatmap table</b> </summary>

```
Omics_data$tables$heatmap_table
```
</details>

<details>
<summary> <b>PCA tables</b> </summary>


```
Omics_data$tables$pca_scores_table
```
```
Omics_data$tables$pca_loadings_table
```
</details>

<details>
<summary> <b>Double bonds table</b> </summary>

```
Omics_data$tables$dbplot_table
```
</details>
