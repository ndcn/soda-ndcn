Lipidomics filter tab
=======================
---

Tab to filter out features and keep only the significant ones.  

1. **Feature count**
Features remaining in the *Lipidomics filtered table* after filtering.  
2. **Feature filtering**
Select either lipid classes or lipid compounds individually, and then choose either to drop them (remove them) or to keep them (remove everything else).  
3. **Blank and Group filtering**
This is used to remove features that are not significantly above blank.  
- Blank multiplier: for a given feature, number of times above the blank mean each sample must be to be considered a significative value.  
- Sample threshold: for a given feautre, proportion of the total amount of samples that must be above the blank values x blank multiplier for that feature to be kept.  
- Group threshold: secondary threshold for the main group of interest. Works like "Sample threshold" but instead of being in proportion to the total amount of samples, it will be in proportion to the number of samples in each group.  
4. **Save or Reset buttons**  
- The save button saves the filtering to the *Lipidomics filtered table*  
- The reset button reverts the *Lipidomics filtered table* to its original imported state.  
5. **Class preview plots**
Two bar plots displaying the absolute and relative compound count per lipid class. The first one is absolute count, with the total compounds in each class and the remaining count after filtering. The second one is the relative count, displaying the percentage of compounds remaining in each class after filtering.  
6. **Download filtered data button**  
Downloads the *Lipidomics filtered table* as a CSV file.  

<img src="./img/upload_lips_filter.png" width="100%">
