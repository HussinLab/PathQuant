# PathQuant (Pathway Quantity)

---------------

R package to *quantify* gene-metabolite associations (e.g. from mGWAS) to 
[KEGG](http://www.genome.jp/kegg/)'s pathways.

*Quantify*: calculate distance defined as the shortest path between a given gene
and a metabolite in a chosen pathway.

---------------

### Installation

```r
--- code to run package ---
# If you do not have devtools installed
install.packages("devtools")

# Install PathQuant 
library(devtools)
devtools::install_github("HussinLab/PathQuant")
library("PathQuant")
```

### Information

License: [GNU General Public License (v3)](http://www.gnu.org/licenses/gpl-3.0.en.html)

### Instructions

* [Userguide Manual](https://github.com/HussinLab/PathQuant/blob/main/manual.pdf)

### How to cite
Baron C, Cherkaoui S, Therrien-Laperriere S, Ilboudo Y, Poujol R, Mehanna P, Garrett ME, Telen MJ, Ashley-Koch AE, Bartolucci P, Rioux JD, Lettre G, Rosiers CD, Ruiz M, Hussin JG. Gene-metabolite annotation with shortest reactional distance enhances metabolite genome-wide association studies results. iScience. 2023 Nov 14;26(12):108473. doi: 10.1016/j.isci.2023.108473. PMID: 38077122; PMCID: PMC10709128.
* [Article](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10709128/)

### Authors
Cantin Baron,  
Sandra Therrien-Laperriere,  
Sarah Cherkaoui. 




