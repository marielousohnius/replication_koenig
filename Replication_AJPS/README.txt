
This archive contains all the data and program files needed to replicate the analysis in the main text and supplemental materials for "The Political Consequences of External Economic Shocks" by John S. Ahlquist, Mark Copelovitch and Stefanie Walter, American Journal of Political Science.  

*Data files:
- Oct2015CBOS.Rdata: the merged raw data from CBOS (Polish polling firm)
- Oct2015CBOS.csv: ditto, but as flat text. 
- AJPScleanedData.Rdata: contains the object "octdat", the cleaned data with variable names corresponding to the codebook
- AJPScleanedData.csv: ditto, but as flat text
- AJPSimputations.RData: contains the (Amelia) object "imp.out" which includes the 20 imputed datasets with variable names corresponding to the codebook.
- PLN_CHF_EUR_FXdata.csv: daily closing values for th CHF-EUR and CHF-PLN exchange rate from 11/3/2014 to 11/30/2015 

*Code:
- ACW_AJPS_imputation.R: takes Oct2015CBOS.Rdata and produces AJPScleanedData.Rdata and AJPSimputations.RData.  Replicating exact results from the manuscript requires use of R for OSX. 
- ACW_AJPS_analysis_code.R: conducts all analysis and produces all figures and tables.

*Metadata
- codebook.pdf includes a description of all the variables in the cleaned dataset and the imputations. 

To replicate all tables and figures in the analysis, simply install AJPSimputations.RData, AJPScleanedData.Rdata, and PLN_CHF_EUR_FXdata.csv in a working directory and then execute the file ACW_AJPS_analysis_code.R.

To assemble the cleaned dataset and produce the imputed datasets, load Oct2015CBOS.Rdata and execute the file ACW_AJPS_imputation.R 

All progams were run on R version 3.3.1 for x86_64-apple-darwin13.4.0