# R codes and scripts
In this folder you can find individual R scripts of the data munging, cleaning, merging and analysis as well as the skeletons for the 2 rmarkdown files (pdf & html skeletons).

There are two R scripts for gathering data:

* [One R script for gathering World Bank data](https://github.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/blob/master/Task2_Cosmin-Covid-Assignment/codes/getWDI_data.R)
* [One R script for gathering CSSE John Jopkinds University Covid-19 data](https://github.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/blob/master/Task2_Cosmin-Covid-Assignment/codes/getCSSE_data.R)

There are three R scripts for cleaning data:

* [One for cleaning WDI data](https://github.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/blob/master/Task2_Cosmin-Covid-Assignment/codes/cleanWDI_data.R)
* [One for cleaning CSSE data](https://github.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/blob/master/Task2_Cosmin-Covid-Assignment/codes/cleanCSSE_data.R)
* [One for merging the two cleaned datasets](https://github.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/blob/master/Task2_Cosmin-Covid-Assignment/codes/merge%26clean_data.R)

There is one R script for analyzing data:

* [Data analysis script](https://github.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/blob/master/Task2_Cosmin-Covid-Assignment/codes/analyzeCOVID_data.R)

There are two rmarkdown scripts for creating reports:

* [One for knitting a PDF](https://github.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/blob/master/Task2_Cosmin-Covid-Assignment/codes/cosmin_covid_analysis_report_pdf.Rmd)
* [One for knitting an HTML](https://github.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/blob/master/Task2_Cosmin-Covid-Assignment/codes/cosmin_covid_analysis_report.Rmd)

The main structure of this code folder means that the gathering and cleaning datasets are self-standing and are not contained within either of the rmarkdown scripts for knitting the analysis reports. The rmarkdown scripts can be considered as the more elegant variant of the R script used for analysis, but they essentially borrow the same structure and close to the same content from each other. As such, if this analysis is to be reproduced, run the R scripts for gathering and cleaning and run either of the rmarkdown files in order to achieve the same results.
