## Abstract

## Data and Methods
Metadata on SAS-Cov2 sequences were collected from GISAID and the Covid-19 Data Portal via their online web interfaces between the epidemiological weeks of the 23rd of December 2019 and 1st of October 2021. Epidemiological data were gathered from the complete Covid-19 dataset made available from  Our World in Data (OWID) (https://github.com/owid/covid-19-data) via a stable public url. Global new case counts were extracted from this dataset which originally come from the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) (https://github.com/CSSEGISandData/COVID-19).  

The aforementioned data sources were linked together using country codes defined by ISO 3166-1 alpha-2. Percentages of Covid-19 cases sequenced per country - cumulatively and weekly - were calculated by aggregating weekly submissions and case counts using the a custom R script (https://github.com/natesheehan/OPEN-GM/blob/main/code/data-wrangle.r) Visualisations and plots were assembled using ggplot2 - a data visualisation package for the statistical programming language R - and are made accessible via the following open scripts (https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-temporal-submissions.r, https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-treemap.r and https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-continent-landscape.r). 


## Data availability

The data that support the findings of this study are available from: https://github.com/natesheehan/OPEN-GM/data/

## Code availability

The code that support the findings of this study are available from: https://github.com/natesheehan/OPEN-GM/data/

## Acknowledgements

We would like to thank the authors from the originating labs - together with the submitting labs -  who were responsible for collecting, codifying and sharing the pathogen related data to which this analysis relies on. To this end, an acknowledgement table can be found from gisaid.org with the following set accession id: EPI_SET_220927xo.  

We would also like to thank the Our World in Data team for their efforts in collecting and curating the epidemiological data used in this study. 

## Competing interests

The authors declare no competing interests.

## Funding

This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme (grant agreement No. 101001145). This paper reflects only the author's view and that the Commission / Agency is not responsible for any use that may be made of the information it contains.

## Author contributions

S.L. conceived and designed the study and wrote the manuscript. N.S. assisted in the design of the study, performed the analysis and discussed the results.

