# COVID-19 Data Representativeness versus Actionability: An Avoidable Trade-Off?

## Abstract

On the 29th of January 2021 the governing board of the European Bioinformatics Institute (EBI) posted a public letter in Nature calling for a greater “openness” in sharing SARS-CoV-2 genome data. The letter argued that “to unleash the fast flow of research advances” the scientific community must remove all formal barriers which restrict data sharing and share all SARS-CoV-2 genome sequences to one of a triad of state genomic surveillance programs (EBI, The GenBank of USA and the DNA Data Bank of Japan). The letter was signed and promoted by Nobel Laureates, Directors of Bioinformatic programs and many researchers at the cutting edge of genome sequencing. At the same time, the Global Initiative on Sharing Avian Influenza Data (GISAID) had just overtaken the EBI’s European COVID-19 Data Portal (C19DP) in the volume of genome sequences being shared to open access databases. GISAID was launched in 2008 to monitor global influenza outbreaks and from the offset positioned itself as an alternative to the public domain sharing model. Its policy requires users to authenticate their academic identity and agree not to republish or link GISAID genomes with other datasets without permission from the data producer. This requirement stems from the recognition that some researchers – often working in low-resourced environments and/or less visible research locations – are reluctant to share data due to fears of better-equipped researchers building on such work without due acknowledgment. Indeed, the GISAID model has fostered trust and information exchange among groups that differ considerably in their geo-political locations, funding levels, material resources and social characteristics, thereby expanding the range of data sources shared online. This proved decisive when, at the beginning of 2020, GISAID launched the EpiCov database which stores, analyses and builds evolutionary trees of SARS COV-2 genome sequences – now the leading open access database for SARS-CoV-2, with over 9 million genomes sequenced by April 2022. At the same time, limiting the extent to which data can be accessed and linked can negatively affect the insight, pace and breadth of future research – leading to the backlash by hundreds of leading researchers concerned about the urgency of an effective pandemic response.

This case encapsulates a tension between actionability and representativeness that is often found in data science initiatives within the biological and biomedical sciences. On the one hand, it is argued, data sharing is only relevant when data can be used to support discovery, thereby becoming ‘actionable’: any constraint on data circulation is therefore conceptualised as an obstacle to knowledge production. On the other hand, data sharing is pointless unless enough data are contributed by a wide and diverse set of sources, thereby guaranteeing that the data is representative of the research communities engaged in investigating the phenomena at hand. We argue that this tension can be negotiated by a model of responsible data sharing that enhances data actionability without sacrificing user protection measures, thereby attempting to maximise both representativeness and actionability. Crucially, such a model can only work when accepting that data do not need to be transparent, or anyhow easily accessible, in order to be actionable. 

## Data and Methods
Metadata on SAS-Cov2 sequences were collected from GISAID and the Covid-19 Data Portal via their online web interfaces between the epidemiological weeks of the 23rd of December 2019 and 1st of October 2021. Epidemiological data were gathered from the complete Covid-19 dataset made available from  Our World in Data (OWID) (https://github.com/owid/covid-19-data) via a stable public url. Global new case counts were extracted from this dataset which originally come from the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) (https://github.com/CSSEGISandData/COVID-19).  

The aforementioned data sources were linked together using country codes defined by ISO 3166-1 alpha-2. Percentages of Covid-19 cases sequenced per country - cumulatively and weekly - were calculated by aggregating weekly submissions and case counts using a custom R script (https://github.com/natesheehan/OPEN-GM/blob/main/code/data-wrangle.r). Visualisations and plots were assembled using ggplot2 - a data visualisation package for the statistical programming language R - and are made accessible via the following open access scripts (https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-temporal-submissions.r, https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-treemap.r and https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-continent-landscape.r). 


## Data availability

The data that support the findings of this study are available from: https://github.com/natesheehan/OPEN-GM/data/

## Code availability

The code that support the findings of this study are available from: https://github.com/natesheehan/OPEN-GM/code/

## Acknowledgements

We would like to thank the authors from the originating labs - together with the submitting labs -  who were responsible for collecting, codifying and sharing the pathogen related data to which this analysis relies on. To this end, an acknowledgement table can be found from gisaid.org with the following set accession id: EPI_SET_220927xo (https://github.com/natesheehan/OPEN-GM/blob/main/gisaid_supplemental_table_epi_set_220927xo.pdf).  

We would also like to thank the Our World in Data team for their efforts in collecting and curating the epidemiological data used in this study. 

## Competing interests

The authors declare no competing interests.

## Funding

This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme (grant agreement No. 101001145). This paper reflects only the author's view and that the Commission / Agency is not responsible for any use that may be made of the information it contains.

## Author contributions

S.L. conceived and designed the study and wrote the manuscript. N.S. assisted in the design of the study, performed the analysis and discussed the results.

