# COVID-19 Data Representativeness versus Actionability: An Avoidable Trade-Off?

## Abstract

This paper discusses a tension between actionability and representativeness that is often found in data science initiatives within the biological and biomedical sciences. On the one hand, engaging in data sharing efforts only makes sense if data can be used to support discovery, thereby becoming ‘actionable’ – a consideration that leads some advocates of open data to argue that any constraint on data circulation constitutes an obstacle to knowledge production. On the other hand, making data actionable for knowledge development presupposes that the data are representative of the phenomena being studied. This in turn assumes that: (1) enough data are contributed by a wide and diverse set of relevant sources; and (2) mechanisms of feedback and inclusion are set up to ensure that data contributors can participate data governance and interpretation, thereby helping to adequately contextualise data. All too often, the requirements for actionability and representativeness of data are conceptualised as incompatible and leading to a trade-off situation where increasing one will unavoidably decrease the other. Through an analysis of two different platforms used to share genomic data about the SARS-COV-2 virus, we critique this framing as damaging to data initiatives and infrastructures. We argue that the tension between actionability and representativeness can be negotiated by a model of responsible data sharing that enhances users’ ability to work with data without sacrificing data protection measures and mechanisms of fair equitable governance, leading to an inclusive approach that maximises both representativeness and actionability. Crucially, such a model can only work when accepting that data do not need to be transparent, or even easily accessible, in order to be actionable; and that including a variety of contributors in efforts of data governance and interpretation may slow down the pace of discovery while boosting the robustness and quality of outputs. 

## Data and Methods

### Global aggregations of Sequence and Epidemiological Data
A recent surge of data science studies have started to explore the hetrogenitiy of the global genomic survlience landscape of the SARS-CoV-2 virus  (Khare .et al  2021, Samlali  2021, Brito .et al 2022,).However, to our knowledge, there has not been a explicit comparison between the GISAID database and its acclaimed foe the Covid-19 Data Portal. To fill this lacuna in the existing academic landscape, our analysis collected over 19 million data points for the respective databases between the epidemiological weeks of the 23rd of December 2019 and 1st of October 2021. Like the previously mentioned studies, we use epidemiological data from the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) (https://github.com/CSSEGISandData/COVID-19) to report on global new case counts. The aforementioned data sources were linked together using country codes defined by ISO 3166-1 alpha-2. Percentages of Covid-19 cases sequenced per country - cumulatively and weekly - were calculated by filtering the data into weekly submissions and aggregating counts per country and continent; this was achieved using a custom R script (https://github.com/natesheehan/OPEN-GM/blob/main/code/data-wrangle.r). Visualisations were crafted together using the ggplot2 visualisation package and are made accessible via the following open access scripts (https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-temporal-submissions.r, https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-treemap.r and https://github.com/natesheehan/OPEN-GM/blob/main/code/plot-continent-landscape.r).

### TCP/IP stack fingerprinting

TCP/IP stack fingerprinting (OS fingerprinting) is a function often used by hackers - ethical and not so ethical - to find out the characteistics of a system they may or may not have access too. As one author in the popular open source security package `nmap` puts it: "The legal ramifications of scanning networks with Nmap are complex and so controversial that third-party organizations have even printed T-shirts and bumper stickers promulgating opinions on the matter". OS fingerprinting works by remotely accessing a number of features in the TCP/IP stack implementation and comparing them to previosuly defined combinations of parameters to infer matches. For the purpose of this study, we deploy this function to uncover the various degrees of openness each database is designed with, as well as the various services and steps in place to access SARS-CoV-2 metadata. We use the open source `whatweb` command line tool to retrieve data concerning geographical location, author, type of server, and different types of plugins/libraries present in the system etc. The following command was used for each portal on the October 31st 2022. ```./whatweb -v https://www.dataportal-url.org -a 1```. Where -v gives a verbose output of the results and -a 1 represents a soft level of pen test. The complete output for each data portal can be found in the supplementary material.
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

