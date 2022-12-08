# A comparison of the benefits of open-access publishing across various sub-fields in Biology

[![DOI](https://zenodo.org/badge/367076215.svg)](https://zenodo.org/badge/latestdoi/367076215)

As an extension of a class project, we investigated the benefits of open-access (OA) publishing across different hybrid access journals in Biology. We selected 152 journals across 12 sub-fields totalling nearly 150k articles. We download citation information directly from Web of Science and merged this with meta-data compiled on each targeted journal.

Abstract

Authors are often faced with the decision of whether to maximize impact or minimize costs when publishing the results of their research. For example, to potentially improve impact via increased accessibility, many subscription-based journals now offer the option of paying a fee to publish open access (i.e., hybrid journals), but this solution excludes authors who lack the capacity to pay to make their research accessible. Here, we tested if paying to publish open access in a subscription-
based journal benefited authors by conferring more citations relative to Closed Access articles. We identified 146,415 articles published in 152 hybrid journals in the field of biology from 2013-2018 to compare the number of citations between various types of open access and Closed Access articles. In a simple generalized linear model analysis of our full dataset, we found that publishing open access in hybrid journals that offer the option confers an average citation advantage to authors
of 17.8 citations compared to Closed Access articles in similar journals. After taking into account the number of authors, journal impact, year of publication, and subject area, we still found that open access generated significantly more citations than closed access (p < 0.0001). However, results were complex, with exact differences in citation rates among access types impacted by these other variables. This citation advantage based on access type was even similar when comparing open
and Closed Access articles published in the same issue of a journal (p < 0.0001). However, by examining articles where the authors paid an article processing charge, we found that cost itself was not predictive of citation rates (p = 0.14). Based on our findings of access type and other model parameters, we suggest that, in most cases, paying for access does confer a citation advantage. For authors with limited budgets, we recommend pursuing open access alternatives that do not require paying a fee as they still yielded more citations than Closed Access. For authors who are considering where to submit their next article, we offer additional suggestions on how to balance exposure via citations with publishing costs

In this github repository is our raw data as well as all the code used to analyze the data. The project is organized into three main folders:

- Data
  - This folder contains the raw data and meta-data used to conduct our analysis. 
- Outputs
  - This folder contains the outputs of our analysis from the scripts used to conduct the analysis.
- Scripts
  - This folder contains the Rproject as well as the scripts used to combine the raw data into a format useful for analysis. We also include code for our statistical models as well as the figures and tables in the manuscript document.

By selecting hybrid journals, we were able to directly comapre the impact of paid OA (other Gold) under a model where the author pays an APC directly to the publisher and free OA (Green) where the author makes their work open access by depositing into an institutional repository or uploading a pre-print. We feel this novel approach to investigating the benefits of OA publishing yields important insights for Biologists regarding decisions in where they publish their work and how they choose to make it available to others.

This project started as part of a graduate level course at Auburn University BIOL 6800: Introduction to Computational Biology. The Graduate TA worked closely with university librarians to develop an assignment where students selected jounrals and did an independent investigation on only their selected journals for a grade. As an extension of the class, we asked students to continue with the project if desired and we re-downloaded the data in a common framework, vastly expanding our journal selection and streamlining our data collection process. A subset of students have continued to help with the data collection, develop the statistical analysis, and/or write the manuscript. 
