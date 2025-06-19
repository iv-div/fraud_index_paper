# Measuring Regional Electoral Fraud in Russia (2000–2018) #
This repository supports the article "Measuring Regional Electoral Fraud", which introduces a new fraud index for Russian regions based on precinct-level electoral results. The index captures two manipulation patterns:

Correlation index: Turnout and incumbent vote share correlation (ballot stuffing).

Roundness index: Excess of round percentage results (protocol falsification).

The index is correlated with institutional quality indicators and informal employment data.

## 📂 Repository Structure: ##

### 📄 Main Article ###
Measuring Regional Electoral Fraud.pdf – Full paper detailing methods, results, and validation of the fraud index.

### 🧮 R Scripts ###
| File                          | Purpose                                                                 |
|-------------------------------|-------------------------------------------------------------------------|
| `1_downloading_data.R`        | Downloads and merges raw precinct data (2000–2018).                     |
| `2_creating_fraud_index.R`    | Builds fraud index using roundness & correlation metrics.              |
| `3_visualisation.R`           | Creates Kobak diagrams and fraud index visualizations.                 |
| `4_component_correlation.R`   | Analyzes correlation between subcomponents of the index.               |
| `5_validationinformality.R`   | Validates fraud index using Rosstat informal employment.               |
| `6_validation_ombudsman.R`    | Validates index using Opora and Ombudsman rankings.                    |
| `MonteCarlo.R`                | Simulates ballot stuffing to illustrate the correlation index.         |

### 🐍 Python Script ###
create_round_prob_table.py – Calculates theoretical probabilities of round % results; outputs round_probabilities_table.csv.

### 📁 data ###
| File                           | Description                                                            |
|--------------------------------|------------------------------------------------------------------------|
| `ombudsmanindex.csv`           | “Administrative Pressure — 2019” index.                                |
| `oporaindex.csv`               | Entrepreneurial climate in Russia: OPORA Index–2012                    |
| `region_code_converter.csv`    | Region name/code lookup table.                                         |
| `round_probabilities_table.csv`| Used to detect regions with suspiciously round numbers reported.       |

### 📁 data/RosstatData/ (informal employment data) ###
.html, .xls, .docx files: extracted tables from Rosstat (2001–2017) used in	5_validationinformality.R for index validation.

### 💾 Saved Data ###
elections_data.RData – Intermediate data file (from downloading/cleaning script).


## 🙏 Acknowledgements ##
The dataset used in this article was collected by Sergey Shpilkin and Dmitry Kobak and is stored on [Dmitry Kobak's GitHub](https://github.com/dkobak/elections)

The Russian regional tilemap adapted from the open-source repository [quillcraft/tilemap-russia](quillcraft/tilemap-russia) for geographic visualizations


