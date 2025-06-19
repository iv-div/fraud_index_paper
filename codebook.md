# 📘 Codebook: Regional Electoral Fraud Dataset (Russia, 2000–2018)

This codebook describes the structure and variables of the dataset produced in the project **"Measuring Regional Electoral Fraud in Russia"**, which constructs a region-year level index of electoral fraud using precinct-level data from nine federal elections between 2000 and 2018.

---

## 📂 `precinct_df` – Precinct-Level Data

| Variable                | Type     | Description                                                             |
|-------------------------|----------|-------------------------------------------------------------------------|
| `year`                 | Integer  | Election year                                                           |
| `region`               | String   | Name of the region                                                      |
| `region_code`          | String   | Region code (standardized identifier)                                   |
| `tik`                  | String   | Territorial election commission ID                                      |
| `uik`                  | String   | Precinct election commission ID                                         |
| `registered_voters`   | Integer  | Number of registered voters at the precinct                            |
| `valid_ballots`       | Integer  | Number of valid ballots cast                                           |
| `invalid_ballots`     | Integer  | Number of invalid ballots                                              |
| `votes_for_incumbent` | Integer  | Number of votes for the incumbent candidate or party                    |
| `turnout_abs`         | Integer  | Absolute turnout (valid + invalid ballots)                              |
| `turnout_share`       | Numeric  | Turnout as share of registered voters                                   |
| `incumbent_share`     | Numeric  | Incumbent's vote share among total ballots cast                         |
| `p001turnout`         | Numeric  | Probability of 1% round result in turnout                               |
| `p005turnout`         | Numeric  | Probability of 5% round result in turnout                               |
| `p001incumbent`       | Numeric  | Probability of 1% round result in incumbent vote share                  |
| `p005incumbent`       | Numeric  | Probability of 5% round result in incumbent vote share                  |
| `001round_turnout`    | Integer  | 1 if reported turnout is a multiple of 1%, 0 otherwise                  |
| `005round_turnout`    | Integer  | 1 if reported turnout is a multiple of 5%, 0 otherwise                  |
| `001round_incumbent`  | Integer  | 1 if incumbent vote share is a multiple of 1%, 0 otherwise              |
| `005round_incumbent`  | Integer  | 1 if incumbent vote share is a multiple of 5%, 0 otherwise              |
| `round_turnout_value` | Integer  | Nearest k% round for turnout (if any)                                   |
| `round_incumbent_value`| Integer | Nearest k% round for incumbent share (if any)                           |
| `fraud_group`         | Factor   | Quartile grouping of the fraud index                                    |
| `round_group`         | String   | Grouping: round index = 0 or > 0                                        |
| `corr_group`          | String   | Grouping: correlation index = 0 or > 0                                  |
| `label`               | String   | Group label used in visualization facets                                |

---

## 📂 `region_summary` – Region-Year Aggregated Indicators

| Variable                   | Type     | Description                                                                |
|----------------------------|----------|----------------------------------------------------------------------------|
| `region_code`             | String   | Region identifier                                                          |
| `year`                    | Integer  | Election year                                                              |
| `n_obs`                   | Integer  | Number of precincts in the region-year                                     |
| `sum_registered_voters`  | Integer  | Total registered voters                                                    |
| `sum_turnout_abs`        | Integer  | Total turnout                                                              |
| `sum_votes_for_incumbent`| Integer  | Total incumbent votes                                                      |
| `overall_turnout`        | Numeric  | Regional turnout (total turnout / total registered)                        |
| `overall_incumbent_share`| Numeric  | Regional incumbent share (total incumbent votes / total turnout)           |
| `sum_001round_turnout`   | Integer  | Precincts with 1% round turnout                                            |
| `sum_005round_turnout`   | Integer  | Precincts with 5% round turnout                                            |
| `sum_001round_incumbent` | Integer  | Precincts with 1% round incumbent share                                    |
| `sum_005round_incumbent` | Integer  | Precincts with 5% round incumbent share                                    |
| `sum_p001turnout`        | Numeric  | Expected number with 1% round turnout                                      |
| `sum_p005turnout`        | Numeric  | Expected number with 5% round turnout                                      |
| `sum_p001incumbent`      | Numeric  | Expected number with 1% round incumbent share                              |
| `sum_p005incumbent`      | Numeric  | Expected number with 5% round incumbent share                              |
| `correlation`            | Numeric  | Pearson correlation (turnout vs incumbent share) in precincts              |
| `correlation_index`      | Numeric  | Sub-index: Correlation indicator (0 if corr < 0.05, else raw value)        |
| `round_index`            | Numeric  | Sub-index: Max observed/expected share where p < 0.05                      |
| `fraud_index`            | Numeric  | Final fraud index: mean of correlation_index and round_index               |
| `fraud_group`            | Factor   | Group 0 = no fraud; Groups 1–4 = fraud quartiles                         |

---

## 📝 Intermediate Variables Used During Processing

| Variable                  | Type     | Description                                                                 |
|---------------------------|----------|-----------------------------------------------------------------------------|
| `chisq_pval_005turnout`  | Numeric  | p-value from chi-squared test (5% turnout)                                  |
| `chisq_pval_001turnout`  | Numeric  | p-value from chi-squared test (1% turnout)                                  |
| `chisq_pval_005incumbent`| Numeric  | p-value from chi-squared test (5% incumbent)                                |
| `chisq_pval_001incumbent`| Numeric  | p-value from chi-squared test (1% incumbent)                                |
| `flag_005turnout`        | Integer  | 1 if p < 0.05 for 5% turnout anomaly                                        |
| `flag_001turnout`        | Integer  | 1 if p < 0.05 for 1% turnout anomaly                                        |
| `flag_005incumbent`      | Integer  | 1 if p < 0.05 for 5% incumbent anomaly                                      |
| `flag_001incumbent`      | Integer  | 1 if p < 0.05 for 1% incumbent anomaly                                      |
| `round_index_005turnout` | Numeric  | Observed/expected ratio (if significant) for 5% turnout                     |
| `round_index_001turnout` | Numeric  | Same, for 1% turnout                                                        |
| `round_index_005incumbent`| Numeric | Same, for 5% incumbent                                                      |
| `round_index_001incumbent`| Numeric | Same, for 1% incumbent                                                      |

---

## 📂 Validation Datasets

### `oporaindex.csv`

| Variable              | Type     | Description                                       |
|-----------------------|----------|---------------------------------------------------|
| `region_code`         | String   | Region identifier                                |
| `opora_2012_rank`     | Integer  | Opora ranking of regional institutional quality  |

### `ombudsmanindex.csv`

| Variable              | Type     | Description                                       |
|-----------------------|----------|---------------------------------------------------|
| `region_code`         | String   | Region identifier                                |
| `ombudsman_rank`      | Integer  | Ranking of administrative pressure, 2019         |

### `informal_employment` (built from Rosstat data)

| Variable              | Type     | Description                                       |
|-----------------------|----------|---------------------------------------------------|
| `region_russian_name` | String   | Region name (in Russian)                         |
| `2001` ... `2017`     | Numeric  | Share of informally employed (by year)           |

