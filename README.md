# ISI-LCA Classifier (Shiny App)

**Author:** Matteo Carpi  

---

## Overview

This interactive Shiny app maps new **Insomnia Severity Index** (ISI; Bastien et al., 2001) responses to the four latent classes identified in Carpi et al. (2025):

- **NI** – No Insomnia  
- **SI** – Subthreshold Insomnia  
- **HI** – High Insomnia Risk  
- **DS** – Predominant Daytime Symptoms  

The app computes posterior class probabilities for each individual response profile, provides ISI total and subscale scores, and visualizes the subject’s pattern against class means.

---

## How it works

Given item responses \(x_1,\dots,x_7\) (0–4), posterior probabilities are computed as:

\[
P(\text{class}=k|\mathbf{x}) \propto \pi_k \prod_{i=1}^{7} \theta_{i,\,x_i+1,\,k}
\]

- \(\pi_k\): class prior (mixing proportion)  
- \(\theta_{i,c,k}\): conditional probability that item *i* takes category *c* in class *k*  
- \(x_i+1\): category index shift ([poLCA](https://dlinzer.github.io/poLCA/) uses 1–5 internally)  

All posteriors are normalized to sum to 1.

---

## Files

| File | Description |
|------|--------------|
| `app.R` | Shiny app script |
| `probs.csv` | Conditional item response probabilities |
| `README.md` | This documentation |

### `probs.csv` structure

| item | category | class | prob |
|------|-----------|-------|------|
| ISI1a | 1 | NI | 0.02 |
| ISI1a | 2 | NI | 0.07 |
| … | … | … | … |

---

## Class priors

Used from the LCA model mixing proportions:

```r
priors_model <- c(NI=0.3181, HI=0.1835, SI=0.3563, DS=0.1422)
```

These are automatically normalized within the app.

---

## Running locally

```r
install.packages(c("shiny","readr","dplyr","tidyr","ggplot2"))
shiny::runApp("path/to/folder")
```

---

## Using the app

### Manual entry
- Choose 0–4 for each ISI item via dropdowns.  
- Click **Score subject** to compute:
  - Posterior probabilities  
  - Most probable class (MAP)  
  - ISI total and subscales  
  - Profile plot vs. class means  

### Batch mode
Upload a `.csv` file with columns:

```
id, ISI1a, ISI1b, ISI1c, ISI2, ISI3, ISI4, ISI5
```

The app returns a table with MAP class, posteriors, and ISI totals per subject.

---

## ISI interpretation guide

| ISI total | Clinical interpretation |
|------------|------------------------|
| 0–7 | No clinically significant insomnia |
| 8–14 | Subthreshold insomnia |
| 15–21 | Moderate insomnia |
| 22–28 | Severe insomnia |

Subscales (Castronovo et al., 2016):

- **Severity:** ISI1a + ISI1b + ISI1c  
- **Dissatisfaction:** ISI1a + ISI2 + ISI5  
- **Impact:** ISI3 + ISI4 + ISI5

---

## Interpretation

This app implements the probability model underlying latent class analysis, estimating the likelihood that a given response pattern belongs to each insomnia subtype (this corresponds to the probability model used in LCA, equivalent to a simple naïve Bayes classifier).  
Outputs are **probabilistic**, not diagnostic, and the model was trained on a sample of **Italian university students**; generalization to other populations is untested.

---

## License & Citation

MIT License – Matteo Carpi (2025)

If you use this code, please cite:

<small>Carpi M. et al. (2025). *Deriving subtypes from the Insomnia Severity Index: a latent class analysis and comparison of features.* *Behavioral Sleep Medicine.* https://doi.org/10.1080/15402002.2025.2539961</small>

**Permanent archive:** OSF repository at https://osf.io/xxxxx

---


**Contact:**  
Matteo Carpi – Sapienza University of Rome  
Email: matteo.carpi@uniroma1.it
