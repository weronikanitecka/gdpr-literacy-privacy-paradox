# Behind the Curtain: GDPR Literacy and the Privacy Paradox

> Does knowing about GDPR actually change what people do? This paper tests whether individual-level GDPR awareness translates into privacy-protective behavior among 10,263 European citizens, using weighted binary logistic regression and formal mediation analysis.

---

## Key findings

- **+14 pp** increase in probability of changing privacy settings for those who have heard of GDPR
- **+6 pp** per additional GDPR right known (36 pp total for full literacy vs. zero knowledge)
- **3%** of the effect is mediated by perceived control — the dominant pathway is direct
- Concern about data use and tracking is *negatively* associated with protective behavior — replicating the privacy paradox

---
## Data

Special Eurobarometer 487a (EB 91.2, 2019) and Special Eurobarometer 347 (EB 83.1, 2015), both administered by the European Commission. Data are publicly available via the [GESIS data archive](https://www.gesis.org/en/eurobarometer-data-service).

## Replication

```r
Rscript code/gdpr-literacy-code-commented.R
```

Requires: `tidyverse`, `haven`, `mediation`, `marginaleffects`, `MASS`, `brant`, `car`

## Methods

Weighted binary logistic regression · Formal mediation analysis (5,000 bootstrap simulations) · Brant test for proportional odds · Average marginal effects · Three robustness checks
