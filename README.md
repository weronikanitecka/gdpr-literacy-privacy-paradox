# Behind the Curtain: GDPR Literacy and the Privacy Paradox

This repository contains replication code for a study testing whether individual-level GDPR awareness translates into privacy-protective behavior among European citizens, and whether this relationship is mediated by perceived control over personal data.

Using **Special Eurobarometer 487a** (EB 91.2, 2019, N = 10,263) and weighted binary logistic regression, the paper finds that hearing of GDPR increases the probability of changing privacy settings by **14.0 percentage points**, and each additional data right known adds a further **6.0 percentage points** — while stated concern about data use is negatively associated with action, replicating the privacy paradox.

📄 Full paper in `/paper/nitecka_2025_gdpr_literacy.pdf`

---

## Research Questions

1. Does individual GDPR awareness increase privacy-protective behavior (changing privacy settings)?
2. Is this relationship mediated by perceived control over personal data?

---

## Methodology

### Identification Strategy

Three binary logistic regression models decompose the total effect of GDPR awareness:

| Model | Specification |
|---|---|
| **M1** — Total effect | GDPR awareness → privacy-protective behavior |
| **M2** — Mediator model | GDPR awareness → perceived control |
| **M3** — Full model | GDPR awareness + perceived control → behavior |

Formal mediation via nonparametric bootstrapping (5,000 simulations) decomposes M1 into ACME (indirect via perceived control) and ADE (direct effect).

### Specification

```
log[P(Yi=1) / (1−P(Yi=1))] = β₀ + β₁Heard_i + β₂Score_i + β₃ConcUse_i + β₄ConcTrack_i + β₅BaseCtrl_c + ε_i
```

All models estimated with Eurobarometer post-stratification weights. Results reported as log-odds coefficients and average marginal effects (AMEs).

### Key Variables

| Variable | Source | Description |
|---|---|---|
| Changed settings (DV) | QB11, EB 91.2 | Binary: changed privacy settings on a platform |
| GDPR heard | QB17, EB 91.2 | Binary: heard of GDPR |
| Knowledge score | QB18, EB 91.2 | Count 0–6: number of GDPR rights known |
| Perceived control (mediator) | QB9, EB 91.2 | Binary: feels some/a lot of control over data |
| Concern: data use | QB10, EB 91.2 | Ordinal: concern about commercial data use |
| Concern: tracking | QB7, EB 91.2 | Ordinal: concern about online tracking |
| Country baseline control | QB4, EB 83.1 | Country-mean pre-GDPR felt control (2015) |

---

## Key Results

| Finding | Result |
|---|---|
| GDPR heard → behavior | +14.0 pp (AME, 95% CI [12.2, 15.8]) |
| Each right known → behavior | +6.0 pp per right; +36 pp for full knowledge vs. none |
| Concern about data use → behavior | −3.7 pp *(privacy paradox)* |
| Concern about tracking → behavior | −5.8 pp *(privacy paradox)* |
| Mediation via perceived control | 3.0% of total effect (negligible) |

GDPR awareness motivates action through **direct pathways** — likely norm salience and rights activation — not through enhanced felt control. 97% of the awareness–behavior association is unmediated by QB9.

---

## Data

### Source

**GESIS — Special Eurobarometer 487a / Wave EB 91.2 (2019)**  
🔗 https://www.gesis.org/en/eurobarometer-data-service/data-and-documentation/standard-special-eb/study-overview/eurobarometer-912-za7562-march-2019

**GESIS — Special Eurobarometer 431 / Wave EB 83.1 (2015)**  
🔗 https://search.gesis.org/research_data/ZA5964

*(Free GESIS account required to download)*
