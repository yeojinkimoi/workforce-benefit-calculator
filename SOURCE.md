# workforce-benefit-calculator

Here we list all the sources and assumptions made by this benefit calculator

  County-dependent (vary by county):                                                           
  - ALICE Expenses — childcare, transportation, food, healthcare, housing costs are all        county-specific lookups    


    Adult age (avg_age)                                                                                                                                                                       - EITC (childless): Hard eligibility cliff at ages 25 and 65. Below 25 or above 64 with no   kids → EITC drops to $0 (swing of ~$3k+).                                                    - SSI/SSDI: Only relevant if disability flags are set (not typical for workforce training).  - Medicaid: Non-MAGI elderly threshold at 60+; usually small impact.

  Child age (child_age)

  - CTC: Cliff at age 18 — child 17 gets $2,000 credit, child 18 gets $0.
  - CCDF: Age buckets 0–4 vs 5–12 have different daily care rates and copays (can swing
  $500–$2,000).
  - WIC: Cliff at age 5 — eligible at 4, ineligible at 5 (~$200/mo).
  - Head Start: Early Head Start (0–2) vs Head Start (3–4) vs ineligible (5+).

  Programs where age does NOT matter

  SNAP, federal/state income tax, ACA — these don't use age fields.

The current defaults (avg_age=25, child_age=4) --  they keep the adult just
  inside EITC eligibility and the child in the richest benefit bucket (CTC-eligible, CCDF
  0–4, WIC-eligible, Head Start-eligible). The validation sweep only tests at these fixed
   ages