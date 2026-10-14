# Jintao Lyu — Selected Research

**Who gains from economic change?**

I study how technological and institutional change reshape economic opportunities, income and wealth. My research combines computational macro-financial modeling with firm–city evidence, with a focus on distribution, fiscal institutions and uneven economic transformation in China.

This collection presents selected research artifacts, the work I contributed, and the questions and evidence behind each visualization.

## Jobs or Transfers?

**Job Guarantees, Fiscal Design, and the Dynamics of Inequality**  
Working paper · with Huiqing Li

How does fiscal design affect the distribution of policy gains? I developed a stock-flow consistent computational model with five heterogeneous household groups and implemented 56 policy–state–horizon configurations, including baselines, under a common net fiscal cost. I trace feedbacks between household demand, private sales, investment, employment and profit–dividend flows.

[![Household income sources and uses across five household groups](jobs-or-transfers/figures/20_income_uses.svg)](jobs-or-transfers/README.md)

*A baseline model snapshot of income sources and household uses. This figure shows economic structure rather than an estimated policy effect.*

**[Explore the project and three visualizations →](jobs-or-transfers/README.md)**

## The Other Side of Progress

**High-Tech Expansion and Urban Crime**  
Undergraduate research paper · Advisor: Lanlan Wang

How does high-tech expansion relate to uneven local adjustment and recorded crime? I processed approximately 7.5 million firm-year tax-survey records and constructed a linked city panel. The selected artifacts bring together conditional regression evidence and cities' initial industrial structures.

[![Archived estimates of recorded crime, structural moderation and related outcomes](high-tech-and-urban-crime/figures/B_forest.svg)](high-tech-and-urban-crime/README.md)

*Archived observational estimates. Measurement and identification remain under review; these associations are not causal effects.*

**[Explore the project and two visualizations →](high-tech-and-urban-crime/README.md)**

## Read and run the research code

| Research example | What it contains | Inputs and execution |
|---|---|---|
| [JG model and analysis](jobs-or-transfers/README.md#run-the-model-example) | The original formal equation file, a small normal-state runner, and income-source/Gini and accounting-check excerpts | Runs with R and `ineq`; no external data required |
| [Frozen empirical benchmark](high-tech-and-urban-crime/README.md#run-the-frozen-baseline) | A portable extraction of the actual L1 regression, its unchanged numerical assertions, and a clear input contract | Requires Stata, `reghdfe`/`ftools`, and the restricted canonical analytical panel; data are not distributed |

The examples contain research-model and estimation logic, rather than image-generation code alone. Each project page gives the command, software requirements, expected outputs, provenance and the scope of the checks. Generated results and restricted datasets are excluded from this repository.

## About this collection

Prepared by **Jintao Lyu** · [GitHub profile](https://github.com/karllouisunder)  
Research-artifact snapshot: **14 October 2026**

The selection emphasizes my contributions to computational modeling, linked-data construction and scientific communication. Project pages explain authorship, methods, figure provenance and limits of interpretation.

This collection contains selected visualizations and research-code examples. It includes the formal JG model snapshot and a focused runner, but not the full 56-configuration research pipeline, independent model variants, restricted empirical data, the raw-to-panel construction chain or full manuscripts. It is a selected research-code collection, not a complete replication package for all displayed artifacts or papers.

No additional reuse license is granted by this repository. Rights in jointly authored work and third-party source material remain with the respective rights holders. Public availability should not be interpreted as permission to redistribute underlying data.
