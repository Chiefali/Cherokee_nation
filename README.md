# A Data Science Workflow for Strengthening Tribal Public Health Surveillance
**Development and Evaluation of a Social Determinants of Health Dashboard and Training Framework**

**Author:** Joseph Ali  
**Partner:** Cherokee Nation Public Health (CNPH)

---

## Overview

This repository documents a data science workflow developed to strengthen Tribal public health surveillance under real‑world data constraints. The project centers on an interactive dashboard that integrates Social Determinants of Health (SDOH) data from **federal sources**, **State of Oklahoma public health data**, and **Cherokee Nation gestational diabetes data**, while explicitly addressing challenges such as small population sizes, data suppression, and cross‑source inconsistency.

The dashboard organizes 20 county‑level SDOH indicators into six domains—**economic instability, educational attainment, housing and transportation, health care access, environmental burden, and social context**—and provides multiple analytic views to support exploration, comparison, and public health and policy planning.

---

## Why This Project Is Important

Tribal public health programs frequently rely on data systems designed outside of Tribal governance structures, population definitions, and service contexts. This misalignment complicates interpretation, obscures uncertainty, and limits the effective use of data for local decision‑making.

This project addresses these challenges by applying the **Indigenous Data Sovereignty framework**, specifically the **CARE Principles (Collective Benefit, Authority to Control, Responsibility, and Ethics)**. It demonstrates that effective surveillance requires more than data visualization alone. By embedding training and interpretability supports directly into the analytic workflow, the project strengthens local capacity to recognize data limitations, evaluate trade‑offs, and use complex data responsibly in Tribal public health practice.

---

## What This Repository Provides

- A reproducible SDOH dashboard integrating Tribal, state, and federal data  
- Analytic modules for spatial mapping, county profiling, clustering, and scenario exploration  
- An embedded training framework focused on interpretation and methodological understanding  
- An applied example of Indigenous Data Sovereignty in public health analytics  

---

## Intended Audience

- Tribal public health analysts and epidemiologists  
- Tribal health leadership and planners  
- Researchers and practitioners working with Indigenous or small‑area data  

---

## Indigenous Data Sovereignty

All analytic design choices prioritize Tribal governance, contextual interpretation, and local analytic capacity. The dashboard is intended to support—rather than replace—Tribal expertise and decision‑making.

---
# Cherokee_nation

Project files for Cherokee Nation dashboard / analysis.

## Structure
- code/ : R scripts
- renv.lock : package dependency lockfile

## Setup
Open R and run:

renv::restore()
EOF

## Dashboard Footer (Optional)

> *This dashboard integrates Social Determinants of Health data across Tribal, state, and federal systems to support locally governed public health decision‑making.*  
> *By pairing interactive analytics with embedded training, it strengthens the capacity to interpret uncertainty and apply data meaningfully within Tribal contexts.*