# Audit of Refalo et al. (2024)

Python verification of the 16 findings in the I4R AI-generated audit of *Similar muscle hypertrophy following eight weeks of resistance training to momentary muscular failure or with repetitions-in-reserve in resistance-trained individuals*.

## Contents

- `claims.py`: claim-by-claim calculations and checks.
- `claims_models.py`: Bayesian model reconstruction and additional statistical calculations.
- `pkg/Data`: seven Excel workbooks supplied with the authors' replication materials.
- `pkg/R Code (QMD scripts)`: the authors' five Quarto scripts.
- `pkg/claims_script.R`: the supplied AI-audit verification script.
- `pkg/*.pdf`: the article supplied with the audit package.

The Python audit was developed with AI assistance. Author materials and the supplied AI-audit script are distinguished from the independent Python checks. Individual comments describe findings, assumptions and limitations; not every statement in an audit claim is established by a matching numerical check.

## Run

Use Python 3.13 in a virtual environment and install the recorded dependencies:

```sh
python -m pip install -r requirements.txt
python claims.py
```

Paths are resolved relative to the script. The article is read from the supplied PDF using PyMuPDF, or from `paper/paper.txt` if available.

On a fresh checkout the Bayesian models are fitted using PyMC, with four chains, 1,000 tuning iterations and 1,000 retained draws per chain. This can take substantial time and may require a working compiler for efficient execution. NetCDF posterior caches are saved locally and reused on subsequent runs. They are excluded from version control. The cache lookup uses the filename, not a data or model fingerprint: refit after changing data, priors or model specifications. Reconstructed Python fits are not the authors' original R fit objects, and numerical results can vary across runs and environments.

## Sources and attribution

Original article: Refalo, M. C., Helms, E. R., Robinson, Z. P., Hamilton, D. L., and Fyfe, J. J. (2024). *Similar muscle hypertrophy following eight weeks of resistance training to momentary muscular failure or with repetitions-in-reserve in resistance-trained individuals*. Journal of Sports Sciences, 42(1), 85–101. https://doi.org/10.1080/02640414.2024.2321021

Original replication repository: https://osf.io/34d92/

This repository contains the local copy used for verification; it does not establish the current completeness of the live OSF repository. Original authors retain attribution for their data, code and article. No new licence is applied to third-party materials.

## Citation

Use the repository's **Cite this repository** entry and include the commit identifier for the version examined. `CITATION.cff` describes this audit package, not the original study.
