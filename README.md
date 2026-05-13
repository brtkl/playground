# Pharmaverse Playground

Reproducible clinical reporting portfolio using the [pharmaverse](https://pharmaverse.org) ecosystem.

**Data:** CDISC Pilot Project (ADaM) — Alzheimer's disease Phase III reference trial  
**Site:** built with [Quarto](https://quarto.org), deployed to GitHub Pages from `docs/`

## Structure

```
├── _quarto.yml              # Quarto website configuration
├── index.qmd                # Landing page
├── about.qmd                # About page
├── reports/                 # Clinical report QMDs
├── assets/                  # CSS and static assets
├── docs/                    # Rendered site (GitHub Pages source)
└── statprog_environment/    # Statistical programming workspace
    ├── data/adam/           # ADaM datasets (CDISC Pilot)
    ├── pgms/                # Analysis programs
    ├── outputs/             # TLF outputs
    └── ...
```

## Rendering

```bash
quarto render
```

Output goes to `docs/` for GitHub Pages deployment.
