# ethnoshiny

**Quantitative ethnobotany in a Shiny app, started with one command.**

ethnoshiny is an R package that puts the standard quantitative ethnobotany workflow into a single point-and-click application. It is for researchers, students and conservation practitioners who collect interview data on plant use and want reliable analyses, figures and reports without writing code.

```r
library(ethnoshiny)
ethnoshiny()
```

## What it does

| Area | What you get |
|---|---|
| **Data** | Upload CSV or Excel files, automatic validation with clear error messages, a downloadable data template |
| **Taxonomy** | Check scientific names (optional online check against GBIF via `rgbif`) |
| **Indices** | Use value (UV), relative frequency of citation (RFC), cultural importance (CI), fidelity level (FL), informant consensus factor (ICF) |
| **Description** | Informant profile, use categories, map of study sites, "Visual lab" with several unconventional charts |
| **Comparison** | Compare groups (for example villages) |
| **Statistics** | Correlation, NMDS ordination, PERMANOVA |
| **Network** | Network graph of species and use categories (interactive with `visNetwork`) |
| **Report** | Export results to Excel, or a full report as HTML or Word |

Other details:

- Charts use the colour-blind-friendly **viridis** palette.
- **Scientific names are written in italics** on axes and tables.
- Optional packages unlock extra features; without them the app still runs with fallbacks.

## Installation

```r
install.packages("remotes")
remotes::install_github("whisnuafrianto/ethnoshiny")
```

Optional packages for the full feature set:

```r
install.packages(c("visNetwork", "leaflet", "ggVennDiagram", "rgbif"))
```

| Optional package | Unlocks |
|---|---|
| `visNetwork` | Interactive network |
| `leaflet` | Base map of study sites |
| `ggVennDiagram` | Venn diagram |
| `rgbif` | Online scientific-name check (GBIF) |

Requires R 4.1.0 or newer.

## Quick start

```r
library(ethnoshiny)

ethnoshiny()                          # start with the built-in example data
ethnoshiny(ethnoshiny_example())      # pass a data frame
ethnoshiny("my_interviews.xlsx")      # pass your own CSV / Excel file
```

You can also upload or replace the data inside the app (tab **Upload and validate**).

### Try it with example data

The built-in example is **synthetic**: 66 informants, 31 species and 876 use reports in 3 villages. It is invented for demonstration and does not describe real communities.

## Preparing your data

Write an empty template to your working folder:

```r
ethnoshiny_template("template_ethnobotany.csv")
```

One row is one use report: one informant mentioning one species for one use.

| Column | Required | Example |
|---|---|---|
| `informant_id` | **yes** | INF001 |
| `scientific_name` | **yes** | Curcuma longa |
| `use_category` | **yes** | Medicinal |
| `gender`, `age`, `village`, `ethnic_group`, `occupation`, `experience_years` | no | Female, 52, Village A |
| `local_name`, `family`, `habitus` | no | Kunyit, Zingiberaceae, herb |
| `plant_part`, `preparation`, `administration` | no | rhizome, decoction, drunk |
| `ailment` | no | digestive disorders |
| `latitude`, `longitude` | no | -7.80120, 111.93870 |

Tips:

- Use one row per informant–species–use combination. An informant who uses the same plant for food and medicine gets two rows.
- Fill `ailment` for medicinal uses if you want fidelity level and informant consensus factor.
- Add `latitude` and `longitude` (decimal degrees) to see the map.
- Rows missing a required value are removed, and the app tells you what was removed.

## Main functions

| Function | Purpose |
|---|---|
| `ethnoshiny()` | Launch the app |
| `ethnoshiny_app()` | Build the app object without running it (for deployment) |
| `ethnoshiny_example()` | The built-in synthetic data frame |
| `ethnoshiny_template()` | Write an empty CSV template |

## Deployment

Use `ethnoshiny::ethnoshiny_app()` as the entry point of an `app.R` file for shinyapps.io, Shiny Server or Posit Connect (including Connect Cloud). The server needs the `ethnoshiny` package installed.

```r
# app.R
ethnoshiny::ethnoshiny_app()
```

For Posit Connect Cloud, install the package from GitHub locally, then run `rsconnect::writeManifest()` in the folder that contains `app.R`, and publish that folder from GitHub.

## Citation

If you use ethnoshiny in your work, please cite it as:

> Afrianto, W. F. (2026). *ethnoshiny: Quantitative Ethnobotany in a Shiny App* (R package version 0.1.3). https://github.com/whisnuafrianto/ethnoshiny

## Author and licence

Whisnu Febry Afrianto (whisnu.afrianto@gmail.com)

Released under the MIT licence. See the `LICENSE` file.

## Contributing and issues

Found a bug or have a feature request? Please open an issue at
https://github.com/whisnuafrianto/ethnoshiny/issues
