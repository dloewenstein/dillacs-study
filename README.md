---
title: Diagnostic performance of left ventricular mechanical dyssynchrony indices using CMR feature tracking
author: Daniel E Loewenstein MD, Björn Wieslander MD PhD, Einar Heiberg PhD, Jimmy Axelsson MD, Igor Klem MD, Robin Nijveldt MD PhD, Erik B Schelbert MSc MD, Peder Sörensson MD PhD, Andreas Sigfridsson PhD, David G Strauss MD PhD, Raymond Kim MD, Brett D Atwater MD, Martin Ugander MD PhD
---

Diagnostic performance of left ventricular mechanical dyssynchrony indices using CMR feature tracking
=====================================================================================================

Instructions
------------

Data and code to reproduce all aspects of the current study is available in the
provided zip file.

To reproduce the study results and manuscript you will need to install [Docker](https://https://www.docker.com/).
Further usage instructions can be found [here](https://docs.docker.com/get-started/).

In a new terminal, navigate to the folder containing the contents of the provided zip file.

Start with building the docker image by running the following command in a terminal

```shell
docker build --tag dillacs .
```

You have now built the docker image containing the software needed to reproduce the study.

Proceed by spinning up a container based on the newly built image. The container
will automatically run all code in the correct order.

```shell
docker run --rm -it --name dillacs --mount type=bind,source=$(pwd),target=/project dillacs
```

(If you are on a windows machine please change the `source=$(pwd)` to `source=C:\\path_to_the_extracted_zip_file`
where you provide the full pathway to the extracted archive.)

The resulting manuscript file can then be found in: `./analysis/paper/_book/Loewenstein-et-al.---Diagnostic-performance-of-left-ventricular-mechanical-dyssynchrony-indices-using-CMR-feature-tracking.docx`

CMR files are analyzed in [Segment](https://medviso.com/) (v3.0, Medviso&reg;,
Lund, Sweden) which is freely available for download to view the CMR images.

A detailed outline of the image content of each exam is provided in the file
named `image_data_content.csv`. In short all CMR exams contain CMR cine
shortaxis stacks covering the left ventricle, 78/80 contain cine 2-chamber
images, 79/80 contain cine 3-chamber images, and 79/80 contain cine 4-chamber
images. All subjects had no focal abnormalities by LGE imaging (images not
provided).

Below is an overview and short explanation of the files contained in, and produced by the current study:

```shell

.
├── analysis
│   ├── data
│   │   ├── derived_data                        # Data produced during code excecution
│   │   │   ├── cleaned_data.Rds                # CMR and demographics data
│   │   │   ├── dyssynchrony_data.Rds           # Addition of dyssynchrony analysis results
│   │   │   ├── figs.Rds                        # Figures data
│   │   │   └── statistics.Rds                  # Results from statistical analysis
│   │   └── raw_data
│   │       ├── demographics.csv                # Demographics data for subjects enrolled in the study
│   │       └── mri_data
│   │           ├── analyzed                    # CMR exams delineated and with feature tracking results
│   │           └── original                    # Original CMR exams
│   ├── figures
│   │   ├── dyssynchrony_distribution.tiff
│   │   ├── performance_measure_cutoff.tiff
│   │   └── roc_by_dyssynchrony_index.tiff
│   ├── paper
│   │   ├── abstract.Rmd                        # Article abstract
│   │   ├── appendix.Rmd                        # Contains images and table 1
│   │   ├── auxilliary
│   │   │   ├── references.bib                  # Biblatex reference file
│   │   │   └── vancouver.csl                   # Citation style format
│   │   ├── background.Rmd                      # Article background
│   │   ├── _book
│   │   │   ├── Loewenstein-et-al.---Diagnostic-performance-of-left-ventricular-mechanical-dyssynchrony-indices-using-CMR-feature-tracking.docx                      # Final article
│   │   │   ├── abstract.md                     # Generated
│   │   │   ├── appendix.md                     # Generated
│   │   │   ├── background.md                   # Generated
│   │   │   ├── discussion.md                   # Generated
│   │   │   ├── methods.md                      # Generated
│   │   │   ├── reference-keys.txt              # Generated
│   │   │   └── results.md                      # Generated
│   │   ├── _bookdown_files
│   │   │   └── appendix_files
│   │   │       └── figure-docx
│   │   │           ├── dysboxplot-1.tiff       # Generated, inserted into paper
│   │   │           ├── dysrocplot-1.tiff       # Genereated, inserted into paper
│   │   │           └── performanceplot-1.tiff  # Generated, inserted into paper
│   │   ├── _bookdown.yml                       # Configuration file for bookdown library
│   │   ├── discussion.Rmd                      # Article discussion
│   │   ├── _main.rds                           # Generated, cached data during article rendering
│   │   ├── methods.Rmd                         # Article methods
│   │   ├── _output.yml                         # Configuration file for bookdown output format
│   │   ├── preamble.tex                        # Configuration for double spacing for pdf output format
│   │   ├── reference.docx                      # Word docx style template
│   │   └── results.Rmd                         # Article results
│   └── scripts                                 # R code extracted from vignettes
│       ├── data_processing-GEN.R
│       ├── data_processing-GEN.Rout
│       ├── dyssynchrony_analysis-GEN.R
│       ├── dyssynchrony_analysis-GEN.Rout
│       ├── figures-GEN.R
│       ├── figures-GEN.Rout
│       ├── statistics-GEN.R
│       └── statistics-GEN.Rout
├── dillacs.Rproj                               # Rstudio project file, containing project specific options
├── Dockerfile                                  # Docker build file, contains instructions for building docker image with required software to reproduce the current study
├── env.yml                                     # Conda environment configuration file, specifies required software libraries
├── Makefile                                    # Makefile, runtime automation instructions for correct runtime order and excecution
├── R                                           # Custom R functions
│   ├── formatCons.R
│   ├── geom_signif.R
│   ├── get_segment_data.R
│   ├── offset2groups.R
│   ├── strain_cure.R
│   └── strain_ssi.R
├── README.md                                   # This README
└── vignettes
    ├── LP-data_processing.Rmd                  # Code for extracting CMR and demographics data
    ├── LP-dyssynchrony_analysis.Rmd            # Code for performing dyssynchrony analysis
    ├── LP-figures.Rmd                          # Code generating figures of study results
    └── LP-statistics.Rmd                       # Code for performing statistical analysis

```
