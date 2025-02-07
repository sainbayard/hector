![R-CMD](https://github.com/JGCRI/hector/workflows/R-CMD/badge.svg)
![Command Line Hector](https://github.com/JGCRI/hector/workflows/Command%20Line%20Hector/badge.svg)
![test-coverage](https://github.com/JGCRI/hector/workflows/test-coverage/badge.svg)
[![codecov](https://codecov.io/gh/JGCRI/hector/branch/master/graph/badge.svg?token=EGM0lXDxRv)](https://codecov.io/gh/JGCRI/hector)
![lint](https://github.com/JGCRI/hector/workflows/lint/badge.svg)

Hector
======

This is the repository for **Hector**, an open source, object-oriented, simple global climate carbon-cycle model. It runs essentially instantaneously while still representing the most critical global scale earth system processes, and is one of a class of models heavily used for for emulating complex climate models and uncertainty analyses. For example, Hector's global temperature rise for the RCP 8.5 scenario, compared to observations and other model results, looks like this:

![](https://github.com/JGCRI/hector/wiki/rcp85.png)

The primary link to Hector model documentation is the [online manual](https://jgcri.github.io/hector/articles/manual), which is included in the repository in the `vignettes/manual` directory.
The code is also documented with [Doxygen](http://doxygen.org)-style comments. A formal model description paper ([Hartin et al. 2015](http://www.geosci-model-dev.net/8/939/2015/gmd-8-939-2015.html)) documents its science internals and performance relative to observed data, the [CMIP5](http://cmip-pcmdi.llnl.gov/cmip5/) archive, and the reduced-complexity [MAGICC](http://www.magicc.org) model (as of [version 1.0](https://github.com/JGCRI/hector/tree/v1.0)). In addition, we have developed two package vignettes demonstrating the [basics of the Hector R interface](http://jgcri.github.io/hector/articles/intro-to-hector.html) and an example application of [solving for an emissions pathway](http://jgcri.github.io/hector/articles/hector_apply.html).

This research was supported by the U.S. Department of Energy, Office of Science, as part of research in Multi-Sector Dynamics, Earth and Environmental System Modeling Program. The Pacific Northwest National Laboratory is operated for DOE by Battelle Memorial Institute under contract DE-AC05-76RL01830.

## Tools and Software That Work with Hector

* [GCAM](https://github.com/JGCRI/gcam-core): Hector can be used as the climate component in [GCAM](http://jgcri.github.io/gcam-doc/).
* [pyhector](https://github.com/openclimatedata/pyhector): a Python
  interface to Hector.
* [HectorUI](https://jgcri.shinyapps.io/HectorUI/): run Hector in a web interface!

 ##  Contributing to Hector

 The Hector team welcomes and values community contributions but please see our [Contribution Guide](KRD-UPDATE).
