# Summary
---------

This a repository collecting shiny apps I have been generating

a) VC_explorer v1 is completed. [Synthea](https://github.com/synthetichealth/synthea) generated patients/control cohort with breast cancer data. [VC_explorer v1 App](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/VC_explorer_v1/)

b) VCExplorer_CT is working. A combination of labor generated data merged with RR data and derivatives thereof from probands wearables. [VCExplorer_CT App](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/VCExplorer_CT/)

c) IME_Score2 is very similar to VCExplorer_CT. There are no wearables data involved. It seems to be more like a beta version. [IME_Score2 App](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/IME_score2/)

d) Colorectal_100 is yet-another virtual cohort based on [Synthea](https://github.com/synthetichealth/synthea). Basic visulization are same as in the above examples. [Colorectal](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/colorectal_100/colorectal_100_app/)

e) HRV_V2 is different. It tries to displays RR_intervals and BPM from cardio measurements coming from wearables (usually 24hrs). From these measurements a lot of derived parameters are produced. [See here for details](https://github.com/Aura-healthcare/hrvanalysis). A PCA for the cohort is provided and finally a set of gauge indicators will try to summarize the stress level of probands. [HRV_V2](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/HRV_V2/) 

# Instructions
--------------

To run the shiny apps, you can either [run on mybinder.org](https://mybinder.org/v2/gh/agiani99/R_shinyapps/) or build locally with [repo2docker](https://repo2docker.readthedocs.io/).

If you decide for direct run clicking links under mybinder, it might take few minutes to build the underlining virtual machine, so be patient and let it run.

To build locally:

 * Install [Docker](https://www.docker.com/) if required
 * Create a virtual environment and install repo2docker from PyPI.
 * Clone this repository
 * Run ``repo2docker``
 * Depending on the permissions, you might have to run the command as an admin and give --user-id xxxx and --user-name name 

```
pip install jupyter-repo2docker
git clone https://github.com/agiani99/R_shinyapps.git
cd R_shinyapps
repo2docker .
```
