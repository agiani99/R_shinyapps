# Summary
---------

This a repository collecting shiny apps I have been generating

a) VC_explorer v1 is completed. [Synthea](https://github.com/synthetichealth/synthea) generated patients/control cohort with breast cancer data. [VC_explorer v1 App](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/VC_explorer_v1/)

b) VCExplorer_CT is an advanced version of IME_Score2. The idea is to display some parameters from a virtual observational cohort especially tailored towards fitness and prevention. [VCExplorer_CT App](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/VCExplorer_CT/)

c) IME_Score2 is very similar to VCExplorer_CT. There are no wearables data involved. [IME_Score2 App](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/IME_score2/)

d) Colorectal_100 is yet-another virtual cohort based on [Synthea](https://github.com/synthetichealth/synthea). Basic visulization are same as in the above examples. [Colorectal](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/colorectal_100/colorectal_100_app/)

e) HRV_V2 is different. It tries to displays RR_intervals and BPM from cardio measurements coming from wearables (usually 24hrs). From these measurements a lot of derived parameters are produced. [See here for details](https://github.com/Aura-healthcare/hrvanalysis). A PCA for the cohort is provided and finally a set of gauge indicators will try to summarize the stress level of probands. [HRV_V2](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/HRV_V2/)

f) This Shiny [Hospitalization](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/Hospitalization_param/) app has to do with the choice of best parameter to obtain the more realistic hospitalization time for a cohort in order to obtain a target Kaplan-Meier plot.

g) [Public COVID-19 reference data](https://github.com/HAIRLAB/Pre_Surv_COVID_19/tree/master/data) have been used for the following Shiny for [COVID-19 Hospitalization](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/COVID19_Hospitalization/). Here a comparison through the Kaplan-Meier plots is provided so that the parameters chosen for the virtual cohort can be checked against the real-world data.

h) Starting from data reported in a [glioblastoma trial](https://pubmed.ncbi.nlm.nih.gov/28861666/), Takoua Korchani and me developed a [Virtual Cohort](https://mybinder.org/v2/gh/agiani99/R_shinyapps/master?urlpath=shiny/Glioblastoma_1_4_v1/) which generates n patients with either Grad_I or Grad_IV glioblastoma and which can be visualized here

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
