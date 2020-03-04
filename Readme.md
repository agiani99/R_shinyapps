This a repository collecting shiny apps I have been generating

a) VC_explorer v1 is not completed. Synthea generated patients/control cohort with breast cancer data. [VC_explorer v1 App](https://mybinder.org/v2/gh/agiani99/shinyapps/binder?urlpath=shiny/VC_explorer_v1/)

b) VCExplorer_CT is somewhat working. A combination of labor generated data merged with RR data and derivatives thereof from probands wearables. [VCExplorer_CT App](https://mybinder.org/v2/gh/agiani99/shinyapps/binder?urlpath=shiny/VCExplorer_CT/)

c) Vindex2 is very similar to VCExplorer_CT but is working. There are no wearables data involved. Anyway, seems to be more like a beta version. [Vindex2 App](https://mybinder.org/v2/gh/agiani99/shinyapps/binder?urlpath=shiny/Vindex2/VCExplorerv2/)


# Instructions
--------------

To run the shiny apps, you can either [run on mybinder.org](https://mybinder.org/v2/gh/agiani99/shinyapps/) or build locally with [repo2docker](https://repo2docker.readthedocs.io/).


To build locally:

 * Install [Docker](https://www.docker.com/) if required
 * Create a virtual environment and install repo2docker from PyPI.
 * Clone this repository
 * Run ``repo2docker``
 * Depending on the permissions, you might have to run the command as an admin

```
pip install jupyter-repo2docker
git clone https://github.com/agiani99/shinyapps.git
cd R_shinyapps
repo2docker .
```
