# Analysis Code - Rocker

> NOTE: This code is modified/improved from old repo [this repository/experiment](https://gitlab.tudelft.nl/mln-lab-public/r-analysis-alex-van-den-berg-2024-vfd-experiment). Readme's etc still need some updating.

The code used for the analysis of ... .

With the files in this repository, you are able to run the code from any device that is able to install and run [Docker](https://docs.docker.com/desktop/install/windows-install/#install-interactively).

Links:
- [Zenodo data upload](https://zenodo.org/uploads/17414638)
> This repository already contains the calculated step parameters datasets (`./runtime/results/*.rds`). If you want to recalculate step parameters, delete these files, and put the participant data folders in the `./runtime/data/` folder, e.g. `runtime/data/101` for participant 1.

### **Cache Management**
The system uses a cache folder (`./runtime/cache/`) to store intermediate results and speed up subsequent runs. **You can safely delete the entire cache folder to save disk space** - all cached data will be automatically recalculated when needed.

### **Data Requirements for Simulation Analysis**
To use the **Simulation Data** page for plotting raw data or training risk models, you need:
- **Raw simulation data**: Individual participant trial files in `./runtime/data/[participant]/trackers/` folders
- **Risk model training**: Requires simulation data from multiple participants/trials to train predictive models
- **Hazard prediction**: Needs both trained models and hazard sample data for risk analysis

Without this raw data, the simulation page will show "No simulation data available" messages.
- [GitHub Repo](https://github.com/Avdbergnmf/R-Analysis-BallOnPlate-Experiment.git)
- [GitLab Repo](https://gitlab.tudelft.nl/mln-lab-public/r-analysis-alex-van-den-berg-2025-ball-on-plate-experiment)

## How to use

**Additional information:**
- [Specific readme for the runtime code](./runtime), ([GitLab specific link](https://gitlab.tudelft.nl/mln-lab-public/r-analysis-alex-van-den-berg-2025-ball-on-plate-experiment/-/tree/main/runtime?ref_type=heads))
- [Supplementary Materials Note](./SUPPLEMENTARY_MATERIALS_INFO.md): This repository is part of the supplementary materials for the associated paper. The `SUPPLEMENTARY_MATERIALS_INFO.md` note provides addition details about this.

### License and usage
This project (including the dataset and code) is licensed under the Creative Commons Attribution 4.0 International.

If used, please adequately cite this work:
```
@inproceedings{WIP,
    author = {Alex van den Berg},
    title = {VizPert},
    year = {2024},
    publisher = {WIP},
    address = {Delft, Zuid-Holland, The Netherlands},
    url = {WIP},
    doi = {WIP}
}
```
> Note: this will be updated before the final submission.

### Use with Docker
1. build and start the docker container defined in docker-compose.yml: `docker-compose up --build -d`
2. Navigate to http://localhost:8787, log in with name `rstudio`, and password defined in the `.rstudio.env` file (by default: `alex123`).
3. Within the R instance, navigate to `/home/rstudio/workspace/`, and open the `index.Rmd` file.
3. Click `Run Document` and allow the popup window to appear.
> Notes:
> 1. Initial calculation of the datasets may take some time (~5 minutes). After the first run, the results should be saved to tables in `runtime/results/` and startup should be significantly faster. If you need to recalculate these tabled for any reason, just delete them and they will be recreated with the current contents of the data folder.\
> 2. If you notice that the output of the *Render* window in R Studio get stuck, and the interface does not load, stop the code, *Clear Prerendered Output* and try again.
4. Browse the tabs and interact with the data. See [runtime readme](./runtime) for more info.

### Without Docker
You can also just use the code with your local R-Studio installation:
1. Within the R instance, navigate to `./runtime/`, and open the `index.Rmd` file.
2. Click `Run Document`. If it asks you to install any packages, click yes. You can find the list of packages used in `./runtime/source/setup.R`.
3. Browse the tabs and interact with the data

## Contents
- `.rstudio.env` - Contains the password for the rocker container.
- `docker-compose.yml` - The description of how to create the docker container. This also sets the port and shared volume.
- `Dockerfile.rocker` - The dockerfile used to build the docker container - based on [rocker/rstudio:4.4.0](https://rocker-project.org).

- `runtime/` - contains all the code that is ran in the docker container. Note: also contains a [`README.md`](./runtime) explaining its contents.