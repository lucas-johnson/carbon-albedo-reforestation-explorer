# CARE - the Carbon and Albedo Reforestation Explorer

#### v1.0.0

Our application is built for managers interested in planting optimal forest-type groups for climate change mitigation outcomes approaching the year 2050. We assist in one of the most fundamental steps in any reforesation project: deciding which forest type or species mix to plant. We used a new carbon storage and albedo offset dataset in concert with FIA species distribution data to answer these questions. This application ranks available FIA-classified forest-type groups on the basis of joint C-albedo (non-soil carbon storage less a carbon-equivalent albedo offset) in a local area of interest. We intend for managers to combine this information with their unique understanding of project- or site specific constraints and goals.

## Methods

### The Forest Inventory and Analysis Database

The United States (US) Forest Inventory and Analysis program (FIA) maintains a nationwide network of permanent inventory plots arranged in a quasi-systematic hexagonal grid with a sample density of one plot for every ~2,400 ha (Bechtold and Patterson, 2005). The inventory records land uses, and within forested plots collects measurements of live and dead trees, litter, downed wood, and soil, as well as observations of stand-age and classifications into 30 forest-type groups. These measurements are later translated into estimates of biomass and carbon for five distinct pools (aboveground and belowground biomass, deadwood, litter, soil), are made available in a public relational database (Gray et al., 2012), and ultimately provide the basis for national forest carbon accounting (Woodall et al., 2015; Domke et al., 2023).

### Joint C-albedo estimates by forest-type and age groups

Healey et al. (In review) applied standard FIA estimation methods to develop tables of non-soil carbon storage for the Inventory’s 30 distinct forest-type groups, indexed by 10-year age bins. Although FIA has since transitioned to using a new system of allometric equations to convert granular measurements (e.g. di- ameter, height) to tree-level estimates of biomass and carbon (Westfall et al., 2024), Healey et al. (In review) derived estimates based on the component-ratio-method (Woodall et al., 2011) which was the national stan- dard at that time. Corresponding carbon-equivalent albedo impacts were identified for each type-by-age group using FIA’s condition-level expansion factors for weights. Albedo impact for every FIA plot was determined by the annualized albedo difference, assessed through 4 years of Landsat albedo measurements (Erb et al., 2022), between the plot and its ten nearest non-forest plots. Albedo differences were converted to a radiative forcing through the use of a radiative kernel (Bright and O’Halloran, 2019), and annualized radiative forcings were converted to carbon-equivalent units (Bright et al., 2016). Refer to Healey et al. (In review) for methodological details and Healey and Yang (2023) for resulting forest-type group-specific carbon stocks and albedo impacts.

### Ranking forest-type groups within 64,000 ha hexagons

We usd a CONUS-wide hexagonal tessellation of 64,000 hectare hexagons to explore spatial patterns of albedo impacts on planting decisions. These hexagons, which are sometimes used for fine- resolution analysis of the FIA database (Menlove and Healey, 2020), each contain ~27 FIA plots. Within each hexagon we identified the set of forest-type groups present in the most recent panel of inventories from each state (Bechtold and Patterson, 2005), and ranked each forest-type group at the 20-30 year-old bin (25-year) on the basis of both joint C-albedo benefits (non-soil carbon stock less a carbon-equivalent albedo offset) and non-soil carbon storage. Additionally, we present 100-year chronosequences (10-year bins) for each attribute (carbon, albedo offsets, joint C-albedo) across the same set of forest-type groups to offer insight into the temporal consistency of joint C-albedo benefits.
See Johnson et al., (in review) for a full description of the methods used to generate the data displayed in this application.

## Limitations
The estimates of joint C-albedo benefits that support our application were made under climatic and atmospheric conditions that are unlikely to remain constant in the next 25 years. Specifically, we relied on chronosequences of non-soil carbon stocks estimated from historical inventory data measured as recently as 2021, thus reflecting forest establishment and growth under the climate of the ~30 preceding years. Similarly, we used carbon-equivalent albedo offset estimates made with Landsat 8 imagery collected between 2016 and 2019, thus reflecting the atmospheric conditions and earth surface properties from this particular time frame (Healey et al., In review). Given the dynamic, and likely interrelated, forecasted shifts in global climate, atmospheric composition, and surface properties, we emphasize the uncertainty surrounding the use of current conditions as a proxy for future climate impacts.

## Support
If you encounter any problems, or have any recommendations for how to improve the app's functionality, please submit a github issue or reach out to the app developers using the contact information provided below.

## Citation
Johnson, Yang, Erb, Bright, Domke, Frescino, Schaaf, Healey, (in review), Integrating albedo offsets in reforestation decisions for climate change mitigation outcomes in 2050: a case study in the USA.

Johnson, L., Healey, S., & Yang, Z. (2024). Carbon and Albedo Reforestation Explorer (1.0.0). Zenodo. https://doi.org/10.5281/zenodo.14563711

## References

Bright, Bogren, Bernier, Astrup, (2016). Carbon-equivalent metrics for albedo changes in land management contexts: Relevance of the time dimension. Ecol. Appl. 26, 1868–1880

Bright, R. M., & O'Halloran, T. L. (2019). Developing a monthly radiative kernel for surface albedo change from satellite climatologies of Earth's shortwave radiation budget: CACK v1. 0. Geoscientific Model Development, 12(9), 3975-3990.

Domke, Walters, Nowak, Greenfield, Smith, Nichols, Ogle, Coulston, Wirth (2022). Greenhouse Gas Emissions and Removals From Forest Land, Woodlands, Urban Trees, and Harvested Wood Products in the United States, 1990–2020. (US Dept. Ag. For. Service, Madison, WI; https://doi.org/10.2737/FS-RU-382 ).
Erb, Li, Sun, Paynter, Wang, & Schaaf, (2022). Evaluation of the Landsat-8 Albedo Product across the Circumpolar Domain. Remote Sensing, 14(21), 5320.

Healey, Yang, Erb, Bright, Domke, Frescino, Schaaf, (in review) New satellite observations expose albedo dynamics offsetting half of carbon storage benefits in US forests.

Healey, S., & Yang, Z. (2023). Carbon storage and carbon-equivalent albedo impact for US forests, by age and forest type [Data set]. Zenodo. https://doi.org/10.5281/zenodo.8320433

## Contact information

- Lucas Johnson - johnsl27@oregonstate.edu
- Zhiqiang Yang - zhiqiang.yang@usda.gov
- Sean Healey - sean.healey@usda.gov
