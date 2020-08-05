# LingraNR

R Interface to [LINGRA-N Tool](https://widgets.figshare.com/articles/11359613/embed?show_title=1), grassland productivity model and functions to implement the grassland productivity metamodel of Qi <i>at al.</i> (2017 & 2018)<sup>[1](#foot1),[2](#foot1)</sup>.


***
### Installation
```R
devtools::install_github('lucabutikofer/LingraNR')
```

***
### Documentation

LINGRA-N  is a  grass growth model which can calculate grass growth and yields under potential, water-limited and nitrogen-limited growing conditions. LINGRA-N is a generic model which can be used for different grass types growing under a large range of soil and weather conditions and with different mowing regimes. The background document and detailed information about LINGRA-N and its FORTRAN code are provided by Wolf (2012)<sup>[3](#footn3)</sup>.

Under the NERC Research Translation: Grassland Management project (NE/R017387/1), supported by the Sustainable Agriculture Research and Innovation Club (SARIC), the original LINGRA-N model was further developed and released as a Microsoft Excel workbook (the [LINGRA-N Tool](https://widgets.figshare.com/articles/11359613/embed?show_title=1)) for use as an adaptive learning tool by students, grassland managers and advisors. A key difference between LINGRA-N Tool and its predecessor is a vastly improved nitrogen balance routine, which motivated the creation of this package.

Package LingraNR is an R wrapper for the LINGRA-N Tool, allowing the model to be ran entirely without leaving the R environment. LINGRA-N computations are carried out internally to the Excel Workbook via the xlsx R package (Java based), while LingraNR manages inputs and outputs, and provides functions to display the model's results.

Additionally, functions to run the grassland productivity metamodel of Qi <i>at al.</i> (2017 & 2018)<sup>[1](#foot1),[2](#foot1)</sup> for the UK are also provided.

#### Running LINGRA-N

LINGRA-N simulations are carried out by function Lingranr(). This requires information on weather, soil, and management.

##### Weather
Weather data is to be provided in daily time steps and must contain information on:

  * Total solar radiation [MJ/m2*day]
  * Minimum daily tempterature [°C]
  * Maximum daily tempterature [°C]
  * Vapour pressure [kPa]
  * Mean daily wind speed [m/s]
  * Total daily rainfall [mm]

Additionally, date and location information are added to the weather input table. See `?Lingranr` for details and `weatherExmpl` for and example.

##### Soil
Soil information is to be provided for each location and must contain:

  * Soil depth [mm]
  * Moisture content at field capacity [mm3/mm3]
  * Moisture content at saturation [mm3/mm3]
  * Moisture content at wilting point [mm3/mm3]

See `?Lingranr` for details and `soilExmpl` for and example.

##### Management
Management information is to be provided as two separate objects specifying harvest dates and fetilisation dates with fertilisation amounts in equivalent mineral nitrogen.
See `?Lingranr` for details and `harvestExmpl; fertilisExmpl` for examples.

#### Example
To run LINGRA-N with example data and plot the model outputs:

```R
# Run model:
lo <- Lingranr(w = weatherExmpl[1:365,],
               s = soilExmpl,
               h = harvestExmpl,
               f = fertilisExmpl,
               lat = 50,
               alt = 50,
               return = 'all')

# Plot outputs:
op <- par(mfrow = c(1,3))
plotw(weatherExmpl[1:365,])
plotdm(lo)
plotn(lo)
```
![alt text][lingraOutpu1]


#### Running Qi <i>et al.</i>'s metamodel

The metamodel is run with three independent functions for permanent (`pg()`), temporary (`tg()`) and semi-natural (rough grazing, `rg()`) grassland. See `?metamodel` for details. The metamodel uses seasonal weather data and only requires additional information on the soils' available water content.

Additionally, a yield modifyer function (`Nchange()`) is available to correct the metamodel's output whenever precise information on nitrogen fertilisation is available. For details about the metamodelmodel see Qi <i>at al.</i> (2017 & 2018)<sup>[1](#foot1),[2](#foot1)</sup>.


#### Bibliography

<a name="footn1">1</a>: Qi, A., Murray, P. J. & Richter, G. M. Modelling productivity and resource use efficiency for grassland ecosystems in the UK. European Journal of Agronomy 89, 148–158 (2017).

<a name="footn2">2</a>: Qi, A., Holland, R. A., Taylor, G. & Richter, G. M. Grassland futures in Great Britain – Productivity assessment and scenarios for land use change opportunities. Science of The Total Environment 634, 1108–1118 (2018).

<a name="footn3">3</a>: Wolf J. LINGRA-N: Simple generic model for simulation of grass growth under potential,
water limited and N limited conditions. (2012) https://models.pps.wur.nl/lingra-n-grassland-model-potential-water-limited-and-n-limited-conditions-fortran .

[lingraOutpu1]: https://github.com/lucabutikofer/LingraNR/blob/master/man/figures/LingranrOutput.png "Output of Lingranr function"
