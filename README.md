# LingraNR
R Interface to [LINGRA-N Tool](https://widgets.figshare.com/articles/11359613/embed?show_title=1), grassland productivity model and functions to implement the grassland productivity metamodel of Qi <i>at al. (2017, 2018)</i><sup>[1](#foot1)</sup><sup>[2](#foot1)</sup>.

***
### Installation

Install with:
```R
devtools::install_github('lucabutikofer/LingraNR')
```

***
### Documentation

LINGRA-N  is a simple  grass growth model which can calculate grass growth and yields under potential (i.e. optimal), water limited (i.e. rainfed) and nitrogen limited growing conditions. LINGRA-N is a generic model which can be used for different grass types growing under a large range of soil and weather conditions and with different mowing regimes. 
The background document and detailed information about LINGRA-N and its FORTRAN code are provided by Wolf (2012)<sup>[3](footn3)</sup>. Under the NERC Research Translation: Grassland Management project (NE/R017387/1), supported by the Sustainable Agriculture Research and Innovation Club (SARIC), the original LINGRA-N model was further developed and released as a Microsoft Excel workbook (the LINGRA-N Tool) for use as an adaptive learning tool by students, grassland managers and advisors. The primary improvement of LINGRA-N Tool compared to its predecessor is a more realistic nitrogen balance routine.

This package is an R wrapper for the LINGRA-N Tool, allowing the model to be ran entirely without leaving the R environment. LINGRA-N computations are carried out internally to the Excel Workbook via the xlsx R package (Java based), while LingraNR manages inputs and outputs, and provides functions to display the model's results.

Additionally, functions to run the grassland productivity metamodel of Qi <i>at al. (2017, 2018)</i><sup>[1](#foot1)</sup><sup>[2](#foot1)</sup> for the UK are also provided.

#### Bibliography
<a name="footn1">1</a>: Qi, A., Murray, P. J. & Richter, G. M. Modelling productivity and resource use efficiency for grassland ecosystems in the UK. European Journal of Agronomy 89, 148–158 (2017).
<a name="footn2>2</a>: Qi, A., Holland, R. A., Taylor, G. & Richter, G. M. Grassland futures in Great Britain – Productivity assessment and scenarios for land use change opportunities. Science of The Total Environment 634, 1108–1118 (2018).
<a name="footn3>3</a>: Wolf J. LINGRA-N: Simple generic model for simulation of grass growth under potential,
water limited and N limited conditions. (2012) https://models.pps.wur.nl/lingra-n-grassland-model-potential-water-limited-and-n-limited-conditions-fortran
