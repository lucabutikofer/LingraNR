# LingraNR
R Interface to [LINGRA-N Tool](https://widgets.figshare.com/articles/11359613/embed?show_title=1), grassland productivity model.

***
### Installation

Install with:
```R
devtools::install_github('lucabutikofer/LingraNR', build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = T)
```

Variable `build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = T` insures that vignettes are built both in Mac and Windows environments.

***
### Documentation

A tutorial is available as vingette. To access the vignette type `browseVignettes('WofostR')`