# KSHStatinfoScraper

The Hungarian Central Statistics Office (KSH in Hungarian) offers - to my best knowledge - unfortunately no API to access its Statinfo dissemination database. This R package is a preliminary attempt to overcome this limitation by using Statinfo's facility to save and load a query (whose structure can be guessed and therefore query file can be automatically generated).

Installation with
```r
devtools::install_github("tamas-ferenci/KSHStatinfoScraper")
```

The main routine is the `KSHStatinfoScrape` which is quite general facility, and requires the knowledge of the particular query. An example to this is `GetPopulationPyramid` which scrapes the population pyramid of Hungary.
