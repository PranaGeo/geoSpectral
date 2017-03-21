# geoSpectral

geoSpectral is an R package providing a new data type for R that stores spectral, temporal and spatial attributes of measurement data as well as methods for accessing and manipulating the spectral (and non-spectral) data. Once spectral data is imported into geoSpectral, the statistical and data processing power of R is available for various kinds of scientific analyses.

It provides the S4 classes: **_Spectra_** (stores spatial/temporal/spectral aspects of data), **_SpcHeader_** (stores metadata in an R list object) and **_SpcList_** (makes a collection of Spectra objects in an R list) as well as basic data access and manipulation methods for importing, acessing and subsetting, converting into R objects, analyzing, plotting and exporting scientific earth observation data.

## License
The package is issued with a [GPLv3](http://www.gnu.org/copyleft/gpl.html) license. Please consult the license documentation if you would like to use **geoSpectral** in your Open Source software projects.

## Requirements
**geoSpectral** depends on several 3rd party R packages. You need to install them before you can install **geoSpectral** :

```
install.packages(c("dplyr","spacetime","xts","maps","rgdal","leaflet","rbokeh","plotly","sp"))
```

## Installation
Luckily, install_github() function from package *devtools* will download dependencies for you if you call it with the argument dependencies=TRUE. First install the *devtools* package using *install.packages()* and then :
```
require(devtools)
install_github("PranaGeo/geoSpectral")
```
**Alternative installation method**
If you somehow already installed the dependencies, it is also possible to the manually downloaded latest version of **geoSpectral** in a zipfile and install it from within Rstudio. Click the green button "*Clone or download* on the top-right of the Github project page https://github.com/PranaGeo/geoSpectral to download the zipfile. If you have a command-line tool like wget or alike, you can also use the following link :

```
wget https://github.com/PranaGeo/geoSpectral/archive/master.zip
```

After unzipping the file, open the Rstudio .rproj file and install the package using CTRL+SHIFT+B

## Usage
After installing the package, you can try from the R prompt : ```?geoSpectral``` to consult the brief documentation of the package or ```?Spectra``` to  see the help of the constructor function the main class : *Spectra()*.

You can go through the the [tutorial](https://pranageo.com/geospectral/geospectral-tutorial/) to learn how to use the package. 

## Contributions
Your comments,suggestions and contributions are very welcome. Please feel free to open issues [here](https://github.com/PranaGeo/geoSpectral/issues).

### Help the development
If you would like to contribute to the development, get a GitHub account, fork the *dev* branch of this project to your GitHub account, clone it to your local machine, work on it, commit your changes, push your changes to your GitHub fork and send us a pull request and we will discuss. For more information, visit the [fork & pull development model page.](https://help.github.com/articles/using-pull-requests/#fork--pull)
