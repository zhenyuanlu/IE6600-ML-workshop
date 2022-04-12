---
course: IE6600-workshop
version: 0.3
created date: 02/01/2021
updated date: 08/10/2021

---



# Workshop Topics

The purpose of the workshop is to make students familiar with how to implement machine learning models by using R.

- Introduction to neural network and deep learning
- Principle Components Analysis (PCA) for Visualization
- k nearest neighbours (KNN)


## Directory structure

```text
ML-workshop-IE6600/
    code/
        data/
          kNN_wbcd.csv
          NN_data.csv
        kNN.Rmd
        NN_keras.Rmd
        VizPCA.Rmd
    slides/
        kNN_workshop.pdf
        NN_workshop.pdf
        VizPCA_workshop.pdf
    readme.md
```

## Environment

- R language
- R studio


## Installation of Keras and Tensorflow in R

The Keras R interface uses the TensorFlow backend engine by default.

```r
install.packages("keras")
```

or install the development version with:

```r
devtools::install_github("rstudio/keras")
```

then

```r
library(keras)
install_keras()
```

This work is licensed under a [Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/)

![](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)

The coding examples in class are released under the MIT license.