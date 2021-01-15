# A bayesian approach to time series forecasting

## The problem

...

## Installation

...

## Project structure

Project is organized as follow
* **src**: ...
- **libs**: external files ...
- **modelFiles**: .stan and .bug files ...
* **docs**: contains jupyter notebooks presenting our results in a convinient way for the reader, as well as mathematical proofs and models.
* **data**: .csv used as data ...

## Notebooks 
In the **docs** folder you can find a set of files summarizing all our analysis without the pain of making long simulation runs. You can see them directly on Github or for a better rendering open them using [Jupyter](https://jupyter.org/).

### Data collection and exploration

* [DataCollection](https://github.com/AlePalu/AirQualityForecasting/blob/master/notebooks/1-DataCollection.ipynb): We interface to Wiseair servers to collect data for the interested period. Remove useless features for statistical analysis and make an hourly aggregation to have data sampled with an uniform frequency. Finally integration of weather conditions using ARPA data is done. At the end the dataset in CSV format is produced (Written in python so to be able to interface with data servers).

* DataExploration: ...

### Classical time series models

We start by trying to forecast our data using well known and simple models. We do not expect to obtain great results, since all models presented in this section work under the stationary assumption and data to forecast are not stationary. Take the content of this section as a warmup.

* AR: ...
* ARX: ...
* SARX: ...
* Model selection and conclusions: ...

### Dynamic Linear Models

...

## Final proposed model and conclusions

...

# Licence
