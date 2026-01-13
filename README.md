# 🎾 Tennis Match Outcome Predictor (R)

## Overview
This project predicts the outcome of ATP tennis matches using historical match data and machine learning models implemented in **R**.  
It demonstrates a complete sports analytics workflow including data processing, feature engineering, model training, evaluation, and interactive visualization.

Data coverage: The model uses ATP match data from 2003–2024, excluding 2020 due to COVID-19 disruptions, to reduce the risk of biased or unreliable predictions.

## Project Features
- Historical ATP match data ingestion and preprocessing
- Feature engineering (ranking differences, surface effects, head-to-head statistics, recent form)
- Supervised machine learning models for match outcome prediction
- Model evaluation using ROC-AUC and classification accuracy
- Interactive visualization interface

## Models & Methods
- Logistic Regression  
- Random Forest (randomForest, ranger)  
- Cross-validation and performance evaluation  

## Tech Stack
- **Language:** R  
- **Libraries:** readr, dplyr, purrr, stringr, tidyverse, randomForest, pROC, ranger  
- **Tools:** RStudio, Git, GitHub  

## Repository Structure
tennisWinnerPredictor/
├── app/ # Shiny app 
├── R/ # Model training and prediction scripts
├── data/ # Dataset location (not included)
├── models/ # Saved trained models
├── README.md
├── requirements.R
└── tennisWinnerPredictor.Rproj

## Data
The raw ATP match datasets are **not included** in this repository due to size constraints.

**Data Source:**
- Jeff Sackmann — ATP Tennis Match Data

### Reproducing the Data
1. Download the ATP datasets from the official source
2. Place the CSV files inside the `data/` directory
3. Run the preprocessing and model scripts in the `R/` folder

## How to Run

### 1. Install dependencies
```r
source("requirements.R")
2. Train or load the model
source("R/tennisWinner.R")
3. Run the application
shiny::runApp("app")

Results
The models achieve competitive predictive performance and highlight how machine learning techniques can be applied to real-world sports analytics problems. 

Future Improvements
Add Elo-based rating features
Incorporate player fatigue and tournament importance
Expand model comparisons (e.g., gradient boosting)
Deploy the application online

Author
Elias Arellano Campos
Computer Science — Data Science & Machine Learning# tennis-winner-predictor
