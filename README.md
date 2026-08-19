# Telecommunications Customer Churn Analysis

## Overview
This repository contains an end-to-end statistical analysis and predictive modeling project focused on telecommunications customer retention. The goal of this project is to identify the key drivers of customer churn and benchmark various parametric and non-parametric machine learning models to predict customer behavior and monthly charges.

## Methodology
The analysis was performed on a deterministically sampled dataset of 997 customers using an explicit 70/30 train-test split. The project is divided into two primary modeling tasks:
*   **Classification (Predicting Churn):** Benchmarking Logistic Regression against Linear Discriminant Analysis (LDA).
*   **Regression (Predicting Monthly Charges):** Benchmarking Multiple Linear Regression against K-Nearest-Neighbors (KNN) Regression across multiple values of *k*.

## Key Findings
*   **Churn Drivers:** Contract duration is the most powerful retention lever; customers on two-year contracts have an 83.6% lower odds of churning compared to month-to-month users.
*   **Classification Performance:** On the unseen test data, the LDA model outperformed Logistic Regression, achieving an 80.33% overall accuracy and a superior false-negative rate, making it more effective at actually catching potential churners.
*   **Regression Performance:** Multiple Linear Regression consistently outperformed KNN Regression (achieving a Test RMSE of $15.57), demonstrating that the additive, tiered pricing structure of the telecom plans is best captured parametrically rather than through local averaging.

## Tech Stack
*   **Language:** R 
*   **Libraries:** `tidyverse`, `MASS` (for LDA), `class` / `FNN` (for KNN), `broom`, `car`
*   **Techniques:** Data Cleaning, Exploratory Data Analysis (EDA), Hypothesis Testing, Predictive Modeling, Model Benchmarking
