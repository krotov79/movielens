# movielens
Movie Recommendation System
This repository contains the code and documentation for developing a movie recommendation system using the MovieLens dataset. The project includes data preparation, exploratory data analysis (EDA), model development, and evaluation using various machine learning techniques. The goal is to predict user ratings for movies accurately.

Repository Contents
code.R: The R script containing all the code used for data loading, cleaning, EDA, model development, and evaluation.
movielens-report.pdf: The PDF report documenting the project, including the methodology, results, and conclusions.
movielens-report.Rmd: The R Markdown file used to generate the PDF report. It includes detailed explanations and code chunks.
Project Overview
The MovieLens dataset is a widely-used dataset for research in recommendation systems. It contains millions of ratings for thousands of movies given by a large number of users. The main goal of this project is to develop a recommendation system that predicts movie ratings based on historical user ratings.

Key Steps
Data Loading and Cleaning: Load the MovieLens dataset, handle missing or anomalous values, and split the data into training and validation sets.
Exploratory Data Analysis (EDA): Analyze the distribution of ratings, ratings per movie, ratings per user, and ratings across different genres.
Model Development: Develop and evaluate various models, including:
Baseline Model
Movie Effect Model
User Effect Model
Regularized Movie + User Effect Model
Matrix Factorization
Hybrid Model
Model Evaluation: Use Root Mean Squared Error (RMSE) to evaluate the performance of each model and select the best-performing model.
Results
The Matrix Factorization model achieved the lowest RMSE and was identified as the best-performing model. This model effectively captures latent factors in the user-movie interaction matrix, resulting in more accurate predictions.
