# FDA Adverse Event Reporting System (FAERS) Dashboard Recreation
[<img width="2240" alt="Screenshot 2025-03-22 at 18 35 30" src="https://github.com/user-attachments/assets/fd76f043-651b-4bc9-a3d8-0bc5a65e7404" />](https://dgalgom.shinyapps.io/FAERS_simulation_test/)

[<img width="2240" alt="Screenshot 2025-03-22 at 18 35 55" src="https://github.com/user-attachments/assets/4ecb9503-a07d-4680-bb89-9d0e0e84b3e2" />](https://dgalgom.shinyapps.io/FAERS_simulation_test/)

Welcome to my repository! This project was developed as part of the Appsilon Tiny Shiny Hackathon, a four-hour online challenge where developers showcase their creativity and technical skills by building applications that combine Shiny and AI. Here, I present my solution: a Shiny dashboard that recreates the FDA Adverse Event Reporting System (FAERS) Public Dashboard, with added enhancements for improved functionality and user experience.

## 🚀 Overview
The goal of this project was to recreate the FAERS Public Dashboard using R Shiny, while incorporating advanced features such as AI-powered predictions to enhance the user experience. The dashboard allows users to:

Filter reports by specific characteristics (e.g., reporter type, event type, date range).

Visualize the data using interactive charts and tabular displays.

Explore predictive insights by leveraging a machine learning model to estimate the probability of adverse events based on user-modulated characteristics.

This project demonstrates my ability to combine Shiny development with AI integration to solve real-world challenges in a time-constrained environment.

## 🛠️ Tools and Technologies Used
To achieve this, I utilized the following tools and technologies:

 + R Shiny: For building the interactive dashboard.

 + AI Models:

  + Shiny Assistant: For streamlining Shiny app development.

  + ChatGPT (GPT-4): For generating code snippets, debugging, and enhancing the app's logic.

  + Claude 3.7: For brainstorming and refining the app's functionality.

  + DeepSeek R-1: For optimizing code efficiency and performance.

 + R Packages:

  + `shiny`: Core framework for building the app.

  + `DT`: For interactive data tables.

  + `ggplot2`: For creating visualizations.

  + `dplyr`: For data manipulation.

  + `caret` or `tidymodels`: For building the predictive model.

Mocked Data: Simulated datasets to replicate the structure and functionality of the FAERS dashboard.

## 🎯 Key Features
1. Interactive Filters
Users can filter the data by:

Reporter Type (e.g., healthcare professional, consumer).

Event Type (e.g., adverse event, product issue).

Date Range (e.g., specific time periods).

2. Data Visualization
Bar Charts: Show the distribution of adverse events over time.

Pie Charts: Display the proportion of events by category.

Line Charts: Visualize trends in adverse event reporting.

3. Tabular Data Display
Interactive tables allow users to explore the filtered data in detail.

4. AI-Powered Predictive Insights
A machine learning model predicts the probability of adverse events based on user-selected characteristics (e.g., age, gender, drug type).

Users can modulate input parameters to see how the predicted probabilities change.

## 🚀 Enhancements Proposed
While recreating the FAERS dashboard, I identified an opportunity to enhance its functionality by integrating predictive analytics. Here's how:

Predictive Model:

Trained on historical adverse event data to estimate the likelihood of future events.

Allows users to input specific characteristics (e.g., patient demographics, drug details) and receive real-time predictions.

User-Friendly Interface:

Simplified input fields for modulating prediction parameters.

Clear visualizations of predicted probabilities.

Dynamic Updates:

Predictions and visualizations update in real-time as users adjust filters or input parameters.

## 📊 Dashboard Structure

├── Data Processing
│   ├── FAERS Data Import
│   ├── Preprocessing
│   └── Feature Engineering
├── User Interface
│   ├── Filtering Panel
│   ├── Summary Statistics
│   ├── Visualization Hub
│   └── Prediction Interface
└── Predictive Module
    ├── Model Training
    ├── Interactive Prediction
    └── Risk Visualization


## 🔍 Hackathon Approach
With only four hours to complete the challenge, I employed a strategic approach:

Rapid Prototyping: Using AI tools to quickly scaffold the application
Feature Prioritization: Focusing on core FAERS functionality first
Incremental Enhancement: Adding predictive capabilities once core features were stable
Continuous Testing: Ensuring functionality throughout development


## Screenshots

The brilliant Shiny Assistant
<img width="2240" alt="Screenshot 2025-03-22 at 17 46 59" src="https://github.com/user-attachments/assets/778add24-fec8-47ce-8bcb-b3d885468e05" />

Claude 3.7 for optimizing processes
<img width="2240" alt="Screenshot 2025-03-22 at 17 47 44" src="https://github.com/user-attachments/assets/79d8cbb3-27c1-41ee-95d6-a965c2b0e545" />

ChatGPT to insert FDA US emblem
<img width="1923" alt="Screenshot 2025-03-22 at 17 48 17" src="https://github.com/user-attachments/assets/5ca998b1-30ed-4bec-b739-95a80010ddf8" />


