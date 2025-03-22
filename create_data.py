import numpy as np
import pandas as pd
import random
from datetime import datetime, timedelta

# Set random seed for reproducibility
np.random.seed(42)
random.seed(42)

# Define the number of samples
n_samples = 1000

# Function to generate artificial data
def generate_artificial_data(n_samples):
    # 1. Continuous numeric columns
    #year = np.random.normal(2020, 4, n_samples).round(1)  # Year, de 2009 à 2024
    
    # 2. Discrete numeric columns
    # Create array of years with specific distribution (more recent years slightly more common)
    years = list(range(2009, 2025))  # Years from 2009 to 2024 inclusive
    # Create weights to favor more recent years slightly
    weights = np.linspace(0.8, 1.2, len(years))
    weights = weights / np.sum(weights)  # Normalize to sum to 1
    year = np.random.choice(years, n_samples, p=weights)
    
    # 3. Categorical columns (nominal)
    sex = np.random.choice(['Male', 'Female', 'Not specified'], n_samples, p=[0.48, 0.48, 0.04])
    
    reporter_region = np.random.choice(['Domestic', 'Foreign', 'Not Specified'], n_samples, p=[0.7, 0.29, 0.01])
    reporter = np.random.choice(['Consumer', 'Healthcare Professionnal', 'Not Specified', 'Other'], n_samples, p=[0.45, 0.53, 0.01, 0.01])
    
    report_seriousness = np.random.choice(['Serious', 'Death', 'Non-Serious'], n_samples, p=[0.6, 0.08, 0.32])
    type = np.random.choice(['BSR', 'Direct', 'Expedited', 'Non-Expedited'], n_samples, p=[0.01, 0.04, 0.6, 0.35])

    # 4. Ordinal categorical columns
    possible_age_groups = ['0-1 Month', '2 Months-2 Years', '3-11 Years', '12-17 Years', '18-64 Years', '65-85 Years', 'More than 85 Years', 'Not Specified']
    age_group = np.random.choice(possible_age_groups, n_samples, p=[0.01, 0.01, 0.03, 0.04, 0.4, 0.25, 0.04, 0.22])
    
    # 5. Binary columns

    # 6. Date/time data

    # Create DataFrame
    data = pd.DataFrame({
        'Year': year,
        'Sex': sex,
        'Reporter Region': reporter_region,
        'Reporter': reporter, 
        'Report Seriousness': report_seriousness,
        'Type': type,
        'Age Group': age_group,
    })
    
    return data

# Generate the data
artificial_data = generate_artificial_data(n_samples)

# Display the first few rows and information about the dataset
print(artificial_data.head())
print("\nDataset Information:")
print(artificial_data.info())
print("\nSummary Statistics:")
print(artificial_data.describe(include='all'))

# Save to CSV (optional)
artificial_data.to_csv('artificial_data.csv', index=False)