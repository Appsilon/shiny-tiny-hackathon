# Import required libraries
from plotnine import *
import pandas as pd

# Create the stacked bar plot with plotnine
def stacked_bar_plot(data, subject):
    # Calculate the total reports by category to determine order
    category_totals = data.groupby(subject)['Reports'].sum().reset_index()
    category_totals = category_totals.sort_values('Reports', ascending=True)

    # Set the order of categories (most frequent at the bottom)
    ordered_categories = category_totals[subject].tolist()

    # Convert Category column to a categorical type with the desired order
    data[subject] = pd.Categorical(data[subject], categories=ordered_categories, ordered=True)

    plot = (
        ggplot(data, aes(x='Year', y='Reports', fill=subject)) +
        geom_bar(stat='identity', position='stack') +
        scale_x_continuous(breaks=True) +
        labs(
            title='Number of Reports by Year and {}'.format(subject),
            x='Year',
            y='Reports',
            fill=subject
        ) +
        theme_minimal() +
        theme(
            plot_title=element_text(ha='center'),
            legend_position='right'
        )
    )
    return plot

