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

    year_totals = data.groupby('Year')['Reports'].sum().reset_index()

    plot = (
        ggplot() +
        geom_bar(data, aes(x='Year', y='Reports', fill=subject),
                 stat='identity', position='stack') +
        scale_x_continuous(breaks=True) +
        labs(
            x='Year',
            y='Reports',
            fill=subject
        ) +
        #Add labels on top of each stacked bar
        geom_text(
            aes(x='Year', y='Reports', label='Reports'),
            data=year_totals,
            angle=90,   # Vertical text
            va='bottom',  # Align text to bottom
            nudge_y=1,    # Move text up a bit
            size=8,       # Text size
            format_string='{}'
        ) +   
        theme_minimal() +
        theme(
            plot_title=element_text(ha='center'),
            legend_position='right'
        )
    )
    return plot

