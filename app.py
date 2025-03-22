import seaborn as sns
from faicons import icon_svg

# Import data from shared.py
from shared import app_dir, df

from shiny import App, reactive, render, ui
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import io
import base64



data = pd.read_csv("artificial_data.csv")

subjects = list(data.columns)[1:7]
reports_by_subjects = ["Reports by " + subject for subject in subjects]

# Define the UI
app_ui = ui.page_fluid(
    ui.h1("FDA Adverse Events Reporting System (FAERS) Public Dashboard"),
    ui.div(
        ui.div(
            ui.input_select("subject_report", "", reports_by_subjects),
            style="margin-bottom: 20px;"
        ),
        ui.div(
            ui.row(
                ui.column(6, ui.output_table("table")),
                #ui.column(6, ui.output_plot("plot"))
            ),
            style="margin-top: 10px;"
        )
    )
)

# Define the server
def server(input, output, session):

    @reactive.Calc
    def get_aggregated_data():
        subject_report = input.subject_report()
        subject = subject_report[11:]
        aggregated = data.groupby(['Year', subject]).size().reset_index(name='Reports')
        return aggregated

    @output
    @render.table
    def table():
        subject_report = input.subject_report()
        subject = subject_report[11:]
        long_agg_data = get_aggregated_data()
        wide_agg_data = long_agg_data.pivot(index='Year', columns=subject).reset_index()
        return wide_agg_data

    
    @output
    @render.plot
    def plot():
        subject_report = input.subject_report()
        subject = subject_report[11:]
        df = data[subject]
        
        fig, ax = plt.subplots(figsize=(10, 6))
        
        # Create a scatter plot of scores vs attendance
        ax.scatter(df['Attendance'], df['Score'], alpha=0.7, s=100, 
                  c='darkblue', edgecolors='black')
        
        # Label each point with student name
        for i, txt in enumerate(df['Student']):
            ax.annotate(txt, (df['Attendance'][i], df['Score'][i]), 
                       fontsize=9, ha='right')
        
        ax.set_xlabel('Attendance (%)')
        ax.set_ylabel('Score')
        ax.set_title(f'{subject} - Attendance vs. Score')
        ax.grid(True, alpha=0.3)
        
        # Set axis limits
        ax.set_xlim(65, 105)
        ax.set_ylim(55, 105)
        
        plt.tight_layout()
        return fig

# Create and run the app
app = App(app_ui, server)