import seaborn as sns
from faicons import icon_svg

from shiny import App, reactive, render, ui
import pandas as pd

from plots import stacked_bar_plot

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
                ui.column(6, ui.output_plot("plot"))
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
        return stacked_bar_plot(get_aggregated_data(), subject)

# Create and run the app
app = App(app_ui, server)