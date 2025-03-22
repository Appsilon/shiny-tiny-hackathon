from shiny import App, reactive, render, ui
import pandas as pd

from plots import stacked_bar_plot

data = pd.read_csv("artificial_data.csv")
min_year = data['Year'].min()
max_year = data['Year'].max()

subjects = list(data.columns)[1:7]
reports_by_subjects = ["Reports by " + subject for subject in subjects]

# Define the UI
app_ui = ui.page_fillable(
    ui.h1("FDA Adverse Events Reporting System (FAERS) Public Dashboard"),
    ui.layout_column_wrap(
        ui.card(ui.input_select("subject_report", "", reports_by_subjects)),
        ui.card(ui.input_slider("year_range", "Select Year Range:", 
                   min=min_year, max=max_year, value=[min_year, max_year],
                   step=1, ticks=True, sep=""))
    ),
    ui.layout_columns(
        ui.card(
            ui.card_header(ui.output_text("title")),
            #ui.output_table("table"),
            ui.output_data_frame("number_data", ),
            full_screen=True,
        ),
        ui.card(
            ui.card_header(ui.output_text("title_plot")),
            ui.output_plot("plot"),
            full_screen=True,
        ),
    ),
    title="FAERS Report Dashboard"
)

# Define the server
def server(input, output, session):

    subject_r = reactive.value("Sex")
    @reactive.effect
    @reactive.event(input.subject_report)
    def _():
        subject_report = input.subject_report()
        subject = subject_report[11:]
        subject_r.set(subject)

    @reactive.Calc
    def filtered_data():
        year_min, year_max = input.year_range()
        return data[(data['Year'] >= year_min) & (data['Year'] <= year_max)]

    @reactive.Calc
    def get_aggregated_data():
        df = filtered_data()
        aggregated = df.groupby(['Year', subject_r.get()]).size().reset_index(name='Reports')
        return aggregated

    @output
    @render.text
    def title():
        year_min, year_max = input.year_range()
        return f'Reports by Year and {subject_r.get()} ({year_min}-{year_max})'
    
    @output
    @render.data_frame
    def number_data():
        long_agg_data = get_aggregated_data()
        wide_agg_data = long_agg_data.pivot(index='Year', columns=subject_r.get(), values='Reports').reset_index()
        return wide_agg_data
    
    @output
    @render.text
    def title_plot():
        return 'See the Number of Reports by Year and {}'.format(subject_r.get())

    @output
    @render.plot
    def plot():
        return stacked_bar_plot(get_aggregated_data(), subject_r.get())

# Create and run the app
app = App(app_ui, server)