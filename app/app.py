# Joshua Tong
# Shiny Tiny Hackathon

from shiny import App, render, ui
import pandas as pd
import random
from datetime import datetime
import plotly.express as px

# Mock Data Generation
random.seed(0)
years = list(range(2009, 2025))

def generate_mock_data():
    return pd.DataFrame({
        'Year': years,
        'Total Reports': [random.randint(500000, 3000000) for _ in years],
        'Expedited': [random.randint(200000, 1500000) for _ in years],
        'Non Expedited': [random.randint(200000, 1500000) for _ in years],
        'Direct': [random.randint(10000, 200000) for _ in years],
        'BSR': [random.randint(0, 1000) for _ in years],
        'Consumer': [random.randint(100000, 1500000) for _ in years],
        'Healthcare Professional': [random.randint(50000, 1000000) for _ in years],
        'Not Specified': [random.randint(10000, 500000) for _ in years],
        'Other': [random.randint(5000, 300000) for _ in years],
        'Domestic': [random.randint(400000, 2500000) for _ in years],
        'Foreign': [random.randint(50000, 500000) for _ in years],
        'Death': [random.randint(1000, 10000) for _ in years],
        'Non-Serious': [random.randint(100000, 1500000) for _ in years],
        'Serious': [random.randint(50000, 1000000) for _ in years],
        '0-1 Month': [random.randint(1000, 20000) for _ in years],
        '2 mos-2 yrs': [random.randint(5000, 50000) for _ in years],
        '3-11yrs': [random.randint(10000, 100000) for _ in years],
        '12-17yrs': [random.randint(10000, 100000) for _ in years],
        '18-64yrs': [random.randint(200000, 2000000) for _ in years],
        '65-85yrs': [random.randint(50000, 500000) for _ in years],
        'More than 85 yrs': [random.randint(5000, 100000) for _ in years],
        'Female': [random.randint(300000, 1500000) for _ in years],
        'Male': [random.randint(300000, 1500000) for _ in years],
        'Sex Not Specified': [random.randint(10000, 500000) for _ in years]
    })

df = generate_mock_data()


def filter_data(report_filter):
    columns_by_filter = {
        'Reports by Report Type': ['Total Reports', 'Expedited', 'Non Expedited', 'Direct', 'BSR'],
        'Reports by Reporter': ['Total Reports', 'Consumer', 'Healthcare Professional', 'Not Specified', 'Other'],
        'Reports by Reporter Region': ['Total Reports', 'Domestic', 'Foreign'],
        'Reports by Report Seriousness': ['Total Reports', 'Death', 'Non-Serious', 'Serious'],
        'Reports by Age Group': ['Total Reports', '0-1 Month', '2 mos-2 yrs', '3-11yrs', '12-17yrs', '18-64yrs', '65-85yrs', 'More than 85 yrs'],
        'Reports by Sex': ['Total Reports', 'Female', 'Male', 'Sex Not Specified']
    }
    return columns_by_filter.get(report_filter, ['Total Reports'])

def create_plot(filtered_df, columns, report_filter):
    fig = px.line(
        filtered_df, x='Year', y=columns,
        title=f'Reports by {report_filter}',
        labels={'value': 'Report Count', 'Year': 'Year'},
    )
    fig.update_layout(template='plotly_white', height=400)
    return fig.to_html(full_html=False)

app_ui = ui.page_fluid(
    ui.tags.div(
        ui.h1('FDA Adverse Event Reporting System (FAERS) Public Dashboard',
               style="background-color: #007bff; padding: 15px; color: white; text-align: center; width: 100%;")
    ),
    ui.row(
        ui.column(6, ui.tags.div(
            ui.h2('Total Reports: 30,179,725', style="font-weight: bold;"),
            ui.h2('Serious Reports (Excluding Deaths): 16,664,479', style="color: orange; font-weight: bold;"),
            ui.h2('Deaths: 2,722,806', style="color: red; font-weight: bold;"),
            style="padding: 20px; background-color: #f0f0f0;"
        )),
        ui.column(6, ui.tags.div([
            ui.input_select('report_filter', 'Filter by', [
                'Reports by Report Type', 'Reports by Reporter', 'Reports by Reporter Region',
                'Reports by Report Seriousness', 'Reports by Age Group', 'Reports by Sex'
            ], selected='Reports by Report Type'),
            ui.input_radio_buttons('time_filter', 'Filter by Time', ['All Years', 'Last 10 Years'], selected='All Years', inline=True),
            ui.input_select('year', 'Year', ['All Years'] + [str(y) for y in years], selected='All Years')
        ], style="padding: 20px;"))
    ),
    ui.row(
        ui.column(6, ui.output_ui('data_table'), style="padding: 20px;"),
        ui.column(6, ui.output_ui('report_plot'), style="padding: 20px;")
    )
)

def server(input, output, session):
    @output
    @render.ui
    def data_table():
        columns = filter_data(input.report_filter())
        filtered_df = df[['Year'] + columns]
        if input.year() != 'All Years':
            filtered_df = filtered_df[filtered_df['Year'] == int(input.year())]
        if input.time_filter() == 'Last 10 Years':
            filtered_df = filtered_df[filtered_df['Year'] >= 2014]
        
        # Sort by Year in descending order
        filtered_df = filtered_df.sort_values('Year', ascending=False)
        
        table_html = filtered_df.to_html(index=False, classes='table table-bordered table-hover table-sm text-start', border=0)
        return ui.HTML(table_html)

    @output
    @render.ui
    def report_plot():
        columns = filter_data(input.report_filter())
        filtered_df = df[['Year'] + columns]
        if input.year() != 'All Years':
            filtered_df = filtered_df[filtered_df['Year'] == int(input.year())]
        if input.time_filter() == 'Last 10 Years':
            filtered_df = filtered_df[filtered_df['Year'] >= 2014]
        plot_html = create_plot(filtered_df, columns, input.report_filter())
        return ui.HTML(plot_html)

app = App(app_ui, server)
