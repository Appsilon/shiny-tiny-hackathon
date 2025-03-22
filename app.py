"""
App made entirely using Shiny Assistant.
The mockup data was genarated manually and Gemini gave the code to load it here without using the original csv file.

Would have loved to have more time to replicate all the functionalities and not just show useless widgets and links.
"""

from shiny import App, render, ui, reactive
import pandas as pd
import plotly.express as px
from shinywidgets import output_widget, render_widget 
from data_sets import data_report_type, data_reporter, data_wide_report_type, data_wide_reporter

# UI layout
app_ui = ui.page_fluid(
    # Add Font Awesome library to the head of your HTML
    ui.tags.head(
        ui.tags.link(
            rel="stylesheet",
            href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
        ),
        # Add bootswatch cosmo theme
        ui.tags.link(
            rel="stylesheet",
            href="https://cdn.jsdelivr.net/npm/bootswatch@5.2.3/dist/cosmo/bootstrap.min.css"
        ),
        # Custom CSS for primary color navbar
        ui.tags.style(
            """
            .navbar {
                background-color: #0275d8 !important;
            }
            .navbar-nav .nav-link {
                color: white !important;
            }
            .navbar-nav .nav-link.active {
                color: white !important;
                font-weight: bold;
            }
            /* Custom CSS for inverted value boxes */
            .bg-secondary {
                background-color: white !important;
                border: 2px solid #6c757d !important;
                color: #6c757d !important;
            }
            .bg-warning {
                background-color: white !important;
                border: 2px solid #ffc107 !important;
                color: #856404 !important;
            }
            .bg-danger {
                background-color: white !important;
                border: 2px solid #dc3545 !important;
                color: #dc3545 !important;
            }
            /* Footer styling with left alignment */
            .footer {
                margin-top: 20px;
                padding: 10px 15px;
                background-color: #f8f9fa;
                text-align: left;
                border-top: 1px solid #e9ecef;
                font-style: italic;
                color: #6c757d;
                display: flex;
                justify-content: space-between;
                align-items: center;
            }
            .footer a {
                color: #0275d8;
                text-decoration: none;
            }
            .footer a:hover {
                text-decoration: underline;
            }
            .filter-btn {
                margin-right: 5px;
            }
            .filter-container {
                display: flex;
                align-items: center;
                gap: 15px;
                margin-bottom: 15px;
            }
            .select-container {
                min-width: 200px;
            }
            """
        )
    ),
    ui.panel_title("FDA Adverse Events Reporting System (FAERS) Public Dashboard"),
    ui.navset_bar(
        ui.nav_panel("Home",
            # Value boxes at the top
            ui.layout_column_wrap(
                ui.value_box("Total Reports",
                    f"{data_report_type['Report_Count'].sum():,}",
                    showcase=ui.HTML('<i class="fa-solid fa-chart-line" style="font-size: 60px;"></i>'),
                    theme="secondary",
                    showcase_layout="left center"),
                ui.value_box("Serious Reports (excluding death)",
                    "16,664,479",
                    showcase=ui.HTML('<i class="fa-solid fa-triangle-exclamation" style="font-size: 60px;"></i>'),
                    theme="warning",
                    showcase_layout="left center"),
                ui.value_box("Death Reports",
                    "2,722,806",
                    showcase=ui.HTML('<i class="fa-solid fa-circle-xmark" style="font-size: 60px;"></i>'),
                    theme="danger",
                    showcase_layout="left center"),
                width=1/3
            ),
            
            # Add some spacing
            ui.br(),
            
            # Filter container with selector and buttons
            ui.div(
                ui.div(
                    ui.input_select(
                        "data_type", 
                        "",  # No label, just the dropdown
                        {
                            "report_type": "Report by Report Type",
                            "reporter": "Report by Reporter"
                        },
                        width="100%"
                    ),
                    class_="select-container"
                ),
                ui.input_action_button(
                    "show_all", "All Years", 
                    class_="btn-primary filter-btn", 
                    icon=ui.HTML('<i class="fa-solid fa-calendar"></i>')
                ),
                ui.input_action_button(
                    "show_recent", "Last 10 Years", 
                    class_="btn-secondary filter-btn", 
                    icon=ui.HTML('<i class="fa-solid fa-clock-rotate-left"></i>')
                ),
                class_="filter-container"
            ),
            
            # Layout for table and plot
            ui.layout_column_wrap(
                # Left column - Interactive DT table
                ui.card(
                    ui.card_header(ui.output_text("table_header")),
                    ui.output_data_frame("data_table")
                ),
                
                # Right column - Bar plot (no interactivity with table)
                ui.card(
                    ui.card_header(ui.output_text("plot_header")),
                    output_widget("bar_plot")
                ),
                width=1/2  # Two columns of equal width
            )
        ),
        ui.nav_panel(ui.HTML('<i class="fa-solid fa-search"></i> Search')),
        # Add external links on the right side directly in the navbar
        ui.nav_spacer(),  # Push the following items to the right
        ui.nav_control(
            ui.a("Disclaimer", href="https://example.com/a", target="_blank", class_="nav-link")
        ),
        ui.nav_control(
            ui.a("Report a Problem", href="https://example.com/b", target="_blank", class_="nav-link")
        ),
        ui.nav_control(
            ui.a("FAQ", href="https://example.com/c", target="_blank", class_="nav-link")
        ),
        ui.nav_control(
            ui.a("Site Feedback", href="https://example.com/d", target="_blank", class_="nav-link")
        ),
        title=""
    ),
    # Footer added here with text aligned to the left and vulnerability disclosure link on the right
    ui.div(
        ui.div(
            ui.p("Data as of January 31, 2025")
        ),
        ui.div(
            ui.a("Vulnerability Disclosure", href="https://example.com/vulnerability", target="_blank")
        ),
        class_="footer"
    )
)

# Server logic
def server(input, output, session):
    # Create reactive values to store states
    filter_state = reactive.value("all")  # Default to showing all years
    
    # Dynamic headers for the cards
    @output
    @render.text
    def table_header():
        if input.data_type() == "report_type":
            return "Report Statistics by Year (Report Type)"
        else:
            return "Report Statistics by Year (Reporter)"
    
    @output
    @render.text
    def plot_header():
        if input.data_type() == "report_type":
            return "Report Types by Year"
        else:
            return "Reporter Types by Year"
    
    # Update filter state based on button clicks
    @reactive.effect
    @reactive.event(input.show_all)
    def _():
        filter_state.set("all")
    
    @reactive.effect
    @reactive.event(input.show_recent)
    def _():
        filter_state.set("recent")
    
    # Get the appropriate data based on the selected data type
    @reactive.calc
    def current_data():
        if input.data_type() == "report_type":
            return data_report_type
        else:
            return data_reporter
    
    @reactive.calc
    def current_data_wide():
        if input.data_type() == "report_type":
            return data_wide_report_type
        else:
            return data_wide_reporter
    
    # Function to filter the data based on the current filter state
    @reactive.calc
    def filtered_data_wide():
        data = current_data_wide()
        if filter_state.get() == "all":
            return data
        else:
            # Get the last 10 years from the data
            years = sorted(data['year'].unique())
            recent_years = years[-10:] if len(years) >= 10 else years
            return data[data['year'].isin(recent_years)]
    
    @reactive.calc
    def filtered_data():
        data = current_data()
        if filter_state.get() == "all":
            return data
        else:
            # Get the last 10 years from the data
            years = sorted(data['year'].unique())
            recent_years = years[-10:] if len(years) >= 10 else years
            return data[data['year'].isin(recent_years)]
    
    # Changed to render.data_frame with filtered data
    @output
    @render.data_frame
    def data_table():
        # Simple DataTable with default sorting capabilities
        return render.DataTable(filtered_data_wide())
    
    @render_widget
    def bar_plot():
        # Use the filtered long-format data for plotting
        current_data_filtered = filtered_data()
        
        # Determine the color column based on data type
        color_col = "Report_Type" if input.data_type() == "report_type" else "Reporter"
        
        # Create bar plot using plotly express
        fig = px.bar(
            current_data_filtered, 
            x='year', 
            y='Report_Count', 
            color=color_col,
            barmode='group',
            labels={
                'Report_Count': 'Number of Reports', 
                'year': 'Year', 
                'Report_Type': 'Report Type',
                'Reporter': 'Reporter Type'
            },
            color_discrete_sequence=px.colors.qualitative.Plotly
        )
        
        # Customize layout
        fig.update_layout(
            legend=dict(orientation='h', y=1.1),
            margin=dict(l=40, r=40, t=60, b=40)
        )
        
        # Return the figure
        return fig

# App object
app = App(app_ui, server)