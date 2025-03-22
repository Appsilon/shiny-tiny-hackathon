FDA Adverse Event Reporting System (FAERS) Dashboard

Overview

This Shiny application recreates the FDA Adverse Event Reporting System (FAERS) public dashboard, utilizing mocked data for rapid prototyping. The objective was to demonstrate how AI-assisted tools can significantly streamline the development process, from data generation and preparation to user interface (UI) design and interaction implementation.

Approach & Reasoning

Given the four-hour limit set by the Appsilon Tiny Shiny Hackathon, efficiency was paramount. My approach involved leveraging powerful AI tools (ChatGPT and Shiny Assistant) to rapidly prototype the app, significantly reducing development time. This allowed more focus on enhancing the dashboard’s usability and interactivity, providing valuable insights into the FAERS data structure.

Development Workflow:

Mock Data Creation (AI-Assisted): To emulate realistic FAERS data, a synthetic dataset was programmatically generated using R scripts. AI tools guided the structuring of the data to ensure realistic distribution and relevant fields.

Data Wrangling (AI-Assisted): ChatGPT generated scripts to rapidly tidy and prepare the mock data, ensuring it was immediately ready for interactive visualizations and analytical exploration.

UI Design & Interaction (AI-Assisted): Using Shiny Assistant and ChatGPT, UI components (KPI cards, interactive tables, charts) were quickly prototyped, ensuring clarity, ease of use, and interactive capability.

How the Code Works

Data Generation:

The R script employs randomization techniques to generate synthetic data representative of real-world FAERS data, including variables like dates, event outcomes, patient demographics, drug information, and reporter details.

UI Components:

Navbar and Sidebar: Provide intuitive navigation and filtering capability across various criteria including dates, demographics, and event details.

Value Boxes: Clearly present key metrics such as Total Reports, Serious Reports (excluding deaths), and Death Reports.

Interactive Charts & Tables: Implemented with Plotly and DT for enhanced user interaction, providing dynamic data visualization and exploration.

Server Logic:

Reactive Data Filtering: Enables dynamic updates based on user-selected filters.

Data Visualization: Utilizes interactive Plotly charts, offering insights into reporting patterns over time.

Interactive Search: Allows targeted exploration of data based on search terms, dynamically rendering results.

AI Tools Utilized

ChatGPT: Assisted in quickly scripting the data structure and wrangling procedures, significantly accelerating the prototyping phase.

Shiny Assistant: Facilitated rapid UI prototyping, allowing quick visualization and iterative improvements of interface components.

Deployment

The final application is deployed and accessible via Shinyapps.io(https://fdaadverseeventsreportingsystemfaers.shinyapps.io/FAERS/), providing a responsive and user-friendly exploration of the mocked FAERS data.

Conclusion

The integration of AI tools streamlined the development process, allowed for rapid iteration, and enhanced the overall user experience of the FAERS dashboard prototype. This approach highlights the potential of AI to transform traditional development workflows, especially in time-constrained environments like hackathons.

This project submission aligns with the Appsilon Tiny Shiny Hackathon guidelines, showcasing both the capabilities of Shiny combined with AI-driven development.

