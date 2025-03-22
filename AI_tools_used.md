# AI tools description

For the task today, I got help from Claude 3.7 Sonnet. I only got help from this AI tool, in the form of a conversation. I have not worked yet with other types of AI help.

## Creating the artificial data

I asked Claude "Can you give me Python code to create articial data with discrete and categorical columns?"

Claude's answer contained code to create all types of columns. I adapted it to copy the categories of the FDA data.

## Creating a basic dashboard app without a sidebar

The code worked right away!

## Putting the data table in a scrollable box

After asking about the basic app, I asked Claude to put the table output into a scrollable box. The code it gave me worked, but it was limited to a width of 400 pixels. I wanted it half-screen, so a few back-and-forths gave new custom .css code. 

Unfortunately, from there, the graph output was laways under the table, and no matter what I tried to fix it, it stuck there.

I scrapped the original layout of the app and used the page_fillable() layout with cards. It looked as I wanted right away.

## Making a stacked barplot graph with plotnine

Claude got me the code to make the stacked barplot with the column names I supplied. To refine the plot (getting the filled categories in frequency order, and adding the total of reports as a label on top of the stack), Claude gave me perfect code.

## Getting a better color palette

Again with Claude 3.7 Sonnet, I asked "Can you give me a palette of 8 distinct colors that would look nice in a graph?". Claude answered: "Here's a palette of 8 distinct colors that would work well in graphs and charts: 

     #4E79A7 - Deep Blue
     #F28E2B - Orange
     #E15759 - Red
     #76B7B2 - Teal
     #59A14F - Green
     #EDC948 - Yellow
     #B07AA1 - Purple
     #FF9DA7 - Pink
     

These colors have been selected to be: 

     Visually distinct from each other
     Work well together harmoniously
     Maintain good contrast when placed adjacent to one another
     Still distinguishable for most forms of color blindness
     Readable when used for lines, bars, or points in data visualization
     

This palette is inspired by professional visualization color schemes used in tools like Tableau and D3.js. "

For implementation, I used a Google search and the plotnine documentation.

## Adding control over the years analyzed

The original dashboard offered to the user a choice between using all the data available or only the data from the last 10 years. Claude supplied me code to modify the app to control the years using a slider input. The data is then filtered with the year range selected by the user, before being aggregated.

Claude also replied succesfully when asked "Can you modify the input slider so that “2009” is displayed rather than “2,019”?". The year display is even better than what I asked!

# Conclusion

I am pleased with how Claude 3.7 Sonnet handled my questions and answered using code and detailed outputs and explanations.