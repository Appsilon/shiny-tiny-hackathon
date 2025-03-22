
// Knowledge base for AI chat
// This file contains prompts and contextual information for the AI assistant

export const systemPrompt = `
You are an expert on FDA Adverse Event Reporting System (FAERS) data. Provide concise, helpful responses about the FAERS data. 
If asked about specific years or data points, focus your analysis on those elements. Keep responses under 150 words unless detailed analysis is requested.

You have access to FAERS data from 1968 through 2024 with the following categories:
- Expedited reports
- Non-Expedited reports
- Direct reports
- BSR (Biologic Surveillance Reports)

Here are important facts about the FAERS data:
- Total reports across all years: 30,179,725
- Of these, there are 16,526,858 Expedited reports, 12,340,868 Non-Expedited reports, 1,311,944 Direct reports, and 863 BSR reports
- Data for 2024 IS available in this system and shows 2,041,829 total reports (11,68,649 Expedited, 8,15,057 Non-Expedited, 58,123 Direct)
- The highest number of reports was recorded in 2022 with 2,337,099 total reports
- BSR reports have been rare, with most occurring between 1996-2005

When discussing data trends, note:
- Reporting volume has increased significantly since 1968, with a major increase starting around 2010
- Expedited reports have generally increased as a proportion of total reports over time
- 2024 shows a slight decrease in total reports compared to 2023 (21,54,455 to 20,41,829)
`;

export const yearBasedPrompts = (year: number): string => {
  let yearInfo = "";
  
  // Add specific information about years that might be commonly queried
  if (year === 2024) {
    yearInfo = `
For 2024, FAERS data shows:
- Total reports: 2,041,829
- Expedited: 1,168,649 (57.2%)
- Non-Expedited: 815,057 (39.9%)
- Direct: 58,123 (2.8%)
- BSR: 0
This represents a 5.2% decrease from 2023.
    `;
  } else if (year === 2023) {
    yearInfo = `
For 2023, FAERS data shows:
- Total reports: 2,154,455
- Expedited: 1,248,041 (57.9%)
- Non-Expedited: 837,783 (38.9%)
- Direct: 68,631 (3.2%)
- BSR: 0
This represents a 7.8% decrease from 2022, which had the highest report volume.
    `;
  } else if (year === 2022) {
    yearInfo = `
For 2022, FAERS data shows:
- Total reports: 2,337,099 (highest yearly total in the database)
- Expedited: 1,308,258 (56.0%)
- Non-Expedited: 950,762 (40.7%)
- Direct: 78,079 (3.3%)
- BSR: 0
    `;
  }
  
  return yearInfo;
};

// Function to incorporate knowledge about data patterns into responses
export const getDataTrendKnowledge = () => {
  return `
Key data trends in FAERS reporting:

1. Long-term growth: From just 107 reports in 1968 to over 2 million annually since 2018
2. Reporting categories evolution: Expedited reports have become the dominant category since 2002
3. Recent pattern: Report numbers peaked in 2022 and have shown a slight decrease in 2023-2024
4. BSR reports: These were most common between 1996-2005 and have since been discontinued
  `;
};

// Function to get knowledge about specific report types
export const getReportTypeKnowledge = (reportType: string) => {
  switch (reportType.toLowerCase()) {
    case "expedited":
      return `
Expedited reports are submitted for serious and unexpected adverse events. They are required to be submitted to FDA within 15 calendar days of receiving the information. In 2024, there were 1,168,649 expedited reports, representing 57.2% of all reports that year.
      `;
    case "non-expedited":
      return `
Non-expedited reports include serious but expected adverse events and non-serious adverse events. In 2024, there were 815,057 non-expedited reports, accounting for 39.9% of all reports that year.
      `;
    case "direct":
      return `
Direct reports are submitted directly to the FDA by healthcare professionals and consumers, rather than through drug manufacturers. In 2024, there were 58,123 direct reports, making up 2.8% of all reports that year.
      `;
    case "bsr":
      return `
Biologic Surveillance Reports (BSR) were a special category for certain biologic products. BSR reporting was most common between 1996-2005, with the highest number (350) in 2000. There have been no BSR reports recorded since 2005.
      `;
    default:
      return "";
  }
};
