import pandas as pd
import io

data_table = """year Report_Type Report_Count
2005 Expedited 74
2005 Non-Expedited 56
2005 Direct 89
2005 BSR 23
2006 Expedited 74
2006 Non-Expedited 56
2006 Direct 89
2006 BSR 23
2007 Expedited 74
2007 Non-Expedited 56
2007 Direct 89
2007 BSR 23
2008 Expedited 74
2008 Non-Expedited 56
2008 Direct 89
2008 BSR 23
2009 Expedited 74
2009 Non-Expedited 56
2009 Direct 89
2009 BSR 23
2010 Expedited 74
2010 Non-Expedited 56
2010 Direct 89
2010 BSR 23
2011 Expedited 74
2011 Non-Expedited 56
2011 Direct 89
2011 BSR 23
2012 Expedited 74
2012 Non-Expedited 56
2012 Direct 89
2012 BSR 23
2013 Expedited 74
2013 Non-Expedited 56
2013 Direct 89
2013 BSR 23
2014 Expedited 74
2014 Non-Expedited 56
2014 Direct 89
2014 BSR 23
2015 Expedited 74
2015 Non-Expedited 56
2015 Direct 89
2015 BSR 23
2016 Expedited 74
2016 Non-Expedited 56
2016 Direct 89
2016 BSR 23
2017 Expedited 74
2017 Non-Expedited 56
2017 Direct 89
2017 BSR 23
2018 Expedited 74
2018 Non-Expedited 56
2018 Direct 89
2018 BSR 23
2019 Expedited 74
2019 Non-Expedited 56
2019 Direct 89
2019 BSR 23
2020 Expedited 74
2020 Non-Expedited 56
2020 Direct 89
2020 BSR 23
2021 Expedited 74
2021 Non-Expedited 56
2021 Direct 89
2021 BSR 23
2022 Expedited 74
2022 Non-Expedited 56
2022 Direct 89
2022 BSR 23
2023 Expedited 74
2023 Non-Expedited 56
2023 Direct 89
2023 BSR 23
2024 Expedited 74
2024 Non-Expedited 56
2024 Direct 89
2024 BSR 23
2025 Expedited 74
2025 Non-Expedited 56
2025 Direct 89
2025 BSR 23
2004 Expedited 74
2004 Non-Expedited 56
2004 Direct 89
2004 BSR 23
"""

data_report_type = pd.read_csv(io.StringIO(data_table), sep='\s+')
# Create a wide-format version of the data for the table display
data_wide_report_type = data_report_type.pivot_table(
    index="year", 
    columns="Report_Type", 
    values="Report_Count"
).reset_index()

# Calculate Total Reports for each year
data_wide_report_type["Total Reports"] = data_wide_report_type[["Expedited", "Non-Expedited", "Direct", "BSR"]].sum(axis=1)

# Reorder columns to have Total Reports first
cols = ["year", "Total Reports", "Expedited", "Non-Expedited", "Direct", "BSR"]
data_wide_report_type = data_wide_report_type[cols]

data = """year Reporter Report_Count
2005 Consumer 74
2005 "Healthcare Professional" 56
2005 "Not Specified" 89
2005 Other 23
2006 Consumer 74
2006 "Healthcare Professional" 56
2006 "Not Specified" 89
2006 Other 23
2007 Consumer 74
2007 "Healthcare Professional" 56
2007 "Not Specified" 89
2007 Other 23
2008 Consumer 74
2008 "Healthcare Professional" 56
2008 "Not Specified" 89
2008 Other 23
2009 Consumer 74
2009 "Healthcare Professional" 56
2009 "Not Specified" 89
2009 Other 23
2010 Consumer 74
2010 "Healthcare Professional" 56
2010 "Not Specified" 89
2010 Other 23
2011 Consumer 74
2011 "Healthcare Professional" 56
2011 "Not Specified" 89
2011 Other 23
2012 Consumer 74
2012 "Healthcare Professional" 56
2012 "Not Specified" 89
2012 Other 23
2013 Consumer 74
2013 "Healthcare Professional" 56
2013 "Not Specified" 89
2013 Other 23
2014 Consumer 74
2014 "Healthcare Professional" 56
2014 "Not Specified" 89
2014 Other 23
2015 Consumer 74
2015 "Healthcare Professional" 56
2015 "Not Specified" 89
2015 Other 23
2016 Consumer 74
2016 "Healthcare Professional" 56
2016 "Not Specified" 89
2016 Other 23
2017 Consumer 74
2017 "Healthcare Professional" 56
2017 "Not Specified" 89
2017 Other 23
2018 Consumer 74
2018 "Healthcare Professional" 56
2018 "Not Specified" 89
2018 Other 23
2019 Consumer 74
2019 "Healthcare Professional" 56
2019 "Not Specified" 89
2019 Other 23
2020 Consumer 74
2020 "Healthcare Professional" 56
2020 "Not Specified" 89
2020 Other 23
2021 Consumer 74
2021 "Healthcare Professional" 56
2021 "Not Specified" 89
2021 Other 23
2022 Consumer 74
2022 "Healthcare Professional" 56
2022 "Not Specified" 89
2022 Other 23
2023 Consumer 74
2023 "Healthcare Professional" 56
2023 "Not Specified" 89
2023 Other 23
2024 Consumer 74
2024 "Healthcare Professional" 56
2024 "Not Specified" 89
2024 Other 23
2025 Consumer 74
2025 "Healthcare Professional" 56
2025 "Not Specified" 89
2025 Other 23
2004 Consumer 74
2004 "Healthcare Professional" 56
2004 "Not Specified" 89
2004 Other 23
"""

data_reporter = pd.read_csv(io.StringIO(data), sep='\s+')
# Create a wide-format version of the data for the table display
data_wide_reporter = data_reporter.pivot_table(
    index="year", 
    columns="Reporter", 
    values="Report_Count"
).reset_index()

# Calculate Total Reports for each year
data_wide_reporter["Total Reports"] = data_wide_reporter[["Consumer", "Healthcare Professional", "Not Specified", "Other"]].sum(axis=1)

# Reorder columns to have Total Reports first
cols = ["year", "Total Reports", "Consumer", "Healthcare Professional", "Not Specified", "Other"]
data_wide_reporter = data_wide_reporter[cols]