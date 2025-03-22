// Mock data for FAERS dashboard

export type ReportTypeCount = {
  type: string;
  count: number;
  color: string;
};

export type YearlyReportData = {
  year: number;
  total: number;
  expedited: number;
  nonExpedited: number;
  direct: number;
  bsr: number;
};

export type DataTableRow = {
  year: number;
  total: number;
  expedited: number;
  nonExpedited: number;
  direct: number;
  bsr: number;
};

export type QuarterlyReportData = {
  quarter: string;
  total: number;
  expedited: number;
  nonExpedited: number;
  direct: number;
  bsr: number;
};

// Total counts
export const totalReports = 30_179_725;
export const seriousReports = 16_664_479;
export const deathReports = 2_722_806;

// Report types
export const reportTypes: ReportTypeCount[] = [
  { type: 'Expedited', count: 16_526_858, color: '#4CAF50' },
  { type: 'Non-Expedited', count: 12_340_868, color: '#8BC34A' },
  { type: 'Direct', count: 1_311_944, color: '#F44336' },
  { type: 'BSR', count: 863, color: '#2196F3' },
];

// Complete yearly report data (1968-2024)
export const completeYearlyReportData: YearlyReportData[] = [
  { year: 1968, total: 107, expedited: 0, nonExpedited: 98, direct: 9, bsr: 0 },
  { year: 1969, total: 5433, expedited: 1, nonExpedited: 2745, direct: 2687, bsr: 0 },
  { year: 1970, total: 17947, expedited: 5, nonExpedited: 9979, direct: 7963, bsr: 0 },
  { year: 1971, total: 13156, expedited: 1, nonExpedited: 8238, direct: 4917, bsr: 0 },
  { year: 1972, total: 9637, expedited: 0, nonExpedited: 7603, direct: 2034, bsr: 0 },
  { year: 1973, total: 11377, expedited: 4, nonExpedited: 7305, direct: 4068, bsr: 0 },
  { year: 1974, total: 9253, expedited: 1, nonExpedited: 7004, direct: 2248, bsr: 0 },
  { year: 1975, total: 10615, expedited: 0, nonExpedited: 8472, direct: 2143, bsr: 0 },
  { year: 1976, total: 10821, expedited: 0, nonExpedited: 6950, direct: 3870, bsr: 1 },
  { year: 1977, total: 13741, expedited: 2, nonExpedited: 9919, direct: 3820, bsr: 0 },
  { year: 1978, total: 9435, expedited: 17, nonExpedited: 7107, direct: 2311, bsr: 0 },
  { year: 1979, total: 8243, expedited: 91, nonExpedited: 6190, direct: 1962, bsr: 0 },
  { year: 1980, total: 12912, expedited: 119, nonExpedited: 9891, direct: 2902, bsr: 0 },
  { year: 1981, total: 14500, expedited: 4721, nonExpedited: 7121, direct: 2658, bsr: 0 },
  { year: 1982, total: 23705, expedited: 6971, nonExpedited: 12868, direct: 3866, bsr: 0 },
  { year: 1983, total: 32021, expedited: 8114, nonExpedited: 20209, direct: 3698, bsr: 0 },
  { year: 1984, total: 32623, expedited: 6756, nonExpedited: 24247, direct: 1620, bsr: 0 },
  { year: 1985, total: 38799, expedited: 9796, nonExpedited: 26522, direct: 2481, bsr: 0 },
  { year: 1986, total: 55928, expedited: 15848, nonExpedited: 36006, direct: 4073, bsr: 1 },
  { year: 1987, total: 54759, expedited: 12972, nonExpedited: 37260, direct: 4527, bsr: 0 },
  { year: 1988, total: 51138, expedited: 8277, nonExpedited: 37078, direct: 5783, bsr: 0 },
  { year: 1989, total: 60682, expedited: 9461, nonExpedited: 43096, direct: 8125, bsr: 0 },
  { year: 1990, total: 75201, expedited: 11405, nonExpedited: 53427, direct: 10367, bsr: 2 },
  { year: 1991, total: 75290, expedited: 12826, nonExpedited: 52027, direct: 10434, bsr: 3 },
  { year: 1992, total: 105954, expedited: 15943, nonExpedited: 77071, direct: 12940, bsr: 0 },
  { year: 1993, total: 124198, expedited: 19111, nonExpedited: 90334, direct: 14752, bsr: 1 },
  { year: 1994, total: 129122, expedited: 22205, nonExpedited: 89936, direct: 16977, bsr: 4 },
  { year: 1995, total: 141667, expedited: 21833, nonExpedited: 103344, direct: 16481, bsr: 9 },
  { year: 1996, total: 172204, expedited: 26312, nonExpedited: 130303, direct: 15569, bsr: 20 },
  { year: 1997, total: 198234, expedited: 37707, nonExpedited: 144258, direct: 16205, bsr: 64 },
  { year: 1998, total: 159890, expedited: 70818, nonExpedited: 73682, direct: 15247, bsr: 143 },
  { year: 1999, total: 224363, expedited: 80970, nonExpedited: 127049, direct: 16175, bsr: 169 },
  { year: 2000, total: 199799, expedited: 94160, nonExpedited: 89169, direct: 16120, bsr: 350 },
  { year: 2001, total: 203208, expedited: 113680, nonExpedited: 70176, direct: 19299, bsr: 53 },
  { year: 2002, total: 184879, expedited: 127770, nonExpedited: 36642, direct: 20447, bsr: 20 },
  { year: 2003, total: 225234, expedited: 143656, nonExpedited: 58613, direct: 22952, bsr: 13 },
  { year: 2004, total: 272831, expedited: 161335, nonExpedited: 89834, direct: 21657, bsr: 5 },
  { year: 2005, total: 321879, expedited: 212087, nonExpedited: 84481, direct: 25306, bsr: 5 },
  { year: 2006, total: 335670, expedited: 219143, nonExpedited: 95548, direct: 20979, bsr: 0 },
  { year: 2007, total: 363325, expedited: 229897, nonExpedited: 110397, direct: 23031, bsr: 0 },
  { year: 2008, total: 439671, expedited: 274092, nonExpedited: 132683, direct: 32896, bsr: 0 },
  { year: 2009, total: 490_488, expedited: 330_083, nonExpedited: 126_159, direct: 34_166, bsr: 0 },
  { year: 2010, total: 672_640, expedited: 409_063, nonExpedited: 234_633, direct: 28_944, bsr: 0 },
  { year: 2011, total: 781_627, expedited: 498_420, nonExpedited: 255_165, direct: 28_042, bsr: 0 },
  { year: 2012, total: 930_179, expedited: 574_943, nonExpedited: 326_224, direct: 29_012, bsr: 0 },
  { year: 2013, total: 1_068_593, expedited: 631_158, nonExpedited: 409_045, direct: 28_390, bsr: 0 },
  { year: 2014, total: 1_198_451, expedited: 741_482, nonExpedited: 412_721, direct: 34_230, bsr: 0 },
  { year: 2015, total: 1_719_791, expedited: 833_112, nonExpedited: 845_020, direct: 41_659, bsr: 0 },
  { year: 2016, total: 1_683_373, expedited: 863_270, nonExpedited: 760_112, direct: 50_991, bsr: 0 },
  { year: 2017, total: 1_805_010, expedited: 941_777, nonExpedited: 801_203, direct: 62_030, bsr: 0 },
  { year: 2018, total: 2_140_149, expedited: 1_155_290, nonExpedited: 897_308, direct: 87_551, bsr: 0 },
  { year: 2019, total: 2_175_284, expedited: 1_215_841, nonExpedited: 854_855, direct: 105_388, bsr: 0 },
  { year: 2020, total: 2_202_814, expedited: 1_242_171, nonExpedited: 882_083, direct: 78_560, bsr: 0 },
  { year: 2021, total: 2_328_590, expedited: 1_387_985, nonExpedited: 868_056, direct: 72_549, bsr: 0 },
  { year: 2022, total: 2_337_099, expedited: 1_308_258, nonExpedited: 950_762, direct: 78_079, bsr: 0 },
  { year: 2023, total: 2_154_455, expedited: 1_248_041, nonExpedited: 837_783, direct: 68_631, bsr: 0 },
  { year: 2024, total: 2_041_829, expedited: 1_168_649, nonExpedited: 815_057, direct: 58_123, bsr: 0 },
];

// Use only the most recent years for the default yearly data
export const yearlyReportData: YearlyReportData[] = completeYearlyReportData.slice(-16);

// Table data
export const tableData: DataTableRow[] = yearlyReportData;

// Generate quarterly data for each year
export const getQuarterlyData = (year: number): QuarterlyReportData[] => {
  const yearData = yearlyReportData.find(data => data.year === year);
  
  if (!yearData) return [];
  
  // Calculate roughly equal distributions across quarters with slight variations
  return [
    {
      quarter: "Q1",
      total: Math.round(yearData.total * 0.22),
      expedited: Math.round(yearData.expedited * 0.23),
      nonExpedited: Math.round(yearData.nonExpedited * 0.21),
      direct: Math.round(yearData.direct * 0.24),
      bsr: Math.round(yearData.bsr * 0.25),
    },
    {
      quarter: "Q2",
      total: Math.round(yearData.total * 0.26),
      expedited: Math.round(yearData.expedited * 0.27),
      nonExpedited: Math.round(yearData.nonExpedited * 0.26),
      direct: Math.round(yearData.direct * 0.25),
      bsr: Math.round(yearData.bsr * 0.25),
    },
    {
      quarter: "Q3",
      total: Math.round(yearData.total * 0.24),
      expedited: Math.round(yearData.expedited * 0.23),
      nonExpedited: Math.round(yearData.nonExpedited * 0.25),
      direct: Math.round(yearData.direct * 0.24),
      bsr: Math.round(yearData.bsr * 0.25),
    },
    {
      quarter: "Q4",
      total: Math.round(yearData.total * 0.28),
      expedited: Math.round(yearData.expedited * 0.27),
      nonExpedited: Math.round(yearData.nonExpedited * 0.28),
      direct: Math.round(yearData.direct * 0.27),
      bsr: Math.round(yearData.bsr * 0.25),
    },
  ];
};
