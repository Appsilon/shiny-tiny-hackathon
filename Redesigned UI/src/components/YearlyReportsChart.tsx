
import { useState } from 'react';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, TooltipProps } from 'recharts';
import { Button } from '@/components/ui/button';
import { ChevronLeft, ChevronRight } from 'lucide-react';
import { YearlyReportData, completeYearlyReportData } from '@/lib/data';
import ReportTypeDetailSheet from './ReportTypeDetailSheet';

interface YearlyReportsChartProps {
  data: YearlyReportData[];
  className?: string;
}

const ITEMS_PER_PAGE = 16;

const CustomTooltip = ({
  active,
  payload,
  label,
}: TooltipProps<number, string>) => {
  if (active && payload && payload.length) {
    return (
      <div className="custom-tooltip bg-white p-4 rounded-md shadow-lg border border-slate-100">
        <p className="font-semibold mb-2">{`Year: ${label}`}</p>
        {payload.map((entry) => (
          <div key={entry.name} className="flex items-center gap-2 mb-1">
            <div
              className="w-3 h-3 rounded-full"
              style={{ backgroundColor: entry.color }}
            />
            <span className="text-sm">{`${entry.name}: ${Number(entry.value).toLocaleString()}`}</span>
          </div>
        ))}
      </div>
    );
  }

  return null;
};

// Define a mapping of dataKey to human-readable report type
const reportTypeMap: Record<string, string> = {
  'bsr': 'BSR',
  'direct': 'Direct',
  'expedited': 'Expedited',
  'nonExpedited': 'Non-Expedited'
};

const YearlyReportsChart = ({ data, className }: YearlyReportsChartProps) => {
  const [page, setPage] = useState(Math.ceil(completeYearlyReportData.length / ITEMS_PER_PAGE) - 1);
  const totalPages = Math.ceil(completeYearlyReportData.length / ITEMS_PER_PAGE);
  
  // State for the detail sheet
  const [isSheetOpen, setIsSheetOpen] = useState(false);
  const [selectedYear, setSelectedYear] = useState<number | null>(null);
  const [selectedReportType, setSelectedReportType] = useState<string | null>(null);
  const [selectedYearData, setSelectedYearData] = useState<YearlyReportData | null>(null);
  
  const startIndex = page * ITEMS_PER_PAGE;
  const endIndex = Math.min(startIndex + ITEMS_PER_PAGE, completeYearlyReportData.length);
  const displayedData = completeYearlyReportData.slice(startIndex, endIndex);
  
  const handlePreviousPage = () => {
    setPage((prev) => Math.max(0, prev - 1));
  };
  
  const handleNextPage = () => {
    setPage((prev) => Math.min(totalPages - 1, prev + 1));
  };
  
  const yearRange = displayedData.length > 0 
    ? `${displayedData[0].year}-${displayedData[displayedData.length - 1].year}`
    : '';

  // Handle bar click
  const handleBarClick = (data: any, index: number, dataKey: string) => {
    const year = data.year;
    const reportType = reportTypeMap[dataKey];
    
    // Find the year data
    const yearData = completeYearlyReportData.find(item => item.year === year);
    
    if (yearData) {
      setSelectedYear(year);
      setSelectedReportType(reportType);
      setSelectedYearData(yearData);
      setIsSheetOpen(true);
    }
  };

  return (
    <div className={className}>
      <div className="flex justify-between items-center mb-6">
        <h3 className="text-lg font-semibold">Reports Received by Year</h3>
        <div className="flex items-center space-x-2">
          <Button 
            variant="outline" 
            size="sm" 
            onClick={handlePreviousPage} 
            disabled={page === 0}
            className="flex items-center"
          >
            <ChevronLeft className="h-4 w-4 mr-1" />
            Previous
          </Button>
          <span className="text-sm text-muted-foreground">
            {yearRange} ({page + 1}/{totalPages})
          </span>
          <Button 
            variant="outline" 
            size="sm" 
            onClick={handleNextPage} 
            disabled={page === totalPages - 1}
            className="flex items-center"
          >
            Next
            <ChevronRight className="h-4 w-4 ml-1" />
          </Button>
        </div>
      </div>
      <div className="w-full h-[400px]">
        <ResponsiveContainer width="100%" height="100%">
          <BarChart
            data={displayedData}
            margin={{
              top: 20,
              right: 30,
              left: 20,
              bottom: 60,
            }}
          >
            <CartesianGrid strokeDasharray="3 3" vertical={false} opacity={0.2} />
            <XAxis 
              dataKey="year" 
              angle={-45} 
              textAnchor="end" 
              tickMargin={15}
              tick={{ fontSize: 12 }}
            />
            <YAxis 
              tickFormatter={(value) => value >= 1000000 ? `${(value / 1000000).toFixed(1)}M` : value >= 1000 ? `${(value / 1000).toFixed(0)}K` : `${value}`}
              tick={{ fontSize: 12 }}
            />
            <Tooltip content={<CustomTooltip />} />
            <Legend 
              layout="horizontal" 
              verticalAlign="top" 
              align="center" 
              wrapperStyle={{ paddingBottom: '20px' }}
            />
            <Bar 
              dataKey="bsr" 
              name="BSR" 
              stackId="a" 
              fill="#2196F3" 
              animationDuration={1500}
              animationEasing="ease-out"
              onClick={(data, index) => handleBarClick(data, index, 'bsr')}
              style={{ cursor: 'pointer' }}
            />
            <Bar 
              dataKey="direct" 
              name="Direct" 
              stackId="a" 
              fill="#F44336" 
              animationDuration={1500}
              animationEasing="ease-out"
              animationBegin={300}
              onClick={(data, index) => handleBarClick(data, index, 'direct')}
              style={{ cursor: 'pointer' }}
            />
            <Bar 
              dataKey="expedited" 
              name="Expedited" 
              stackId="a" 
              fill="#4CAF50" 
              animationDuration={1500}
              animationEasing="ease-out"
              animationBegin={600}
              onClick={(data, index) => handleBarClick(data, index, 'expedited')}
              style={{ cursor: 'pointer' }}
            />
            <Bar 
              dataKey="nonExpedited" 
              name="Non-Expedited" 
              stackId="a" 
              fill="#8BC34A" 
              animationDuration={1500}
              animationEasing="ease-out"
              animationBegin={900}
              onClick={(data, index) => handleBarClick(data, index, 'nonExpedited')}
              style={{ cursor: 'pointer' }}
            />
          </BarChart>
        </ResponsiveContainer>
      </div>
      
      {/* Report Detail Sheet */}
      <ReportTypeDetailSheet
        isOpen={isSheetOpen}
        onClose={() => setIsSheetOpen(false)}
        year={selectedYear}
        reportType={selectedReportType}
        reportData={selectedYearData}
      />
    </div>
  );
};

export default YearlyReportsChart;
