
import { useState } from "react";
import Header from "@/components/Header";
import StatCard from "@/components/StatCard";
import ReportTypeChart from "@/components/ReportTypeChart";
import YearlyReportsChart from "@/components/YearlyReportsChart";
import TimeframeSelector from "@/components/TimeframeSelector";
import DataTable from "@/components/DataTable";
import AIChat from "@/components/AIChat";
import { 
  AlertCircle, 
  BarChart3, 
  FileText
} from "lucide-react";
import { 
  totalReports, 
  seriousReports, 
  deathReports, 
  reportTypes, 
  yearlyReportData, 
  tableData,
  completeYearlyReportData
} from "@/lib/data";

const Index = () => {
  const [timeframe, setTimeframe] = useState<"all" | "last18">("all");
  
  // Filter data based on selected timeframe
  const filteredYearlyData = timeframe === "all" 
    ? yearlyReportData 
    : yearlyReportData.slice(-18);
    
  const filteredTableData = timeframe === "all"
    ? tableData
    : tableData.slice(-18);

  return (
    <div className="min-h-screen bg-slate-50">
      <Header />
      
      <main className="max-w-7xl mx-auto px-4 py-8">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
          <StatCard 
            title="Total Reports" 
            value={totalReports} 
            icon={<FileText size={24} />}
            animationDelay="animation-delay-100"
          />
          <StatCard 
            title="Serious Reports (Excluding Death)" 
            value={seriousReports} 
            icon={<AlertCircle size={24} />}
            trend={{ value: "12.3% increase from last year", positive: true }}
            animationDelay="animation-delay-200"
          />
          <StatCard 
            title="Death Reports" 
            value={deathReports} 
            icon={<BarChart3 size={24} />}
            animationDelay="animation-delay-300"
          />
        </div>
        
        <div className="flex justify-end mb-6">
          <TimeframeSelector 
            onChange={setTimeframe} 
            className="opacity-0 animate-slide-up animation-delay-400"
          />
        </div>
        
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
          <div className="glass-card rounded-xl p-6 opacity-0 animate-fade-in animation-delay-400">
            <YearlyReportsChart data={completeYearlyReportData} />
          </div>
          <div className="glass-card rounded-xl p-6 opacity-0 animate-fade-in animation-delay-500">
            <ReportTypeChart data={reportTypes} />
          </div>
        </div>
        
        <div className="glass-card rounded-xl p-6 mb-8 opacity-0 animate-fade-in animation-delay-500">
          <DataTable data={filteredTableData} />
        </div>
        
        <div className="text-center text-sm text-slate-500 mt-12 mb-8 opacity-0 animate-fade-in animation-delay-500">
          FDA Adverse Event Reporting System (FAERS) Public Dashboard
          <br />
          <span className="font-semibold">Disclaimer:</span> Report data is for demonstration purposes only.
        </div>
      </main>
      
      {/* Add the AI Chat component */}
      <AIChat />
    </div>
  );
};

export default Index;
