
import { useState } from 'react';
import { Sheet, SheetContent, SheetHeader, SheetTitle } from '@/components/ui/sheet';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { DataTableRow, QuarterlyReportData, getQuarterlyData } from '@/lib/data';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { BarChart2, PieChart, Presentation } from 'lucide-react';
import { PieChart as RechartsPieChart, Pie, Cell, BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, CartesianGrid, Legend } from 'recharts';

interface ReportDetailModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  data: DataTableRow;
}

const COLORS = ['#4CAF50', '#8BC34A', '#F44336', '#2196F3'];

const ReportDetailModal = ({ open, onOpenChange, data }: ReportDetailModalProps) => {
  const [view, setView] = useState<'table' | 'bar' | 'pie'>('table');
  const quarterlyData = getQuarterlyData(data.year);
  
  const pieChartData = [
    { name: 'Expedited', value: data.expedited },
    { name: 'Non-Expedited', value: data.nonExpedited },
    { name: 'Direct', value: data.direct },
    { name: 'BSR', value: data.bsr },
  ];

  return (
    <Sheet open={open} onOpenChange={onOpenChange}>
      <SheetContent side="left" className="sm:max-w-md w-[90vw]">
        <SheetHeader className="mb-6">
          <SheetTitle className="text-xl">{data.year} Detailed Reports</SheetTitle>
        </SheetHeader>
        
        <div className="space-y-6">
          <Tabs defaultValue="quarterly" className="w-full">
            <TabsList className="w-full">
              <TabsTrigger value="quarterly" className="flex-1">Quarterly</TabsTrigger>
              <TabsTrigger value="summary" className="flex-1">Summary</TabsTrigger>
            </TabsList>

            <TabsContent value="quarterly" className="space-y-4 mt-6">
              <div className="flex justify-center gap-4 mb-4">
                <button 
                  onClick={() => setView('table')}
                  className={`p-2 rounded-md ${view === 'table' ? 'bg-blue-100 text-blue-600' : 'text-slate-600'}`}
                >
                  <Presentation size={20} />
                </button>
                <button 
                  onClick={() => setView('bar')}
                  className={`p-2 rounded-md ${view === 'bar' ? 'bg-blue-100 text-blue-600' : 'text-slate-600'}`}
                >
                  <BarChart2 size={20} />
                </button>
                <button 
                  onClick={() => setView('pie')}
                  className={`p-2 rounded-md ${view === 'pie' ? 'bg-blue-100 text-blue-600' : 'text-slate-600'}`}
                >
                  <PieChart size={20} />
                </button>
              </div>

              {view === 'table' && (
                <div className="border rounded-md">
                  <Table>
                    <TableHeader className="bg-slate-50">
                      <TableRow>
                        <TableHead>Quarter</TableHead>
                        <TableHead className="text-right">Total</TableHead>
                        <TableHead className="text-right">Expedited</TableHead>
                        <TableHead className="text-right">Non-Expedited</TableHead>
                        <TableHead className="text-right">Direct</TableHead>
                        <TableHead className="text-right">BSR</TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {quarterlyData.map((quarter) => (
                        <TableRow key={quarter.quarter}>
                          <TableCell className="font-medium">{quarter.quarter}</TableCell>
                          <TableCell className="text-right">{quarter.total.toLocaleString()}</TableCell>
                          <TableCell className="text-right">{quarter.expedited.toLocaleString()}</TableCell>
                          <TableCell className="text-right">{quarter.nonExpedited.toLocaleString()}</TableCell>
                          <TableCell className="text-right">{quarter.direct.toLocaleString()}</TableCell>
                          <TableCell className="text-right">{quarter.bsr.toLocaleString()}</TableCell>
                        </TableRow>
                      ))}
                    </TableBody>
                  </Table>
                </div>
              )}

              {view === 'bar' && (
                <div className="h-80">
                  <ResponsiveContainer width="100%" height="100%">
                    <BarChart
                      data={quarterlyData}
                      margin={{ top: 20, right: 30, left: 20, bottom: 5 }}
                    >
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="quarter" />
                      <YAxis />
                      <Tooltip formatter={(value) => value.toLocaleString()} />
                      <Legend />
                      <Bar dataKey="expedited" name="Expedited" fill="#4CAF50" />
                      <Bar dataKey="nonExpedited" name="Non-Expedited" fill="#8BC34A" />
                      <Bar dataKey="direct" name="Direct" fill="#F44336" />
                      <Bar dataKey="bsr" name="BSR" fill="#2196F3" />
                    </BarChart>
                  </ResponsiveContainer>
                </div>
              )}

              {view === 'pie' && (
                <div className="h-80 flex justify-center items-center">
                  <ResponsiveContainer width="100%" height="100%">
                    <RechartsPieChart>
                      <Pie
                        data={pieChartData}
                        cx="50%"
                        cy="50%"
                        labelLine={false}
                        label={({ name, percent }) => `${name}: ${(percent * 100).toFixed(0)}%`}
                        outerRadius={80}
                        fill="#8884d8"
                        dataKey="value"
                      >
                        {pieChartData.map((entry, index) => (
                          <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                        ))}
                      </Pie>
                      <Tooltip formatter={(value) => value.toLocaleString()} />
                    </RechartsPieChart>
                  </ResponsiveContainer>
                </div>
              )}
            </TabsContent>

            <TabsContent value="summary" className="space-y-4 pt-4">
              <div className="glass-card p-4 rounded-lg border border-slate-200">
                <h4 className="font-medium text-lg mb-2">Year Overview</h4>
                <p className="text-slate-600 mb-4">
                  {data.year} had a total of {data.total.toLocaleString()} reported adverse events.
                </p>
                
                <div className="space-y-2">
                  <div className="flex justify-between">
                    <span className="text-slate-600">Expedited Reports:</span>
                    <span className="font-medium">{data.expedited.toLocaleString()} ({((data.expedited / data.total) * 100).toFixed(1)}%)</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-slate-600">Non-Expedited Reports:</span>
                    <span className="font-medium">{data.nonExpedited.toLocaleString()} ({((data.nonExpedited / data.total) * 100).toFixed(1)}%)</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-slate-600">Direct Reports:</span>
                    <span className="font-medium">{data.direct.toLocaleString()} ({((data.direct / data.total) * 100).toFixed(1)}%)</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-slate-600">BSR Reports:</span>
                    <span className="font-medium">{data.bsr.toLocaleString()} ({((data.bsr / data.total) * 100).toFixed(1)}%)</span>
                  </div>
                </div>
              </div>
              
              <div className="glass-card p-4 rounded-lg border border-slate-200">
                <h4 className="font-medium text-lg mb-2">Quarterly Distribution</h4>
                <div className="space-y-2">
                  {quarterlyData.map((quarter) => (
                    <div key={quarter.quarter} className="flex justify-between">
                      <span className="text-slate-600">{quarter.quarter}:</span>
                      <span className="font-medium">{quarter.total.toLocaleString()} ({((quarter.total / data.total) * 100).toFixed(1)}%)</span>
                    </div>
                  ))}
                </div>
              </div>
            </TabsContent>
          </Tabs>
        </div>
      </SheetContent>
    </Sheet>
  );
};

export default ReportDetailModal;
