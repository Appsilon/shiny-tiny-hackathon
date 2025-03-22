
import React from 'react';
import {
  Sheet,
  SheetContent,
  SheetDescription,
  SheetHeader,
  SheetTitle,
} from '@/components/ui/sheet';
import { YearlyReportData } from '@/lib/data';

interface ReportTypeDetailSheetProps {
  isOpen: boolean;
  onClose: () => void;
  year: number | null;
  reportType: string | null;
  reportData: YearlyReportData | null;
}

const ReportTypeDetailSheet = ({
  isOpen,
  onClose,
  year,
  reportType,
  reportData,
}: ReportTypeDetailSheetProps) => {
  if (!year || !reportType || !reportData) return null;

  // Get the correct report count based on the report type
  const getReportCount = () => {
    switch (reportType.toLowerCase()) {
      case 'expedited':
        return reportData.expedited;
      case 'non-expedited':
        return reportData.nonExpedited;
      case 'direct':
        return reportData.direct;
      case 'bsr':
        return reportData.bsr || 0;
      default:
        return 0;
    }
  };

  const reportCount = getReportCount();
  const percentOfTotal = ((reportCount / reportData.total) * 100).toFixed(1);

  // Get report type description
  const getReportTypeDescription = () => {
    switch (reportType.toLowerCase()) {
      case 'expedited':
        return 'Expedited reports are submitted by manufacturers within 15 days of receiving information about serious, unexpected adverse events.';
      case 'non-expedited':
        return 'Non-expedited reports are submitted by manufacturers for less serious or expected adverse events, typically within 90 days.';
      case 'direct':
        return 'Direct reports are submitted voluntarily by healthcare professionals and consumers directly to the FDA via MedWatch forms.';
      case 'bsr':
        return 'Bioavailability/Bioequivalence Study Reports (BSR) are from studies comparing the performance of different drug formulations.';
      default:
        return '';
    }
  };

  return (
    <Sheet open={isOpen} onOpenChange={onClose}>
      <SheetContent side="left" className="sm:max-w-md">
        <SheetHeader>
          <SheetTitle className="text-xl">{reportType} Reports ({year})</SheetTitle>
          <SheetDescription>
            Detailed information about {reportType} reports from {year}
          </SheetDescription>
        </SheetHeader>
        <div className="mt-6 space-y-6">
          <div className="grid gap-4">
            <div className="bg-slate-50 p-4 rounded-lg border border-slate-200">
              <h3 className="text-sm font-medium mb-2">Report Summary</h3>
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <p className="text-sm text-slate-500">Total {reportType} Reports</p>
                  <p className="text-xl font-semibold">{reportCount.toLocaleString()}</p>
                </div>
                <div>
                  <p className="text-sm text-slate-500">Percentage of All Reports</p>
                  <p className="text-xl font-semibold">{percentOfTotal}%</p>
                </div>
              </div>
            </div>
            
            <div className="bg-slate-50 p-4 rounded-lg border border-slate-200">
              <h3 className="text-sm font-medium mb-2">What are {reportType} Reports?</h3>
              <p className="text-sm text-slate-700">
                {getReportTypeDescription()}
              </p>
            </div>
            
            <div className="bg-slate-50 p-4 rounded-lg border border-slate-200">
              <h3 className="text-sm font-medium mb-2">Context</h3>
              <p className="text-sm text-slate-700">
                In {year}, the FDA received a total of {reportData.total.toLocaleString()} reports,
                with {reportType} reports making up {percentOfTotal}% of all submissions.
              </p>
            </div>
          </div>
        </div>
      </SheetContent>
    </Sheet>
  );
};

export default ReportTypeDetailSheet;
