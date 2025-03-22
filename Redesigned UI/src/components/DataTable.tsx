
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { DataTableRow } from "@/lib/data";
import { useState } from "react";
import ReportDetailModal from "./ReportDetailModal";

interface DataTableProps {
  data: DataTableRow[];
  className?: string;
}

const DataTable = ({ data, className }: DataTableProps) => {
  const [selectedRow, setSelectedRow] = useState<DataTableRow | null>(null);
  const [isModalOpen, setIsModalOpen] = useState(false);

  const handleRowClick = (row: DataTableRow) => {
    setSelectedRow(row);
    setIsModalOpen(true);
  };

  return (
    <div className={className}>
      <h3 className="text-lg font-semibold mb-6">Reports by Year and Type</h3>
      <div className="overflow-auto max-h-[400px] rounded-md border border-slate-200 shadow-sm">
        <Table>
          <TableHeader className="sticky top-0 z-10 bg-slate-50">
            <TableRow>
              <TableHead className="font-semibold">Year</TableHead>
              <TableHead className="font-semibold text-right">Total Reports</TableHead>
              <TableHead className="font-semibold text-right">Expedited</TableHead>
              <TableHead className="font-semibold text-right">Non-Expedited</TableHead>
              <TableHead className="font-semibold text-right">Direct</TableHead>
              <TableHead className="font-semibold text-right">BSR</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {data.map((row) => (
              <TableRow 
                key={row.year} 
                className="hover:bg-slate-50 cursor-pointer" 
                onClick={() => handleRowClick(row)}
              >
                <TableCell className="font-medium">{row.year}</TableCell>
                <TableCell className="text-right">{row.total.toLocaleString()}</TableCell>
                <TableCell className="text-right">{row.expedited.toLocaleString()}</TableCell>
                <TableCell className="text-right">{row.nonExpedited.toLocaleString()}</TableCell>
                <TableCell className="text-right">{row.direct.toLocaleString()}</TableCell>
                <TableCell className="text-right">{row.bsr.toLocaleString()}</TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </div>
      <div className="text-xs text-slate-500 mt-2 text-right">
        Data as of December 31, 2024
      </div>

      {selectedRow && (
        <ReportDetailModal
          open={isModalOpen}
          onOpenChange={setIsModalOpen}
          data={selectedRow}
        />
      )}
    </div>
  );
};

export default DataTable;
