
import { PieChart, Pie, Cell, ResponsiveContainer, Legend, Tooltip } from "recharts";
import { ReportTypeCount } from "@/lib/data";

interface ReportTypeChartProps {
  data: ReportTypeCount[];
  className?: string;
}

const RADIAN = Math.PI / 180;
const renderCustomizedLabel = ({ 
  cx, 
  cy, 
  midAngle, 
  innerRadius, 
  outerRadius, 
  percent, 
  index, 
  name, 
  value 
}: any) => {
  const radius = innerRadius + (outerRadius - innerRadius) * 0.5;
  const x = cx + radius * Math.cos(-midAngle * RADIAN);
  const y = cy + radius * Math.sin(-midAngle * RADIAN);

  // Only show percentage for larger segments
  if (percent < 0.05) return null;

  return (
    <text 
      x={x} 
      y={y} 
      fill="white" 
      textAnchor={x > cx ? 'start' : 'end'} 
      dominantBaseline="central"
      fontWeight="500"
      fontSize="14"
    >
      {`${(percent * 100).toFixed(0)}%`}
    </text>
  );
};

const ReportTypeChart = ({ data, className }: ReportTypeChartProps) => {
  return (
    <div className={className}>
      <h3 className="text-lg font-semibold mb-6">Reports by Type</h3>
      <div className="w-full h-[300px]">
        <ResponsiveContainer width="100%" height="100%">
          <PieChart>
            <Pie
              data={data}
              cx="50%"
              cy="50%"
              labelLine={false}
              label={renderCustomizedLabel}
              outerRadius={120}
              fill="#8884d8"
              dataKey="count"
              nameKey="type"
            >
              {data.map((entry, index) => (
                <Cell key={`cell-${index}`} fill={entry.color} />
              ))}
            </Pie>
            <Tooltip 
              formatter={(value: number) => [value.toLocaleString(), 'Reports']} 
              labelFormatter={(label) => `${label} Reports`}
            />
            <Legend 
              layout="horizontal" 
              verticalAlign="bottom" 
              align="center"
              margin={{ top: 20, right: 0, left: 0, bottom: 0 }}
            />
          </PieChart>
        </ResponsiveContainer>
      </div>
    </div>
  );
};

export default ReportTypeChart;
