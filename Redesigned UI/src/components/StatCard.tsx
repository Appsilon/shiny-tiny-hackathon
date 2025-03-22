
import { cn } from "@/lib/utils";

interface StatCardProps {
  title: string;
  value: string | number;
  icon?: React.ReactNode;
  trend?: {
    value: string;
    positive?: boolean;
  };
  className?: string;
  animationDelay?: string;
}

const StatCard = ({
  title,
  value,
  icon,
  trend,
  className,
  animationDelay = "animation-delay-100"
}: StatCardProps) => {
  // Format large numbers with commas
  const formattedValue = typeof value === 'number' 
    ? value.toLocaleString() 
    : value;

  return (
    <div 
      className={cn(
        "glass-card rounded-xl p-6 opacity-0 animate-fade-in",
        animationDelay,
        className
      )}
    >
      <div className="flex items-start justify-between">
        <div>
          <h3 className="text-sm font-medium text-slate-500 mb-1">{title}</h3>
          <p className="text-3xl font-semibold">{formattedValue}</p>
          {trend && (
            <p className={cn(
              "text-sm mt-1 flex items-center",
              trend.positive ? "text-emerald-500" : "text-rose-500"
            )}>
              {trend.positive ? "↑" : "↓"} {trend.value}
            </p>
          )}
        </div>
        {icon && (
          <div className="p-2 rounded-lg bg-blue-50 text-blue-500">
            {icon}
          </div>
        )}
      </div>
    </div>
  );
};

export default StatCard;
