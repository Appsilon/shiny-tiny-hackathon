
import { useState } from "react";
import { Button } from "@/components/ui/button";
import { cn } from "@/lib/utils";

interface TimeframeSelectorProps {
  onChange: (timeframe: "all" | "last18") => void;
  className?: string;
}

const TimeframeSelector = ({ onChange, className }: TimeframeSelectorProps) => {
  const [activeTimeframe, setActiveTimeframe] = useState<"all" | "last18">("all");

  const handleTimeframeChange = (timeframe: "all" | "last18") => {
    setActiveTimeframe(timeframe);
    onChange(timeframe);
  };

  return (
    <div className={cn("flex rounded-lg overflow-hidden border border-slate-200", className)}>
      <Button
        variant="ghost"
        className={cn(
          "rounded-none flex-1 px-8 border-0 h-12",
          activeTimeframe === "all" ? "bg-blue-500 text-white hover:bg-blue-600" : "bg-white text-slate-700 hover:bg-slate-100"
        )}
        onClick={() => handleTimeframeChange("all")}
      >
        All Years
      </Button>
      <Button
        variant="ghost"
        className={cn(
          "rounded-none flex-1 px-8 border-0 h-12",
          activeTimeframe === "last18" ? "bg-blue-500 text-white hover:bg-blue-600" : "bg-white text-slate-700 hover:bg-slate-100"
        )}
        onClick={() => handleTimeframeChange("last18")}
      >
        Last 18 Years
      </Button>
    </div>
  );
};

export default TimeframeSelector;
