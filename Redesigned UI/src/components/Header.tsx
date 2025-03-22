
import { Search } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";

const Header = () => {
  return (
    <header className="w-full bg-white border-b border-slate-100 py-3 px-6 sticky top-0 z-50 shadow-sm backdrop-blur-sm bg-white/90">
      <div className="max-w-7xl mx-auto flex flex-col sm:flex-row items-center justify-between gap-4">
        <div className="flex items-center gap-2">
          <div className="bg-blue-600 w-10 h-10 flex items-center justify-center rounded">
            <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path d="M20 8H4V6H20V8ZM20 18H4V12H20V18ZM20 4H4C2.9 4 2 4.9 2 6V18C2 19.1 2.9 20 4 20H20C21.1 20 22 19.1 22 18V6C22 4.9 21.1 4 20 4ZM12 13H6V17H12V13Z" fill="white"/>
            </svg>
          </div>
          <div>
            <h1 className="text-xl font-semibold text-slate-800">FDA Adverse Events Reporting System</h1>
            <p className="text-sm text-slate-500">FAERS Public Dashboard</p>
          </div>
        </div>
        
        <div className="flex w-full sm:w-auto gap-2">
          <div className="relative w-full sm:w-64">
            <Search className="absolute left-2 top-1/2 transform -translate-y-1/2 text-slate-400" size={18} />
            <Input 
              placeholder="Search reports..." 
              className="pl-9 pr-4 py-2 h-10 w-full" 
            />
          </div>
          
          <div className="flex gap-2">
            <Button variant="outline" size="sm" className="h-10">FAQ</Button>
            <Button size="sm" className="h-10">Report a Problem</Button>
          </div>
        </div>
      </div>
    </header>
  );
};

export default Header;
