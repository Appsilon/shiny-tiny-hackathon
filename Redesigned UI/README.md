# FAERS Dashboard Helper

A modern dashboard for analyzing FDA Adverse Events Reporting System (FAERS) data with AI assistance. This application provides interactive visualizations and AI-powered analysis of FAERS reports.

## 🎥 Demo & Features
- [Watch Demo Video](https://www.loom.com/share/975fcff0c65840cea504aa310896ea60?sid=1480f2e7-1aaa-41b1-b9ce-d58bd815404a)
- Interactive data visualization
- AI-powered analysis assistant
- Year-over-year trend analysis
- Detailed report breakdowns
- Real-time data filtering

## 🚀 Getting Started

### Prerequisites
- Node.js (v18 or higher)
- npm or yarn
- OpenAI API key

### Installation

1. **Clone the repository**
```bash
git clone https://github.com/Shaik-mohd-huzaifa/faers-dashboard-helper.git
cd faers-dashboard-helper
```

2. **Install dependencies**
```bash
npm install
```

3. **Configure environment**
Create a `.env` file in the root directory:
```bash
VITE_OPENAI_API_KEY=your_openai_api_key_here
```
Get your API key from [OpenAI Platform](https://platform.openai.com/api-keys)

4. **Start development server**
```bash
npm run dev
```
The app will be available at `http://localhost:8080`

## 🛠️ Technology Stack
- **Frontend:** React 18 with TypeScript
- **Styling:** Tailwind CSS + shadcn/ui
- **State Management:** React Query
- **Data Visualization:** Recharts
- **AI Integration:** OpenAI API
- **Build Tool:** Vite

## 📦 Production Deployment

Build the application:
```bash
npm run build
```

Preview production build:
```bash
npm run preview
```

## 📝 License
MIT License - feel free to use this project for your own purposes.

## 🤝 Contributing
Contributions are welcome! Please feel free to submit a Pull Request.
