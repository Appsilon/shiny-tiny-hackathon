
import React, { useState, useRef, useEffect } from 'react';
import { X, Send, MessageSquare, BarChart3, Info, FileText } from 'lucide-react';
import { cn } from '@/lib/utils';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { completeYearlyReportData } from '@/lib/data';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts';
import OpenAI from 'openai';
import { systemPrompt, yearBasedPrompts, getDataTrendKnowledge } from '@/lib/chatKnowledgeBase';
import ReactMarkdown from 'react-markdown';

interface Message {
  id: string;
  content: string;
  isUser: boolean;
  widget?: React.ReactNode;
}

// Get the OpenAI API key from environment variables
const OPENAI_API_KEY = import.meta.env.VITE_OPENAI_API_KEY;
const openai = new OpenAI({
  apiKey: OPENAI_API_KEY,
  dangerouslyAllowBrowser: true // Only for demo, in production use backend
});

// Create a serializable version of messages for storage
interface SerializableMessage {
  id: string;
  content: string;
  isUser: boolean;
}

const AIChat = () => {
  const [isOpen, setIsOpen] = useState(false);
  const [input, setInput] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const [messages, setMessages] = useState<Message[]>([]);
  
  const messagesEndRef = useRef<HTMLDivElement>(null);
  
  // Load chat history from localStorage when component mounts
  useEffect(() => {
    const storedMessages = localStorage.getItem('chatHistory');
    if (storedMessages) {
      try {
        const parsedMessages: SerializableMessage[] = JSON.parse(storedMessages);
        // Recreate widgets for messages if needed based on content
        const reconstructedMessages = parsedMessages.map(msg => {
          const message: Message = {
            id: msg.id,
            content: msg.content,
            isUser: msg.isUser
          };
          
          // If it's not a user message, check if we need to add widgets
          if (!msg.isUser) {
            const yearMatch = msg.content.match(/\b(19|20)\d{2}\b/);
            if (yearMatch) {
              const year = yearMatch[0];
              const yearData = completeYearlyReportData.find(item => item.year === parseInt(year));
              
              if (yearData) {
                if (msg.content.includes('chart') || msg.content.includes('distribution')) {
                  message.widget = generateYearChart(year, yearData);
                } else if (msg.content.includes('report') && (msg.content.includes('summary') || msg.content.includes('breakdown'))) {
                  message.widget = generateReportExplanation(year, yearData);
                }
              }
            }
          }
          
          return message;
        });
        
        setMessages(reconstructedMessages.length ? reconstructedMessages : [
          {
            id: '1',
            content: 'Hello! I can help you analyze FAERS data. Ask me about specific years, report types, or trends.',
            isUser: false,
          },
        ]);
      } catch (error) {
        console.error("Error parsing stored messages:", error);
        setDefaultWelcomeMessage();
      }
    } else {
      setDefaultWelcomeMessage();
    }
  }, []);
  
  // Set default welcome message
  const setDefaultWelcomeMessage = () => {
    setMessages([
      {
        id: '1',
        content: 'Hello! I can help you analyze FAERS data. Ask me about specific years, report types, or trends.',
        isUser: false,
      },
    ]);
  };
  
  // Save messages to localStorage whenever they change
  useEffect(() => {
    if (messages.length > 0) {
      // Create a serializable version of messages (without widget JSX)
      const serializableMessages: SerializableMessage[] = messages.map(({ id, content, isUser }) => ({
        id,
        content,
        isUser
      }));
      
      localStorage.setItem('chatHistory', JSON.stringify(serializableMessages));
    }
  }, [messages]);
  
  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };
  
  useEffect(() => {
    if (isOpen) {
      scrollToBottom();
    }
  }, [messages, isOpen]);

  const handleToggle = () => {
    setIsOpen(!isOpen);
    if (!isOpen) {
      setTimeout(scrollToBottom, 100);
    }
  };

  // Add a function to clear chat history
  const clearChatHistory = () => {
    localStorage.removeItem('chatHistory');
    setDefaultWelcomeMessage();
  };

  const analyzeQuery = (query: string) => {
    const lowerQuery = query.toLowerCase();
    
    const yearMatch = lowerQuery.match(/\b(19|20)\d{2}\b/);
    if (yearMatch && lowerQuery.includes('chart') || (lowerQuery.includes('data') && yearMatch)) {
      const year = yearMatch[0];
      const yearData = completeYearlyReportData.find(item => item.year === parseInt(year));
      
      if (yearData) {
        return {
          type: 'year-chart',
          year,
          data: yearData
        };
      }
    }
    
    if (lowerQuery.includes('explain') && lowerQuery.includes('report') && yearMatch) {
      const year = yearMatch[0];
      const yearData = completeYearlyReportData.find(item => item.year === parseInt(year));
      
      if (yearData) {
        return {
          type: 'report-explanation',
          year,
          data: yearData
        };
      }
    }
    
    return {
      type: 'text'
    };
  };

  const generateYearChart = (year: string, data: any) => {
    const chartData = [
      { name: 'Expedited', value: data.expedited },
      { name: 'Non-Expedited', value: data.nonExpedited },
      { name: 'Direct', value: data.direct },
      { name: 'BSR', value: data.bsr || 0 }
    ];
    
    return (
      <div className="bg-white p-4 rounded-lg shadow-sm mt-2 mb-2">
        <h4 className="text-sm font-medium mb-2">Report Distribution for {year}</h4>
        <div style={{ width: '100%', height: 200 }}>
          <ResponsiveContainer>
            <BarChart data={chartData}>
              <CartesianGrid strokeDasharray="3 3" vertical={false} opacity={0.2} />
              <XAxis dataKey="name" />
              <YAxis 
                tickFormatter={(value) => 
                  value >= 1000000 
                    ? `${(value / 1000000).toFixed(1)}M` 
                    : value >= 1000 
                      ? `${(value / 1000).toFixed(0)}K` 
                      : `${value}`
                } 
              />
              <Tooltip 
                formatter={(value: any) => [Number(value).toLocaleString(), 'Reports']}
              />
              <Bar 
                dataKey="value" 
                fill="#1E88E5" 
                name="Reports" 
              />
            </BarChart>
          </ResponsiveContainer>
        </div>
      </div>
    );
  };

  const generateReportExplanation = (year: string, data: any) => {
    const total = data.total;
    const expeditedPercent = ((data.expedited / total) * 100).toFixed(1);
    const nonExpeditedPercent = ((data.nonExpedited / total) * 100).toFixed(1);
    const directPercent = ((data.direct / total) * 100).toFixed(1);
    const bsrPercent = data.bsr ? ((data.bsr / total) * 100).toFixed(1) : "0.0";
    
    return (
      <div className="bg-slate-50 p-4 rounded-lg border border-slate-200 mt-2 mb-2">
        <h4 className="text-sm font-medium flex items-center gap-2 mb-2">
          <FileText size={16} />
          Report Summary for {year}
        </h4>
        <div className="grid grid-cols-2 gap-2 text-xs">
          <div className="flex flex-col">
            <span className="text-slate-500">Total Reports</span>
            <span className="font-medium">{total.toLocaleString()}</span>
          </div>
          <div className="flex flex-col">
            <span className="text-slate-500">Expedited</span>
            <span className="font-medium">{data.expedited.toLocaleString()} ({expeditedPercent}%)</span>
          </div>
          <div className="flex flex-col">
            <span className="text-slate-500">Non-Expedited</span>
            <span className="font-medium">{data.nonExpedited.toLocaleString()} ({nonExpeditedPercent}%)</span>
          </div>
          <div className="flex flex-col">
            <span className="text-slate-500">Direct</span>
            <span className="font-medium">{data.direct.toLocaleString()} ({directPercent}%)</span>
          </div>
          {data.bsr > 0 && (
            <div className="flex flex-col">
              <span className="text-slate-500">BSR</span>
              <span className="font-medium">{data.bsr.toLocaleString()} ({bsrPercent}%)</span>
            </div>
          )}
        </div>
      </div>
    );
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!input.trim()) return;
    
    const userMessage: Message = {
      id: Date.now().toString(),
      content: input,
      isUser: true,
    };
    
    setMessages((prev) => [...prev, userMessage]);
    setInput('');
    setIsLoading(true);
    
    try {
      const queryAnalysis = analyzeQuery(input);
      
      const yearMatch = input.toLowerCase().match(/\b(19|20)\d{2}\b/);
      let yearSpecificKnowledge = "";
      if (yearMatch) {
        const year = parseInt(yearMatch[0]);
        yearSpecificKnowledge = yearBasedPrompts(year);
      }
      
      const enhancedSystemPrompt = `${systemPrompt}
${yearSpecificKnowledge}
${yearMatch ? '' : getDataTrendKnowledge()}
      `;
      
      const response = await openai.chat.completions.create({
        model: "gpt-4o",
        messages: [
          {
            role: "system" as const,
            content: enhancedSystemPrompt
          },
          ...messages.map(msg => ({
            role: msg.isUser ? "user" as const : "assistant" as const,
            content: msg.content
          })),
          { role: "user" as const, content: input }
        ],
        temperature: 0.7,
        max_tokens: 500,
      });
      
      const aiContent = response.choices[0]?.message?.content || "I'm sorry, I couldn't process that request.";
      
      const aiMessage: Message = {
        id: (Date.now() + 1).toString(),
        content: aiContent,
        isUser: false,
      };
      
      if (queryAnalysis.type === 'year-chart') {
        aiMessage.widget = generateYearChart(queryAnalysis.year, queryAnalysis.data);
      } else if (queryAnalysis.type === 'report-explanation') {
        aiMessage.widget = generateReportExplanation(queryAnalysis.year, queryAnalysis.data);
      }
      
      setMessages((prev) => [...prev, aiMessage]);
    } catch (error) {
      console.error("Error getting AI response:", error);
      
      const errorMessage: Message = {
        id: (Date.now() + 1).toString(),
        content: "I'm sorry, I encountered an error processing your request. Please try again later.",
        isUser: false,
      };
      
      setMessages((prev) => [...prev, errorMessage]);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <>
      <button
        onClick={handleToggle}
        className="fixed bottom-6 right-6 p-4 rounded-full bg-primary text-white shadow-lg hover:shadow-xl z-40 transition-all duration-200 hover:scale-105"
        aria-label="Open AI Chat"
      >
        <MessageSquare className="w-6 h-6" />
      </button>

      {isOpen && (
        <div className="fixed inset-y-0 right-0 w-[30%] z-50 flex flex-col bg-white border-l border-gray-200 shadow-xl">
          <div className="px-4 py-3 border-b flex justify-between items-center">
            <h2 className="font-semibold text-lg">FAERS AI Assistant</h2>
            <div className="flex gap-2">
              <Button
                variant="ghost"
                size="sm"
                onClick={clearChatHistory}
                className="text-gray-500 hover:text-gray-700"
                aria-label="Clear chat history"
              >
                Clear History
              </Button>
              <Button
                variant="ghost"
                size="icon"
                onClick={handleToggle}
                aria-label="Close chat"
              >
                <X className="w-5 h-5" />
              </Button>
            </div>
          </div>

          <div className="flex-1 overflow-y-auto p-4 space-y-4">
            {messages.map((message) => (
              <div key={message.id}>
                <div 
                  className={cn(
                    "max-w-[90%] rounded-lg p-3", 
                    message.isUser 
                      ? "bg-primary text-white ml-auto" 
                      : "bg-gray-100 text-gray-800"
                  )}
                >
                  {message.isUser ? (
                    message.content
                  ) : (
                    <div className="prose prose-sm max-w-none dark:prose-invert">
                      <ReactMarkdown>{message.content}</ReactMarkdown>
                    </div>
                  )}
                </div>
                {message.widget && <div className="mt-2">{message.widget}</div>}
              </div>
            ))}
            {isLoading && (
              <div className="max-w-[90%] rounded-lg p-3 bg-gray-100 text-gray-800">
                <div className="flex items-center gap-2">
                  <div className="animate-pulse flex gap-1">
                    <div className="h-2 w-2 bg-gray-400 rounded-full"></div>
                    <div className="h-2 w-2 bg-gray-400 rounded-full"></div>
                    <div className="h-2 w-2 bg-gray-400 rounded-full"></div>
                  </div>
                  <span className="text-sm text-gray-500">Thinking...</span>
                </div>
              </div>
            )}
            <div ref={messagesEndRef} />
          </div>

          <form onSubmit={handleSubmit} className="border-t p-4">
            <div className="flex gap-2">
              <Input
                value={input}
                onChange={(e) => setInput(e.target.value)}
                placeholder="Ask about FAERS data..."
                className="flex-1"
                disabled={isLoading}
              />
              <Button type="submit" aria-label="Send message" disabled={isLoading}>
                <Send className="w-4 h-4" />
              </Button>
            </div>
            <div className="text-xs text-gray-400 mt-1 text-center">
              Try: "Show me a chart for 2024" or "Explain reports for 2023"
            </div>
          </form>
        </div>
      )}
    </>
  );
};

export default AIChat;
