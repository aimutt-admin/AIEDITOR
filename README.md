# AIEDITOR  
**AI-Enabled Text Editor for IBM PC/XT (QuickBASIC 4.0)**

AIEDITOR is a retro-modern text editor designed for the **IBM PC/XT (5150/5160)** that combines a classic full-screen editing experience with real-time AI interaction over a serial connection.

It is part of the broader *IBM-AI-Terminal* concept — bringing modern AI capabilities to authentic 1980s hardware.

---

## ✨ Features

- Full-screen text editor written in QuickBASIC 4.0  
- Keyboard-driven interface with function key controls  
- Load and save text files on DOS systems  
- Cut, copy, paste, and text selection support  
- Word wrap with proper reflow during editing  
- Prompts to prevent loss of unsaved changes  
- Built-in AI chat integration via serial port (COM1)  
- Communicates with a Raspberry Pi running a local LLM  
- AI responses formatted for 80-column display  

---

## 🧠 How It Works

AIEDITOR runs on a vintage IBM PC and communicates with a modern AI system through a serial connection:
IBM PC/XT (AIEDITOR)
│
│ RS-232 (Null Modem)
▼
Raspberry Pi (Python Bridge)
│
▼
llama.cpp (LLM Server)
│
▼
Qwen 2.5 1.5B (GGUF Model)

- The editor sends user prompts over COM1  
- A Python bridge on the Raspberry Pi receives the data  
- The bridge forwards the request to a local LLM (via llama-server)  
- The response is sent back and displayed in the editor  

---

## ⚙️ Requirements

### IBM PC/XT Side
- IBM 5150 / 5160 or compatible  
- MS-DOS or PC-DOS  
- Microsoft QuickBASIC 4.0 (for editing/compiling)  
- Serial port (COM1)  
- Null modem cable  

### Raspberry Pi Side
- Raspberry Pi (tested with Pi 5)  
- Python 3  
- llama.cpp with server mode enabled  
- GGUF model (example: Qwen 2.5 1.5B Instruct)  

---

## 🔌 Serial Configuration

AIEDITOR uses the same configuration as the working terminal:
COM1:9600,N,8,1,CS0,DS0,CD0


Make sure:
- Baud rate matches on both systems  
- You are using a **null modem cable** (not straight-through)  
- The correct `/dev/ttyUSB*` device is selected on the Raspberry Pi  

---

## 🧾 Editor Interface

The editor uses a structured layout similar to classic DOS applications:

[ Function Key Menu ]
| |
| Text Editing Viewport |
| |

[ Status Line / Messages ]
[ Command / Input Line ]


- Viewport scrolls independently  
- Status lines display system and AI messages  
- Prompts do not overwrite the menu  

---

## 🧪 Development Notes

- Serial communication logic is based on the stable `IBMTERM.BAS` implementation  
- Editor and serial systems are merged into a single module to avoid linker issues  
- Uses `ON ERROR` for file handling and recovery  
- Designed to compile into a standalone `.EXE`  

---

## ⚠️ Known Limitations

- Designed for 80x25 text mode  
- Depends on external Raspberry Pi for AI features  
- Performance limited by serial speed and AI response time  
- Keyboard-only interface (no mouse support)  

---

## 🚀 Future Enhancements

- Printer support  
- Search and replace  
- Improved clipboard handling  
- AI-assisted editing (rewrite, summarize, etc.)  
- Persistent AI conversation sessions  

---

## 📜 License

MIT License
“You can use this code however you want — just give credit and don’t blame me if something breaks.”
---

## 🧠 Inspiration

> "AI doesn’t have to live in the cloud — it can live anywhere… even inside a 1980s terminal."