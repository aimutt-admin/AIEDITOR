DECLARE SUB AiSerialUseDefaultConfig ()
DECLARE SUB AiSerialClose ()
DECLARE SUB AiSerialSendLine (Msg$)
DECLARE SUB AiSerialOpen ()
DECLARE SUB AiSerialBuildOpenSpec ()
DECLARE SUB AiSerialClearError ()
DECLARE FUNCTION AiSerialCharsWaiting% ()
DECLARE FUNCTION AiSerialErrorCode% ()
DECLARE FUNCTION AiSerialErrorText$ ()
DECLARE FUNCTION AiSerialIsOpen% ()
DECLARE FUNCTION AiSerialOpenSpec$ ()
DECLARE FUNCTION AiSerialPortName$ ()
DECLARE FUNCTION AiSerialReadChar$ ()
DECLARE FUNCTION AiSerialReadLine$ ()
DEFINT A-Z

'=========================================================
' IBM 5150 AI Editor - aimutt.com 2026
' QuickBasic 4 style program with module-level ON ERROR.
'
' Current features:
'   - Full-screen text editor viewport
'   - AI / serial message pane
'   - COM1 serial open/close
'   - Ask AI popup to query the AI over serial port
'   - Send highlighted selection to AI over serial
'   - Save / Load popups
'   - Dirty flag with file name in menu bar
'   - Word wrap while typing
'   - Backspace pull-up / paragraph reflow
'   - Save prompt on exit if document is dirty
'=========================================================

'------------------------------
' Screen and editor constants
'------------------------------
CONST SCREENW = 80
CONST SCREENH = 25

CONST EDITTOP = 2
CONST EDITBOTTOM = 16
CONST EDITHEIGHT = 15

CONST AITOP = 18
CONST AIBOTTOM = 24
CONST AIHEIGHT = 7

CONST STATUSROW = 25
CONST SEPROW = 17

CONST MAXLINES = 500
CONST maxlen = 255
CONST MAXCHAT = 100

'------------------------------
' Action constants used by the main loop
'------------------------------
CONST ACTNONE = 0
CONST ACTSAVE = 1
CONST ACTLOAD = 2
CONST ACTEXIT = 3
CONST ACTOPEN = 4
CONST ACTCLOSE = 5
CONST ACTASKAI = 6
CONST ACTSENDSEL = 7

'------------------------------
' Shared editor state
'------------------------------
DIM SHARED Doc$(1 TO MAXLINES)
DIM SHARED SoftBreak(1 TO MAXLINES)
DIM SHARED TotalLines
DIM SHARED CurLine
DIM SHARED CurCol
DIM SHARED TopLine
DIM SHARED FileName$
DIM SHARED IsModified

'------------------------------
' Shared AI pane state
'------------------------------
DIM SHARED Chat$(1 TO MAXCHAT)
DIM SHARED ChatCount
DIM SHARED ChatTop

'------------------------------
' Shared program / file-operation state
'------------------------------
DIM SHARED PendingAction
DIM SHARED CurrentAction
DIM SHARED Running
DIM SHARED WorkName$
DIM SHARED WorkLine$
DIM SHARED LastSaveOK

'------------------------------
' Shared serial / AI state
'------------------------------
DIM SHARED RxBuffer$

' Serial module state. These MUST be SHARED because the serial
' routines below are separate SUBs/FUNCTIONs. Without SHARED,
' AiSerialBuildOpenSpec builds SOpenSpec$ locally, but AiSerialOpen
' sees a different/empty SOpenSpec$ and COM1 fails to open.
DIM SHARED SPortName$
DIM SHARED SBaud$
DIM SHARED SParity$
DIM SHARED SDataBits$
DIM SHARED SStopBits$
DIM SHARED SOpenSpec$
DIM SHARED SPortOpen%
DIM SHARED SPortFileNum%
DIM SHARED SLastErrorCode%
DIM SHARED SLastError$

'------------------------------
' Shared selection state
'------------------------------
DIM SHARED SelActive
DIM SHARED SelAnchorLine
DIM SHARED SelAnchorCol

'------------------------------
' Internal clipboard state
'------------------------------
DIM SHARED ClipText$
DIM SHARED ClipHasText

'------------------------------
' Procedure declarations
'------------------------------
DECLARE SUB InitEditor ()
DECLARE SUB DrawFrame ()
DECLARE SUB DrawMenuBar ()
DECLARE SUB DrawSeparator ()
DECLARE SUB DrawStatus (Msg$)
DECLARE SUB DrawEditor ()
DECLARE SUB DrawAIWindow ()
DECLARE SUB PutCursor ()
DECLARE SUB RedrawScreen ()
DECLARE SUB EnsureCursorVisible ()

DECLARE SUB HandleExtendedKey (k%)
DECLARE SUB HandleNormalKey (k%)
DECLARE SUB MoveLeft ()
DECLARE SUB MoveRight ()
DECLARE SUB MoveUp ()
DECLARE SUB MoveDown ()
DECLARE SUB MoveHome ()
DECLARE SUB MoveEnd ()

DECLARE SUB InsertCharacter (ch$)
DECLARE SUB BackspaceKey ()
DECLARE SUB EnterKey ()
DECLARE SUB WrapCurrentLine ()
DECLARE SUB WrapLineAt (lineNum%)
DECLARE SUB RewrapFromLine (startLine%)
DECLARE SUB FillLineFromNext (lineNum%)
DECLARE SUB ReflowForwardFromLine (startLine%)

DECLARE SUB AppendChat (Msg$)
DECLARE SUB AppendChatWrapped (Msg$)
DECLARE SUB PollSerial ()
DECLARE SUB OpenSerialPort ()
DECLARE SUB CloseSerialPort ()
DECLARE SUB AskAIQuestion ()
DECLARE SUB SendSelectionToAI ()
DECLARE FUNCTION GetSelectionText$ ()
DECLARE FUNCTION GetAskInput$ (title$, maxlen%)
DECLARE SUB DrawAskBox (toprow%, leftcol%, boxw%, boxh%, title$, value$)
DECLARE SUB ClearAskBox (toprow%, leftcol%, boxw%, boxh%)
DECLARE FUNCTION PortState$ ()
DECLARE SUB ShowAIPlaceholder ()
DECLARE SUB FlushKeys ()

DECLARE FUNCTION GetPopupInput$ (title$, prompt$, maxlen%)
DECLARE SUB DrawPopupBox (toprow%, leftcol%, boxw%, title$, prompt$, value$)
DECLARE SUB ClearPopupBox (toprow%, leftcol%, boxw%)

DECLARE FUNCTION ConfirmPopup% (title$, line1$, line2$)
DECLARE SUB DrawConfirmBox (toprow%, leftcol%, boxw%, title$, line1$, line2$)

DECLARE FUNCTION ConfirmSaveExit% ()
DECLARE SUB DrawSaveExitBox (toprow%, leftcol%, boxw%)
DECLARE FUNCTION ConfirmSaveMode% ()
DECLARE SUB DrawSaveModeBox (toprow%, leftcol%, boxw%)

DECLARE SUB InitSelection ()
DECLARE SUB ClearSelection ()
DECLARE SUB BeginSelection ()
DECLARE SUB SelectAll ()
DECLARE FUNCTION IsShiftDown% ()
DECLARE FUNCTION IsPositionBefore% (line1%, col1%, line2%, col2%)
DECLARE FUNCTION HasSelection% ()
DECLARE SUB GetSelectionBounds (startLine%, startCol%, endLine%, endCol%)
DECLARE SUB CopySelectionToClipboard ()
DECLARE SUB DeleteSelection ()
DECLARE SUB CutSelection ()
DECLARE SUB PasteClipboard ()

'=========================================================
' Program start
'=========================================================
COLOR 7, 0
CLS
LOCATE , , 1, 6, 7

InitEditor
DrawFrame
DrawEditor
DrawAIWindow
DrawStatus "F2=Save F3=Load F4=Ask F5=SelAI F6=Open F7=Close Shift+Arrow=Sel Esc=Exit"

AppendChat "AI> Ready."
AppendChat "AI> Serial AI features ready."
DrawAIWindow

PendingAction = ACTNONE
CurrentAction = ACTNONE
Running = 1
RxBuffer$ = ""
CALL AiSerialUseDefaultConfig

' QB4-style module-level error handler.
ON ERROR GOTO FileError

FlushKeys

'=========================================================
' Main loop
'=========================================================
DO WHILE Running = 1
    DIM k$
    DIM a%

    PutCursor
    k$ = INKEY$

    IF k$ <> "" THEN
        IF LEN(k$) = 2 THEN
            a% = ASC(RIGHT$(k$, 1))
            HandleExtendedKey a%
        ELSE
            a% = ASC(k$)
            HandleNormalKey a%
        END IF

        EnsureCursorVisible
        DrawEditor
        DrawAIWindow
    END IF

    IF PendingAction = ACTSAVE THEN
        CurrentAction = ACTSAVE
        GOSUB DoSave
        FlushKeys
        CurrentAction = ACTNONE
        PendingAction = ACTNONE
        DrawEditor
        DrawAIWindow

    ELSEIF PendingAction = ACTLOAD THEN
        CurrentAction = ACTLOAD
        GOSUB DoLoad
        FlushKeys
        CurrentAction = ACTNONE
        PendingAction = ACTNONE
        DrawEditor
        DrawAIWindow

    ELSEIF PendingAction = ACTOPEN THEN
        CurrentAction = ACTOPEN
        OpenSerialPort
        CurrentAction = ACTNONE
        PendingAction = ACTNONE
        RedrawScreen

    ELSEIF PendingAction = ACTCLOSE THEN
        CurrentAction = ACTCLOSE
        CloseSerialPort
        CurrentAction = ACTNONE
        PendingAction = ACTNONE
        RedrawScreen

    ELSEIF PendingAction = ACTASKAI THEN
        CurrentAction = ACTASKAI
        AskAIQuestion
        CurrentAction = ACTNONE
        PendingAction = ACTNONE
        RedrawScreen

    ELSEIF PendingAction = ACTSENDSEL THEN
        CurrentAction = ACTSENDSEL
        SendSelectionToAI
        CurrentAction = ACTNONE
        PendingAction = ACTNONE
        RedrawScreen

    ELSEIF PendingAction = ACTEXIT THEN
        IF IsModified <> 0 THEN
            SELECT CASE ConfirmSaveExit%
                CASE 1
                    CurrentAction = ACTSAVE
                    GOSUB DoSave
                    CurrentAction = ACTNONE
                    PendingAction = ACTNONE
                    IF LastSaveOK <> 0 THEN
                        Running = 0
                    END IF
                    DrawEditor
                    DrawAIWindow

                CASE 2
                    Running = 0

                CASE ELSE
                    PendingAction = ACTNONE
                    DrawEditor
                    DrawAIWindow
            END SELECT
        ELSE
            Running = 0
        END IF
    END IF

    PollSerial

MainLoopContinue:
LOOP

IF AiSerialIsOpen% THEN CALL AiSerialClose
END

'=========================================================
' Module-level save routine
'=========================================================
DoSave:
    DIM saveMode%

    LastSaveOK = 0

    ' If there is no current file name yet, this save must behave like Save As.
    IF FileName$ = "" THEN
        saveMode% = 2
    ELSE
        ' Existing named file: let the user choose Save or Save As.
        saveMode% = ConfirmSaveMode%
    END IF

    IF saveMode% = 0 THEN
        AppendChat "AI> Save cancelled."
        RedrawScreen
        RETURN
    END IF

    IF saveMode% = 1 THEN
        ' Standard Save: write back to the current file name.
        WorkName$ = FileName$
    ELSE
        ' Save As: ask for a new file name.
        WorkName$ = GetPopupInput$("Save File", "Save as: ", 30)

        IF WorkName$ = "" THEN
            AppendChat "AI> Save cancelled."
            RedrawScreen
            RETURN
        END IF
    END IF

    OPEN WorkName$ FOR OUTPUT AS #2

    FOR i = 1 TO TotalLines
        PRINT #2, Doc$(i)
    NEXT i

    CLOSE #2

    FileName$ = WorkName$
    IsModified = 0
    LastSaveOK = 1

    IF saveMode% = 1 THEN
        AppendChat "AI> Saved: " + FileName$
    ELSE
        AppendChat "AI> Saved As: " + FileName$
    END IF

    RedrawScreen
    RETURN

'=========================================================
' Module-level load routine
'=========================================================
DoLoad:
    IF IsModified <> 0 THEN
        IF ConfirmPopup%("Unsaved Changes", "Current file has unsaved work.", "Load another file anyway?") = 0 THEN
            AppendChat "AI> Load cancelled."
            RedrawScreen
            RETURN
        END IF
    END IF

    WorkName$ = GetPopupInput$("Load File", "Load file: ", 30)

    IF WorkName$ = "" THEN
        AppendChat "AI> Load cancelled."
        RedrawScreen
        RETURN
    END IF

    OPEN WorkName$ FOR INPUT AS #2

    FOR i = 1 TO MAXLINES
        Doc$(i) = ""
        SoftBreak(i) = 0
    NEXT i

    TotalLines = 0

    DO WHILE NOT EOF(2)
        LINE INPUT #2, WorkLine$
        IF TotalLines < MAXLINES THEN
            TotalLines = TotalLines + 1
            Doc$(TotalLines) = LEFT$(WorkLine$, maxlen)
        END IF
    LOOP

    CLOSE #2

    IF TotalLines = 0 THEN
        TotalLines = 1
        Doc$(1) = ""
    END IF

    CurLine = 1
    CurCol = 1
    TopLine = 1
    FileName$ = WorkName$
    IsModified = 0
    InitSelection

    AppendChat "AI> Loaded: " + FileName$
    RedrawScreen
    RETURN

'=========================================================
' Module-level error handler
'=========================================================
FileError:
    IF CurrentAction = ACTLOAD THEN
        CLOSE #2
        AppendChat "AI> Load failed. Error " + LTRIM$(STR$(ERR))
        RESUME LoadRecover

    ELSEIF CurrentAction = ACTSAVE THEN
        CLOSE #2
        AppendChat "AI> Save failed. Error " + LTRIM$(STR$(ERR))
        LastSaveOK = 0
        RESUME SaveRecover

    ELSEIF CurrentAction = ACTOPEN THEN
        SPortOpen% = 0
        SLastErrorCode% = ERR
        SLastError$ = "Serial open failed"
        AppendChat "AI> Serial open failed. Error " + LTRIM$(STR$(ERR))
        RESUME SerialRecover

    ELSEIF CurrentAction = ACTASKAI THEN
        AppendChat "AI> Ask AI failed. Error " + LTRIM$(STR$(ERR))
        RESUME SerialRecover

    ELSEIF CurrentAction = ACTSENDSEL THEN
        AppendChat "AI> Send selection failed. Error " + LTRIM$(STR$(ERR))
        RESUME SerialRecover

    ELSE
        CLOSE
        IF AiSerialIsOpen% THEN CALL AiSerialClose
        AppendChat "AI> Unhandled error " + LTRIM$(STR$(ERR))
        Running = 0
        RESUME ProgramAbort
    END IF

SerialRecover:
    CurrentAction = ACTNONE
    PendingAction = ACTNONE
    RedrawScreen
    RESUME MainLoopContinue

SaveRecover:
    CurrentAction = ACTNONE
    PendingAction = ACTNONE
    RedrawScreen
    RESUME NEXT

LoadRecover:
    CurrentAction = ACTNONE
    PendingAction = ACTNONE
    RedrawScreen
    RESUME NEXT

ProgramAbort:
    IF AiSerialIsOpen% THEN CALL AiSerialClose
    END

'=========================================================
' Placeholder AI menu action
'=========================================================

'=========================================================
' Serial / AI support
'
' These routines use SERIAL.BAS/SERIAL.BI.  They are kept
' separate from the core editor routines to avoid disturbing
' stable editing behavior.
'=========================================================


'=========================================================
' Built-in serial routines extracted from IBMTERM.BAS
'=========================================================
'==============================================================
'  SERIAL_IBMTERM_MATCH.BAS
'  QuickBASIC 4.x reusable COM1 module
'
'  Purpose:
'  --------
'  Reusable serial-port routines extracted from the working
'  IBMTERM.BAS terminal program.
'
'  This version intentionally matches the proven IBMTERM.BAS
'  serial behavior:
'
'      OPEN "COM1:1200,N,8,1,CS0,DS0,CD0" FOR RANDOM AS #1 LEN = 1
'      PRINT #1, Msg$       ' sends message with CR/LF
'      IF LOC(1) > 0 THEN LINE INPUT #1, Msg$
'
'  It contains no terminal screen code.  The calling program owns
'  the UI and any module-level ON ERROR handler.
'==============================================================




' Initialize the serial module defaults when this module loads.

'--------------------------------------------------------------
' Build the exact COM open string style used by IBMTERM.BAS.
' CS0, DS0, and CD0 avoid waiting on modem-control lines.
'--------------------------------------------------------------
SUB AiSerialBuildOpenSpec
    SOpenSpec$ = SPortName$ + ":" + SBaud$ + "," + SParity$ + "," + SDataBits$ + "," + SStopBits$ + ",CS0,DS0,CD0"
END SUB

'--------------------------------------------------------------
' Return number of waiting serial characters, matching IBMTERM's
' LOC(1) test but using the module's file number.
'--------------------------------------------------------------
FUNCTION AiSerialCharsWaiting%
    IF SPortOpen% THEN
        AiSerialCharsWaiting% = LOC(SPortFileNum%)
    ELSE
        AiSerialCharsWaiting% = 0
    END IF
END FUNCTION

'--------------------------------------------------------------
' Clear saved module error text.  The module itself does not use
' ON ERROR; the main program should own error trapping.
'--------------------------------------------------------------
SUB AiSerialClearError
    SLastErrorCode% = 0
    SLastError$ = ""
END SUB

'--------------------------------------------------------------
' Close COM port if open.
'--------------------------------------------------------------
SUB AiSerialClose
    IF SPortOpen% THEN
        CLOSE #SPortFileNum%
        SPortOpen% = 0
    END IF
END SUB

FUNCTION AiSerialErrorCode%
    AiSerialErrorCode% = SLastErrorCode%
END FUNCTION

FUNCTION AiSerialErrorText$
    AiSerialErrorText$ = SLastError$
END FUNCTION

FUNCTION AiSerialIsOpen%
    AiSerialIsOpen% = SPortOpen%
END FUNCTION

'--------------------------------------------------------------
' Open the serial port.
'
' This matches IBMTERM.BAS:
'   OPEN COM1:1200,N,8,1,CS0,DS0,CD0 FOR RANDOM AS #1 LEN = 1
'
' The caller should wrap this with a module-level ON ERROR handler
' if it wants graceful failure handling.
'--------------------------------------------------------------
SUB AiSerialOpen
    IF SPortOpen% THEN EXIT SUB

    CALL AiSerialClearError
    CALL AiSerialBuildOpenSpec

    ' Match IBMTERM.BAS exactly: COM1:1200,N,8,1,CS0,DS0,CD0
    ' opened FOR RANDOM AS #1 LEN = 1.  The module-level error
    ' handler catches any OPEN failure and skips the success path.
    SPortFileNum% = 1
    OPEN SOpenSpec$ FOR RANDOM AS #SPortFileNum% LEN = 1
    SPortOpen% = -1
END SUB

FUNCTION AiSerialOpenSpec$
    CALL AiSerialBuildOpenSpec
    AiSerialOpenSpec$ = SOpenSpec$
END FUNCTION

FUNCTION AiSerialPortName$
    AiSerialPortName$ = SPortName$
END FUNCTION

'--------------------------------------------------------------
' Read a single character if one is waiting.
' IBMTERM.BAS does not use this, but it is useful for future
' protocol work.  It does not affect line-based operation.
'--------------------------------------------------------------
FUNCTION AiSerialReadChar$
    DIM C$

    C$ = ""
    IF SPortOpen% THEN
        IF LOC(SPortFileNum%) > 0 THEN
            C$ = INPUT$(1, #SPortFileNum%)
        END IF
    END IF

    AiSerialReadChar$ = C$
END FUNCTION

'--------------------------------------------------------------
' Read one incoming line, matching IBMTERM.BAS:
'   IF LOC(1) > 0 THEN LINE INPUT #1, Msg$
'
' Because AiSerialSendLine uses PRINT #, received lines are expected
' to be terminated by CR/LF.
'--------------------------------------------------------------
FUNCTION AiSerialReadLine$
    DIM Msg$

    Msg$ = ""
    IF SPortOpen% THEN
        IF LOC(SPortFileNum%) > 0 THEN
            LINE INPUT #SPortFileNum%, Msg$
        END IF
    END IF

    AiSerialReadLine$ = Msg$
END FUNCTION

'--------------------------------------------------------------
' Send one line, matching IBMTERM.BAS:
'   PRINT #1, OutMsg$
'
' In QuickBASIC this appends CR/LF automatically.
'--------------------------------------------------------------
SUB AiSerialSendLine (Msg$)
    IF SPortOpen% THEN
        PRINT #SPortFileNum%, Msg$
    END IF
END SUB

'--------------------------------------------------------------
' Send raw text without adding CR/LF.  This is not used by
' IBMTERM.BAS, but is useful later for custom AI protocols.
'--------------------------------------------------------------
SUB AiSerialSendRaw (Msg$)
    IF SPortOpen% THEN
        PRINT #SPortFileNum%, Msg$;
    END IF
END SUB

'--------------------------------------------------------------
' Change COM configuration.  If port is open, close it first.
'--------------------------------------------------------------
SUB AiSerialSetConfig (PortName$, Baud$, Parity$, DataBits$, StopBits$)
    IF SPortOpen% THEN CALL AiSerialClose

    SPortName$ = UCASE$(PortName$)
    SBaud$ = Baud$
    SParity$ = UCASE$(Parity$)
    SDataBits$ = DataBits$
    SStopBits$ = StopBits$

    CALL AiSerialBuildOpenSpec
END SUB

'--------------------------------------------------------------
' Defaults taken directly from IBMTERM.BAS.
'--------------------------------------------------------------
SUB AiSerialUseDefaultConfig
    SPortName$ = "COM1"
    SBaud$ = "1200"
    SParity$ = "N"
    SDataBits$ = "8"
    SStopBits$ = "1"
    SPortOpen% = 0
    SPortFileNum% = 1
    CALL AiSerialClearError
    CALL AiSerialBuildOpenSpec
END SUB

'=========================================================
' Append a line to the AI message window
'=========================================================
SUB AppendChat (Msg$)
    DIM i

    IF ChatCount < MAXCHAT THEN
        ChatCount = ChatCount + 1
        Chat$(ChatCount) = Msg$
    ELSE
        FOR i = 1 TO MAXCHAT - 1
            Chat$(i) = Chat$(i + 1)
        NEXT i
        Chat$(MAXCHAT) = Msg$
    END IF

    IF ChatCount <= AIHEIGHT THEN
        ChatTop = 1
    ELSE
        ChatTop = ChatCount - AIHEIGHT + 1
    END IF
END SUB

' Add a long line to the AI pane by wrapping it across monitor rows.
SUB AppendChatWrapped (Msg$)
    DIM part$

    DO WHILE LEN(Msg$) > 76
        part$ = LEFT$(Msg$, 76)
        AppendChat part$
        Msg$ = "    " + MID$(Msg$, 77)
    LOOP

    IF Msg$ <> "" THEN AppendChat Msg$
END SUB

' Ask the AI a direct question without changing the document.
SUB AskAIQuestion
    DIM q$

    IF NOT AiSerialIsOpen% THEN
        AppendChat "AI> Port is closed. Press F6 to open COM1."
        EXIT SUB
    END IF

    q$ = GetAskInput$("Ask AI", 240)

    IF q$ = "" THEN
        AppendChat "AI> Ask AI cancelled."
        EXIT SUB
    END IF

    CALL AiSerialSendLine(q$)
    AppendChatWrapped "TX> " + q$
END SUB

'=========================================================
' Handle Backspace
'
' Cases:
'   1. Backspace within a line
'      - delete character to left of cursor
'      - try to pull text up from following wrapped lines
'
'   2. Backspace at column 1
'      - join current line to previous line
'      - re-wrap downward so nothing is lost
'      - then do forward reflow to tighten paragraph layout
'=========================================================
SUB BackspaceKey
    DIM s$
    DIM i
    DIM joinPos

    IF CurCol > 1 THEN
        s$ = Doc$(CurLine)

        IF CurCol <= LEN(s$) + 1 THEN
            Doc$(CurLine) = LEFT$(s$, CurCol - 2) + MID$(s$, CurCol)
        ELSE
            IF LEN(s$) > 0 THEN
                Doc$(CurLine) = LEFT$(s$, LEN(s$) - 1)
            END IF
        END IF

        CurCol = CurCol - 1

        ' Only pull text up from the next line when this line flowed into
        ' the next line because of automatic wrapping. Never cross a hard
        ' return that the user created with Enter.
        IF SoftBreak(CurLine) <> 0 THEN
            ReflowForwardFromLine CurLine
        END IF

        IsModified = 1
        DrawMenuBar
        EXIT SUB
    END IF

    IF CurLine > 1 THEN
        joinPos = LEN(Doc$(CurLine - 1))

        IF Doc$(CurLine - 1) <> "" AND Doc$(CurLine) <> "" THEN
            Doc$(CurLine - 1) = Doc$(CurLine - 1) + " " + Doc$(CurLine)
            CurCol = joinPos + 2
        ELSE
            Doc$(CurLine - 1) = Doc$(CurLine - 1) + Doc$(CurLine)
            CurCol = joinPos + 1
        END IF

        ' The joined line inherits the continuation state that used to
        ' belong to the current line.
        SoftBreak(CurLine - 1) = SoftBreak(CurLine)

        FOR i = CurLine TO TotalLines - 1
            Doc$(i) = Doc$(i + 1)
            SoftBreak(i) = SoftBreak(i + 1)
        NEXT i

        Doc$(TotalLines) = ""
        SoftBreak(TotalLines) = 0
        TotalLines = TotalLines - 1
        CurLine = CurLine - 1

        RewrapFromLine CurLine
        ReflowForwardFromLine CurLine

        IsModified = 1
        DrawMenuBar
    END IF
END SUB

SUB BeginSelection
    IF SelActive = 0 THEN
        SelActive = -1
        SelAnchorLine = CurLine
        SelAnchorCol = CurCol
    END IF
END SUB

SUB ClearAskBox (toprow%, leftcol%, boxw%, boxh%)
    DIM r%

    COLOR 7, 0
    FOR r% = toprow% TO toprow% + boxh% - 1
        LOCATE r%, leftcol%
        PRINT SPACE$(boxw%);
    NEXT r%
END SUB

'=========================================================
' Clear popup area
'=========================================================
SUB ClearPopupBox (toprow%, leftcol%, boxw%)
    DIM r%

    FOR r% = toprow% TO toprow% + 4
        LOCATE r%, leftcol%
        PRINT SPACE$(boxw%);
    NEXT r%
END SUB

SUB ClearSelection
    SelActive = 0
    SelAnchorLine = CurLine
    SelAnchorCol = CurCol
END SUB

SUB CloseSerialPort
    IF AiSerialIsOpen% THEN
        CALL AiSerialClose
        RxBuffer$ = ""
        AppendChat "AI> Port closed."
    ELSE
        AppendChat "AI> Port is already closed."
    END IF
END SUB

'=========================================================
' Popup: yes / no confirmation
' Returns 1 for Yes, 0 for No/Esc
'=========================================================
FUNCTION ConfirmPopup% (title$, line1$, line2$)
    DIM k$
    DIM a%
    DIM boxw%
    DIM boxl%
    DIM boxt%

    boxw% = 44
    boxl% = 19
    boxt% = 10

    DrawConfirmBox boxt%, boxl%, boxw%, title$, line1$, line2$

    DO
        k$ = INKEY$
        IF k$ <> "" THEN
            IF LEN(k$) = 1 THEN
                a% = ASC(UCASE$(k$))
                SELECT CASE a%
                    CASE 89
                        ConfirmPopup% = 1
                        EXIT DO
                    CASE 78, 27
                        ConfirmPopup% = 0
                        EXIT DO
                END SELECT
            END IF
        END IF
    LOOP

    ClearPopupBox boxt%, boxl%, boxw%
    RedrawScreen
END FUNCTION

'=========================================================
' Popup: save before exit
' Returns 1 = Save, 2 = Discard, 0 = Cancel
'=========================================================
FUNCTION ConfirmSaveExit%
    DIM k$
    DIM a%
    DIM boxw%
    DIM boxl%
    DIM boxt%

    boxw% = 46
    boxl% = 18
    boxt% = 10

    DrawSaveExitBox boxt%, boxl%, boxw%

    DO
        k$ = INKEY$
        IF k$ <> "" THEN
            IF LEN(k$) = 1 THEN
                a% = ASC(UCASE$(k$))
                SELECT CASE a%
                    CASE 89
                        ConfirmSaveExit% = 1
                        EXIT DO
                    CASE 78
                        ConfirmSaveExit% = 2
                        EXIT DO
                    CASE 27
                        ConfirmSaveExit% = 0
                        EXIT DO
                END SELECT
            END IF
        END IF
    LOOP

    ClearPopupBox boxt%, boxl%, boxw%
    RedrawScreen
END FUNCTION

'=========================================================
' Popup: choose between Save and Save As
' Returns 1 = Save, 2 = Save As, 0 = Cancel
'=========================================================
FUNCTION ConfirmSaveMode%
    DIM k$
    DIM a%
    DIM boxw%
    DIM boxl%
    DIM boxt%

    boxw% = 48
    boxl% = 17
    boxt% = 10

    DrawSaveModeBox boxt%, boxl%, boxw%

    DO
        k$ = INKEY$
        IF k$ <> "" THEN
            IF LEN(k$) = 1 THEN
                a% = ASC(UCASE$(k$))
                SELECT CASE a%
                    CASE 83
                        ConfirmSaveMode% = 1
                        EXIT DO
                    CASE 65
                        ConfirmSaveMode% = 2
                        EXIT DO
                    CASE 27
                        ConfirmSaveMode% = 0
                        EXIT DO
                END SELECT
            END IF
        END IF
    LOOP

    ClearPopupBox boxt%, boxl%, boxw%
    RedrawScreen
END FUNCTION

' Copy the current highlighted selection into the internal clipboard.
SUB CopySelectionToClipboard
    DIM startLine%
    DIM startCol%
    DIM endLine%
    DIM endCol%
    DIM i%

    IF HasSelection% = 0 THEN
        AppendChat "AI> No selection to copy."
        EXIT SUB
    END IF

    GetSelectionBounds startLine%, startCol%, endLine%, endCol%

    ClipText$ = ""

    IF startLine% = endLine% THEN
        ClipText$ = MID$(Doc$(startLine%), startCol%, endCol% - startCol%)
    ELSE
        ClipText$ = MID$(Doc$(startLine%), startCol%)

        FOR i% = startLine% + 1 TO endLine% - 1
            ClipText$ = ClipText$ + CHR$(10) + Doc$(i%)
        NEXT i%

        ClipText$ = ClipText$ + CHR$(10) + LEFT$(Doc$(endLine%), endCol% - 1)
    END IF

    ClipHasText = -1
    AppendChat "AI> Selection copied."
END SUB

' Cut = copy the selection, then delete it from the document.
SUB CutSelection
    DIM startLine%
    DIM startCol%
    DIM endLine%
    DIM endCol%
    DIM i%

    IF HasSelection% = 0 THEN
        AppendChat "AI> No selection to cut."
        EXIT SUB
    END IF

    GetSelectionBounds startLine%, startCol%, endLine%, endCol%

    ClipText$ = ""

    IF startLine% = endLine% THEN
        ClipText$ = MID$(Doc$(startLine%), startCol%, endCol% - startCol%)
    ELSE
        ClipText$ = MID$(Doc$(startLine%), startCol%)

        FOR i% = startLine% + 1 TO endLine% - 1
            ClipText$ = ClipText$ + CHR$(10) + Doc$(i%)
        NEXT i%

        ClipText$ = ClipText$ + CHR$(10) + LEFT$(Doc$(endLine%), endCol% - 1)
    END IF

    ClipHasText = -1
    DeleteSelection
    AppendChat "AI> Selection cut."
END SUB

' Delete the current highlighted selection and reflow the text.
' This is also used by the Delete key when a selection is active.
SUB DeleteSelection
    DIM startLine%
    DIM startCol%
    DIM endLine%
    DIM endCol%
    DIM i%
    DIM removeCount%
    DIM prefix$
    DIM suffix$
    DIM tailBreak%

    IF HasSelection% = 0 THEN EXIT SUB

    GetSelectionBounds startLine%, startCol%, endLine%, endCol%

    IF startLine% = endLine% THEN
        Doc$(startLine%) = LEFT$(Doc$(startLine%), startCol% - 1) + MID$(Doc$(startLine%), endCol%)
    ELSE
        prefix$ = LEFT$(Doc$(startLine%), startCol% - 1)
        suffix$ = MID$(Doc$(endLine%), endCol%)
        tailBreak% = SoftBreak(endLine%)
        Doc$(startLine%) = prefix$ + suffix$
        SoftBreak(startLine%) = tailBreak%

        removeCount% = endLine% - startLine%

        FOR i% = startLine% + 1 TO TotalLines - removeCount%
            Doc$(i%) = Doc$(i% + removeCount%)
            SoftBreak(i%) = SoftBreak(i% + removeCount%)
        NEXT i%

        FOR i% = TotalLines - removeCount% + 1 TO TotalLines
            Doc$(i%) = ""
            SoftBreak(i%) = 0
        NEXT i%

        TotalLines = TotalLines - removeCount%
    END IF

    IF TotalLines < 1 THEN
        TotalLines = 1
        Doc$(1) = ""
        SoftBreak(1) = 0
    END IF

    CurLine = startLine%
    CurCol = startCol%

    IF LEN(Doc$(CurLine)) > 80 THEN
        RewrapFromLine CurLine
    END IF
    ReflowForwardFromLine CurLine

    ClearSelection
    IsModified = 1
    DrawMenuBar
END SUB

'=========================================================
' Draw AI pane contents
'=========================================================
SUB DrawAIWindow
    DIM screenRow
    DIM chatIndex

    COLOR 7, 0

    FOR screenRow = AITOP TO AIBOTTOM
        LOCATE screenRow, 1
        PRINT SPACE$(80);
    NEXT screenRow

    chatIndex = ChatTop

    FOR screenRow = AITOP TO AIBOTTOM
        IF chatIndex <= ChatCount THEN
            LOCATE screenRow, 1
            PRINT LEFT$(Chat$(chatIndex) + SPACE$(80), 80);
        END IF
        chatIndex = chatIndex + 1
    NEXT screenRow
END SUB

' Draw a four-line question popup for Ask AI.
SUB DrawAskBox (toprow%, leftcol%, boxw%, boxh%, title$, value$)
    DIM r%
    DIM textw%
    DIM i%
    DIM part$
    DIM askLen%
    DIM cursorRow%
    DIM cursorCol%

    textw% = boxw% - 4

    COLOR 0, 7

    LOCATE toprow%, leftcol%
    PRINT CHR$(218); STRING$(boxw% - 2, 196); CHR$(191);

    FOR r% = toprow% + 1 TO toprow% + boxh% - 2
        LOCATE r%, leftcol%
        PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);
    NEXT r%

    LOCATE toprow% + boxh% - 1, leftcol%
    PRINT CHR$(192); STRING$(boxw% - 2, 196); CHR$(217);

    LOCATE toprow%, leftcol% + 2
    PRINT LEFT$(title$, boxw% - 4);

    LOCATE toprow% + 1, leftcol% + 2
    PRINT "Question:";

    LOCATE toprow% + 6, leftcol% + 2
    PRINT "Enter=Send   Esc=Cancel   Backspace=Edit";

    FOR i% = 0 TO 3
        part$ = MID$(value$, i% * textw% + 1, textw%)
        LOCATE toprow% + 2 + i%, leftcol% + 2
        PRINT LEFT$(part$ + SPACE$(textw%), textw%);
    NEXT i%

    askLen% = LEN(value$)
    cursorRow% = askLen% \ textw%
    cursorCol% = askLen% MOD textw%
    IF cursorRow% > 3 THEN cursorRow% = 3

    LOCATE toprow% + 2 + cursorRow%, leftcol% + 2 + cursorCol%, 1
    COLOR 7, 0
END SUB

'=========================================================
' Draw generic confirmation popup
'=========================================================
SUB DrawConfirmBox (toprow%, leftcol%, boxw%, title$, line1$, line2$)
    COLOR 0, 7

    LOCATE toprow%, leftcol%
    PRINT CHR$(218); STRING$(boxw% - 2, 196); CHR$(191);

    LOCATE toprow% + 1, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 2, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 3, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 4, leftcol%
    PRINT CHR$(192); STRING$(boxw% - 2, 196); CHR$(217);

    LOCATE toprow%, leftcol% + 2
    PRINT LEFT$(title$, boxw% - 4);

    LOCATE toprow% + 1, leftcol% + 2
    PRINT LEFT$(line1$ + SPACE$(boxw% - 4), boxw% - 4);

    LOCATE toprow% + 2, leftcol% + 2
    PRINT LEFT$(line2$ + SPACE$(boxw% - 4), boxw% - 4);

    LOCATE toprow% + 3, leftcol% + 2
    PRINT "Press Y for Yes, N for No";

    COLOR 7, 0
END SUB

'=========================================================
' Draw visible document lines.
'
' Selection is rendered in reverse video. To keep redraw smooth,
' each line is printed in at most three chunks:
'   normal text before selection
'   selected text
'   normal text after selection
'=========================================================
SUB DrawEditor
    DIM screenRow
    DIM docIndex
    DIM s$
    DIM lineLen
    DIM hasSel
    DIM selStartLine
    DIM selStartCol
    DIM selEndLine
    DIM selEndCol
    DIM startCol
    DIM endCol
    DIM visStart
    DIM visEnd

    hasSel = 0
    IF SelActive <> 0 THEN
        IF SelAnchorLine <> CurLine OR SelAnchorCol <> CurCol THEN
            hasSel = -1
            IF IsPositionBefore%(SelAnchorLine, SelAnchorCol, CurLine, CurCol) THEN
                selStartLine = SelAnchorLine
                selStartCol = SelAnchorCol
                selEndLine = CurLine
                selEndCol = CurCol
            ELSE
                selStartLine = CurLine
                selStartCol = CurCol
                selEndLine = SelAnchorLine
                selEndCol = SelAnchorCol
            END IF
        END IF
    END IF

    FOR screenRow = EDITTOP TO EDITBOTTOM
        docIndex = TopLine + (screenRow - EDITTOP)

        LOCATE screenRow, 1
        COLOR 7, 0
        PRINT SPACE$(80);

        IF docIndex <= TotalLines THEN
            s$ = LEFT$(Doc$(docIndex) + SPACE$(80), 80)
            lineLen = LEN(Doc$(docIndex))
            IF lineLen > 80 THEN lineLen = 80

            IF hasSel = 0 OR docIndex < selStartLine OR docIndex > selEndLine THEN
                LOCATE screenRow, 1
                COLOR 7, 0
                PRINT s$;
            ELSE
                startCol = 1
                endCol = lineLen + 1

                IF docIndex = selStartLine THEN startCol = selStartCol
                IF docIndex = selEndLine THEN endCol = selEndCol

                IF startCol < 1 THEN startCol = 1
                IF endCol < 1 THEN endCol = 1
                IF startCol > 81 THEN startCol = 81
                IF endCol > 81 THEN endCol = 81

                visStart = startCol
                visEnd = endCol - 1

                IF visStart > lineLen OR visStart > visEnd THEN
                    LOCATE screenRow, 1
                    COLOR 7, 0
                    PRINT s$;
                ELSE
                    IF visEnd > 80 THEN visEnd = 80

                    LOCATE screenRow, 1
                    COLOR 7, 0
                    IF visStart > 1 THEN
                        PRINT LEFT$(s$, visStart - 1);
                    END IF

                    COLOR 0, 7
                    PRINT MID$(s$, visStart, visEnd - visStart + 1);

                    COLOR 7, 0
                    IF visEnd < 80 THEN
                        PRINT MID$(s$, visEnd + 1);
                    END IF
                END IF
            END IF
        END IF
    NEXT screenRow

    COLOR 7, 0
END SUB

'=========================================================
' Draw static frame elements
'=========================================================
SUB DrawFrame
    CLS
    DrawMenuBar
    DrawSeparator
END SUB

'=========================================================
' Draw top menu bar with current file name and dirty marker
'=========================================================
SUB DrawMenuBar
    DIM rightText$
    DIM displayName$
    DIM portText$

    COLOR 0, 7
    LOCATE 1, 1
    PRINT SPACE$(80);

    ' These labels now match active function-key areas:
    ' File=Save/Load, Serial=Open/Close, AI=Ask/Send selection.
    LOCATE 1, 1
    PRINT " ";

    IF FileName$ = "" THEN
        displayName$ = "Untitled"
    ELSE
        displayName$ = FileName$
        IF LEN(displayName$) > 16 THEN displayName$ = LEFT$(displayName$, 16)
    END IF

    IF IsModified <> 0 THEN displayName$ = displayName$ + "*"

    portText$ = PortState$
    rightText$ = "[" + displayName$ + "] [" + portText$ + "]"
    IF LEN(rightText$) > 36 THEN rightText$ = RIGHT$(rightText$, 36)

    LOCATE 1, 81 - LEN(rightText$)
    PRINT rightText$;

    COLOR 7, 0
END SUB

'=========================================================
' Draw popup input box
'=========================================================
SUB DrawPopupBox (toprow%, leftcol%, boxw%, title$, prompt$, value$)
    DIM promptrow%
    DIM s$

    promptrow% = toprow% + 1

    COLOR 0, 7

    LOCATE toprow%, leftcol%
    PRINT CHR$(218); STRING$(boxw% - 2, 196); CHR$(191);

    LOCATE toprow% + 1, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 2, leftcol%
    PRINT CHR$(192); STRING$(boxw% - 2, 196); CHR$(217);

    LOCATE toprow%, leftcol% + 2
    PRINT LEFT$(title$, boxw% - 4);

    s$ = prompt$ + value$
    LOCATE promptrow%, leftcol% + 2
    PRINT LEFT$(s$ + SPACE$(boxw% - 4), boxw% - 4);

    LOCATE promptrow%, leftcol% + 2 + LEN(prompt$) + LEN(value$), 1
    COLOR 7, 0
END SUB

'=========================================================
' Draw save-on-exit popup
'=========================================================
SUB DrawSaveExitBox (toprow%, leftcol%, boxw%)
    COLOR 0, 7

    LOCATE toprow%, leftcol%
    PRINT CHR$(218); STRING$(boxw% - 2, 196); CHR$(191);

    LOCATE toprow% + 1, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 2, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 3, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 4, leftcol%
    PRINT CHR$(192); STRING$(boxw% - 2, 196); CHR$(217);

    LOCATE toprow%, leftcol% + 2
    PRINT "Unsaved Changes";

    LOCATE toprow% + 1, leftcol% + 2
    PRINT "Save changes before closing?";

    LOCATE toprow% + 2, leftcol% + 2
    PRINT "Y=Save   N=Discard   Esc=Cancel";

    COLOR 7, 0
END SUB

'=========================================================
' Draw save / save-as choice popup
'=========================================================
SUB DrawSaveModeBox (toprow%, leftcol%, boxw%)
    COLOR 0, 7

    LOCATE toprow%, leftcol%
    PRINT CHR$(218); STRING$(boxw% - 2, 196); CHR$(191);

    LOCATE toprow% + 1, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 2, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 3, leftcol%
    PRINT CHR$(179); SPACE$(boxw% - 2); CHR$(179);

    LOCATE toprow% + 4, leftcol%
    PRINT CHR$(192); STRING$(boxw% - 2, 196); CHR$(217);

    LOCATE toprow%, leftcol% + 2
    PRINT "Save Options";

    LOCATE toprow% + 1, leftcol% + 2
    PRINT "Current file: " + LEFT$(FileName$, boxw% - 18)

    LOCATE toprow% + 2, leftcol% + 2
    PRINT "S=Save   A=Save As   Esc=Cancel"

    COLOR 7, 0
END SUB

'=========================================================
' Draw separator above AI pane
'=========================================================
SUB DrawSeparator
    COLOR 7, 0
    LOCATE SEPROW, 1
    PRINT STRING$(80, 196);
    LOCATE SEPROW, 3
    PRINT "AI Assistant";
END SUB

'=========================================================
' Draw bottom command/help line
'=========================================================
SUB DrawStatus (Msg$)
    COLOR 0, 7
    LOCATE STATUSROW, 1
    PRINT SPACE$(80);
    LOCATE STATUSROW, 1
    PRINT LEFT$(Msg$ + SPACE$(80), 80);
    COLOR 7, 0
END SUB

'=========================================================
' Keep cursor within visible viewport
'=========================================================
SUB EnsureCursorVisible
    IF CurLine < TopLine THEN TopLine = CurLine
    IF CurLine > TopLine + EDITHEIGHT - 1 THEN
        TopLine = CurLine - EDITHEIGHT + 1
    END IF
    IF TopLine < 1 THEN TopLine = 1
END SUB

'=========================================================
' Handle Enter key (hard line split)
'=========================================================
SUB EnterKey
    DIM s$
    DIM leftPart$
    DIM rightPart$
    DIM i
    DIM oldBreak

    IF TotalLines >= MAXLINES THEN
        AppendChat "AI> Maximum lines reached."
        EXIT SUB
    END IF

    s$ = Doc$(CurLine)
    leftPart$ = LEFT$(s$, CurCol - 1)
    rightPart$ = MID$(s$, CurCol)
    oldBreak = SoftBreak(CurLine)

    FOR i = TotalLines + 1 TO CurLine + 2 STEP -1
        Doc$(i) = Doc$(i - 1)
        SoftBreak(i) = SoftBreak(i - 1)
    NEXT i

    Doc$(CurLine) = leftPart$
    Doc$(CurLine + 1) = rightPart$
    SoftBreak(CurLine) = 0
    SoftBreak(CurLine + 1) = oldBreak
    TotalLines = TotalLines + 1
    CurLine = CurLine + 1
    CurCol = 1

    IsModified = 1
    DrawMenuBar
END SUB

'=========================================================
' Pull as many whole words as possible from the next line up to
' the current line, without exceeding 80 columns.
'=========================================================
SUB FillLineFromNext (lineNum%)
    DIM nextText$
    DIM firstWord$
    DIM p%
    DIM addText$

    IF lineNum% < 1 OR lineNum% >= TotalLines THEN EXIT SUB
    IF SoftBreak(lineNum%) = 0 THEN EXIT SUB
    IF Doc$(lineNum% + 1) = "" THEN EXIT SUB

    DO
        nextText$ = Doc$(lineNum% + 1)

        DO WHILE LEN(nextText$) > 0 AND LEFT$(nextText$, 1) = " "
            nextText$ = MID$(nextText$, 2)
        LOOP

        IF nextText$ = "" THEN EXIT DO

        p% = INSTR(nextText$, " ")
        IF p% = 0 THEN
            firstWord$ = nextText$
        ELSE
            firstWord$ = LEFT$(nextText$, p% - 1)
        END IF

        IF Doc$(lineNum%) = "" THEN
            addText$ = firstWord$
        ELSE
            addText$ = " " + firstWord$
        END IF

        IF LEN(Doc$(lineNum%)) + LEN(addText$) > 80 THEN EXIT DO

        Doc$(lineNum%) = Doc$(lineNum%) + addText$

        IF p% = 0 THEN
            Doc$(lineNum% + 1) = ""
        ELSE
            Doc$(lineNum% + 1) = MID$(nextText$, p% + 1)
        END IF
    LOOP
END SUB

'=========================================================
' Flush any buffered keys
'=========================================================
SUB FlushKeys
    DO WHILE INKEY$ <> ""
    LOOP
END SUB

FUNCTION GetAskInput$ (title$, maxlen%)
    DIM s$
    DIM k$
    DIM a%
    DIM boxw%
    DIM boxl%
    DIM boxt%
    DIM boxh%

    s$ = ""
    boxw% = 66
    boxl% = 8
    boxt% = 7
    boxh% = 8

    DrawAskBox boxt%, boxl%, boxw%, boxh%, title$, s$

    DO
        k$ = INKEY$

        IF k$ <> "" THEN
            IF LEN(k$) = 1 THEN
                a% = ASC(k$)

                SELECT CASE a%
                    CASE 13
                        EXIT DO
                    CASE 27
                        s$ = ""
                        EXIT DO
                    CASE 8
                        IF LEN(s$) > 0 THEN s$ = LEFT$(s$, LEN(s$) - 1)
                    CASE 32 TO 126
                        IF LEN(s$) < maxlen% THEN s$ = s$ + CHR$(a%)
                END SELECT

                DrawAskBox boxt%, boxl%, boxw%, boxh%, title$, s$
            END IF
        END IF
    LOOP

    ClearAskBox boxt%, boxl%, boxw%, boxh%
    RedrawScreen

    GetAskInput$ = s$
END FUNCTION

'=========================================================
' Popup line-input function
'=========================================================
FUNCTION GetPopupInput$ (title$, prompt$, maxlen%)
    DIM s$
    DIM k$
    DIM a%
    DIM boxw%
    DIM boxl%
    DIM boxt%

    s$ = ""
    boxw% = 40
    boxl% = 21
    boxt% = 10

    DrawPopupBox boxt%, boxl%, boxw%, title$, prompt$, s$

    DO
        k$ = INKEY$

        IF k$ <> "" THEN
            IF LEN(k$) = 1 THEN
                a% = ASC(k$)

                SELECT CASE a%
                    CASE 13
                        EXIT DO
                    CASE 27
                        s$ = ""
                        EXIT DO
                    CASE 8
                        IF LEN(s$) > 0 THEN
                            s$ = LEFT$(s$, LEN(s$) - 1)
                        END IF
                    CASE 32 TO 126
                        IF LEN(s$) < maxlen% THEN
                            s$ = s$ + CHR$(a%)
                        END IF
                END SELECT

                DrawPopupBox boxt%, boxl%, boxw%, title$, prompt$, s$
            END IF
        END IF
    LOOP

    ClearPopupBox boxt%, boxl%, boxw%
    RedrawScreen

    GetPopupInput$ = s$
END FUNCTION

SUB GetSelectionBounds (startLine%, startCol%, endLine%, endCol%)
    IF IsPositionBefore%(SelAnchorLine, SelAnchorCol, CurLine, CurCol) THEN
        startLine% = SelAnchorLine
        startCol% = SelAnchorCol
        endLine% = CurLine
        endCol% = CurCol
    ELSE
        startLine% = CurLine
        startCol% = CurCol
        endLine% = SelAnchorLine
        endCol% = SelAnchorCol
    END IF
END SUB

' Return selected text without changing the editor clipboard.
FUNCTION GetSelectionText$
    DIM startLine%
    DIM startCol%
    DIM endLine%
    DIM endCol%
    DIM i%
    DIM result$

    result$ = ""

    IF HasSelection% = 0 THEN
        GetSelectionText$ = ""
        EXIT FUNCTION
    END IF

    GetSelectionBounds startLine%, startCol%, endLine%, endCol%

    IF startLine% = endLine% THEN
        result$ = MID$(Doc$(startLine%), startCol%, endCol% - startCol%)
    ELSE
        result$ = MID$(Doc$(startLine%), startCol%)

        FOR i% = startLine% + 1 TO endLine% - 1
            result$ = result$ + " " + Doc$(i%)
        NEXT i%

        result$ = result$ + " " + LEFT$(Doc$(endLine%), endCol% - 1)
    END IF

    GetSelectionText$ = result$
END FUNCTION

'=========================================================
' Handle extended keys (arrows, function keys, etc.)
'
' Shift+Left and Shift+Right extend the current selection.
' Plain cursor movement clears any active selection first.
'=========================================================
SUB HandleExtendedKey (k%)
    SELECT CASE k%
        CASE 72
            ClearSelection
            MoveUp
        CASE 80
            ClearSelection
            MoveDown
        CASE 75
            IF IsShiftDown% THEN
                BeginSelection
                MoveLeft
            ELSE
                ClearSelection
                MoveLeft
            END IF
        CASE 77
            IF IsShiftDown% THEN
                BeginSelection
                MoveRight
            ELSE
                ClearSelection
                MoveRight
            END IF
        CASE 71
            ClearSelection
            MoveHome
        CASE 79
            ClearSelection
            MoveEnd
        CASE 83
            IF HasSelection% <> 0 THEN
                DeleteSelection
                AppendChat "AI> Selection deleted."
            END IF
        CASE 60
            PendingAction = ACTSAVE
        CASE 61
            PendingAction = ACTLOAD
        CASE 62
            PendingAction = ACTASKAI
        CASE 63
            PendingAction = ACTSENDSEL
        CASE 64
            PendingAction = ACTOPEN
        CASE 65
            PendingAction = ACTCLOSE
    END SELECT
END SUB

'=========================================================
' Handle normal ASCII keys
'=========================================================
SUB HandleNormalKey (k%)
    SELECT CASE k%
        CASE 1
            SelectAll

        CASE 27
            PendingAction = ACTEXIT

        CASE 3
            CopySelectionToClipboard

        CASE 22
            PasteClipboard

        CASE 24
            CutSelection

        CASE 8
            ClearSelection
            BackspaceKey

        CASE 13
            ClearSelection
            EnterKey

        CASE 32 TO 126
            ClearSelection
            InsertCharacter CHR$(k%)
    END SELECT
END SUB

'=========================================================
' Clipboard and selection support
'
' Selection bounds are treated as:
'   start position = inclusive
'   end position   = exclusive
'
' This matches the way the editor already draws highlighted text.
'=========================================================
FUNCTION HasSelection%
    IF SelActive = 0 THEN
        HasSelection% = 0
    ELSEIF SelAnchorLine = CurLine AND SelAnchorCol = CurCol THEN
        HasSelection% = 0
    ELSE
        HasSelection% = -1
    END IF
END FUNCTION

'=========================================================
' Initialize editor state
'=========================================================
SUB InitEditor
    DIM i

    IsModified = 0
    LastSaveOK = 0

    FOR i = 1 TO MAXLINES
        Doc$(i) = ""
        SoftBreak(i) = 0
    NEXT i

    TotalLines = 1
    CurLine = 1
    CurCol = 1
    TopLine = 1
    FileName$ = ""

    ChatCount = 0
    ChatTop = 1

    InitSelection
    ClipText$ = ""
    ClipHasText = 0
END SUB

'=========================================================
' Selection helpers
'=========================================================
SUB InitSelection
    SelActive = 0
    SelAnchorLine = 1
    SelAnchorCol = 1
END SUB

'=========================================================
' Insert a printable character.
'
' Steps:
'   - Insert into current line at cursor
'   - Wrap if line grows past 80 columns
'   - Reflow forward so later wrapped lines stay packed
'=========================================================
SUB InsertCharacter (ch$)
    DIM s$
    DIM startLine

    s$ = Doc$(CurLine)

    IF LEN(s$) >= maxlen THEN
        AppendChat "AI> Line is full."
        EXIT SUB
    END IF

    IF CurCol > LEN(s$) + 1 THEN
        s$ = s$ + SPACE$(CurCol - LEN(s$) - 1)
    END IF

    startLine = CurLine

    Doc$(CurLine) = LEFT$(s$, CurCol - 1) + ch$ + MID$(s$, CurCol)
    CurCol = CurCol + 1

    IF LEN(Doc$(CurLine)) > 80 THEN
        WrapCurrentLine
    END IF

    ReflowForwardFromLine startLine

    IsModified = 1
    DrawMenuBar
END SUB

FUNCTION IsPositionBefore% (line1%, col1%, line2%, col2%)
    IF line1% < line2% THEN
        IsPositionBefore% = -1
    ELSEIF line1% > line2% THEN
        IsPositionBefore% = 0
    ELSEIF col1% <= col2% THEN
        IsPositionBefore% = -1
    ELSE
        IsPositionBefore% = 0
    END IF
END FUNCTION

FUNCTION IsShiftDown%
    DIM flags%
    DEF SEG = &H40
    flags% = PEEK(&H17)
    DEF SEG

    IF (flags% AND 3) <> 0 THEN
        IsShiftDown% = -1
    ELSE
        IsShiftDown% = 0
    END IF
END FUNCTION

'=========================================================
' Cursor movement routines
'=========================================================
SUB MoveDown
    IF CurLine < TotalLines THEN
        CurLine = CurLine + 1
        IF CurCol > LEN(Doc$(CurLine)) + 1 THEN CurCol = LEN(Doc$(CurLine)) + 1
    END IF
END SUB

SUB MoveEnd
    CurCol = LEN(Doc$(CurLine)) + 1
END SUB

SUB MoveHome
    CurCol = 1
END SUB

SUB MoveLeft
    IF CurCol > 1 THEN
        CurCol = CurCol - 1
    ELSEIF CurLine > 1 THEN
        CurLine = CurLine - 1
        CurCol = LEN(Doc$(CurLine)) + 1
    END IF
END SUB

SUB MoveRight
    IF CurCol <= LEN(Doc$(CurLine)) THEN
        CurCol = CurCol + 1
    ELSEIF CurLine < TotalLines THEN
        CurLine = CurLine + 1
        CurCol = 1
    END IF
END SUB

SUB MoveUp
    IF CurLine > 1 THEN
        CurLine = CurLine - 1
        IF CurCol > LEN(Doc$(CurLine)) + 1 THEN CurCol = LEN(Doc$(CurLine)) + 1
    END IF
END SUB

SUB OpenSerialPort
    IF AiSerialIsOpen% THEN
        AppendChat "AI> Port already open: " + AiSerialPortName$
        EXIT SUB
    END IF

    CALL AiSerialUseDefaultConfig
    CALL AiSerialOpen
    AppendChat "AI> Port opened: " + AiSerialOpenSpec$
END SUB

' Paste the internal clipboard at the cursor.
' If text is selected, the selection is replaced first.
SUB PasteClipboard
    DIM parts$(1 TO MAXLINES)
    DIM partCount%
    DIM src$
    DIM p%
    DIM i%
    DIM insertLine%
    DIM insertCol%
    DIM prefix$
    DIM suffix$
    DIM startLine%
    DIM startCol%
    DIM endLine%
    DIM endCol%
    DIM linesRemoved%
    DIM oldBreak%

    IF ClipHasText = 0 OR ClipText$ = "" THEN
        AppendChat "AI> Clipboard is empty."
        EXIT SUB
    END IF

    src$ = ClipText$
    partCount% = 0

    DO
        p% = INSTR(src$, CHR$(10))
        partCount% = partCount% + 1

        IF p% = 0 THEN
            parts$(partCount%) = src$
            EXIT DO
        ELSE
            parts$(partCount%) = LEFT$(src$, p% - 1)
            src$ = MID$(src$, p% + 1)
        END IF
    LOOP

    insertLine% = CurLine
    insertCol% = CurCol
    linesRemoved% = 0

    IF HasSelection% <> 0 THEN
        GetSelectionBounds startLine%, startCol%, endLine%, endCol%
        insertLine% = startLine%
        insertCol% = startCol%
        linesRemoved% = endLine% - startLine%
    END IF

    IF TotalLines - linesRemoved% + partCount% - 1 > MAXLINES THEN
        AppendChat "AI> Paste is too large for document."
        EXIT SUB
    END IF

    IF HasSelection% <> 0 THEN
        DeleteSelection
        insertLine% = CurLine
        insertCol% = CurCol
    END IF

    prefix$ = LEFT$(Doc$(insertLine%), insertCol% - 1)
    suffix$ = MID$(Doc$(insertLine%), insertCol%)
    oldBreak% = SoftBreak(insertLine%)

    IF partCount% = 1 THEN
        Doc$(insertLine%) = prefix$ + parts$(1) + suffix$
        CurLine = insertLine%
        CurCol = LEN(prefix$ + parts$(1)) + 1
    ELSE
        FOR i% = TotalLines TO insertLine% + 1 STEP -1
            Doc$(i% + partCount% - 1) = Doc$(i%)
            SoftBreak(i% + partCount% - 1) = SoftBreak(i%)
        NEXT i%

        TotalLines = TotalLines + partCount% - 1

        Doc$(insertLine%) = prefix$ + parts$(1)
        SoftBreak(insertLine%) = 0

        FOR i% = 2 TO partCount% - 1
            Doc$(insertLine% + i% - 1) = parts$(i%)
            SoftBreak(insertLine% + i% - 1) = 0
        NEXT i%

        Doc$(insertLine% + partCount% - 1) = parts$(partCount%) + suffix$
        SoftBreak(insertLine% + partCount% - 1) = oldBreak%

        CurLine = insertLine% + partCount% - 1
        CurCol = LEN(parts$(partCount%)) + 1
    END IF

    ' Re-wrap only from the insertion point downward.
    ' Do NOT run forward paragraph reflow here, because paste should not
    ' pull text up from an intentionally separate following paragraph.
    ' A blank line used as a paragraph separator must stay blank after paste.
    RewrapFromLine insertLine%

    ClearSelection
    IsModified = 1
    DrawMenuBar
    AppendChat "AI> Clipboard pasted."
END SUB

' Poll serial input without blocking the editor.
' Characters are collected until CR or LF is received.
SUB PollSerial
    DIM C$

    IF NOT AiSerialIsOpen% THEN EXIT SUB

    DO WHILE AiSerialCharsWaiting% > 0
        C$ = AiSerialReadChar$

        IF C$ <> "" THEN
            IF C$ = CHR$(13) OR C$ = CHR$(10) THEN
                IF RxBuffer$ <> "" THEN
                    AppendChatWrapped "RX> " + RxBuffer$
                    RxBuffer$ = ""
                    DrawAIWindow
                    PutCursor
                END IF
            ELSE
                IF LEN(RxBuffer$) < 240 THEN RxBuffer$ = RxBuffer$ + C$
            END IF
        ELSE
            EXIT DO
        END IF
    LOOP
END SUB

FUNCTION PortState$
    IF AiSerialIsOpen% THEN
        PortState$ = AiSerialPortName$ + " OPEN"
    ELSE
        PortState$ = "COM1 CLOSED"
    END IF
END FUNCTION

'=========================================================
' Place hardware cursor in editor window
'=========================================================
SUB PutCursor
    DIM screenRow
    DIM screenCol

    screenRow = EDITTOP + (CurLine - TopLine)
    screenCol = CurCol

    IF screenRow < EDITTOP OR screenRow > EDITBOTTOM THEN EXIT SUB
    IF screenCol < 1 THEN screenCol = 1
    IF screenCol > 80 THEN screenCol = 80

    LOCATE screenRow, screenCol, 1
END SUB

'=========================================================
' Full redraw helper
'=========================================================
SUB RedrawScreen
    DrawFrame
    DrawEditor
    DrawAIWindow
    DrawStatus "F2=Save F3=Load F4=Ask F5=SelAI F6=Open F7=Close ^A/^C/^X/^V Esc=Exit"
    PutCursor
END SUB

'=========================================================
' Reflow a paragraph forward starting at a line.
'
' Purpose:
'   - after insert/delete, try to keep wrapped paragraphs packed neatly
'   - preserve the user's cursor as much as possible
'=========================================================
SUB ReflowForwardFromLine (startLine%)
    DIM i
    DIM j
    DIM savedLine
    DIM savedCol
    DIM nextBreak

    savedLine = CurLine
    savedCol = CurCol

    i = startLine%

    DO WHILE i < TotalLines
        IF SoftBreak(i) <> 0 THEN
            FillLineFromNext i
        END IF

        IF SoftBreak(i) <> 0 AND Doc$(i + 1) = "" THEN
            nextBreak = SoftBreak(i + 1)

            FOR j = i + 1 TO TotalLines - 1
                Doc$(j) = Doc$(j + 1)
                SoftBreak(j) = SoftBreak(j + 1)
            NEXT j

            Doc$(TotalLines) = ""
            SoftBreak(TotalLines) = 0
            TotalLines = TotalLines - 1
            SoftBreak(i) = nextBreak

            IF savedLine > i + 1 THEN
                savedLine = savedLine - 1
            ELSEIF savedLine = i + 1 THEN
                savedLine = i
                IF savedCol > LEN(Doc$(savedLine)) + 1 THEN
                    savedCol = LEN(Doc$(savedLine)) + 1
                END IF
            END IF
        ELSE
            IF LEN(Doc$(i + 1)) > 80 THEN
                WrapLineAt i + 1

                IF savedLine > i + 1 THEN
                    savedLine = savedLine + 1
                END IF
            ELSE
                i = i + 1
            END IF
        END IF
    LOOP

    IF savedLine < 1 THEN savedLine = 1
    IF savedLine > TotalLines THEN savedLine = TotalLines
    IF savedCol < 1 THEN savedCol = 1
    IF savedCol > LEN(Doc$(savedLine)) + 1 THEN
        savedCol = LEN(Doc$(savedLine)) + 1
    END IF

    CurLine = savedLine
    CurCol = savedCol
END SUB

'=========================================================
' Re-wrap downward from a given line.
' This is mainly used after line joins caused by column-1 Backspace.
'=========================================================
SUB RewrapFromLine (startLine%)
    DIM i

    i = startLine%

    DO WHILE i <= TotalLines
        IF LEN(Doc$(i)) > 80 THEN
            WrapLineAt i
        ELSE
            i = i + 1
        END IF
    LOOP
END SUB

' Select the entire document so copy, cut, or delete can act on all text at once.
SUB SelectAll
    IF TotalLines < 1 THEN EXIT SUB

    SelActive = -1
    SelAnchorLine = 1
    SelAnchorCol = 1
    CurLine = TotalLines
    CurCol = LEN(Doc$(TotalLines)) + 1
    EnsureCursorVisible
END SUB

' Send only the highlighted text to the AI.
SUB SendSelectionToAI
    DIM Msg$

    IF NOT AiSerialIsOpen% THEN
        AppendChat "AI> Port is closed. Press F6 to open COM1."
        EXIT SUB
    END IF

    IF HasSelection% = 0 THEN
        AppendChat "AI> No text selected. Highlight text first."
        EXIT SUB
    END IF

    Msg$ = GetSelectionText$

    IF Msg$ = "" THEN
        AppendChat "AI> Selection is empty."
        EXIT SUB
    END IF

    CALL AiSerialSendLine(Msg$)
    AppendChatWrapped "TX> " + Msg$
END SUB

SUB ShowAIPlaceholder
    AppendChat "AI> Use F4 to ask AI, F5 to send selection."
    RedrawScreen
END SUB

'=========================================================
' Wrap the CURRENT line and move the cursor to the new line.
' Used while the user is actively typing.
'=========================================================
SUB WrapCurrentLine
    DIM breakPos
    DIM overflow$
    DIM i
    DIM s$
    DIM movedLen
    DIM oldBreak

    s$ = Doc$(CurLine)

    IF LEN(s$) <= 80 THEN EXIT SUB
    IF TotalLines >= MAXLINES THEN EXIT SUB

    breakPos = 80
    DO WHILE breakPos > 1
        IF MID$(s$, breakPos, 1) = " " THEN EXIT DO
        breakPos = breakPos - 1
    LOOP

    IF breakPos <= 1 THEN breakPos = 80

    overflow$ = MID$(s$, breakPos + 1)
    Doc$(CurLine) = LEFT$(s$, breakPos)

    IF LEN(Doc$(CurLine)) > 0 THEN
        IF RIGHT$(Doc$(CurLine), 1) = " " THEN
            Doc$(CurLine) = LEFT$(Doc$(CurLine), LEN(Doc$(CurLine)) - 1)
        END IF
    END IF

    DO WHILE LEN(overflow$) > 0 AND LEFT$(overflow$, 1) = " "
        overflow$ = MID$(overflow$, 2)
    LOOP

    movedLen = LEN(overflow$)
    oldBreak = SoftBreak(CurLine)

    IF CurLine = TotalLines THEN
        TotalLines = TotalLines + 1
        Doc$(TotalLines) = ""
        SoftBreak(TotalLines) = 0
    ELSE
        FOR i = TotalLines + 1 TO CurLine + 2 STEP -1
            Doc$(i) = Doc$(i - 1)
            SoftBreak(i) = SoftBreak(i - 1)
        NEXT i
        TotalLines = TotalLines + 1
    END IF

    Doc$(CurLine + 1) = overflow$
    SoftBreak(CurLine) = -1
    SoftBreak(CurLine + 1) = oldBreak

    CurLine = CurLine + 1
    CurCol = movedLen + 1
END SUB

'=========================================================
' Wrap a SPECIFIC line without moving the user's cursor.
' Used internally by rewrap / reflow routines.
'=========================================================
SUB WrapLineAt (lineNum%)
    DIM breakPos
    DIM overflow$
    DIM i
    DIM s$
    DIM oldBreak

    IF lineNum% < 1 OR lineNum% > TotalLines THEN EXIT SUB
    IF LEN(Doc$(lineNum%)) <= 80 THEN EXIT SUB
    IF TotalLines >= MAXLINES THEN EXIT SUB

    s$ = Doc$(lineNum%)

    breakPos = 80
    DO WHILE breakPos > 1
        IF MID$(s$, breakPos, 1) = " " THEN EXIT DO
        breakPos = breakPos - 1
    LOOP

    IF breakPos <= 1 THEN breakPos = 80

    overflow$ = MID$(s$, breakPos + 1)
    Doc$(lineNum%) = LEFT$(s$, breakPos)

    IF LEN(Doc$(lineNum%)) > 0 THEN
        IF RIGHT$(Doc$(lineNum%), 1) = " " THEN
            Doc$(lineNum%) = LEFT$(Doc$(lineNum%), LEN(Doc$(lineNum%)) - 1)
        END IF
    END IF

    DO WHILE LEN(overflow$) > 0 AND LEFT$(overflow$, 1) = " "
        overflow$ = MID$(overflow$, 2)
    LOOP

    oldBreak = SoftBreak(lineNum%)

    FOR i = TotalLines + 1 TO lineNum% + 2 STEP -1
        Doc$(i) = Doc$(i - 1)
        SoftBreak(i) = SoftBreak(i - 1)
    NEXT i

    TotalLines = TotalLines + 1
    Doc$(lineNum% + 1) = overflow$
    SoftBreak(lineNum%) = -1
    SoftBreak(lineNum% + 1) = oldBreak
END SUB

