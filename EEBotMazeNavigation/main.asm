;*******************************************************************
;* Final Project eebot Program (9S32C)                             *
;*******************************************************************

; export symbols
        	XDEF Entry, _Startup      	; export 'Entry' symbol
        	ABSENTRY Entry            	; for absolute assembly: mark this as application entry point

; Include derivative-specific definitions
    		INCLUDE 'derivative.inc'

ROMStart  	EQU 	$4000           	; absolute address to place my code/constant data

; equates section
;*******************************************************************
;Constants for LCD/State Values
LCD_DAT   	EQU 	PORTB           	; LCD Data Port (Port B) (PB7,...,PB0)
LCD_CNTR  	EQU 	PTJ             	; LCD Control Port (Port P) (PE7(RS),PE4(E))
LCD_E     	EQU 	$80             	; E-signal control pin for LCD
LCD_RS    	EQU 	$40             	; RS-signal control pin for LCD

;Delay Constants (for timing in 23Hz intervals)
FWD_INT   	  EQU 	69              	; 3 second delay (at 23Hz)
REV_INT   	  EQU 	11              	; 3 second delay (at 23Hz)
FWD_TRN_INT   EQU 	46              	; 2 second delay (at 23Hz)
REV_TRN_INT   EQU 	40              	; 2 second delay (at 23Hz)

;State Equates
START     	EQU 	0               	; START state value
FWD       	EQU 	1               	; FWD state
REV       	EQU 	2               	; REV state
ALL_STP   	EQU 	3               	; ALL_STOP state
FWD_TRN   	EQU 	4               	; FWD_TRN state
REV_TRN   	EQU 	5               	; REV_TRN state

; variable section
;*******************************************************************
          	ORG 	$3800           	; Where our TOF counter register lives
         	 
TOF_COUNTER dc.b	0               	; The timer, incremented at 23Hz
CRNT_STATE	dc.b	3               	; Current state register
T_FWD     	ds.b	1               	; FWD time
T_REV     	ds.b	1               	; REV time
T_FWD_TRN 	ds.b	1               	; FWD_TURN time
T_REV_TRN 	ds.b	1               	; REV_TURN time
TEN_THOUS 	ds.b	1               	; 10,000 digit
THOUSANDS 	ds.b	1               	; 1,000 digit
HUNDREDS  	ds.b	1               	; 100 digit
TENS      	ds.b	1               	; 10 digit
UNITS     	ds.b	1               	; 1 digit
NO_BLANK  	ds.b	1               	; Used in 'leading zero' blanking by BCD2ASC
BCD_SPARE 	RMB 	10              	; Extra space for decimal point and string terminator

;---------------------------------------------------------------------------
; Storage Registers (9S12C32 RAM space: $3800 ... $3FFF)
SENSOR_LINE   	FCB 	$01           	; Storage for guider sensor readings
SENSOR_BOW  	  FCB 	$23           	; Initialized to test values
SENSOR_PORT  	  FCB 	$45
SENSOR_MID  	  FCB 	$67
SENSOR_STBD   	FCB 	$89

SENSOR_NUM		RMB 	1             	; The currently selected sensor

TEMP      		RMB 	1             	; Temporary location
TENDSF    		RMB 	1

FT       		EQU               	$C0
PT       		EQU               	$A6
MT       		EQU               	$C0
ST       		EQU               	$C0
LTL      		EQU               	$66
LTR      		EQU               	$66

STK     dc.b            	$4
				dc.b            	$4
				dc.b            	$4
				dc.b            	$4
				dc.b            	$4
				dc.b            	$4
				dc.b            	$4
				dc.b            	$4
       	 

STK_PNTR          	dc.b      	$0
CUSTOM_OFFSET      	dc.b        $0

; code section
;*******************************************************************
; Entry Point: Initializes stack pointer, enables interrupts, sets up ports, and displays initial LCD messages
          	ORG 	$4000           	; Where the code starts --------------------
                                      	;                                       	|
Entry:                                	;                                       	|
_Startup:                             	;                                       	|
          	CLI                     	; Enable global interrupts              	|
          	LDS 	#$4000          	; Initialize the stack pointer          	|
         	 
          	JSR 	INIT          	; Initialize ports
          	JSR 	openADC       	; Initialize the ATD
                                      	;                                       	I
          	BSET	DDRA,%00000011  	; STAR_DIR, PORT_DIR - motor control    	N
          	BSET	DDRT,%00110000  	; STAR_SPEED, PORT_SPEED - motor speed  	I
                                      	;                                       	T
          	;initialize subsytems                                               	I
          	;JSR 	initAD          	; Initialize ATD converter              	A
          	;JSR 	initLCD         	; Initialize the LCD                    	L
          	;JSR 	clrLCD          	; Clear LCD & home cursor               	I
                                      	;                                       	Z
          	;Display initial messages on LCD                                    	|
          	;LDX 	#msg1           	; Point X to "Battery volt " msg        	A
          	;JSR 	putsLCD         	; display btry msg on LCD               	T
                                      	;                                       	I
          	;LDAA	#$C0            	; Move LCD cursor to the 2nd row of LCD 	O
          	;JSR 	cmd2LCD         	; Issue cursor move cmd                 	N
                                      	;                                       	|
          	;LDX 	#msg2           	; Point X to "State " msg               	|
          	;JSR 	putsLCD         	; display state msg on LCD              	|
                                      	;                                       	|
          	;Enable Time Overflow for timing state transitions                  	|
          	JSR 	ENABLE_TOF      	;Set TOF to inc cntr at 23Hz for ST timing--

MAIN      	JSR 	G_LEDS_ON     	; Enable the guider LEDs
          	JSR 	READ_SENSORS  	; Read the 5 guider sensors
          	JSR 	G_LEDS_OFF    	; Disable the guider LEDs
         	 
          	;JSR 	UPDT_DISPL      	; ----------------------------------------- M
          	LDAA	CRNT_STATE      	;                                         	A
          	JSR 	DISPATCHER      	;                                         	I
          	BRA 	MAIN            	; ----------------------------------------- N

; data section
;*******************************************************************
;Message Strings for LCD
msg1      	dc.b	"S: ",0   			;LCD 1st row displayed msg
msg2      	dc.b	"STK: ",0      					;LCD 2nd row displayed msg
tab       	dc.b	"START  ",0      				;Text for Each State displayed on LCD
          	dc.b	"FWD  	",0
          	dc.b	"REV   	",0
          	dc.b	"ALL_STP",0
          	dc.b	"FWD_TRN",0
          	dc.b	"REV_TRN",0

; subroutine section
;**********************************************************************
;*  	`   State Dispatcher
;*
;* This routine calls the appropriate state handler based on the current
;*  state.
;* Input:	Current state in ACCA
;* Returns:  None
;* Clobbers: Everything
;
; State Dispatcher
DISPATCHER	CMPA	#START          	; If it's the START state -----------------
          	BNE 	NOT_START       	;	if not, skip to next state check      |
          	JSR 	START_ST        	;  then call START_ST handler          	|
          	BRA 	DISP_EXIT       	;  and exit                            	D
                                      	;                               		|
NOT_START 	CMPA	#FWD            	; Else if it's the FWD state           	I
          	BNE 	NOT_FWD         	;	if not, skip to next state check     |
          	JSR 	FWD_ST          	;  then call FWD_ST handler            	S
          	BRA 	DISP_EXIT       	;  and exit                            	|
                                      	;                               		P
NOT_FWD   	CMPA	#REV            	; Else if it's the REV state           	|
          	BNE 	NOT_REV         	;	if not, skip to next state check      A
          	JSR 	REV_ST          	;  then call REV_ST handler            	|
          	BRA 	DISP_EXIT       	;  and exit                            	T
                                      	;                               		|
NOT_REV   	CMPA	#ALL_STP        	; Else if it's the ALL_STP state       	C
          	BNE 	NOT_ALL_STP     	;	if not, skip to next state check      |
          	JSR     ALL_STP_ST			  ;                   										H
          	BRA 	DISP_EXIT       	;  and exit                            	|
                                      	                                 		|
NOT_ALL_STP   	CMPA	#FWD_TRN        	; Else if it's the FWD_TRN state  	  E
            	BNE 	NOT_FWD_TRN     	;	if not, skip to next state check    |
            	JSR 	FWD_TRN_ST      	;  then call FWD_TRN_ST handler       |
            	BRA 	DISP_EXIT       	;  and exit                           R 
                      											                               	|
                      											                               	|
NOT_FWD_TRN   	CMPA	#REV_TRN        	; Else if it's the REV_TRN state  	  |
            	BNE 	NOT_REV_TRN     	;	if not, skip to next state check    |
            	JSR 	REV_TRN_ST      	;  then call REV_TRN_ST handler       |
            	BRA 	DISP_EXIT       	;  and exit                           |
                                      		;                               	|
NOT_REV_TRN   SWI                    ; Else the CRNT_ST is not defined, so stop |
DISP_EXIT 	  RTS                    ; Exit from the state dispatcher  ----------

;****************************************************************
;*      	START STATE HANDLER
;*
;*   Advances state to the FORWARD state if /FWD-BUMP.
;*
;* Passed:   Current state in ACCA
;* Returns:  New state in ACCA
;* Clobbers: None
START_ST  	BRCLR   PORTAD0,$04,NO_FWD  ; If FWD_BUMP, branch to NO_FWD
          	JSR 	INIT_FWD        	; Initialize FWD State
          	MOVB	#FWD,CRNT_STATE 	; Go into FWD State
          	BRA 	START_EXIT      	;
         	 
NO_FWD    	NOP                     	; Else
START_EXIT	RTS                     	;  return to the MAIN routine

;****************************************************************
;*      	FORWARD STATE HANDLER
;*
;* Algorithm:
;*      	If FWD_BMP then
;*          	Initialize the REVERSE state
;*          	Change the state to REVERSE
;*          	Return
;*      	If REAR_BUMP then
;*          	Initialize the ALL_STOP state.
;*          	Change the state to ALL_STOP
;*          	Return
;*      	If Tc>Tfwd then
;*          	Initialize the FORWARD_TURN state
;*          	Change the state to FORWARD_TURN
;*          	Return
;*      	Else
;*          	Return
;*
;* Passed:   Current state in ACCA
;* Returns:  New state in ACCA
;* Clobbers: Everything, probably. Make no assumptions.

;*******************************************************************

STARON  	LDAA PTT
        	ORAA #%00100000
        	STAA PTT
        	RTS
       	 
STAROFF 	LDAA PTT
        	ANDA #%11011111
        	STAA PTT
        	RTS

PORTON  	LDAA PTT
        	ORAA #%00010000
        	STAA PTT
        	RTS
       	 
PORTOFF 	LDAA PTT
        	ANDA #%11101111
        	STAA PTT
        	RTS


FWD_ST    	BRSET PORTAD0,$04,NO_FWD_BUMP ; If FWD_BUMP then transition REV
          	JSR 	INIT_REV              	; Initialize REV State
          	MOVB	#REV,CRNT_STATE       	; Go to REV State
          	JMP 	FWD_EXIT              	;  return from handler

NO_FWD_BUMP 	BRSET   PORTAD0,$08,NO_REAR_BUMP; If REAR_BUMP then should stop
            	JSR 	INIT_ALL_STP        	; Initialize ALL_STOP State
            	MOVB	#ALL_STP,CRNT_STATE 	; Go to ALL_STOP State
            	JMP 	FWD_EXIT            	;  return from handler


NO_REAR_BUMP	JSR STAROFF
            	JSR PORTOFF

            	JSR CHECK_INTERSECTION
            	JSR CHECK_ALIGN
            	JSR NO_FWD_TRN
            	JMP FWD_EXIT

CHECK_INTERSECTION        	LDAA SENSOR_MID
                          	CMPA #MT
                          	BHI INTERSECTION1
                           	RTS

INTERSECTION1   LDAA SENSOR_STBD
              	CMPA #ST
              	BHI INTERSECTION2
             	 
              	LDAA SENSOR_PORT
              	CMPA #PT
              	BHI INTERSECTION2
             	 
              	RTS
           	 
INTERSECTION2   LDAA SENSOR_STBD
              	CMPA #ST
              	BHI TURN_RIGHT
           	 
              	LDAA SENSOR_BOW
              	CMPA #FT
              	BHI STRAIGHT
           	 
              	LDAA SENSOR_PORT
              	CMPA #PT
              	BHI TURN_LEFT
             	 
              	RTS

RETURN RTS

STRAIGHT  	JSR STAROFF
          	JSR PORTOFF
          	LDY #3000
          	JSR PORTON
          	JSR STARON
          	JSR del_50us
          	JSR STAROFF
          	JSR PORTOFF

          	LDAA #$1
          	ADDA CUSTOM_OFFSET

          	JSR PUSH_STACK
       	 
          	RTS

TURN_LEFT   JSR STAROFF
          	JSR PORTOFF
          	LDY #32500
          	JSR STARON
          	JSR del_50us
          	LDY #2000
          	JSR PORTON
          	JSR del_50us
          	JSR STAROFF
          	JSR PORTOFF

          	LDAA #$2
          	ADDA CUSTOM_OFFSET

          	JSR PUSH_STACK
       	 
          	RTS
       	 
TURN_RIGHT  JSR STAROFF
          	JSR PORTOFF
          	LDY #32500
          	JSR PORTON
          	JSR del_50us
          	LDY #2000
           	JSR STARON
          	JSR del_50us
          	JSR STAROFF
          	JSR PORTOFF

          	LDAA #$0
          	ADDA CUSTOM_OFFSET

          	JSR PUSH_STACK
         	 
          	RTS
         	 
         	 
PUSH_STACK  LDX #STK
          	LDAB STK_PNTR
          	ABX
       	 
          	STAA 0,X
          	INC STK_PNTR
          	
          	LDAA #$0
          	STAA CUSTOM_OFFSET

          	RTS
         	 

CHECK_ALIGN 	LDAA SENSOR_LINE
            	CMPA #LTL
            	BLO LEFT_ALIGN
            	BHI RIGHT_ALIGN
            	;LDY #1000
          	  	;JSR del_50us
            	RTS
         	 
LEFT_ALIGN	JSR STARON
          	LDY #750
          	JSR del_50us
          	JSR PORTON
          	LDY #500
          	JSR del_50us
          	JSR STAROFF
          	JSR PORTOFF
          	RTS
      	 
RIGHT_ALIGN JSR PORTON
          	LDY #750
          	JSR del_50us
          	JSR STARON
          	LDY #500
          	JSR del_50us
          	JSR STAROFF
          	JSR PORTOFF
          	RTS

EXIT      	RTS

NO_FWD_TRN	NOP                     	; Else (Since no other cond. met, remain in FWD)
FWD_EXIT  	RTS                     	;  return to the MAIN routine

;****************************************************************
;*      	REVERSE STATE HANDLER
;*
;* Algorithm:
;*      	If Tc>Trev then
;*          	Initialize the REVERSE_TURN state
;*          	Change the state to REVERSE_TURN
;*          	Return
;*
;* Moves robot in reverse until time limit reached and then transitions to REV_TRN after time up.
REV_ST    	LDAA	TOF_COUNTER     	; If Tc>Trev then
          	CMPA	T_REV           	;  the robot should make a FWD turn
          	BNE 	NO_REV_TRN      	;  so
          	JSR 	INIT_REV_TRN    	;  initialize the REV_TRN state
          	MOVB	#REV_TRN,CRNT_STATE ;  set state to REV_TRN
          	BRA 	REV_EXIT        	;  and return from handler

NO_REV_TRN	NOP                     	; Else
REV_EXIT  	RTS                     	;  return to the MAIN routine

;*******************************************************************
;*      	ALL_STOP STATE HANDLER
;*
;* Stops all movement and waits for a forward bump to restart.
ALL_STP_ST	BRSET   PORTAD0,$04,NO_START; If FWD_BUMP
          	BCLR	PTT,%00110000   	;   initialize the START state (both motors off)
          	MOVB	#START,CRNT_STATE ;   set the state to START
          	BRA 	ALL_STP_EXIT    	;   and return from handler
         	 
NO_START  	  NOP                     	; Else
ALL_STP_EXIT  RTS                     	;   return to the MAIN routine

;*******************************************************************
;*      	ALL_STOP STATE HANDLER
;*
;* follow forward routine with stack
BACK_TRACK    	JSR   STAROFF
              	JSR   PORTOFF

				        JSR 	G_LEDS_ON     	; Enable the guider LEDs
              	JSR 	READ_SENSORS  	; Read the 5 guider sensors
              	JSR 	G_LEDS_OFF    	; Disable the guider LEDs           	 

				        JSR 	UPDT_DISPL

              	JSR   CHECK_INTERSECTION2
              	JSR   CHECK_ALIGN
              	NOP

                BRA   BACK_TRACK
                
                RTS

CHECK_INTERSECTION2        	LDAA SENSOR_MID
                          	CMPA #MT
                          	BHI INTERSECTION12
                           	RTS

INTERSECTION12  LDAA SENSOR_STBD
              	CMPA #ST
              	BHI INTERSECTION22
             	 
              	LDAA SENSOR_PORT
              	CMPA #PT
              	BHI INTERSECTION22
             	 
              	RTS
           	 
INTERSECTION22        LDX #STK
                    	LDAB STK_PNTR
                    	ABX

                    	LDAA 0,X

                    	CMPA #$2
                    	BEQ TURN_RIGHT2
                   	 
                    	CMPA #$1
                    	BEQ STRAIGHT2
                   	 
                    	CMPA #$0
                    	BEQ TURN_LEFT2
                   	 
                    	RTS

STRAIGHT2 	JSR STAROFF
          	JSR PORTOFF
          	LDY #3000
          	JSR PORTON
          	JSR STARON
          	JSR del_50us
          	JSR STAROFF
          	JSR PORTOFF

          	DEC STK_PNTR
       	 
          	RTS

TURN_LEFT2  JSR STAROFF
          	JSR PORTOFF
          	LDY #32500
          	JSR STARON
          	JSR del_50us
          	LDY #2000
          	JSR PORTON
          	JSR del_50us
          	JSR STAROFF
          	JSR PORTOFF

          	DEC STK_PNTR

          	RTS
       	 
TURN_RIGHT2 JSR STAROFF
          	JSR PORTOFF
          	LDY #32500
          	JSR PORTON
          	JSR del_50us
          	LDY #2000
           	JSR STARON
          	JSR del_50us
          	JSR STAROFF
          	JSR PORTOFF

          	DEC STK_PNTR
       	 
          	RTS

;*******************************************************************
;*      	FORWARD_TURN STATE HANDLER
;*
;* Turns robot for defined time, then resumes forward movement.
FWD_TRN_ST	LDAA	TOF_COUNTER     	; If Tc>Tfwdturn then
          	CMPA	T_FWD_TRN       	;  the robot should go FWD
          	BNE 	NO_FWD_FT       	;  so
          	JSR 	INIT_FWD        	;  initialize the FWD state
          	MOVB	#FWD,CRNT_STATE 	;  set state to FWD
          	BRA 	FWD_TRN_EXIT    	;  and return

NO_FWD_FT 	  NOP                     	; Else
FWD_TRN_EXIT  RTS                     	;  return to the MAIN routine

;*******************************************************************
;*      	REVERSE_TURN STATE HANDLER
;*
;* Turns robot while reversing, then resumes forward movement.
REV_TRN_ST	LDAA	TOF_COUNTER     	; If Tc>Trevturn then
          	CMPA	T_REV_TRN       	;  the robot should go FWD
          	BNE 	NO_FWD_RT       	;  so
          	JSR 	INIT_FWD        	;  initialize the FWD state
          	MOVB	#FWD,CRNT_STATE 	;  set state to FWD
          	BRA 	REV_TRN_EXIT    	;  and return
         	 
NO_FWD_RT 	  NOP                     	; Else
REV_TRN_EXIT  RTS                     	;  return to the MAIN routine



; INITIALIZATION SUBROUTINES
;*******************************************************************
;Initializes Forward Movement State
INIT_FWD  	BCLR	PORTA,%00000011 	; Set FWD direction for both motors
          	BSET	PTT,%00110000   	; Turn on the drive motors
          	LDAA	TOF_COUNTER     	; Mark the fwd time Tfwd
          	ADDA	#FWD_INT
          	STAA	T_FWD
          	RTS

;*******************************************************************
;Initializes Reverse Movement State
INIT_REV  	BSET	PORTA,%00000011 	; Set REV direction for both motors
          	BSET	PTT,%00110000   	; Turn on the drive motors
          	LDAA	TOF_COUNTER     	; Mark the fwd time Tfwd
          	ADDA	#REV_INT
          	STAA	T_REV

      			DEC STK_PNTR
      			
      			LDX #STK
      			LDAB STK_PNTR
      			ABX
        	  
          	LDAA 0,X
          	ADDA #$1
          	STAA CUSTOM_OFFSET

          	RTS

;*******************************************************************
;Initializes All Stop Movements State
INIT_ALL_STP  ;BCLR	PTT,%00110000   	; Turn off the drive motors
            	JSR BACK_TRACK
				      RTS

;*******************************************************************
;Initializes Forward Turn Movement State
INIT_FWD_TRN  	BSET	PORTA,%00000010 	; Set REV dir. for STARBOARD (right) motor
              	LDAA	TOF_COUNTER     	; Mark the fwd_turn time Tfwdturn
              	ADDA	#FWD_TRN_INT
              	STAA	T_FWD_TRN
              	RTS

;*******************************************************************
;Initializes Reverse Turn Movement State
INIT_REV_TRN  BCLR	PORTA,%00000010 	; Set FWD dir. for STARBOARD (right) motor
            	LDAA	TOF_COUNTER     	; Mark the fwd time Tfwd
            	ADDA	#REV_TRN_INT
            	STAA	T_REV_TRN
            	RTS


; utility subroutines
;*******************************************************************
;* Initialization of the LCD: 4-bit data width, 2-line display,	*
;* turn on display, cursor and blinking off. Shift cursor right.   *
;*******************************************************************
initLCD:	BSET  DDRB, %11111111 ; configure pins PS7, PS6, PS5, PS4 for output
        	BSET  DDRJ, %11000000 ; configure pins PE7, PE4 for output
        	LDY   #2000       	; wait for LCD to be ready
        	JSR   del_50us    	; delay
        	LDAA  #$28        	; set 4-bit data, 2-line display
        	JSR   cmd2LCD     	; send command to LCD
        	LDAA  #$0C        	; display on, cursor off, blinking off
        	JSR   cmd2LCD     	; send command to LCD
        	LDAA  #$06        	; move cursor right after entering a character
        	JSR   cmd2LCD     	; send command to LCD
        	RTS               	; return from subroutine
;*******************************************************************
;* Clear display and home cursor                               	*
;*******************************************************************
clrLCD: 	LDAA  #$01        	; clear cursor and return to home position
        	JSR   cmd2LCD     	; send command to LCD
        	LDY   #40         	; wait until "clear cursor" command is complete
        	JSR   del_50us    	; Call delay subroutine
        	RTS               	; return from subroutine

;*******************************************************************
;* ([Y] x 50us)-delay subroutine. E-clk = 41.67ns.             	*
;*******************************************************************
del_50us: PSHX              	;2 E-clk
eloop:  	LDX   #30         	;2 E-clk -
iloop:  	PSHA              	;2 E-clk  |
        	PULA              	;3 E-clk  |
;                                       	|       	 
; 50us delay � 41.67ns E-clk = 1200 Cycles  |
; 1200 cycles / 30 (counter loaded in X) = 40 E-clk's per loop to achieve 50us delay
;                                       	|
;       	6 more pairs of push and pull (5 E-clk per pair) for 30 total E-Clk's
        	PSHA              	;2 E-clk  |
        	PULA              	;3 E-clk  | Pair 1
        	PSHA              	;2 E-clk  |
        	PULA              	;3 E-clk  | Pair 2
        	PSHA              	;2 E-clk  |
        	PULA              	;3 E-clk  | Pair 3  | 50us
        	PSHA              	;2 E-clk  |
        	PULA              	;3 E-clk  | Pair 4
        	PSHA              	;2 E-clk  |
        	PULA              	;3 E-clk  | Pair 5
        	PSHA              	;2 E-clk  |
        	PULA              	;3 E-clk  | Pair 6
;                                       	|
        	DBNE  X,iloop     	;3 E-clk -   2 E-clk + 5 E-clk (initial push/pull) + 30 E-clk (6 pairs of push/pull) + 3 E-clk (Decrement&Branch if not equal to 0) = 40 E-clk's
        	DBNE  Y,eloop     	;3 E-clk
        	PULX              	;3 E-clk
        	RTS               	;5 E-clk

;*******************************************************************
;* This function sends a command in accumulator A to the LCD   	*                 	(RS = 0 = command data)
;*******************************************************************
cmd2LCD:	BCLR  LCD_CNTR, LCD_RS ; Clear RS bit to select the LCD Instruction Register (IR)
        	JSR   dataMov      	; send data to IR
        	RTS                	; return from subroutine

;*******************************************************************
;* This function outputs a NULL-terminated string pointed to by X  *
;*******************************************************************
putsLCD:	LDAA  1, X+       	; loading next char from the string into A & increment X
        	BEQ   donePS      	; A reach NULL/0 character? Call donePS subroute to end loop
        	JSR   putcLCD     	; write the character to LCD by calling putcLCD
        	BRA   putsLCD     	; continue to next character by branching back in a loop

donePS: 	RTS               	; return from subroutine

;*******************************************************************
;* This function outputs the character in accumulator in A to LCD  *                	(RS = 1 = char data)
;*******************************************************************
putcLCD:	BSET  LCD_CNTR, LCD_RS ; Set RS high to select the LCD Data Register (DR)   
        	JSR   dataMov      	; send data (character in A) to DR using dataMov subroutine
        	RTS                	; return from subroutine.

;*******************************************************************
;* This function sends data to the LCD IR or DR depening on RS 	*
;*******************************************************************
dataMov:  	BSET  LCD_CNTR, LCD_E  ; pull the LCD E signal high to start transfer
          	STAA  LCD_DAT      	; send the upper 4 bits of data to LCD
          	BCLR  LCD_CNTR, LCD_E  ; pull the LCD E signal low to finish write operation
         	 
          	LSLA               	; shift the lower 4 bits to match the 4 bits with the LCD data pins
          	LSLA               	; -"-
          	LSLA               	; -"-
          	LSLA               	; -"-
         	 
          	BSET  LCD_CNTR, LCD_E  ; pull the LCD E signal high to transfer lower 4 bits
          	STAA  LCD_DAT      	; send the lower 4 bits to LCD
          	BCLR  LCD_CNTR, LCD_E  ; pull the LCD E signal low to finish write operation
         	 
          	LDY   #1           	; delay for 50us to complete the write operation
          	JSR   del_50us     	; call 50us delay subroutine
          	RTS                	; return from subroutine
;*******************************************************************       	 
initAD    	MOVB	#$C0,ATDCTL2    	;power up A/D Converter, select fast flag clear mode
          	JSR 	del_50us        	;wait for 50 us
          	MOVB	#$00,ATDCTL3    	;set up the A/D to do 8 conversions per sequence
          	MOVB	#$85,ATDCTL4    	;set resolution=8-bit, conversion-clks=2, prescaler=12
          	BSET	ATDDIEN,$0C     	;configure pins AN03,AN02 as digital inputs
          	RTS

;*****************************************************************
;* Integer to BCD Conversion Routine
;* This routine converts a 16 bit binary number in .D into
;* BCD digits in BCD_BUFFER.
;* Peter Hiscocks
;* Algorithm:
;*  Because the IDIV (Integer Division) instruction is available on
;*	the HCS12, we can determine the decimal digits by repeatedly
;*	dividing the binary number by ten: the remainder each time is
;*	a decimal digit. Conceptually, what we are doing is shifting
;*	the decimal number one place to the right past the decimal
;*	point with each divide operation. The remainder must be
;*	a decimal digit between 0 and 9, because we divided by 10.
;* The algorithm terminates when the quotient has become zero.
;* Bug note: XGDX does not set any condition codes, so test for
;* quotient zero must be done explicitly with CPX.
;*****************************************************************
int2BCD  	XGDX                     	;Save the binary number into .X
         	LDAA   #0                	;Clear the BCD_BUFFER
         	STAA TEN_THOUS
         	STAA THOUSANDS
         	STAA HUNDREDS
         	STAA TENS
         	STAA UNITS
         	STAA BCD_SPARE
         	STAA BCD_SPARE+1

         	CPX  #0                  	;Check for a zero input
         	BEQ  CON_EXIT            	;and if so, exit

         	XGDX                     	;Not zero, get the binary number back to .D as dividend
         	LDX  #10                 	;Setup 10 (Decimal!) as the divisor
         	IDIV                     	;Divide: Quotient is now in .X, remainder in .D
         	STAB UNITS               	;Store remainder
         	CPX  #0                  	;If quotient is zero,
         	BEQ  CON_EXIT            	; then exit

         	XGDX                     	;else swap first quotient back into .D
         	LDX  #10                 	;and setup for another divide by 10
         	IDIV
         	STAB TENS
         	CPX  #0
         	BEQ  CON_EXIT

         	XGDX                     	;Swap quotient back into .D
         	LDX  #10                 	;and setup for another divide by 10
         	IDIV
         	STAB HUNDREDS
         	CPX  #0
         	BEQ  CON_EXIT

         	XGDX                     	;Swap quotient back into .D
         	LDX  #10                 	;and setup for another divide by 10
         	IDIV
         	STAB THOUSANDS
         	CPX  #0
         	BEQ  CON_EXIT

         	XGDX                     	;Swap quotient back into .D
         	LDX  #10                 	;and setup for another divide by 10
         	IDIV
         	STAB TEN_THOUS

CON_EXIT 	RTS                      	;We�re done the conversion
;****************************************************************
;* BCD to ASCII Conversion Routine
;* This routine converts the BCD number in the BCD_BUFFER
;* into ascii format, with leading zero suppression.
;* Leading zeros are converted into space characters.
;* The flag �NO_BLANK� starts cleared and is set once a non-zero
;* digit has been detected.
;* The �units� digit is never blanked, even if it and all the
;* preceding digits are zero.
;* Peter Hiscocks
BCD2ASC  	LDAA   #0                	;Initialize the blanking flag
         	STAA   NO_BLANK

C_TTHOU  	LDAA   TEN_THOUS         	;Check the �ten_thousands� digit
         	ORAA   NO_BLANK
         	BNE	NOT_BLANK1

ISBLANK1 	LDAA   #' '              	;It's blank
         	STAA   TEN_THOUS         	; so store a space
         	BRA	C_THOU            	; and check the �thousands� digit

NOT_BLANK1  LDAA   TEN_THOUS         	;Get the �ten_thousands� digit
           	ORAA   #$30              	;Convert to ascii
           	STAA   TEN_THOUS
           	LDAA   #$1               	;Signal that we have seen a �non-blank� digit
           	STAA   NO_BLANK

C_THOU   	LDAA   THOUSANDS         	;Check the thousands digit for blankness
         	ORAA   NO_BLANK          	;If it�s blank and �no-blank� is still zero
         	BNE	NOT_BLANK2

ISBLANK2 	LDAA   #' '              	;Thousands digit is blank
         	STAA   THOUSANDS         	; so store a space
         	BRA	C_HUNS            	; and check the hundreds digit

NOT_BLANK2  LDAA   THOUSANDS         	;(similar to �ten_thousands� case)
           	ORAA   #$30
           	STAA   THOUSANDS
           	LDAA   #$1
           	STAA   NO_BLANK

C_HUNS   	LDAA   HUNDREDS          	;Check the hundreds digit for blankness
         	ORAA   NO_BLANK          	;If it�s blank and �no-blank� is still zero
         	BNE	NOT_BLANK3

ISBLANK3 	LDAA   #' '              	;Hundreds digit is blank
         	STAA   HUNDREDS          	; so store a space
         	BRA	C_TENS            	; and check the tens digit

NOT_BLANK3  LDAA   HUNDREDS          	;(similar to �ten_thousands� case)
           	ORAA   #$30
           	STAA   HUNDREDS
           	LDAA   #$1
           	STAA   NO_BLANK

C_TENS   	LDAA   TENS              	;Check the tens digit for blankness
         	ORAA   NO_BLANK          	;If it�s blank and �no-blank� is still zero
         	BNE	NOT_BLANK4  ;

ISBLANK4 	LDAA   #' '              	;Tens digit is blank
         	STAA   TENS              	; so store a space
         	BRA	C_UNITS           	; and check the units digit

NOT_BLANK4   LDAA   TENS              	;(similar to �ten_thousands� case)
           	 ORAA   #$30
           	 STAA   TENS

C_UNITS  	LDAA   UNITS             	;No blank check necessary, convert to ascii.
         	ORAA   #$30
         	STAA   UNITS

         	RTS                      	;We�re done

;************************************************************
;Timer Setup
ENABLE_TOF  LDAA	#%10000000
          	STAA	TSCR1       	; Enable TCNT (Timer) - 16-bit free-running timer that increments with eack clock tick)
          	STAA	TFLG2       	; Clear TOF (Clears timer overflow flag)
          	LDAA	#%10000100  	; Enable TOI and select prescale factor equal to 16 (Thus timer overflow occur every 43.7ms)
          	STAA	TSCR2       	; Enables timer overflow interrupt (TOI) and sets prescaler to divide clock
          	RTS
       	 
;************************************************************
;Timer overflow interrupt service routine
TOF_ISR 	INC 	TOF_COUNTER 	; Increments TOF_COUNTER every time 16-bit TCNT (Timer) overflows (23 times/sec)
        	LDAA	#%10000000  	; Loading value to clear TOF Flag
        	STAA	TFLG2       	; Clear TOF Flag by writing 1 to the TOF bit
        	RTI                 	; Return from interrupt

;*******************************************************************
;*    	Update Display (Battery Voltage + Current State)     	*
;*******************************************************************
UPDT_DISPL	;Move LCD cursor to end of msg1 for vltg display
          	LDAA   #$83              	;move LCD cursor to the 1st row, end of msg1
          	JSR	cmd2LCD
         	 
          	LDAA SENSOR_BOW
          	JSR BIN2ASC
          	STAB TENDSF
           	JSR putcLCD
          	LDAA TENDSF
          	JSR putcLCD

          	LDAA #'_'
          	JSR putcLCD
        	 
          	LDAA SENSOR_PORT
          	JSR BIN2ASC
          	STAB TENDSF
          	JSR putcLCD
          	LDAA TENDSF
          	JSR putcLCD

          	LDAA #'_'
          	JSR putcLCD
        	 
          	LDAA SENSOR_MID
          	JSR BIN2ASC
          	STAB TENDSF
           	JSR putcLCD
          	LDAA TENDSF
          	JSR putcLCD

          	LDAA #'_'
          	JSR putcLCD
        	 
          	LDAA SENSOR_STBD
          	JSR BIN2ASC
          	STAB TENDSF
          	JSR putcLCD
          	LDAA TENDSF
          	JSR putcLCD
        	 
          	LDAA #'_'
          	JSR putcLCD
        	 
          	;Display vltg digits
          	LDAA SENSOR_LINE
          	JSR BIN2ASC
          	STAB TENDSF
          	JSR putcLCD
          	LDAA TENDSF
          	JSR putcLCD
         	 
          	;LDAA   TEN_THOUS         	;load ASCII of Ten-thous' place for battery voltage
          	;JSR	putcLCD           	; output value onto LCD
          	;LDAA   THOUSANDS         	;load ASCII of thousand's place
          	;JSR	putcLCD           	; output value onto LCD
          	;LDAA   #'.'              	;Load ASCII of the decimal point
          	;JSR	putcLCD           	; output value onto LCD
          	;LDAA   HUNDREDS          	;Load ASCII of hundred's place
          	;JSR	putcLCD           	; output value onto LCD
;-------------------------
          	LDAA	#$C5            	; Move LCD cursor to the 2nd row, end of msg2
          	JSR 	cmd2LCD         	;
         	 
          	;LDAB	CRNT_STATE      	; Display current state
          	;LSLB                    	; "
          	;LSLB                    	; "
          	;LSLB                    	; "
          	;LDX 	#tab            	; "
          	;ABX                     	; "
          	;JSR 	putsLCD         	; "

        	LDX #STK

        	LDAA 0,X
        	JSR GET_ASCII
        	JSR putcLCD
       	 
        	LDAA #'_'
        	JSR putcLCD

        	LDAA 1,X
        	JSR GET_ASCII
        	JSR putcLCD
        	LDAA #'_'
        	JSR putcLCD

        	LDAA 2,X
        	JSR GET_ASCII
        	JSR putcLCD
        	LDAA #'_'
        	JSR putcLCD

        	LDAA 3,X
        	JSR GET_ASCII
        	JSR putcLCD
        	LDAA #'_'
        	JSR putcLCD

        	LDAA 4,X
        	JSR GET_ASCII
        	JSR putcLCD
        	LDAA #'_'
        	JSR putcLCD

        	LDAA 5,X
        	JSR GET_ASCII
        	JSR putcLCD
        	LDAA #'_'
        	JSR putcLCD

        	LDAA 6,X
        	JSR GET_ASCII
        	JSR putcLCD
        	;LDAA #'_'
        	;JSR putcLCD

        	;LDAA 7,X
        	;JSR GET_ASCII
        	;JSR putcLCD
       	 
          	RTS


GET_ASCII   CMPA #$0
          	BEQ  LOAD_R

          	CMPA #$1
          	BEQ  LOAD_S

          	CMPA #$2
          	BEQ  LOAD_L
         	 
          	LDAA #'?'

          	RTS     

LOAD_R  	LDAA #'R'
        	RTS

LOAD_S  	LDAA #'S'
        	RTS

LOAD_L  	LDAA #'L'
        	RTS         	 
         	 
HEX_TABLE 	FCC 	'0123456789ABCDEF'; Table for converting values

BIN2ASC   	PSHA                  	; Save a copy of the input number on the stack
          	TAB                   	; and copy it into ACCB
          	ANDB	#%00001111    	; Strip off the upper nibble of ACCB
          	CLRA                  	; D now contains 000n where n is the LSnibble
          	ADDD	#HEX_TABLE    	; Set up for indexed load
          	XGDX
          	LDAA	0,X           	; Get the LSnibble character
         	 
          	PULB                  	; Retrieve the input number into ACCB
          	PSHA                  	; and push the LSnibble character in its place
          	RORB                  	; Move the upper nibble of the input number
          	RORB                  	; into the lower nibble position.
          	RORB
          	RORB
          	ANDB	#%00001111    	; Strip off the upper nibble
          	CLRA                  	; D now contains 000n where n is the MSnibble
          	ADDD	#HEX_TABLE    	; Set up for indexed load
          	XGDX
          	LDAA	0,X           	; Get the MSnibble character into ACCA
          	PULB                  	; Retrieve the LSnibble character into ACCB
          	RTS
   
   	 
;---------------------------------------------------------------------------
;         	Initialize ports
INIT      	BCLR	DDRAD,$FF     	; Make PORTAD an input (DDRAD @ $0272)
          	BSET	DDRA,$FF      	; Make PORTA an output (DDRA @ $0002)
          	BSET	DDRB,$FF      	; Make PORTB an output (DDRB @ $0003)
          	BSET	DDRJ,$C0      	; Make pins 7,6 of PTJ outputs (DDRJ @ $026A)
          	RTS
         	 
;---------------------------------------------------------------------------
;         	Initialize the ADC
openADC   	MOVB	#$80,ATDCTL2  	; Turn on ADC (ATDCTL2 @ $0082)
          	LDY 	#1            	; Wait for 50 us for ADC to be ready
          	JSR 	del_50us      	; - " -
          	MOVB	#$20,ATDCTL3  	; 4 conversions on channel AN1 (ATDCTL3 @ $0083)
          	MOVB	#$97,ATDCTL4  	; 8-bit resolution, prescaler=48 (ATDCTL4 @ $0084)
          	BSET	ATDDIEN,$0C     	;configure pins AN03,AN02 as digital inputs
          	RTS

;---------------------------------------------------------------------------
;         	Guider LEDs ON
;
; This routine enables the guider LEDs so that readings of the sensor
;  correspond to the 'illuminated' situation.
;
; Passed: Nothing
; Returns: Nothing
; Side: PORTA bit 5 is changed
G_LEDS_ON 	BSET	PORTA,%00100000   ; Set bit 5
          	RTS
         	 
;
;         	Guider LEDs OFF
;
; This routine disables the guider LEDs. Readings of the sensor
;  correspond to the 'ambient lighting' situation.
;
; Passed: Nothing
; Returns: Nothing
; Side: PORTA bit 5 is changed
G_LEDS_OFF	BCLR	PORTA,%00100000   ; Clear bit 5
          	RTS
         	 
;---------------------------------------------------------------------------
;         	Read Sensors
;
; This routine reads the eebot guider sensors and puts the results in RAM
;  registers.
;
; Note: Do not confuse the analog multiplexer on the Guider board with the
;  multiplexer in the HCS12. The guider board mux must be set to the
;  appropriate channel using the SELECT_SENSOR routine. The HCS12 always
;  reads the selected sensor on the HCS12 A/D channel AN1.
;
; The A/D conversion mode used in this routine is to read the A/D channel
; AN1 four times into HCS12 data registers ATDDR0,1,2,3. The only result
; used in this routine is the value from AN1, read from ATDDR0. However,
; other routines may wish to use the results in ATDDR1, 2 and 3.
; Consequently, Scan=0, Mult=0 and Channel=001 for the ATDCTL5 control word.
;
; Passed: None
; Returns: Sensor readings in:
;         	SENSOR_LINE (0) (Sensor E/F)
;         	SENSOR_BOW (1) (Sensor A)
;         	SENSOR_PORT (2) (Sensor B)
;         	SENSOR_MID (3) (Sensor C)
;         	SENSOR_STBD (4) (Sensor D)
; Note:
;   The sensor number is shown in brackets
;
; Algorithm:
;    	Initialize the sensor number to 0
;    	Initialize a pointer into the RAM at the start of the Sensor Array storage
; Loop   Store %10000001 to the ATDCTL5 (to select AN1 and start a conversion)
;    	Repeat
;       	Read ATDSTAT0
;    	Until Bit SCF of ATDSTAT0 == 1 (at which time the conversion is complete)
;    	Store the contents of ATDDR0L at the pointer
;    	If the pointer is at the last entry in Sensor Array, then
;       	Exit
;    	Else
;       	Increment the sensor number
;       	Increment the pointer
;    	Loop again.
READ_SENSORS    CLR 	SENSOR_NUM    	; Select sensor number 0
          	    LDX 	#SENSOR_LINE  	; Point at the start of the sensor array
         	 
RS_MAIN_LOOP    LDAA	SENSOR_NUM    	; Select the correct sensor input
          		  JSR 	SELECT_SENSOR 	; on the hardware
         	 
        				LDY  	#150          	; 20 ms delay to allow the
        				JSR 	del_50us      	; sensor to stabilize
        				
        				LDAA	#%10000001    	; Start A/D conversion on AN1
        				STAA	ATDCTL5
        				BRCLR ATDSTAT0,$80,*	; Repeat until A/D signals done
        				
        				LDAA	ATDDR0L       	; A/D conversion is complete in ATDDR0L
        				STAA	0,X           	; so copy it to the sensor register
        				CPX 	#SENSOR_STBD  	; If this is the last reading
        				BEQ 	RS_EXIT       	; Then exit
        				
        				INC 	SENSOR_NUM    	; Else, increment the sensor number
        				INX                   	; and the pointer into the sensor array
        				BRA 	RS_MAIN_LOOP  	; and do it again

RS_EXIT   		RTS
;---------------------------------------------------------------------------
;         	Select Sensor
; This routine selects the sensor number passed in ACCA. The motor direction
;  bits 0, 1, the guider sensor select bit 5 and the unused bits 6,7 in the
;  same machine register PORTA are not affected.
; Bits PA2,PA3,PA4 are connected to a 74HC4051 analog mux on the guider board,
;  which selects the guider sensor to be connected to AN1.

; Passed: Sensor Number in ACCA
; Returns: Nothing
; Side Effects: ACCA is changed

; Algorithm:
; First, copy the contents of PORTA into a temporary location TEMP and clear
;    	the sensor bits 2,3,4 in the TEMP to zeros by ANDing it with the mask
;    	11100011. The zeros in the mask clear the corresponding bits in the
;    	TEMP. The 1's have no effect.
; Next, move the sensor selection number left two positions to align it
;    	with the correct bit positions for sensor selection.
; Clear all the bits around the (shifted) sensor number by ANDing it with
;  the mask 00011100. The zeros in the mask clear everything except
;    	the sensor number.
; Now we can combine the sensor number with the TEMP using logical OR.
;  The effect is that only bits 2,3,4 are changed in the TEMP, and these
;  bits now correspond to the sensor number.
; Finally, save the TEMP to the hardware.
SELECT_SENSOR PSHA                  	; Save the sensor number for the moment

            	LDAA	PORTA         	; Clear the sensor selection bits to zeros
            	ANDA	#%11100011    	;
            	STAA	TEMP          	; and save it into TEMP
           	 
            	PULA                  	; Get the sensor number
            	ASLA                  	; Shift the selection number left, twice
            	ASLA                  	;
            	ANDA	#%00011100    	; Clear irrelevant bit positions
           	 
            	ORAA	TEMP          	; OR it into the sensor bit positions
            	STAA	PORTA         	; Update the hardware
            	RTS
 


;*******************************************************************
;* Interrupt Vectors *
;*******************************************************************
          	ORG 	$FFFE
          	DC.W	Entry           	; Reset Vector
          	ORG 	$FFDE
          	DC.W	TOF_ISR         	; Timer Overflow Interrupt Vector


