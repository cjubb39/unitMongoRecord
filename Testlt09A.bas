



























DECLARE SUB MeasZeroA (PressNormCoeff#, BSA%, port%, RM%(), M%)
' ****************************************************************************
' Version  Date      Prog   Change
' -------  --------  -----  --------------------------------------------------
' TESTLT09 06/17/11  ANDYB  Fixed summary format, Fixed QC$, Fixed MP, Added NFSO set
' TESTLT09 06/17/11  ANDYB  Save data in file by S/N .rd and .op
' TESTLOT1 01/07/99  LP     Changed all Non-Lin results to absolute value as per Tim Nunn.
' TESTLOT1 11/25/98  LP     Fixed issues when removing units from Summary. Fixed calculations to work correctly for 3 temps.
'                           Note: Program may need restructuring to fix calculations for more than 4 temperatures.
' TestLot1 10/29/98  LP     Added option to measure ZeroA reading when testing 'G' unit.
' B24V035  10/20/98  LP     Added min and max values to summary report. Changed format.
' B24V034  09/28/98  LP     Incorporating TestLot Program features.
'          09/98     LP     Added Proof Pressure test procedure.  Approval given by M. Patel.
'          10/12/98  LP     Added TCGF, TCR and removed unneeded calculations.
'                           Added Null Shift calculation.
'          10/19/98  LP     Added TestLot Summary Report.
' B24V033  09/22/97  AB     Added QuickTest REPRINT option to the program.
' B24V032  09/05/97  AB     Added QuickTest options to the program.
' B24V031  08/14/97  LP     Added ability to read Rin on multiple ports.
' B24V030  04/23/97  AB     Error that caused last cycle fsp skipping fixed
' B24V029  03/18/97  LP     Added ability to use Load resistance pins on A/D
'                           for pretests and final tests.
' B24V028  01/28/97  LP     Print sens in mV/Bar if not in Barr already.
'                           Made all 75ms delays 100ms.
'          02/27/97  AB     Added multiple reading options for option 99.
'          10/28/96  LP     Added sensitvity in mV/Barr for printout.
' B24V027  10/28/96  AB     Added Command$ to combine executable code
' B24V026  10/03/96  AB     RinCal is seperate sub now
'                           RinCal now is effected by loading of internal R's
'                           RinCalibrate only called before Rin measurements.
'                           Rin can use new coefficients for type 1 boxes.
'                           Added code to select correct resistors.
'                           Added 25ms delay to LoadTrain.
' B24V025  09/01/96  LP     Corrected output report data field lengths.
'                           Added resistors to MeasVinNew.
'                           Fixed MeasVinNew for accuracy.
' B24V024  08/14/96  AB     Fixed MU = NumPos bug. Changed ! to # for better
'                           precision, changed delay from 50 to 75 mSec
'                           Added fix to VinSelect (no 32 Volts).
' B24V023  08/13/96  AB&LP  Fixed Reference transducer order of equation.
'                           Fixed LoadRefTrans to incorporate serial number.
'                           Added continous readings in SetBits sub.
' B24V022  07/25/96  AB&LP  Fixed Rin Measurement Problems
' B24V021  07/08/96  AB&LP  Removed calibration routines to separate program.
' B24V020  07/08/96  AB&LP  Fixed up calibration routine. Added curve fitting.
' B24V019  07/03/96  AB     Added Calibration Routine.
' B24V018  07/02/96  AB&LP  Added IV to Savedata and Readdata.  Correct
'                           MeasVoutNew, Added Warmup time function.
'                           Added RefTrans savedata safety routine.
' B24V017  07/01/96  AB     Removed All Old Routines. Removed Optional Delays.
'                           Corrected Amp Vout for Debug. New MeasVout Routine.
' B24V016  06/27/96  AB     Added Program exiting options
' B24V015  06/26/96  AB     Corrected A2D Routines, Removed NP, MR%, QT%, OP$,
'                           RV, RI$, OP, IS1, VD, DP%, NEWPARAM%, OLDPARAM%,
'                           Added a New RioArray routine.
' B24V014  06/24/96  AB     Added a Data Selection Algorithm
' B24V013  06/19/96  AB&LP  Fixed ITER%, Fixed Amp$ and Reg$, Cleaned up a bit
' B24V012  06/12/96  AB     Setbits, <>5v Pressure Port Added
' B24V011  06/10/96  LP&AB  Added Samples% to Rin, Vin, etc. fixed set_bits
' B24V010  06/07/96  LP&AB  Fixed Rin, MeasVin.
' B24V009  06/05/96  AB     PS Errors Corrected, Multiple session fix
' B24V008  06/04/96  AB     Error Handling Replaced, Delay Replaced
' B24V007  05/30/96  AB     New a2d routine plus more stuff.
' B24V006  05/29/96  AB&LP  Lot's o' functionality added. Tested OK.
' B24V005  05/28/96  AB&LP  Pressure TRANSDUCER code added.
' B24V002  05/24/96  LP     Added code to use New A/D Boxes. Tested ok.
' B24V001  05/20/96  AB     Major Cleanup in Prep for Modification
' BASE24   10/28/94  KB     MADE IT 24 POSITION
' NEWBASE  12/15/93  KB     THIS IS THE OFFICIAL BASE NOW.
' BASET    11/22/93  KB     TRYING TO GET ACCURATE A/D - ADDED TOGGLE
' BASE     11/17/93  KB     Trying to get accurate a/d results
' NEWBASE  11/11/93  KB
' BASECLN  11/05/93  KB
' BASENEW  11/02/93  KB
' COMBASE  ??/??/??  HB     Howie's Base Program for the C64
'*****************************************************************************

' ***** To Run under QB itself *****
' QB B24V???.BAS /L ALTQUICK.QLB

' ***** Compile and link options *******
' BC /O/S/AH B24V???.BAS /sw-dev/;
' LINK B24V???.OBJ,,,altquick.lib;

' ***** Support Programs *****
' C2BOXES.BAS   --   Powers A2D Boxes to 1.26V.
'                    Resets Boxes.cfg and Boxes2.cfg.
'                    Saves system bootup time.
' Cflags.bas    --   Resets all dos ipc registers.

' ***** Complile for Channels A or B *****
' Set Channel$ = to "A" or "B"

' ***** DECLARATION OF External SUBROUTINES *****
' ***** Assembly-Language Toolbox for QuickBASIC/VisualBASIC DOS

        DECLARE FUNCTION Attrib% (Filespec$)            ' File Attributes
        DECLARE FUNCTION GetFlag% (flag%)               ' Get Value for Flag
        DECLARE FUNCTION GetDir$ (Drive$)               ' Current dir
        DECLARE FUNCTION CapsLock% (BYVAL Switch%)      ' Set Caps Lock Key
        DECLARE FUNCTION NumLock% (BYVAL Switch%)       ' Set Num Lock Key
        DECLARE FUNCTION ScrLock% (BYVAL Switch%)       ' Set Scroll Lock Key
        DECLARE SUB CheckPrinter (Prntr%, Ready%)       ' Check Printer
        DECLARE SUB DELAY (BYVAL MilliSeconds%)         ' Delay in mSec
        DECLARE SUB SetDrive (PathName$)                ' Change dir
        DECLARE SUB Setflag (flag%, Setting%)           ' Set Value of flag

' ***** DECLARATION OF Internal SUBROUTINES *****
       
        DECLARE FUNCTION DataStdDev# (Samples%, TestData#(), DMean#)
        DECLARE FUNCTION SetRefPressure# (P, BSA%, port%, RM%(), M%)
        DECLARE FUNCTION MeasVoutNew# (BSA%, port%, RM%(), M%)
        DECLARE FUNCTION ReadA2DNew (BSA%, port%, RM%(), M%)
        DECLARE FUNCTION FSTR$ (VALUE, SP.BEFORE, DIG.AFTER)
        DECLARE FUNCTION DataSelect# (Samples%, TestData#())
        DECLARE FUNCTION DataMean# (Samples%, TestData#())
        DECLARE FUNCTION MEASROUT (BSA%, port%, RM%(), M%)
        DECLARE FUNCTION MEASRIN (BSA%, port%, RM%(), M%)
        DECLARE FUNCTION MEASCMV (BSA%, port%, RM%(), M%)
        DECLARE FUNCTION FORMATSTR$ (X, LF, LI)
        DECLARE FUNCTION Measpressure# ()
        DECLARE SUB VOARRAY (PressNormCoeff#, BSA%, port%, RM%(), M%)
        DECLARE SUB MeasVinNew (BSA%, port%, RM%(), M%, PReport$)
        DECLARE SUB PROOFPRESSURE (BSA%, port%, RM%(), M%)
        DECLARE SUB RinCalibrate (BSA%, port%, RM%(), M%)
        DECLARE SUB SetPowerLow (BSA%, port%, RM%(), M%)
        DECLARE SUB RioArrayNew (BSA%, port%, RM%(), M%)
        DECLARE SUB SINGLETEMP (BSA%, port%, RM%(), M%)
        DECLARE SUB LOADTRAIN (BSA%, port%, RM%(), M%)
        DECLARE SUB PSTestNew (BSA%, port%, RM%(), M%)
        DECLARE SUB QUICKTEST (BSA%, port%, RM%(), M%)
        DECLARE SUB TEMPSTAB (BSA%, port%, RM%(), M%)
        DECLARE SUB CMVARRAY (BSA%, port%, RM%(), M%)
        DECLARE SUB FINALCAL (BSA%, port%, RM%(), M%)
        DECLARE SUB BoxTest (BSA%, port%, RM%(), M%)
        DECLARE SUB RUNTEST (BSA%, port%, RM%(), M%)
        DECLARE SUB InitA2D (BSA%, port%, RM%(), M%)
        DECLARE SUB PRETEST (BSA%, port%, RM%(), M%)
        DECLARE SUB SetBits (BSA%, port%, RM%(), M%)
        DECLARE SUB Change (BSA%, port%, RM%(), M%)
        DECLARE SUB VOUT (BSA%, port%, RM%(), M%)
        DECLARE SUB ClearTrain (RM%(), M%)
        DECLARE SUB LoadPowSupVoltages ()
        DECLARE SUB LoadCOEFFICIENTS ()
        DECLARE SUB LoadBoxesAvail ()
        DECLARE SUB LoadPosAddress ()
        DECLARE SUB DEFPRESSURES ()
        DECLARE SUB LoadRefTrans ()
        DECLARE SUB InitCTM (BSA%)
        DECLARE SUB FINALREPORT ()
        DECLARE SUB DATACHECK ()
        DECLARE SUB VINSELECT ()
        DECLARE SUB READDATA ()
        DECLARE SUB DEFTEMPS ()
        DECLARE SUB SAVEDATA ()
        DECLARE SUB LOADSENS ()
        DECLARE SUB ECHODATA ()
        DECLARE SUB RESULTS ()
          DECLARE SUB SummaryReport ()
          DECLARE SUB TestLotReport ()
          DECLARE SUB PrintProofReport (iNumPressures AS INTEGER)
          DECLARE SUB RUNPROOFTEST (BSA%, port%, RM%(), M%)
          DECLARE SUB TESTLOT (BSA%, port%, RM%(), M%)


' ***** Set Stack area  *****

CLEAR 11000
   
   
' ***** Set Up Keyboard *****

   numon% = NumLock%(1)

   capsOn% = CapsLock%(1) ' Turns CAPS LOCK on

   scrLon% = ScrLock%(0)


' ***** Set Channel for Base to operate *****

DIM SHARED Channel$                             ' CTM05 Card Address
DIM StartUp$
                            
StartUp$ = UCASE$(LTRIM$(RTRIM$(COMMAND$)))     'Get Command line value

SELECT CASE StartUp$
        CASE "/A"
                Channel$ = "A"                  'Hex address &H300
        CASE "/B"
                Channel$ = "B"                  'Hex address &H380
        CASE ELSE
                Channel$ = "A"                  'Hex address &H300
END SELECT

' ***** Set debug flag *****

DIM SHARED Debug                                ' Debugging flag
Debug = 0                                       '-1 BYPASS a/d subs and
                                                'generate typical data

' ***** Set Memory Flag to prevent multiple sessions *****

IF NOT Debug THEN
baseA% = 111
baseB% = 112

flag% = 0
FOR I% = 16 TO 1 STEP -1
        result% = GetFlag%(I%)
        IF Channel$ = "A" THEN
                IF result% = baseA% THEN
                        PRINT "Program already started flag "; I%; " is set"
                        END
                END IF
        ELSE
                IF result% = baseB% THEN
                        PRINT "Program already started flag "; I%; " is set"
                        END
                END IF
        END IF
        IF Channel$ = "A" THEN
                IF result% = 0 AND result% <> baseB% THEN flag% = I%
        ELSE
                IF result% = 0 AND result% <> baseA% THEN flag% = I%
        END IF
NEXT I%
IF flag% <> 0 THEN
        IF Channel$ = "A" THEN
                CALL Setflag(flag%, baseA%)
        ELSE
                CALL Setflag(flag%, baseB%)
        END IF
ELSE
        PRINT "Cannot set memory flag.  Ending Program. "
        END
END IF
END IF

' ***** Set Dos Parameters *****

        currentdir$ = GetDir$("C")
        CALL SetDrive(currentdir$ + "\DATA")
        COLOR 15, 3
        SCREEN 0
        WIDTH 80, 25
        CLS

' ***** Data Statements ******

        DATA "PSI ","BAR ","KPA "
        DATA "TORR ","IN H20 "
        DATA 1,14.5,0.145,0.01934,0.03613
        DATA 18,17,16,15,14,13,12,11
        DATA 26,25,24,23,22,21,20,19
        DATA 70,69,68,67,66,65,64,63
        DATA 32,28,24,20,15,12,10,8,7.5
        DATA 7,6,5,4,3.5,3,2.5,2,1.26

' data 1 Pressure units
' data 2 Pressure units
' data 3 Pressure conversion array
' data 4 Address port array data
' data 5 Address port array data
' data 6 Address port array data
' data 7 Power Supply Levels
' data 8 Power Supply Levels

' ***** ARRAY SIZES *****
       
        DIM SHARED NV                   ' Number of PS voltages
        DIM SHARED MU                   ' Maximum # of Units
        DIM SHARED MT                   ' Maximum Temperatures
        DIM SHARED MP                   ' Maximum Pressures per tempurature
        
        MU = 24         ' Maximum # of Units
        M% = 70         ' Maximum bits in a/d box control train
        MT = 13         ' Maximum Temperatures
        NV = 17         ' Number of Power Supply Voltages
        MP = 31         ' Maximum Pressures per tempurature
      
' ***** DIMENSION VARIABLES AND ARRAYS *****
       
        DIM SHARED glbArrZeroA(MU, MT) AS SINGLE        ' array of zeroA measurements
        DIM SHARED glbbolMeasZeroA AS INTEGER    ' True if tester wants to measure Zero A.
        DIM SHARED glbArrLin(MU) AS SINGLE  ' array of linearity results.
        DIM SHARED glbArrNullShift(MU, 3) AS SINGLE ' array of null shifts
        DIM SHARED glbArrSpanShift(MU, 3) AS SINGLE
        DIM SHARED glbArrTCR(MU, 3) AS SINGLE       ' array of TCR's
'        DIM SHARED glbArrTCGF(MU, 3)  AS SINGLE     ' array of TCGF's
        DIM SHARED glbProofPrVal(MP) AS SINGLE    ' holds Proof pressure values
        DIM SHARED glbArrVoProof(MU, MP)  AS SINGLE ' Array of proof pressure readings (used in TESTLOT)
        DIM SHARED Samples%             ' Number of reading taken per test
        DIM RM%(M%)                     ' A/D Box control array (train)
        DIM SHARED MM%(MU)              ' Test Port Address Array
        DIM SHARED VI(NV)               ' Power Supply Voltage Array
        DIM SHARED RI(MU, -1 TO MT)     ' Input Resistance Array
        DIM SHARED Ro(MU, -1 TO MT)     ' Output Resistance Array
        DIM SHARED CMVGTB(MU, -1 TO MT) ' CM Voltage GTB Array
        DIM SHARED CMVWTB(MU, -1 TO MT) ' CM Voltage WTB Array
        DIM SHARED VO(MU, MP, -1 TO MT) ' Voltage Out Array
        DIM SHARED P0                   ' Pressure used for calc absolute 0
        DIM SHARED P(5)                 ' Pressure Conversion Array
        DIM SHARED PR(MP, 2)            ' Applied pressure array
        DIM SHARED TM(-1 TO MT)         ' Applied temperature array
        DIM SHARED Vs                   ' Power supply actual
        DIM SHARED Vs(MT)               ' Power supply actual at test temp
        DIM SHARED SN(MU)               ' Serial # array
        DIM SHARED SE(MU)               ' Array used durring correct null
        DIM SHARED PF(MU)               ' Proof pressure array
        DIM SHARED IT                   ' Index for current temp (6 max)
        DIM SHARED IU                   ' Index for current unit (24 max)
        DIM SHARED IP                   ' Index for current pressure (13 max)
        DIM SHARED TR                   ' Room Temperature
        DIM SHARED LD                   ' Load Resistor Value
        DIM SHARED a#                   ' Coefficient for Rin
        DIM SHARED B#                   ' Coefficient for Rin
        DIM SHARED R1K#                  ' Measured R1 1000
        DIM SHARED R4K#                  ' Measured R4 4000
        DIM SHARED P                    ' Current Pressure
        DIM SHARED PM                   ' Full Scale Pressure Entered
        DIM SHARED VE                   ' Power Supply Voltage Level Entered
        DIM SHARED VT                   ' Sim Power Supply Voltage Level
        DIM SHARED LT                   ' Last Test Number
        DIM SHARED NT                   ' number of temp steps
        DIM SHARED HT                   ' index of high temp
        DIM SHARED IV                   ' Current power supply setting
        DIM SHARED O                    ' Divide by zero protection
        DIM SHARED NU                   ' Number of units under test
        DIM SHARED PF                   ' Proof Pressure
        DIM SHARED PR                   ' Selected pressure type
        DIM SHARED AP                   ' Ambient pressure
        DIM SHARED SP                   ' Pressure steps
        DIM SHARED LP
        DIM SHARED PP
        DIM SHARED NP(2)
        DIM SHARED MP(2)
        DIM SHARED SV                   ' Preselected voltage for test type
        DIM SHARED DATFILOPT%           ' Testing selecting flag
        DIM SHARED filegood%            ' Data file good flag -1 = none
        DIM SHARED MenuOp%              ' Menu selection
        DIM SHARED FINISHTST%           ' Finish test flag
        DIM SHARED STARTOVER%           ' Start over flag
        DIM SHARED SC$                  ' Spacing constant
        DIM SHARED ZS$                  ' Spacing constant
        DIM SHARED TB$                  ' Default answer for tested by
        DIM SHARED PN$                  ' Default answer for kulite part #
        DIM SHARED CN$                  ' Default answer for cust part #
        DIM SHARED LOT$                 ' Default answer for lot #
        DIM SHARED DATFILE$             ' Data file type
        DIM SHARED P$(5)                ' Pressure Units
        DIM SHARED SN$(MU)              ' Serial Number Array Character
        DIM SHARED PR$                  ' "PSI"
        DIM SHARED PT$                  ' "PSI"
        DIM SHARED AMP$                 ' Amplified flag
        DIM SHARED Reg$                 ' Regulated flag
        DIM SHARED PF$                  ' Default for proof pressure test
        DIM SHARED BD$                  ' Used as a bad port designator
        DIM SHARED LD$                  ' Load Resistor flag
        DIM SHARED NEWPAGE$             ' Form feed command
        DIM SHARED AT$                  ' Default pressure type - Top
        DIM SHARED AB$                  ' Default pressure type - Bottom
        DIM SHARED QC$                  ' Program information
        DIM SHARED DAT$                 ' Date program executed
        DIM SHARED LTD$                 ' Date program terminated
        DIM SHARED DP$                  ' Default Differential Unit Selection
        DIM SHARED RP$                  ' Default for repeatability test
        DIM SHARED RTN                  ' Default Room temp return pressures
        DIM SHARED TP$                  ' Default temperature units
        DIM SHARED C$                   ' Output control string
        DIM SHARED S$                   ' Output control string
        DIM SHARED Range%               ' Specifies measurement range
        DIM SHARED Boxid AS STRING      ' A/D Box Serial Number
        DIM SHARED CO(10, 10) AS DOUBLE ' Coefficients array for A/D box
        DIM SHARED COX(10, 10) AS DOUBLE ' TEMP Coefficients array for A/D box
        DIM SHARED CoeffFile$           ' Coefficients file name for A/D box
        DIM SHARED BoxType%             ' New or Old Generation
        DIM SHARED RTDATE$              'Date data file created
        DIM SHARED RTNUM#               'Number of Reference transducer
        DIM SHARED RTCOEF3#             'Third order coeff
        DIM SHARED RTCOEF2#             'Second order coeff
        DIM SHARED RTCOEF1#             'First order coeff
        DIM SHARED RTCOEF0#             'Zero order coeff
        DIM SHARED RTFSP#               'Ref Trans FSO
        DIM SHARED PressNormCoeff#      'Proof Pressure Coefficient
        DIM SHARED warmup#              'delay for warmup time
        DIM SHARED stabwait#            'set new stability time
        DIM SHARED NWTIME               'new stability time
        DIM SHARED RTPTYPE$             'Pressure type for ref transducer
        DIM SHARED StopNow%             'Flag to stop test after temperature
        DIM SHARED PportV#              'Pressure Port Voltage
        DIM SHARED Continue$            'Continue or Exit
        DIM SHARED STTime               'Computer boot up time (in sec)
        DIM SHARED STime                'Program start time
        DIM SHARED Hr                   'Boot up time hour
        DIM SHARED Mn                   'Boot up time minute
        DIM SHARED PportVX#             'Temporary P Port Voltage
        DIM SHARED DataFactor#          'Data selection/rejection criteria
        DIM SHARED VIN99
        DIM SHARED ZAVEDSET$            'SAVE testlot DATA TO FILE
        DIM SHARED proof$            'SAVE Overpressure DATA TO FILE

' ***** OPTIONS *****

        Samples% = 5     ' The number of reading taken per test.

' ***** INITIALIZE VARIABLES *****
                                 
        QC$ = "Program Version: TESTLT09 Released on 06-17-2011. Updated by AndyB"
        IF Channel$ = "A" THEN
                BSA% = &H300            ' Port address CTM05 Card Channel A
        ELSE
                BSA% = &H380            ' Port address CTM05 Card Channel B
        END IF
        port% = 0                       ' Initialize Port address
        TR = 80                         ' Default room temperature value
        O = 1E-10                       ' Divide by zero protection
        IV = 11                         ' Default Power Supply Setting
        DAT$ = DATE$                    ' Date program initiated
        NU = 1                          ' Number of units under test
        LD$ = "N"                       ' Default for presense of load R
        LD = 0                          ' Default Load Resistor Value
        DP$ = "N"                       ' Default Differential Unit Selection
        AMP$ = "N"                      ' Default amplified selection
        Reg$ = "N"                      ' Default regulated selection
        RP$ = "N"                       ' Default for repeatability test
        PF$ = "N"                       ' Default for proof pressure test
        RTN = 0                         ' Default Room temp return pressures
        AT$ = "A"                       ' Default pressure type - Top
        AB$ = "G"                       ' Default pressure type - Bottom
        PT$ = "PSI"                     ' Pressure Type prefix
        PR$ = "PSI"                     ' ?????
        TP$ = "F"                       ' Default temperature units
        SC$ = "                  "      ' Spacing constant for report
        ZS$ = "000000000000000000"      ' Spacing constant for report
        C$ = CHR$(13)                   ' Report Output Control
        NEWPAGE$ = CHR$(12)             ' Report Output Control
        S$ = "       "                  ' Spacing constant for report
        SN$(1) = "1"                    ' Default Serial # for first position
        BD$ = "BAD        "             ' Used as a bad port designator
        TB$ = "."                       ' Default answer for tested by
        PN$ = "."                       ' Default answer for kulite part #
        CN$ = "."                       ' Default answer for cust part #
        LOT$ = "."                      ' Default answer for lot #
        IT = 0                          ' Default value for current temp
        RTNUM# = 0                      ' Default setting for no ref trans
        RTFSP# = 0                      ' Default setting for no ref trans
        warmup# = -1                    ' Default is on
        stabwait# = -1                  ' Default is on
        NWTIME = 5                      ' Default
        StopNow% = 0                    ' Flag to stop test after temperature
        Continue$ = "Y"                 ' Default is Yes
        Hr = GetFlag%(10)               ' Boot up hour
        Mn = GetFlag%(11)               ' Boot up minute
        STTime = Hr * 60 * 60 + Mn * 60 ' Boot up time in seconds
        STime = TIMER                   ' Program start time
        DataFactor# = .001#              'Data selection/rejection criteria
       

   '**** constants used to locate coefficients within the array
   '  ** e.g.: Co(VOUT200MV,2) indicates the second coefficient
   '           for the 200 mV range.

        CONST VOUT80MV = 1
        CONST Vout200mV = 2
        CONST Vout400mV = 3
        CONST VOUTVDC = 4
        CONST PSVOLT = 5
        CONST PSVOLT80MV = 5
        CONST PSVOLT200MV = 6
        CONST PSVOLT400MV = 7
        CONST RIN = 8
        CONST ROUT = 9
        CONST RinCal = 10
        CONST CMVGTOB = 11

' ***** Define equations *****

        DEF fnx (X) = INT(1000 * X + .5 * SGN(X)) / 1000        'Round to .001
        DEF FNR (X) = INT(X + .5 * SGN(X))                      'Round to 1

' ***** MNEMONICS *****

        FINISHTST% = 2
        STARTOVER% = 4
        filegood% = -1


' ***** load A/D connected to this machine:
IF NOT Debug THEN CALL LoadBoxesAvail

' ***** Load coefficients for this box
IF NOT Debug THEN CALL LoadCOEFFICIENTS

' ***** Load POWER SUPPLY voltage ARRAY (POSSIBLE VOLTAGES) *****
IF NOT Debug THEN CALL LoadPowSupVoltages

' ***** Load TEST PORT address array *****
IF NOT Debug THEN CALL LoadPosAddress


' ***** Load Pressure Units Array *****

        FOR I = 1 TO 5
             READ P$(I)
        NEXT I

  
' ***** Load Pressure conversion array *****
       
        FOR I = 1 TO 5
          READ P(I)
        NEXT I

 IF Debug THEN

' ***** Load TEST PORT address array *****
  
        FOR I = 1 TO MU
          READ MM%(I)
        NEXT I

' ***** Load POWER SUPPLY voltage ARRAY (POSSIBLE VOLTAGES) *****

        FOR I = 0 TO NV
          READ VI(I)
        NEXT I

END IF
       
' ***** Initialize A to D board *****

CALL InitA2D(BSA%, port%, RM%(), M%)

' ***** Main Menu *****

MENU:
        DO
          RTN = 0                               'Fixes fsp cycle skipping error
          StopNow% = 0
          Continue$ = "Y"
          CALL SetPowerLow(BSA%, port%, RM%(), M%)
          CLS
          PRINT QC$
          PRINT
          PRINT
          IF NOT Debug THEN
                PRINT "Channel "; Channel$; " Is Connected to box #";
                PRINT Boxid$ + "."
          ELSE
                PRINT "Debug Mode Active"
          END IF
          IF RTNUM# = 0 THEN
                PRINT
                PRINT
          ELSE
                PRINT "REFERENCE TRANSDUCER #"; RTNUM#; "IN USE"
                PRINT "FSP ="; RTFSP#; PR$
          END IF
          PRINT
          PRINT "TESTING OPTIONS:"
          PRINT
          PRINT "0 - END PROGRAM"
          PRINT "1 - POWER SUPPLY DIAGNOSTIC"
          PRINT "2 - CONFIGURE REFERENCE TRANSDUCER"
          PRINT "3 - LINEARITY/TEMP QUICK TEST"
          PRINT "4 - PRETEST FOR COMP"
          PRINT "5 - FINAL CALIBRATION TEST"
          PRINT "7 - FINAL CALIBRATION REPORT"
          PRINT "8 - TESTLOT PROGRAM"
          PRINT "9 -   PROOF PRESSURE TEST"
          PRINT "10 -  TESTLOT REPORT BY UNIT"
          PRINT "11 -  TESTLOT SUMMARY REPORT"
          PRINT
           INPUT "SELECT OPTION(1-7)"; MenuOp%
          CLS
          SELECT CASE MenuOp%
          CASE 0
            EXIT DO
          CASE 1
                CALL PSTestNew(BSA%, port%, RM%(), M%)
          CASE 2
            CALL LoadRefTrans
          CASE 3
            CALL QUICKTEST(BSA%, port%, RM%(), M%)
          CASE 4
            CALL PRETEST(BSA%, port%, RM%(), M%)
          CASE 5
            CALL FINALCAL(BSA%, port%, RM%(), M%)
          CASE 7
            CALL FINALREPORT
          CASE 8
            CALL TESTLOT(BSA%, port%, RM%(), M%)
          CASE 9
            CALL RUNPROOFTEST(BSA%, port%, RM%(), M%)
          CASE 10
            CALL TestLotReport
          CASE 11
            CALL SummaryReport
          CASE 99
            CALL BoxTest(BSA%, port%, RM%(), M%)
          CASE -13
            CALL Change(BSA%, port%, RM%(), M%)
          END SELECT
        LOOP

' ***** End of Program *****
       
CLS
PRINT "POWERING DOWN A/D BOX"
CALL SetPowerLow(BSA%, port%, RM%(), M%)
CALL SetDrive(currentdir$)
CALL Setflag(flag%, 0)
END

' ****************************************************************************

SUB BoxTest (BSA%, port%, RM%(), M%)
DIM Sel%
DIM YN$
DIM RATIO#
DIM VO
DIM RI
DIM Ro
DIM CMVGTB
DIM VCMV
DIM CMVWTB
DIM IUF
DIM IUL
DIM RoData#(24)                     'Rout Data Array
DIM RoA#                            'Rout Ave
DIM RoS#                            'Rout Std Dev
DIM RiData#(24)                     'Rin data Array
DO
CLS
PRINT "Test Options"
PRINT
PRINT "0) Exit"
PRINT "1) Change Box Number"
PRINT
PRINT "Single Test Options"
PRINT
PRINT "2) Test Pressure Port"
PRINT "3) Vin Measurement"
PRINT "4) Vout Measurement"
PRINT "5) Rin Measurement"
PRINT "6) Rout Measurement"
PRINT "7) CMV Measurement"
PRINT
PRINT "Multiple Test Options"
PRINT
PRINT "8) Rout Measurement"
PRINT "9) Vin Heating Effects"
PRINT "10) Rin Measurement"
PRINT
INPUT "Please select action"; Sel%
SELECT CASE Sel%
        CASE 0
           EXIT DO
        CASE 1
            INPUT "Enter the Boxid number"; Boxid$
            INPUT "Enter the number of Positions"; NumPos
            ' ***** Load coefficients for this box
            IF NOT Debug THEN CALL LoadCOEFFICIENTS

            ' ***** Load POWER SUPPLY voltage ARRAY (POSSIBLE VOLTAGES) *****
            IF NOT Debug THEN CALL LoadPowSupVoltages

            ' ***** Load TEST PORT address array *****
            IF NOT Debug THEN CALL LoadPosAddress
           
        CASE 2
           IF RTNUM# = 0 THEN
                PRINT "NO REFERENCE TRANSDUCER SELECTED"
                INPUT "PRESS ENTER TO CONTINUE"; YN$
           ELSE
                P = RTFSP#
                RATIO# = SetRefPressure#(P, BSA%, port%, RM%(), M%)
                PRINT
                PRINT "Ratio ="; RATIO#
                INPUT "PRESS ENTER TO CONTINUE"; YN$
           END IF
        CASE 3
           CALL VINSELECT
           INPUT "Correct for Wire resistance"; YN$
           IF YN$ = "Y" THEN
             INPUT "Wire resistance value"; RW
             INPUT "Load resistance value"; RL
           END IF
           PRINT "PRESS B TO STOP READINGS": PRINT
           IF YN$ = "Y" THEN
             PRINT "Rload ="; RL; "   Rwire ="; RW; "     Vin =";
           ELSE
             PRINT "Vin =";
           END IF
           XPOS = POS(0)
           YPOS = CSRLIN
           temp% = Samples%
           Samples% = 1
           DO
             CALL MeasVinNew(BSA%, port%, RM%(), M%, "99")
             LOCATE YPOS, XPOS
             IF YN$ = "Y" THEN
               PRINT USING "##.#### ##.####"; VIN99 * RL / (RW + RL); VIN99
             ELSE
               PRINT VIN99
             END IF
           LOOP WHILE INKEY$ <> "B"
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           CALL SetPowerLow(BSA%, port%, RM%(), M%)
           Samples% = temp%
        CASE 4
           INPUT "What is the test port number"; IU
           INPUT "Add V Divider (Y/N)"; AMP$
           INPUT "Correct for Input Voltage (Y/N)"; Reg$
           IF Reg$ = "N" THEN
                Reg$ = "Y"
           ELSE
                Reg$ = "N"
           END IF
           CALL VINSELECT
           CALL MeasVinNew(BSA%, port%, RM%(), M%, "Y")
           PRINT "PRESS B TO STOP READINGS": PRINT
           PRINT "Port#"; IU; "Vout =";
           XPOS = POS(0)
           YPOS = CSRLIN
           DO
                VO = MeasVoutNew#(BSA%, port%, RM%(), M%)
                LOCATE XPOS, YPOS
                PRINT VO
           LOOP WHILE INKEY$ <> "B"
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           CALL SetPowerLow(BSA%, port%, RM%(), M%)
        CASE 5
           INPUT "What is the test port number"; IU
           CALL VINSELECT
           CALL MeasVinNew(BSA%, port%, RM%(), M%, "Y")
           IF BoxType% = 1 THEN CALL RinCalibrate(BSA%, port%, RM%(), M%)
           RI = MEASRIN(BSA%, port%, RM%(), M%)
           PRINT "Port#"; IU; "Rin ="; RI
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           CALL SetPowerLow(BSA%, port%, RM%(), M%)
        CASE 6
           INPUT "What is the test port number"; IU
           Ro = MEASROUT(BSA%, port%, RM%(), M%)
           PRINT "Port#"; IU; "Rout ="; Ro
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           CALL SetPowerLow(BSA%, port%, RM%(), M%)
        CASE 7
           INPUT "What is the test port number"; IU
           CALL VINSELECT
           CALL MeasVinNew(BSA%, port%, RM%(), M%, "Y")
           CMVGTB = MEASCMV(BSA%, port%, RM%(), M%)
           VCMV = MeasVoutNew#(BSA%, port%, RM%(), M%)
           CMVWTB = CMVGTB - VCMV / 1000
           PRINT "Port#"; IU; "CMVGTB ="; CMVGTB; "CMVWTB ="; CMVWTB
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           CALL SetPowerLow(BSA%, port%, RM%(), M%)
        CASE 8
           INPUT "What is the first port number"; IUF
           INPUT "What is the last port number"; IUL
           WIDTH 80, 50
           CLS
           FOR IU = IUF TO IUL
             Ro = MEASROUT(BSA%, port%, RM%(), M%)
             PRINT "Port#"; IU; "Rout ="; Ro
             RoData#(IU - IUF + 1) = Ro
           NEXT IU
           RoA# = DataMean((IUL - IUF + 1), RoData#())
           RoS# = DataStdDev((IUL - IUF + 1), RoData#(), RoA#)
           PRINT "Rout Average ="; RoA#; "   Rout Std ="; RoS#
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           WIDTH 80, 25
           CALL SetPowerLow(BSA%, port%, RM%(), M%)
        CASE 9
           CALL VINSELECT
           INPUT "Wire resistance value"; RW
           INPUT "Load resistance value"; RL
           PRINT "PRESS B TO STOP READINGS": PRINT
           STime = TIMER
           PRINT
           XPOS = POS(0)
           YPOS = CSRLIN
           temp% = Samples%
           Samples% = 1
           F% = FREEFILE
           OPEN "VINTIME.TXT" FOR OUTPUT AS F%
           DO
             CALL MeasVinNew(BSA%, port%, RM%(), M%, "99")
             LOCATE YPOS, XPOS
             ETime = TIMER - STime
             VinC = VIN99 * RL / (RW + RL)
             PRINT USING "& #####.## & ##.####"; "Elapsed Time ="; ETime; "    Vin Corrected ="; VinC
             PRINT #F%, USING "& #####.## & ##.####"; "Elapsed Time ="; ETime; "    Vin Corrected ="; VinC
           LOOP WHILE INKEY$ <> "B"
           CLOSE #F%
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           CALL SetPowerLow(BSA%, port%, RM%(), M%)
           Samples% = temp%
        CASE 10         ' Rin from port x to y
           CALL VINSELECT
           CALL MeasVinNew(BSA%, port%, RM%(), M%, "Y")
           IF BoxType% = 1 THEN CALL RinCalibrate(BSA%, port%, RM%(), M%)
           INPUT "What is the first port number"; IUF
           INPUT "What is the last port number"; IUL
           WIDTH 80, 50
           CLS
           IF IUF > IUL THEN
            X = IUL
            IUL = IUF
            IUF = X
           END IF
           FOR IU = IUF TO IUL
             RI = MEASRIN(BSA%, port%, RM%(), M%)
             PRINT "Port#"; IU; "Rin ="; RI
             RiData#(IU - IUF + 1) = RI
           NEXT IU
           RiA# = DataMean((IUL - IUF + 1), RiData#())
           RiS# = DataStdDev((IUL - IUF + 1), RiData#(), RiA#)
           PRINT "RIN Average ="; RiA#; "   RIN Std ="; RiS#
           CALL SetPowerLow(BSA%, port%, RM%(), M%)
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           WIDTH 80, 25
          
END SELECT
             
LOOP
END SUB

SUB Change (BSA%, port%, RM%(), M%)

'***** ALLOW CHANGING PROGRAM VARIABLES DURING OPERATION *****'

        INPUT PassWD$
        IF PassWD$ <> "go2hell" THEN EXIT SUB

        DO
                CLS
        
                PRINT "Program variable control mode now active"
                PRINT
                PRINT "status of variables"
                PRINT
                PRINT " 0) Exit"
                PRINT " 1) Samples% ="; Samples%
                PRINT " 2) Warmup delay ="; warmup#
                PRINT " 3) Adjust Stability wait time ="; stabwait#
                PRINT " 4) New stability waiting time ="; NWTIME
                PRINT " 5) Maximum Temperatures ="; MT
                PRINT " 6) Maximum Pressures ="; MP
                PRINT " 7) Set bits and load"
                PRINT " 8) Change data selection/rejection criteria ="; DataFactor#
                PRINT
                INPUT "Which variable would you like to change"; DUMBY
                SELECT CASE DUMBY
                        CASE 0
                          EXIT DO
                        CASE 1
                          INPUT "Samples%"; Samples%
                        CASE 2
                          INPUT "Warmup delay"; warmup#
                        CASE 3
                          INPUT "Adjust Stability wait time"; stabwait#
                        CASE 4
                          INPUT "New stability waiting time"; NWTIME
                        CASE 5
                          INPUT "Maximum Temperatures"; MT
                        CASE 6
                          INPUT "Maximum Pressures"; MP
                        CASE 7
                          CALL SetBits(BSA%, port%, RM%(), M%)
                        CASE 8
                          INPUT "Data Factor"; DataFactor#
                END SELECT
        LOOP
END SUB

SUB ClearTrain (RM%(), M%)
       
        'TITLE:         CLEARTRAIN
        'COEFFICIENTS:  RM%()   - The a/d box control array (Train Array)
        '               M%      - The maximum element # for RM%()
        'USE:           CALL CLEARTRAIN(RM%(),M%)
        'NOTE:          This routine loads all train elements with 0

        FOR Q% = 1 TO M%
        RM%(Q%) = 0
        NEXT Q%
END SUB

SUB CMVARRAY (BSA%, port%, RM%(), M%)

DIM YN$

CALL MeasVinNew(BSA%, port%, RM%(), M%, "N")


' **** COMMON MODE VOLTAGE ***
        PRINT
        PRINT "MEASURING C.M.V. @"; VT; "VDC  P.S.="; fnx(Vs)
        IF VT <> VE THEN PRINT "CORRECTED TO "; VE; " VDC  INPUT"
        PRINT
        PRINT "PLEASE SET PRESSURE TO ATMOSHERIC (0G)."
        INPUT "PRESS ENTER TO CONTINUE..."; YN$
        PRINT
        PRINT " PORT         CMV GR TO BL   CMV WH TO BL "
        PRINT "  #               (VDC)          (VDC)    "
        PRINT "-----         ------------   ------------ "
        CMVFORM$ = " ###            ###.###        ###.###    "
        FOR IU = 1 TO NU
          VX = MEASCMV(BSA%, port%, RM%(), M%)
          CMVGTB(IU, IT) = fnx(VX)
          VX = MeasVoutNew#(BSA%, port%, RM%(), M%)
          CMVWTB(IU, IT) = CMVGTB(IU, IT) - VX / 1000
          PRINT USING CMVFORM$; IU; CMVGTB(IU, IT); CMVWTB(IU, IT)
        NEXT IU
        PRINT
END SUB

SUB DATACHECK

DIM YN$

' **** DATA FILE CHECK
        filegood% = Attrib%(DATFILE$)
        IF filegood% <> -1 THEN
          OPEN DATFILE$ FOR INPUT AS #10
          INPUT #10, X$, LDT$, X$, X$, X$, X$, X$, X$, X$, TB$
          CLOSE 10
        ELSE
          PRINT "THERE IS NO PREVIOUS TEST DATA OR"
          PRINT "PARAMETER FILE ON THE DISK."
          DATFILOPT% = 1
          PRINT
          PRINT "YOU MUST ENTER NEW PARAMETERS TO RUN A TEST."
          PRINT
          INPUT "PRESS ENTER TO CONTINUE..."; YN$
          EXIT SUB
        END IF
        PRINT
        PRINT "THERE IS A PREVIOUS TEST ON DISK."
        PRINT
        PRINT " INFORMATION ON DATA FILE: "
        PRINT "   TESTED BY:  "; TB$
        PRINT "   TESTED ON:  "; LDT$
        PRINT
        PRINT "  OPTIONS:"
        PRINT "   1) START A COMPLETELY NEW TEST"
        PRINT "   2) FINISH OLD TEST"
        PRINT "   3) RUN NEW TEST WITH OLD PARAMETERS"
        PRINT "   4) BACK TO MAIN MENU"
        PRINT
        DO
          INPUT "  OPTION #"; DATFILOPT%
          IF DATFILOPT% > 0 AND DATFILOPT% <= STARTOVER% THEN
            EXIT DO
          END IF
          PRINT "  NOT AN OPTION!"
          PRINT
        LOOP
        OLDRTNUM# = RTNUM#
        IF DATFILOPT% = FINISHTST% OR DATFILOPT% = 3 THEN
          CALL READDATA
          IF DATFILOPT% = 3 THEN RTNUM# = OLDRTNUM#
        END IF
        IF RTNUM# <> OLDRTNUM# AND DATFILOPT% = FINISHTST% THEN              'IF USING DIFF TRANSDUCER
           PRINT "To continue with this test,"
           PRINT "reference transducer"; RTNUM#; "must be selected."
           RTNUM# = OLDRTNUM#
           INPUT "PRESS ENTER TO CONTINUE"; YN$
           Continue$ = "N"
           EXIT SUB
        END IF

END SUB

FUNCTION DataMean# (Samples%, TestData#())
       
        DIM INDEX                                       ' Data Index Counter
        DIM Mean#                                       ' Data Mean

        Mean# = 0                                       ' Initialize Mean
      
        FOR INDEX = 1 TO Samples%                       ' Loop through data
         Mean# = Mean# + TestData#(INDEX)               ' Add up data
        NEXT INDEX                                      ' Loop index
       
        DataMean# = Mean# / Samples%                    ' Calculate Mean

END FUNCTION

FUNCTION DataSelect# (Samples%, TestData#())

  DIM INDEX                                             ' Data Index Counter
  DIM Mean#                                             ' Data Mean#
  DIM StdDev#                                           ' Data Standard Dev
  DIM DataCount%                                        ' Amount of Data
  DIM Delta#                                            ' Difference
  DIM Dpoint                                            ' Bad Data Point
  DIM DataArray#(Samples%)                              ' New Data Array
  DIM factor#                                           ' Error Factor

  DataCount% = Samples%                                 ' New Variable
       
  FOR INDEX = 1 TO DataCount%                           ' New Array Loop
    DataArray#(INDEX) = TestData#(INDEX)                ' New Variable
  NEXT INDEX                                            ' End New Array Loop

  Mean# = DataMean#(DataCount%, DataArray#())           ' Data Mean#
  StdDev# = DataStdDev#(DataCount%, DataArray#(), Mean#)' Data Standard Dev

  factor# = ABS(100# * StdDev# / (Mean# + O))              ' Error Factor
  DO WHILE factor# > DataFactor# AND DataCount% > 2#    ' Select Bad Data
    Delta# = 0                                          ' Initialize Delta#
    Dpoint = 0                                          ' Initialize Dpoint
    FOR INDEX = 1 TO DataCount%                         ' Loop Through Data
      IF ABS(DataArray#(INDEX) - Mean#) > Delta# THEN   ' Detect Worst Data
        Dpoint = INDEX                                  ' Set Dpoint
        Delta# = ABS(DataArray#(INDEX) - Mean#)         ' Set Delta#
      END IF                                            ' End Detect
    NEXT INDEX                                          ' End Loop Data
    DataCount% = DataCount% - 1                         ' Reduce DataCount% 1
    FOR INDEX = Dpoint TO DataCount%                    ' Loop Remove Bad Data
      DataArray#(INDEX) = DataArray#(INDEX + 1)         ' Remove Data
    NEXT INDEX                                          ' End Loop Remove
    Mean# = DataMean#(DataCount%, DataArray#())         ' New Data Mean#
    StdDev# = DataStdDev#(DataCount%, DataArray#(), Mean#) ' New Data Standard Dev
    factor# = (100# * StdDev# / (Mean# + O))            ' Error Factor
  LOOP                                                  ' End Bad Data Select

  DataSelect# = DataMean#(DataCount%, DataArray#())     ' Data Mean#

END FUNCTION

FUNCTION DataStdDev# (Samples%, TestData#(), DMean#)
  DIM INDEX                                       ' Data Index Counter
  DIM StdDev#                                     ' Data Standard Dev
  DIM DataArray#(Samples%)                        ' New Data Array

  StdDev# = 0                                     ' Initialize Stddev
      
  FOR INDEX = 1 TO Samples%                       ' Loop through data
    StdDev# = StdDev# + (TestData#(INDEX) - DMean#) ^ 2#
  NEXT INDEX                                      ' Loop index
  DataStdDev# = SQR(StdDev# / Samples%)           ' Calculate Standard Dev

END FUNCTION

SUB DEFPRESSURES

DIM YN$

' **** DEFINE TEST PRESSURES ****

        CLS
        PRINT
        P0 = 0
        PP = 1
        PR$ = "PSI"
        PR = 1
        AB$ = "A"
        AT$ = "A"
        IF MenuOp% >= 3 THEN
          PRINT
          PRINT "DEFINE PRESSURES ACCORDING TO SPECS"
          INPUT "SPEC PRESSURES ARE 1)ABSOLUTE 2)GAGE"; X
          AB$ = "G"
          IF X = 1 THEN AB$ = "A"
          PRINT
          PRINT "POSSIBLE UNITS"
          FOR I = 1 TO 5
            PRINT "#"; I; " : "; P$(I)
          NEXT I
          DO
            INPUT "CHOOSE UNITS #:"; I
          LOOP WHILE (I < 1 OR I > 5)
          PR$ = P$(I)
          PR$ = PR$ + AB$
          PR = P(I)
          PRINT
        END IF
        PRINT
        IF AB$ = "A" THEN
          INPUT "PRESS USED FOR ABSOLUTE ZERO (PSIA)"; P0
          P0 = P0 / PR
        END IF
        PRINT "FULL SCALE PRESSURE ("; PR$; ")";
        INPUT PM
3770    IF MenuOp% = 4 OR MenuOp% = 3 THEN
          IF MenuOp% = 4 THEN
              NP(PP) = 2
              MP(PP) = NP(PP)
              PR(1, PP) = 0
              PR(NP(PP), PP) = PM
            ELSE
              NP(PP) = 3
              MP(PP) = NP(PP)
              PR(1, PP) = 0
              PR(NP(PP), PP) = PM
              INPUT "LINEARITY MIDDLE PRESSURE"; PR(2, PP)
          END IF
        ELSE
          PRINT "TEST PRESSURE OPTIONS"
          INPUT "1-EVEN STEP CYCLE(0-FS-0) 2-OTHER"; X%
          IF X% = 1 THEN
            DO
              DO
               INPUT "STEP SIZE %FSP(MIN=10)"; PS
              LOOP UNTIL PS >= 10
              NP(PP) = 200 / PS + 1
              MP(PP) = (NP(PP) + 1) / 2
              PRINT "NUMBER OF PRESSURES="; NP(PP)
              INPUT "O.K.(Y/N)"; YN$
            LOOP WHILE YN$ = "N"
            FOR IP = 1 TO NP(PP)
              PR(IP, PP) = PM * ABS(ABS(MP(PP) - IP) / (MP(PP) - 1) - 1)
            NEXT IP
          ELSE
            DO
              DO
                INPUT "TOTAL NUMBER OF PRESSURES (13 MAX)"; NP(PP)
              LOOP WHILE NP(PP) > MP
              PRINT "ENTER"; NP(PP); "PRESSURES (AS WRITTEN IN SPECS)"
              FOR IP = 1 TO NP(PP)
                PRINT "#"; IP
                PRINT "P= ("; PR$; ")";
                INPUT X
                IF X = PM THEN MP(PP) = IP
                IF X = P0 AND AB$ = "A" THEN X = 0
                PR(IP, PP) = X
              NEXT IP
              INPUT "ANY ERRORS(Y/N)"; YN$
            LOOP UNTIL YN$ <> "Y"
          END IF
        END IF
        IF PP = 1 THEN
          DF$ = "N"
          IF (MenuOp% = 5 OR MenuOp% = 8) AND NT > 0 THEN
            INPUT "DIFF PRESSURES FOR  NON-ROOM-TEMPS"; DF$
          END IF
          PRINT
          PP = 2
          IF DF$ = "Y" THEN 3770
          NP(2) = NP(1)
          MP(2) = MP(1)
          FOR IP = 1 TO NP(2)
            PR(IP, 2) = PR(IP, 1)
          NEXT IP
        END IF
        PRINT
        PF$ = "N"
        IF MenuOp% = 5 THEN INPUT "DO A PROOF PRESS TEST(Y/N)"; PF$
        IF PF$ = "Y" THEN
          PRINT "PROOF PRESS=("; PR$; ")";
          INPUT PF
        END IF
        PRINT
        PRINT "TEST PRESSURES WILL BE SUPPLIED AS:"
        INPUT "     1)ABSOLUTE 2)GAGE"; X
        AT$ = "A"
        IF X = 2 THEN AT$ = "G"
        PT$ = "PSI" + AT$
        AP = 0
        IF AB$ <> AT$ THEN INPUT "CURRENT AMBIENT PRESSURE(PSIA)"; AP
        IF AB$ = "A" AND AT$ = "G" THEN AP = -AP
        PP = 1
        glbbolMeasZeroA = 0
        YN$ = ""
        IF AB$ = "G" AND AT$ = "G" THEN
             DO
                    PRINT
                    INPUT "Do you wish to measure Zero A (Y/N)"; YN$
             LOOP UNTIL YN$ = "Y" OR YN$ = "N"
        END IF
        IF YN$ = "Y" THEN glbbolMeasZeroA = -1



END SUB

SUB DEFTEMPS

' **** DEFINE TEST TEMPERATURES ****

     CLS
     PRINT
     PRINT "DEFINE TEST TEMPERATURES"
     PRINT
     INPUT "TEMPERATURE UNITS:"; TP$
     PRINT
     PRINT "ENTER ALL TEMPERATURES IN THESE UNITS"
     PRINT
4090 INPUT "HOW MANY TEMPERATURE STEPS TOTAL (INCLUDING RETURN)"; NT
     NT = NT - 1
     IF NT > MT THEN PRINT "ONLY "; MT + 1; " TEMPS ARE ALLOWED!": GOTO 4090
     HT = 1
     RTN = 0
     IF NT = 0 THEN
       INPUT "ROOM TEMP:"; TR
       TM(0) = TR
     ELSE
       PRINT "ENTER THEM IN TEST SEQUENCE"
       FOR IT = 0 TO NT
         PRINT "TEMP #"; IT + 1
         PRINT "VALUE(DEG "; TP$; ")";
         INPUT TM(IT)
       NEXT IT
       IT = 0
       TR = TM(0)
       IF TM(NT) = TR THEN
         PRINT
         PRINT "        RT RETURN PRESSURES:"
         PRINT
         PRINT "   1) ZERO ONLY"
         PRINT "   2) 0-FSP "
         PRINT "   3) 0-FSP-0"
         PRINT "   4) ALL RT PRESSURES"
         PRINT
         INPUT "   ENTER CHOICE (1-4) "; RTN
       END IF
     END IF
     RP$ = "N"
     IF MenuOp% = 5 THEN
       INPUT "RUN A RT REPEATABILITY TEST  (Y/N)"; RP$
     END IF
     DP$ = "N"
     IF MenuOp% = 5 THEN INPUT "IS THIS A DIFFERENTIAL UNIT (Y/N)"; DP$
     IF DP$ = "Y" THEN INPUT "PRESSURIZE FRONT AND BACK (Y/N)"; DP$
     IF RP$ = "Y" THEN
       NT = NT + 1
       HT = HT + 1
       FOR IT = NT TO 1 STEP -1
         TM(IT) = TM(IT - 1)
       NEXT IT
     END IF
     IF DP$ = "Y" THEN
       NT = NT + 1
       HT = HT + 1
       FOR IT = NT TO 1 STEP -1
         TM(IT) = TM(IT - 1)
       NEXT IT
     END IF
     IF NT > MT THEN
       PRINT "ONLY A TOTAL OF "; MT; "TEMPS ALLOWED"
       PRINT "THIS INCLUDES REPEAT TEST AND BACK PORT TEST."
       PRINT "YOU HAVE EXCEEDED THIS. REDEFINE TEMPS!": GOTO 4090
     END IF

END SUB

SUB ECHODATA

DIM YN$

' ****  ECHO DATA FILE  ***

        IF DATFILOPT% = FINISHTST% THEN
          PRINT
          PRINT "TESTED BY  :"; TB$
          IF LDT$ = "" THEN LDT$ = DAT$
          PRINT "TEST WAS TERMINATED "; LDT$
          PRINT
        END IF
       
        INPUT "DO YOU WANT TO SEE A SUMMARY OF THE TEST PARAMETERS(Y/N)"; YN$
        PRINT
        IF YN$ <> "Y" THEN EXIT SUB

        dispdata$ = "SCRN:"
        WIDTH 80, 50
        CLS
        DO
          OPEN dispdata$ FOR OUTPUT AS #4
          IF DATFILOPT% = FINISHTST% THEN LDT$ = DAT$
          PRINT #4, ""
          PRINT #4, S$; "PREVIOUS TEST DATA"
          PRINT #4, ""
          PRINT #4, S$; "TESTED BY: "; TB$
          PRINT #4, ""
          IF LDT$ = "" THEN LDT$ = DAT$
          PRINT #4, S$; "DATE: "; LDT$
          PRINT #4, ""
          PRINT #4, S$; "EXCITATION VOLTAGE: "; VE; "V";
          PRINT #4, S$; "REGULATED = "; Reg$;
          PRINT #4, S$; "AMPLIFIED = "; AMP$
          PRINT #4, ""
          PRINT #4, S$; "PART # .."; PN$
          IF DATFILOPT% = FINISHTST% OR MenuOp% = 7 THEN
            PRINT #4, S$; "LOT# ...."; LOT$
            PRINT #4, ""
            PRINT #4, S$; "PORT #      S/N "
            FOR IU = 1 TO NU
              PRINT #4, USING "&###      &"; S$; IU; SN$(IU)
            NEXT IU
          END IF
          PRINT #4, ""
          PRINT #4, S$; "RT PRESSURES";
          P$ = "(" + PR$ + "):"
          T$ = "(" + TP$ + "):"
          PRINT #4, P$;
          FOR IP = 1 TO NP(1)
            P = PR(IP, 1)
            IF P = 0 THEN P = P0
            PRINT #4, USING "#####.##"; P;
            IF NP(1) > 1 AND NP(1) > IP THEN PRINT ",";
          NEXT IP
          PRINT #4, ""
          IF NT <> 0 THEN
            PRINT #4, S$; "NON RT PRESS"; P$;
            FOR IP = 1 TO NP(2)
              P = PR(IP, 2)
              IF P = 0 THEN P = P0
              PRINT #4, USING "#####.##"; P;
              IF NP(2) > 1 AND NP(2) > IP THEN PRINT ",";
            NEXT IP
            PRINT #4, ""
          END IF
          PRINT #4, ""
          PRINT #4, S$; "TEMPS"; T$;
          FOR JT = 0 TO NT
            PRINT #4, USING "####"; TM(JT);
            IF NT > 0 AND NT > JT THEN PRINT ",";
          NEXT JT
          PRINT #4, ""
          IF DATFILOPT% = FINISHTST% OR MenuOp% = 7 THEN
            PRINT #4, ""
            PRINT #4, S$; "LAST TEMP RUN: #"; LT + 1; "@"; TM(LT); TP$
          END IF
          IF glbbolMeasZeroA THEN PRINT #4, "": PRINT #4, S$; "ZERO A IS MEASURED AT EACH TEMPERATURE."
          IF dispdata$ = "LPT1:" THEN
            PRINT #4, NEWPAGE$
          END IF
          CLOSE #4
          PRINT
          IF dispdata$ = "SCRN:" THEN
            PRINT
            PRINT S$;
            INPUT "DO YOU WANT A HARD COPY OF THIS SUMMARY SHEET"; YN$
            IF YN$ = "Y" THEN
                Ready% = 0
                 DO
                        CALL CheckPrinter(1, Ready%)
                LOOP UNTIL Ready% = -1
              dispdata$ = "LPT1:"
            ELSE
              EXIT DO
            END IF
          ELSE
            EXIT DO
          END IF
        LOOP
        PRINT
        PRINT S$;
        INPUT "PRESS ENTER TO CONTINUE..."; YN$
        WIDTH 80, 25
        CLS
        PRINT
        PRINT

END SUB

SUB FINALCAL (BSA%, port%, RM%(), M%)

'**** FINAL CALIBRATION TEST

        Continue$ = "Y"
        IV = 6
        SV = 1
        PRINT
        PRINT "FINAL CALIBRATION TEST"
        PRINT
        DATFILE$ = "FC"
        CALL DATACHECK
        IF Continue$ = "N" THEN EXIT SUB
        IF DATFILOPT% = STARTOVER% THEN EXIT SUB 'OR filegood% = -1
        IF DATFILOPT% = 1 THEN
          CALL VINSELECT
          CALL DEFTEMPS
          INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
          IF Continue$ = "N" THEN EXIT SUB
          CALL DEFPRESSURES
          INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
          IF Continue$ = "N" THEN EXIT SUB
        END IF
        IF DATFILOPT% <> FINISHTST% THEN
                CALL LOADSENS
                IF Continue$ = "N" THEN EXIT SUB
        END IF
        CALL RUNTEST(BSA%, port%, RM%(), M%)
END SUB

SUB FINALREPORT

DIM YN$

'**** FINAL CALIBRATION REPORT

     REM
     REM    GENERAL FINAL CALIBRATION FORMAT
     REM
     PRINT
     PRINT
     PRINT "FINAL CALIBRATION REPORT"
     PRINT

' **** READ IN DATA

        DATFILE$ = "FC"
        filegood% = Attrib%(DATFILE$)
        IF filegood% <> -1 THEN
          OPEN DATFILE$ FOR INPUT AS #10
          INPUT #10, X$, LDT$, X$, X$, X$, X$, X$, X$, X$, TB$
          CLOSE 10
          PRINT
          PRINT " INFORMATION ON LAST TEST: "
          PRINT
          PRINT " TESTED BY:  "; TB$
          PRINT " TESTED ON:  "; LDT$
          PRINT
        ELSE
          PRINT "THERE IS NO PREVIOUS TEST DATA ON DISK."
          PRINT "YOU CANNOT PRINT A REPORT AT THIS TIME."
          PRINT
          INPUT "PRESS ENTER TO CONTINUE..."; YN$
          EXIT SUB
        END IF

        CALL READDATA
        INPUT "ARE YOU SURE YOU WANT TO PRINT THIS REPORT"; YN$
        IF YN$ <> "Y" THEN EXIT SUB
           
            Ready% = 0
            DO
                CALL CheckPrinter(1, Ready%)
            LOOP UNTIL Ready% = -1

' **** ASK ANY NECESSARY QUESTIONS:


        DO
          PRINT "LINEARITY CALCULATION:"
          INPUT "    1) END POINT  2) BEST FIT - "; AM
        LOOP WHILE AM <> 1 AND AM <> 2
        INPUT "INCLUDE LEAD RESISTANCE INFO(Y/N) "; LC$

'    *** OPEN TESTREPORT DATA FILE ***

        OPEN "TESTREP" FOR OUTPUT AS #10
        PRINT #10, X$; C$; LDT$; C$; NU

' **** OPEN PRINTING FILE #

        OPEN "LPT1:" FOR OUTPUT AS #4

' **** LOOP THROUGH UNITS AND PRINT ***

        FOR IU = 1 TO NU
          UNITGOOD = 1
'    *** PRINT FOR ONE UNIT
          PRINT #4, S$; "FINAL CALIBRATION TEST        DATE: "; LDT$
          PRINT #4, S$; QC$
          PRINT #4, ""
          PRINT #4, S$; "KULITE S/N: "; SN$(IU)
          PRINT #4, S$; "TEST PORT NUMBER: "; IU
          PRINT #4, S$; "KSP MODEL P/N:     "; PN$
          IF CN$ <> "" THEN PRINT #4, S$; "CUSTOMER P/N:      "; CN$
          IF LOT$ <> "." THEN PRINT #4, S$; "LOT NUMBER:        "; LOT$
          PRINT #4, S$; "FULL SCALE PRESS= "; PM; PR$
          PRINT #4, S$; "TEST INPUT VOLTS= "; VT; "VDC"
          PRINT #4, S$; "ACTUAL P.S. VOLTS="; fnx(Vs); "VDC"
          PRINT #4, ""
          IF LTRIM$(RTRIM$(SN$(IU))) <> "BAD" THEN
            PRINT #4, S$; "TEMPERATURE="; TR
            IF RP$ = "N" THEN
              PRINT #4, S$; "PRESSURE    OUTPUT    LINEAR   MV-DEV  NORM FSO"
            ELSE
              PRINT #4, S$; "PRESSURE    OUTPUT    LINEAR   MV-DEV  ";
              PRINT #4, "NORM FSO   REPEAT   MV-DEV"
            END IF
            VZ = VO(IU, 1, 0)
            FS = VO(IU, MP(1), 0) - VZ
            LM = 0
            CM = 0
            HM = 0
            RM = 0
            SE = FS / (PM - PR(1, 1))
            PRINT #10, SN$(IU); C$; fnx(VZ); C$; fnx(SE); C$; FNR(Ro(IU, 0)); C$; FNR(RI(IU, 0))
            DP = 0
            DN = 0
            FOR IP = 1 TO NP(1)
              VI = VO(IU, IP, 0)
              PI = PR(IP, 1)
              DV = VI - (VZ + SE * (PI - PR(1, 1)))
              IF DV > DP THEN DP = DV
              IF DV < DN THEN DN = DV
            NEXT IP
            BF = 0
            IF AM = 2 THEN BF = (DP + DN) / 2
            FOR IP = 1 TO NP(1)
              VI = VO(IU, IP, 0)
              PI = PR(IP, 1)
              SP$ = "         "
              VL = VZ + BF + FS * PI / (PM - PR(1, 1))
              DV = VI - VL
              IF IP <= MP(1) AND ABS(DV) > ABS(LM) THEN LM = DV
              IF MP(1) < NP(1) AND ABS(DV) > ABS(CM) THEN CM = DV
              IF IP > MP(1) THEN
                L = NP(1) + 1 - IP
                HY = ABS(VI - VO(IU, L, 0))
                IF HY > HM THEN HM = HY
              END IF
              IF RP$ = "Y" THEN
                VR = VO(IU, IP, 1)
                DR = VI - VR
                IF ABS(DR) > ABS(RM) THEN RM = DR
              END IF
              IF AMP$ = "N" THEN
                LI = 7
                LF = 2
                FORMAT$ = "&  ####.#   ####.##   ####.##   ###.##&   ###.##   ###.##"
              ELSE
                LI = 6
                LF = 3
                FORMAT$ = "&  ####.#   ###.###   ###.###   ###.###&  ###.###   ##.###"
              END IF
              IF IP = MP(1) THEN
                SP$ = FSTR$(FS, LI, LF)
              ELSE
                SP$ = SPACE$(LI + LF + 1)
              END IF
              M = 1
              IF AMP$ = "Y" THEN
                M = 1000
              END IF
              IF RP$ = "N" THEN
                PRINT #4, USING FORMAT$; S$; PI; VI; VL; M * DV; SP$
              ELSE
                PRINT #4, USING FORMAT$; S$; PI; VI; VL; M * DV; SP$; VR; M * DR
              END IF
            NEXT IP
            IF Reg$ = "N" THEN PRINT #4, USING "&&#####"; S$; "INPUT RES= "; RI(IU, 0);
            IF AMP$ = "N" THEN PRINT #4, USING "&&#####"; S$; "OUTPUT RES="; Ro(IU, 0);
            PRINT #4, USING "&&####.##"; S$; "CMV= "; CMVGTB(IU, 0)
            IF NT > 0 THEN
              FOR IT = HT TO NT
                PRINT #4, ""
                PRINT #4, S$; "TEMPERATURE="; TM(IT);
                IF IT = NT AND RTN <> 0 THEN
                  PP = 1
                ELSE
                  PP = 2
                END IF
                SP = 1
                LP = NP(PP)
                IF IT = NT THEN
                  IF RTN = 1 THEN LP = 1
                  IF RTN = 2 THEN LP = MP(PP): SP = LP - 1
                  IF RTN = 3 THEN SP = MP(PP) - 1
                END IF
                IF LP > 1 THEN PRINT #4, "      NORM FSO";
                PRINT #4, ""
                FOR IP = 1 TO LP STEP SP
                  PI = PR(IP, PP)
                  LI = 7: LF = 2: IF AMP$ = "Y" THEN LI = 6: LF = 3
                  IF IP = MP(PP) THEN
                    FSO = VO(IU, MP(PP), IT) - VO(IU, 1, IT)
                    SP$ = FSTR$(FSO, LI, LF)
                  ELSE
                    SP$ = SPACE$(LI + LF + 1)
                  END IF
                  VI = VO(IU, IP, IT)
                  PRINT #4, USING "&  ####.#   ####.##&"; S$; PI; VI; SP$
                NEXT IP
                IF Reg$ = "N" THEN PRINT #4, USING "&&#####"; S$; "INPUT RES= "; RI(IU, IT);
                IF AMP$ = "N" THEN PRINT #4, USING "&&#####"; S$; "OUTPUT RES="; Ro(IU, IT);
                PRINT #4, ""
              NEXT IT
            END IF
            ' **** CHECK IT OUT!!!!!!!
            IF FS < 1 THEN
              PRINT #4, "": PRINT #4, S$; "BAD UNIT!! - NO OUTPUT @ FSP"
              ELSE
              IF LC$ = "Y" THEN
                PRINT #4, ""
                PRINT #4, S$; "IR LEADS TO CASE   ___________________________"
                PRINT #4, ""
                PRINT #4, S$; "IR LEADS TO SHIELD ___________________________"
              END IF
              PERCENT.FS = 100 / FS
              LM = PERCENT.FS * LM
              CM = PERCENT.FS * CM
              HM = PERCENT.FS * HM
              RM = PERCENT.FS * RM
              SB = SQR(LM * LM + HM * HM + RM * RM)
              IF AM = 1 THEN PRINT #4, S$; "END POINT LINEARITY= "; fnx(LM); "%FSO"
              IF AM = 2 THEN PRINT #4, S$; "BEST FIT LINEARITY= "; fnx(LM); "%FSO"
              IF CM <> 0 THEN PRINT #4, S$; "COMBINED LIN & HYS=  "; fnx(CM); "%FSO"
              IF HM <> 0 THEN PRINT #4, S$; "MAXIMUM HYSTERESIS=  "; fnx(HM); "%FSO"
              IF RP$ = "Y" THEN PRINT #4, S$; "NON-REPEATABILITY=   "; fnx(RM); "%FSO"
              PR$ = LEFT$(PR$, 3)
              PRINT #4, S$; "BRIDGE SENSITIVITY=  "; fnx(SE); "MV/"; PR$; " @"; VT; "VOLTS"
              ' ADDED 12/4/96
              ' MOD AGAIN 1/28/97
              IF UCASE$(PR$) = "PSI" THEN
                  PRINT #4, S$; "BRIDGE SENSITIVITY=  "; fnx(SE * 14.5); "MV/"; "Bar"; " @"; VT; "VOLTS"
              END IF
              PRINT #4, S$; "BRIDGE SENSITIVITY=  "; fnx(SE / VT); "MV/V/"; PR$
              PRINT #4, S$; "STATIC ERROR BAND=   "; fnx(SB); "%FSO(R.S.S)"
              IF NT > 0 THEN
                LT = NT
                IF RTN > 0 THEN LT = NT - 1
                Z1 = 0
                Z2 = 0
                F1 = 0
                F2 = 0
                FOR IT = HT TO LT
                  SIGN = SGN(TM(IT) - TR)
                  IF ABS(TM(IT) - TR) <> 0 THEN
                        PER.100F = 100 / ABS(TM(IT) - TR)
                  ELSE
                        PER.100F = 1
                  END IF
                  ZT = VO(IU, 1, IT)
                  FT = VO(IU, MP(2), IT) - ZT
                  DEL.Z = SIGN * (ZT - VZ)
                  DEL.FS = SIGN * (FT - FS)
                  D1 = DEL.Z
                  IF ABS(D1) > ABS(Z1) THEN
                    T1 = TM(IT)
                    Z1 = D1
                  END IF
                  D2 = DEL.Z * PER.100F
                  IF ABS(D2) > ABS(Z2) THEN
                    T2 = TM(IT)
                    Z2 = D2
                  END IF
                  D3 = DEL.FS
                  IF ABS(D3) > ABS(F1) THEN
                    F1 = D3
                    T3 = TM(IT)
                  END IF
                  D4 = DEL.FS * PER.100F
                  IF ABS(D4) > ABS(F2) THEN
                    F2 = D4
                    T4 = TM(IT)
                  END IF
                NEXT IT
                Z1 = PERCENT.FS * Z1
                F1 = PERCENT.FS * F1
                Z2 = PERCENT.FS * Z2
                F2 = PERCENT.FS * F2
                TB = SQR(SB * SB + Z1 * Z1 + F1 * F1)
                PRINT #4, S$; "MAX NULL DEV   REL TO RT="; fnx(Z1); "%FSO        @ T="; T1
                PRINT #4, S$; "MAX NULL SHIFT REL TO RT="; fnx(Z2); "%FSO/100 "; TP$; "  @ T="; T2
                PRINT #4, S$; "MAX SPAN DEV   REL TO RT="; fnx(F1); "%FSO        @ T="; T3
                PRINT #4, S$; "MAX SPAN SHIFT REL TO RT="; fnx(F2); "%FSO/100 "; TP$; "  @ T="; T4
                IF RTN > 0 THEN
                  PRINT #4, S$; "NULL SET @ RT RETURN="; fnx(VO(IU, 1, NT) - VZ); "MV"
                END IF
                PRINT #4, S$; "TOTAL ERROR BAND=    "; fnx(TB); "%FSO(R.S.S)"
              END IF
            END IF
          ELSE
            PRINT #4, S$; "BAD UNIT!!!!"
          END IF
          UL$ = "____________________"
          PRINT #4, ""
          PRINT #4, S$; "TESTED BY:   "; UL$
          PRINT #4, ""
          PRINT #4, S$; "CHECKED BY:  "; UL$
          PRINT #4, ""
          PRINT #4, S$; "LEAK TEST:   "; UL$
          PRINT #4, NEWPAGE$

'    *** END OF LOOP ***

        NEXT IU
        CLOSE 4
        CLOSE 10
        PRINT

END SUB

FUNCTION FORMATSTR$ (X, LF, LI)

' **** PRINTER FORMAT ROUTINE ***

        Ax = ABS(X)
        RX = INT(Ax * 10 ^ LF + .5) / 10 ^ LF + .000001
        IX = INT(RX)
        FX = RX - IX
        IX$ = STR$(SGN(X) * IX)
        IF IX = 0 AND X < 0 THEN IX$ = "-0"
        IF LEN(IX$) < LI THEN IX$ = LEFT$(SC$, LI - LEN(IX$)) + IX$
        IF LF = 0 THEN
          FORMATSTR$ = IX$
          EXIT FUNCTION
        END IF
        IF FX > .01 THEN
          FX$ = STR$(FX)
          FX$ = RIGHT$(FX$, LEN(FX$) - 1)
        ELSE
          IF FX < .0001 THEN
            FX$ = ".0"
          ELSE
            FX$ = STR$(100 * FX)
            FX$ = ".00" + RIGHT$(FX$, LEN(FX$) - 2)
          END IF
        END IF
        IF LEN(FX$) < (LF + 1) THEN
          FX$ = FX$ + LEFT$(ZS$, LF + 1 - LEN(FX$))
        END IF
        IF LEN(FX$) > (LF + 1) THEN
          FX$ = LEFT$(FX$, LF + 1)
        END IF
        FORMATSTR$ = IX$ + FX$

END FUNCTION

FUNCTION FSTR$ (VALUE!, SP.BEFORE, DIG.AFTER)
        factor = 10 ^ DIG.AFTER
        X! = INT(VALUE! * factor + .5) / factor
        X$ = LTRIM$(RTRIM$(STR$(X!)))
        DECIMAL = INSTR(X$, ".")
        IF DECIMAL = 0 THEN
          X$ = X$ + "." + STRING$(DIG.AFTER, "0")
        ELSE
          X$ = X$ + STRING$(LEN(X$) - DECIMAL, "0")
        END IF
        FSTR$ = SPACE$(SP.BEFORE - (LEN(X$) - 1 - DIG.AFTER)) + X$
END FUNCTION

SUB InitA2D (BSA%, port%, RM%(), M%)
' ***** INITIALIZE THE A/D BOARD *****
        IF NOT Debug THEN
          CALL InitCTM(BSA%)            ' INITIALIZE COUNTER CHIP
          CALL ClearTrain(RM%(), M%)    ' set train to 0
          CALL LOADTRAIN(BSA%, port%, RM%(), M%)
          port% = port% OR &H14         ' STROBE ENABLE TRAIN DISABLE
          RM%(56) = 0
          RM%(59) = 1
          CALL LOADTRAIN(BSA%, port%, RM%(), M%)
                                        ' enable=0  disable=1 stepper
          port% = port% AND &HEB        ' TRAIN ENABLE STROBE DISABLE
          OUT BSA% + 3, port%           ' ENABLE TRAIN
        END IF
END SUB

SUB InitCTM (BSA%)
        OUT BSA% + 1, &H17
        OUT BSA%, &HF0
        OUT BSA%, &HCA
        OUT BSA% + 1, &H1                'data pointer for Ctr. 1
        OUT BSA%, &H2D                    'mode for Ctr. 1
        OUT BSA%, &H1
        OUT BSA% + 1, &H2                'data pointer for Ctr. 2
        OUT BSA%, &H2D                   'mode for Ctr. 2, SRC2 selected
        OUT BSA%, &H2
        OUT BSA% + 1, &H3                'data pointer for Ctr. 3
        OUT BSA%, &H2D                   'mode for Ctr. 3, SRC3 selected
        OUT BSA%, &H3
        OUT BSA% + 1, &H4                'data pointer for Ctr. 4
        OUT BSA%, &H2D                   'mode for Ctr. 4, SRC4 selected
        OUT BSA%, &H4
        OUT BSA% + 1, &H5                'data pointer for Ctr. 5
        OUT BSA%, &H2D                   'mode for Ctr. 5, SRC5 selected
        OUT BSA%, &H5
END SUB

SUB LoadBoxesAvail
    DIM filboxavail AS INTEGER
        filboxavail = FREEFILE
        IF Channel$ = "A" THEN
                BoxesFile$ = "C:\atodcfg\boxes.cfg"
        ELSE
                BoxesFile$ = "C:\atodcfg\boxes2.cfg"
        END IF
        filegood% = Attrib%(BoxesFile$)
        IF filegood% = -1 THEN
                PRINT "File: "; BoxesFile$; " is not present."
                PRINT "Please locate file and restart"
                END
        END IF
        OPEN BoxesFile$ FOR INPUT AS filboxavail
        INPUT #filboxavail, NumBoxesAvail%
        INPUT #filboxavail, Boxid$
        INPUT #filboxavail, NumPos
        INPUT #filboxavail, Inuse
        CLOSE #filboxavail
        MU = NumPos

END SUB

SUB LoadCOEFFICIENTS
' This sub reads in the coefficients file for A/D boxes.
' For each measurement type there are three coefficients for the
' correction equation.

DIM Coeffile$
DIM Cofile%
DIM NumMeasType%, NumOrder%
DIM iOrder%, iMeas%, ibox%
DIM boxSN$, ExpDate$, MeasName$
DIM DUMMY$
DIM DUMMY

CoeffFile$ = "C:\atodcfg\Coeff" + Boxid$ + ".cfg"
Cofile% = FREEFILE

filegood% = Attrib%(CoeffFile$)
IF filegood% = -1 THEN
        PRINT "File: "; CoeffFile$; " is not present."
        PRINT "Please locate file and restart"
        END
END IF

OPEN CoeffFile$ FOR INPUT AS Cofile%

' Read in the 1st line: Date last Calibrated, Expiration date
  INPUT #Cofile%, boxSN$, EntryDate$, ExpDate$


  ' Read in 2nd line: Number of measurements that have coefficients
    INPUT #Cofile%, NumMeasType%

    '  loop thru all measurements listed in .cfg file

    FOR iMeas% = 1 TO NumMeasType%
        ' 3rd line: Name of Measurement
                INPUT #Cofile%, MeasName$

            ' next line is the order
                INPUT #Cofile%, NumOrder%  ' Order n has n+1 coefficients

        ' Load coefficient array
         INPUT #Cofile%, CO(iMeas%, iOrder% + 1), CO(iMeas%, iOrder% + 2), CO(iMeas%, iOrder + 3)
         COX(iMeas%, iOrder% + 1) = CO(iMeas%, iOrder% + 1)
         COX(iMeas%, iOrder% + 2) = CO(iMeas%, iOrder% + 2)
         COX(iMeas%, iOrder% + 3) = CO(iMeas%, iOrder% + 3)
NEXT iMeas%

IF EOF(Cofile%) THEN
        PportV# = 5#
ELSE
        INPUT #Cofile%, DUMMY$
        INPUT #Cofile%, DUMMY
        INPUT #Cofile%, PportV#                    ' Pressure Port Voltage
        IF PportV# = 0 THEN PportV# = 5#
END IF
PportVX# = PportV#
CLOSE Cofile%


END SUB

SUB LoadPosAddress
'  Loads position addresses from POSBOX##.cfg file into the array

' This SUB replaces this code:
        '*******************
        '   FOR I = 1 TO MU
        '       READ MM%(I)
        '   NEXT I
        ' ******************
        filPOSBOX = FREEFILE
        POSBOXfile$ = "c:\atodcfg\POSBOX" + Boxid$ + ".cfg"
        filegood% = Attrib%(POSBOXfile$)
        IF filegood% = -1 THEN
                PRINT "File: "; POSBOXfile$; " is not present."
                PRINT "Please locate file and restart"
                END
        END IF
        OPEN POSBOXfile$ FOR INPUT AS filPOSBOX
        INPUT #filPOSBOX, MU
 
        FOR iPos% = 1 TO MU
            INPUT #filPOSBOX, MM%(iPos%), PosGood%
        NEXT iPos%
       CLOSE #filPOSBOX
END SUB

SUB LoadPowSupVoltages
' Reads possible power supply voltages from .cfg file

'       FOR I = 0 TO NV
'          READ VI(I)
'        NEXT I

DIM filPower AS INTEGER
DIM PowerFile$

PowerFile$ = "c:\atodcfg\POWER" + (Boxid$) + ".cfg"
filegood% = Attrib%(PowerFile$)
IF filegood% = -1 THEN
        PRINT "File: "; PowerFile$; " is not present."
        PRINT "Please locate file and restart"
        END
END IF
filPower = FREEFILE
OPEN PowerFile$ FOR INPUT AS filPower

INPUT #filPower, BoxType%  'reads box version number
INPUT #filPower, NV        ' reads number of power supply settings

FOR iVolt% = 0 TO NV
     INPUT #filPower, VI(iVolt%)
NEXT iVolt%

CLOSE #filPower

END SUB

SUB LoadRefTrans
DIM DataFile$                                   'Name of ref trans data file
DIM NTRANS%                                     'Number of transducers
DIM COEF%                                       'Number of coefficients
DIM RTRANS$(50, 7)                              'Ref trans data array
'   RTRANS$(50, 1)                              'Serial Number
'   RTRANS$(50, 2)                              'FSP
'   RTRANS$(50, 3)                              'a coeff
'   RTRANS$(50, 4)                              'b coeff
'   RTRANS$(50, 5)                              'c coeff
'   RTRANS$(50, 6)                              'd coeff
'   RTRANS$(50, 7)                              'p type

DIM I                                           'Counter
DIM NUM                                         'transducer selection
DIM PTYPE$(50)                                  'Pressure Type
DIM YN$

DataFile$ = "c:\atodcfg\rtrans.cfg"             'Ref Trans Data file
filegood% = Attrib%(DataFile$)
IF filegood% = -1 THEN
        PRINT "File: "; DataFile$; " is not present."
        INPUT "Press enter to continue"; YN$
        EXIT SUB
END IF

filGetref% = FREEFILE                           'Get File Number

OPEN DataFile$ FOR INPUT AS #filGetref%         'Open Ref Trans Data file

INPUT #filGetref%, RTDATE$                      'Date in ref trans file
NTRANS% = 0                                     'initialize ntrans%
DO                                              'loop through data file
   NTRANS% = NTRANS% + 1                        'increment ntrans%
   COEF% = 0                                    'initialize coef
   DO                                           'loop through coef group
      COEF% = COEF% + 1                         'increment coef%
      IF COEF% = 7 THEN
        INPUT #filGetref%, PTYPE$(NTRANS%)
      ELSE
        INPUT #filGetref%, RTRANS$(NTRANS%, COEF%)
      END IF
   LOOP WHILE COEF% < 7                         'escape loop
LOOP WHILE NOT EOF(filGetref%)                  'escape loop at eof
CLOSE #filGetref%                               'Close Ref Trans Data file
CLS
PRINT "PRESSURE REFERENCE TRANSDUCERS - DATE OF FILE: "; RTDATE$
PRINT "(SELECT 0 TO DISABLE REFERENCE TRANSDUCER)"
PRINT
PRINT "## SERIAL #   FS(PSI) PT  ## SERIAL #   FS(PSI) PT  ## SERIAL #   FS(PSI) PT "
PRINT "------------------------  ------------------------  ------------------------ "
FOR I = 1 TO NTRANS%                            'loop through selection
        IF (I MOD 3) = 0 THEN
                PRINT USING "## \        \ #####.## !  "; I; RTRANS$(I, 1); VAL(RTRANS$(I, 2)); PTYPE$(I)
        ELSE
                PRINT USING "## \        \ #####.## !  "; I; RTRANS$(I, 1); VAL(RTRANS$(I, 2)); PTYPE$(I);
        END IF
NEXT I
PRINT
PRINT
DO
        INPUT "ENTER THE NUMBER OF YOUR SELECTION"; NUM
LOOP UNTIL NUM >= 0 AND NUM <= NTRANS%
RTNUM# = VAL(RTRANS$(NUM, 1))                        'Transducer Number
RTCOEF3# = VAL(RTRANS$(NUM, 3))                      'Third Order coeff
RTCOEF2# = VAL(RTRANS$(NUM, 4))                      'Second order coeff
RTCOEF1# = VAL(RTRANS$(NUM, 5))                      'First order coeff
RTCOEF0# = VAL(RTRANS$(NUM, 6))                      'Zero order coeff
RTFSP# = VAL(RTRANS$(NUM, 2))                        'Ref Trans FSO
RTPTYPE$ = PTYPE$(NUM)                          'Ref Trans P-type

END SUB

SUB LOADSENS

DIM YN$

'  *** LOAD AND IDENTIFY SENSORS ***

        CLS
        PRINT
        PRINT "LOAD AND IDENTIFY ALL UNITS"
        PRINT
        PRINT
        INPUT "TESTED BY:"; TB$
        PRINT
        IF TB$ = "" THEN TB$ = "."
        IF DATFILOPT% <> 3 THEN
          INPUT "ARE UNITS AMPLIFIED(Y/N)"; AMP$
          INPUT "ARE UNITS REGULATED(Y/N)"; Reg$
          INPUT "KSP MODEL P/N"; PN$
          INPUT "CUSTOMER P/N"; CN$
        ELSE
          IF AT$ <> AB$ THEN INPUT "AMBIENT PRESSURE (PSIA):"; AP
        END IF
        INPUT "LOT NUMBER"; LOT$
        IF LOT$ = "" THEN LOT$ = "."
        CLS
        PRINT
        PRINT "LOAD UNITS SEQUENTIALLY STARTING WITH POSITION #1"
        PRINT
        INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
        IF Continue$ = "N" THEN EXIT SUB
        PRINT
        DO
          INPUT "NUMBER OF UNITS "; NU
          IF NU > MU THEN PRINT "MAX. OF "; MU; "UNITS"
        LOOP UNTIL NU <= MU
        INPUT "USING MULTIPLE S/N TYPES(Y/N)"; YN$
        IF YN$ = "N" THEN
          INPUT "FIXED PART OF S/N (XXXX-XX-)"; FIXX$
          INPUT "ARE S/N SEQUENTIAL(Y/N)"; SEQ$
          IF SEQ$ = "Y" THEN
            INPUT "STARTING S/N"; SN(1)
            PRINT
          ELSE
            PRINT "ENTER SERIAL NUMBERS FOR  EACH PORT"
            PRINT
          END IF
          FOR IU = 1 TO NU
            IF SEQ$ = "Y" THEN
              SN(IU) = SN(1) + IU - 1
              PRINT "PORT #"; IU; " S/N="; SN(IU)
            ELSE
              PRINT "PORT #"; IU
              INPUT "            S/N="; SN(IU)
            END IF
          NEXT IU
          PRINT
          INPUT "ANY CORRECTIONS(Y/N)"; YN$
          IF YN$ = "Y" THEN
           INPUT "HOW MANY"; NC
           FOR I = 1 TO NC
             INPUT "PORT #"; K
             INPUT "S/N="; SN(K)
           NEXT I
          END IF
          FOR I = 1 TO NU
            Q$ = STR$(SN(I))
            SN$(I) = FIXX$ + RIGHT$(Q$, LEN(Q$) - 1)
          NEXT I
        ELSE
          PRINT "MUST ENTER ENTIRE S/N (XXXX-XX-XXX)":
          PRINT
          FOR IU = 1 TO NU
            PRINT "PORT #"; IU
            INPUT "            S/N="; SN$(IU)
          NEXT IU
          PRINT
          INPUT "ANY CORRECTIONS(Y/N)"; YN$
          IF YN$ = "Y" THEN
            INPUT "HOW MANY"; NC
            FOR I = 1 TO NC
              INPUT "PORT #"; K
              INPUT "S/N="; SN$(K):
            NEXT I
          END IF
        END IF

END SUB

SUB LOADTRAIN (BSA%, port%, RM%(), M%)
        IF Debug THEN EXIT SUB          ' Exit if in debug
        FOR Q% = M% TO 1 STEP -1        ' M% length of train
           port% = port% AND &HFD       ' reset b2
           DT% = 2 * RM%(Q%)            ' set train data bit as 2 or 0 to b1
                                        ' CLOCK = OP0 activ 0 to 1
                                        ' DATA  = OP1 true
           port% = port% OR DT%
           OUT BSA% + 3, port%          ' DT set data
           OUT BSA% + 3, port% AND 254  ' DT AND 254 clock =0
           OUT BSA% + 3, port% OR 1     ' DT OR 1    clock =1
        NEXT Q%
           OUT BSA% + 3, port% OR &H80  ' st1 high DT OR 4
           OUT BSA% + 3, port% AND &H7F ' st1 low  DT AND 251
        CALL DELAY(25)                  ' 25ms DELAY
END SUB

FUNCTION MEASCMV (BSA%, port%, RM%(), M%)

        DIM TestData#(Samples%)

' **** MEASURE SINGLE COMMON MODE VOLTAGE ***

        IF Debug THEN
          MEASCMV = 2.5
          EXIT FUNCTION
        END IF
       
       INDEX% = 0
       DO
          CALL ClearTrain(RM%(), M%)
          RM%(33) = 1
          RM%(34) = 1
          RM%(5) = 1
          RM%(6) = 1
          RM%(31) = 1
          IF IV >= 0 AND IV < 18 THEN RM%(36 + IV) = 1
          RM%(MM%(IU)) = 1
         IF IV = 17 THEN RM%(31) = 0
         Counts = ReadA2DNew(BSA%, port%, RM%(), M%)
          X# = Counts / 20000#
          INDEX% = INDEX% + 1
          TestData#(INDEX%) = X#
       LOOP UNTIL INDEX% = Samples%
       X# = DataSelect#(Samples%, TestData#())

          A2DVolts# = CO(Range%, 1) * X# * X# + CO(Range%, 2) * X# + CO(Range%, 3)
          IF RM%(34) = 1 THEN
                A2DVolts# = CO(VOUTVDC, 1) * A2DVolts# * A2DVolts# + CO(VOUTVDC, 2) * A2DVolts# + CO(VOUTVDC, 3)
          END IF
          ' error handling for overrange
          IF ABS(Counts) >= 40000 THEN
            PRINT "WARNING A/D OVER RANGE!  "
          END IF
       
          MEASCMV = A2DVolts#
         ' MEASCMV = ABS(counts) * 60 / 100000#
       
END FUNCTION

FUNCTION MEASRIN (BSA%, port%, RM%(), M%)

        DIM TestData#(Samples%)
        DIM OverRideRin%

        IF SN$(IU) = BD$ THEN
          MEASRIN = 0
          EXIT FUNCTION
        END IF

        IF Debug THEN
         ' MEASRIN = 1002 * (1 + .001 * (TM(IT) - TR)) + IU
          MEASRIN = INT((RND * 400) + 801)
          EXIT FUNCTION
        END IF
      
   
    INDEX% = 0
    DO
        CALL ClearTrain(RM%(), M%)
        RM%(8) = 1
        RM%(4) = 1
        RM%(29) = 1
        RM%(31) = 1
        RM%(IV + 36) = 1
        RM%(MM%(IU)) = 1
        IF IV = 17 THEN RM%(31) = 0
        Counts# = ReadA2DNew(BSA%, port%, RM%(), M%)
        OverRideRin% = 0
        IF Counts# = 20001 THEN
                RM%(34) = 1
                OverRideRin% = 1
                Counts# = ReadA2DNew(BSA%, port%, RM%(), M%)
        END IF
        X# = Counts# / 20000#
    INDEX% = INDEX% + 1
    TestData#(INDEX%) = X#
    LOOP UNTIL INDEX% = Samples%
    X# = DataSelect#(Samples%, TestData#())

        A2DVolts# = CO(Range%, 1) * X# * X# + CO(Range%, 2) * X# + CO(Range%, 3)
        IF RM%(34) = 1 THEN
           A2DVolts# = A2DVolts# * AtodVolts * CO(VOUTVDC, 1) + CO(VOUTVDC, 2) * A2DVolts# + CO(VOUTVDC, 3)
        END IF
        X# = (Vs - A2DVolts#) / (A2DVolts# + O)
        
        IF BoxType% = 1 THEN                            ' first gen box
          RX# = CO(RIN, 1) * X#
          MEASRIN = a# * RX# + B#
        ELSE                                            ' second gen box
            MEASRIN = CO(RIN, 1) * X# * X# + CO(RIN, 2) * X# + CO(RIN, 3)
        END IF

        IF OverRideRin% = 1 THEN
                RM%(34) = 0
                OverRideRin% = 0
        END IF
       
END FUNCTION

FUNCTION MEASROUT (BSA%, port%, RM%(), M%)

        DIM TestData#(Samples%)

        ' don't test bad channels
        IF SN$(IU) = BD$ THEN
          MEASROUT = 0
          EXIT FUNCTION
        END IF

        ' DEBUG fakeout
        IF Debug THEN
         ' MEASROUT = 1000 * (1 + .001 * (TM(IT) - TR)) + IU
           MEASROUT = INT((RND * 400) + 701)
          EXIT FUNCTION
        END IF
      
    INDEX% = 0
    DO
        CALL ClearTrain(RM%(), M%)
        RM%(5) = 1
        RM%(6) = 1
        RM%(4) = 1
        RM%(30) = 1
        RM%(MM%(IU)) = 1
        IF LD > 0 THEN RM%(1) = 1                'connect Load Resistor to A/D
        Counts = ReadA2DNew(BSA%, port%, RM%(), M%)
        X# = ABS(Counts) / 20000#
    INDEX% = INDEX% + 1
    TestData#(INDEX%) = X#
    LOOP UNTIL INDEX% = Samples%
    X# = DataSelect#(Samples%, TestData#())
       
        A2DVolts# = CO(Range%, 1) * X# * X# + CO(Range%, 2) * X# + CO(Range%, 3)

        IF RM%(34) = 1 THEN
                A2DVolts# = A2DVolts# * A2DVolts# * CO(VOUTVDC, 1) + A2DVolts# * CO(VOUTVDC, 2) + CO(VOUTVDC, 3)
        END IF
        XO# = CO(ROUT, 1) * A2DVolts# / (CO(ROUT, 2) - A2DVolts#)

      '  VX = ABS(counts) / 100000#
      '  XO = 250000# * VX / (5# - VX)
        IF XO# > 0 AND LD > 0 THEN
                MEASROUT = 1 / (1 / XO# - 1 / LD)
        ELSE
                MEASROUT = XO#
        END IF
END FUNCTION

SUB MeasVinNew (BSA%, port%, RM%(), M%, PReport$)

DIM Iter%
DIM TestData#(Samples%)
DIM YN$

' **** CALIBRATE POWER SUPPLY ***

        IF Debug THEN
          VX = VI(IV)
          Vs = VX - .001
          VT = VX
          Vs(IT) = Vs
          EXIT SUB
        END IF

        INDEX = 17 - IV
      
' ***** Measure Voltage In *****
Iter% = 0
DO
        Iter% = Iter% + 1
        CALL ClearTrain(RM%(), M%)              'Zero train bits
        RM%(7) = 1                              'Set to read PS
        IF BoxType% = 1 THEN                    'Box type old
                RM%(27) = 1                     '4K Resistor
        ELSE                                    'Box Type New
                RM%(28) = 1                     '2K Resistor
        END IF                                  'End select
        RM%(29) = 1                             '20 Ohms bus
        SELECT CASE INDEX
           CASE 0                               '1.26V range
                   RM%(31) = 0                  'Set to 1.26V
           CASE 16                              '32V range
                   RM%(31) = 1                  'Set Bit 31
                   RM%(37) = 1                  'Set to 28V
                   RM%(34) = 1                  'Set Divider On
           CASE 17                              '28V range
                   RM%(31) = 1                  'Set to 32V
                   RM%(34) = 1                  'Set Divider On
           CASE ELSE                            '2V to 24V range
                   RM%(31) = 1                  'Set Bit On
                   RM%(53 - INDEX) = 1          'Set PS bit On
        END SELECT
        RM%(1) = 0                        ' Turn off Load Resistor bit
        Counts = ReadA2DNew(BSA%, port%, RM%(), M%)
        SELECT CASE Range%                      'Select Voltage Range
           CASE VOUT80MV
                Vs = CO(PSVOLT80MV, 1) * Counts * Counts + CO(PSVOLT80MV, 2) * Counts + CO(PSVOLT80MV, 3)
           CASE Vout200mV
                Vs = CO(PSVOLT200MV, 1) * Counts * Counts + CO(PSVOLT200MV, 2) * Counts + CO(PSVOLT200MV, 3)
           CASE Vout400mV
                Vs = CO(PSVOLT400MV, 1) * Counts * Counts + CO(PSVOLT400MV, 2) * Counts + CO(PSVOLT400MV, 3)
        END SELECT
        IF RM%(34) = 1 THEN                     'Vout Divider
           Vs = Vs * Vs * CO(VOUTVDC, 1) + CO(VOUTVDC, 2) * Vs + CO(VOUTVDC, 3)
        END IF
        TestData#(Iter%) = Vs
LOOP UNTIL Iter% = Samples%
Vs = DataSelect#(Samples%, TestData#())
Vs(IT) = Vs

IF PReport$ = "Y" THEN
  PRINT "THE POWER SUPPLY IS NOW SET TO ";
  PRINT USING "##.#####"; Vs;
  PRINT " VDC"
  PRINT
END IF

IF PReport$ = "99" THEN
 VIN99 = Vs
END IF

IF Vs > 1.1 * VE THEN                         ' check for accidental overvoltage
  PRINT "POWER SUPPLY ERROR"
  EXIT SUB
END IF

IF NOT (VE > 24 OR Vs > .9 * VE) THEN         ' check for shorts
  PRINT "STOP! LOW P.S. VOLTAGE="; fnx(Vs)
  PRINT "REMOVE SHORTS OR EXTRA UNITS"
  INPUT "TO CONTINUE PRESS ENTER"; YN$
  StopNow% = -1
  EXIT SUB
END IF
   
END SUB

FUNCTION MeasVoutNew# (BSA%, port%, RM%(), M%)

' ***** Get a Output Voltage from a Test Port
      
DIM TestData#(Samples%)                         ' Array for Test Data
DIM INDEX%                                      ' Index for Samples
DIM LowVolt$                                    ' Low Voltage Flag
DIM Counts&                                     ' Counts returned by A2D
DIM RATIO#                                      ' Counts ratioed to maximum
DIM VX#                                         ' Voltage from Range

IF Debug OR SN$(IU) = BD$ THEN                  ' Debug Code
  IF AMP$ = "N" THEN
    MeasVoutNew# = (99 * (P / PM - .00009 * (TM(IT) - TR)) + IU) * (VE / VT) + RND
  ELSE
    MeasVoutNew = RND * 1000
    'MeasVoutNew# = (60# * (99# * (P / PM - .00009 * (TM(IT) - TR)) + IU) * (VE / VT)) / 1000# + RND
  END IF
  EXIT FUNCTION
END IF

LowVolt$ = "N"                                  ' Set LowVolt Reading to No
INDEX% = 0                                      ' Initialize counter
DO                                              ' Loop Through Samples
  CALL ClearTrain(RM%(), M%)                    ' Clear The Train
  IF AMP$ = "Y" AND LowVolt$ = "N" THEN
    RM%(34) = 1                                 ' Turn on Voltage Divider
  END IF
  RM%(5) = 1                                    ' Set to Meas Vout
  RM%(6) = 1                                    ' Set to Meas Vout
  RM%(MM%(IU)) = 1                              ' Set Port Position
  RM%(36 + IV) = 1                              ' Set PS Setting Bit
  IF IV = 17 THEN
    RM%(31) = 0                                 ' 1.26V Setting
    RM%(36 + IV) = 0                            ' Unset incorrect bit
  ELSE
    RM%(31) = 1                                 ' All other PS Settings
  END IF
  IF LD > 0 THEN RM%(1) = 1                'connect Load Resistor to A/D
  Counts& = ReadA2DNew(BSA%, port%, RM%(), M%)  ' Get A2D Reading
  RATIO# = Counts& / 20000#                     ' Get Counts Ratioed to Maximum
  VX# = CO(Range%, 1) * RATIO# ^ 2 + CO(Range%, 2) * RATIO# + CO(Range%, 3)
  IF AMP$ = "Y" AND LowVolt$ = "N" THEN         ' Correct for Voltage Divider
    VX# = CO(VOUTVDC, 1) * VX# ^ 2 + CO(VOUTVDC, 2) * VX# + CO(VOUTVDC, 3)
  END IF
  IF Reg$ = "N" THEN                            ' Correct for Non-Regulated
    VX# = VX# * VE / Vs                         ' Correction formula
  END IF
  IF AMP$ = "N" THEN
    VX# = VX# * 1000                            ' Convert V to mV
  END IF
  INDEX% = INDEX% + 1                           ' Increment counter
  TestData#(INDEX%) = VX#                       ' Save data sample
  IF AMP$ = "Y" AND LowVolt$ = "N" AND ABS(VX#) < .38 THEN
                                                ' Remove Volt Divider - Low V
    INDEX% = INDEX% - 1                         ' Retest Sample
    LowVolt$ = "Y"                              ' Set LowVolt$ flag on
  ELSE
    LowVolt$ = "N"                              ' Set LowVolt$ flag off
  END IF
LOOP UNTIL INDEX% = Samples%                    ' Escape loop when Samples done
MeasVoutNew# = DataSelect#(Samples%, TestData#())
                                                ' Select Data
END FUNCTION

SUB MeasZeroA (PressNormCoeff#, BSA%, port%, RM%(), M%)
        DO
           IF glbbolMeasZeroA THEN
               LI = 3: LF = 1
               PRINT
               PRINT "SET PRESSURE TO ZERO A (0 A)."
               PRINT
               INPUT "READY TO PROCEED(Y/N)"; YN$
            END IF
          LOOP UNTIL YN$ = "Y"
       
       
        ' report to screen
        PRINT "  #    OUTPUT"
        FOR IU = 1 TO NU
          VX = MeasVoutNew#(BSA%, port%, RM%(), M%)
          glbArrZeroA(IU, IT) = VX / PressNormCoeff#
          PRINT USING "### ######.###"; IU; glbArrZeroA(IU, IT)
        NEXT IU

END SUB

SUB PRETEST (BSA%, port%, RM%(), M%)

DIM YN$
Continue$ = "Y"

' **** PRETEST FOR COMPENSATION ****

        IV = 11
        SV = 11
        PRINT
        DATFILE$ = "PTC"
        PRINT "PRETEST FOR COMPENSATION"
        CALL DATACHECK
        IF Continue$ = "N" THEN EXIT SUB
        IF DATFILOPT% = STARTOVER% THEN EXIT SUB 'OR filegood% = -1
        IF DATFILOPT% = 1 THEN
          CALL VINSELECT
          CALL DEFPRESSURES
          CLS
          PRINT
          PRINT
          PRINT "COMPENSATION METHODS"
          PRINT "1 - DATA @ RT ONLY AND ASSUMED TCR/TCGF"
          PRINT "2 - DATA @ RT AND ANOTHER TEMP"
          PRINT "3 - DATA @ RT AND TWO OTHER TEMPS"
          INPUT "SELECT METHOD 1,2,OR 3:"; NT
          NT = NT - 1
          IF NT = 0 THEN
            TM(0) = TR
          ELSE
            DO
              PRINT
              FOR IT = 0 TO NT
                PRINT "TEMP#"; IT + 1; "=";
                INPUT TM(IT)
              NEXT IT
              PRINT
              INPUT "ANY ERRORS"; YN$
            LOOP WHILE YN$ = "Y"
          END IF
        END IF
        IF DATFILOPT% <> FINISHTST% THEN
                CALL LOADSENS
                IF Continue$ = "N" THEN EXIT SUB
        END IF
        CALL RUNTEST(BSA%, port%, RM%(), M%)
        IF Continue$ = "N" THEN EXIT SUB
        YN$ = "N"
        INPUT "DO YOU WANT A PRINTOUT OF THIS TEST DATA"; YN$
        IF YN$ = "Y" THEN
          DO
            Ready% = 0
            DO
                CALL CheckPrinter(1, Ready%)
            LOOP UNTIL Ready% = -1
            OPEN "LPT1:" FOR OUTPUT AS #4
            PRINT #4, ""
            PRINT #4, S$; "PRETEST DATA       DATE: "; DAT$
            PRINT #4, ""
            PRINT #4, S$; QC$
            PRINT #4, ""
            PRINT #4, S$; "KSP MODEL P/N:   "; PN$
            IF CN$ <> "" THEN PRINT #4, S$; "CUSTOMER P/N:    "; CN$
            IF LOT$ <> "." THEN PRINT #4, S$; "LOT NUMBER:      "; LOT$
            PRINT #4, S$; "INPUT VOLTAGE=  "; VE; "VOLTS"
            PRINT #4, S$; "ACTUAL VOLTAGE= "; fnx(Vs); "VOLTS"
            PRINT #4, S$; "F.S. PRESSURE=  "; PM; PR$
            FOR IU = 1 TO NU
              PRINT #4, ""
              PRINT #4, S$; "KULITE S/N:    "; SN$(IU)
              PRINT #4, ""
              PRINT #4, S$; "TEMP  OUT-ZSP  OUT-FSP  NORM-FS   R-IN  R-OUT"
              FOR IT = 0 TO NT
                VZ = VO(IU, 1, IT)
                VF = VO(IU, 2, IT)
                FS = VF - VZ
                XI = RI(IU, IT)
                XO = Ro(IU, IT)
                FORMAT$ = "&####  ####.##  ####.##  ####.##   ####   ####"
                PRINT #4, USING FORMAT$; S$; TM(IT); VZ; VF; FS; XI; XO
              NEXT IT
            NEXT IU
            PRINT #4, NEWPAGE$
            CLOSE #4
            PRINT
            INPUT "WAS PRINTING O.K.(Y/N)"; YN$
            IF YN$ = "Y" THEN EXIT DO
            INPUT "RESET PRINTER - PRESS ENTER"; YN$
          LOOP
        END IF
        CLS
        PRINT "EXIT AND START COMP PROGRAM TO GENERATE COMP PRINTOUTS."
END SUB

SUB PrintProofReport (iNumPressures AS INTEGER)
' Prints a report for the proof pressures taken
 DIM R AS INTEGER
 DIM K AS INTEGER
 DIM A1 AS INTEGER
 DIM A2 AS INTEGER
 DIM dispdata$
 dispdata$ = "SCRN:"
     DO
            OPEN dispdata$ FOR OUTPUT AS #4
            PRINT #4, ""
            PRINT #4, "COMPILED RESULTS FOR PROOF PRESSURE TEST"
            PRINT #4, ""
            PRINT #4, S$; "KSP MODEL P/N:     "; PN$
            IF CN$ <> "" THEN PRINT #4, S$; "CUSTOMER P/N:      "; CN$
            IF LOT$ <> "." THEN PRINT #4, S$; "LOT NUMBER:        "; LOT$
            PRINT #4, S$; "NOMINAL TEST VOLTAGE="; VE
            PRINT #4, S$; "ACTUAL PS VALUE="; fnx(Vs)
            PRINT #4, ""
            PRINT #4, "OUTPUT VS. PRESSURE"
            R = INT((NU - 1) / 8)
            FOR K = 0 TO R
              HD$ = "PORT  #"
              SX$ = "S/N   :"
              UNDLIN$ = "-------"
              A1 = 1 + 8 * K
              A2 = A1 + 7
              IF A2 > NU THEN A2 = NU
              LI = 9: LF = 0
              FOR IU = A1 TO A2
                X = IU: X$ = FORMATSTR$(X, LF, LI)
                HD$ = HD$ + X$
                X$ = RIGHT$(SN$(IU), 3)
                SX$ = SX$ + LEFT$(SC$, 9 - LEN(X$)) + X$
                UNDLIN$ = UNDLIN$ + "   ------"
              NEXT IU
              PRINT #4, HD$
              PRINT #4, SX$
              PRINT #4, UNDLIN$
              FOR IP = 1 TO iNumPressures ' STEP SP
                PRINT #4, USING "#####.#"; glbProofPrVal(IP); 'PR(IP, PP);
                FOR IU = A1 TO A2
                  IF AMP$ = "Y" THEN
                    PRINT #4, USING "#####.###"; glbArrVoProof(IU, IP);
                  ELSE
                    PRINT #4, USING "######.##"; glbArrVoProof(IU, IP);
                  END IF
                NEXT IU: PRINT #4, ""
              NEXT IP
              PRINT #4, ""
            NEXT K
            IF NU > 16 THEN
              IF DISP$ = "LPT1:" THEN
                PRINT #4, NEWPAGE$
              ELSE
                PRINT #4, ""
                INPUT "HIT ANY KEY TO CONTINUE..."; YN$
              END IF
            END IF
            PRINT #4, ""
      '      PRINT #4, "  #    R-IN   R-OUT     C.M.V."
      '      PRINT #4, " ---  ------ -------   --------"
      '      FOR IU = 1 TO NU
      '        PRINT #4, USING "### ####### ####### ######.##"; IU; RI(IU, IT); RO(IU, IT); CMVGTB(IU, IT)
      '      NEXT IU
            IF dispdata$ = "LPT1:" THEN
              PRINT #4, NEWPAGE$
            END IF
            CLOSE #4
            PRINT
            IF dispdata$ = "SCRN:" THEN
              INPUT "PRESS ENTER TO CONTINUE..."; YN$
              WIDTH 80, 25
              CLS
              PRINT
              PRINT
              PRINT
              INPUT "DO YOU WANT A HARD COPY OF THIS DATA"; YN$
              IF YN$ = "Y" THEN
                Ready% = 0
                DO
                        CALL CheckPrinter(1, Ready%)
                LOOP UNTIL Ready% = -1
                dispdata$ = "LPT1:"
              ELSE
                EXIT DO
              END IF
            ELSE
              EXIT DO
            END IF
    LOOP

END SUB

SUB PROOFPRESSURE (BSA%, port%, RM%(), M%)

DIM YN$

' **** PROOF PRESSURE ***

        CLS
        PRINT "  PROOF PRESSURE TEST  "
        PRINT
        PRINT
        PRINT
        PRINT "SET PRESSURE TO "; PF; PR$;
        IF PR$ <> PT$ THEN
          PRINT "("; fnx(PF * PR + AP); PT$; ")";
        END IF
        PRINT
        INPUT "PRESS ENTER TO TAKE MEASUREMENTS"; YN$
        DO
          PRINT
          CALL ClearTrain(RM%(), M%)
          FOR IU = 1 TO NU
            VX = MeasVoutNew#(BSA%, port%, RM%(), M%)
            PF(IU) = VX
            PRINT "PORT #"; IU; "  PPO="; PF(IU); "MV"
          NEXT IU
          INPUT "WAS PRESSURE CORRECT"; YN$
        LOOP UNTIL YN$ = "Y"
        PRINT "TAKE PROOF PRESSURE OFF"
        INPUT "PRESS ENTER TO CONTINUE"; YN$

END SUB

SUB PSTestNew (BSA%, port%, RM%(), M%)
       
' ***** Power Supply Test ***** '
DIM INDEX                                       'Power Supply counter
DIM Counts                                      'A2D Results
DIM PV                                          'Power Supply Voltage
DIM Iter%
DIM TestData#(Samples%)
DIM YN$
DIM RATIO#                                      ' PERCENT DIFF ERROR
DIM aVolt#                                      ' ACTUAL PS VOLTAGE (DMM)

PRINT
YN$ = "N"
PRINT
PRINT "DISCONNECT ALL UNITS FROM THE RACK"
'PRINT
'INPUT "DO YOU WISH CALIBRATION MODE"; YN$
CLS                                             'Clear Screen
YN$ = "N"
PRINT
IF YN$ = "Y" THEN
   PRINT "  #   TARGET(V)   ACTUAL(V)    RATIO(%)"             'Header
   PRINT " --   ---------   ---------    --------"             'Header
ELSE
   PRINT "  #   TARGET(V)   ACTUAL(V)"             'Header
   PRINT " --   ---------   ---------"             'Header
END IF
FOR INDEX = 0 TO 17                             'Loop Through PS Settings
        CALL ClearTrain(RM%(), M%)              'Zero train bits
        RM%(7) = 1                              'Set to read PS
        IF BoxType% = 1 THEN                    'Box type old
                RM%(27) = 1                     '4K Resistor
        ELSE                                    'Box Type New
                RM%(28) = 1                     '2K Resistor
        END IF                                  'End select
        RM%(29) = 1                             '20 Ohms bus
        SELECT CASE INDEX
           CASE 0                               '1.26V range
                   RM%(31) = 0                  'Set to 1.26V
           CASE 16                              '32V range
                   RM%(31) = 1                  'Set Bit 31
                   RM%(37) = 1                  'Set to 28V
                   RM%(34) = 1                  'Set Divider On
           CASE 17                              '28V range
                   RM%(31) = 1                  'Set to 32V
                   RM%(34) = 1                  'Set Divider On
           CASE ELSE                            '2V to 24V range
                   RM%(31) = 1                  'Set Bit On
                   RM%(53 - INDEX) = 1          'Set PS bit On
        END SELECT
        Iter% = 0
        DO
        Iter% = Iter% + 1
        Counts = ReadA2DNew(BSA%, port%, RM%(), M%)
        SELECT CASE Range%                      'Select Voltage Range
           CASE VOUT80MV
                PV = CO(PSVOLT80MV, 1) * Counts * Counts + CO(PSVOLT80MV, 2) * Counts + CO(PSVOLT80MV, 3)
           CASE Vout200mV
                PV = CO(PSVOLT200MV, 1) * Counts * Counts + CO(PSVOLT200MV, 2) * Counts + CO(PSVOLT200MV, 3)
           CASE Vout400mV
                PV = CO(PSVOLT400MV, 1) * Counts * Counts + CO(PSVOLT400MV, 2) * Counts + CO(PSVOLT400MV, 3)
        END SELECT
        IF RM%(34) = 1 THEN
           PV = PV * PV * CO(VOUTVDC, 1) + CO(VOUTVDC, 2) * PV + CO(VOUTVDC, 3)
        END IF
        TestData#(Iter%) = PV
        LOOP UNTIL Iter% = Samples%
        PV = DataSelect#(Samples%, TestData#())
        
        IF YN$ = "Y" THEN
           INPUT "What is the actual voltage:"; aVolt#
           RATIO# = (aVolt# - PV) / aVolt# * 100#
           PRINT USING " ##    ##.####     ##.####     ##.####"; INDEX; aVolt#; PV; RATIO#
        ELSE
           PRINT USING " ##    ##.####     ##.#### "; INDEX; VI(17 - INDEX); PV
        END IF
NEXT INDEX                                      'End Loop
PRINT
CALL SetPowerLow(BSA%, port%, RM%(), M%)
INPUT "Press Enter To Continue"; YN$
END SUB

SUB QUICKTEST (BSA%, port%, RM%(), M%)

'*      Quick Test For Linearity and or  Temperature

        DIM YN$
        Continue$ = "Y"

        IV = 11
        SV = 11
        PRINT
        DATFILE$ = "QT"
        PRINT "QUICK TEST FOR LINEARITY AND TEMPERATURE"
        CALL DATACHECK
        IF Continue$ = "N" THEN EXIT SUB
        IF DATFILOPT% = STARTOVER% THEN EXIT SUB 'OR filegood% = -1
       
'       DATFILOPT% = 1
        IF DATFILOPT% = 1 THEN
          CLS
          PRINT
          PRINT
          PRINT "QUICK TEST OPTIONS"
          PRINT "1 - LINEARITY"
          PRINT "2 - TEMPERATURE SHIFTS"
          DO
            INPUT "SELECT OPTION 1 OR 2:"; QT
          LOOP UNTIL QT = 1 OR QT = 2
          CALL LOADSENS
          CALL VINSELECT
          IF QT = 2 THEN
              CALL DEFTEMPS
              MenuOp% = 4
              CALL DEFPRESSURES
              MenuOp% = 3
            ELSE
              NT = 0
              INPUT "ROOM TEMP:"; TR
              TM(0) = TR
              CALL DEFPRESSURES
          END IF
        END IF
        CALL RUNTEST(BSA%, port%, RM%(), M%)
        
          Ready% = 0
          DO
            CALL CheckPrinter(1, Ready%)
          LOOP UNTIL Ready% = -1
          OPEN "LPT1:" FOR OUTPUT AS #4
          PRINT #4, ""
          PRINT #4, S$; "QUICK TEST         DATE: "; DAT$
          PRINT #4, ""
          PRINT #4, S$; QC$
          PRINT #4, ""
          PRINT #4, S$; "KSP MODEL P/N:   "; PN$
          IF CN$ <> "" THEN PRINT #4, S$; "CUSTOMER P/N:    "; CN$
          IF LOT$ <> "." THEN PRINT #4, S$; "LOT NUMBER:      "; LOT$
          PRINT #4, S$; "INPUT VOLTAGE=  "; VE; "VOLTS"
          PRINT #4, S$; "ACTUAL VOLTAGE= "; fnx(Vs); "VOLTS"
          PRINT #4, S$; "F.S. PRESSURE=  "; PM; PR$
          IF QT = 1 THEN
              PRINT #4, S$; "TEMPERATURE=  "; TR
              PRINT #4, ""
              PRINT #4, "     S/N      OUT-ZSP  OUT-MSP  OUT-FSP  NORM-FS   %LIN   R-IN  ROUT"
              PRINT #4, "------------  -------  -------  -------  -------  ------  ----  ----"
              FOR IU = 1 TO NU
                VZ = VO(IU, 1, 0)
                VM = VO(IU, 2, 0)
                VF = VO(IU, 3, 0)
                FS = fnx(VF - VZ)
                LN = fnx(100 * ((VM - VZ) / (VF - VZ + O) - PR(2, 1) / PM))
                XI = RI(IU, 0)
                XO = Ro(IU, 0)
                FORMAT$ = "\          \  ####.##  ####.##  ####.##  ####.##  ###.##  ####  ####"
                PRINT #4, USING FORMAT$; SN$(IU); VZ; VM; VF; FS; LN; XI; XO
              NEXT IU
            ELSE
              FOR IU = 1 TO NU
                PRINT #4, ""
                PRINT #4, S$; "KULITE S/N:    "; SN$(IU)
                PRINT #4, ""
                PRINT #4, "TEMP  OUT-ZSP  OUT-FSP  NORM-FS  R-IN  ROUT"
                PRINT #4, "----  -------  -------  -------  ----  ----"
                FOR IT = 0 TO NT
                  VZ = VO(IU, 1, IT)
                  VF = VO(IU, 2, IT)
                  FS = VF - VZ
                  XI = RI(IU, IT)
                  XO = Ro(IU, IT)
                FORMAT$ = "####  ####.##  ####.##  ####.##  ####  ####"
                PRINT #4, USING FORMAT$; TM(IT); VZ; VF; FS; XI; XO
              NEXT IT
            NEXT IU
          END IF
          PRINT #4, NEWPAGE$
          CLOSE #4
          CLS
           
END SUB

FUNCTION ReadA2DNew (BSA%, port%, RM%(), M%)

DIM lastread&                               'STABILITY VERIFICATION VARIABLE
DIM Iter%                                   'PREVENT ENDLESS LOOPING
DIM R&                                      'A/D READING
DIM Counts&                                 'COUNTS FOR A/D BOX
DIM PORN%                                   'OUTPUT + OR -
                                           
IF Debug THEN EXIT FUNCTION

IF BoxType% = 1 THEN                        'SELECT RANGE BY BOX GENERATION
   Range% = 1                               'FIRST GEN BOX
ELSE
   Range% = 0                               'SECOND GEN BOX
END IF

DO   ' START MEASUREMENT LOOP
   Range% = Range% + 1                      'SET TO NEXT HIGHEST RANGE
   SELECT CASE Range%                       'SET VOLTAGE RANGE
      CASE 1                                '80MV RANGE
        RM%(35) = 0
        RM%(36) = 1
      CASE 2                                '200MV RANGE
        RM%(35) = 0
        RM%(36) = 0
      CASE 3                                '400MV RANGE
        RM%(35) = 1
        RM%(36) = 0
   END SELECT
   DO                                       'LOOP FOR GOOD COUNTS & READING
      CALL LOADTRAIN(BSA%, port%, RM%(), M%)'LOAD SETTING TO REGISTERS
      OUT BSA% + 1, &HDF                    'SETS COUNTER TO ZERO
      OUT BSA% + 1, &H9                     'POINT TO LOAD REGISTER CTR.1
      OUT BSA%, 0                           'WRITE A ZERO TO LEAST SIG
      OUT BSA%, 0                           'WRITE A ZERO TO MOST SIG
      OUT BSA% + 1, &H7F                    'RESET AND ENABLE COUNTING
      OUT BSA% + 3, &H10 OR port%           'START A/D COUNTING
      CALL DELAY(100)                        'We need this delay here
      lastread& = 0                         'INITIALIZE LAST READING
      Iter% = 0                             'INITIALIZE ITERATION COUNTER
      R& = 0                                'INITIALIZE A/D READING
      DO                                    'LOOP FOR GOOD R& READING
         Iter% = Iter% + 1                  'ITERATE ITER
         IF Iter% > 500 THEN                'DETECT BAD A/D
            Iter% = -99                     'SET VARIABLE
            EXIT DO                         'ESCAPE DO
         END IF
         lastread& = R&                     'SET LASTREAD
         OUT BSA% + 1, &HBF                 'A/D DATA TO HOLDING REGISTERS
         OUT BSA% + 1, &H11                 'POINT TO CTR.1 HOLD REGISTER
         CALL DELAY(100)                     'DELAY TO SET REGISTERS
         R& = INP(BSA%)                     'READ LEAST SIGNIGICANT BIT
         R& = R& + 256& * INP(BSA%)          'READ MOST SIGNIFICANT BIT AND ADD
         IF R& <> 0 THEN                    'END A/D IF READING <> 0
            OUT BSA% + 3, &HEF AND port%    'END A/D
         END IF
      LOOP UNTIL R& = lastread& AND R& <> 0 'ESCAPE LOOP IF R& IS GOOD
      Counts& = R& - 10001                  'CONVERT TO COUNTS
      IF Iter% = -99 THEN EXIT DO           'EXIT DO FOR ITER%
   LOOP UNTIL Counts& >= 0                  'ESCAPE LOOP IF COUNTS& IS GOOD
   IF Iter% = -99 THEN EXIT DO              'EXIT DO FOR ITER%
LOOP UNTIL Counts& < 20000 OR Range% = 3    'ESCAPE RANGE LOOP

PORN% = INP(BSA% + 2) AND &H10              'DETECT + OR - VALUE
IF PORN% = 0 THEN Counts& = -Counts&        'SET + OR -

IF Iter% = -99 THEN                         'EXECUTE ONLY ON A ZERO
        PRINT "ZERO READING ON A/D BOX"     'READING AFTER 30000 ITERS
        Counts& = -32000                    'SET TO A LOW VALUE
END IF

IF Counts& >= 20001 THEN                    'EXECUTE ONLY ON A
        PRINT "A/D BOX OVER ON ALL RANGES"  'OVER RANGE READING
END IF

SELECT CASE Range%                          'SET VOLTAGE RANGE
   CASE 1                                   '80MV RANGE
      Range% = VOUT80MV                     'SET RANGE% TO V RANGE
      RM%(36) = 0                           'RESET TO 200MV RANGE
   CASE 2                                   '200MV RANGE
      Range% = Vout200mV                    'SET RANGE% TO V RANGE
   CASE 3                                   '400MV RANGE
      Range% = Vout400mV                    'SET RANGE% TO V RANGE
      RM%(35) = 0                           'RESET TO 200MV RANGE
END SELECT

ReadA2DNew = Counts&                        'RETURN NUMBER OF COUNTS + PORN
                                            
END FUNCTION

SUB READDATA

'****   READ DATA FILE ***

         OPEN DATFILE$ FOR INPUT AS #10
         INPUT #10, X$, LDT$, CN$, PN$, TP$, AB$, RTN, LOT$
         INPUT #10, PR$, TB$, RP$, X$, DP$, PT$, AT$, X$
         INPUT #10, IV, RTNUM#, X, X, VT, Vs, NT, LT
         INPUT #10, NP(1), NP(2), MP(1), MP(2), PP, P0, PM, NU
         INPUT #10, VE, Vs(0), Vs(1), Vs(2), Vs(3), Vs(4), Vs(5), Vs(6)
         INPUT #10, Vs(7), Vs(8), Vs(9), Vs(10), Vs(11), Vs(12), Vs(13)
         INPUT #10, PR, AP, HT, LD, AMP$, Reg$, X, X
         INPUT #10, glbbolMeasZeroA
         IF glbbolMeasZeroA THEN        ' read in zeroA measurements
              FOR IUNIT = 1 TO NU
                FOR ITEMP = 0 TO NT
                   INPUT #10, glbArrZeroA(IUNIT, ITEMP)
                NEXT
              NEXT
         END IF
        
         FOR j = 0 TO NT
           INPUT #10, TM(j)
         NEXT j
         FOR P = 1 TO 2
           FOR K = 1 TO NP(P)
             INPUT #10, PR(K, P)
           NEXT K
         NEXT P
         IF DATFILOPT% <> 3 THEN
           FOR I = 1 TO NU
             INPUT #10, SN$(I), SE(I), PF(I), X, X
             P = 1
             FOR j = 0 TO NT
               INPUT #10, RI(I, j), Ro(I, j), CMVGTB(I, j), CMVWTB(I, j), X
               IF j >= HT THEN
                 P = 2
                 IF j = NT AND RTN <> 0 THEN P = 1
               END IF
               FOR K = 1 TO NP(P)
                 INPUT #10, VO(I, K, j)
               NEXT K
             NEXT j
           NEXT I
         END IF
         CLOSE 10
         CALL ECHODATA

END SUB

SUB RESULTS

DIM YN$

          dispdata$ = "SCRN:"
          WIDTH 80, 50
          CLS
          DO
            OPEN dispdata$ FOR OUTPUT AS #4
            PRINT #4, ""
            PRINT #4, "COMPILED RESULTS FOR  TEMP="; TM(IT); TP$
            PRINT #4, ""
            PRINT #4, "NOMINAL TEST VOLTAGE="; VE
            PRINT #4, "ACTUAL PS VALUE="; fnx(Vs)
            PRINT #4, ""
            PRINT #4, "OUTPUT VS. PRESSURE"
            R = INT((NU - 1) / 8)
            FOR K = 0 TO R
               LI = 14
               LF = 3
              HD$ = "PORT  #"
              SX$ = "S/N   :"
              UNDLIN$ = "-------"
              A1 = 1 + 8 * K
              A2 = A1 + 7
              IF A2 > NU THEN A2 = NU
              LI = 9: LF = 0
              FOR IU = A1 TO A2
                X = IU: X$ = FORMATSTR$(X, LF, LI)
                HD$ = HD$ + X$
                X$ = RIGHT$(SN$(IU), 3)
                SX$ = SX$ + LEFT$(SC$, 9 - LEN(X$)) + X$
                UNDLIN$ = UNDLIN$ + "   ------"
              NEXT IU
              PRINT #4, HD$
              PRINT #4, SX$
              PRINT #4, UNDLIN$
             
              IF glbbolMeasZeroA THEN
                PRINT #4, USING "####.#&"; 0; "A";
                FOR IU = A1 TO A2
                  IF AMP$ = "Y" THEN
                    PRINT #4, USING "#####.###"; glbArrZeroA(IU, IT);
                  ELSE
                    PRINT #4, USING "######.##"; glbArrZeroA(IU, IT);
                  END IF
                NEXT IU
                PRINT #4, ""
              END IF

              FOR IP = 1 TO LP STEP SP
                PRINT #4, USING "####.#&"; PR(IP, PP); AB$;
     
                FOR IU = A1 TO A2
                  IF AMP$ = "Y" THEN
                    PRINT #4, USING "#####.###"; VO(IU, IP, IT);
                  ELSE
                    PRINT #4, USING "######.##"; VO(IU, IP, IT);
                  END IF
                NEXT IU: PRINT #4, ""
              NEXT IP
              PRINT #4, ""
              PRINT #4, "NET FSO";
              FOR IU = A1 TO A2
                   FOR IP = 1 TO LP STEP SP
                        IF IP = MP(PP) THEN
                          FSO = VO(IU, MP(PP), IT) - VO(IU, 1, IT)
                          'SP$ = FSTR$(FSO, LI, LF)
                          PRINT #4, USING "######.##"; FSO;
                        END IF
                   NEXT IP
               NEXT IU
               PRINT #4, ""
               PRINT #4, ""
            NEXT K
            IF NU > 16 THEN
              IF DISP$ = "LPT1:" THEN
                PRINT #4, NEWPAGE$
              ELSE
                PRINT #4, ""
                INPUT "HIT ANY KEY TO CONTINUE..."; YN$
              END IF
            END IF
            PRINT #4, ""
            PRINT #4, "  #    R-IN   R-OUT     C.M.V."
            PRINT #4, " ---  ------ -------   --------"
            FOR IU = 1 TO NU
              PRINT #4, USING "### ####### ####### ######.##"; IU; RI(IU, IT); Ro(IU, IT); CMVGTB(IU, IT)
            NEXT IU
            IF dispdata$ = "LPT1:" THEN
              PRINT #4, NEWPAGE$
            END IF
            CLOSE #4
            PRINT
            IF dispdata$ = "SCRN:" THEN
              INPUT "PRESS ENTER TO CONTINUE..."; YN$
              WIDTH 80, 25
              CLS
              PRINT
              PRINT
              PRINT
              INPUT "DO YOU WANT A HARD COPY OF THIS DATA"; YN$
              IF YN$ = "Y" THEN
                Ready% = 0
                DO
                        CALL CheckPrinter(1, Ready%)
                LOOP UNTIL Ready% = -1
                dispdata$ = "LPT1:"
              ELSE
                EXIT DO
              END IF
            ELSE
              EXIT DO
            END IF
          LOOP

END SUB

SUB RinCalibrate (BSA%, port%, RM%(), M%)

' ***** Measure A and B Coefficients for Rin *****

DIM Iter%
DIM TestDataVRin4#(Samples%)
DIM TestDataVRin1#(Samples%)
DIM TestDataVin4#(Samples%)
DIM TestDataVin1#(Samples%)
DIM VRin4#
DIM VRin1#
DIM YN$
DIM Vin4#
DIM Vin1#
DIM INDEX

IF Debug OR BoxType% = 2 THEN                                 'Debug code
  EXIT SUB
END IF

INDEX = 17 - IV                                 'Get correct Voltage Index
   
IF BoxType% = 1 THEN

Iter% = 0                                       'Initialize Iter%
DO
  Iter% = Iter% + 1
  ' Get Vin with 4k cal resistor in place
  CALL ClearTrain(RM%(), M%)                    'Zero train bits
  RM%(7) = 1                                    'Set to read PS
  RM%(29) = 1                                   '20 Ohm B/T bus 1 and 3
  RM%(27) = 1                                   'K5 4kOhm Calibration resistor
  SELECT CASE INDEX
    CASE 0                                      '1.26V range
      RM%(31) = 0                               'Set to 1.26V
    CASE 16                                     '28V range
      RM%(31) = 1                               'Set Bit 31
      RM%(37) = 1                               'Set to 28V
      RM%(34) = 1                               'Set Divider On
    CASE 17                                     '32V range
      RM%(31) = 1                               'Set to 32V
      RM%(34) = 1                               'Set Divider On
    CASE ELSE                                   '2V to 24V range
      RM%(31) = 1                               'Set Bit On
      RM%(53 - INDEX) = 1                       'Set PS bit On
  END SELECT
  Counts = ReadA2DNew(BSA%, port%, RM%(), M%)   'Get Reading
  SELECT CASE Range%                            'Select Voltage Range
    CASE VOUT80MV
      Vin4# = CO(PSVOLT80MV, 1) * Counts * Counts + CO(PSVOLT80MV, 2) * Counts + CO(PSVOLT80MV, 3)
    CASE Vout200mV
      Vin4# = CO(PSVOLT200MV, 1) * Counts * Counts + CO(PSVOLT200MV, 2) * Counts + CO(PSVOLT200MV, 3)
    CASE Vout400mV
      Vin4# = CO(PSVOLT400MV, 1) * Counts * Counts + CO(PSVOLT400MV, 2) * Counts + CO(PSVOLT400MV, 3)
  END SELECT
  IF RM%(34) = 1 THEN                           'Vout Divider
    Vin4# = Vin4# * Vin4# * CO(VOUTVDC, 1) + CO(VOUTVDC, 2) * Vin4# + CO(VOUTVDC, 3)
  END IF
  TestDataVin4#(Iter%) = Vin4#
LOOP UNTIL Iter% = Samples%

Iter% = 0                                       'Initialize Iter%
DO
  Iter% = Iter% + 1
  ' Get V across 18 ohm series input resistor with 4K Ohm cal resistor in place
  CALL ClearTrain(RM%(), M%)                    'Zero train bits
  RM%(31) = 1                                   'Low power supply setting
  RM%(8) = 1                                    'K2 20 Ohm Resistor for input measurements
  RM%(29) = 1                                   '20 Ohm B/T bus 1 and 3
  RM%(27) = 1                                   'K5 4kOhm Calibration resistor
  SELECT CASE INDEX
    CASE 0                                      '1.26V range
      RM%(31) = 0                               'Set to 1.26V
    CASE 16                                     '28V range
      RM%(31) = 1                               'Set Bit 31
      RM%(37) = 1                               'Set to 28V
      RM%(34) = 1                               'Set Divider On
    CASE 17                                     '32V range
      RM%(31) = 1                               'Set to 32V
      RM%(34) = 1                               'Set Divider On
    CASE ELSE                                   '2V to 24V range
      RM%(31) = 1                               'Set Bit On
      RM%(53 - INDEX) = 1                       'Set PS bit On
  END SELECT
  Counts = ReadA2DNew(BSA%, port%, RM%(), M%)
  Counts = Counts / 20000
  SELECT CASE Range%                            'Select Voltage Range
    CASE VOUT80MV
      VRin4# = CO(VOUT80MV, 1) * Counts * Counts + CO(VOUT80MV, 2) * Counts + CO(VOUT80MV, 3)
    CASE Vout200mV
      VRin4# = CO(Vout200mV, 1) * Counts * Counts + CO(Vout200mV, 2) * Counts + CO(Vout200mV, 3)
    CASE Vout400mV
      VRin4# = CO(Vout400mV, 1) * Counts * Counts + CO(Vout400mV, 2) * Counts + CO(Vout400mV, 3)
    END SELECT
  IF RM%(34) = 1 THEN                         'Vout Divider
    VRin4# = VRin4# * VRin4# * CO(VOUTVDC, 1) + CO(VOUTVDC, 2) * VRin4# + CO(VOUTVDC, 3)
  END IF
  TestDataVRin4#(Iter%) = VRin4#
LOOP UNTIL Iter% = Samples%
 
Iter% = 0                                       'Initialize Iter%
DO
  Iter% = Iter% + 1
  ' Get Vin with 1k cal resistor in place
  CALL ClearTrain(RM%(), M%)                    'Zero train bits
  RM%(7) = 1                                    'Set to read PS
  RM%(29) = 1                                   '20 Ohm B/T bus 1 and 3
  RM%(28) = 1                                   'K6 1kOhm calibration resistor
  SELECT CASE INDEX
    CASE 0                                      '1.26V range
      RM%(31) = 0                               'Set to 1.26V
    CASE 16                                     '28V range
      RM%(31) = 1                               'Set Bit 31
      RM%(37) = 1                               'Set to 28V
      RM%(34) = 1                               'Set Divider On
    CASE 17                                     '32V range
      RM%(31) = 1                               'Set to 32V
      RM%(34) = 1                               'Set Divider On
    CASE ELSE                                   '2V to 24V range
      RM%(31) = 1                               'Set Bit On
      RM%(53 - INDEX) = 1                       'Set PS bit On
  END SELECT
  Counts = ReadA2DNew(BSA%, port%, RM%(), M%)   'Get Reading
  SELECT CASE Range%                            'Select Voltage Range
    CASE VOUT80MV
      Vin1# = CO(PSVOLT80MV, 1) * Counts * Counts + CO(PSVOLT80MV, 2) * Counts + CO(PSVOLT80MV, 3)
    CASE Vout200mV
      Vin1# = CO(PSVOLT200MV, 1) * Counts * Counts + CO(PSVOLT200MV, 2) * Counts + CO(PSVOLT200MV, 3)
    CASE Vout400mV
      Vin1# = CO(PSVOLT400MV, 1) * Counts * Counts + CO(PSVOLT400MV, 2) * Counts + CO(PSVOLT400MV, 3)
  END SELECT
  IF RM%(34) = 1 THEN                           'Vout Divider
    Vin1# = Vin1# * Vin1# * CO(VOUTVDC, 1) + CO(VOUTVDC, 2) * Vin1# + CO(VOUTVDC, 3)
  END IF
  TestDataVin1#(Iter%) = Vin1#
LOOP UNTIL Iter% = Samples%

Iter% = 0                                       'Initialize Iter%
DO
  Iter% = Iter% + 1
  ' Get V across 18 ohm series input resistor with 1K Ohm cal resistor in place
  CALL ClearTrain(RM%(), M%)                    'Zero train bits
  RM%(29) = 1                                   'Connect 20 ohm B/T bus 1 and 3
  RM%(31) = 1                                   'Low power supply setting
  RM%(8) = 1                                    '20 Ohm Resistor
  RM%(28) = 1                                   'K6 1kOhm calibration resistor
  SELECT CASE INDEX
    CASE 0                                      '1.26V range
      RM%(31) = 0                               'Set to 1.26V
    CASE 16                                     '28V range
      RM%(31) = 1                               'Set Bit 31
      RM%(37) = 1                               'Set to 28V
      RM%(34) = 1                               'Set Divider On
    CASE 17                                     '32V range
      RM%(31) = 1                               'Set to 32V
      RM%(34) = 1                               'Set Divider On
    CASE ELSE                                   '2V to 24V range
      RM%(31) = 1                               'Set Bit On
      RM%(53 - INDEX) = 1                       'Set PS bit On
  END SELECT
  Counts = ReadA2DNew(BSA%, port%, RM%(), M%)
  Counts = Counts / 20000
  SELECT CASE Range%                            'Select Voltage Range
    CASE VOUT80MV
      VRin1# = CO(VOUT80MV, 1) * Counts * Counts + CO(VOUT80MV, 2) * Counts + CO(VOUT80MV, 3)
    CASE Vout200mV
      VRin1# = CO(Vout200mV, 1) * Counts * Counts + CO(Vout200mV, 2) * Counts + CO(Vout200mV, 3)
    CASE Vout400mV
      VRin1# = CO(Vout400mV, 1) * Counts * Counts + CO(Vout400mV, 2) * Counts + CO(Vout400mV, 3)
  END SELECT
  IF RM%(34) = 1 THEN                           'Vout Divider
    VRin1# = VRin1# * VRin1# * CO(VOUTVDC, 1) + CO(VOUTVDC, 2) * VRin1# + CO(VOUTVDC, 3)
  END IF
  TestDataVRin1#(Iter%) = VRin1#
LOOP UNTIL Iter% = Samples%

VRin4# = DataSelect#(Samples%, TestDataVRin4#())        'Select Data
Vin4# = DataSelect#(Samples%, TestDataVin4#())          'Select Data
VRin1# = DataSelect#(Samples%, TestDataVRin1#())        'Select Data
Vin1# = DataSelect#(Samples%, TestDataVin1#())          'Select Data
R4K# = CO(RIN, 1) * (Vin4# - VRin4#) / (VRin4# + O)     'Calc R4
R1K# = CO(RIN, 1) * (Vin1# - VRin1#) / (VRin1# + O)     'Calc R1

IF (CO(RIN, 2) + CO(RIN, 3)) = 0 THEN
  CO(RIN, 2) = 1000                                     'Old Style Cal
  CO(RIN, 3) = 4000                                     'Old Style Cal
END IF

a# = (CO(RIN, 3) - CO(RIN, 2)) / (R4K# - R1K# + O)      'A Coeff
B# = CO(RIN, 2) - a# * R1K#                             'B Coeff

END IF

END SUB

SUB RioArrayNew (BSA%, port%, RM%(), M%)

' ***** Input And Output Resistance *****

DIM YN$
DIM BU$

BU$ = "N"

IF AMP$ = "Y" AND Reg$ = "Y" THEN
        PRINT "No Resistance Measurements"
        EXIT SUB
END IF

DO
        PRINT
        PRINT "  Resistance Measurements"
        PRINT
        IF Reg$ = "N" AND AMP$ = "N" THEN
                PRINT "Port#    R-In      R-Out"
                PRINT "-----  --------  ---------"
        END IF
        IF Reg$ = "N" AND AMP$ = "Y" THEN
                PRINT "Port#    R-In"
                PRINT "-----  --------"
        END IF
        IF Reg$ = "Y" AND AMP$ = "N" THEN
                PRINT "Port#    R-Out"
                PRINT "-----  ---------"
        END IF
       
        IF Reg$ = "N" THEN
          CALL RinCalibrate(BSA%, port%, RM%(), M%)
        END IF

        FOR IU = 1 TO NU
                PRINT USING "  ##"; IU;
                IF Reg$ = "N" THEN
                        RI(IU, IT) = MEASRIN(BSA%, port%, RM%(), M%)
                        IF AMP$ = "Y" THEN
                                PRINT USING "    #####"; RI(IU, IT)
                        ELSE
                                PRINT USING "    #####"; RI(IU, IT);
                        END IF
                END IF
                IF AMP$ = "N" THEN
                        Ro(IU, IT) = MEASROUT(BSA%, port%, RM%(), M%)
                        PRINT USING "      #####"; Ro(IU, IT)
                END IF
        NEXT IU

        PRINT
        IF IT = 0 THEN
          INPUT "EXAMINE DATA - ANY BAD UNITS(Y/N)"; BU$
          IF BU$ = "Y" THEN
            INPUT "HOW MANY BAD UNITS"; NB
            PRINT
            PRINT "CODE:1-REWIRE 2-REPLACE 3-IGNORE"
            PRINT
            FOR IB = 1 TO NB
              INPUT "PORT #"; KP
              INPUT "CODE(1-3)"; KD
              IF KD = 2 THEN INPUT "NEW S/N(XXXX-XX-XXX)"; SN$(KP)
              IF KD = 3 THEN SN$(KP) = BD$
            NEXT IB
            INPUT "PRESS ENTER TO CONTINUE"; YN$
          END IF
        END IF


LOOP UNTIL BU$ = "N"

END SUB

SUB RUNPROOFTEST (BSA%, port%, RM%(), M%)
'  Runs proof pressure test.
'  Procedure:  User enters proof pressure value.
'              Computer reads output voltages.
'              Repeat above until user asks to stop test.
'              Generate the report

  DIM IUNIT AS INTEGER        ' index to loop through units
  DIM bolFinished AS INTEGER  ' flag to indicate this test is finished
  DIM iPrCount AS INTEGER     ' Keeps track of the number of pressures used
  DIM FILNUM AS INTEGER         ' File number for data file
  DIM bolChange AS INTEGER      ' Keeps track of amplified flag change.
  bolFinished = 0
  iPrCount = 1
  bolChange = 0
  CLS
  IF AMP$ <> "Y" THEN
     AMP$ = "Y"
     bolChange = -1
  END IF
  
  PRINT , , "PROOF PRESSURE TEST"
 
     '   DatFile$ = "PROOF"
     '   CALL DATACHECK
     '   IF Continue$ = "N" THEN EXIT SUB
     '   IF DATFILOPT% = STARTOVER% THEN EXIT SUB 'OR filegood% = -1
     '   IF DATFILOPT% = 1 THEN
     '     CALL VINSELECT
         ' CALL DEFTEMPS
     '     INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
     '     IF Continue$ = "N" THEN EXIT SUB
         ' CALL DEFPRESSURES
     '     INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
     '     IF Continue$ = "N" THEN EXIT SUB
    '    END IF
       
        IF DATFILOPT% <> FINISHTST% THEN
          DO
               INPUT "ARE THESE NEW UNITS (Y/N)"; YN$
          LOOP UNTIL YN$ = "Y" OR YN$ = "N" OR YN$ = ""
               
          IF YN$ = "Y" THEN
               CALL LOADSENS
          END IF
          IF Continue$ = "N" THEN EXIT SUB
        END IF
 
 
  CALL VINSELECT
  PRINT
  DO
    PRINT "IS THE SETUP READY FOR"; VE; "VOLTS TO BE APPLIED";
    INPUT YN$
    PRINT
  LOOP UNTIL YN$ = "Y"
  CALL MeasVinNew(BSA%, port%, RM%(), M%, "Y")
 
  DO
      ' prompt for pressure
      INPUT "Enter Proof Pressure Value:"; glbProofPrVal(iPrCount)
      PRINT "Set Proof Pressure to "; glbProofPrVal(iPrCount)
      INPUT "Press ENTER key to take readings..."; YN$
      PRINT
      

      ' Loop through each unit and take a reading
      YN$ = "Y"
      DO
            FOR IU = 1 TO NU
                glbArrVoProof(IU, iPrCount) = MeasVoutNew(BSA%, port%, RM%(), M%) * 1000#
                PRINT "Unit "; IU; ":  Output: "; fnx(glbArrVoProof(IU, iPrCount))
            NEXT
            PRINT
            INPUT "Was Pressure Correct"; YN$
            AMP$ = "Y"
      LOOP WHILE YN$ = "N"

      DO
            INPUT "More Pressures"; YN$
            IF YN$ = "N" THEN bolFinished = 1
      LOOP UNTIL YN$ = "Y" OR YN$ = "N"

      iPrCount = iPrCount + 1
  LOOP WHILE bolFinished = 0
  
  ' *** Save Data
  FILNUM = FREEFILE
  OPEN "Proof" FOR OUTPUT AS FILNUM
           
  PRINT #FILNUM, S$; "KSP MODEL P/N:     "; PN$
  IF CN$ <> "" THEN PRINT #FILNUM, S$; "CUSTOMER P/N:      "; CN$
  IF LOT$ <> "." THEN PRINT #FILNUM, S$; "LOT NUMBER:        "; LOT$
  PRINT #FILNUM, S$; "NOMINAL TEST VOLTAGE="; VE
  PRINT #FILNUM, S$; "ACTUAL PS VALUE="; fnx(Vs)
  PRINT #FILNUM, ""

  PRINT #FILNUM, "", LDT$, "", "", "", "", "", "", "", TB$
  PRINT #FILNUM, "Unit Num", "Pressure", "Vout"     ' Print file header
 
  FOR IUNIT = 1 TO NU                 ' for every unit...
    FOR iCount = 1 TO iPrCount - 1           ' for every pressure taken...
      ' Write Unit, Pressure value, and Vout for that unit/pressure
      PRINT #FILNUM, SN$(IUNIT), glbProofPrVal(iCount), glbArrVoProof(IUNIT, iCount)
    NEXT
  NEXT
  CLOSE #FILNUM
  FILNUM = FREEFILE
  proof$ = LEFT$(SN$(1), 8) + ".OP"
  OPEN proof$ FOR OUTPUT AS FILNUM
 
  PRINT #FILNUM, S$; "KSP MODEL P/N:     "; PN$
  IF CN$ <> "" THEN PRINT #FILNUM, S$; "CUSTOMER P/N:      "; CN$
  IF LOT$ <> "." THEN PRINT #FILNUM, S$; "LOT NUMBER:        "; LOT$
  PRINT #FILNUM, S$; "NOMINAL TEST VOLTAGE="; VE
  PRINT #FILNUM, S$; "ACTUAL PS VALUE="; fnx(Vs)
  PRINT #FILNUM, ""

  PRINT #FILNUM, "", LDT$, "", "", "", "", "", "", "", TB$
  PRINT #FILNUM, "Unit Num", "Pressure", "Vout"     ' Print file header

  FOR IUNIT = 1 TO NU                 ' for every unit...
    FOR iCount = 1 TO iPrCount - 1           ' for every pressure taken...
      ' Write Unit, Pressure value, and Vout for that unit/pressure
      PRINT #FILNUM, SN$(IUNIT), glbProofPrVal(iCount), glbArrVoProof(IUNIT, iCount)
    NEXT
  NEXT
  CLOSE #FILNUM

 
  CALL PrintProofReport(iPrCount - 1)
  EXIT SUB
  ' Print a report
  FILNUM = FREEFILE
  OPEN "lpt1:" FOR OUTPUT AS FILNUM
  PRINT #FILNUM, S$; "PROOF PRESSURE TEST        DATE: "; LDT$
  PRINT #FILNUM, S$; QC$
  PRINT #FILNUM, ""
  PRINT #FILNUM, S$; "KSP MODEL P/N:     "; PN$
  IF CN$ <> "" THEN PRINT #FILNUM, S$; "CUSTOMER P/N:      "; CN$
  IF LOT$ <> "." THEN PRINT #FILNUM, S$; "LOT NUMBER:        "; LOT$
  PRINT #FILNUM, S$; "TEST INPUT VOLTS= "; VT; "VDC"
  PRINT #FILNUM, S$; "ACTUAL P.S. VOLTS="; fnx(Vs); "VDC"
  PRINT #FILNUM, ""
  '   PRINT #filnum, S$; "FULL SCALE PRESS= "; PM; PR$
  PRINT #FILNUM, ,
  FOR iCount = 1 TO iPrCount - 1
        PRINT #FILNUM, glbProofPrVal(iCount),
  NEXT
 
  PRINT
 
 
  FOR IU = 1 TO NU
     FOR iCount = 1 TO iPrCount - 1
          '  PRINT #filnum, S$; "TEST PORT NUMBER: "; iU
          ' PRINT #filnum, S$; "KULITE S/N: "; SN$(IU)
          PRINT #FILNUM, S$; SN$(IU), glbArrVoProof(IU, iCount),
      NEXT
  NEXT
  
  CLOSE #FILNUM
  IF bolChange THEN AMP$ = "N"
END SUB

SUB RUNTEST (BSA%, port%, RM%(), M%)

' **** RESISTANCE AND VOLTAGE MEASUREMENTS @ TEMPERATURE ***

DIM YN$

' **** DELAY LOOP ***

        CLS
        INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
        IF Continue$ = "N" THEN EXIT SUB
        IF STTime = 0 THEN STTime = STime
        TGONE = (ABS(TIMER - STTime) / 60)
        IF TGONE < 5 THEN
                IF NOT Debug THEN
                        PRINT "A MINIMUM 5 WARM-UP IS REQUIRED."
                        PRINT "PLEASE WAIT."
                        TWAIT# = (5 - TGONE) * 60
                        IF NOT warmup# THEN TWAIT# = 5
                        WTIME = TIMER + TWAIT#
                        DO
                        LOOP UNTIL TIMER > WTIME

                ELSE
                        PRINT "A MINIMUM 3 MIN WARM-UP IS REQUIRED."
                        PRINT "PLEASE WAIT - CANCELED DUE TO DEBUG MODE."
                END IF
        END IF


' **** INITIALIZE POWER SUPPLY

        PRINT
        PRINT
        DO
          PRINT "IS THE SETUP READY FOR"; VE; "VOLTS TO BE APPLIED";
          INPUT YN$
          PRINT
        LOOP UNTIL YN$ = "Y"
        CALL MeasVinNew(BSA%, port%, RM%(), M%, "Y")

' **** TAKE MEASUREMENTS

      '  CLS
        PRINT
        PRINT "MEASUREMENTS OF RES AND OUTPUT VOLTAGE"
        JT = 0
        IF DATFILOPT% = FINISHTST% THEN JT = LT + 1
        IF JT <= NT THEN
          PP = 1
          FOR IT = JT TO NT
            IF IT >= HT THEN PP = 2: IF IT = NT AND RTN <> 0 THEN PP = 1
            LT = IT
            CALL SINGLETEMP(BSA%, port%, RM%(), M%)
            IF Continue$ = "N" THEN EXIT SUB
            IF StopNow% THEN EXIT SUB
          NEXT IT
          IF NT = 0 THEN EXIT SUB
        END IF

 '**** REDO TEMPERATURES IF NEEDED

        PRINT
        IF NOT glbbolMeasZeroA THEN INPUT "DO YOU WISH TO MEASURE ZERO A OUTPUT (Y/N)"; YN$
        IF YN$ = "Y" THEN glbbolMeasZeroA = -1
        PRINT "TEST HAS BEEN COMPLETED."
        INPUT "WANT TO RETEST ANY TEMP TEST (Y/N)"; YN$
        IF YN$ = "Y" THEN
          DO
            FOR IT = 0 TO NT
              PRINT "TEMP #"; IT + 1; "="; TM(IT)
            NEXT IT
            DO
              INPUT "WHICH TEMP #:"; CT
              CT = CT - 1
              BADNUM = CT < 0 OR CT > NT
              IF BADNUM THEN
                PRINT "WRONG!! TRY AGAIN"
                PRINT
              END IF
            LOOP UNTIL NOT BADNUM
            PP = 1
            IF CT >= HT THEN
              PP = 2
              IF CT = NT AND RTN <> 0 THEN PP = 1
            END IF
            IT = CT
            LT = NT
            CALL SINGLETEMP(BSA%, port%, RM%(), M%)
            IF Continue$ = "N" THEN EXIT SUB
            IF StopNow% THEN EXIT SUB
            INPUT "REDO ANOTHER TEST(Y/N)"; YN$
          LOOP WHILE YN$ = "Y"
        END IF

END SUB

SUB SAVEDATA

DIM YN$

' **** SAVE DATA FILE ****

         OPEN DATFILE$ FOR OUTPUT AS #10
         X = 0
         X$ = "."
         PRINT #10, X$; C$; DAT$; C$; CN$; C$; PN$; C$; TP$; C$; AB$; C$; RTN; C$; LOT$
         PRINT #10, PR$; C$; TB$; C$; RP$; C$; QC$; C$; DP$; C$; PT$; C$; AT$; C$; X$
         PRINT #10, IV; C$; RTNUM#; C$; X; C$; X; C$; VT; C$; Vs; C$; NT; C$; LT
         PRINT #10, NP(1); C$; NP(2); C$; MP(1); C$; MP(2); C$; PP; C$; P0; C$; PM; C$; NU
         PRINT #10, VE; C$; Vs(0); C$; Vs(1); C$; Vs(2); C$; Vs(3); C$; Vs(4); C$; Vs(5); C$; Vs(6)
         PRINT #10, Vs(7); C$; Vs(8); C$; Vs(9); C$; Vs(10); C$; Vs(11); C$; Vs(12); C$; Vs(13)
         PRINT #10, PR; C$; AP; C$; HT; C$; LD; C$; AMP$; C$; Reg$; C$; X; C$; X
         PRINT #10, glbbolMeasZeroA
        
         IF glbbolMeasZeroA THEN        ' read in zeroA measurements
              FOR IUNIT = 1 TO NU
                FOR ITEMP = 0 TO NT
                   PRINT #10, glbArrZeroA(IUNIT, ITEMP)
                NEXT
              NEXT
         END IF

         FOR j = 0 TO NT
           PRINT #10, TM(j)
         NEXT j
         FOR P = 1 TO 2
           FOR K = 1 TO NP(P)
             PRINT #10, PR(K, P)
           NEXT K
         NEXT P
         FOR I = 1 TO NU
           PRINT #10, SN$(I); C$; SE(I); C$; PF(I); C$; X; C$; X
           P = 1
           FOR j = 0 TO NT
             PRINT #10, RI(I, j); C$; Ro(I, j); C$; CMVGTB(I, j); C$; CMVWTB(I, j); C$; X
             IF j >= HT THEN
               P = 2
               IF j = NT AND RTN <> 0 THEN P = 1
             END IF
             FOR K = 1 TO NP(P)
               PRINT #10, VO(I, K, j)
             NEXT K
           NEXT j
         NEXT I
         CLOSE 10
         YN$ = "N"
         IF LT < NT THEN
           PRINT
           INPUT "STOP TEST AND FINISH LATER(Y/N)"; YN$
           PRINT
         END IF
         IF YN$ = "Y" THEN
           StopNow% = -1
           CALL ECHODATA
         ELSE
           StopNow% = 0
         END IF

END SUB

SUB SetBits (BSA%, port%, RM%(), M%)
DIM bits%(70)
DIM bitdesc$(70)
DIM Counts

bitdesc$(1) = "D Group (Spare)"
bitdesc$(2) = "C Group (Current Source Res)"
bitdesc$(3) = "C Group (Current Source Res)"
bitdesc$(4) = "B Group (Sensor Select I Res)"
bitdesc$(5) = "A Group (Sensor Output)"
bitdesc$(6) = "A Group (Sensor Output)"
bitdesc$(7) = "K1 (PS Bus Measurements)"
bitdesc$(8) = "K2 (20 Ohm res Input Meas)"
bitdesc$(9) = "K3 (Pressure Sensor Output)"
bitdesc$(10) = "K4 (Current Source Connect)"
bitdesc$(11) = "Select unit Port #8"
bitdesc$(12) = "Select unit Port #7"
bitdesc$(13) = "Select unit Port #6"
bitdesc$(14) = "Select unit Port #5"
bitdesc$(15) = "Select unit Port #4"
bitdesc$(16) = "Select unit Port #3"
bitdesc$(17) = "Select unit Port #2"
bitdesc$(18) = "Select unit Port #1"
bitdesc$(19) = "Select unit Port #16"
bitdesc$(20) = "Select unit Port #15"
bitdesc$(21) = "Select unit Port #14"
bitdesc$(22) = "Select unit Port #13"
bitdesc$(23) = "Select unit Port #12"
bitdesc$(24) = "Select unit Port #11"
bitdesc$(25) = "Select unit Port #10"
bitdesc$(26) = "Select unit Port #9"
bitdesc$(27) = "K5 (4 K-Ohm Cal Res)"
bitdesc$(28) = "K6 (1 K-Ohm Cal Res)"
bitdesc$(29) = "K7 (20 ohm Bus #1&-3)"
bitdesc$(30) = "K8 (Output Resistance)"
bitdesc$(31) = "K9 (Low P.S. -  1.26 V)"
bitdesc$(32) = "K10 (Temp Probe Output)"
bitdesc$(33) = "K11 (Common Mode Voltage)"
bitdesc$(34) = "K12 (High V Divider)"
bitdesc$(35) = "K13 (400 mV Range)"
bitdesc$(36) = "K14 (80 mV Range)"
bitdesc$(37) = "PS 28.0V"
bitdesc$(38) = "PS 24.0V"
bitdesc$(39) = "PS 20.0V"
bitdesc$(40) = "PS 15.0V"
bitdesc$(41) = "PS 12.0V"
bitdesc$(42) = "PS 10.0V"
bitdesc$(43) = "PS 8.0V"
bitdesc$(44) = "PS 7.5V"
bitdesc$(45) = "PS 7.0V"
bitdesc$(46) = "PS 6.0V"
bitdesc$(47) = "PS 5.0V"
bitdesc$(48) = "PS 4.0V"
bitdesc$(49) = "PS 3.5V"
bitdesc$(50) = "PS 3.0V"
bitdesc$(51) = "PS 2.5V"
bitdesc$(52) = "PS 2.0V"
bitdesc$(53) = "Not Used"
bitdesc$(54) = "Not Used"
bitdesc$(55) = "Not Used"
bitdesc$(56) = "Not Used"
bitdesc$(57) = "Not Used"
bitdesc$(58) = "Not Used"
bitdesc$(59) = "Not Used"
bitdesc$(60) = "Not Used"
bitdesc$(61) = "Not Used"
bitdesc$(62) = "Not Used"
bitdesc$(63) = "Select unit Port #24"
bitdesc$(64) = "Select unit Port #23"
bitdesc$(65) = "Select unit Port #22"
bitdesc$(66) = "Select unit Port #21"
bitdesc$(67) = "Select unit Port #20"
bitdesc$(68) = "Select unit Port #19"
bitdesc$(69) = "Select unit Port #18"
bitdesc$(70) = "Select unit Port #17"

'  SET BITS AND EXECUTE....
WIDTH 80, 50
Counts = 0
FOR I% = 1 TO 70
        bits%(I%) = 0
NEXT I%
CALL ClearTrain(RM%(), M%)
DO
        CLS
        PRINT
        PRINT "   Exit = 0 / Send train = -1 / Read counts = -2  / Continuous Reading = -3"
        PRINT
        PRINT "                      Raw = ";
        X.POS = POS(0)                             'CAPTURE X POSITION
        Y.POS = CSRLIN                             'CAPTURE Y POSITION
        PRINT Counts; " A/D Counts = "; Counts - 10001
        PRINT
        PRINT "bit  0/1  Description                    bit  0/1  Description"
PRINT "---------------------------------------  ---------------------------------------"
FOR I% = 1 TO 35
PRINT USING " ##   #   \                           \  ##   #   \                         \"; I%; bits%(I%); bitdesc$(I%); (I% + 35); bits%(I% + 35); bitdesc$(I% + 35)
        NEXT I%
        PRINT
        PRINT
        INPUT "WHICH BIT DO YOU WANT TO SET OR RESET: "; bit%
        IF bit% = -3 THEN PRINT "(Press B to stop)"
        IF bit% > 0 AND bit% < 71 THEN
                IF RM%(bit%) = 1 THEN
                   RM%(bit%) = 0
                   bits%(bit%) = 0
                ELSE
                   RM%(bit%) = 1
                   bits%(bit%) = 1
                END IF
        END IF
        IF bit% = -1 THEN CALL LOADTRAIN(BSA%, port%, RM%(), M%)
        IF bit% = -2 THEN
                OUT BSA% + 1, &HDF
                OUT BSA% + 1, &H9
                OUT BSA%, 0
                OUT BSA%, 0
                OUT BSA% + 1, &H7F
                OUT BSA% + 3, &H10 OR port%
                CALL DELAY(100)
                lastread& = 0
        DO
                lastread& = Counts
                OUT BSA% + 1, &HBF                ' A/D DATA TO HOLDING REGISTERS
                OUT BSA% + 1, &H11                ' POINT TO CTR 1 HOLD REG
                CALL DELAY(100)                    ' DELAY TO SET REGISTERS
              
               Counts = INP(BSA%)
               Counts = Counts + 256 * INP(BSA%)
                IF Counts <> 0 THEN
                        OUT BSA% + 3, &HEF AND port%
                END IF
        LOOP UNTIL lastread& = Counts AND Counts <> 0
        END IF ' if bit = -2

        IF bit% = -3 THEN
          DO
                OUT BSA% + 1, &HDF
                OUT BSA% + 1, &H9
                OUT BSA%, 0
                OUT BSA%, 0
                OUT BSA% + 1, &H7F
                OUT BSA% + 3, &H10 OR port%
                CALL DELAY(100)
                lastread& = 0
        DO
                lastread& = Counts
                OUT BSA% + 1, &HBF                ' A/D DATA TO HOLDING REGISTERS
                OUT BSA% + 1, &H11                ' POINT TO CTR 1 HOLD REG
                CALL DELAY(100)                    ' DELAY TO SET REGISTERS
             
               Counts = INP(BSA%)
               Counts = Counts + 256& * INP(BSA%)
                IF Counts <> 0 THEN
                        OUT BSA% + 3, &HEF AND port%
                END IF
        LOOP UNTIL lastread& = Counts AND Counts <> 0
        LOCATE Y.POS, X.POS                     'SET CURSOR TO PRINT LOCATION
        PRINT Counts; " A/D Counts = "; Counts - 10001
    LOOP UNTIL UCASE$(INKEY$) = "B"
     END IF ' if bit = -3




LOOP UNTIL bit% = 0
CALL SetPowerLow(BSA%, port%, RM%(), M%)
WIDTH 80, 25
END SUB

SUB SetPowerLow (BSA%, port%, RM%(), M%)

' **** SET POWER SUPPLY TO 1.26 V
        CALL ClearTrain(RM%(), M%)
        RM%(29) = 1                             'Set 20 Ohm connection
        IF BoxType% = 1 THEN                    'Box type old
                RM%(27) = 1                     '4K Resistor
        ELSE                                    'Box Type New
                RM%(28) = 1                     '2K Resistor
        END IF                                  'End select
        CALL LOADTRAIN(BSA%, port%, RM%(), M%)
END SUB

FUNCTION SetRefPressure# (P, BSA%, port%, RM%(), M%)
DIM X.POS                                  'X COOR FOR SCREEN
DIM Y.POS                                  'Y COOR FOR SCREEN
DIM Counts&                                'COUNTS FOR A/D BOX
DIM JUMP$                                  'LOOP ESCAPE VARIABLE
DIM Pressure#                              'PRESSURE READING
DIM HOLD31%                                'TRAIN BIT 34'S SETTING
DIM I%                                     'INDEX OF COUNT
DIM TBIT%                                  'TRAIN BIT SET

IF RTNUM# = 0 OR P = 0 THEN
        PRINT
        PRINT "SET PRESSURE TO "; P; PR$;
        IF PR$ <> PT$ THEN
           X = P * PR + AP: X$ = FORMATSTR$(X, LF, LI)
           PRINT " ("; X$; " "; PT$; ")"
        END IF
        SetRefPressure# = 1                'Set coeff to one
        EXIT FUNCTION                      'REFFERENCE TRANSDUCER NOT USED
END IF
PRINT
PRINT
PRINT "REFERENCE TRANSDUCER #"; RTNUM#; " FSP = "; RTFSP#; RTPTYPE$
PRINT
PRINT "TARGET PRESSURE ="; P; PR$;
IF PR$ <> PT$ THEN
        X = P * PR + AP: X$ = FORMATSTR$(X, LF, LI)
        PRINT " ("; X$; " "; PT$; ")"
END IF
PRINT
PRINT
PRINT "ENTER B TO PROCEED WITH VOLTAGE READING"
PRINT
IF Debug THEN
        PRINT "DEBUG MODE CANCELS PRESSURE REFERENCE TESTING"
        PRINT "ENTER B TO PROCEED WITH VOLTAGE READING"
        DO                                 'LOOP
                JUMP$ = INKEY$             'ESCAPE LOOP
        LOOP UNTIL JUMP$ = "B"             'DETECT ESCAPE
        SetRefPressure# = 1                'Set coeff to one
        EXIT FUNCTION                      'REFFERENCE TRANSDUCER NOT USED
END IF
HOLD31% = RM%(31)                          'KEEP BIT 31'S SETTING
TBIT% = 0                                  'DEFAULT TBIT
FOR I% = 37 TO 52                          'LOOP THROUGH PS SETTINGS
IF RM%(I%) = 1 THEN                        'IDENTIFY BIT
        TBIT% = I%                         'SAVE CORRECT BIT
END IF
NEXT I%                                    'END LOOP
PRINT
PRINT "PRESSURE =";
X.POS = POS(0)                             'CAPTURE X POSITION
Y.POS = CSRLIN                             'CAPTURE Y POSITION
CALL ClearTrain(RM%(), M%)                 'CLEAR THE BIT TRAIN
IF BoxType% = 2 THEN
        RM%(32) = 1                        'READ REF TRANS PORT
ELSE
        RM%(9) = 1                         'READ REF TRANS PORT
END IF
RM%(31) = HOLD31%                          'SET BIT 31
IF TBIT% > 0 THEN RM%(TBIT%) = 1           'SET POWER SUPPLY BIT

DO                                         'LOOP UNTIL PRESSURE IS CORRECT
     Counts& = ReadA2DNew(BSA%, port%, RM%(), M%)
     X# = Counts& / 20000                  'Max counts are 20000
     VX# = CO(Range%, 1) * X# * X# + CO(Range%, 2) * X# + CO(Range%, 3)
     VX# = VX# * 1000                      'Convert mV to V
     Pressure# = (RTCOEF3# * VX# * VX# * VX# + RTCOEF2# * VX# * VX# + RTCOEF1# * VX# + RTCOEF0#) * (5# / PportV#)
     LOCATE Y.POS, X.POS                   'SET CURSER TO PRINT LOCATION
     PRINT USING "######.## !"; Pressure#; RTPTYPE$;  'PRINT PRESSURE READING
     JUMP$ = INKEY$                        'ESCAPE LOOP
LOOP UNTIL JUMP$ = "B"                     'DETECT ESCAPE
     SetRefPressure# = ABS(Pressure# / P)  'Calculate Normalization Coeff.
END FUNCTION

SUB SINGLETEMP (BSA%, port%, RM%(), M%)

DIM YN$

' **** SINGLE TEMP MEASUREMENT ***
        DO
          PRINT
          TE = TM(IT)
          PRINT "TEMPERATURE #:"; IT + 1
          IF IT = 1 AND RP$ = "Y" THEN PRINT "(REPEATABILITY CYCLE)";
          IF IT = 1 AND RP$ = "N" AND DP$ = "Y" THEN PRINT "(BACK PORT CYCLE)";
          IF IT = 2 AND RP$ = "Y" AND DP$ = "Y" THEN PRINT "(BACK PORT CYCLE)";
          PRINT
          PRINT "SET OVEN TEMPERATURE TO"; TE; TP$
          PRINT
          PRINT

'**** TEMPERATURE STABILITY TEST ***

        CALL TEMPSTAB(BSA%, port%, RM%(), M%)
        INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
        IF Continue$ = "N" THEN EXIT SUB

' *** proof pressure

          IF IT = 0 AND PF$ = "Y" THEN
                CALL PROOFPRESSURE(BSA%, port%, RM%(), M%)
                INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
                IF Continue$ = "N" THEN EXIT SUB
          END IF

' *** INPUT/OUTPUT RESISTANCES

          CALL RioArrayNew(BSA%, port%, RM%(), M%)  'In/Out resistance
          INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
          IF Continue$ = "N" THEN EXIT SUB

' *** COMMON MODE VOLTAGE

          IF MenuOp% = 4 OR IT = 0 THEN
                CALL CMVARRAY(BSA%, port%, RM%(), M%)
                INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
                IF Continue$ = "N" THEN EXIT SUB
          END IF

' *** VOUT at pressure

               CALL VOUT(BSA%, port%, RM%(), M%)
               INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
               IF Continue$ = "N" THEN EXIT SUB
    
' **** COMPILED RESULTS ****

               CALL RESULTS                    ' Print or View results

' ***** Retest at temperature *****

                INPUT "DO YOU WANT TO REDO THIS TEMPERATURE TEST(Y/N)"; YN$
        LOOP WHILE YN$ = "Y"

' ***** Save data ******

        IF MenuOp% > 2 THEN
                CALL SAVEDATA
                ZAVEDSET$ = DATFILE$
                DATFILE$ = LEFT$(SN$(1), 8) + ".RD"
                CALL SAVEDATA
                DATFILE$ = ZAVEDSET$
        END IF
          
END SUB

SUB SummaryReport
' Purpose:  Prints summary report for TestLot.

DIM strAns AS STRING
DIM arrRejected(MU) AS INTEGER      ' keeps track of rejected ports
DIM iRejectCount AS INTEGER         ' Counts the number of rejected units
DIM iTotalUnits AS INTEGER          ' Total number of units excluding rejects
DIM sngTCRSum(3) AS SINGLE          ' Sum of TCR's
DIM sngArrTCGFSum(3) AS SINGLE      ' Sum of TCGF's
DIM sngArrNullShiftSum(3) AS SINGLE ' Sum of Null Shifts (absolute)
DIM sngRoutSum AS SINGLE            ' Sum of R out's
DIM sngLinSum AS SINGLE             ' Sum of absolute nonlinearities
DIM sngNetSum AS SINGLE             ' Sum of RT net outputs
DIM sngZeroSum AS SINGLE            ' Sum of Nulls
DIM sngBrSensSum AS SINGLE          ' Sum of bridge sensitivities
DIM sngHystSum AS SINGLE            ' Sum of Hysteresis values
DIM sngRinSum AS SINGLE             ' Sum of Input resistances
DIM sngSensMax AS SINGLE            ' Maximum value of sensitivity
DIM sngSensMIN AS SINGLE            ' Minimum value of sensitivity
DIM sngHystMax AS SINGLE
DIM sngHystMin AS SINGLE
DIM sngLinMin  AS SINGLE
DIM sngLinMax AS SINGLE
DIM sngNetMin AS SINGLE
DIM sngNetMax  AS SINGLE
DIM sngRinMin  AS SINGLE
DIM sngRinMax AS SINGLE
DIM sngRoutMin AS SINGLE
DIM sngRoutMax AS SINGLE
DIM sngSetMin AS SINGLE
DIM sngSetMax AS SINGLE
DIM sngZeroMin AS SINGLE
DIM sngZeroMax AS SINGLE


     PRINT
     PRINT
     PRINT "TESTLOT SUMMARY REPORT"
     PRINT

' **** READ IN DATA

        DATFILE$ = "TL"
        filegood% = Attrib%(DATFILE$)
        IF filegood% <> -1 THEN
          OPEN DATFILE$ FOR INPUT AS #10
          INPUT #10, X$, LDT$, X$, X$, X$, X$, X$, X$, X$, TB$
          CLOSE 10
          PRINT
          PRINT " INFORMATION ON LAST TEST: "
          PRINT
          PRINT " TESTED BY:  "; TB$
          PRINT " TESTED ON:  "; LDT$
          PRINT
        ELSE
          PRINT "THERE IS NO PREVIOUS TEST DATA ON DISK."
          PRINT "YOU CANNOT PRINT A REPORT AT THIS TIME."
          PRINT
          INPUT "PRESS ENTER TO CONTINUE..."; YN$
          EXIT SUB
        END IF

        CALL READDATA
        INPUT "ARE YOU SURE YOU WANT TO PRINT THIS REPORT"; YN$
        IF YN$ <> "Y" THEN EXIT SUB
         
            Ready% = 0
            DO
                CALL CheckPrinter(1, Ready%)
            LOOP UNTIL Ready% = -1

' **** ASK ANY NECESSARY QUESTIONS:


        DO
          PRINT "LINEARITY CALCULATION:"
          INPUT "    1) END POINT  2) BEST FIT - "; AM
        LOOP WHILE AM <> 1 AND AM <> 2
      '  INPUT "INCLUDE LEAD RESISTANCE INFO(Y/N) "; LC$

'    *** OPEN SUMMARY REPORT DATA FILE ***

        OPEN "SUMMARY" FOR OUTPUT AS #10
        PRINT #10, X$; C$; LDT$; C$; NU

' **** OPEN PRINTING FILE #
       ' OPEN "SCRN:" FOR OUTPUT AS #4
        OPEN "LPT1:" FOR OUTPUT AS #4

iRejectCount = 0
sngRoutSum = 0
sngLinSum = 0
sngNetSum = 0
sngZeroSum = 0
sngBrSensSum = 0
sngHystSum = 0
sngRinSum = 0
sngSetSum = 0

FOR ITEMP = 1 TO 3
     sngTCRSum(ITEMP) = 0
     sngArrTCGFSum(ITEMP) = 0
     sngArrNullShiftSum(ITEMP) = 0
NEXT

' ASK USER IF ANY UNITS NEED TO BE REJECTED:
PRINT
PRINT "There are "; NU; " units in this test group."
DO
   INPUT "Do you wish to reject any units (Y/N)"; strAns
LOOP UNTIL strAns = "Y" OR strAns = "N" OR strAns = ""

IF strAns = "Y" THEN
     DO
          PRINT
          PRINT "There are "; NU - iRejectCount; " units in this test group."
          iRejectCount = iRejectCount + 1
          INPUT "Enter Port Number to reject: ", iPort
          arrRejected(iPort) = 1
          PRINT
          INPUT "Any others (Y/N)"; strAns
     LOOP WHILE strAns = "Y"
END IF

iTotalUnits = NU - iRejectCount    ' Used to compute average

FOR IU = 1 TO NU
  IF arrRejected(IU) = 0 THEN
     VZ = VO(IU, 1, 0)
     FS = VO(IU, MP(1), 0) - VZ    ' THIS IS FSO FOR ROOM TEMP
     LM = 0
     CM = 0
     HM = 0
     RM = 0
     SE = FS / (PM - PR(1, 1))
     PRINT #10, SN$(IU); C$; fnx(VZ); C$; fnx(SE); C$; FNR(Ro(IU, 0)); C$; FNR(RI(IU, 0))
     DP = 0
     DN = 0
     FOR IP = 1 TO NP(1)
       VI = VO(IU, IP, 0)
       PI = PR(IP, 1)
       DV = VI - (VZ + SE * (PI - PR(1, 1)))
       IF DV > DP THEN DP = DV
       IF DV < DN THEN DN = DV
     NEXT IP
     BF = 0
     IF AM = 2 THEN BF = (DP + DN) / 2
     FOR IP = 1 TO NP(1)
       VI = VO(IU, IP, 0)
       PI = PR(IP, 1)
       SP$ = "         "
       VL = VZ + BF + FS * PI / (PM - PR(1, 1))
       DV = VI - VL
       IF IP <= MP(1) AND ABS(DV) > ABS(LM) THEN LM = DV
       IF MP(1) < NP(1) AND ABS(DV) > ABS(CM) THEN CM = DV
       IF IP > MP(1) THEN
         L = NP(1) + 1 - IP
         HY = ABS(VI - VO(IU, L, 0))
         IF HY > HM THEN HM = HY
       END IF
       IF RP$ = "Y" THEN
         VR = VO(IU, IP, 1)
         DR = VI - VR
         IF ABS(DR) > ABS(RM) THEN RM = DR
       END IF
       IF AMP$ = "N" THEN
         LI = 7
         LF = 2
         FORMAT$ = "&  ####.#   ####.##   &"
       ELSE
         LI = 6
         LF = 3
         FORMAT$ = "&  ####.#   ###.###   &   ###.##  ###.##"
       END IF
       IF IP = MP(1) THEN
         SP$ = FSTR$(FS, LI, LF)
       ELSE
         SP$ = SPACE$(LI + LF + 1)
       END IF
       M = 1
       IF AMP$ = "Y" THEN
         M = 1000
       END IF
                  
     NEXT IP
     IF NT > 0 THEN
       FOR IT = HT TO NT
         IF IT = NT AND RTN <> 0 THEN
           PP = 1
         ELSE
           PP = 2
         END IF
         SP = 1
         LP = NP(PP)
         IF IT = NT THEN
           IF RTN = 1 THEN LP = 1
           IF RTN = 2 THEN LP = MP(PP): SP = LP - 1
           IF RTN = 3 THEN SP = MP(PP) - 1
         END IF
         FOR IP = 1 TO LP STEP SP
           PI = PR(IP, PP)
           LI = 7: LF = 2: IF AMP$ = "Y" THEN LI = 6: LF = 3
           IF IP = MP(PP) THEN
             FSO = VO(IU, MP(PP), IT) - VO(IU, 1, IT)
             SP$ = FSTR$(FSO, LI, LF)
           ELSE
             SP$ = SPACE$(LI + LF + 1)
           END IF
           VI = VO(IU, IP, IT)
         
         NEXT IP
       NEXT IT
     END IF
' **** CHECK IT OUT!!!!!!!
IF FS < 1 THEN
  
ELSE
  PERCENT.FS = 100 / FS
  LM = ABS(PERCENT.FS * LM)
  CM = PERCENT.FS * CM
  HM = PERCENT.FS * HM
  RM = PERCENT.FS * RM
  SB = SQR(LM * LM + HM * HM + RM * RM)

             
   IF NT = 4 THEN
     iNewNT = 3
   ELSE
     iNewNT = NT
   END IF

             
   ' *** TCR CALCULATIONS ***

   FORMAT$ = "&###.###   ###.###   ###.###"
   IF iNewNT = 2 THEN
     ' COMPUTE TCR FOR R-H ONLY
     glbArrTCR(IU, 1) = ((RI(IU, 0) - RI(IU, 1)) / RI(IU, 0)) / ((TM(0) - TM(1)) / 100#) * 100#
   ELSE
     glbArrTCR(IU, 1) = ((RI(IU, 2) - RI(IU, 1)) / RI(IU, 0)) / ((TM(2) - TM(1)) / 100#) * 100#
     glbArrTCR(IU, 2) = ((RI(IU, 0) - RI(IU, 1)) / RI(IU, 0)) / ((TM(0) - TM(1)) / 100#) * 100#
     glbArrTCR(IU, 3) = ((RI(IU, 2) - RI(IU, 0)) / RI(IU, 0)) / ((TM(2) - TM(0)) / 100#) * 100#
   END IF
   ' *** END TCR CALCULATIONS
              
   'PRINT #4, S$; "TCGF(%/100F)"; S$;
  IF iNewNT = 3 THEN
   IF FS <> 0 THEN
        ' Calculate Span shift from cold to high temp
        FSO = VO(IU, 2, 2) - VO(IU, 1, 2)
        CFS = VO(IU, 2, 1) - VO(IU, 1, 1)
        glbArrSpanShift(IU, 1) = ((FSO - CFS) / FS) / ((TM(2) - TM(1)) / 100#) * 100#
                  
        ' Calculate Span shift from cold to room temp
        FSO = VO(IU, 2, 1) - VO(IU, 1, 1)
        glbArrSpanShift(IU, 2) = ((FSO - FS) / FS) / ((TM(1) - TM(0)) / 100#) * 100#

        ' Calculate Span shift from room to high temp
        FSO = VO(IU, 2, 2) - VO(IU, 1, 2)
        glbArrSpanShift(IU, 3) = ((FSO - FS) / FS) / ((TM(2) - TM(0)) / 100#) * 100#

        ' Calculate Null Shift from Cold to Hot
        glbArrNullShift(IU, 1) = ((VO(IU, 1, 2) - VO(IU, 1, 1)) / FS) / ((TM(2) - TM(1)) / 100#) * 100#

        ' Calculate Null Shift from cold to room
         glbArrNullShift(IU, 2) = ((VO(IU, 1, 0) - VO(IU, 1, 1)) / FS) / ((TM(0) - TM(1)) / 100#) * 100#
                    
        ' Calculate Null Shift from Room to Hot
        glbArrNullShift(IU, 3) = ((VO(IU, 1, 2) - VO(IU, 1, 0)) / FS) / ((TM(2) - TM(0)) / 100#) * 100#
   END IF
  ELSE
     ' calculate for R-H only
        ' Calculate Span shift from Hot to room temp
        FSO = VO(IU, 2, 1) - VO(IU, 1, 1)
        glbArrSpanShift(IU, 1) = ((FSO - FS) / FS) / ((TM(1) - TM(0)) / 100#) * 100#
        ' calc null shift
        glbArrNullShift(IU, 1) = ((VO(IU, 1, 0) - VO(IU, 1, 1)) / FS) / ((TM(0) - TM(1)) / 100#) * 100#
  END IF

            
       IF NT > 0 THEN
         LT = NT
         IF RTN > 0 THEN LT = NT - 1
         Z1 = 0
         Z2 = 0
         F1 = 0
         F2 = 0
         FOR IT = HT TO LT
           SIGN = SGN(TM(IT) - TR)
           IF ABS(TM(IT) - TR) <> 0 THEN
                 PER.100F = 100 / ABS(TM(IT) - TR)
           ELSE
                 PER.100F = 1
           END IF
           ZT = VO(IU, 1, IT)
           FT = VO(IU, MP(2), IT) - ZT
           DEL.Z = SIGN * (ZT - VZ)
           DEL.FS = SIGN * (FT - FS)
           D1 = DEL.Z
           IF ABS(D1) > ABS(Z1) THEN
             T1 = TM(IT)
             Z1 = D1
           END IF
           D2 = DEL.Z * PER.100F
           IF ABS(D2) > ABS(Z2) THEN
             T2 = TM(IT)
             Z2 = D2
           END IF
           D3 = DEL.FS
           IF ABS(D3) > ABS(F1) THEN
             F1 = D3
             T3 = TM(IT)
           END IF
           D4 = DEL.FS * PER.100F
           IF ABS(D4) > ABS(F2) THEN
             F2 = D4
             T4 = TM(IT)
           END IF
         NEXT IT
         Z1 = PERCENT.FS * Z1
         F1 = PERCENT.FS * F1
         Z2 = PERCENT.FS * Z2
         F2 = PERCENT.FS * F2
         TB = SQR(SB * SB + Z1 * Z1 + F1 * F1)
       END IF
     END IF

'******************
     IF (arrRejected(IU) = 0) THEN           ' IF THIS UNIT IS NOT REJECTED, INCLUDE ITS STATS.
          FOR ITEMP = 1 TO 3   ' loop thru 3 categories: C-H, C-R, R-H
               sngTCRSum(ITEMP) = sngTCRSum(ITEMP) + glbArrTCR(IU, ITEMP)
               sngArrTCGFSum(ITEMP) = sngArrTCGFSum(ITEMP) + glbArrSpanShift(IU, ITEMP)
               sngArrNullShiftSum(ITEMP) = sngArrNullShiftSum(ITEMP) + glbArrNullShift(IU, ITEMP)
          NEXT
    
          IF sngBrSensSum = 0 THEN
               sngSensMax = SE
               sngSensMIN = sngSensMax
              
               sngHystMax = HM
               sngHystMin = sngHystMax
              
               sngLinMin = LM
               sngLinMax = sngLinMin
              
               sngNetMin = FS
               sngNetMax = sngNetMin
              
               sngRinMin = RI(IU, 0)
               sngRinMax = sngRinMin
              
               sngRoutMin = Ro(IU, 0)
               sngRoutMax = sngRoutMin
              
               sngSetMin = (VO(IU, 1, NT) - VZ)
               sngSetMax = sngSetMin
              
               sngZeroMin = VZ
               sngZeroMax = sngZeroMin

          ELSE
               ' compute max
               IF SE > sngSensMax THEN
                    sngSensMax = SE
               END IF
              
               IF HM > sngHystMax THEN
                    sngHystMax = HM
               END IF
              
               IF LM > sngLinMax THEN
                    sngLinMax = LM
               END IF

               IF FS > sngNetMax THEN
                    sngNetMax = FS
               END IF

               IF RI(IU, 0) > sngRinMax THEN
                    sngRinMax = RI(IU, 0)
               END IF

               IF Ro(IU, 0) > sngRoutMax THEN
                    sngRoutMax = Ro(IU, 0)
               END IF

               IF (VO(IU, 1, NT) - VZ) > sngSetMax THEN
                    sngSetMax = (VO(IU, 1, NT) - VZ)
               END IF

               IF VZ > sngZeroMax THEN
                    sngZeroMax = VZ
               END IF

               '*** compute min****
               IF SE < sngSensMIN THEN
                    sngSensMIN = SE
               END IF
              
               IF HM < sngHystMin THEN
                    sngHystMin = HM
               END IF

               IF LM < sngLinMin THEN
                    sngLinMin = LM
               END IF
               
               IF FS < sngNetMin THEN
                    sngNetMin = FS
               END IF

               IF RI(IU, 0) < sngRinMin THEN
                    sngRinMin = RI(IU, 0)
               END IF
              
               IF Ro(IU, 0) < sngRoutMin THEN
                    sngRoutMin = Ro(IU, 0)
               END IF
              
               IF (VO(IU, 1, NT) - VZ) < sngSetMin THEN
                    sngSetMin = (VO(IU, 1, NT) - VZ)
               END IF

               IF VZ < sngZeroMin THEN
                    sngZeroMin = VZ
               END IF
              
          END IF

          ' Compute Sums
          sngBrSensSum = sngBrSensSum + SE
          sngHystSum = sngHystSum + ABS(HM)
          sngLinSum = sngLinSum + ABS(LM)
          sngNetSum = sngNetSum + FS
          sngRinSum = sngRinSum + RI(IU, 0)
          sngRoutSum = sngRoutSum + Ro(IU, 0)
          sngSetSum = sngSetSum + ABS((VO(IU, 1, NT) - VZ))
          sngZeroSum = sngZeroSum + ABS(VZ)
          
     END IF
 END IF  ' Unit not rejected condition
NEXT
' Print the report

FORMAT$ = "&###.###   ###.###   ###.###"
PRINT #4, S$; "TESTLOT SUMMARY REPORT:"
PRINT #4, ""
PRINT #4, ""
IF LOT$ <> "." THEN PRINT #4, S$; "LOT NUMBER: "; LOT$
PRINT #4, S$; "DATE: "; LDT$
PRINT #4, S$; "FULL SCALE PRESS= "; PM; PR$
PRINT #4, S$; "TEST INPUT VOLTS= "; VT; "VDC"; S$; "ACTUAL P.S. VOLTS="; fnx(Vs); "VDC"
PRINT #4, ""
PRINT #4, S$; "NUMBER OF UNITS:"; iTotalUnits
PRINT #4, ""
PRINT #4, S$; S$; S$; "      AVG (MAG)"; S$; "MIN"; S$; "MAX"
PRINT #4, S$; USING "&###.###      ###.###   ###.###"; "NET OUTPUT (MV):    "; fnx(sngNetSum / iTotalUnits); fnx(sngNetMin); fnx(sngNetMax)
PRINT #4, S$; USING "&###.###      ###.###   ###.###"; "LINEARITY (%FS):    "; fnx(sngLinSum / iTotalUnits); fnx(sngLinMin); fnx(sngLinMax)
PRINT #4, S$; USING "&###.###      ###.###   ###.###"; "HYSTERESIS (%FS):   "; fnx(sngHystSum / iTotalUnits); fnx(sngHystMin); fnx(sngHystMax)
PRINT #4, S$; USING "&&&###.###      ###.###   ###.###"; "BR SENS (MV/"; PR$; "): "; fnx(sngBrSensSum / iTotalUnits); fnx(sngSensMIN); fnx(sngSensMax)
PRINT #4, ""
PRINT #4, S$; USING "&####.###      ####.###  ####.###"; "NULL (MV):        "; fnx(sngZeroSum / iTotalUnits); fnx(sngZeroMin); fnx(sngZeroMax)
PRINT #4, S$; USING "&##.###        ##.###    ##.###"; "NULL SET (MV):      "; fnx(sngSetSum / iTotalUnits); fnx(sngSetMin); fnx(sngSetMax)
PRINT #4, ""
PRINT #4, S$; USING "&#####         #####     #####"; "R IN (OHMS):        "; sngRinSum / iTotalUnits; fnx(sngRinMin); fnx(sngRinMax)
PRINT #4, S$; USING "&#####         #####     #####"; "R OUT (OHMS):       "; sngRoutSum / iTotalUnits; fnx(sngRoutMin); fnx(sngRoutMax)
PRINT #4, ""

IF iNewNT = 3 THEN
     PRINT #4, S$; S$; S$; S$; "     C-H"; S$; "C-R"; S$; "R-H"
ELSE
     PRINT #4, S$; S$; S$; S$; "     R-H"
END IF

PRINT #4, S$;
IF iNewNT = 3 THEN
     PRINT #4, USING FORMAT$; "AVG TCR(%/100F):       "; sngTCRSum(1) / iTotalUnits; sngTCRSum(2) / iTotalUnits; sngTCRSum(3) / iTotalUnits
ELSE
     PRINT #4, USING FORMAT$; "AVG TCR(%/100F):       "; sngTCRSum(1) / iTotalUnits
END IF
PRINT #4, S$;

IF iNewNT = 3 THEN
     PRINT #4, USING FORMAT$; "AVG TCGF(%/100F):      "; sngArrTCGFSum(1) / iTotalUnits; sngArrTCGFSum(2) / iTotalUnits; sngArrTCGFSum(3) / iTotalUnits
ELSE
     PRINT #4, USING FORMAT$; "AVG TCGF(%/100F):      "; sngArrTCGFSum(1) / iTotalUnits
END IF

PRINT #4, S$;

IF iNewNT = 3 THEN
     PRINT #4, USING FORMAT$; "AVG NULL SHIFT(%/100F):"; sngArrNullShiftSum(1) / iTotalUnits; sngArrNullShiftSum(2) / iTotalUnits; sngArrNullShiftSum(3) / iTotalUnits
ELSE
     PRINT #4, USING FORMAT$; "AVG NULL SHIFT(%/100F):"; sngArrNullShiftSum(1) / iTotalUnits
END IF

PRINT #4, NEWPAGE$
CLOSE 4
CLOSE 10
PRINT

END SUB

SUB TEMPSTAB (BSA%, port%, RM%(), M%)
            
DIM YN$

         T = 0
         LV = 0
         IF NOT Debug THEN
                WTIME = 5
         ELSE
                WTIME = .25
         END IF
         IF NOT stabwait# THEN WTIME = NWTIME

          PRINT "TEMPERATURE STABILITY TEST"
       DO
          INPUT "PLEASE CHOOSE INPUT(I) OR OUTPUT(O) RESISTANCE"; IORO$
       LOOP UNTIL IORO$ = "I" OR IORO$ = "O"
       IF IORO$ = "I" THEN
          PSTRING$ = "TIME(MIN)   AVG RIN    %CHANGE/MIN"
       ELSE
          PSTRING$ = "TIME(MIN)   AVG ROUT   %CHANGE/MIN"
       END IF
          INPUT "(HIT ENTER TO BEGIN AND B TO CONTINUE)"; YN$
          PRINT
          PRINT PSTRING$
          PRINT "---------   --------   -----------"
          DO
             BT = TIMER
             SM = 0
             NG = 0
             IF IORO$ = "I" THEN
               CALL RinCalibrate(BSA%, port%, RM%(), M%)
             END IF
             FOR IU = 1 TO NU
             IF IORO$ = "I" THEN
                XIO = MEASRIN(BSA%, port%, RM%(), M%)
             ELSE
                XIO = MEASROUT(BSA%, port%, RM%(), M%)
             END IF
               IF XIO > 0 AND XIO < 10000 AND SN$(IU) <> BD$ THEN
                 NG = NG + 1
                 SM = SM + XIO
               END IF
             NEXT IU
             LI = 10: LF = 0
             X = T * WTIME: TM$ = FORMATSTR$(X, LF, LI)
             IF NG <> 0 THEN
               AV = SM / NG
             ELSE
               AV = 1000000
             END IF
             AV$ = FORMATSTR$(AV, LF, LI)
             X = AV
             IF X <> LV AND LV <> 0 THEN
               LF = 3
               X = 20 * (X - LV) / LV: SLP$ = FORMATSTR$(X, LF, LI)
             ELSE
               SLP$ = "         0.000"
             END IF
             PRINT TM$; AV; SLP$: LV = AV
             DO
               X = (TIMER - BT) / 60
               X$ = INKEY$
               IF X$ = "B" THEN
                 INPUT "ARE YOU SURE"; YN$:
                 IF YN$ <> "Y" THEN X$ = "CONTINUE"
               END IF
             LOOP UNTIL X >= WTIME OR X$ = "B"
             T = T + 1
          LOOP UNTIL X$ = "B"

END SUB

SUB TESTLOT (BSA%, port%, RM%(), M%)
' Runs TestLot

        CLS

        Continue$ = "Y"
        IV = 6
        SV = 1
        PRINT
        PRINT "TESTLOT PROGRAM"
        PRINT
        ' ***** need to change data file routines ******8
        DATFILE$ = "TL"
        CALL DATACHECK
        IF Continue$ = "N" THEN EXIT SUB
        IF DATFILOPT% = STARTOVER% THEN EXIT SUB 'OR filegood% = -1
        IF DATFILOPT% = 1 THEN
          CALL VINSELECT
          CALL DEFTEMPS
          INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
          IF Continue$ = "N" THEN EXIT SUB
          CALL DEFPRESSURES
          INPUT "DO YOU WISH TO CONTINUE (Y/N)"; Continue$
          IF Continue$ = "N" THEN EXIT SUB
        END IF
        IF DATFILOPT% <> FINISHTST% THEN
                CALL LOADSENS
                IF Continue$ = "N" THEN EXIT SUB
        END IF
        CALL RUNTEST(BSA%, port%, RM%(), M%)
        ' WRITE CODE TO CHECK FOR EXISTING PROOF PRESSURE DATA
        ' IF IT EXISTS, PROMPT USER.
        DO
          INPUT "Do you wish to run the PROOF PRESSURE TEST (Y/N)"; YN$
        LOOP UNTIL YN$ = "Y" OR YN$ = "N"
        IF YN$ = "Y" THEN CALL RUNPROOFTEST(BSA%, port%, RM%(), M%)
END SUB

SUB TestLotReport
'  Prints Test Lot Report.  This routine is a modified version of the FinalCalReport Sub.
'  TO DO:
'       ----done:   1) Null Shift Calculations
'         2) Create array to store:  Linearity, mV Out, Rout, Zero Reading, and Zero Shift
'                for Summary report.



DIM YN$


'**** TESTLOT REPORT

     PRINT
     PRINT
     PRINT "TESTLOT REPORT"
     PRINT

' **** READ IN DATA

        DATFILE$ = "TL"
        filegood% = Attrib%(DATFILE$)
        IF filegood% <> -1 THEN
          OPEN DATFILE$ FOR INPUT AS #10
          INPUT #10, X$, LDT$, X$, X$, X$, X$, X$, X$, X$, TB$
          CLOSE 10
          PRINT
          PRINT " INFORMATION ON LAST TEST: "
          PRINT
          PRINT " TESTED BY:  "; TB$
          PRINT " TESTED ON:  "; LDT$
          PRINT
        ELSE
          PRINT "THERE IS NO PREVIOUS TEST DATA ON DISK."
          PRINT "YOU CANNOT PRINT A REPORT AT THIS TIME."
          PRINT
          INPUT "PRESS ENTER TO CONTINUE..."; YN$
          EXIT SUB
        END IF

        CALL READDATA
        INPUT "ARE YOU SURE YOU WANT TO PRINT THIS REPORT"; YN$
        IF YN$ <> "Y" THEN EXIT SUB
          
            Ready% = 0
            DO
                CALL CheckPrinter(1, Ready%)
            LOOP UNTIL Ready% = -1

' **** ASK ANY NECESSARY QUESTIONS:


        DO
          PRINT "LINEARITY CALCULATION:"
          INPUT "    1) END POINT  2) BEST FIT - "; AM
        LOOP WHILE AM <> 1 AND AM <> 2
      '  INPUT "INCLUDE LEAD RESISTANCE INFO(Y/N) "; LC$

'    *** OPEN TESTREPORT DATA FILE ***

        OPEN "TESTLOT" FOR OUTPUT AS #10
        PRINT #10, X$; C$; LDT$; C$; NU

' **** OPEN PRINTING FILE #
        'OPEN "SCRN:" FOR OUTPUT AS #4
        OPEN "LPT1:" FOR OUTPUT AS #4

' **** LOOP THROUGH UNITS AND PRINT ***

        FOR IU = 1 TO NU
          UNITGOOD = 1
'    *** PRINT FOR ONE UNIT
          PRINT #4, S$; "TESTLOT REPORT        DATE: "; LDT$
          PRINT #4, S$; QC$
          PRINT #4, ""
          PRINT #4, S$; "KULITE S/N: "; SN$(IU); S$; "TEST PORT NUMBER: "; IU
          PRINT #4, S$; "KSP MODEL P/N:     "; PN$
          IF CN$ <> "" THEN PRINT #4, S$; "CUSTOMER P/N:      "; CN$
          IF LOT$ <> "." THEN PRINT #4, S$; "LOT NUMBER:        "; LOT$
          PRINT #4, S$; "FULL SCALE PRESS= "; PM; PR$
          PRINT #4, S$; "TEST INPUT VOLTS= "; VT; "VDC"; S$; "ACTUAL P.S. VOLTS="; fnx(Vs); "VDC"
          PRINT #4, ""
          IF LTRIM$(RTRIM$(SN$(IU))) <> "BAD" THEN
            PRINT #4, S$; "TEMPERATURE="; TR
            IF RP$ = "N" THEN
             ' PRINT #4, S$; "PRESSURE    OUTPUT    LINEAR   MV-DEV  NORM FSO"
               PRINT #4, S$; "PRESSURE    OUTPUT    NORM FSO"
            ELSE
             ' PRINT #4, S$; "PRESSURE    OUTPUT    LINEAR   MV-DEV  ";
               PRINT #4, S$; "PRESSURE    OUTPUT    ";
               PRINT #4, "NORM FSO   REPEAT   MV-DEV"
            END IF
            IF glbbolMeasZeroA THEN
                  PRINT #4, USING "&  ###.#&   ####.##   &"; S$; 0; "A"; glbArrZeroA(IU, 0)
            END IF
            VZ = VO(IU, 1, 0)
            FS = VO(IU, MP(1), 0) - VZ    ' THIS IS FSO FOR ROOM TEMP
            LM = 0
            CM = 0
            HM = 0
            RM = 0
            SE = FS / (PM - PR(1, 1))
            PRINT #10, SN$(IU); C$; fnx(VZ); C$; fnx(SE); C$; FNR(Ro(IU, 0)); C$; FNR(RI(IU, 0))
            DP = 0
            DN = 0
            FOR IP = 1 TO NP(1)
              VI = VO(IU, IP, 0)
              PI = PR(IP, 1)
              DV = VI - (VZ + SE * (PI - PR(1, 1)))
              IF DV > DP THEN DP = DV
              IF DV < DN THEN DN = DV
            NEXT IP
            BF = 0
            IF AM = 2 THEN BF = (DP + DN) / 2
            FOR IP = 1 TO NP(1)
              VI = VO(IU, IP, 0)
              PI = PR(IP, 1)
              SP$ = "         "
              VL = VZ + BF + FS * PI / (PM - PR(1, 1))
              DV = VI - VL
              IF IP <= MP(1) AND ABS(DV) > ABS(LM) THEN LM = DV
              IF MP(1) < NP(1) AND ABS(DV) > ABS(CM) THEN CM = DV
              IF IP > MP(1) THEN
                L = NP(1) + 1 - IP
                HY = ABS(VI - VO(IU, L, 0))
                IF HY > HM THEN HM = HY
              END IF
              IF RP$ = "Y" THEN
                VR = VO(IU, IP, 1)
                DR = VI - VR
                IF ABS(DR) > ABS(RM) THEN RM = DR
              END IF
              IF AMP$ = "N" THEN
                LI = 7
                LF = 2
                FORMAT$ = "&  ###.#&   ####.##   &"
              ELSE
                LI = 6
                LF = 3
                FORMAT$ = "&  ####.#   ###.###   &   ###.##  ###.##"
              END IF
              IF IP = MP(1) THEN
                SP$ = FSTR$(FS, LI, LF)
              ELSE
                SP$ = SPACE$(LI + LF + 1)
              END IF
              M = 1
              IF AMP$ = "Y" THEN
                M = 1000
              END IF
             
              IF RP$ = "N" THEN
               ' PRINT #4, USING FORMAT$; S$; PI; VI; VL; M * DV; SP$
                 PRINT #4, USING FORMAT$; S$; PI; AB$; VI; SP$
              ELSE
                'PRINT #4, USING FORMAT$; S$; PI; VI; VL; M * DV; SP$; VR; M * DR
                 PRINT #4, USING FORMAT$; S$; PI; VI; SP$; VR; M * DR
              END IF
              
            NEXT IP
            IF Reg$ = "N" THEN PRINT #4, USING "&&#####"; S$; "INPUT RES= "; RI(IU, 0);
            IF AMP$ = "N" THEN PRINT #4, USING "&&#####"; S$; "OUTPUT RES="; Ro(IU, 0);
            PRINT #4, USING "&&####.##"; S$; "CMV= "; CMVGTB(IU, 0)
            IF NT > 0 THEN
              FOR IT = HT TO NT
                PRINT #4, ""
                PRINT #4, S$; "TEMPERATURE="; TM(IT);
                IF IT = NT AND RTN <> 0 THEN
                  PP = 1
                ELSE
                  PP = 2
                END IF
                SP = 1
                LP = NP(PP)
                IF IT = NT THEN
                  IF RTN = 1 THEN LP = 1
                  IF RTN = 2 THEN LP = MP(PP): SP = LP - 1
                  IF RTN = 3 THEN SP = MP(PP) - 1
                END IF
                IF LP > 1 THEN PRINT #4, "      NORM FSO";
                PRINT #4, ""
                IF glbbolMeasZeroA THEN
                  PRINT #4, USING "&   ###.#&   ####.##   &"; S$; 0; "A"; glbArrZeroA(IU, IT)
                END IF
               
                FOR IP = 1 TO LP STEP SP
                  PI = PR(IP, PP)
                  LI = 7: LF = 2: IF AMP$ = "Y" THEN LI = 6: LF = 3
                  IF IP = MP(PP) THEN
                    FSO = VO(IU, MP(PP), IT) - VO(IU, 1, IT)
                    '**** FSO FOR TEMPERATURES HERE****
                    SP$ = FSTR$(FSO, LI, LF)
                  ELSE
                    SP$ = SPACE$(LI + LF + 1)
                  END IF
                  VI = VO(IU, IP, IT)
                  PRINT #4, USING "&  ####.#&   ####.##&"; S$; PI; AB$; VI; SP$
                NEXT IP
                IF Reg$ = "N" THEN PRINT #4, USING "&&#####"; S$; "INPUT RES= "; RI(IU, IT);
                IF AMP$ = "N" THEN PRINT #4, USING "&&#####"; S$; "OUTPUT RES="; Ro(IU, IT);
                PRINT #4, ""
              NEXT IT
            END IF
            ' **** CHECK IT OUT!!!!!!!
            IF FS < 1 THEN
              PRINT #4, "": PRINT #4, S$; "BAD UNIT!! - NO OUTPUT @ FSP"
              ELSE
           '   IF LC$ = "Y" THEN
           '     PRINT #4, ""
           '     PRINT #4, S$; "IR LEADS TO CASE   ___________________________"
           '     PRINT #4, ""
           '     PRINT #4, S$; "IR LEADS TO SHIELD ___________________________"
           '   END IF
              PERCENT.FS = 100 / FS
              LM = ABS(PERCENT.FS * LM)
              CM = PERCENT.FS * CM
              HM = PERCENT.FS * HM
              RM = PERCENT.FS * RM
              SB = SQR(LM * LM + HM * HM + RM * RM)
              PRINT #4, ""
              IF AM = 1 THEN PRINT #4, S$; "EPNL="; fnx(LM); "%FSO",
              IF AM = 2 THEN PRINT #4, S$; "BF LIN="; fnx(LM); "%FSO",
              IF HM <> 0 THEN PRINT #4, S$; S$; "MAX HYST="; fnx(HM); "%FSO"
             ' IF CM <> 0 THEN PRINT #4, S$; "CNLH= "; FNX(CM); "%FSO"
              IF RP$ = "Y" THEN PRINT #4, S$; "NON-REPEATABILITY=   "; fnx(RM); "%FSO"
            '  PR$ = LEFT$(PR$, 3)
              IF RTN > 0 THEN
                PRINT #4, S$; "NULL SET="; fnx(VO(IU, 1, NT) - VZ); "MV",
                PRINT #4, S$; S$; "NFSO SET="; fnx(FSO - FS); "MV"
              END IF
              PRINT #4, S$; "BR SENS= "; fnx(SE); "MV/"; PR$ '; " @"; VT; "VOLTS"
              PRINT #4, ""
              ' ADDED 12/4/96
              ' MOD AGAIN 1/28/97
             ' IF UCASE$(PR$) = "PSI" THEN
             '     PRINT #4, S$; "BRIDGE SENSITIVITY=  "; FNX(SE * 14.5); "MV/"; "Bar"; " @"; VT; "VOLTS"
             ' END IF
             ' PRINT #4, S$; "BRIDGE SENSITIVITY=  "; FNX(SE / VT); "MV/V/"; PR$
            '  PRINT #4, S$; "STATIC ERROR BAND=   "; FNX(SB); "%FSO(R.S.S)"
          '     PRINT #4, ""
               ' Print table columns
               
              ' format$ = "&   ###.###   ###.###   ###.###"
           IF NT = 4 THEN
               iNewNT = 3
           ELSE
               iNewNT = NT
           END IF

             IF iNewNT = 3 THEN
               PRINT #4, S$; S$; S$; S$; "C-H"; S$; "C-R"; S$; "R-H"
             ELSE
                PRINT #4, S$; S$; S$; S$; "R-H"
             END IF
               ' *** TCR CALCULATIONS ***
              ' PRINT #4, S$; "TCR(%/100F)"; S$;
               FORMAT$ = "&###.###   ###.###   ###.###"
               IF RI(IU, 0) <> 0# THEN
                    IF iNewNT = 3 THEN
                         glbArrTCR(IU, 1) = ((RI(IU, 2) - RI(IU, 1)) / RI(IU, 0)) / ((TM(2) - TM(1)) / 100#) * 100#
                         glbArrTCR(IU, 2) = ((RI(IU, 0) - RI(IU, 1)) / RI(IU, 0)) / ((TM(0) - TM(1)) / 100#) * 100#
                         glbArrTCR(IU, 3) = ((RI(IU, 2) - RI(IU, 0)) / RI(IU, 0)) / ((TM(2) - TM(0)) / 100#) * 100#
                    ELSE
                         glbArrTCR(IU, 1) = ((RI(IU, 0) - RI(IU, 1)) / RI(IU, 0)) / ((TM(0) - TM(1)) / 100#) * 100#
                    END IF
               END IF
               PRINT #4, S$;
               IF iNewNT = 3 THEN
                    PRINT #4, USING FORMAT$; "TCR(%/100F)       "; fnx(glbArrTCR(IU, 1)); fnx(glbArrTCR(IU, 2)); fnx(glbArrTCR(IU, 3))
               ELSE
                    PRINT #4, USING FORMAT$; "TCR(%/100F)       "; fnx(glbArrTCR(IU, 1))
               END IF
               ' *** END TCR CALCULATIONS
               
               'PRINT #4, S$; "TCGF(%/100F)"; S$;
             IF iNewNT = 3 THEN
               IF FS <> 0 THEN
                    ' Calculate Span shift from cold to high temp
                    FSO = VO(IU, 2, 2) - VO(IU, 1, 2)
                    CFS = VO(IU, 2, 1) - VO(IU, 1, 1)
                    glbArrSpanShift(IU, 1) = ((FSO - CFS) / FS) / ((TM(2) - TM(1)) / 100#) * 100#
                   
                    ' Calculate Span shift from cold to room temp
                    FSO = VO(IU, 2, 1) - VO(IU, 1, 1)
                    glbArrSpanShift(IU, 2) = ((FSO - FS) / FS) / ((TM(1) - TM(0)) / 100#) * 100#

                    ' Calculate Span shift from room to high temp
                    FSO = VO(IU, 2, 2) - VO(IU, 1, 2)
                    glbArrSpanShift(IU, 3) = ((FSO - FS) / FS) / ((TM(2) - TM(0)) / 100#) * 100#

                    ' Calculate Null Shift from Cold to Hot
                    glbArrNullShift(IU, 1) = ((VO(IU, 1, 2) - VO(IU, 1, 1)) / FS) / ((TM(2) - TM(1)) / 100#) * 100#

                    ' Calculate Null Shift from cold to room
                     glbArrNullShift(IU, 2) = ((VO(IU, 1, 0) - VO(IU, 1, 1)) / FS) / ((TM(0) - TM(1)) / 100#) * 100#
                     
                    ' Calculate Null Shift from Room to Hot
                    glbArrNullShift(IU, 3) = ((VO(IU, 1, 2) - VO(IU, 1, 0)) / FS) / ((TM(2) - TM(0)) / 100#) * 100#
               END IF
              
             ELSE
                    ' calculate for R-H only
                       ' Calculate Span shift from Hot to room temp
                       FSO = VO(IU, 2, 1) - VO(IU, 1, 1)
                       glbArrSpanShift(IU, 1) = ((FSO - FS) / FS) / ((TM(1) - TM(0)) / 100#) * 100#
                       ' calc null shift
                       glbArrNullShift(IU, 1) = ((VO(IU, 1, 0) - VO(IU, 1, 1)) / FS) / ((TM(0) - TM(1)) / 100#) * 100#
             END IF

               PRINT #4, S$;
               IF iNewNT = 3 THEN
                    PRINT #4, USING FORMAT$; "TCGF(%/100F)      "; (glbArrSpanShift(IU, 1)); (glbArrSpanShift(IU, 2)); (glbArrSpanShift(IU, 3))
               ELSE
                    PRINT #4, USING FORMAT$; "TCGF(%/100F)      "; (glbArrSpanShift(IU, 1))
               END IF

               PRINT #4, S$;
              
               IF iNewNT = 3 THEN
                    PRINT #4, USING FORMAT$; "NULL SHIFT(%/100F)"; (glbArrNullShift(IU, 1)); (glbArrNullShift(IU, 2)); (glbArrNullShift(IU, 3))
               ELSE
                    PRINT #4, USING FORMAT$; "NULL SHIFT(%/100F)"; (glbArrNullShift(IU, 1))
               END IF

              IF NT > 0 THEN
                LT = NT
                IF RTN > 0 THEN LT = NT - 1
                Z1 = 0
                Z2 = 0
                F1 = 0
                F2 = 0
                FOR IT = HT TO LT
                  SIGN = SGN(TM(IT) - TR)
                  IF ABS(TM(IT) - TR) <> 0 THEN
                        PER.100F = 100 / ABS(TM(IT) - TR)
                  ELSE
                        PER.100F = 1
                  END IF
                  ZT = VO(IU, 1, IT)
                  FT = VO(IU, MP(2), IT) - ZT
                  DEL.Z = SIGN * (ZT - VZ)
                  DEL.FS = SIGN * (FT - FS)
                  D1 = DEL.Z
                  IF ABS(D1) > ABS(Z1) THEN
                    T1 = TM(IT)
                    Z1 = D1
                  END IF
                  D2 = DEL.Z * PER.100F
                  IF ABS(D2) > ABS(Z2) THEN
                    T2 = TM(IT)
                    Z2 = D2
                  END IF
                  D3 = DEL.FS
                  IF ABS(D3) > ABS(F1) THEN
                    F1 = D3
                    T3 = TM(IT)
                  END IF
                  D4 = DEL.FS * PER.100F
                  IF ABS(D4) > ABS(F2) THEN
                    F2 = D4
                    T4 = TM(IT)
                  END IF
                NEXT IT
                Z1 = PERCENT.FS * Z1
                F1 = PERCENT.FS * F1
                Z2 = PERCENT.FS * Z2
                F2 = PERCENT.FS * F2
                TB = SQR(SB * SB + Z1 * Z1 + F1 * F1)
            '    PRINT #4, S$; "MAX NULL DEV   REL TO RT="; FNX(Z1); "%FSO        @ T="; T1
            '    PRINT #4, S$; "MAX NULL SHIFT REL TO RT="; FNX(Z2); "%FSO/100 "; TP$; "  @ T="; T2
            '    PRINT #4, S$; "MAX SPAN DEV   REL TO RT="; FNX(F1); "%FSO        @ T="; T3
             '   PRINT #4, S$; "MAX SPAN SHIFT REL TO RT="; FNX(F2); "%FSO/100 "; TP$; "  @ T="; T4
               ' PRINT #4, S$; "TOTAL ERROR BAND=    "; FNX(TB); "%FSO(R.S.S)"
              END IF
            END IF
          ELSE
            PRINT #4, S$; "BAD UNIT!!!!"
          END IF
          UL$ = "____________________"
          PRINT #4, ""
          PRINT #4, S$; "TESTED BY:   "; TB$
          PRINT #4, ""
          PRINT #4, S$; "CHECKED BY:  "; UL$
'          PRINT #4, ""
'          PRINT #4, S$; "LEAK TEST:   "; UL$
          PRINT #4, NEWPAGE$

'    *** END OF LOOP ***

        NEXT IU
        CLOSE 4
        CLOSE 10
        PRINT


END SUB

SUB VINSELECT

DIM YN$

' **** SELECT INPUT VOLTAGE ***

        CLS
        PRINT
        PRINT "SELECT INPUT VOLTAGE"
        PRINT
        YN$ = "N"
        IV = 99
        DO WHILE YN$ = "N"
          DO
            INPUT "ENTER DESIRED INPUT VOLTAGE"; VT
            FOR I = 0 TO NV
              IF VI(I) = VT THEN
                IV = I
                VE = VT
              END IF
            NEXT I
            PRINT
            IF IV = 99 THEN
                PRINT "You have entered an invalid voltage level!"
                FOR I = 0 TO NV
                   PRINT VI(I)
                NEXT I
            ELSE
                PRINT "VOLTAGE SELECTED="; VI(IV); "VDC"
            END IF
            PRINT
            IF IV <> 99 THEN
                INPUT "IS THIS O.K.(Y/N)"; YN$
            END IF
            PRINT
          LOOP UNTIL IV <> 99
        LOOP
        

' **** LOAD RESISTOR

        IF DATFILOPT% = 1 THEN ' AND MenuOp% > 4
          INPUT "IS THERE A LOAD RESISTOR (Y/N)"; LD$
          IF LD$ = "Y" THEN
            INPUT "WHAT IS THE LOAD RESISTOR VALUE"; LD
          END IF
        END IF
        PRINT

END SUB

SUB VOARRAY (PressNormCoeff#, BSA%, port%, RM%(), M%)

' **** OUTPUT VOLTAGE ARRAY ***

        PRINT
        IF IP = 1 THEN
          PRINT "OUTPUT MEASURED @"; VT; "VDC  P.S.="; fnx(Vs)
        END IF
        IF IP = 1 AND VT <> VE THEN
          PRINT "OUTPUT CORRECTED  TO :"; VE; "VDC"
        END IF

        ' report to screen
        PRINT "  #    OUTPUT"
        FOR IU = 1 TO NU
          VX = MeasVoutNew#(BSA%, port%, RM%(), M%)
          VO(IU, IP, IT) = VX / PressNormCoeff#
          PRINT USING "### ######.###"; IU; VO(IU, IP, IT)
        NEXT IU
END SUB

SUB VOUT (BSA%, port%, RM%(), M%)

DIM YN$

          SP = 1
          LP = NP(PP)
          IF IT = NT THEN
            IF RTN = 1 THEN LP = 1
            IF RTN = 2 THEN LP = MP(PP): SP = LP - 1
            IF RTN = 3 THEN SP = MP(PP) - 1
          END IF
          IF glbbolMeasZeroA THEN CALL MeasZeroA(1, BSA%, port%, RM%(), M%)
         

          FOR IP = 1 TO LP STEP SP
            P = PR(IP, PP)
            IF P = 0 THEN P = P0
            DO
              DO
                LI = 3: LF = 1
                PressNormCoeff# = SetRefPressure#(P, BSA%, port%, RM%(), M%)
                PRINT
                IF RTNUM# <> 0 THEN EXIT DO
                INPUT "READY TO PROCEED(Y/N)"; YN$
              LOOP UNTIL YN$ = "Y"
              CALL VOARRAY(PressNormCoeff#, BSA%, port%, RM%(), M%)
              INPUT "WAS PRESSURE CORRECT(Y/N)"; YN$
            LOOP UNTIL YN$ = "Y"
          NEXT IP
          IF AB$ = "A" AND P0 <> 0 THEN

                '**** CORRECT DATA FOR  NON-ZERO ABSOLUTE ZERO MEASUREMENTS

                X = P0 / (PM - P0)
                QT = IT
                FOR IP = 1 TO NP(PP)
                IF PR(IP, PP) = 0 THEN
                        FOR IU = 1 TO NU
                                V1 = VO(IU, IP, QT)
                                VM = VO(IU, MP(PP), QT)
                                IF IT = NT AND RTN = 1 THEN
                                        V0 = V1 - SE(IU) * X
                                ELSE
                                        IF IT = 0 THEN SE(IU) = VM - V1
                                        V0 = V1 - (VM - V1) * X
                                END IF
                                VO(IU, IP, QT) = V0
                        NEXT IU
                END IF
                NEXT IP
        END IF

END SUB

