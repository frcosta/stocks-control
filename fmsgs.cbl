       Identification Division.
       Program-id. showmsg.

      * Environment Division.

       Data Division.
       Working-Storage Section.
       77 CURSOR-VAL   PIC S9(4) COMP Value 0.
       77 BLKLINE      PIC X(76) VALUE SPACES.
       77 WS-ENTER     PIC X.

       01 WS-ANSWER    PIC X.
           88 WS-VALID VALUE 'S' 's' 'N' 'n'.

       Linkage Section.
       01 LK-Msg      PIC X(76).
       01 LK-Type     PIC X.
       01 LK-Delay    PIC 9V9999.
       01 LK-Ret      PIC X.

       Procedure Division Using LK-Msg, LK-Type, LK-Delay, LK-Ret.
       Init00.
           Display BLKLINE AT 2305.

           Evaluate LK-Type 
               When 'S' 
                  Display LK-Msg AT 2305 WITH FOREGROUND-COLOR 3
                                              HIGHLIGHT
               When 'A' 
                  Display LK-Msg AT 2305 WITH FOREGROUND-COLOR 4
                                              HIGHLIGHT BLINK
               When 'Q'
                  Display "[ ]"  AT 2305 WITH FOREGROUND-COLOR 3
                  Display LK-Msg AT 2309 WITH FOREGROUND-COLOR 3
                                              HIGHLIGHT       
                  Accept WS-ENTER AT 2306 WITH AUTO
                  Move Function UPPER-CASE(WS-ENTER) to LK-ret
                  Display BLKLINE AT 2305
                  Goback
               When 'V'
                  Display "[ ]"  AT 2305 WITH FOREGROUND-COLOR 3
                  Display LK-Msg AT 2309 WITH FOREGROUND-COLOR 3
                                              HIGHLIGHT       
                  ACCEPT WS-ENTER AT 2306 WITH AUTO
                  Display BLKLINE AT 2305
                  Goback
               When 'Y'
                  Display "[ ]"  AT 2305 WITH FOREGROUND-COLOR 3
                  Display LK-Msg AT 2309 WITH FOREGROUND-COLOR 3
                  HIGHLIGHT       
                  Perform with test after until WS-VALID 
                     Accept WS-ANSWER AT 2306 WITH AUTO
                 End-Perform
                 Move Function UPPER-CASE(WS-ANSWER) to LK-ret
                 Display BLKLINE AT 2305
                 Goback
           End-Evaluate.

           Move 0 to CURSOR-VAL.
           CALL "curs_set" Using by Value CURSOR-VAL.
           CALL "C$SLEEP" Using LK-Delay End-Call.
           Move 1 to CURSOR-VAL.
           CALL "curs_set" Using by Value CURSOR-VAL.
           Display BLKLINE AT 2305.
           Goback. 
