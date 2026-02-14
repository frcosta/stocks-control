       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCKS.
       AUTHOR. Fabiano Costa.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STK01 ASSIGN TO "stk01.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-STATUS-STK01.

           SELECT STK02 ASSIGN TO "stk02.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-STATUS-STK02.

       DATA DIVISION.
       FILE SECTION.
       FD STK01.
       01 STK01-REGISTER.
           03 WFS-HB-COST        PIC 9(2)V99.
           03 WFS-DESK-COST      PIC 9(1)V99.

       FD STK02.
       01 STK02-REGISTER.
         03 WFS-DATA-ANO         PIC 9(2).
         03 WFS-DATA-MES         PIC 9(2).
         03 WFS-DATA-DIA         PIC 9(2).


       WORKING-STORAGE SECTION.
       77 WS-CHK-STOCK          PIC X.
       77 WS-STATUS-STK01       PIC X(02).
       77 WS-STATUS-STK02       PIC X(02).
       77 WS-SELECT-OPTION      PIC X.
       77 WS-DRAWLINE PIC X(80) VALUE ALL "-".

       01 CONSTS                PIC 9(1)V99999999.
           78 WS-STOCK-TRF        VALUE 0,00005.
           78 WS-STOCK-LIQ        VALUE 0,00022371.
           78 WS-STOCK-TTA        VALUE 0,00002591.
           78 WS-OPTION-TRF       VALUE 0,00037.
           78 WS-OPTION-LIQ       VALUE 0,00027469.
           78 WS-OPTION-REG       VALUE 0,00070.
           78 WS-ISS-TX           VALUE 0,1061.
           78 WS-PIS-TX           VALUE 0,0065.
           78 WS-COFINS-TX        VALUE 0,004.
           78 WS-OUTROS-TX        VALUE 0,059.
           78 WS-IRRF-DT          VALUE 0,01.
           78 WS-IRRF-ST          VALUE 0,00005.

       01 WS-FLAG               PIC X.
           88 WS-STOCK-SELECT     VALUE 'Y'.
           88 WS-OPTION-SELECT    VALUE 'N'.

       01 WS-DATA.
           05 WS-ANO            PIC 9(2).
           05 WS-MES            PIC 9(2).
           05 WS-DIA            PIC 9(2).
       01 WS-STOCK.
           05 WS-ORDER          PIC X.
           05 WS-TICKER         PIC X(10).
           05 WS-QTY            PIC 9(6).
           05 WS-PRICE          PIC 9(7)V99.
           05 WS-HB             PIC X.
           05 WS-DT             PIC X.
       01 WS-TAXES.
           05 WS-ISS            PIC 9(3)V99.
           05 WS-PIS            PIC 9(3)V99.
           05 WS-COFINS         PIC 9(3)V99.
           05 WS-OUTROS         PIC 9(3)V99.
           05 WS-TOT-TX         PIC 9(3)V99.
       01 WS-CLEARING-EXCHANGE.
           05 WS-NET-OPR        PIC 9(7)V99.
           05 WS-LIQUIDITY      PIC 9(4)V99.
           05 WS-REGISTER       PIC 9(4)V99.
           05 WS-TR-FEE         PIC 9(7)V99.
           05 WS-TTA            PIC 9(7)V99.
       01 WS-OTHERS.
           05 WS-OPERATIONAL    PIC 9(4)V99.
           05 WS-OTHER          PIC 9(4)V99.
           05 WS-IRRF           PIC 9(4)V99.
           05 WS-TOTAL-COSTS    PIC 9(7)V99.
           05 WS-BROKE-COST     PIC 9(4)V99.
           05 WS-HB-COST        PIC 9(2)V99.
           05 WS-DESK-COST      PIC 9(1)V99.

       SCREEN SECTION.
       01 CLEAR-SCREEN BLANK SCREEN.
       01 MENU-PRINCIPAL-SCREEN.
           05 LINE 1       COL  2 VALUE "Stocks Control Center".
           05 LINE PLUS 2  COL  2 VALUE "Main Menu".
           05 LINE PLUS 2  COL  2 VALUE "1. Reg. Operation Record".
           05 LINE PLUS 1  COL  2 VALUE "2. Cancel Registration".
           05 LINE PLUS 1  COL  2 VALUE "3. Asset custody".
           05 LINE PLUS 1  COL  2 VALUE "4. Close month / IR Calc".
           05 LINE PLUS 1  COL  2 VALUE "5. Define initial position".
           05 LINE PLUS 1  COL  2 VALUE "6. Exit".
           05 LINE PLUS 2  COL  2 VALUE "Select your option".
           05              COL PLUS 2 PIC X TO WS-SELECT-OPTION AUTO.
       01 MENU-INPUT-CONFIRM.
           05 LINE 23     COL 1 VALUE "Confirm (Y/N) ?" HIGHLIGHT.
           05 LINE 23     COL PLUS 2 PIC X TO WS-SELECT-OPTION AUTO.
       01 COST-CALC-SCREEN.
           05 LINE 1      COL 1 FROM WS-DRAWLINE LOWLIGHT.
           05 LINE 2      COL 1 VALUE "DATE" HIGHLIGHT.
           05             COL PLUS 2 PIC 99
                                     USING WS-DIA AUTO.
           05             COL PLUS 1 VALUE "/".
           05             COL PLUS 1 PIC 99
                                     USING WS-MES AUTO.
           05             COL PLUS 1 VALUE "/".
           05             COL PLUS 1 PIC 99
                                     USING WS-ANO AUTO.
           05             COL PLUS 2 VALUE "ORDER" HIGHLIGHT.
           05             COL PLUS 2 PIC X
                                     USING WS-ORDER AUTO LOWLIGHT.
           05             COL PLUS 2 VALUE "TICKER" HIGHLIGHT.
           05             COL PLUS 2 PIC X(10)
                                     USING WS-TICKER.
           05             COL PLUS 2 VALUE "QTY" HIGHLIGHT.
           05             COL PLUS 2 PIC ZZZZZZ 
                                     USING WS-QTY.
           05             COL PLUS 2 VALUE "PRICE" HIGHLIGHT.
           05             COL PLUS 2 PIC ZZZZZZZ,ZZ
                                     USING WS-PRICE.
           05             COL PLUS 4 VALUE "HB" HIGHLIGHT.
           05             COL PLUS 2 PIC X
                                     USING WS-HB AUTO.
           05             COL PLUS 2 VALUE "DT" HIGHLIGHT.
           05             COL PLUS 2 PIC X
                                     USING WS-DT AUTO.
           05 LINE PLUS 1 COL 1      FROM WS-DRAWLINE LOWLIGHT.

           05 LINE  PLUS 2 COL  1 VALUE "HB Brokerage Cost".
           05              COL 24 PIC ZZ,ZZ USING WS-HB-COST HIGHLIGHT.
           05 LINE  PLUS 1 COL  1 VALUE "Trading Desk Cost".
           05              COL 25 PIC 9,99 USING WS-DESK-COST HIGHLIGHT.
           05              COL 30 VALUE "%".

           05 LINE  PLUS 2 COL  1 VALUE "CLEARING"
                                  HIGHLIGHT UNDERLINE.
           05              COL 40 VALUE "EXCHANGE" HIGHLIGHT UNDERLINE.
           05 LINE  PLUS 2 COL  1 VALUE "Net Operational.:" LOWLIGHT.
           05              COL 19 PIC Z.ZZZ.ZZ9,99 FROM WS-NET-OPR.
           05              COL 40 VALUE "Transaction Fee.:" LOWLIGHT.
           05              COL 58 PIC Z.ZZZ.ZZ9,99 FROM WS-TR-FEE.
           05 LINE  PLUS 1 COL  1 VALUE "Liquidity Tax...:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-LIQUIDITY.
           05              COL 40 VALUE "TTA.............:" LOWLIGHT.
           05              COL 62 PIC Z.ZZ9,99     FROM WS-TTA.
           05 LINE  PLUS 1 COL  1 VALUE "Register Tax....:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-REGISTER.

           05 LINE  PLUS 2 COL  1 VALUE "OPERATIONAL COSTS" HIGHLIGHT
                                  UNDERLINE.
           05 LINE  PLUS 2 COL  1 VALUE "Operational Tax.:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-BROKE-COST.
           05 LINE  PLUS 1 COL  1 VALUE "Taxes...........:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-TOT-TX.
           05 LINE  PLUS 1 COL  1 VALUE "IRRF............:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-IRRF.
           05 LINE  PLUS 1 COL  1 VALUE "Others..........:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-OUTROS.
           05 LINE  PLUS 2 COL  1 VALUE "Total Costs / Expenses:".

           05 LINE 22      COL 1 FROM WS-DRAWLINE LOWLIGHT.
           05 LINE 24      COL 1 FROM WS-DRAWLINE LOWLIGHT.

       PROCEDURE DIVISION.
       LOAD-DATA.
           OPEN INPUT STK01.
           IF WS-STATUS-STK01 IS EQUAL TO "35"
             PERFORM CREATE-DEFAULT-FILE
             GO TO LOAD-DATA
           END-IF.
           READ STK01.
           MOVE WFS-HB-COST TO WS-HB-COST.
           MOVE WFS-DESK-COST TO WS-DESK-COST.
           CLOSE STK01.

           ACCEPT WS-DATA FROM DATE.
 
       INICIO.
           PERFORM UNTIL WS-SELECT-OPTION = '6' 
              DISPLAY CLEAR-SCREEN
              DISPLAY MENU-PRINCIPAL-SCREEN
              ACCEPT MENU-PRINCIPAL-SCREEN
              EVALUATE WS-SELECT-OPTION
                  WHEN '1'
                      PERFORM REG-OPERATION
                      MOVE SPACE TO WS-SELECT-OPTION
                  WHEN '6'
                      DISPLAY CLEAR-SCREEN
                      GO TO ENDPROGRAM
              END-EVALUATE
           END-PERFORM
           GO TO ENDPROGRAM.
       
       REG-OPERATION.
           PERFORM CLEAR-LOCAL-FIELDS.
           DISPLAY CLEAR-SCREEN.
           DISPLAY COST-CALC-SCREEN.
           ACCEPT COST-CALC-SCREEN.
           PERFORM CALCULA.

      **** Check change for Home Broker Cost or Desk Cost change
      **** and update the default values for both on file
           IF WS-HB-COST NOT = WFS-HB-COST 
               OR WS-DESK-COST NOT = WFS-DESK-COST
               PERFORM UPDATE-BROKE-COST
           END-IF.

           DISPLAY COST-CALC-SCREEN.
           DISPLAY MENU-INPUT-CONFIRM.
           ACCEPT MENU-INPUT-CONFIRM.

           MOVE SPACE TO WS-SELECT-OPTION.
           PERFORM UNTIL WS-SELECT-OPTION = "Y" OR "N"
               DISPLAY MENU-INPUT-CONFIRM
               ACCEPT MENU-INPUT-CONFIRM
           END-PERFORM.
       ENDPROGRAM.
           STOP RUN.

       CALCULA.
      **** Check if ticker represents a stock or an option
           MOVE WS-TICKER(5:1) TO WS-CHK-STOCK.
           IF WS-CHK-STOCK IS NUMERIC
               SET WS-STOCK-SELECT TO TRUE
           ELSE
               SET WS-OPTION-SELECT TO TRUE
           END-IF.

      **** Start calculations
           MULTIPLY WS-QTY BY WS-PRICE GIVING WS-NET-OPR.
           IF WS-STOCK-SELECT
               MOVE ZERO TO WS-REGISTER
               MULTIPLY WS-NET-OPR BY WS-STOCK-TRF GIVING WS-TR-FEE
               MULTIPLY WS-NET-OPR BY WS-STOCK-LIQ GIVING WS-LIQUIDITY
               MULTIPLY WS-NET-OPR BY WS-STOCK-TTA GIVING WS-TTA
           ELSE
               MOVE ZERO TO WS-TTA
               MULTIPLY WS-NET-OPR BY WS-OPTION-TRF GIVING WS-TR-FEE
               MULTIPLY WS-NET-OPR BY WS-OPTION-LIQ GIVING WS-LIQUIDITY
               MULTIPLY WS-NET-OPR BY WS-OPTION-REG GIVING WS-REGISTER
           END-IF.

           IF WS-HB = "Y" OR WS-HB = "y"
               MOVE WS-HB-COST TO WS-BROKE-COST
           ELSE
               COMPUTE WS-BROKE-COST = WS-NET-OPR * WS-DESK-COST / 100
           END-IF.

      **** Calculate taxes
           MULTIPLY WS-BROKE-COST BY WS-ISS-TX    GIVING WS-ISS.
           MULTIPLY WS-BROKE-COST BY WS-PIS-TX    GIVING WS-PIS.
           MULTIPLY WS-BROKE-COST BY WS-COFINS-TX GIVING WS-COFINS.
           MULTIPLY WS-BROKE-COST BY WS-OUTROS-TX GIVING WS-OUTROS.
           COMPUTE WS-TOT-TX = WS-ISS + WS-PIS + WS-COFINS.

      **** Calculate Income Tax (snitch)
           IF WS-ORDER = "S" OR WS-ORDER = "s"
               IF WS-DT = "Y" OR WS-DT = "y"
                   MULTIPLY WS-NET-OPR BY WS-IRRF-DT GIVING WS-IRRF
               ELSE
                   MULTIPLY WS-NET-OPR BY WS-IRRF-ST GIVING WS-IRRF
               END-IF
           END-IF.

           EXIT.

       CREATE-DEFAULT-FILE.
           OPEN OUTPUT STK01.
           MOVE 4,90 TO WFS-HB-COST.
           MOVE 0,50 TO WFS-DESK-COST.
           WRITE STK01-REGISTER.
           CLOSE STK01.
           EXIT.

       UPDATE-BROKE-COST.
           OPEN OUTPUT STK01.
           MOVE WS-HB-COST TO WFS-HB-COST.
           MOVE WS-DESK-COST TO WFS-DESK-COST.
           WRITE STK01-REGISTER.
           CLOSE STK01.
           EXIT.

       CLEAR-LOCAL-FIELDS.
           MOVE SPACES TO WS-ORDER WS-TICKER WS-HB WS-DT.
           MOVE ZEROES TO WS-QTY WS-PRICE.
           MOVE ZEROES TO WS-NET-OPR WS-TR-FEE WS-LIQUIDITY WS-TTA 
                          WS-REGISTER WS-BROKE-COST WS-TOT-TX WS-IRRF
                          WS-OUTROS.
           EXIT.

