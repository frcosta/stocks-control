       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCKS.
       AUTHOR. Fabiano Costa.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-CHK-STOCK          PIC X.

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
           05 WS-HB-COST        PIC 9(1)V99  VALUE 4,90.
           05 WS-DESK-COST      PIC 9(1)V99  VALUE 0,50.

       SCREEN SECTION.
       01 CLEAR-SCREEN BLANK SCREEN.
       01 MAIN-SCREEN.
           05 LINE 1 COL 24 VALUE "=== STOCK COST CALCULATION ==="
               HIGHLIGHT.
           05 LINE  3 COL  1 VALUE "ORDER".
           05 LINE  3 COL  7 PIC X TO WS-ORDER AUTO HIGHLIGHT.
           05 LINE  3 COL  9 VALUE "TICKER".
           05 LINE  3 COL 16 PIC X(10) TO WS-TICKER HIGHLIGHT.
           05 LINE  3 COL 27 VALUE "QTY".
           05 LINE  3 COL 31 PIC ZZZZZZ TO WS-QTY HIGHLIGHT.
           05 LINE  3 COL 38 VALUE "PRICE".
           05 LINE  3 COL 44 PIC ZZZZZZ9,99 TO WS-PRICE HIGHLIGHT.
           05 LINE  3 COL 55 VALUE "HB".
           05 LINE  3 COL 58 PIC X TO WS-HB AUTO HIGHLIGHT.
           05 LINE  3 COL 60 VALUE "DT".
           05 LINE  3 COL 63 PIC X TO WS-DT AUTO HIGHLIGHT.

           05 LINE  5 COL  1 VALUE "HB Brokerage Cost".
           05 LINE  5 COL 23 PIC ZZ9,99 USING WS-HB-COST HIGHLIGHT.
           05 LINE  6 COL  1 VALUE "Trading Desk Cost".
           05 LINE  6 COL 25 PIC 9,99 USING WS-DESK-COST HIGHLIGHT.
           05 LINE  6 COL 30 VALUE "%".

           05 LINE  8 COL  1 VALUE "CLEARING" HIGHLIGHT.
           05 LINE  8 COL 40 VALUE "EXCHANGE" HIGHLIGHT.
           05 LINE 10 COL  1 VALUE "Net Operational.:".
           05 LINE 10 COL 40 VALUE "Transaction Fee.:".
           05 LINE 11 COL 40 VALUE "TTA.............:".

           05 LINE 11 COL  1 VALUE "Liquidity Tax...:".
           05 LINE 12 COL  1 VALUE "Register Tax....:".
           05 LINE 14 COL  1 VALUE "OPERATIONAL COSTS" HIGHLIGHT.
           05 LINE 16 COL  1 VALUE "Operational Tax.:".
           05 LINE 17 COL  1 VALUE "Taxes...........:".
           05 LINE 18 COL  1 VALUE "IRRF............:".
           05 LINE 19 COL  1 VALUE "Others..........:".
           05 LINE 21 COL  1 VALUE "Total Costs / Expenses:".


           05 LINE 10 COL 19 PIC Z.ZZZ.ZZ9,99 FROM WS-NET-OPR.
           05 LINE 10 COL 58 PIC Z.ZZZ.ZZ9,99 FROM WS-TR-FEE.
           05 LINE 11 COL 23 PIC Z.ZZ9,99     FROM WS-LIQUIDITY.
           05 LINE 11 COL 62 PIC Z.ZZ9,99     FROM WS-TTA.
           05 LINE 12 COL 23 PIC Z.ZZ9,99     FROM WS-REGISTER.
           05 LINE 16 COL 23 PIC Z.ZZ9,99     FROM WS-BROKE-COST.
           05 LINE 17 COL 23 PIC Z.ZZ9,99     FROM WS-TOT-TX.
           05 LINE 18 COL 23 PIC Z.ZZ9,99     FROM WS-IRRF.
           05 LINE 19 COL 23 PIC Z.ZZ9,99     FROM WS-OUTROS.

       PROCEDURE DIVISION.

       INICIO.
           DISPLAY CLEAR-SCREEN.
           DISPLAY MAIN-SCREEN.
           ACCEPT MAIN-SCREEN.
           PERFORM CALCULA.
           DISPLAY MAIN-SCREEN.
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

