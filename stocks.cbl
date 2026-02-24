       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCKS.
       AUTHOR. Fabiano Costa.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY 'control_stk01'.
           COPY 'control_custody'.
           COPY 'control_register'.

       DATA DIVISION.
       FILE SECTION.
       FD STK01.
           COPY 'stk01'.
       FD STK02.
           COPY 'custody'.
       FD STK03.
           COPY 'register'.

       WORKING-STORAGE SECTION.
       77 WS-LN                 PIC 9(02).
       77 WS-CHAVE-PRIMARIA     PIC X(14).
       77 WS-CHK-STOCK          PIC X.
       77 WS-STATUS-STK01       PIC X(02).
       77 WS-STATUS-STK02       PIC X(02).
       77 WS-STATUS-STK03       PIC X(02).
       77 WS-STATUS-STK04       PIC X(02).
       77 WS-SELECT-OPTION      PIC X.
       77 WS-SYSTEM-TIME        PIC 9(08).
       77 WS-DRAWLINE PIC X(80) VALUE ALL "-".

       01 CONSTS                PIC 9(1)V99999999.
           78 WS-STOCK-TRF        VALUE 0,00005.
           78 WS-STOCK-LIQ        VALUE 0,00022371.
           78 WS-STOCK-TTA        VALUE 0,00002591.
           78 WS-OPTION-TRF       VALUE 0,00037.
           78 WS-OPTION-LIQ       VALUE 0,00027469.
           78 WS-OPTION-REG       VALUE 0,00070.
           78 WS-ISS-TX           VALUE 0,05.
           78 WS-PIS-TX           VALUE 0,0065.
           78 WS-COFINS-TX        VALUE 0,04.
           78 WS-OUTROS-TX        VALUE 0,059.
           78 WS-IRRF-DT          VALUE 0,01.
           78 WS-IRRF-ST          VALUE 0,00005.

       01 STK02-REGISTER-LOCAL.
           03 WS-STK02-TICKER           PIC X(10).
           03 WS-STK02-QTY              PIC 9(06).
           03 WS-STK02-PMA              PIC 9(04)V99.
           03 WS-STK02-BALANCE          PIC 9(07)V99.
           03 WS-STK02-TOT-BALANCE      PIC 9(08)V99.
           03 WS-STK02-TOT-BALANCE-MASK PIC Z.ZZZ.ZZ9,99.

       01 WS-FLAG               PIC X.
           88 WS-STOCK-SELECT     VALUE 'Y'.
           88 WS-OPTION-SELECT    VALUE 'N'.

       01 WS-DATA.
           05 WS-ANO            PIC 9(2).
           05 WS-MES            PIC 9(2).
           05 WS-DIA            PIC 9(2).
       01 WS-TIME.
           05 WS-HORA           PIC 9(2).
           05 WS-MINUTO         PIC 9(2).
           05 WS-SEGUNDO        PIC 9(2).
       01 WS-STOCK.
           05 WS-ORDER          PIC X.
           05 WS-TICKER         PIC X(10).
           05 WS-QTY            PIC 9(6).
           05 WS-PRICE          PIC 9(7)V99.
           05 WS-HB             PIC X.
           05 WS-DT             PIC X.
           05 WS-PM             PIC 9(7)V99.
           05 WS-ALLOC          PIC 9(7)V99.
       01 WS-STOCK-MASK.
           05 WS-QTY-MASK       PIC ZZZ.ZZZ.
           05 WS-PMA-MASK       PIC Z.ZZ9,99.
           05 WS-BALANCE-MASK   PIC Z.ZZZ.ZZ9,99.
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
           05 WS-KEY            PIC 9(12).
           05 WS-OPERATIONAL    PIC 9(4)V99.
           05 WS-AMOUNT         PIC 9(6).
           05 WS-STK02-AMOUNT   PIC 9(6).
           05 WS-OTHER          PIC 9(4)V99.
           05 WS-IRRF           PIC 9(4)V99.
           05 WS-TOTAL-COSTS    PIC 9(7)V99.
           05 WS-BROKE-COST     PIC 9(4)V99.
           05 WS-HB-COST        PIC 9(2)V99.
           05 WS-DESK-COST      PIC 9(1)V99.
           05 WS-NET            PIC 9(7)V99.
           

       SCREEN SECTION.
       01 CLEAR-SCREEN BLANK SCREEN.
       01 MENU-PRINCIPAL-SCREEN.
           05 LINE 1       COL  2 VALUE "Stocks Control Center".
           05 LINE PLUS 2  COL  2 VALUE "Main Menu".
           05 LINE PLUS 2  COL  2 VALUE "1. Registration of Custody".
           05 LINE PLUS 1  COL  2 VALUE "2. BUY/SELL Operation".
           05 LINE PLUS 1  COL  2 VALUE "3. Asset custody".
           05 LINE PLUS 1  COL  2 VALUE "4. Sort registers".
           05 LINE PLUS 1  COL  2 VALUE "5. Define initial position".
           05 LINE PLUS 1  COL  2 VALUE "6. Exit".
           05 LINE PLUS 2  COL  2 VALUE "Select your option".
           05              COL PLUS 2 PIC X TO WS-SELECT-OPTION AUTO.
       01 MENU-INPUT-CONFIRM.
           05 LINE 23    COL 1 VALUE "Do you confirm this opp (Y/N) ?"
                               HIGHLIGHT.
           05 LINE 23    COL PLUS 2 PIC X TO WS-SELECT-OPTION.
       01 LIST-CUSTODY.
           05 LINE 1   COL 1 FROM WS-DRAWLINE LOWLIGHT.
           05 LINE 1   COL 1 VALUE "Custody Report  ".
           05 LINE 3   COL 1 VALUE "TICKER" HIGHLIGHT UNDERLINE.
           05 LINE 3   COL 17 VALUE "QTY" HIGHLIGHT UNDERLINE.
           05 LINE 3   COL 28 VALUE "PMA" HIGHLIGHT UNDERLINE.
           05 LINE 3   COL 42 VALUE "BALANCE" HIGHLIGHT UNDERLINE.
           05 LINE 22  COL 1 FROM WS-DRAWLINE LOWLIGHT.
           05 LINE 24  COL 1 FROM WS-DRAWLINE LOWLIGHT.
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
           05             COL PLUS 2 PIC ZZZZZZ9,99
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
           05 LINE  PLUS 1 COL  1 VALUE "ISS/PIS/COFINS..:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-TOT-TX.
           05 LINE  PLUS 1 COL  1 VALUE "IRRF............:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-IRRF.
           05 LINE  PLUS 1 COL  1 VALUE "Others..........:" LOWLIGHT.
           05              COL 23 PIC Z.ZZ9,99     FROM WS-OUTROS.

           05 LINE  PLUS 2 COL  1      VALUE "Total Cost......:".
           05              COL PLUS 2  PIC Z.ZZZ.ZZ9,99 FROM
                                                        WS-TOTAL-COSTS
                                       FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 1.

           05              COL 38      VALUE "Net:".
           05              COL PLUS 2  PIC Z.ZZZ.ZZ9,99 FROM
                                                        WS-NET
                                       FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 1.
                                                        
           05              COL PLUS 6  VALUE "A.Price:".
           05              COL PLUS 2  PIC Z.ZZZ.ZZ9,99 FROM
                                                        WS-PM
                                       FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 1.

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
                  WHEN '2'
                      PERFORM BUYSELL-REG
                      MOVE SPACE TO WS-SELECT-OPTION
                  WHEN '3'
                      PERFORM LST-CUSTODY
                      MOVE SPACE TO WS-SELECT-OPTION
                  WHEN '4'
                      CALL 'SORTREG'
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
           MOVE SPACE TO WS-SELECT-OPTION.
           ACCEPT MENU-INPUT-CONFIRM.
           IF WS-SELECT-OPTION = "Y" OR WS-SELECT-OPTION = "y"
              PERFORM UPD-CUSTODY 
           END-IF.

       BUYSELL-REG.
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
           MOVE SPACE TO WS-SELECT-OPTION.
           ACCEPT MENU-INPUT-CONFIRM.
           IF WS-SELECT-OPTION = "Y" OR WS-SELECT-OPTION = "y"
              PERFORM UPD-REGISTER 
           END-IF.


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
           COMPUTE WS-OUTROS = (WS-BROKE-COST + WS-TR-FEE +
                               WS-LIQUIDITY) * WS-OUTROS-TX.
           COMPUTE WS-TOT-TX = WS-ISS + WS-PIS + WS-COFINS.

           COMPUTE WS-TOTAL-COSTS = WS-LIQUIDITY + WS-REGISTER
                                    + WS-TR-FEE + WS-TTA
                                    + WS-BROKE-COST + WS-TOT-TX
                                    + WS-OUTROS.


      **** Calculate Income Tax (snitch)
           IF WS-ORDER = "S" OR WS-ORDER = "s"
               IF WS-DT = "Y" OR WS-DT = "y"
                   MULTIPLY WS-NET-OPR BY WS-IRRF-DT GIVING WS-IRRF
               ELSE
                   MULTIPLY WS-NET-OPR BY WS-IRRF-ST GIVING WS-IRRF
               END-IF
               COMPUTE WS-NET = WS-NET-OPR - WS-TOTAL-COSTS
           ELSE
               COMPUTE WS-NET = WS-NET-OPR + WS-TOTAL-COSTS
           END-IF.

      **** Calculate average price
           DIVIDE WS-NET BY WS-QTY GIVING WS-PM.

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
                          WS-OUTROS WS-TOTAL-COSTS.
           EXIT.

       UPD-CUSTODY.
           OPEN I-O STK02.
           IF WS-STATUS-STK02 EQUAL TO "35"
               CLOSE STK02
               OPEN OUTPUT STK02
               CLOSE STK02
               OPEN I-O STK02
           END-IF.
           MOVE WS-TICKER TO WFS-STK02-TICKER.
           READ STK02 KEY IS WFS-STK02-TICKER
           EVALUATE WS-STATUS-STK02
               WHEN "23"
                  MOVE WS-QTY    TO WFS-STK02-QTY
                  MOVE WS-PM     TO WFS-STK02-PMA
                  MOVE WS-NET    TO WFS-STK02-BALANCE
                  WRITE STK02-REGISTER   
               WHEN "00"
                  MOVE WFS-STK02-QTY     TO WS-STK02-QTY
                  MOVE WFS-STK02-PMA     TO WS-STK02-PMA
                  MOVE WFS-STK02-BALANCE TO WS-STK02-BALANCE

                  IF WS-ORDER = "B" OR WS-ORDER = "b"
                     ADD WS-QTY TO WS-STK02-QTY
                     ADD WS-NET TO WS-STK02-BALANCE
                     MOVE WS-STK02-BALANCE TO WFS-STK02-BALANCE
                     MOVE WS-STK02-QTY TO WFS-STK02-QTY
                     DIVIDE WS-STK02-BALANCE BY WS-STK02-QTY GIVING
                                                WFS-STK02-PMA
                  ELSE
                     SUBTRACT WS-QTY FROM WS-STK02-QTY GIVING
                                                WFS-STK02-QTY
                  END-IF
                  REWRITE STK02-REGISTER
           END-EVALUATE.
           CLOSE STK02.

       LST-CUSTODY.
           DISPLAY CLEAR-SCREEN.
           DISPLAY LIST-CUSTODY.
           MOVE 5 TO WS-LN.
           MOVE 0 TO WS-STK02-TOT-BALANCE.
           OPEN INPUT STK02.
           MOVE "00" TO WS-STATUS-STK02.
           PERFORM UNTIL WS-STATUS-STK02 = "10"
              READ STK02 NEXT RECORD
                AT END
                  MOVE "10" TO WS-STATUS-STK02
                NOT AT END
                  MOVE WFS-STK02-TICKER  TO WS-TICKER
                  MOVE WFS-STK02-QTY     TO WS-QTY-MASK
                  MOVE WFS-STK02-PMA     TO WS-PMA-MASK
                  MOVE WFS-STK02-BALANCE TO WS-BALANCE-MASK
                  ADD WFS-STK02-BALANCE TO WS-STK02-TOT-BALANCE
                  DISPLAY WS-TICKER   AT LINE WS-LN COLUMN 01
                  DISPLAY WS-QTY-MASK AT LINE WS-LN COLUMN 13
                  DISPLAY WS-PMA-MASK AT LINE WS-LN COLUMN 23
                  DISPLAY WS-BALANCE-MASK  AT LINE WS-LN COLUMN 37
                  ADD 1 TO WS-LN
               END-READ
           END-PERFORM.
           CLOSE STK02.
           ADD 1 TO WS-LN.
           MOVE WS-STK02-TOT-BALANCE TO WS-STK02-TOT-BALANCE-MASK.
           DISPLAY "Total Balance" AT LINE WS-LN COLUMN 1.
           DISPLAY WS-STK02-TOT-BALANCE-MASK   AT LINE WS-LN COLUMN 37.
           ACCEPT WS-SELECT-OPTION AT LINE 23 COLUMN 80.
           EXIT.

       UPD-REGISTER.

           ACCEPT WS-TIME FROM TIME.
           STRING 
              WS-ANO
              WS-MES
              WS-DIA
              WS-HORA
              WS-MINUTO
              WS-SEGUNDO
              DELIMITED BY SIZE
              INTO WS-KEY
           END-STRING.

           OPEN EXTEND STK03.
           IF WS-STATUS-STK03 = "35"
               OPEN OUTPUT STK03
           END-IF.

           MOVE WS-KEY         TO STK03-KEY
           MOVE WS-ORDER       TO STK03-ORDER.
           MOVE WS-TICKER      TO STK03-TICKER.
           MOVE WS-QTY         TO STK03-QTY.
           MOVE WS-PRICE       TO STK03-PRICE.
           MOVE WS-IRRF        TO STK03-IRRF.
           MOVE WS-TOTAL-COSTS TO STK03-COST.
           MOVE WS-NET         TO STK03-NET.
           MOVE WS-PM          TO STK03-AVPRICE.

           WRITE STK03-REGISTER.
           CLOSE STK03.
           EXIT.

