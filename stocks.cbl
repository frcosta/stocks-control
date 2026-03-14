       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCKS.
       AUTHOR. Fabiano Costa.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
           CRT STATUS IS WS-CRT-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY 'control_stk01'.
           COPY 'control_custody'.
           COPY 'control_register'.
           COPY 'control_stk04'.

       DATA DIVISION.
       FILE SECTION.
       FD STK01.
           COPY 'stk01'.
       FD STK02.
           COPY 'custody'.
       FD STK03.
           COPY 'register'.
       FD STK04.
           COPY 'stk04'.

       WORKING-STORAGE SECTION.
       77 CURSOR-VAL            PIC S9(4) COMP VALUE 0.
       77 WS-CRT-STATUS         PIC 9(04).
       77 WS-DELAY              PIC 9V9999 VALUE 2,0. *> 2,0 seg
       77 WS-VALIDADO           PIC 9(02) VALUE 0.
       77 WS-MSG                PIC X(76)  VALUE SPACES.
       77 WS-LN                 PIC 9(02).
       77 WS-POS-ARRAY          PIC 9(3).
       77 WS-CHAVE-PRIMARIA     PIC X(14).
       77 WS-CHK-STOCK          PIC X.
       77 WS-STATUS-STK01       PIC X(02).
       77 WS-STATUS-STK02       PIC X(02).
       77 WS-STATUS-STK03       PIC X(02).
       77 WS-STATUS-STK04       PIC X(02).
       77 WS-SELECT-OPTION      PIC X.
       77 WS-SYSTEM-TIME        PIC 9(08).
       77 WS-DRAWLINE           PIC X(80) VALUE ALL "_".
       77 WS-BLANK              PIC X(76) VALUE ALL " ".
       
       01 WS-QUESTION           PIC X.
           88 WS-VALID-QUESTION VALUE 'S' 's' 'N' 'n'.



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
           03 WS-STK02-TICKER           PIC X(10)    OCCURS 100 TIMES.
           03 WS-STK02-QTY              PIC 9(06)    OCCURS 100 TIMES.
           03 WS-STK02-PRICE            PIC 9(04)V99 OCCURS 100 TIMES.
           03 WS-STK02-BALANCE          PIC 9(07)V99 OCCURS 100 TIMES.
           03 WS-STK02-TOT-BALANCE      PIC 9(08)V99.
           03 WS-STK02-TOT-BALANCE-MASK PIC Z.ZZZ.ZZ9,99.

       01 WS-FLAG               PIC X.
           88 WS-STOCK-SELECT     VALUE 'Y'.
           88 WS-OPTION-SELECT    VALUE 'N'.

       01 WS-DATA.
           05 WS-ANO            PIC 9(2).
           05 WS-MES            PIC 9(2).
           05 WS-DIA            PIC 9(2).
       01 WS-DADOS-INICIAIS.
           05 WS-ANO-INICIAL    PIC 9(2).
           05 WS-MES-INICIAL    PIC 9(2).
           05 WS-DIA-INICIAL    PIC 9(2).
           05 WS-PREJ-COM-INI   PIC 9(6)V99.
           05 WS-PREJ-DT-INI    PIC 9(6)V99.
           05 WS-PREJ-FII-INI   PIC 9(6)V99.
           05 WS-IRRF-COM-INI   PIC 9(6)V99.
           05 WS-IRRF-DT-INI    PIC 9(6)V99.
           05 WS-IRRF-FII-INI   PIC 9(6)V99.
       01 WS-TIME.
           05 WS-HORA           PIC 9(2).
           05 WS-MINUTO         PIC 9(2).
           05 WS-SEGUNDO        PIC 9(2).

       01 WS-ORDER              PIC X.
           88 WS-VALID-ORDER VALUE 'C', 'c', 'V', 'v'.
       01 WS-HB                 PIC X.
           88 WS-VALID-HB VALUE 'S', 's', 'N', 'n'.
       01 WS-DT                 PIC X.
           88 WS-VALID-DT VALUE 'S', 's', 'N', 'n'.

       01 WS-STOCK.
           05 WS-TICKER         PIC X(10).
           05 WS-QTY            PIC 9(6).
           05 WS-PRICE          PIC 9(7)V99.
           05 WS-PM             PIC 9(7)V99.
           05 WS-ALLOC          PIC 9(7)V99.
           05 WS-BALANCE        PIC 9(7)V99.
       01 WS-STOCK-MASK.
           05 WS-QTY-MASK       PIC ZZZ.ZZZ.
           05 WS-PMA-MASK       PIC Z.ZZ9,99.
           05 WS-PRICE-MASK     PIC Z.ZZZ.ZZ9,99.
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

       01 MESSAGE-SCREEN.
           05 LINE 23 COL 5 FROM WS-MSG BLINK FOREGROUND-COLOR 4.
       01 MESSAGE-CLEAR.
           05 LINE 23 COL 5 BLANK LINE.

       01 PRT-MSG.
           05 LINE 23 COL 5 PIC X(76) FROM WS-MSG FOREGROUND-COLOR 3.
 
       01 QUESTION.
           05 LINE 23 COL 5 VALUE "[ ] ".
           05 LINE 23 COL 9 PIC X(40) FROM WS-MSG FOREGROUND-COLOR 3
                                      HIGHLIGHT.
           05 LINE 23 COL 6 PIC X TO WS-QUESTION HIGHLIGHT.

       01 MENU-PRINCIPAL2-SCREEN.
          05 LINE 1  COL 5 PIC X(76) FROM WS-BLANK HIGHLIGHT UNDERLINE. 
          05 LINE 1  COL 60 VALUE "CONTROLE DE PORTFOLIO" UNDERLINE
                                                          HIGHLIGHT. 
          05 LINE 2  COL 5 VALUE "Menu Principal" HIGHLIGHT.

          05 LINE 4  COL 5 VALUE "Configuracoes Iniciais" UNDERLINE 
                                  FOREGROUND-COLOR 1 HIGHLIGHT.
          05 LINE 5  COL 5 VALUE "a) Definir custodia inicial".
          05 LINE 5  COL 45 VALUE "d) Exibir custodia inicial".

          05 LINE 6  COL 5 VALUE "b) Definir prejuizos acumulados".
          05 LINE 6  COL 45 VALUE "e) Exibir prejuizos acumulados".
          05 LINE 7  COL 5 VALUE "c) Definir data inicial".
          05 LINE 7  COL 45 VALUE "c) Redefinir configuracoes iniciais".
          05 LINE 9  COL 5 VALUE "Lancamentos" UNDERLINE
                                  FOREGROUND-COLOR 1 HIGHLIGHT.
          05 LINE 10 COL 5 VALUE "1.Lancar ordens de compra e venda".
          05 LINE 11 COL 5 VALUE "2.Listar ordens".
          05 LINE 12 COL 5 VALUE "3.Excluir ordem".
          05 LINE 14 COL 5 VALUE "Imposto de renda" UNDERLINE
                                  FOREGROUND-COLOR 1 HIGHLIGHT.

          05 LINE 15 COL 5 VALUE "4.Fechar mes".
          05 LINE 16 COL 5 VALUE "5.Acusar pagamento do imposto".
          05 LINE 18 COL 5 VALUE "Area de Trabalho" UNDERLINE
                                  FOREGROUND-COLOR 1 HIGHLIGHT.
          05 LINE 19 COL 5 VALUE "6.Iniciar novo ano fiscal".
          05 LINE 20 COL 5 VALUE "7.Fechar ano fiscal".
          05 LINE 21 COL 5 VALUE "8.Encerrar sistema".
          05 LINE 23 COL 5 VALUE "Selecione opcao"
                                  FOREGROUND-COLOR 3 HIGHLIGHT.
          05 LINE 23 COL 21 PIC X TO WS-SELECT-OPTION AUTO.
          05 LINE 24 COL 5 PIC X(76) FROM WS-BLANK UNDERLINE. 
 
       01 MENU-INPUT-CONFIRM.
           05 LINE 23    COL 5 VALUE "Confirma lancamento (S/N) ?"
                                  FOREGROUND-COLOR 3 HIGHLIGHT.
           05 LINE 23    COL PLUS 2 PIC X TO WS-QUESTION.
       01 LIST-CUSTODY.
           05 LINE 1  COL 1 FROM WS-DRAWLINE LOWLIGHT.
           05 LINE 1  COL 1 VALUE "Custody Report  ".
           05 LINE 3  COL 1 VALUE "TICKER" HIGHLIGHT UNDERLINE.
           05 LINE 3  COL 17 VALUE "QTY" HIGHLIGHT UNDERLINE.
           05 LINE 3  COL 28 VALUE "PMA" HIGHLIGHT UNDERLINE.
           05 LINE 3  COL 42 VALUE "BALANCE" HIGHLIGHT UNDERLINE.
           05 LINE 22 COL 1 FROM WS-DRAWLINE LOWLIGHT.
           05 LINE 24 COL 1 FROM WS-DRAWLINE LOWLIGHT.

       01 DEF-DADOS-INICIAIS-SCR.
           05 LINE 1  COL 5 PIC X(76) FROM WS-BLANK HIGHLIGHT UNDERLINE. 
           05 LINE 1  COL 61 VALUE "CONFIGURACAO INICIAL"
                                   HIGHLIGHT UNDERLINE. 
           05 LINE 2 COL 5 VALUE "DATA INICIAL" HIGHLIGHT.
           05        COL PLUS 2 PIC 99 USING WS-DIA-INICIAL
                                       AUTO.
           05        COL PLUS 1 VALUE "/".
           05        COL PLUS 1 PIC 99 USING WS-MES-INICIAL
                                       AUTO.
           05        COL PLUS 1 VALUE "/".
           05        COL PLUS 1 PIC 99 USING WS-ANO-INICIAL
                                       AUTO.
           05 LINE 4 COL 05 VALUE "OPERACOES COMUNS  " HIGHLIGHT
                                                      UNDERLINE.
           05        COL 35 VALUE "DAY TRADES        " HIGHLIGHT
                                                      UNDERLINE.
           05        COL 63 VALUE "FIIs              " HIGHLIGHT
                                                      UNDERLINE.

           05 LINE 5 COL 5 VALUE "Prejuizo".
           05        COL PLUS 2 PIC ZZZZZZ,ZZ USING WS-PREJ-COM-INI.
           05        COL 35 VALUE "Prejuizo".
           05        COL PLUS 2 PIC ZZZZZZ,ZZ USING WS-PREJ-DT-INI.
           05        COL 63 VALUE "Prejuizo".
           05        COL PLUS 2 PIC ZZZZZZ,ZZ USING WS-PREJ-FII-INI.

           05 LINE 6 COL 5 VALUE "IRRF".
           05        COL PLUS 7 PIC ZZZZZ,ZZ USING WS-IRRF-COM-INI.
           05        COL 35 VALUE "IRRF".
           05        COL PLUS 7 PIC ZZZZZ,ZZ USING WS-IRRF-DT-INI.
           05        COL 63 VALUE "IRRF".
           05        COL PLUS 7 PIC ZZZZZ,ZZ USING WS-IRRF-FII-INI.
           05 LINE 7 COL 5 PIC X(76) FROM WS-BLANK HIGHLIGHT UNDERLINE. 
           05 LINE 24 COL  5 PIC X(76) FROM WS-BLANK UNDERLINE. 
           
       01 DEF-CUSTODIA-INICIAL-SCR.
           05 LINE 9 COL 5 VALUE "PREGAO" HIGHLIGHT.
           05        COL PLUS 2 PIC 99 USING WS-DIA AUTO.
           05        COL PLUS 1 VALUE "/".
           05        COL PLUS 1 PIC 99 USING WS-MES AUTO.
           05        COL PLUS 1 VALUE "/".
           05        COL PLUS 1 PIC 99 USING WS-ANO AUTO.
           05        COL PLUS 2 VALUE "ATIVO" HIGHLIGHT.
           05        COL PLUS 2 PIC X(10) USING WS-TICKER.
           05        COL PLUS 2 VALUE "QTD" HIGHLIGHT.
           05        COL PLUS 2 PIC ZZZZZZ USING WS-QTY REQUIRED.
           05        COL PLUS 2 VALUE "PM" HIGHLIGHT.
           05        COL PLUS 2 PIC ZZZZZZZ,ZZ USING WS-PRICE
                                                REQUIRED.
           05        COL PLUS 2 VALUE "TOTAL" HIGHLIGHT.
           05        COL PLUS 2  PIC Z.ZZZ.ZZ9,99 FROM WS-BALANCE
                                                  REVERSE-VIDEO.
           05 LINE 11 COL  5 VALUE "SEQ" HIGHLIGHT UNDERLINE. 
           05 LINE 11 COL 15 VALUE "ATIVO" HIGHLIGHT UNDERLINE. 
           05 LINE 11 COL 30 VALUE "QUANTIDADE" HIGHLIGHT UNDERLINE. 
           05 LINE 11 COL 50 VALUE "PRECO MEDIO" HIGHLIGHT UNDERLINE. 
           05 LINE 11 COL 74 VALUE "POSICAO" HIGHLIGHT UNDERLINE. 
           05 LINE 24 COL  5 PIC X(76) FROM WS-BLANK UNDERLINE. 

       01 COST-CALC-SCREEN.
           05 LINE 1  COL 5 PIC X(76) FROM WS-BLANK HIGHLIGHT UNDERLINE. 
           05 LINE 1  COL 65 VALUE "CUSTODIA INICIAL"
                                   HIGHLIGHT UNDERLINE. 
           05 LINE 3  COL 5 PIC 99 USING WS-DIA AUTO.
           05         COL PLUS 1 VALUE "/".
           05         COL PLUS 1 PIC 99 USING WS-MES AUTO.
           05         COL PLUS 1 VALUE "/".
           05         COL PLUS 1 PIC 99 USING WS-ANO AUTO.
           05         COL PLUS 2 VALUE "ORDEM" HIGHLIGHT.
           05         COL PLUS 2 PIC X USING WS-ORDER AUTO REQUIRED.
           05         COL PLUS 2 VALUE "TICKER" HIGHLIGHT.
           05         COL PLUS 2 PIC X(10) USING WS-TICKER.
           05         COL PLUS 2 VALUE "QTD" HIGHLIGHT.
           05         COL PLUS 2 PIC ZZZZZZ USING WS-QTY REQUIRED.
           05         COL PLUS 2 VALUE "PRECO" HIGHLIGHT.
           05         COL PLUS 2 PIC ZZZZZZZ,ZZ USING WS-PRICE
                                 REQUIRED.
           05         COL PLUS 4 VALUE "HB" HIGHLIGHT.
           05         COL PLUS 2 PIC X USING WS-HB AUTO REQUIRED.
           05         COL PLUS 4 VALUE "DT" HIGHLIGHT.
           05         COL PLUS 2 PIC X USING WS-DT AUTO REQUIRED.
           05 LINE 5  COL  5 VALUE "Corretagem HB    ".
           05         COL 29 PIC ZZ,ZZ USING WS-HB-COST HIGHLIGHT.
           05 LINE 6  COL  5 VALUE "Corretagem Mesa  ".
           05         COL 30 PIC 9,99 USING WS-DESK-COST HIGHLIGHT.
           05         COL 35 VALUE "%".

           05 LINE 8  COL  5 VALUE "Clearing"
                                  HIGHLIGHT UNDERLINE
                                  FOREGROUND-COLOR 1.
           05         COL 44 VALUE "Bolsa"
                                  HIGHLIGHT UNDERLINE
                                  FOREGROUND-COLOR 1.
           05 LINE 10 COL  5 VALUE "Net Operacional.:" LOWLIGHT.
           05         COL 23 PIC Z.ZZZ.ZZ9,99 FROM WS-NET-OPR.
           05         COL 44 VALUE "Transaction Fee.:" LOWLIGHT.
           05         COL 62 PIC Z.ZZZ.ZZ9,99 FROM WS-TR-FEE.
           05 LINE 11 COL  5 VALUE "Tx. Liquidacao..:" LOWLIGHT.
           05         COL 27 PIC Z.ZZ9,99     FROM WS-LIQUIDITY.
           05         COL 44 VALUE "TTA.............:" LOWLIGHT.
           05         COL 66 PIC Z.ZZ9,99     FROM WS-TTA.
           05 LINE 12 COL  5 VALUE "Tx. Registro....:" LOWLIGHT.
           05         COL 27 PIC Z.ZZ9,99     FROM WS-REGISTER.

           05 LINE 14 COL  5 VALUE "Custos Operacionais"
                                  HIGHLIGHT UNDERLINE
                                  FOREGROUND-COLOR 1.
           05 LINE 16 COL  5 VALUE "Corretagem......:" LOWLIGHT.
           05         COL 27 PIC Z.ZZ9,99     FROM WS-BROKE-COST.
           05 LINE 17 COL  5 VALUE "ISS/PIS/COFINS..:" LOWLIGHT.
           05         COL 27 PIC Z.ZZ9,99     FROM WS-TOT-TX.
           05 LINE 18 COL  5 VALUE "IRRF............:" LOWLIGHT.
           05         COL 27 PIC Z.ZZ9,99     FROM WS-IRRF.
           05 LINE 19 COL  5 VALUE "Outros..........:" LOWLIGHT.
           05         COL 27 PIC Z.ZZ9,99     FROM WS-OUTROS.

           05 LINE 21 COL  5      VALUE "Custo Total.....:".
           05         COL PLUS 2  PIC Z.ZZZ.ZZ9,99 FROM WS-TOTAL-COSTS
                                  FOREGROUND-COLOR 3.

           05         COL 38      VALUE "Net:".
           05         COL PLUS 2  PIC Z.ZZZ.ZZ9,99 FROM WS-NET
                                  FOREGROUND-COLOR 3.
                                                        
           05         COL PLUS 6  VALUE "P.Medio:".
           05         COL PLUS 2  PIC Z.ZZZ.ZZ9,99 FROM WS-PM
                                  FOREGROUND-COLOR 3.
           05 LINE 24 COL  5 PIC X(76) FROM WS-BLANK UNDERLINE. 

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
           MOVE WS-ANO TO WS-ANO-INICIAL.
           MOVE WS-MES TO WS-MES-INICIAL.
           MOVE WS-DIA TO WS-DIA-INICIAL.

       INICIO.
           PERFORM UNTIL WS-SELECT-OPTION = '8' 
              DISPLAY CLEAR-SCREEN
              DISPLAY MENU-PRINCIPAL2-SCREEN
              ACCEPT MENU-PRINCIPAL2-SCREEN
              EVALUATE WS-SELECT-OPTION
                  WHEN 'a'
      *                PERFORM NEW-DEF-CUSTODIA 
                      PERFORM DEF-CUSTODIA-INICIAL
                      MOVE SPACE TO WS-SELECT-OPTION
                  WHEN '1'
                      PERFORM BUYSELL-REG
                      MOVE SPACE TO WS-SELECT-OPTION
                  WHEN '2'
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

       DEF-CUSTODIA-INICIAL.
           PERFORM CLEAR-LOCAL-FIELDS.
           DISPLAY CLEAR-SCREEN.
           DISPLAY DEF-DADOS-INICIAIS-SCR.
           ACCEPT DEF-DADOS-INICIAIS-SCR.

           MOVE "Confirma dados iniciais?" TO WS-MSG.
           DISPLAY QUESTION.
           PERFORM WITH TEST AFTER UNTIL WS-VALID-QUESTION
              ACCEPT QUESTION
           END-PERFORM.
           IF WS-QUESTION NOT = "S" AND WS-QUESTION NOT = "s"
              EXIT PARAGRAPH
           END-IF.

           PERFORM UPD-INITAL-LOSS.

           MOVE "Data inicial, prejuizos acumulados e IRRF definidos"
                TO WS-MSG.
           PERFORM MOSTRA-MSG.
           MOVE "Entre com a custodia inicial" TO WS-MSG
           PERFORM MOSTRA-MSG.

           MOVE 12 TO WS-LN.
           DISPLAY DEF-CUSTODIA-INICIAL-SCR.
           PERFORM VARYING WS-POS-ARRAY FROM 1 BY 1
                                        UNTIL WS-POS-ARRAY > 100
                                        
              ACCEPT  DEF-CUSTODIA-INICIAL-SCR
              MULTIPLY WS-PRICE BY WS-QTY GIVING WS-BALANCE
              MOVE FUNCTION UPPER-CASE(WS-TICKER)  TO WS-TICKER
              DISPLAY DEF-CUSTODIA-INICIAL-SCR
   
              MOVE "Confirma lancamento?" TO WS-MSG
              DISPLAY QUESTION
              PERFORM WITH TEST AFTER UNTIL WS-VALID-QUESTION
                 ACCEPT QUESTION
              END-PERFORM
              DISPLAY MESSAGE-CLEAR
              IF WS-QUESTION NOT = "S" AND WS-QUESTION NOT = "s"
                 EXIT PARAGRAPH
              END-IF

              MOVE WS-TICKER  TO WS-STK02-TICKER(WS-POS-ARRAY)
              MOVE WS-QTY     TO WS-STK02-QTY(WS-POS-ARRAY)
                                 WS-QTY-MASK
              MOVE WS-PRICE   TO WS-STK02-PRICE(WS-POS-ARRAY)
                                 WS-PRICE-MASK
              MOVE WS-BALANCE TO WS-STK02-BALANCE(WS-POS-ARRAY)
                                 WS-BALANCE-MASK

              DISPLAY WS-POS-ARRAY AT LINE WS-LN COLUMN 5
              DISPLAY WS-TICKER AT LINE WS-LN COLUMN 15  
              DISPLAY WS-QTY-MASK AT LINE WS-LN COLUMN 33
              DISPLAY WS-PRICE-MASK AT LINE WS-LN COLUMN 49 
              DISPLAY WS-BALANCE-MASK AT LINE WS-LN COLUMN 69 

              MOVE SPACES TO WS-TICKER
              MOVE ZEROES TO WS-QTY WS-PRICE WS-BALANCE

              ADD 1 TO WS-LN
           END-PERFORM.




       NEW-DEF-CUSTODIA.

           PERFORM CLEAR-LOCAL-FIELDS.
           DISPLAY CLEAR-SCREEN.
           DISPLAY COST-CALC-SCREEN.

           PERFORM WITH TEST AFTER UNTIL WS-VALIDADO = 0
              MOVE 0 TO WS-VALIDADO
              ACCEPT COST-CALC-SCREEN

              IF NOT WS-VALID-ORDER
                 MOVE "Tipo de ordem invalida. C=Compra / V=Venda"
                      TO WS-MSG
                 PERFORM MOSTRA-MSG
                 ADD 1 TO WS-VALIDADO
              END-IF

              IF NOT WS-VALID-HB
                 MOVE "Campo HB (Home Broker) incorreto." TO WS-MSG
                 PERFORM MOSTRA-MSG
                 ADD 1 TO WS-VALIDADO
              END-IF

              IF NOT WS-VALID-DT
                 MOVE "Campo DT (Day Trade) incorreto" TO WS-MSG
                 PERFORM MOSTRA-MSG
                 ADD 1 TO WS-VALIDADO
              END-IF

           END-PERFORM.

           MOVE FUNCTION UPPER-CASE(WS-ORDER)  TO WS-ORDER.
           MOVE FUNCTION UPPER-CASE(WS-TICKER) TO WS-TICKER.
           MOVE FUNCTION UPPER-CASE(WS-HB)     TO WS-HB.
           MOVE FUNCTION UPPER-CASE(WS-DT)     TO WS-DT.

           PERFORM CALCULA.

           IF WS-HB-COST NOT = WFS-HB-COST
               OR WS-DESK-COST NOT = WFS-DESK-COST
               PERFORM UPDATE-BROKE-COST
           END-IF.

           DISPLAY COST-CALC-SCREEN.

           MOVE "Confirma inclusao do registro?" TO WS-MSG.
           DISPLAY QUESTION.
           PERFORM WITH TEST AFTER UNTIL WS-VALID-QUESTION
              ACCEPT QUESTION
           END-PERFORM.
           IF WS-QUESTION = "S" OR WS-QUESTION = "s"
              PERFORM UPD-INITIAL-CUSTODY
              MOVE "Registro incluido na custodia inicial" TO WS-MSG
           ELSE
              MOVE "Registro desconsiderado" TO WS-MSG
           END-IF.
           PERFORM MOSTRA-MSG.

       BUYSELL-REG.
           PERFORM CLEAR-LOCAL-FIELDS.
           DISPLAY CLEAR-SCREEN.
           DISPLAY COST-CALC-SCREEN.
           ACCEPT COST-CALC-SCREEN.
           PERFORM CALCULA.

           IF WS-HB-COST NOT = WFS-HB-COST
               OR WS-DESK-COST NOT = WFS-DESK-COST
               PERFORM UPDATE-BROKE-COST
           END-IF.

           DISPLAY COST-CALC-SCREEN.

           DISPLAY MENU-INPUT-CONFIRM.
           MOVE SPACE TO WS-SELECT-OPTION.
           ACCEPT MENU-INPUT-CONFIRM.
           IF WS-SELECT-OPTION = "S" OR WS-SELECT-OPTION = "s"
              MOVE "Registro Incluido" TO WS-MSG
              DISPLAY PRT-MSG
              CALL "C$SLEEP" USING WS-DELAY END-CALL
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

       UPD-INITAL-LOSS.
           OPEN OUTPUT STK04.
           MOVE WS-ANO-INICIAL  TO WFS-ANO-INICIAL.
           MOVE WS-MES-INICIAL  TO WFS-MES-INICIAL.
           MOVE WS-DIA-INICIAL  TO WFS-DIA-INICIAL.
           MOVE WS-PREJ-COM-INI TO WFS-PREJ-COM-INI.
           MOVE WS-PREJ-DT-INI  TO WFS-PREJ-DT-INI.
           MOVE WS-PREJ-FII-INI TO WFS-PREJ-FII-INI.
           MOVE WS-IRRF-COM-INI TO WFS-IRRF-COM-INI.
           MOVE WS-IRRF-DT-INI  TO WFS-IRRF-DT-INI.
           MOVE WS-IRRF-FII-INI TO WFS-IRRF-FII-INI.
           WRITE STK04-REGISTER.
           CLOSE STK04.

       UPD-INITIAL-CUSTODY.
           OPEN I-O STK02.
           IF WS-STATUS-STK02 EQUAL TO "35"
               CLOSE STK02
               OPEN OUTPUT STK02
               CLOSE STK02
               OPEN I-O STK02
           END-IF.

           MOVE WS-TICKER TO WFS-STK02-TICKER.
           READ STK02 KEY IS WFS-STK02-TICKER

           MOVE WS-QTY    TO WFS-STK02-QTY
           MOVE WS-PRICE  TO WFS-STK02-PRICE
           MOVE WS-NET    TO WFS-STK02-BALANCE
 
           EVALUATE WS-STATUS-STK02
               WHEN "23"
                  WRITE STK02-REGISTER   
                  MOVE "Novo ativo incluido com sucesso" TO WS-MSG
               WHEN "00"
                  REWRITE STK02-REGISTER
                  MOVE "Ativo atualizado com sucesso" TO WS-MSG
               WHEN OTHER 
                  MOVE "Erro na inclusao do ativo" TO WS-MSG
           END-EVALUATE.
           PERFORM MOSTRA-MSG.
           CLOSE STK02.




       UPD-CUSTODY.
           EXIT.
      *     OPEN I-O STK02.
      *     IF WS-STATUS-STK02 EQUAL TO "35"
      *         CLOSE STK02
      *         OPEN OUTPUT STK02
      *         CLOSE STK02
      *         OPEN I-O STK02
      *     END-IF.
      *     MOVE WS-TICKER TO WFS-STK02-TICKER.
      *     READ STK02 KEY IS WFS-STK02-TICKER
      *     EVALUATE WS-STATUS-STK02
      *         WHEN "23"
      *            MOVE WS-QTY    TO WFS-STK02-QTY
      *            MOVE WS-PM     TO WFS-STK02-PMA
      *            MOVE WS-NET    TO WFS-STK02-BALANCE
      *            WRITE STK02-REGISTER   
      *         WHEN "00"
      *            MOVE WFS-STK02-QTY     TO WS-STK02-QTY
      *            MOVE WFS-STK02-PMA     TO WS-STK02-PMA
      *            MOVE WFS-STK02-BALANCE TO WS-STK02-BALANCE
      *
      *            IF WS-ORDER = "B" OR WS-ORDER = "b"
      *               ADD WS-QTY TO WS-STK02-QTY
      *               ADD WS-NET TO WS-STK02-BALANCE
      *               MOVE WS-STK02-BALANCE TO WFS-STK02-BALANCE
      *               MOVE WS-STK02-QTY TO WFS-STK02-QTY
      *               DIVIDE WS-STK02-BALANCE BY WS-STK02-QTY GIVING
      *                                          WFS-STK02-PMA
      *            ELSE
      *               SUBTRACT WS-QTY FROM WS-STK02-QTY GIVING
      *                                          WFS-STK02-QTY
      *            END-IF
      *            REWRITE STK02-REGISTER
      *     END-EVALUATE.
      *     CLOSE STK02.

       LST-CUSTODY.
           DISPLAY CLEAR-SCREEN.
           DISPLAY LIST-CUSTODY.
      *     MOVE 5 TO WS-LN.
      *     MOVE 0 TO WS-STK02-TOT-BALANCE.
      *     OPEN INPUT STK02.
      *     MOVE "00" TO WS-STATUS-STK02.
      *     PERFORM UNTIL WS-STATUS-STK02 = "10"
      *        READ STK02 NEXT RECORD
      *          AT END
      *            MOVE "10" TO WS-STATUS-STK02
      *          NOT AT END
      *            MOVE WFS-STK02-TICKER  TO WS-TICKER
      *            MOVE WFS-STK02-QTY     TO WS-QTY-MASK
      *            MOVE WFS-STK02-PMA     TO WS-PMA-MASK
      *            MOVE WFS-STK02-BALANCE TO WS-BALANCE-MASK
      *            ADD WFS-STK02-BALANCE TO WS-STK02-TOT-BALANCE
      *            DISPLAY WS-TICKER   AT LINE WS-LN COLUMN 01
      *            DISPLAY WS-QTY-MASK AT LINE WS-LN COLUMN 13
      *            DISPLAY WS-PMA-MASK AT LINE WS-LN COLUMN 23
      *            DISPLAY WS-BALANCE-MASK  AT LINE WS-LN COLUMN 37
      *            ADD 1 TO WS-LN
      *         END-READ
      *     END-PERFORM.
      *     CLOSE STK02.
      *     ADD 1 TO WS-LN.
      *     MOVE WS-STK02-TOT-BALANCE TO WS-STK02-TOT-BALANCE-MASK.
      *     DISPLAY "Total Balance" AT LINE WS-LN COLUMN 1.
      *     DISPLAY WS-STK02-TOT-BALANCE-MASK   AT LINE WS-LN COLUMN 37.
      *     ACCEPT WS-SELECT-OPTION AT LINE 23 COLUMN 80.
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

       MOSTRA-MSG.
           DISPLAY MESSAGE-SCREEN.
           MOVE 0 TO CURSOR-VAL.
           CALL "curs_set" USING BY VALUE CURSOR-VAL.
           CALL "C$SLEEP" USING WS-DELAY END-CALL.
           MOVE 1 TO CURSOR-VAL.
           CALL "curs_set" USING BY VALUE CURSOR-VAL.
           DISPLAY MESSAGE-CLEAR.
           EXIT.


