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
           COPY 'control_custody'.             *> STK02
           COPY 'control_register'.            *> STK03
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
       77 MSGDELAY              PIC 9V9999 VALUE 3,0. *> 3,0 seg
       77 WS-VALIDADO           PIC 9(02) VALUE 0.
       77 WS-MSG                PIC X(76)  VALUE SPACES.
       77 WS-LN                 PIC 9(02).
       77 WS-POS-ARRAY          PIC 9(3).
       77 WS-CONTADOR           PIC 9(3).
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
       77 WS-FIM-ARQ            PIC X.
       77 WS-SCAN               PIC 9(3).
       77 WS-STATUS             PIC X.
       
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

       01 CONST-MSG             PIC X.
           78 MSGSTD              VALUE 'S'.
           78 MSGALERT            VALUE 'A'.
           78 MSGVOID             VALUE 'V'.
           78 MSGQUESTION         VALUE 'Q'.
           78 MSGYESNO            VALUE 'Y'.
           

       01 STK02-REGISTER-LOCAL.
           03 WS-STK02-TICKER           PIC X(10)     OCCURS 100 TIMES.
           03 WS-STK02-QTY              PIC S9(06)    OCCURS 100 TIMES.
           03 WS-STK02-PRICE            PIC 9(04)V99  OCCURS 100 TIMES.
           03 WS-STK02-BALANCE          PIC S9(07)V99 OCCURS 100 TIMES.
           03 WS-STK02-TOT-BALANCE      PIC S9(08)V99.
      *     03 WS-STK02-TOT-BALANCE-MASK PIC Z.ZZZ.ZZ9,99.

       01 WS-FLAG-FOUND         PIC 9.
           88 WS-NOT-FOUND       VALUE 0.
           88 WS-FOUND           VALUE 1.

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
           05 WS-QTY            PIC S9(6).
           05 WS-PRICE          PIC 9(4)V99.
           05 WS-PM             PIC 9(4)V99.
           05 WS-ALLOC          PIC S9(7)V99.
           05 WS-BALANCE        PIC S9(7)V99.
       01 WS-STOCK-MASK.
           05 WS-QTY-MASK       PIC -ZZZ.ZZZ.
           05 WS-PMA-MASK       PIC Z.ZZ9,99.
           05 WS-PRICE-MASK     PIC Z.ZZZ.ZZ9,99.
           05 WS-BALANCE-MASK   PIC -Z.ZZZ.ZZ9,99.
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

       01 CLEAR-SCREEN-PART-00.
           05 LINE 10 COL 1 BLANK LINE.
           05 LINE 11 COL 1 BLANK LINE.

       01 CLEAR-SCREEN-PART-01.
           05 LINE 12 COL 1 BLANK LINE.
           05 LINE 13 COL 1 BLANK LINE.
           05 LINE 14 COL 1 BLANK LINE.
           05 LINE 15 COL 1 BLANK LINE.
           05 LINE 16 COL 1 BLANK LINE.
           05 LINE 17 COL 1 BLANK LINE.
           05 LINE 18 COL 1 BLANK LINE.
           05 LINE 19 COL 1 BLANK LINE.
           05 LINE 20 COL 1 BLANK LINE.
           05 LINE 21 COL 1 BLANK LINE.

       01 MENU-PRINCIPAL2-SCREEN.
          05 BLANK SCREEN.
          05 LINE 1  COL 5 PIC X(76) FROM WS-BLANK HIGHLIGHT UNDERLINE. 
          05 LINE 1  COL 60 VALUE "CONTROLE DE PORTFOLIO" UNDERLINE
                                                          HIGHLIGHT. 
          05 LINE 2  COL 5 VALUE "Menu Principal" HIGHLIGHT.

          05 LINE 4  COL 5 VALUE "Configuracoes Iniciais" UNDERLINE 
                                  FOREGROUND-COLOR 1 HIGHLIGHT.
          05 LINE 5  COL 5 VALUE "a) Definir custodia inicial".
          05 LINE 6  COL 5 VALUE "b) Consultar dados iniciais".
          05 LINE 7  COL 5 VALUE "c) Redefinir configuracoes iniciais".
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
          05 LINE 23 COL 21 PIC X USING WS-SELECT-OPTION AUTO.
          05 LINE 24 COL 5 PIC X(76) FROM WS-BLANK UNDERLINE. 
 
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
           05 LINE 1  COL 61 VALUE "CONFIGURACAO INICIAL" HIGHLIGHT
                                                          UNDERLINE. 
           05 LINE 2 COL 5 VALUE "DATA INICIAL   /  /  "  HIGHLIGHT.
           05        COL 18 PIC 99 USING WS-DIA-INICIAL   AUTO.
           05        COL 21 PIC 99 USING WS-MES-INICIAL   AUTO.
           05        COL 24 PIC 99 USING WS-ANO-INICIAL   AUTO.

           05 LINE 4 COL 05 VALUE "OPERACOES COMUNS  "    HIGHLIGHT
                                                          UNDERLINE.
           05        COL 35 VALUE "DAY TRADES        "    HIGHLIGHT
                                                          UNDERLINE.
           05        COL 63 VALUE "FIIs              "    HIGHLIGHT
                                                          UNDERLINE.

           05 LINE 5 COL  5 VALUE "Prejuizo".
           05        COL 35 VALUE "Prejuizo".
           05        COL 63 VALUE "Prejuizo".

           05        COL 14 PIC ZZZZZZ,ZZ USING WS-PREJ-COM-INI.
           05        COL 44 PIC ZZZZZZ,ZZ USING WS-PREJ-DT-INI.
           05        COL 72 PIC ZZZZZZ,ZZ USING WS-PREJ-FII-INI.

           05 LINE 6 COL  5 VALUE "IRRF".
           05        COL 35 VALUE "IRRF".
           05        COL 63 VALUE "IRRF".

           05        COL 15 PIC ZZZZZ,ZZ USING WS-IRRF-COM-INI.
           05        COL 45 PIC ZZZZZ,ZZ USING WS-IRRF-DT-INI.
           05        COL 73 PIC ZZZZZ,ZZ USING WS-IRRF-FII-INI.

           05 LINE 7 COL 5 PIC X(76) FROM WS-BLANK HIGHLIGHT UNDERLINE. 
           05 LINE 24 COL  5 PIC X(76) FROM WS-BLANK UNDERLINE. 
           
       01 DEF-CUSTODIA-INICIAL-SCR.
           05 LINE 9 COL 5 VALUE "PREGAO   /  /  "        HIGHLIGHT.
           05        COL 21 VALUE "ATIVO"                 HIGHLIGHT.
           05        COL 38 VALUE "QTD"                   HIGHLIGHT.
           05        COL 50 VALUE "PM"                    HIGHLIGHT.
           05        COL 62 VALUE "TOTAL"                 HIGHLIGHT.

           05        COL 12 PIC 99 USING WS-DIA           AUTO.
           05        COL 15 PIC 99 USING WS-MES           AUTO.
           05        COL 18 PIC 99 USING WS-ANO           AUTO.
           05        COL 27 PIC X(10) USING WS-TICKER.
           05        COL 42 PIC -ZZZZZZ USING WS-QTY.
           05        COL 53 PIC ZZZZ,ZZ USING WS-PRICE.
           05        COL 68 PIC -Z.ZZZ.ZZ9,99 FROM WS-BALANCE
                                             REVERSE-VIDEO.
           05 LINE 11 COL  5 VALUE "SEQ"           HIGHLIGHT UNDERLINE. 
           05         COL 15 VALUE "ATIVO"         HIGHLIGHT UNDERLINE. 
           05         COL 29 VALUE "QUANTIDADE"    HIGHLIGHT UNDERLINE. 
           05         COL 49 VALUE "PRECO MEDIO"   HIGHLIGHT UNDERLINE. 
           05         COL 68 VALUE "        TOTAL" HIGHLIGHT UNDERLINE. 

           05 LINE 24 COL  5 PIC X(76) FROM WS-BLANK       UNDERLINE. 

       01 DEF-CUSTODIA-INICIAL-TITULO-SCR.
           05 LINE 09 COL  5 VALUE "SEQ"          HIGHLIGHT UNDERLINE. 
           05         COL 15 VALUE "ATIVO"        HIGHLIGHT UNDERLINE. 
           05         COL 30 VALUE "QUANTIDADE"   HIGHLIGHT UNDERLINE. 
           05         COL 50 VALUE "PRECO MEDIO"  HIGHLIGHT UNDERLINE. 
           05         COL 69 VALUE "       TOTAL" HIGHLIGHT UNDERLINE. 

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
           05         COL PLUS 2 PIC X USING WS-ORDER AUTO.
           05         COL PLUS 2 VALUE "TICKER" HIGHLIGHT.
           05         COL PLUS 2 PIC X(10) USING WS-TICKER.
           05         COL PLUS 2 VALUE "QTD" HIGHLIGHT.
           05         COL PLUS 2 PIC ZZZZZZ USING WS-QTY.
           05         COL PLUS 2 VALUE "PRECO" HIGHLIGHT.
           05         COL PLUS 2 PIC ZZZZZZZ,ZZ USING WS-PRICE.
           05         COL PLUS 4 VALUE "HB" HIGHLIGHT.
           05         COL PLUS 2 PIC X USING WS-HB AUTO.
           05         COL PLUS 4 VALUE "DT" HIGHLIGHT.
           05         COL PLUS 2 PIC X USING WS-DT AUTO.
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
              DISPLAY MENU-PRINCIPAL2-SCREEN

              MOVE SPACE TO WS-SELECT-OPTION
              ACCEPT MENU-PRINCIPAL2-SCREEN

              EVALUATE WS-SELECT-OPTION
                  WHEN 'a'
                      PERFORM DEF-CUSTODIA-INICIAL
                      MOVE SPACE TO WS-SELECT-OPTION
                  WHEN 'b'
                      PERFORM LST-CUSTODIA-INICIAL
                      MOVE SPACE TO WS-SELECT-OPTION
                  WHEN '1'
                      PERFORM REG-BUY-SELL
                      MOVE SPACE TO WS-SELECT-OPTION
                  WHEN '2'
      *               PERFORM LST-CUSTODY
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

      *    Limpa array que acumula dados dos lancamentos dos ativos     
           PERFORM VARYING WS-SCAN FROM 1 BY 1 UNTIL WS-SCAN = 100
               MOVE ZEROES TO WS-STK02-TICKER(WS-SCAN)
               MOVE ZEROES TO WS-STK02-QTY(WS-SCAN)
               MOVE ZEROES TO WS-STK02-PRICE(WS-SCAN)
               MOVE ZEROES TO WS-STK02-BALANCE(WS-SCAN)
           END-PERFORM.


           DISPLAY CLEAR-SCREEN.
           DISPLAY DEF-DADOS-INICIAIS-SCR.
           ACCEPT DEF-DADOS-INICIAIS-SCR.

           CALL 'showmsg' USING "Confirma dados iniciais?",
                          MSGYESNO, MSGDELAY, WS-QUESTION

           IF WS-QUESTION NOT = "S"
              EXIT PARAGRAPH
           END-IF.

           DISPLAY DEF-DADOS-INICIAIS-SCR.

           PERFORM UPD-INITIAL-LOSS.

           CALL 'showmsg' USING "Dados iniciais definidos ",
                          MSGSTD, MSGDELAY.

           MOVE 12 TO WS-LN.
           MOVE  1 TO WS-POS-ARRAY.
           DISPLAY DEF-CUSTODIA-INICIAL-SCR.
           PERFORM VARYING WS-CONTADOR FROM 1 BY 1
                                        UNTIL WS-CONTADOR > 100
                                        
              ACCEPT  DEF-CUSTODIA-INICIAL-SCR
              IF WS-TICKER = SPACES 
                IF WS-POS-ARRAY = 1
                  EXIT PERFORM 
                END-IF

                CALL 'showmsg' USING "Confirma inclusao da custodia?",
                               MSGYESNO, MSGDELAY, WS-QUESTION

                IF WS-QUESTION NOT = "S"
                    EXIT PARAGRAPH
                ELSE
                    PERFORM UPD-CUSTODIA-INICIAL
                    EXIT PERFORM
                END-IF
              END-IF


              MULTIPLY WS-PRICE BY WS-QTY GIVING WS-BALANCE
              MOVE FUNCTION UPPER-CASE(WS-TICKER)    TO WS-TICKER
              MOVE FUNCTION TRIM(WS-TICKER TRAILING) TO WS-TICKER
              MOVE FUNCTION TRIM(WS-TICKER LEADING)  TO WS-TICKER
              DISPLAY DEF-CUSTODIA-INICIAL-SCR

              CALL 'showmsg' USING "Confirma lancamento?",
                               MSGYESNO, MSGDELAY, WS-QUESTION

              IF WS-QUESTION = "S"
                PERFORM FUNCT-VERIFICA-TICKER-REPETIDO
                IF WS-NOT-FOUND 
                  MOVE WS-TICKER  TO WS-STK02-TICKER(WS-POS-ARRAY)
                  MOVE WS-QTY     TO WS-STK02-QTY(WS-POS-ARRAY)
                                     WS-QTY-MASK
                  MOVE WS-PRICE   TO WS-STK02-PRICE(WS-POS-ARRAY)
                                     WS-PRICE-MASK
                  MOVE WS-BALANCE TO WS-STK02-BALANCE(WS-POS-ARRAY)
                                     WS-BALANCE-MASK

                  IF WS-LN > 21
                    MOVE 12 TO WS-LN
                    DISPLAY CLEAR-SCREEN-PART-01
                  END-IF

                  DISPLAY WS-POS-ARRAY    AT LINE WS-LN COLUMN  5
                  DISPLAY WS-TICKER       AT LINE WS-LN COLUMN 15  
                  DISPLAY WS-QTY-MASK     AT LINE WS-LN COLUMN 31
                  DISPLAY WS-PRICE-MASK   AT LINE WS-LN COLUMN 48 
                  DISPLAY WS-BALANCE-MASK AT LINE WS-LN COLUMN 68 

                  ADD 1 TO WS-LN WS-POS-ARRAY
                ELSE
                  CALL 'showmsg' USING "Ativo ja cadastrado",
                                 MSGALERT, MSGDELAY
                END-IF
                MOVE SPACES TO WS-TICKER
                MOVE ZEROES TO WS-QTY WS-PRICE WS-BALANCE
              ELSE  *> WS-QUESTION NOT = "S"
                PERFORM CLEAR-LOCAL-FIELDS
              END-IF
           END-PERFORM.

       FUNCT-VERIFICA-TICKER-REPETIDO.
           MOVE 0 TO WS-FLAG-FOUND.
           PERFORM VARYING WS-SCAN FROM 1 BY 1 
               UNTIL WS-SCAN > WS-POS-ARRAY OR WS-SCAN = 100

               IF WS-TICKER = WS-STK02-TICKER(WS-SCAN)
                   MOVE 1 TO WS-FLAG-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.



       UPD-CUSTODIA-INICIAL.
           OPEN OUTPUT STK02.
           PERFORM VARYING WS-LN FROM 1 BY 1
                                        UNTIL WS-LN >= WS-POS-ARRAY
             MOVE WS-STK02-TICKER(WS-LN)  TO WFS-STK02-TICKER
             MOVE WS-STK02-QTY(WS-LN)     TO WFS-STK02-QTY
             MOVE WS-STK02-PRICE(WS-LN)   TO WFS-STK02-PRICE
             MOVE WS-STK02-BALANCE(WS-LN) TO WFS-STK02-BALANCE

             WRITE STK02-REGISTER
           END-PERFORM.
           CLOSE STK02.

       LST-CUSTODIA-INICIAL.
           PERFORM READ-INITIAL-LOSS.
           DISPLAY CLEAR-SCREEN.
           DISPLAY DEF-DADOS-INICIAIS-SCR.
           DISPLAY DEF-CUSTODIA-INICIAL-TITULO-SCR.
 
           OPEN INPUT STK02.
           IF WS-STATUS-STK02 EQUAL TO "35"
             CALL 'showmsg' USING "Custodia nao cadastrada",
                            MSGALERT, MSGDELAY
             EXIT PARAGRAPH
           END-IF.
           
           MOVE 10 TO WS-LN.
           MOVE  1 TO WS-POS-ARRAY.
           MOVE "N" TO WS-FIM-ARQ.
           PERFORM LST-CUSTODIA-INICIAL-SEQ UNTIL WS-FIM-ARQ = "S".
           CLOSE STK02.

           CALL 'showmsg' USING "Tecle ENTER para retornar",
                          MSGVOID, MSGDELAY.

       LST-CUSTODIA-INICIAL-SEQ.
           READ STK02 AT END MOVE "S" TO WS-FIM-ARQ.

           IF WS-STATUS-STK02 = "00"
             MOVE WFS-STK02-TICKER  TO WS-TICKER
             MOVE WFS-STK02-QTY     TO WS-QTY-MASK
             MOVE WFS-STK02-PRICE   TO WS-PRICE-MASK
             MOVE WFS-STK02-BALANCE TO WS-BALANCE-MASK

             DISPLAY WS-POS-ARRAY    AT LINE WS-LN COLUMN  5
             DISPLAY WS-TICKER       AT LINE WS-LN COLUMN 15  
             DISPLAY WS-QTY-MASK     AT LINE WS-LN COLUMN 32
             DISPLAY WS-PRICE-MASK   AT LINE WS-LN COLUMN 49 
             DISPLAY WS-BALANCE-MASK AT LINE WS-LN COLUMN 68

             IF WS-LN < 21
                 ADD 1 TO WS-LN
             ELSE
                 MOVE 10 TO WS-LN

                 CALL 'showmsg' USING "Continua... Pressione ENTER",
                                MSGVOID, MSGDELAY

                 DISPLAY CLEAR-SCREEN-PART-00
                 DISPLAY CLEAR-SCREEN-PART-01
             END-IF

             ADD 1 TO WS-POS-ARRAY
             ELSE
                 IF WS-STATUS-STK02 = "10"
                     CALL 'showmsg' USING "Custodia completa",
                                    MSGSTD, MSGDELAY
                 ELSE
                     CALL 'showmsg' USING "Erro lendo o arquivo",
                                    MSGALERT, MSGDELAY
                 END-IF
                 MOVE "S" TO WS-FIM-ARQ
           END-IF.


       REG-BUY-SELL.                                  *> Registra Ordens de compra e venda de ativos
           PERFORM CLEAR-LOCAL-FIELDS.
           DISPLAY CLEAR-SCREEN.
           DISPLAY COST-CALC-SCREEN.

           MOVE SPACE TO WS-STATUS.
           PERFORM VARYING WS-CONTADOR FROM 1 BY 1 UNTIL WS-STATUS = "F"
              MOVE ZERO TO WS-FLAG-FOUND
              ACCEPT COST-CALC-SCREEN

              MOVE FUNCTION UPPER-CASE(WS-ORDER)   TO WS-ORDER
              MOVE FUNCTION UPPER-CASE(WS-TICKER)  TO WS-TICKER
              MOVE FUNCTION UPPER-CASE(WS-HB)      TO WS-HB
              MOVE FUNCTION UPPER-CASE(WS-DT)      TO WS-DT

              IF WS-TICKER = SPACES
                  EXIT PERFORM
              END-IF

              IF WS-ORDER NOT = "C" AND WS-ORDER NOT = "V"
                CALL 'showmsg' USING "Ordem aceita [C]ompra ou [V]enda",
                               MSGALERT, MSGDELAY
                MOVE 1 TO WS-FLAG-FOUND
              END-IF

              IF WS-QTY = 0
                CALL 'showmsg' USING "Qtd. deve ser maior que zero",
                               MSGALERT, MSGDELAY
                MOVE 1 TO WS-FLAG-FOUND
              END-IF

              IF WS-PRICE = 0 
                CALL 'showmsg' USING "Preco deve ser maior que zero",
                               MSGALERT, MSGDELAY
                MOVE 1 TO WS-FLAG-FOUND
              END-IF

              IF WS-DT NOT = "S" AND WS-DT NOT = "N"
                CALL 'showmsg' USING "DT deve ser [S]im ou [N]ao",
                               MSGALERT, MSGDELAY
                MOVE 1 TO WS-FLAG-FOUND
              END-IF

              IF WS-HB NOT = "S" AND WS-HB NOT = "N"
                CALL 'showmsg' USING "HB deve ser [S]im ou [N]ao",
                               MSGALERT, MSGDELAY
                MOVE 1 TO WS-FLAG-FOUND
              END-IF

              IF WS-NOT-FOUND       *> Nenhum erro encontrado na entrada
                 PERFORM CALCULA 

                 IF WS-HB-COST NOT = WFS-HB-COST
                    OR WS-DESK-COST NOT = WFS-DESK-COST
                    PERFORM UPDATE-BROKE-COST
                 END-IF

                 DISPLAY COST-CALC-SCREEN

                 CALL 'showmsg' USING "Confirma lancamento?",
                                MSGYESNO, MSGDELAY, WS-QUESTION

                 IF WS-QUESTION = "S"
                    PERFORM UPD-REGISTER
                    CALL 'showmsg' USING "Registro incluido",
                                   MSGSTD, MSGDELAY
                    ELSE 
                        PERFORM CLEAR-LOCAL-FIELDS
                 END-IF

                 CALL 'showmsg' USING "Incluir novo registro",
                                MSGYESNO, MSGDELAY, WS-QUESTION
                 IF WS-QUESTION NOT = "S"
                    MOVE "F" TO WS-STATUS
                 END-IF
              END-IF
            
              PERFORM CLEAR-LOCAL-FIELDS
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

           IF WS-HB = "S" OR WS-HB = "s"
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
               IF WS-DT = "S" OR WS-DT = "s"
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
           MOVE ZEROES TO WS-TOTAL-COSTS WS-NET WS-PM WS-BALANCE.

           MOVE ZEROES TO WS-STK02-TOT-BALANCE.

           MOVE ZEROES TO WS-PREJ-COM-INI WS-PREJ-DT-INI
                          WS-PREJ-FII-INI.
           MOVE ZEROES TO WS-IRRF-COM-INI WS-IRRF-DT-INI
                          WS-IRRF-FII-INI.
           EXIT.

       CLEAR-STK04-FIELDS.
           MOVE 0 TO WS-ANO-INICIAL WS-MES-INICIAL WS-DIA-INICIAL.
           MOVE 0 TO WS-PREJ-COM-INI WS-PREJ-DT-INI WS-PREJ-FII-INI. 
           MOVE 0 TO WS-IRRF-COM-INI WS-IRRF-DT-INI WS-IRRF-FII-INI.

       UPD-INITIAL-LOSS.
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

       READ-INITIAL-LOSS.
           OPEN INPUT STK04.
           IF WS-STATUS-STK04 EQUAL TO "35"
               PERFORM CLEAR-STK04-FIELDS
               PERFORM UPD-INITIAL-LOSS
               GO TO READ-INITIAL-LOSS
           END-IF.
           READ STK04.
           MOVE WFS-ANO-INICIAL  TO WS-ANO-INICIAL.
           MOVE WFS-MES-INICIAL  TO WS-MES-INICIAL.
           MOVE WFS-DIA-INICIAL  TO WS-DIA-INICIAL.
           MOVE WFS-PREJ-COM-INI TO WS-PREJ-COM-INI.
           MOVE WFS-PREJ-DT-INI  TO WS-PREJ-DT-INI.
           MOVE WFS-PREJ-FII-INI TO WS-PREJ-FII-INI.
           MOVE WFS-IRRF-COM-INI TO WS-IRRF-COM-INI.
           MOVE WFS-IRRF-DT-INI  TO WS-IRRF-DT-INI.
           MOVE WFS-IRRF-FII-INI TO WS-IRRF-FII-INI.
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
                 CALL 'showmsg' USING "Novo ativo incluido com sucesso",
                                MSGSTD, MSGDELAY
               WHEN "00"
                 REWRITE STK02-REGISTER
                 CALL 'showmsg' USING "Ativo atualizado com sucesso",
                                MSGSTD, MSGDELAY
               WHEN OTHER 
                 CALL 'showmsg' USING "Erro na inclusao do ativo",
                                MSGALERT, MSGDELAY
           END-EVALUATE.
           CLOSE STK02.

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

