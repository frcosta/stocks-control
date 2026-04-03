       Identification Division.
       Program-id. processa.
       Remarks.
      *****************************************************************
      * Processa registros                                            *
      *                                                               *
      *****************************************************************
       Environment Division.
       Input-Output Section.
       File-Control.
           Copy 'control_stk02'.    *> Custodia inicial 
           Copy 'control_stk03'.    *> Registros de compra e venda 
           Copy 'control_stk05'.    *> Custodia atual

       Data Division.
       File Section.
       FD STK02.
           Copy 'stk02'.
       FD STK03.
           Copy 'stk03'.
       FD STK05.
           Copy 'stk05'.

       Working-Storage Section.
       77 WS-STATUS-STK02 PIC X(02).
       77 WS-STATUS-STK03 PIC X(02).
       77 WS-STATUS-STK05 PIC X(02).
       77 WS-SCAN         PIC 9(03).
       77 WS-FIM-ARQ      PIC X       VALUE "N".

       Copy 'stk02_local'.
       Copy 'stk03_local'.
       Copy 'stk05_local'.

       Procedure Division.
       Proc00.
           Open INPUT STK02.

           If WS-STATUS-STK02 equal to "00"   *> Existe custodia inicial

             Perform Varying WS-SCAN from 1 by 1 until WS-SCAN = 100
               Move 0 to WS-STK02-TICKER(WS-SCAN)
               Move 0 to WS-STK02-QTY(WS-SCAN)
               Move 0 to WS-STK02-PRICE(WS-SCAN)
               Move 0 to WS-STK02-BALANCE(WS-SCAN)
             End-Perform

             Move 1 to WS-SCAN
             Perform Load00 until WS-FIM-ARQ = "S"  *> Carrega custodia
             close stk02
           End-If.

       Proc99.
           Goback. 

       Load00.
           Read STK02 at end Move "S" to WS-FIM-ARQ.
           If WS-STATUS-STK02 = "00"
               Move WFS-STK02-TICKER  to WS-STK02-TICKER(WS-SCAN)
               Move WFS-STK02-QTY     to WS-STK02-QTY(WS-SCAN)
               Move WFS-STK02-PRICE   to WS-STK02-PRICE(WS-SCAN)
               Move WFS-STK02-BALANCE to WS-STK02-BALANCE(WS-SCAN)
               Add 1 to WS-SCAN
           End-If.
