       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTE01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STK02 ASSIGN TO "stk02.dat"
           ORGANIZATION IS SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL 
           FILE STATUS IS WS-STATUS-STK02.
       DATA DIVISION.
       FILE SECTION.
       FD STK02.
       01 STK02-REGISTER.
           05 WFS-STK02-TICKER           PIC X(10).
           05 WFS-STK02-QTY              PIC S9(06).
           05 WFS-STK02-PRICE            PIC 9(04)V99.
           05 WFS-STK02-BALANCE          PIC S9(07)V99.
           05 WFS-DESK-COST              PIC 9(01)V99.

       WORKING-STORAGE SECTION.
       01 WS-STATUS-STK02  PIC X(02).

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT STK02.
           DISPLAY "OPEN... FS STATUS= " WS-STATUS-STK02. 
           IF WS-STATUS-STK02 = "35"
               OPEN OUTPUT STK02
           ELSE
               READ STK02
               DISPLAY "Reading... FS STATUS= " WS-STATUS-STK02 
           END-IF.
           CLOSE STK02.
           STOP RUN.
