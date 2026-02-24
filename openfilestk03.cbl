       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLSTK03.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy 'control_register'.
       DATA DIVISION.
       FILE SECTION.
           copy 'register'.
       LINKAGE SECTION.
       01 LNK-STATUS            PIC X(02).
       WORKING-STORAGE SECTION.
       01 WS-STATUS-STK03       PIC X(02).

       PROCEDURE DIVISION USING LNK-STATUS.
       INICIO.
           OPEN I-O STK03.
           EVALUATE WS-STATUS-STK03
                   WHEN "00" *> Success
                          DISPLAY "Status 00"
                          CLOSE STK03
                   WHEN "35" *> File not found
                           OPEN OUTPUT STK03
                           IF WS-STATUS-STK03 = "00"
                               DISPLAY "Status 35 -> 00"
                               CLOSE STK03
                           ELSE
                               DISPLAY "Status -> " WS-STATUS-STK03
                           END-IF
           END-EVALUATE.
           GOBACK.
