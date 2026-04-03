       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTREG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY 'control_register'.

           SELECT ARQ-OUT ASSIGN TO "sregisters.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS  WS-STATUS-ARQ-OUT.

           SELECT ARQ-SRT ASSIGN TO SORT-WORK-FILE.

       DATA DIVISION.
       FILE SECTION.
       FD STK03.
           COPY 'register'.
       FD ARQ-OUT.
           COPY 'sregister'.
       SD ARQ-SRT.
       01 ARQ-OUT-REGISTER.
           05 SRT-KEY                 PIC 9(12).
           05 SRT-ORDER               PIC X(1).
           05 SRT-TICKER              PIC X(10).
           05 SRT-QTY                 PIC 9(06).
           05 SRT-PRICE               PIC 9(07)V99.
           05 SRT-IRRF                PIC 9(04)V99.
           05 SRT-COST                PIC 9(07)V99.
           05 SRT-NET                 PIC 9(07)V99.
           05 SRT-AVPRICE             PIC 9(07)V99.

       WORKING-STORAGE SECTION.
       01 WS-STATUS-STK03                    PIC X(02).
       01 WS-STATUS-ARQ-OUT                  PIC X(02).
       01 WS-EOF                             PIC X(01) VALUE "N".

       PROCEDURE DIVISION.
       MAIN-SECTION.

           SORT ARQ-SRT
                ON ASCENDING KEY SRT-KEY
                USING STK03
                GIVING ARQ-OUT.

           GOBACK.
