       Identification Division.
       Program-id. chkdate.
       Author. Fabiano Costa.
       Date-Written. Mar/26.
       Remarks. Valida Data.

       Data Division.
       Working-Storage Section.
       01 WS-YEAR          PIC 9(2).
       01 WS-MONTH         PIC 9(2) OCCURS 12 TIMES.
       01 WS-LEAP-YEAR.
           05 WS-QUO       PIC 9(3).
           05 WS-REMAINDER PIC 9(2).
           

       Linkage Section.
       01 LK-RETURN    PIC 9.
       01 LK-YEAR      PIC 9(2).
       01 LK-MONTH     PIC 9(2).
       01 LK-DAY       PIC 9(2).

       Procedure Division using LK-YEAR, LK-MONTH, LK-DAY, LK-RETURN.
       Proc00.
           Move LK-YEAR to WS-YEAR.
           Move ZERO    to LK-RETURN.
           Add  2000    to WS-YEAR.

           Perform LoadMonths.

           If WS-YEAR < 2000 OR > 2099
               Move 1 to LK-RETURN
               Exit Paragraph
           End-If.

           If LK-MONTH > 12
               Move 1 to LK-RETURN
               Exit Paragraph
           End-If.

           If LK-DAY > WS-MONTH(LK-MONTH)
               Move 1 to LK-RETURN
           End-If.

       Proc99.
           Goback.

       LoadMonths.
           Move 31 to WS-MONTH(1).
           Move 31 to WS-MONTH(3).
           Move 30 to WS-MONTH(4).
           Move 31 to WS-MONTH(5).
           Move 30 to WS-MONTH(6).
           Move 31 to WS-MONTH(7).
           Move 31 to WS-MONTH(8).
           Move 30 to WS-MONTH(9).
           Move 31 to WS-MONTH(10).
           Move 30 to WS-MONTH(11).
           Move 31 to WS-MONTH(12).

           Divide WS-YEAR by 4 giving WS-QUO remainder WS-REMAINDER.
           If WS-REMAINDER = 0
               Move 29 to WS-MONTH(2)
           Else
               Move 28 to WS-MONTH(2)
           End-If.
