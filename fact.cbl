       IDENTIFICATION DIVISION.
       PROGRAM-ID. Factorial-hopefully.
       AUTHOR. Ben Wendt.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Keeping-Track-Variables.
          05 Operand                      PIC S99     VALUE 0.
          05 Product                      PIC 9(18)   VALUE 1.
       PROCEDURE DIVISION.
       PERFORM-FACTORIAL.
        DISPLAY SPACES
        PERFORM VARYING Operand FROM 16 BY -1 UNTIL Operand = 0
          
          DISPLAY "Before Product " Product " Operand " Operand
          MULTIPLY Product By Operand GIVING Product
          DISPLAY "After Product " Product " Operand " Operand
        END-PERFORM
        DISPLAY Product.
       STOP RUN.

