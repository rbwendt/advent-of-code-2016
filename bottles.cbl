       IDENTIFICATION DIVISION.
       PROGRAM-ID. 99-Bottles-of-Beer-On-The-Wall.
       AUTHOR. Joseph James Frantz.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Keeping-Track-Variables.
          05 Bottles                      PIC S99   VALUE 0.
          05 Remaining-Bottles            PIC S99   VALUE 0.
          05 Counting                     PIC 99    VALUE 0.
          05 Start-Position               PIC 99    VALUE 0.
          05 Positions                    PIC 99    VALUE 0.
       PROCEDURE DIVISION.
       PASS-AROUND-THOSE-BEERS.
        PERFORM VARYING Bottles FROM 99 BY -1 UNTIL Bottles = -1
          DISPLAY SPACES
          SUBTRACT 1 FROM Bottles GIVING Remaining-Bottles
          EVALUATE Bottles
            WHEN 0
              DISPLAY "No more bottles of beer on the wall, "
                      "no more bottles of beer."
              DISPLAY "Go to the store and buy some more, "
                      "99 bottles of beer on the wall."
            WHEN 1
              DISPLAY "1 bottle of beer on the wall, "
                      "1 bottle of beer."
              DISPLAY "Take one down and pass it around, "
                      "no more bottles of beer on the wall."
            WHEN 2 Thru 99
              MOVE ZEROES TO Counting
              INSPECT Bottles,
                TALLYING Counting FOR LEADING ZEROES
              ADD 1 TO Counting GIVING Start-Position
              SUBTRACT Counting FROM 2 GIVING Positions
              DISPLAY Bottles(Start-Position:Positions)
                      " bottles of beer on the wall, "
                      Bottles(Start-Position:Positions)
                      " bottles of beer."
              MOVE ZEROES TO Counting
              INSPECT Remaining-Bottles TALLYING
                Counting FOR LEADING ZEROES
              ADD 1 TO Counting GIVING Start-Position
              SUBTRACT Counting FROM 2 GIVING Positions
              DISPLAY "Take one down and pass it around, "
                      Remaining-Bottles(Start-Position:Positions)
                      " bottles of beer on the wall."
          END-EVALUATE
        END-PERFORM
       STOP RUN.

