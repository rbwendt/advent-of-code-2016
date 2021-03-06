IDENTIFICATION DIVISION.
PROGRAM-ID. Advent-2016-Day-1.b.
AUTHOR. Ben Wendt.

ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.
    SELECT DIRECTIONS ASSIGN TO 'day1.txt'
    ORGANIZATION IS LINE SEQUENTIAL.            

DATA DIVISION.
   FILE SECTION.
   FD DIRECTIONS.
   01 DIRECTION-FILE.
      05 DIRECTION PIC X(1).
      05 MAGNITUDE PIC X(3).

   WORKING-STORAGE SECTION.
   01 Keeping-Track-Variables.
      05 Horizontal    PIC S9(3) VALUE 0.
      05 Vertical      PIC S9(3) VALUE 0.
      05 V-Horizontal  PIC S9(3) VALUE 0.
      05 V-Vertical    PIC S9(3) VALUE 1.
      05 Manhattan     PIC S9(3) VALUE 0.
      05 Num-Magnitude PIC 9(3).
   01 Something-Else.
      05 Raw-Magnitude PIC X(3).
      05 Move-Magnitude REDEFINES Raw-Magnitude PIC Z(3).
   01 WS-DIRECTIONS.
      05 WS-DIRECTION PIC X(1).
      05 WS-MAGNITUDE PIC 9(3).
   01 Previous-Visit-Variables.
      05 Visited-Spots OCCURS 178 TIMES.
        07 First-Coord PIC 9(3) VALUE 0.
        07 Other-Coord OCCURS 2 TIMES.
          09 Second-Coord PIC 9(3) VALUE 0.
   01 WS-EOF PIC A(1).

PROCEDURE DIVISION.
   OPEN INPUT DIRECTIONS.
      PERFORM UNTIL WS-EOF='Y'
         READ DIRECTIONS INTO WS-DIRECTIONS
            AT END MOVE 'Y' TO WS-EOF
            NOT AT END
              SET Raw-Magnitude to WS-MAGNITUDE
              *> https://community.microfocus.com/microfocus/cobol/rm_cobol/w/knowledge_base/3653/how-do-you-right-justify-a-numeric-value
              MOVE Move-Magnitude TO Num-Magnitude
              EVALUATE WS-DIRECTION
              WHEN "L"
                EVALUATE V-Horizontal
                WHEN 0
                  IF V-Vertical > 0
                    MULTIPLY -1 BY Num-Magnitude GIVING V-Horizontal
                  ELSE
                    SET V-Horizontal TO Num-Magnitude
                  END-IF
                  SET V-Vertical TO 0
                WHEN OTHER
                  IF V-Horizontal > 0
                    SET V-Vertical TO Num-Magnitude
                  ELSE
                    MULTIPLY -1 BY Num-Magnitude GIVING V-Vertical
                  END-IF
                  SET V-Horizontal TO 0
                END-EVALUATE
              WHEN "R"
                EVALUATE V-Horizontal
                WHEN 0
                  IF V-Vertical > 0
                    SET V-Horizontal TO Num-Magnitude
                  ELSE
                    MULTIPLY -1 BY Num-Magnitude GIVING V-Horizontal
                  END-IF
                  SET V-Vertical TO 0
                WHEN OTHER
                  IF V-Horizontal > 0
                    SET V-Vertical TO Num-Magnitude
                    MULTIPLY V-Vertical BY -1 GIVING V-Vertical
                  ELSE
                    SET V-Vertical TO Num-Magnitude
                  END-IF

                  SET V-Horizontal TO 0
                END-EVALUATE
              END-EVALUATE

              ADD V-Horizontal to Horizontal GIVING Horizontal
              ADD V-Vertical to Vertical GIVING Vertical

              IF Horizontal > 0 AND Vertical > 0    
                ADD Horizontal TO Vertical GIVING Manhattan
              END-IF
              IF Horizontal < 0 AND Vertical < 0    
                ADD Horizontal TO Vertical GIVING Manhattan
                MULTIPLY Manhattan BY -1 GIVING Manhattan
              END-IF
              IF Horizontal > 0 AND Vertical < 0    
                SUBTRACT Vertical FROM Horizontal GIVING Manhattan
              END-IF
              IF Horizontal < 0 AND Vertical > 0    
                SUBTRACT Horizontal FROM Vertical GIVING Manhattan
              END-IF

         END-READ
      END-PERFORM.
   CLOSE DIRECTIONS.
   DISPLAY "vh " V-Horizontal " v-v " V-Vertical " H " Horizontal " V " Vertical " M " Manhattan
STOP RUN.
