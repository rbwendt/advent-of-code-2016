IDENTIFICATION DIVISION.
PROGRAM-ID. Advent-2016-Day-1.
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
   01 WS-EOF PIC A(1).

PROCEDURE DIVISION.
   OPEN INPUT DIRECTIONS.
      PERFORM UNTIL WS-EOF='Y'
         READ DIRECTIONS INTO WS-DIRECTIONS
            AT END MOVE 'Y' TO WS-EOF
            NOT AT END
              DISPLAY WS-DIRECTIONS
              DISPLAY WS-MAGNITUDE
              SET Raw-Magnitude to WS-MAGNITUDE
              *> INSPECT WS-MAGNITUDE REPLACING ALL SPACES BY ''
              DISPLAY Raw-Magnitude
              MOVE Move-Magnitude TO Num-Magnitude
              DISPLAY Num-Magnitude
              EVALUATE WS-DIRECTION
              WHEN "L"
                *> DISPLAY "LEFT"
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
                *> DISPLAY "RIGHT"
                EVALUATE V-Horizontal
                WHEN 0
                  *> DISPLAY "right and horizontal zero"
                  IF V-Vertical > 0
                    SET V-Horizontal TO Num-Magnitude
                    *> DISPLAY "setting " Num-Magnitude " " V-Horizontal
                  ELSE
                    MULTIPLY -1 BY Num-Magnitude GIVING V-Horizontal
                  END-IF
                  SET V-Vertical TO 0
                WHEN OTHER
                  *> DISPLAY "right and horizontal non zero " V-Horizontal
                  IF V-Horizontal > 0
                    *> display "+ve horixx"
                    *> display "v vert " V-Vertical
                    SET V-Vertical TO Num-Magnitude
                    MULTIPLY V-Vertical BY -1 GIVING V-Vertical
                    *> display "v vert " V-Vertical
                  ELSE
                    *> display "-ve horixx"
                    SET V-Vertical TO Num-Magnitude
                  END-IF
                  *> DISPLAY "now v-vertical - " V-Vertical " - should be "
                  *> DISPLAY "related to mag - " Num-Magnitude
                  SET V-Horizontal TO 0
                END-EVALUATE
              END-EVALUATE
              *> DISPLAY "Horizontal " Horizontal " vhorizni" V-Horizontal
              *> DISPLAY "Vertical " Vertical " v vertical " V-Vertical
              ADD V-Horizontal to Horizontal GIVING Horizontal
              ADD V-Vertical to Vertical GIVING Vertical
              *> DISPLAY "Horizontal " Horizontal " vhorizni" V-Horizontal
              *> DISPLAY "Vertical " Vertical " v vertical " V-Vertical

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

              DISPLAY "vh " V-Horizontal " v-v " V-Vertical " H " Horizontal " V " Vertical " M " Manhattan
         END-READ
      END-PERFORM.
   CLOSE DIRECTIONS.
STOP RUN.
