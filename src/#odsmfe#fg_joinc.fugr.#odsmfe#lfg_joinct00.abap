*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_JOINC................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_JOINC              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_JOINC              .
CONTROLS: TCTRL_/ODSMFE/TB_JOINC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_JOINC              .
TABLES: /ODSMFE/TB_JOINC               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
