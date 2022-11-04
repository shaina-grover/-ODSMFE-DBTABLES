*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_UROLE................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_UROLE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_UROLE              .
CONTROLS: TCTRL_/ODSMFE/TB_UROLE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_UROLE              .
TABLES: /ODSMFE/TB_UROLE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
