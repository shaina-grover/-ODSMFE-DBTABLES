*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_CVVAL................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_CVVAL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_CVVAL              .
CONTROLS: TCTRL_/ODSMFE/TB_CVVAL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_CVVAL              .
TABLES: /ODSMFE/TB_CVVAL               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
