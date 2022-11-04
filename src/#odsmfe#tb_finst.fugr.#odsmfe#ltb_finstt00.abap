*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FINST................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FINST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FINST              .
CONTROLS: TCTRL_/ODSMFE/TB_FINST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FINST              .
TABLES: /ODSMFE/TB_FINST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
