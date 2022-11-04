*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FOASS................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FOASS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FOASS              .
CONTROLS: TCTRL_/ODSMFE/TB_FOASS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FOASS              .
TABLES: /ODSMFE/TB_FOASS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
