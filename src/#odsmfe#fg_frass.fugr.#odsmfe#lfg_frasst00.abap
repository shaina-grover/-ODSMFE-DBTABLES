*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FRASS................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FRASS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FRASS              .
CONTROLS: TCTRL_/ODSMFE/TB_FRASS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FRASS              .
TABLES: /ODSMFE/TB_FRASS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
