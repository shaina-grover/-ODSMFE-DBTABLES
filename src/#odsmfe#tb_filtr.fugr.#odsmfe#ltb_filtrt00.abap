*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FILTR................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FILTR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FILTR              .
CONTROLS: TCTRL_/ODSMFE/TB_FILTR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FILTR              .
TABLES: /ODSMFE/TB_FILTR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
