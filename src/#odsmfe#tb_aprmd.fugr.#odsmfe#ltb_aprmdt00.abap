*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_APRMD................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_APRMD              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_APRMD              .
CONTROLS: TCTRL_/ODSMFE/TB_APRMD
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_APRMD              .
TABLES: /ODSMFE/TB_APRMD               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
