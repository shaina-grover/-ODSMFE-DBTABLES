*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_MSTFL................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_MSTFL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_MSTFL              .
CONTROLS: TCTRL_/ODSMFE/TB_MSTFL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_MSTFL              .
TABLES: /ODSMFE/TB_MSTFL               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
