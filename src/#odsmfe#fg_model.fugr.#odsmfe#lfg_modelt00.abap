*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_MODEL................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_MODEL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_MODEL              .
CONTROLS: TCTRL_/ODSMFE/TB_MODEL
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_MODEL              .
TABLES: /ODSMFE/TB_MODEL               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
