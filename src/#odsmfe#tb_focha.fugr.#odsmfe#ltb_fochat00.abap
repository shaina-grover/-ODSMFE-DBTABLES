*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FOCHA................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FOCHA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FOCHA              .
CONTROLS: TCTRL_/ODSMFE/TB_FOCHA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FOCHA              .
TABLES: /ODSMFE/TB_FOCHA               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
