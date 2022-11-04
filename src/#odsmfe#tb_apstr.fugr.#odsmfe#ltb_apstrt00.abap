*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_APSTR................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_APSTR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_APSTR              .
CONTROLS: TCTRL_/ODSMFE/TB_APSTR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_APSTR              .
TABLES: /ODSMFE/TB_APSTR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
