*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_APCON................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_APCON              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_APCON              .
CONTROLS: TCTRL_/ODSMFE/TB_APCON
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_APCON              .
TABLES: /ODSMFE/TB_APCON               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
