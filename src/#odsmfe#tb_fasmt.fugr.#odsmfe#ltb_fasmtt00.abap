*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FASMT................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FASMT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FASMT              .
CONTROLS: TCTRL_/ODSMFE/TB_FASMT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FASMT              .
TABLES: /ODSMFE/TB_FASMT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
