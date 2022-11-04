*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FMASS................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FMASS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FMASS              .
CONTROLS: TCTRL_/ODSMFE/TB_FMASS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FMASS              .
TABLES: /ODSMFE/TB_FMASS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
