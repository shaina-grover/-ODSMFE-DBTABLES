*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FAPVR................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FAPVR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FAPVR              .
CONTROLS: TCTRL_/ODSMFE/TB_FAPVR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FAPVR              .
TABLES: /ODSMFE/TB_FAPVR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
