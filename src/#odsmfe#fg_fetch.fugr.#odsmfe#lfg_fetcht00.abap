*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_FETCH................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_FETCH              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_FETCH              .
CONTROLS: TCTRL_/ODSMFE/TB_FETCH
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_FETCH              .
TABLES: /ODSMFE/TB_FETCH               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
