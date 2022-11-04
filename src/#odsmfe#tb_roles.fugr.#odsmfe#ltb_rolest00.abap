*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_ROLES................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_ROLES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_ROLES              .
CONTROLS: TCTRL_/ODSMFE/TB_ROLES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_ROLES              .
TABLES: /ODSMFE/TB_ROLES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
