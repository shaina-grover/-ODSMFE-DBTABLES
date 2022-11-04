*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_DEPTM................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_DEPTM              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_DEPTM              .
CONTROLS: TCTRL_/ODSMFE/TB_DEPTM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_DEPTM              .
TABLES: /ODSMFE/TB_DEPTM               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
