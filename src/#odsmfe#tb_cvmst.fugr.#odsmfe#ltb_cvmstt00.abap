*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_CVMST................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_CVMST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_CVMST              .
CONTROLS: TCTRL_/ODSMFE/TB_CVMST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_CVMST              .
TABLES: /ODSMFE/TB_CVMST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
