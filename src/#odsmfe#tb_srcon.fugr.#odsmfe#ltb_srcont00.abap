*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_SRCON................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_SRCON              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_SRCON              .
CONTROLS: TCTRL_/ODSMFE/TB_SRCON
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_SRCON              .
TABLES: /ODSMFE/TB_SRCON               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
