*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/TB_MDP..................................*
DATA:  BEGIN OF STATUS_/ODSMFE/TB_MDP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/TB_MDP                .
CONTROLS: TCTRL_/ODSMFE/TB_MDP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/TB_MDP                .
TABLES: /ODSMFE/TB_MDP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
