FUNCTION-POOL /odsmfe/fg_forms_rpt.                                     "MESSAGE-ID ..

DATA: go_dock_container  TYPE REF TO cl_gui_docking_container.         "Docking Control Container
DATA: go_dock_container1 TYPE REF TO cl_gui_docking_container.         "Docking Control Container
DATA: go_alv             TYPE REF TO cl_salv_table.                    "Basisklasse für einfache Tabellen
DATA: go_alv1            TYPE REF TO cl_salv_table.                    "Basisklasse für einfache Tabellen
DATA: go_control         TYPE REF TO /odsmfe/cl_control_rpt.           "Controller

DATA: gv_formid  TYPE /odsmfe/de_formid,                               "ODS Form ID
      gv_uname   TYPE /odsmfe/de_createdby,                            "ODS Created By
      gv_version TYPE /odsmfe/de_version,                              "ODS Version
      gt_date    TYPE /odsmfe/core_range_tab,
      gs_date    LIKE LINE OF gt_date.


INCLUDE lsvimdat                                .                      "General data decl.
