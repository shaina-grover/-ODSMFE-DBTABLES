FUNCTION-POOL /odsmfe/fg_forms_ui.                                     "MESSAGE-ID ..

* INCLUDE /ODSMFE/LFG_FORMS_UID...           " Local class definition
DATA: go_dock_container  TYPE REF TO cl_gui_docking_container.         "Docking Control Container
DATA: go_dock_container1 TYPE REF TO cl_gui_docking_container.         "Docking Control Container
DATA: go_alv             TYPE REF TO cl_salv_table.                    "Basisklasse für einfache Tabellen
DATA: go_alv1            TYPE REF TO cl_salv_table.                    "Basisklasse für einfache Tabellen
DATA: go_control         TYPE REF TO /odsmfe/cl_control.               "Controller



INCLUDE lsvimdat                                .                      "general data decl.
*INCLUDE /odsmfe/lfg_forms_uidt00                . "view rel. data dcl.
