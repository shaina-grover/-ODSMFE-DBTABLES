class /ODSMFE/CL_PR_APPSTORE_MPC_EXT definition
  public
  inheriting from /ODSMFE/CL_PR_APPSTORE_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_PR_APPSTORE_MPC_EXT IMPLEMENTATION.


  METHOD define.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :30.06.2021
* Transport No.          :ES1K902703
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*
    DATA: lo_model     TYPE REF TO /iwbep/if_mgw_odata_model,
          lo_mod       TYPE REF TO /iwbep/cl_mgw_odata_model,          "implementation of odata meta data model
          lo_extension TYPE REF TO /odsmfe/cl_model_extension,         "Create Extensions for Model
          lo_project   TYPE REF TO /iwbep/cl_mgw_push_abs_model.       "Abstract Push Model Provider
* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*
* ----------------------------------------------------------------------*
* --- Call the superclass method(this is a must)------------------------*
* ----------------------------------------------------------------------*

    super->define( ).

* ----------------------------------------------------------------------*
* --- Call the extensions.----------------------------------------------*
* ----------------------------------------------------------------------*

    lo_model   = model.
    lo_project = me .

    CREATE OBJECT lo_extension
      EXPORTING
        im_model   = lo_model
        im_project = lo_project.

    lo_mod ?= model.

* ---- Below method will work only if there is data maintained in config table
    lo_extension->/odsmfe/if_model_extension~gmib_process_entity( im_srv_name = lo_mod->/iwbep/if_mgw_odata_re_model~get_schema_namespace( ) ).

  ENDMETHOD.
ENDCLASS.
