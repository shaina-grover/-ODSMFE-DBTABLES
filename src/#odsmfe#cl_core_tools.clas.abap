class /ODSMFE/CL_CORE_TOOLS definition
  public
  final
  create public .

public section.

  class-methods CHECK_SOFTWARE_COMPONENT
    importing
*!IM_BUFFERED type SUGU_PARA-XBFLAG default 'X'
*     " !IM_COMPONENT type DLVUNIT  default 'ODS'
      !IM_BUFFERED type abap_boolean default 'X' " " changed SUGU_PARA-XBFLAG to abap_boolean by Priyanka
      !IM_COMPONENT type /odsmfe/st_cvers_sdu-component default 'ODS'  " changed DLVUNIT to /odsmfe/st_cvers_sdu-component by Priyanka
    exporting
      !EX_EXISTS type abap_boolean
    RAISING
      cx_rfc_dest_provider_error .
protected section.
private section.

ENDCLASS.



CLASS /ODSMFE/CL_CORE_TOOLS IMPLEMENTATION.


 METHOD check_software_component.
************************************************************************
* Data Declaration Section
************************************************************************

* Tables & Structures
 "DATA: lt_comp TYPE TABLE OF cvers_sdu,
 "       ls_comp TYPE cvers_sdu.

*SOC By Priyanka
    DATA: lt_comp TYPE TABLE OF /odsmfe/st_cvers_sdu,
          ls_comp TYPE /odsmfe/st_cvers_sdu.
*EOC By Priyanka

************************************************************************
* Main Section
************************************************************************
*SOC By Priyanka

 data: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

   call method lr_rfc->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

*EOC By Priyanka
    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
    DESTINATION lv_rfc
      EXPORTING
        iv_language      = sy-langu
        iv_buffered      = im_buffered
      TABLES
        tt_comptab       = lt_comp
      EXCEPTIONS
        no_release_found = 1
        OTHERS           = 2.

    READ TABLE lt_comp INTO ls_comp WITH KEY component = im_component.
    IF sy-subrc = 0.
      ex_exists = abap_true.
    ELSE.
      ex_exists = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
