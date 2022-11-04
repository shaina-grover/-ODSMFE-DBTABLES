class /ODSMFE/CL_CORE_TOOLS definition
  public
  final
  create public .

public section.

  class-methods CHECK_SOFTWARE_COMPONENT
    importing
      !IM_BUFFERED type SUGU_PARA-XBFLAG default 'X'
      !IM_COMPONENT type DLVUNIT default 'ODS'
    exporting
      !EX_EXISTS type BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_CORE_TOOLS IMPLEMENTATION.


  METHOD check_software_component.
************************************************************************
* Data Declaration Section
************************************************************************
* Tables & Structures
    DATA: lt_comp TYPE TABLE OF cvers_sdu,
          ls_comp TYPE cvers_sdu.
************************************************************************
* Main Section
************************************************************************
    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
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
