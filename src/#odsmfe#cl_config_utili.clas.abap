class /ODSMFE/CL_CONFIG_UTILI definition
  public
  final
  create private .

public section.
  "type-pools ABAP .

  class-data GV_INST type ref to /ODSMFE/CL_CONFIG_UTILI .

  class-methods GET_INSTANCE
    returning
      value(RO_INST) type ref to /ODSMFE/CL_CONFIG_UTILI .
  methods GET_MOBILE_FILTERS
    importing
      !LV_TABNAME type /ODSMFE/CORE_RANGE_TAB optional
      !LV_ACTIVE type C OPTIONAL
      !LV_ENTITY_SET_NAME type STRING optional
    exporting
      !ET_FILTERS type /ODSMFE/MOBFILTERS_TAB .
  methods GET_TABLE_CONFIG
    importing
      !IV_ENTITY_SET_NAME type STRING optional
      !IT_FILTER_SELECT_OPTIONS type if_rap_query_filter=>tt_name_range_pairs OPTIONAL "/IWBEP/T_MGW_SELECT_OPTION optional
    exporting
      !GV_ACTIVE type STRING .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_CONFIG_UTILI IMPLEMENTATION.


  METHOD GET_INSTANCE.
*----------------------------------------------------------------------*
*<ODSDOC>
*  This Method is used for instantite the class
*</ODSDOC>
*----------------------------------------------------------------------*
* Author     : Madhur Kanungo                                          *
* Date       : 10/05/2016                                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*----------------------------------------------------------------------*
* Date       |Request No| User Id    |Description                      *
*----------------------------------------------------------------------*
* mm/dd/yyyy |          | sap userid |…                                *
*----------------------------------------------------------------------*
************************************************************************
************************************************************************
* Main Section
************************************************************************
    IF gv_inst IS NOT BOUND.
* Create Object
      CREATE OBJECT ro_inst.
      gv_inst = ro_inst.
    ELSE.
      ro_inst = gv_inst.
    ENDIF.
  ENDMETHOD.


  METHOD GET_MOBILE_FILTERS.
*----------------------------------------------------------------------*
*<ODSDOC>
*  This method is used for mobile filter as configuration
*</ODSDOC>
*----------------------------------------------------------------------*
* Author     : Madhur Kanungo                                          *
* Date       : 11/05/2016                                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*----------------------------------------------------------------------*
* Date       |Request No| User Id    |Description                      *
*----------------------------------------------------------------------*
* mm/dd/yyyy |          | sap userid |…                                *
*----------------------------------------------------------------------*
************************************************************************
************************************************************************
* Main Section
************************************************************************
* Get Data Filter Values
    IF et_filters IS INITIAL.
      SELECT * FROM /odsmfe/tb_filtr
        WHERE active = @lv_active
        AND entitysetname = @lv_entity_set_name
        AND tabname IN @lv_tabname  INTO CORRESPONDING FIELDS OF TABLE @et_filters.
      IF sy-subrc = 0.
        SORT  et_filters BY entitysetname tabname.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_table_config.
*----------------------------------------------------------------------*
*<ODSDOC>
*  This method is used a Table configuration
*</ODSDOC>
*----------------------------------------------------------------------*
* Author     : Madhur Kanungo                                          *
* Date       : 11/05/2016                                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*----------------------------------------------------------------------*
* Date       |Request No| User Id    |Description                      *
*----------------------------------------------------------------------*
* mm/dd/yyyy |          | sap userid |…                                *
*----------------------------------------------------------------------*
************************************************************************
************************************************************************
* Main Section
************************************************************************
* Check if table is active or not in table /odsmfe/servic_conf
    DATA: lc_active TYPE c LENGTH 1 VALUE 'X'.

    DATA :" ls_filter       TYPE LINE OF  if_rap_query_filter=>tt_name_range_pairs,"/iwbep/s_mgw_select_option,
        "   ls_filter_range like ls_filter-range, "/iwbep/s_cod_select_option,
           lv_mobileuser   TYPE string,
           lv_roleid       TYPE /odsmfe/roleid.

    IF gv_active IS INITIAL.
* Start of Changes to read ENTEREDBY
      IF it_filter_select_options IS NOT INITIAL.
        READ TABLE it_filter_select_options INTO data(ls_filter) WITH KEY name = 'EnteredBy'.
        IF sy-subrc IS INITIAL.
          "TRANSLATE ls_filter-name TO UPPER CASE.
          READ TABLE ls_filter-range INTO data(ls_filter_range) iNDEX 1.
          IF sy-subrc EQ 0.
            lv_mobileuser = ls_filter_range-low.
          ENDIF.
        ENDIF.
      ENDIF.


      IF lv_mobileuser IS INITIAL.
        lv_mobileuser = sy-uname.
      ENDIF.

      SELECT SINGLE roleid FROM /odsmfe/tb_urole
                           WHERE userid = @lv_mobileuser INTO @lv_roleid.

* End of changes to read Enteredby
      SELECT SINGLE active FROM /odsmfe/tb_srcon
             WHERE active = @lc_active
             AND entityset_name = @iv_entity_set_name
             AND roleid = @lv_roleid INTO @gv_active.

      IF sy-subrc NE 0 .
        CLEAR gv_active.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
