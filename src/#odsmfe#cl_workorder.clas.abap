class /ODSMFE/CL_WORKORDER definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_WORKORDER .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_WORKORDER .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_WORKORDER IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Displays Work order data in service to fetch forms
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    DATA: lst_key_tab       TYPE /iwbep/s_mgw_name_value_pair,
          lo_filter         TYPE  REF TO /iwbep/if_mgw_req_filter,
          lrs_filter_values TYPE /odsmfe/st_workorder_fil_vals,
          lv_aufnr          TYPE aufnr,
          lrs_workorder     TYPE /odsmfe/st_core_range_str,
          lit_return        TYPE /iwbep/t_mgw_select_option.


    DATA: lit_select_options TYPE /iwbep/t_cod_select_options,
          lst_select_options TYPE /iwbep/s_cod_select_option.
*    field-SYMBOLS
    FIELD-SYMBOLS : <lfsst_ls_return> TYPE /iwbep/s_mgw_select_option.
* constants
    CONSTANTS: lc_workordernumber TYPE string VALUE 'WorkOrderNumber',
               lc_ordertype       TYPE string VALUE 'OrderType'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF im_tech_request_context IS SUPPLIED.
      REFRESH : lit_return, lit_select_options.
      lo_filter = im_tech_request_context->get_filter( ).
      lit_return = lo_filter->get_filter_select_options( ).
      READ TABLE  lit_return ASSIGNING <lfsst_ls_return> INDEX 1.

      IF <lfsst_ls_return>-property IS ASSIGNED.
        lit_select_options =  <lfsst_ls_return>-select_options.
        IF lit_select_options IS NOT INITIAL.
          CLEAR: lrs_filter_values.
          LOOP AT lit_select_options INTO lst_select_options.

            IF lst_select_options-low IS NOT INITIAL.
              lv_aufnr = lst_select_options-low.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lv_aufnr
                IMPORTING
                  output = lv_aufnr.

              lrs_workorder-sign = text-002.
              lrs_workorder-option = text-003.
              lrs_workorder-low = lv_aufnr.
              APPEND lrs_workorder TO lrs_filter_values-aufnr[].
              CLEAR: lst_select_options, lv_aufnr.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
* Maps key fields to function module parameters
    IF im_key_tab IS NOT INITIAL.
      READ TABLE im_key_tab INTO lst_key_tab WITH KEY name = text-001.
      IF sy-subrc = 0 AND lst_key_tab-value IS NOT INITIAL.
        lv_aufnr = lst_key_tab-value.

        IF lv_aufnr IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_aufnr
            IMPORTING
              output = lv_aufnr.
        ENDIF.
        lrs_workorder-sign = text-002.
        lrs_workorder-option = text-003.
        lrs_workorder-low = lv_aufnr.
        APPEND lrs_workorder TO lrs_filter_values-aufnr[].
      ENDIF.
    ENDIF.
    SELECT DISTINCT aufnr auart ktext
           FROM aufk
           INTO TABLE gitib_entity
           WHERE aufnr IN lrs_filter_values-aufnr[].
    IF sy-subrc NE 0.
      CLEAR gitib_entity.
    ENDIF.
    IF im_key_tab IS NOT INITIAL.
      READ TABLE gitib_entity INTO gstib_entity INDEX 1.
      GET REFERENCE OF gstib_entity INTO ex_entity.
    ELSE.
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
