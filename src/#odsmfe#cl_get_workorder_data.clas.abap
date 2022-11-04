class /ODSMFE/CL_GET_WORKORDER_DATA definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ltys_tech ,
              pernr TYPE pernr_d,
              usrid TYPE sysid,
            END OF ltys_tech .
  types:
    BEGIN OF ltys_usrid ,
              pernr TYPE pernr_d,
              usrid TYPE /odsmfe/de_createdby,
            END OF ltys_usrid .

  data:
    lit_usrid TYPE TABLE OF ltys_usrid .
  data:
    lit_tech TYPE TABLE OF ltys_tech .

  methods GMIB_GET_WORKORDER_DATA
    importing
      !IM_MOBILEUSER type STRING optional
      !IM_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !IM_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional
      !IM_ENTITY_NAME type STRING optional
    exporting
      !LIT_VALID_WO type /ODSMFE/PM_VALID_AUFNR_TAB
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GMIB_GET_TECHNICIAN_DATA
    importing
      !IM_MOBILEUSER type STRING optional
    exporting
      !EX_TECH_DATA like LIT_TECH .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_GET_WORKORDER_DATA IMPLEMENTATION.


  METHOD gmib_get_technician_data.
***********************************************************************************
* Data Declaration
***********************************************************************************
    DATA : lit_pa0105 TYPE TABLE OF pa0105,
           lst_pa0105 TYPE pa0105.

    DATA : lv_mstbr TYPE mstbr,
           lv_parva TYPE xuvalue.

    TYPES : BEGIN OF ltys_pernr,
              pernr TYPE pernr_d,
            END OF ltys_pernr.

    DATA : lit_pa0001 TYPE TABLE OF ltys_pernr,
           lst_pa0001 TYPE ltys_pernr.
***********************************************************************************
* Main Section
***********************************************************************************
* Fetch pernr of supervisor user
    SELECT * FROM pa0105
             INTO CORRESPONDING FIELDS OF TABLE lit_pa0105 "#EC CI_NOFIRST
             WHERE endda GE sy-datum
             AND   begda LE sy-datum
             AND   usrid EQ im_mobileuser.

    IF sy-subrc = 0.
      SORT lit_pa0105 BY pernr.
    ENDIF.

    IF lit_pa0105 IS NOT INITIAL.                         "#EC CI_SUBRC
* Sort & Delete
      SORT lit_pa0105 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lit_pa0105 COMPARING pernr.

      READ TABLE lit_pa0105 INTO lst_pa0105 INDEX 1.
      IF sy-subrc = 0.
        lv_mstbr = lst_pa0105-pernr.
      ENDIF.
* Get Personal details - HR Master Record: Infotype 0001 (Org. Assignment)
      SELECT pernr
        FROM pa0001
        INTO CORRESPONDING FIELDS OF TABLE lit_pa0001   "#EC CI_NOFIELD
        WHERE pa0001~endda GE sy-datum
        AND   pa0001~begda LE sy-datum
        AND   pa0001~mstbr EQ lv_mstbr.

      IF sy-subrc = 0.
        SORT lit_pa0001 BY pernr.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_mstbr
        IMPORTING
          output = lv_mstbr.

      SELECT pernr
        FROM pa0001
        APPENDING CORRESPONDING FIELDS OF TABLE lit_pa0001
        WHERE pa0001~endda GE sy-datum
        AND   pa0001~begda LE sy-datum
        AND   pa0001~mstbr EQ lv_mstbr.

    ENDIF.
    lst_pa0001-pernr = lv_mstbr.
    APPEND lst_pa0001 TO lit_pa0001.
    CLEAR lst_pa0001.
    IF lit_pa0001 IS NOT INITIAL.

      SELECT pernr usrid FROM pa0105
                         INTO TABLE lit_tech
                         FOR ALL ENTRIES IN lit_pa0001
                         WHERE pernr = lit_pa0001-pernr.
    ENDIF.

    ex_tech_data[] = lit_tech[].

  ENDMETHOD.


  METHOD gmib_get_workorder_data.
***********************************************************************************
* Data Declaration
***********************************************************************************
* Object reference
    DATA: lo_filter    TYPE REF TO /iwbep/if_mgw_req_filter,
          lo_ref       TYPE REF TO object,
          lv_classname TYPE seoclsname.

    DATA:lst_filter_range TYPE /iwbep/s_cod_select_option,
         lst_filter       TYPE /iwbep/s_mgw_select_option,
         lv_mobileuser    TYPE string,
         lv_mobile_app    TYPE /syclo/core_mobile_app_dte,
         lv_exchobj       TYPE /odsmfe/exchobj_dte,
         lv_component     TYPE boolean,
         lv_delta_token   TYPE timestamp.

    DATA: lo_data_provider TYPE REF TO /odsmfe/if_get_entityset_main.

    DATA: lit_workorder_header      TYPE STANDARD TABLE OF /odsmfe/cs_caufv_str,
          lst_workorder_header      TYPE /odsmfe/cs_caufv_str,
          lst_valid_wos             TYPE /odsmfe/pm_valid_aufnr_str,
          lit_valid_wos             TYPE STANDARD TABLE OF /odsmfe/pm_valid_aufnr_str,
          lit_workorder_header_temp TYPE STANDARD TABLE OF /odsmfe/cs_caufv_str,
          lst_workorder_header_temp TYPE /odsmfe/cs_caufv_str,
          lst_workorder_headers     TYPE /odsmfe/cs_caufv_str,
          lit_workorder_headers     TYPE STANDARD TABLE OF /odsmfe/cs_caufv_str.

    DATA: lo_delta_context TYPE REF TO /iwbep/if_mgw_req_entityset,
          lo_ref_exch_data TYPE REF TO data.

* Constants
    CONSTANTS: lc_tzone TYPE tznzonesys VALUE 'UTC',
               lc_i     TYPE string VALUE 'I',
               lc_u     TYPE string VALUE 'U'.

* Field Symbols
    FIELD-SYMBOLS: <lfsit_delta_tab> TYPE STANDARD TABLE,
                   <lfs_wo>          LIKE lit_valid_wos,
                   <lfsst_delta_str> TYPE /odsmfe/tb_wo_ex.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*--Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   INTO lv_mobile_app
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME'.

*--Get Exchange Object Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   INTO lv_exchobj
                   WHERE entitysetname = 'Wo_Exchange'
                   AND field = 'EXCHANGE_OBJECT_NAME'.

    IF im_tech_request_context IS SUPPLIED.
      lo_filter = im_tech_request_context->get_filter( ).
    ENDIF.

    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO lst_filter.
        TRANSLATE lst_filter-property TO UPPER CASE.
        CASE lst_filter-property.
          WHEN text-003."EnteredBy
            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
              lv_mobileuser = lst_filter_range-low.
              TRANSLATE lv_mobileuser TO UPPER CASE.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF.

* Incoming delta token
    lo_delta_context ?= im_tech_request_context.
    lv_delta_token = lo_delta_context->get_deltatoken( ).

    DATA: o_ref TYPE REF TO object.
    lv_classname = /odsmfe/cl_exchmechwo=>wo_classname_get( ).
    TRANSLATE lv_classname TO UPPER CASE.
    CREATE OBJECT o_ref TYPE (lv_classname).

* Fetch Work Order Header Details
    IF  lv_mobileuser IS NOT INITIAL AND lv_delta_token IS INITIAL.

      CALL METHOD O_REF->('GET_WORKORDER_COMMON')
        EXPORTING
          iv_mobileuser = lv_mobileuser.

      ASSIGN ('O_REF->GT_AUFNR_DELTA1[]') TO <lfs_wo>.
      lit_valid_wos[] = <lfs_wo>.
    ENDIF.
* Determine delta objects
    IF lv_delta_token IS NOT INITIAL.

      CALL METHOD /smfnd/cl_core_exobj_tools=>determine_delta_objkeys
        EXPORTING
          iv_mobile_app  = lv_mobile_app
          iv_exchobj     = lv_exchobj
          iv_time_token  = lv_delta_token
          iv_time_zone   = lc_tzone
        IMPORTING
          eref_exch_data = lo_ref_exch_data.

      IF lo_ref_exch_data IS NOT INITIAL.
        ASSIGN lo_ref_exch_data->* TO <lfsit_delta_tab>.
        LOOP AT <lfsit_delta_tab> ASSIGNING <lfsst_delta_str>.
          IF <lfsst_delta_str>-action = lc_i OR <lfsst_delta_str>-action = lc_u.
            lst_workorder_header-aufnr = <lfsst_delta_str>-objkey.
            APPEND lst_workorder_header TO lit_workorder_header_temp.
            CLEAR lst_workorder_header.
          ENDIF.
        ENDLOOP.

        LOOP AT  lit_workorder_header_temp  INTO lst_workorder_header_temp .
          MOVE-CORRESPONDING lst_workorder_header_temp TO lst_workorder_headers .
          APPEND lst_workorder_headers  TO lit_workorder_headers .
          CLEAR : lst_workorder_header_temp ,lst_workorder_headers .
        ENDLOOP.

        IF lit_workorder_headers IS NOT INITIAL.
* Fetch Work Order Header Details
          CALL METHOD O_REF->('GET_WORKORDER_COMMON')
            EXPORTING
              iv_mobileuser       = lv_mobileuser
              it_workorder_header = lit_workorder_headers.

          ASSIGN ('O_REF->GT_AUFNR_DELTA1[]') TO <lfs_wo>.
          lit_valid_wos[] = <lfs_wo>.

        ENDIF.
      ENDIF.
    ENDIF.

* Final Table
    lit_valid_wo[] = lit_valid_wos[].

  ENDMETHOD.
ENDCLASS.
