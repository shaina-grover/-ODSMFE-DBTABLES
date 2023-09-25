class /ODSMFE/CL_GET_WORKORDER_DATA definition
  public
*  final
   INHERITING FROM /odsmfe/cl_get_ent_super_bapi
  create public .

public section.

  types:
    BEGIN OF ltys_tech ,
               pernr TYPE n LENGTH 8,
              usrid TYPE SYuname,
            END OF ltys_tech .
  types:
    BEGIN OF ltys_usrid ,
              pernr TYPE c LENGTH 8,
              usrid TYPE c LENGTH 30,
            END OF ltys_usrid .

   TYPES : BEGIN OF ltys_pa0105,
           pernr TYPE c LENGTH 8,

           END OF ltys_PA0105.

   DATA: lit_pa0105 TYPE TABLE of ltys_pa0105,
         lst_pa0105 TYPE ltys_pa0105.

  data:
    lit_usrid TYPE TABLE OF ltys_usrid .
  data:
    lit_tech TYPE TABLE OF ltys_tech .

   TYPES:
      BEGIN OF ty_data,
        wa(512) TYPE c,
      END OF ty_data.

    TYPES: BEGIN OF ty_options,
             text(72) TYPE c,
           END OF ty_options.
    TYPES: BEGIN OF ty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF ty_fields.

    DATA: lt_options TYPE TABLE OF ty_options,
          lt_fields  TYPE TABLE OF ty_fields,
          lt_data    TYPE TABLE OF ty_data.

     DATA: lv_rowskip  TYPE int4,
          lv_rowcount TYPE int4.

  methods GMIB_GET_WORKORDER_DATA
    importing
      !IM_MOBILEUSER type STRING optional
      !im_filter_select_options TYPE if_rap_query_filter=>tt_name_range_pairs OPTIONAL
      !IM_TECH_REQUEST_CONTEXT type ref to if_rap_query_request OPTIONAL
      !IM_ENTITY_NAME type STRING optional
*      !IM_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
*      !IM_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional

    exporting
      !LIT_VALID_WO type /ODSMFE/PM_VALID_AUFNR_TAB
    raising
    cx_rap_query_prov_not_impl
      cx_rap_query_provider
      cx_rap_query_filter_no_range
      cx_rfc_dest_provider_error.
*      /IWBEP/CX_MGW_TECH_EXCEPTION
*      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GMIB_GET_TECHNICIAN_DATA
    importing
      !IM_MOBILEUSER type STRING optional
      !im_request TYPE REF TO if_rap_query_request OPTIONAL

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
*    DATA : lit_pa0105 TYPE TABLE OF pa0105,
*           lst_pa0105 TYPE pa0105.

    DATA : lv_mstbr TYPE c LENGTH 8,
           lv_parva TYPE c LENGTH 40.

    TYPES : BEGIN OF ltys_pernr,
              pernr TYPE c LENGTH 8,
            END OF ltys_pernr.

    DATA : lit_pa0001 TYPE TABLE OF ltys_pernr,
           lst_pa0001 TYPE ltys_pernr.
***********************************************************************************
* Main Section
***********************************************************************************
* Fetch pernr of supervisor user
*    SELECT * FROM pa0105
*             INTO CORRESPONDING FIELDS OF TABLE @lit_pa0105 "#EC CI_NOFIRST
*             WHERE endda GE @sy-datum
*             AND   begda LE @sy-datum
*             AND   usrid EQ @im_mobileuser.

*     data: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.
*
*            CREATE OBJECT lr_rfc
*              EXPORTING
*                im_entity_name  = im_entity_name
*
*              .
*





   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

      "DATA(lv_top)     = im_request->get_paging( )->get_page_size( ).
     " DATA(lv_skip)    = im_request->get_paging( )->get_offset( ).

       lt_fields = VALUE #( ( fieldname = 'PERNR' ) ).
       lt_options = VALUE #( ( text = |ENDDA| & | | & |EQ| & | | & |'| & |{ sy-datum }| & |'|  ) ).
        DATA(lv_and) = 'AND'.
        lt_options = VALUE #( ( text = |BEGDA| & | | & |EQ| & | | & |'| & |{ sy-datum }| & |'|  ) ).
         lv_and = 'AND'.
         lt_options = VALUE #( ( text = |USRID| & | | & |EQ| & | | & |'| & |{ im_mobileuser }| & |'|  ) ).
          lv_and = 'AND'.
        lv_and = 'AND'.

*       lv_rowskip = lv_Skip.
*      IF lv_top > 0.
*        lv_rowcount = 1.
*      ENDIF.

      "Call RFC to get work orders
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'PA0105'
          "rowskips    = lv_rowskip
          "rowcount    = lv_rowcount
        TABLES
          options     = lt_options
          fields      = lt_fields
          data        = lt_data.

      Loop at lt_data INTO data(ls_data).
         ls_data-wa = lst_pa0105-pernr.
         APPEND lst_pa0105 to lit_pa0105.
         clear lst_pa0105.

      ENDLOOP.
     clear lt_data.
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
*      SELECT pernr
*        FROM pa0001
*        INTO CORRESPONDING FIELDS OF TABLE @lit_pa0001   "#EC CI_NOFIELD
*        WHERE pa0001~endda GE @sy-datum
*        AND   pa0001~begda LE @sy-datum
*        AND   pa0001~mstbr EQ @lv_mstbr.

      call method ME->get_cloud_dest
    IMPORTING
      ex_dest = lv_rfc.

      "lv_top     = im_request->get_paging( )->get_page_size( ).
      "lv_skip    = im_request->get_paging( )->get_offset( ).

       lt_fields = VALUE #( ( fieldname = 'PERNR' ) ).
       lt_options = VALUE #( ( text = |ENDDA| & | | & |EQ| & | | & |'| & |{ sy-datum }| & |'|  ) ).
        lv_and = 'AND'.
        lt_options = VALUE #( ( text = |BEGDA| & | | & |EQ| & | | & |'| & |{ sy-datum }| & |'|  ) ).
         lv_and = 'AND'.
         lt_options = VALUE #( ( text = |MSTBR| & | | & |EQ| & | | & |'| & |{ lv_mstbr }| & |'|  ) ).
          lv_and = 'AND'.
        lv_and = 'AND'.

*       lv_rowskip = lv_Skip.
*      IF lv_top > 0.
*        lv_rowcount = 1.
*      ENDIF.

      "Call RFC to get work orders
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'PA0001'
          rowskips    = lv_rowskip
          rowcount    = lv_rowcount
        TABLES
          options     = lt_options
          fields      = lt_fields
          data        = lt_data.

       Loop at lt_data INTO ls_data.
         ls_data-wa = lst_pa0001-pernr.
         APPEND lst_pa0001 to lit_pa0001.
         clear lst_pa0001.
      ENDLOOP.

      clear lt_data.

      IF sy-subrc = 0.
        SORT lit_pa0001 BY pernr.
      ENDIF.

      lv_mstbr = |{ lv_mstbr ALPHA = OUT }|.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      DESTINATION lv_rfc
*        EXPORTING
*          input  = lv_mstbr
*        IMPORTING
*          output = lv_mstbr.

*      SELECT pernr
*        FROM pa0001
*        APPENDING CORRESPONDING FIELDS OF TABLE @lit_pa0001
*        WHERE pa0001~endda GE @sy-datum
*        AND   pa0001~begda LE @sy-datum
*        AND   pa0001~mstbr EQ @lv_mstbr.

    ENDIF.
    lst_pa0001-pernr = lv_mstbr.
    APPEND lst_pa0001 TO lit_pa0001.
    CLEAR lst_pa0001.
    "clear: lv_skip, lv_rowskip, lv_top, lv_rowcount, lv_and.


    IF lit_pa0001 IS NOT INITIAL.

     call method ME->get_cloud_dest
    IMPORTING
      ex_dest = lv_rfc.

*      lv_top     = im_request->get_paging( )->get_page_size( ).
*      lv_skip    = im_request->get_paging( )->get_offset( ).

      lt_fields = VALUE #( ( fieldname = 'PERNR' )
                            ( fieldname = 'USRID' ) ).

      READ TABLE lit_pa0001 INTO lst_pa0001 INDEX 1.


      lt_options = VALUE #( ( text = |PERNR| & | | & |EQ| & | | & |'| & |{ lst_pa0001-pernr  }| & |'|  ) ).

*       lv_rowskip = lv_Skip.
*      IF lv_top > 0.
*        lv_rowcount = 1.
*      ENDIF.

      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'PA0105'
          "rowskips    = lv_rowskip
          "rowcount    = lv_rowcount
        TABLES
          options     = lt_options
          fields      = lt_fields
          data        = lt_data.

      Loop at lt_data INTO ls_data.
         ls_data-wa = lst_pa0105-pernr.
         APPEND lst_pa0105 to lit_tech.
         clear lst_pa0105.
      ENDLOOP.


*      SELECT pernr, usrid FROM pa0105
*                         INTO TABLE @lit_tech
*                         FOR ALL ENTRIES IN @lit_pa0001
*                         WHERE pernr = @lit_pa0001-pernr.
    ENDIF.

    ex_tech_data[] = lit_tech[].

  ENDMETHOD.


  METHOD gmib_get_workorder_data.
***********************************************************************************
* Data Declaration
***********************************************************************************
* Object reference
    DATA: "lo_filter    TYPE REF TO /iwbep/if_mgw_req_filter,
          lo_ref       TYPE REF TO object,
          lv_classname TYPE c LENGTH 30.                       "seoclsname.

    DATA:"lst_filter_range TYPE /iwbep/s_cod_select_option,
         "lst_filter       TYPE /iwbep/s_mgw_select_option,
         lv_mobileuser    TYPE string,
         "lv_mobile_app    TYPE /syclo/core_mobile_app_dte,
         lv_exchobj       TYPE /odsmfe/exchobj_dte,
         lv_component     TYPE abap_boolean,
         lv_delta_token   TYPE timestamp.

    DATA: lo_data_provider TYPE REF TO /odsmfe/if_get_entityset_main.
            DATA: lo_filter  TYPE REF TO if_rap_query_filter,
          lit_return TYPE  if_rap_query_filter=>tt_name_range_pairs.

    DATA: lit_workorder_header      TYPE STANDARD TABLE OF  /odsmfe/cs_caufv_str,
          lst_workorder_header      TYPE /odsmfe/cs_caufv_str,
          lst_valid_wos             TYPE /odsmfe/pm_valid_aufnr_str,
          lit_valid_wos             TYPE STANDARD TABLE OF /odsmfe/pm_valid_aufnr_str,
          lit_workorder_header_temp TYPE STANDARD TABLE OF /odsmfe/cs_caufv_str,
          lst_workorder_header_temp TYPE /odsmfe/cs_caufv_str,
          lst_workorder_headers     TYPE /odsmfe/cs_caufv_str,
          lit_workorder_headers     TYPE STANDARD TABLE OF /odsmfe/cs_caufv_str,
          lit_aufnr TYPE STANDARD TABLE OF /odsmfe/tb_aufnr.

    DATA: lo_delta_context TYPE REF TO if_rap_query_request,             "/iwbep/if_mgw_req_entityset,
          lo_ref_exch_data TYPE REF TO data.

* Constants
    CONSTANTS: lc_tzone TYPE c LENGTH 6  VALUE 'UTC',                            " tznzonesys
               lc_i     TYPE string VALUE 'I',
               lc_u     TYPE string VALUE 'U',
               lc_createdby TYPE string VALUE 'CREATEDBY'.

* Field Symbols
    FIELD-SYMBOLS: <lfsit_delta_tab> TYPE STANDARD TABLE,
                   <lfs_wo>          LIKE lit_valid_wos,
                   <lfsst_delta_str> TYPE /odsmfe/tb_wo_ex.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*--Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME'
                   INTO @data(lv_mobile_app).

*--Get Exchange Object Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   WHERE entitysetname = 'Wo_Exchange'
                   AND field = 'EXCHANGE_OBJECT_NAME'
                   INTO @lv_exchobj.

*    IF im_tech_request_context IS SUPPLIED.
*      lo_filter = im_tech_request_context->get_filter( ).
*    ENDIF.

    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO data(lst_filter).
        TRANSLATE lst_filter-name TO UPPER CASE.
        CASE lst_filter-name.
          WHEN  lc_createdby.
            READ TABLE lst_filter-range INTO data(lst_filter_range) INDEX 1.
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
" Commented now for checking the service need to check again
*   lo_delta_context ?= im_tech_request_context.
*    lv_delta_token = lo_delta_context->get_search_expression(  ).          " get_deltatoken( )

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

*SELECT * FROM /odsmfe/tb_aufnr INTO TABLE @lit_aufnr.
*
*     MOVE-CORRESPONDING lit_aufnr[] to lit_valid_wos[].   "Added for Test purpose
     lit_valid_wos[] = <lfs_wo>.
    ENDIF.
* Determine delta objects
    IF lv_delta_token IS NOT INITIAL.

*      CALL METHOD /smfnd/cl_core_exobj_tools=>determine_delta_objkeys
*        EXPORTING
*          iv_mobile_app  = lv_mobile_app
*          iv_exchobj     = lv_exchobj
*          iv_time_token  = lv_delta_token
*          iv_time_zone   = lc_tzone
*        IMPORTING
*          eref_exch_data = lo_ref_exch_data.

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

*           SELECT * FROM /odsmfe/tb_aufnr INTO TABLE @lit_aufnr.
*
*           MOVE-CORRESPONDING lit_aufnr[] to lit_valid_wos[].  "Added for test purpose

        ENDIF.
      ENDIF.
    ENDIF.

* Final Table
   MOVE-CORRESPONDING lit_valid_wos[] to lit_valid_wo[].

*    lit_valid_wo[] = lit_valid_wos[].

  ENDMETHOD.
ENDCLASS.
