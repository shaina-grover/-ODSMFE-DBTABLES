"! <p class="shorttext synchronized" lang="en">Abstract Class For Getting the Data Based on Entityset</p>
CLASS /odsmfe/cl_workorder DEFINITION
  PUBLIC
   INHERITING FROM /odsmfe/cl_get_ent_super_bapi
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: tab TYPE STANDARD TABLE OF REF TO /odsmfe/cl_workorder WITH DEFAULT KEY.

    METHODS /odsmfe/if_get_entityset_bapi~gmib_read_entityset
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
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

    DATA:   lt_options TYPE TABLE OF ty_options,
                lt_fields  TYPE TABLE OF ty_fields,
                lt_data    TYPE TABLE OF ty_data.

    DATA: lrs_filter_values TYPE /odsmfe/st_workorder_fil_vals,
              lv_aufnr          TYPE aufnr,
             lrs_workorder     TYPE /odsmfe/st_core_range_str,
            lit_table         TYPE TABLE OF /odsmfe/ce_workorder.

    DATA: lt_workorder  TYPE STANDARD TABLE OF /odsmfe/cds_workorder,
             lit_WoNum     TYPE TABLE of /odsmfe/st_core_range_str,
             lit_wotyp     type TABLE of /odsmfe/st_core_range_str,
             lit_workorder TYPE TABLE OF  /odsmfe/ce_workorder,
            lst_workorder TYPE  /odsmfe/ce_workorder.

    data: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

    DATA: lv_rowskip  TYPE int4,
          lv_rowcount TYPE int4.

    DATA: lo_filter  TYPE REF TO if_rap_query_filter,
               lit_return TYPE  if_rap_query_filter=>tt_name_range_pairs.

* constants
    CONSTANTS: lc_workordernumber TYPE string VALUE 'WORKORDERNUMBER',
               lc_ordertype       TYPE string VALUE 'ORDERTYPE'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

      LOOP AT im_filter_select_options INTO DATA(ls_filter_select_options).
        CASE ls_filter_select_options-name.
          WHEN lc_workordernumber.
            lit_WoNum = CORRESPONDING #( ls_filter_select_options-range ).
            DELETE lit_WoNum WHERE low IS INITIAL.

          WHEN lc_ordertype.
           lit_wotyp = CORRESPONDING #( ls_filter_select_options-range ).
            DELETE lit_wotyp WHERE low IS INITIAL.
        ENDCASE.

      ENDLOOP.

      CREATE OBJECT lr_rfc
        EXPORTING
          im_entity_name           = im_entity_name.

      call METHOD lr_rfc->get_cloud_dest
        IMPORTING
          ex_dest = DATA(lv_rfc).

      DATA(lv_top)     = im_request->get_paging( )->get_page_size( ).
      DATA(lv_skip)    = im_request->get_paging( )->get_offset( ).

      lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                           ( fieldname = 'AUART' )
                           ( fieldname = 'KTEXT' ) ).

      IF lit_WoNum IS NOT INITIAL.
        lt_options = VALUE #( ( text = |AUFNR| & | | & |{ lit_WoNum[ 1 ]-option }| & | | & |'| & |{ lit_WoNum[ 1 ]-low }| & |'|  ) ).
        DATA(lv_and) = 'AND'.
      ENDIF.

      IF lit_wotyp IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |AUART| & | | & |{ lit_wotyp[ 1 ]-option }| & | | & |'| & |{ lit_wotyp[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.

      lv_rowskip = lv_Skip.
      IF lv_top > 0.
        lv_rowcount = lv_top.
      ENDIF.

      "Call RFC to get work orders
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AUFK'
          rowskips    = lv_rowskip
          rowcount    = lv_rowcount
        TABLES
          options     = lt_options
          fields      = lt_fields
          data        = lt_data.

      LOOP AT lt_data INTO DATA(ls_Data).
        lst_workorder-WorkOrderNumber = ls_Data+0(12).
        lst_workorder-OrderType = ls_Data+12(15).
        lst_workorder-Description = ls_Data+16(56).
        APPEND lst_workorder TO lit_workorder.
        CLEAR lst_workorder.
      ENDLOOP.

      MOVE-CORRESPONDING lit_workorder TO ex_response_data.

  ENDMETHOD.
ENDCLASS.
