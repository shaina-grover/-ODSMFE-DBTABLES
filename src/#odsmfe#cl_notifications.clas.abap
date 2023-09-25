class /ODSMFE/CL_NOTIFICATIONS definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_NOTIFICATIONS IMPLEMENTATION.


    METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS-VSANAGALA
* Creation Date          : 24.02.2023
* Transport No.          : ES1K903619
* Program Description    : Get the Notifications in service to fetch forms
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*---------------------------------------------------------------------*
*                  D A T A    D E C L A R A T I O N                   *
*---------------------------------------------------------------------*

     "/Types
     TYPES: BEGIN OF ltys_data,
                    wa(512) TYPE c,
                 END OF ltys_data,

                 BEGIN OF ltys_options,
                    text(72) TYPE c,
                 END OF ltys_options,

                 BEGIN OF ltys_fields,
                    fieldname(30) TYPE c,
                    offset(6)   TYPE n,
                    length(6) TYPE n,
                    type(1) TYPE c,
                    fieldtext(60) TYPE c,
                 END OF ltys_fields.

      "/ Tables and Structures
      DATA: lrt_notification TYPE TABLE OF /odsmfe/st_core_range_str,
                lrt_ordertype     TYPE TABLE OF /odsmfe/st_core_range_str,
                lit_notification   TYPE TABLE OF /odsmfe/ce_notification,
                lst_notification  TYPE /odsmfe/ce_notification,
                lit_options         TYPE TABLE OF ltys_options,
                lit_fields            TYPE TABLE OF ltys_fields,
                lit_data              TYPE TABLE OF ltys_data.

      DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

      DATA: lv_rowskip  TYPE int4,
                 lv_rowcount TYPE int4.


      "/ Constants
      CONSTANTS: lc_notification TYPE string VALUE 'NOTIFICATION',
                 lc_ordertype    TYPE string VALUE 'ORDERTYPE',
                 lc_notif        TYPE string VALUE 'Notification',
                 lc_i            TYPE c LENGTH 1  VALUE 'I',
                 lc_eq           TYPE c LENGTH 2  VALUE 'EQ'.

*---------------------------------------------------------------------*
*           E N D   O F   D A T A   D E C L A R A T I O N             *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*                    M A I N    S E C T I O N                         *
*---------------------------------------------------------------------*

      "/ Read filter values
      IF im_filter_select_options IS NOT INITIAL.
        LOOP AT im_filter_select_options INTO DATA(lst_filter_select_options).
          TRANSLATE lst_filter_select_options-name TO UPPER CASE.
          CASE lst_filter_select_options-name.

            WHEN lc_notification.
            lrt_notification = CORRESPONDING #( lst_filter_select_options-range ).
            DELETE lrt_notification WHERE low IS INITIAL.

            WHEN lc_ordertype.
              lrt_ordertype = CORRESPONDING #( lst_filter_select_options-range ).
              DELETE lrt_ordertype WHERE low IS INITIAL.

          ENDCASE. "/ CASE lst_filter_select_options-property.
          CLEAR:  lst_filter_select_options.
        ENDLOOP. "/ LOOP AT im_filter_select_options INTO lst_filter_select_options.
      ENDIF. "/ IF im_filter_select_options IS NOT INITIAL.


      CREATE OBJECT lr_rfc
          EXPORTING
            im_entity_name           = im_entity_name .

      CALL METHOD lr_rfc->get_cloud_dest
        IMPORTING
          ex_dest = DATA(lv_rfc) .


      DATA(lv_top)     = im_request->get_paging( )->get_page_size( ).
      DATA(lv_skip)    = im_request->get_paging( )->get_offset( ).

      lit_fields = VALUE #( ( fieldname = 'QMNUM' )
                                        ( fieldname = 'QMART' )
                                        ( fieldname = 'QMTXT' ) ).

     if lrt_notification IS NOT INITIAL.
       lit_options = VALUE #( ( text = |QMNUM| & | | & |{ lrt_notification[ 1 ]-option }| & | | & |'| & |{ lrt_notification[ 1 ]-low }| & |'| ) ).
       DATA(lv_and) = 'AND'.
     endif.

     if lrt_ordertype IS NOT INITIAL.
         APPEND VALUE #( text = |{ lv_and }| & | | & |QMART| & | | & |{ lrt_ordertype[ 1 ]-option }| & | | & |'| & |{ lrt_ordertype[ 1 ]-low }| & |'| )  TO lit_options.
     endif.

      lv_rowskip = lv_Skip.
      IF lv_top > 0.
        lv_rowcount = lv_top.
      ENDIF.

      "/Call RFC to get work orders
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'QMEL'
          rowskips    = lv_rowskip
          rowcount    = lv_rowcount
        TABLES
          options     = lit_options
          fields      = lit_fields
          data        = lit_data.

       LOOP AT lit_data INTO DATA(lst_data).
            lst_notification-Notification = lst_data+0(12).
            lst_notification-OrderType   = lst_data+12(15).
            lst_notification-Description = lst_data+16(56).
            APPEND lst_notification TO lit_notification.
            clear: lst_notification.
       ENDLOOP.

      "/ Mapping the properties to export in the gateway service.
      MOVE-CORRESPONDING lit_notification TO ex_response_data.

    ENDMETHOD.
ENDCLASS.
