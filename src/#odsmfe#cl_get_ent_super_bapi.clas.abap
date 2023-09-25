CLASS /odsmfe/cl_get_ent_super_bapi DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
*  type-pools ABAP .

    INTERFACES /odsmfe/if_get_entityset_main .
    INTERFACES /odsmfe/if_get_entityset_bapi .

    METHODS constructor
      IMPORTING
        !im_entity_name           TYPE string
        !im_entity_set_name       TYPE string OPTIONAL
        !im_request               TYPE REF TO if_rap_query_request OPTIONAL
        !im_service_document_name TYPE REF TO string OPTIONAL
        !im_service_version       TYPE REF TO numc4 OPTIONAL
        !im_service_namespace     TYPE REF TO string OPTIONAL.

    METHODS get_cloud_dest
      EXPORTING
        ex_dest TYPE rfcdest.

  PROTECTED SECTION.

*  data GOIO_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT .
*  data GOIO_REQUEST_DETAILS type ref to /IWBEP/IF_MGW_CORE_SRV_RUNTIME=>TY_S_MGW_REQUEST_CONTEXT .
    DATA goio_service_document_name TYPE REF TO string .
    DATA goio_service_version TYPE REF TO numc4 .
    DATA goio_service_namespace TYPE REF TO string .
*  data GOIO_INJECTION type ref to /IWBEP/IF_SB_GEN_DPC_INJECTION .
    DATA gvio_http_method TYPE string .


  PRIVATE SECTION.
ENDCLASS.



CLASS /ODSMFE/CL_GET_ENT_SUPER_BAPI IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_bapi_transaction_commit.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    :
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

    DATA: lst_return TYPE bapiret2,
          lv_message TYPE bapi_msg.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
    CLEAR lst_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = lst_return.
* Error handling
    IF lst_return-type EQ 'E' OR lst_return-type EQ 'A'.
      lv_message = lst_return-message.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid  = /iwbep/cx_mgw_busi_exception=>business_error
*          message = lv_message.
    ENDIF.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_delete_entityset.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :
* Transport No.          : ES1K901528
* Program Description    :
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
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


    CHECK sy-subrc EQ 0.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_error_handling.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :
* Transport No.          : ES1K901528
* Program Description    :
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
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


    CHECK sy-subrc EQ 0.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :
* Transport No.          : ES1K901528
* Program Description    :
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
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


    CHECK sy-subrc EQ 0.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    :
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
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


    CHECK sy-subrc EQ 0.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_get_entityset_data.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    :
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

*
*    FIELD-SYMBOLS: <lfsst_req> TYPE /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context.
**-------------------------------------------------------------
** Main Section
**-------------------------------------------------------------
*
*    ASSIGN goio_request_details->* TO <lfsst_req>.
*
*    CHECK <lfsst_req> IS ASSIGNED.
*
*    CASE <lfsst_req>-http_method.
*
*      WHEN 'POST'.
*        me->/odsmfe/if_get_entityset_bapi~gmib_create_entityset(
*        EXPORTING
*          im_data_ext_class        = me
*          im_entity_name           = im_entity_name
*          im_entity_set_name       = im_entity_set_name
*          im_source_name           = im_source_name
*          im_filter_select_options = im_filter_select_options
*          im_paging                = im_paging
*          im_key_tab               = im_key_tab
*          im_navigation_path       = im_navigation_path
*          im_order                 = im_order
*          im_filter_string         = im_filter_string
*          im_search_string         = im_search_string
*          im_tech_request_context  = im_tech_request_context_ent_c
*          im_data_provider         = im_data_provider
*          im_changeset_request     = im_changeset_request
*        IMPORTING
*          ex_entityset             = ex_entityset
*          ex_changeset_response    = ex_changeset_response
*          ex_response_context      = ex_response_context
*          ex_entity                = ex_entity
*          ex_deep_entity           = ex_deep_entity ).
*
**      WHEN 'GET'.
*        IF im_tech_request_context_entity IS SUPPLIED.

    me->/odsmfe/if_get_entityset_bapi~gmib_read_entityset(
  EXPORTING
     im_entity_name           = im_entity_name
     im_entity_set_name       = im_entity_set_name
     im_filter_select_options = im_filter_select_options
     im_paging                = im_paging
      im_filter_string        = im_filter_string
      im_request              = im_request
    IMPORTING
    ex_response               = ex_response
    ex_response_data          = ex_response_data ).

*        ENDIF.

**        IF im_tech_request_context IS SUPPLIED.
*
**         me->/odsmfe/if_get_entityset_bapi~gmib_read_entityset(
**         EXPORTING
**            im_data_ext_class        = me
**            im_entity_name           = im_entity_name
**           im_entity_set_name       = im_entity_set_name
**           im_source_name           = im_source_name
**           im_filter_select_options = im_filter_select_options
**           im_paging                = im_paging
**            im_key_tab               = im_key_tab
**            im_navigation_path       = im_navigation_path
**            im_order                 = im_order
**            im_filter_string         = im_filter_string
**            im_search_string         = im_search_string
**            im_tech_request_context  = im_tech_request_context
**            im_data_provider         = im_data_provider
**         IMPORTING
**           ex_entityset             = ex_entityset
**           ex_response_context      = ex_response_context
**           ex_entity                = ex_entity
**           ex_deleted_entityset     = ex_deleted_entityset
**           ex_deep_entity           = ex_deep_entity  ).
**       ENDIF.
*
*      WHEN 'PUT'.
*        me->/odsmfe/if_get_entityset_bapi~gmib_modify_entityset(
*        EXPORTING
*          im_data_ext_class        = me
*          im_entity_name           = im_entity_name
*          im_entity_set_name       = im_entity_set_name
*          im_source_name           = im_source_name
*          im_filter_select_options = im_filter_select_options
*          im_paging                = im_paging
*          im_key_tab               = im_key_tab
*          im_navigation_path       = im_navigation_path
*          im_order                 = im_order
*          im_filter_string         = im_filter_string
*          im_search_string         = im_search_string
*          im_tech_request_context  = im_tech_request_context_ent_u
*          im_data_provider         = im_data_provider
*          im_changeset_request     = im_changeset_request
*        IMPORTING
*          ex_entityset             = ex_entityset
*          ex_response_context      = ex_response_context
*          ex_changeset_response    = ex_changeset_response
*          ex_entity                = ex_entity
*          ex_deep_entity           = ex_deep_entity ).
*
*      WHEN 'DELETE'.
*
*        CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_delete_entityset
*          EXPORTING
*            im_filter_select_options      = im_filter_select_options
*            im_paging                     = im_paging
*            im_source_name                = im_source_name
*            im_key_tab                    = im_key_tab
*            im_order                      = im_order
*            im_filter_string              = im_filter_string
*            im_search_string              = im_search_string
*            im_tech_request_context_ent_d = im_tech_request_context_ent_d
*            im_data_provider              = im_data_provider
*            im_data_ext_class             = im_data_ext_class
*            im_entity_name                = im_entity_name
*            im_entity_set_name            = im_entity_set_name
*          IMPORTING
*            ex_entityset                  = ex_entityset
*            ex_response_context           = ex_response_context
*            ex_entity                     = ex_entity
*            ex_deep_entity                = ex_deep_entity.
*
*
*      WHEN 'PATCH'.
*        IF gvio_http_method = 'GET'.
*          CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_read_entityset(
*            EXPORTING
*              im_filter_select_options       = im_filter_select_options
*              im_paging                      = im_paging
*              im_source_name                 = im_source_name
*              im_key_tab                     = im_key_tab
*              im_order                       = im_order
*              im_filter_string               = im_filter_string
*              im_search_string               = im_search_string
*              im_tech_request_context_entity = im_tech_request_context_entity
*              im_data_provider               = im_data_provider
*              im_data_ext_class              = me
*              im_entity_name                 = im_entity_name
*              im_entity_set_name             = im_entity_set_name
*              im_navigation_path             = im_navigation_path
*            IMPORTING
*              ex_entityset                   = ex_entityset
*              ex_response_context            = ex_response_context
*              ex_entity                      = ex_entity
*              ex_deep_entity                 = ex_deep_entity ).
*        ENDIF.
*
*        IF gvio_http_method = 'PUT'.
*          TRY.
*              CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_modify_entityset(
*                EXPORTING
*                  im_filter_select_options = im_filter_select_options
*                  im_paging                = im_paging
*                  im_source_name           = im_source_name
*                  im_key_tab               = im_key_tab
*                  im_order                 = im_order
*                  im_filter_string         = im_filter_string
*                  im_search_string         = im_search_string
*                  im_tech_request_context  = im_tech_request_context_ent_u
*                  im_data_provider         = im_data_provider
*                  im_data_ext_class        = me
*                  im_entity_name           = im_entity_name
*                  im_entity_set_name       = im_entity_set_name
*                  im_navigation_path       = im_navigation_path
*                IMPORTING
*                  ex_entityset             = ex_entityset
*                  ex_response_context      = ex_response_context
*                  ex_entity                = ex_entity
*                  ex_deep_entity           = ex_deep_entity ).
*          ENDTRY.
*        ENDIF.
*
*
*      WHEN OTHERS.
*
*    ENDCASE.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_get_entityset_filter_data.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to set filter data
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

    DATA: lo_filter_values TYPE REF TO data,
          lst_filter_val   TYPE /odsmfe/st_core_range_str,
          lit_filter_val   TYPE /odsmfe/tt_core_range_tab,
          lit_filters      TYPE STANDARD TABLE OF /odsmfe/tb_filtr,
          lst_filters      TYPE /odsmfe/tb_filtr.

    FIELD-SYMBOLS: <lfsit_filter_val>    TYPE /odsmfe/tt_core_range_tab,
                   <lfsst_field>         TYPE /odsmfe/tb_filtr,
                   <lfsst_filter_values> TYPE any.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF ex_filter_values IS SUPPLIED.

* Select all the filters from config table for the respective entityset

      SELECT mandt, entitysetname, tabname, field, recordno, field_descr, sign, options, low, high, active
       FROM /odsmfe/tb_filtr
       WHERE  entitysetname = @im_entity_set_name
       AND   active = @abap_on  INTO TABLE @lit_filters.

      IF sy-subrc EQ 0.
        GET REFERENCE OF ex_filter_values INTO lo_filter_values.
        ASSIGN lo_filter_values->* TO <lfsst_filter_values>.

        IF <lfsst_filter_values> IS ASSIGNED.
          LOOP AT lit_filters INTO lst_filters.

            IF lst_filters-low IS INITIAL.
              CONTINUE.
            ENDIF.
            MOVE-CORRESPONDING lst_filters TO lst_filter_val.
            lst_filter_val-option = lst_filters-options.
            APPEND lst_filter_val TO lit_filter_val.
            CLEAR lst_filter_val.

            ASSIGN COMPONENT 'FIELD' OF STRUCTURE lst_filters TO <lfsst_field>.
            IF <lfsst_field> IS ASSIGNED.
              ASSIGN COMPONENT <lfsst_field> OF STRUCTURE <lfsst_filter_values> TO <lfsit_filter_val>.
              IF <lfsit_filter_val> IS ASSIGNED.
                APPEND LINES OF lit_filter_val  TO <lfsit_filter_val>.
                UNASSIGN <lfsit_filter_val>.
              ENDIF.
              UNASSIGN <lfsst_field>.
            ENDIF.

            CLEAR lit_filter_val[].
          ENDLOOP.

          ex_filter_values = <lfsst_filter_values>.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_get_entityset_serv_config.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :05/03/2021
* Transport No.          :ES1K902573
* Program Description    :Method to Get Service Config Data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    "/ Check if table is active or not in table /odsmfe/tb_srcon
    SELECT SINGLE active FROM /odsmfe/tb_srcon
           WHERE active = @abap_true
           AND entityset_name = @im_entity_set_name INTO @ex_active.
    IF sy-subrc NE 0 .
      CLEAR ex_active.
    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_get_user_parameters.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SGROVER
* Creation Date          :  12/06/2020
* Transport No.          :
* Program Description    :
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
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
    TYPES: BEGIN OF ltys_paravalue,
             low(100) TYPE c,
           END OF ltys_paravalue,
           ltyt_paravalue TYPE STANDARD TABLE OF ltys_paravalue,

           BEGIN OF ty_options,
             text(72) TYPE c,
           END OF ty_options,

           BEGIN OF ty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF ty_fields,

           BEGIN OF ty_data,
             wa(512) TYPE c,
           END OF ty_data.

    DATA: lit_paravalues       TYPE ltyt_paravalue,
          lst_paravalues       TYPE ltys_paravalue,
          lst_parameter_values TYPE /odsmfe/st_core_range_str,
          lt_options           TYPE TABLE OF ty_options,
          lt_fields            TYPE TABLE OF ty_fields,
          lt_data              TYPE TABLE OF ty_data.
    DATA: lv_rowskip  TYPE int4,
          lv_rowcount TYPE int4.
* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*
*    IF im_parameter IS NOT INITIAL.
*      SELECT SINGLE parva
*           FROM usr05
*           WHERE  bname = @im_user AND parid = @im_parameter INTO @ex_parameter_value.
*    ELSE.
*      SELECT DISTINCT parva
*               FROM usr05
*               WHERE  bname = @im_user INTO TABLE @lit_paravalues. "#EC CI_BYPASS
*
*      LOOP AT lit_paravalues INTO lst_paravalues.
*        lst_parameter_values-sign   = 'I'.
*        lst_parameter_values-option = 'EQ'.
*        lst_parameter_values-low    = lst_paravalues-low.
*        APPEND lst_parameter_values TO ex_parameter_values.
*      ENDLOOP.
*    ENDIF.

    DATA(lo_rfc_dest) = cl_rfc_destination_provider=>create_by_cloud_destination(
                   i_name = 'ED1_TEST_001'
                   ).

    DATA(lv_rfc_dest_name) = lo_rfc_dest->get_destination_name(  ).

    lt_fields = VALUE #( ( fieldname = 'PARVA' ) ).


    lt_options = VALUE #( ( text = |BNAME| & | | & |EQ| & | | & |'| & |{ im_user }| & |'| ) ).

    IF im_parameter IS NOT INITIAL.
      APPEND VALUE #( text = |AND| & | | & |PARID| & | | & |EQ| & | | & |'| & |{ im_parameter }| & |'| ) TO lt_options.
    ENDIF.

    "Call RFC to get user parameters
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc_dest_name
      EXPORTING
        query_table = 'USR05'
        rowskips    = lv_rowskip
        rowcount    = lv_rowcount
      TABLES
        options     = lt_options
        fields      = lt_fields
        data        = lt_data.

    LOOP AT lt_data INTO DATA(ls_Data).

      IF im_parameter IS NOT INITIAL.
        ex_parameter_value = ls_Data.
        EXIT.
      ELSE.
        lst_parameter_values-sign   = 'I'.
        lst_parameter_values-option = 'EQ'.
        lst_parameter_values-low    = ls_data.
        APPEND lst_parameter_values TO ex_parameter_values.
        CLEAR lst_parameter_values.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_set_global_variable.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to set global variable
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
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


*    goio_context                 =     im_context                 .
*    goio_request_details         =     im_request_details         .
*    goio_service_document_name   =     im_service_document_name   .
*    goio_service_version         =     im_service_version         .
*    goio_service_namespace       =     im_service_namespace       .
*    goio_injection               =     im_injection               .
*    gvio_http_method             =     im_http_method.


  ENDMETHOD.


  METHOD constructor.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    :
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
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


    CHECK sy-subrc EQ 0.
  ENDMETHOD.


  METHOD get_cloud_dest.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SGROVER
* Creation Date          : 04/01/2023
* Transport No.          :
* Program Description    : Get RFC Destination
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
    DATA: lv_Dest TYPE string.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    SELECT SINGLE param_value FROM /odsmfe/tb_apcon
    WHERE param_name = 'CLOUD_DEST'
    INTO @lv_dest.

    TRY.

        DATA(lo_rfc_dest) = cl_rfc_destination_provider=>create_by_cloud_destination(
                i_name = lv_dest
                ).

        ex_dest = lo_rfc_dest->get_destination_name(  ).

      CATCH  cx_rfc_dest_provider_error INTO DATA(lx_dest).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
