class /ODSMFE/CL_GET_ENT_SUPER_BAPI definition
  public
  create public .

public section.
  type-pools ABAP .

  interfaces /ODSMFE/IF_GET_ENTITYSET_MAIN .
  interfaces /ODSMFE/IF_GET_ENTITYSET_BAPI .

  methods CONSTRUCTOR
    importing
      !IM_ENTITY_NAME type STRING
      !IM_ENTITY_SET_NAME type STRING optional
      !IM_DATA_EXT_CLASS type ref to OBJECT optional
      !IM_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT optional
      !IM_REQUEST_DETAILS type ref to /IWBEP/IF_MGW_CORE_SRV_RUNTIME=>TY_S_MGW_REQUEST_CONTEXT optional
      !IM_SERVICE_DOCUMENT_NAME type ref to STRING optional
      !IM_SERVICE_VERSION type ref to NUMC4 optional
      !IM_SERVICE_NAMESPACE type ref to STRING optional
      !IM_INJECTION type ref to /IWBEP/IF_SB_GEN_DPC_INJECTION optional .
protected section.

  data GOIO_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT .
  data GOIO_REQUEST_DETAILS type ref to /IWBEP/IF_MGW_CORE_SRV_RUNTIME=>TY_S_MGW_REQUEST_CONTEXT .
  data GOIO_SERVICE_DOCUMENT_NAME type ref to STRING .
  data GOIO_SERVICE_VERSION type ref to NUMC4 .
  data GOIO_SERVICE_NAMESPACE type ref to STRING .
  data GOIO_INJECTION type ref to /IWBEP/IF_SB_GEN_DPC_INJECTION .
  data GVIO_HTTP_METHOD type STRING .
private section.
ENDCLASS.



CLASS /ODSMFE/CL_GET_ENT_SUPER_BAPI IMPLEMENTATION.


  method /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_BAPI_TRANSACTION_COMMIT.
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

  DATA: lst_return  TYPE bapiret2,
        lv_message TYPE bapi_msg.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
  CLEAR lst_return.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    WAIT   = 'X'
  IMPORTING
    RETURN = lst_return.
* Error handling
  IF lst_return-TYPE EQ 'E' OR lst_return-TYPE EQ 'A'.
    lv_message = lst_return-MESSAGE.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
    EXPORTING
      textid  = /iwbep/cx_mgw_busi_exception=>business_error
      MESSAGE = lv_message.
  ENDIF.

  endmethod.


  method /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_DELETE_ENTITYSET.

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
  endmethod.


  method /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_ERROR_HANDLING.
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
  endmethod.


  method /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_MODIFY_ENTITYSET.
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
  endmethod.


  METHOD /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET.

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


    FIELD-SYMBOLS: <lfsst_req> TYPE /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    ASSIGN goio_request_details->* TO <lfsst_req>.

    CHECK <lfsst_req> IS ASSIGNED.

    CASE <lfsst_req>-http_method.

      WHEN 'POST'.
        me->/odsmfe/if_get_entityset_bapi~gmib_create_entityset(
        EXPORTING
          im_data_ext_class        = me
          im_entity_name           = im_entity_name
          im_entity_set_name       = im_entity_set_name
          im_source_name           = im_source_name
          im_filter_select_options = im_filter_select_options
          im_paging                = im_paging
          im_key_tab               = im_key_tab
          im_navigation_path       = im_navigation_path
          im_order                 = im_order
          im_filter_string         = im_filter_string
          im_search_string         = im_search_string
          im_tech_request_context  = im_tech_request_context_ent_c
          im_data_provider         = im_data_provider
          im_changeset_request     = im_changeset_request
        IMPORTING
          ex_entityset             = ex_entityset
          ex_changeset_response    = ex_changeset_response
          ex_response_context      = ex_response_context
          ex_entity                = ex_entity
          ex_deep_entity           = ex_deep_entity ).

      WHEN 'GET'.
        IF im_tech_request_context_entity IS SUPPLIED.

          me->/odsmfe/if_get_entityset_bapi~gmib_read_entityset(
          EXPORTING
            im_data_ext_class        = me
            im_entity_name           = im_entity_name
            im_entity_set_name       = im_entity_set_name
            im_source_name           = im_source_name
            im_filter_select_options = im_filter_select_options
            im_paging                = im_paging
            im_key_tab               = im_key_tab
            im_navigation_path       = im_navigation_path
            im_order                 = im_order
            im_filter_string         = im_filter_string
            im_search_string         = im_search_string
            im_tech_request_context_entity = im_tech_request_context_entity
            im_data_provider         = im_data_provider
          IMPORTING
            ex_entityset             = ex_entityset
            ex_response_context_entity = ex_response_context_entity
            ex_entity                = ex_entity
            ex_deleted_entityset     = ex_deleted_entityset
            ex_deep_entity           = ex_deep_entity ).
        ENDIF.

        IF im_tech_request_context IS SUPPLIED.

          me->/odsmfe/if_get_entityset_bapi~gmib_read_entityset(
          EXPORTING
            im_data_ext_class        = me
            im_entity_name           = im_entity_name
            im_entity_set_name       = im_entity_set_name
            im_source_name           = im_source_name
            im_filter_select_options = im_filter_select_options
            im_paging                = im_paging
            im_key_tab               = im_key_tab
            im_navigation_path       = im_navigation_path
            im_order                 = im_order
            im_filter_string         = im_filter_string
            im_search_string         = im_search_string
            im_tech_request_context  = im_tech_request_context
            im_data_provider         = im_data_provider
          IMPORTING
            ex_entityset             = ex_entityset
            ex_response_context      = ex_response_context
            ex_entity                = ex_entity
            ex_deleted_entityset     = ex_deleted_entityset
            ex_deep_entity           = ex_deep_entity  ).
        ENDIF.

      WHEN 'PUT'.
        me->/odsmfe/if_get_entityset_bapi~gmib_modify_entityset(
        EXPORTING
          im_data_ext_class        = me
          im_entity_name           = im_entity_name
          im_entity_set_name       = im_entity_set_name
          im_source_name           = im_source_name
          im_filter_select_options = im_filter_select_options
          im_paging                = im_paging
          im_key_tab               = im_key_tab
          im_navigation_path       = im_navigation_path
          im_order                 = im_order
          im_filter_string         = im_filter_string
          im_search_string         = im_search_string
          im_tech_request_context  = im_tech_request_context_ent_u
          im_data_provider         = im_data_provider
          im_changeset_request     = im_changeset_request
        IMPORTING
          ex_entityset             = ex_entityset
          ex_response_context      = ex_response_context
          ex_changeset_response    = ex_changeset_response
          ex_entity                = ex_entity
          ex_deep_entity           = ex_deep_entity ).

      WHEN 'DELETE'.

        CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_delete_entityset
          EXPORTING
            im_filter_select_options      = im_filter_select_options
            im_paging                     = im_paging
            im_source_name                = im_source_name
            im_key_tab                    = im_key_tab
            im_order                      = im_order
            im_filter_string              = im_filter_string
            im_search_string              = im_search_string
            im_tech_request_context_ent_d = im_tech_request_context_ent_d
            im_data_provider              = im_data_provider
            im_data_ext_class             = im_data_ext_class
            im_entity_name                = im_entity_name
            im_entity_set_name            = im_entity_set_name
          IMPORTING
            ex_entityset                  = ex_entityset
            ex_response_context           = ex_response_context
            ex_entity                     = ex_entity
            ex_deep_entity                = ex_deep_entity.


      WHEN 'PATCH'.
        IF gvio_http_method = 'GET'.
          CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_read_entityset(
            EXPORTING
              im_filter_select_options       = im_filter_select_options
              im_paging                      = im_paging
              im_source_name                 = im_source_name
              im_key_tab                     = im_key_tab
              im_order                       = im_order
              im_filter_string               = im_filter_string
              im_search_string               = im_search_string
              im_tech_request_context_entity = im_tech_request_context_entity
              im_data_provider               = im_data_provider
              im_data_ext_class              = me
              im_entity_name                 = im_entity_name
              im_entity_set_name             = im_entity_set_name
              im_navigation_path             = im_navigation_path
            IMPORTING
              ex_entityset                   = ex_entityset
              ex_response_context            = ex_response_context
              ex_entity                      = ex_entity
              ex_deep_entity                 = ex_deep_entity ).
        ENDIF.

        IF gvio_http_method = 'PUT'.
          TRY.
              CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_modify_entityset(
                EXPORTING
                  im_filter_select_options = im_filter_select_options
                  im_paging                = im_paging
                  im_source_name           = im_source_name
                  im_key_tab               = im_key_tab
                  im_order                 = im_order
                  im_filter_string         = im_filter_string
                  im_search_string         = im_search_string
                  im_tech_request_context  = im_tech_request_context_ent_u
                  im_data_provider         = im_data_provider
                  im_data_ext_class        = me
                  im_entity_name           = im_entity_name
                  im_entity_set_name       = im_entity_set_name
                  im_navigation_path       = im_navigation_path
                IMPORTING
                  ex_entityset             = ex_entityset
                  ex_response_context      = ex_response_context
                  ex_entity                = ex_entity
                  ex_deep_entity           = ex_deep_entity ).
          ENDTRY.
        ENDIF.


      WHEN OTHERS.

    ENDCASE.
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

    FIELD-SYMBOLS: <lfsit_filter_val> TYPE /odsmfe/tt_core_range_tab,
                   <lfsst_field>      TYPE /odsmfe/tb_filtr,
                   <lfsst_filter_values> TYPE any.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF ex_filter_values IS SUPPLIED.

* Select all the filters from config table for the respective entityset

      SELECT mandt entitysetname tabname field recordno field_descr sign options low high active
       FROM /odsmfe/tb_filtr
       INTO TABLE lit_filters
       WHERE  entitysetname = im_entity_set_name
       AND   active = abap_on.

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

            REFRESH lit_filter_val[].
          ENDLOOP.

          ex_filter_values = <lfsst_filter_values>.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  method /ODSMFE/IF_GET_ENTITYSET_MAIN~GMIB_GET_ENTITYSET_SERV_CONFIG.
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
    SELECT SINGLE active FROM /odsmfe/tb_srcon INTO ex_active
           WHERE active = abap_true
           AND entityset_name = im_entity_set_name.
    IF sy-subrc ne 0 .
      clear ex_active.
    ENDIF.
  endmethod.


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
             low TYPE char100,
           END OF ltys_paravalue,
           ltyt_paravalue TYPE STANDARD TABLE OF ltys_paravalue.

    DATA: lit_paravalues       TYPE ltyt_paravalue,
          lst_paravalues       TYPE ltys_paravalue,
          lst_parameter_values TYPE /odsmfe/st_core_range_str.
* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*
    IF im_parameter IS NOT INITIAL.
      SELECT SINGLE parva
           FROM usr05
           INTO ex_parameter_value
           WHERE  bname = im_user AND parid = im_parameter.
    ELSE.
      SELECT DISTINCT parva
               FROM usr05
               INTO TABLE lit_paravalues
               WHERE  bname = im_user.                   "#EC CI_BYPASS

      LOOP AT lit_paravalues INTO lst_paravalues.
        lst_parameter_values-sign   = 'I'.
        lst_parameter_values-option = 'EQ'.
        lst_parameter_values-low    = lst_paravalues-low.
        APPEND lst_parameter_values TO ex_parameter_values.
      ENDLOOP.
    ENDIF.
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


    goio_context                 =     im_context                 .
    goio_request_details         =     im_request_details         .
    goio_service_document_name   =     im_service_document_name   .
    goio_service_version         =     im_service_version         .
    goio_service_namespace       =     im_service_namespace       .
    goio_injection               =     im_injection               .
    gvio_http_method             =     im_http_method.


  ENDMETHOD.


method CONSTRUCTOR.
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
endmethod.
ENDCLASS.
