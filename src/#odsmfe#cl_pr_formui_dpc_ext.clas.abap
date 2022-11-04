class /ODSMFE/CL_PR_FORMUI_DPC_EXT definition
  public
  inheriting from /ODSMFE/CL_PR_FORMUI_DPC
  create public .

public section.
  type-pools ABAP .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~DELETE_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET_DELTA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_PR_FORMUI_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
    CHECK sy-subrc = 0.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
    check sy-subrc = 0.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
    check sy-subrc = 0.
  endmethod.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 15/05/2020
* Transport No.          : ES1K901774
* Program Description    : Method to Create entity data
***********************************************************************
************************* CHANGE HISTORY *****************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

***********************************************************************
*         DATA Declaration
***********************************************************************
    DATA: lo_data_provider        TYPE REF TO /odsmfe/if_get_entityset_main,
          lo_tech_request_context TYPE REF TO data,
          lo_response_context     TYPE REF TO data,
          lo_msg                  TYPE REF TO /iwbep/cx_mgw_busi_exception,
          lo_msg1                 TYPE REF TO /iwbep/cx_mgw_tech_exception.
    DATA: lv_msg  TYPE string,
          lv_msg1 TYPE string.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

********************************************************************************************
*  Get the Class Instance for the Data Provider Class
*  Interface Below Serves as FACADE for all the classes being called from here
*  We will perform two common actions of setting all variables in the handler class
*  and we will also call the method to get the data/create/modify the entityset
*  currently there are two branches only which represents a BAPI and a select query
********************************************************************************************
*

    lo_data_provider ?=  /odsmfe/cl_model_factory=>gosb_obj->/odsmfe/if_model_factory~gmib_get_instance_model(

      im_entity_set_name       = iv_entity_set_name )."++ES1K901991

    IF lo_data_provider IS BOUND.

** Set the global data for later use.
      lo_data_provider->gmib_set_global_variable(
      EXPORTING
        im_context                =  mo_context
        im_request_details        =  mr_request_details
        im_service_document_name  =  mr_service_document_name
        im_service_version        =  mr_service_version
        im_service_namespace      =  mr_service_namespace
        im_injection              =  mo_injection ).

* Call the method to get the data.
      lo_data_provider->gmib_get_entityset_data(
      EXPORTING
        im_data_provider              =   io_data_provider
        im_data_ext_class             =   me
        im_entity_name                =   iv_entity_name
        im_entity_set_name            =   iv_entity_set_name
        im_source_name                =   iv_source_name
        im_key_tab                    =   it_key_tab
        im_navigation_path            =   it_navigation_path
        im_tech_request_context_ent_c =   io_tech_request_context
      IMPORTING
        ex_entity                     =   er_entity ).
    ELSE.
      TRY.
* SAP Gateway Application Interface.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~create_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              io_data_provider        = io_data_provider
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = er_entity.

        CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
          lo_msg->get_text( RECEIVING result = lv_msg ).
          lo_msg->get_longtext( RECEIVING result = lv_msg ).
          MESSAGE lv_msg TYPE 'I'.
        CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
          lo_msg1->get_text( RECEIVING result = lv_msg1 ).
          lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
          MESSAGE lv_msg1 TYPE 'I'.

      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~delete_entity.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : MRAKESH
* Creation Date          : 17/05/21
* Transport No.          : ES1K000027
* Program Description    : Method to Read entity data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

***********************************************************************
*         DATA Declaration
***********************************************************************
    DATA: lo_data_provider        TYPE REF TO /odsmfe/if_get_entityset_main,
          lo_tech_request_context TYPE REF TO data,
          lo_response_context     TYPE REF TO data,
          lo_msg                  TYPE REF TO /iwbep/cx_mgw_busi_exception,
          lo_msg1                 TYPE REF TO /iwbep/cx_mgw_tech_exception.
    DATA: lv_msg  TYPE string,
          lv_msg1 TYPE string.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


********************************************************************************************
*  Get the Class Instance for the Data Provider Class
*  Interface Below Serves as FACADE for all the classes being called from here
*  We will perform two common actions of setting all variables in the handler class
*  and we will also call the method to get the data/create/modify the entityset
*  currently there are two branches only which represents a BAPI and a select query
********************************************************************************************
*

    lo_data_provider ?=  /odsmfe/cl_model_factory=>gosb_obj->/odsmfe/if_model_factory~gmib_get_instance_model(

    im_entity_set_name       = iv_entity_set_name ).

    IF lo_data_provider IS BOUND.

* Set the global data for later use.
      lo_data_provider->gmib_set_global_variable(
      EXPORTING
        im_context                =  mo_context
        im_request_details        =  mr_request_details
        im_service_document_name  =  mr_service_document_name
        im_service_version        =  mr_service_version
        im_service_namespace      =  mr_service_namespace
        im_injection              =  mo_injection ).

* Call the method to get the data.
      lo_data_provider->gmib_get_entityset_data(
      EXPORTING
        im_data_ext_class              =   me
        im_entity_name                 =   iv_entity_name
        im_entity_set_name             =   iv_entity_set_name
        im_source_name                 =   iv_source_name
        im_key_tab                     =   it_key_tab
        im_navigation_path             =   it_navigation_path
        im_tech_request_context_ent_d  =   io_tech_request_context ).

    ELSE.
      TRY.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~delete_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context.

        CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
          lo_msg->get_text( RECEIVING result = lv_msg ).
          lo_msg->get_longtext( RECEIVING result = lv_msg ).
          MESSAGE lv_msg TYPE 'I'.
        CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
          lo_msg1->get_text( RECEIVING result = lv_msg1 ).
          lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
          MESSAGE lv_msg1 TYPE 'I'.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 19/02/20
* Transport No.          : ES1K901528
* Program Description    : Method to Read entity data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

***********************************************************************
*         DATA Declaration
***********************************************************************
    DATA: lo_data_provider        TYPE REF TO /odsmfe/if_get_entityset_main,
          lo_tech_request_context TYPE REF TO data,
          lo_response_context     TYPE REF TO data,
          lo_msg                  TYPE REF TO /iwbep/cx_mgw_busi_exception,
          lo_msg1                 TYPE REF TO /iwbep/cx_mgw_tech_exception.
    DATA: lst_workorder            TYPE /odsmfe/st_workorder,
          lst_respcap1             TYPE /odsmfe/st_formrsp,
          lv_aufnr                 TYPE aufnr,
          lv_tech_request_context1 TYPE abap_abstypename,
          lv_msg                   TYPE string,
          lv_msg1                  TYPE string.
    CONSTANTS: lc_get TYPE string VALUE 'GET'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


********************************************************************************************
*  Get the Class Instance for the Data Provider Class
*  Interface Below Serves as FACADE for all the classes being called from here
*  We will perform two common actions of setting all variables in the handler class
*  and we will also call the method to get the data/create/modify the entityset
*  currently there are two branches only which represents a BAPI and a select query
********************************************************************************************
*

    lo_data_provider ?=  /odsmfe/cl_model_factory=>gosb_obj->/odsmfe/if_model_factory~gmib_get_instance_model(

      im_entity_set_name       = iv_entity_set_name )."++ES1K901991

    IF lo_data_provider IS BOUND.

* Set the global data for later use.
      lo_data_provider->gmib_set_global_variable(
      EXPORTING
        im_context                =  mo_context
        im_request_details        =  mr_request_details
        im_service_document_name  =  mr_service_document_name
        im_service_version        =  mr_service_version
        im_service_namespace      =  mr_service_namespace
        im_injection              =  mo_injection
        im_http_method            =  lc_get ).

* Call the method to get the data.
      lo_data_provider->gmib_get_entityset_data(
      EXPORTING
        im_data_ext_class              =   me
        im_entity_name                 =   iv_entity_name
        im_entity_set_name             =   iv_entity_set_name
        im_source_name                 =   iv_source_name
        im_key_tab                     =   it_key_tab
        im_navigation_path             =   it_navigation_path
        im_tech_request_context_entity =   io_tech_request_context
      IMPORTING
        ex_entity                      =   er_entity
        ex_response_context_entity     =   es_response_context ).

    ELSE.
      TRY.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = er_entity
              es_response_context     = es_response_context.

        CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
          lo_msg->get_text( RECEIVING result = lv_msg ).
          lo_msg->get_longtext( RECEIVING result = lv_msg ).
          MESSAGE lv_msg TYPE 'I'.
        CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
          lo_msg1->get_text( RECEIVING result = lv_msg1 ).
          lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
          MESSAGE lv_msg1 TYPE 'I'.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 18/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to Read entity set data
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

    DATA: lo_data_provider        TYPE REF TO /odsmfe/if_get_entityset_main,
          lo_tech_request_context TYPE REF TO data,
          lo_response_context     TYPE REF TO data,
          lo_msg                  TYPE REF TO /iwbep/cx_mgw_busi_exception,
          lo_msg1                 TYPE REF TO /iwbep/cx_mgw_tech_exception.
    DATA: lit_respcap TYPE STANDARD TABLE OF /odsmfe/tb_forsp.
    DATA: lv_tech_request_context1 TYPE abap_abstypename,
          lv_aufnr                 TYPE aufnr,
          lv_msg                   TYPE string,
          lv_msg1                  TYPE string.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
********************************************************************************************
*  Get the Class Instance for the Data Provider Class
*  Interface Below Serves as FACADE for all the classes being called from here
*  We will perform two common actions of setting all variables in the handler class
*  and we will also call the method to get the data/create/modify the entityset
*  currently there are two branches only which represents a BAPI and a select query
********************************************************************************************
**********************************************************************************************************

    lo_data_provider    ?=  /odsmfe/cl_model_factory=>gosb_obj->/odsmfe/if_model_factory~gmib_get_instance_model(

      im_entity_set_name = iv_entity_set_name )."++ES1K901991

    IF lo_data_provider IS BOUND.

* Set The Global Data For Later Use.
      lo_data_provider->gmib_set_global_variable(
      EXPORTING
        im_context                   =  mo_context
        im_request_details           =  mr_request_details
        im_service_document_name     =  mr_service_document_name
        im_service_version           =  mr_service_version
        im_service_namespace         =  mr_service_namespace
        im_injection                 =  mo_injection ).

* Call The Method To Get The Data.
      lo_data_provider->gmib_get_entityset_data(
      EXPORTING
        im_data_ext_class              = me
        im_entity_name                 = iv_entity_name
        im_entity_set_name             = iv_entity_set_name
        im_source_name                 = iv_source_name
        im_filter_select_options       = it_filter_select_options
        im_paging                      = is_paging
        im_key_tab                     = it_key_tab
        im_navigation_path             = it_navigation_path
        im_order                       = it_order
        im_filter_string               = iv_filter_string
        im_search_string               = iv_search_string
        im_tech_request_context        = io_tech_request_context
      IMPORTING
        ex_entityset                   = er_entityset
        ex_response_context            = es_response_context ).

    ELSE.
      TRY.
* SAP Gateway Application Interface.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              er_entityset             = er_entityset
              es_response_context      = es_response_context.

        CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
          lo_msg->get_text( RECEIVING result = lv_msg ).
          lo_msg->get_longtext( RECEIVING result = lv_msg ).
          MESSAGE lv_msg TYPE 'I'.
        CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
          lo_msg1->get_text( RECEIVING result = lv_msg1 ).
          lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
          MESSAGE lv_msg1 TYPE 'I'.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset_delta.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS
* Creation Date          : 18/02/2022
* Transport No.          : ES1K902883
* Program Description    : Method to Read entity set data delta
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : ODS
* Change Date            : 11.03.2022
* Transport No.          : ES1K902967
* Change Description     : ODSMFE Refactoring
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    DATA: lo_data_provider        TYPE REF TO /odsmfe/if_get_entityset_main,
          lo_tech_request_context TYPE REF TO data,
          lo_response_context     TYPE REF TO data,
          lo_msg                  TYPE REF TO /iwbep/cx_mgw_busi_exception,
          lo_msg1                 TYPE REF TO /iwbep/cx_mgw_tech_exception.
    DATA: lit_respcap TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
          lit_headers TYPE tihttpnvp.
    DATA: lv_tech_request_context1 TYPE abap_abstypename,
          lv_aufnr                 TYPE aufnr,
          lv_msg                   TYPE string,
          lv_entity_set_name       TYPE string,
          lv_entity_name           TYPE string,
          lv_msg1                  TYPE string.
    DATA: lo_tech_request     TYPE REF TO /iwbep/cl_mgw_request,
          lo_tech_request_sub TYPE REF TO /odsmfe/cl_sub_mgw_request, "Reference Class Name Update
          lo_request_details  TYPE REF TO /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context,
          ls_mr_request       TYPE /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
********************************************************************************************
*  Get the Class Instance for the Data Provider Class
*  Interface Below Serves as FACADE for all the classes being called from here
*  We will perform two common actions of setting all variables in the handler class
*  and we will also call the method to get the data/create/modify the entityset
*  currently there are two branches only which represents a BAPI and a select query
********************************************************************************************
**********************************************************************************************************

**Casting and Assigning  importing parameter to local variable
    lo_tech_request ?= io_tech_request_context.

**Instantialte subclass with some dumy header parameters
    CREATE OBJECT lo_tech_request_sub
      EXPORTING
        ir_request_details = lo_request_details
        it_headers         = lit_headers.

    CALL METHOD lo_tech_request_sub->gmib_get_request_header_values
      EXPORTING
        im_tech_request_context = lo_tech_request
      RECEIVING
        ex_mr_request_ref       = ls_mr_request.
* Get Reference of Request data
    GET REFERENCE OF ls_mr_request INTO mr_request_details.

    lv_entity_set_name = ls_mr_request-target_entity_set.
    lv_entity_name = ls_mr_request-target_entity.

    lo_data_provider    ?=  /odsmfe/cl_model_factory=>gosb_obj->/odsmfe/if_model_factory~gmib_get_instance_model(
      im_entity_set_name = lv_entity_set_name ).

    IF lo_data_provider IS BOUND.

* Set The Global Data For Later Use.
      lo_data_provider->gmib_set_global_variable(
      EXPORTING
        im_context                   =  mo_context
        im_request_details           =  mr_request_details
        im_service_document_name     =  mr_service_document_name
        im_service_version           =  mr_service_version
        im_service_namespace         =  mr_service_namespace
        im_injection                 =  mo_injection ).

* Call The Method To Get The Data.
      lo_data_provider->gmib_get_entityset_data(
      EXPORTING
        im_data_ext_class              = me
        im_entity_name                 = lv_entity_name
        im_entity_set_name             = lv_entity_set_name
        im_tech_request_context        = io_tech_request_context
      IMPORTING
        ex_entityset                   = er_entityset
        ex_deleted_entityset           = er_deleted_entityset
        ex_response_context            = es_response_context ).

    ELSE.
      TRY.
* SAP Gateway Application Interface.
          CALL METHOD me->/iwbep/if_mgw_appl_srv_runtime~get_entityset
            EXPORTING
              iv_entity_name          = lv_entity_name
              iv_entity_set_name      = lv_entity_set_name
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entityset            = er_entityset
              es_response_context     = es_response_context.

        CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
          lo_msg->get_text( RECEIVING result = lv_msg ).
          lo_msg->get_longtext( RECEIVING result = lv_msg ).
          MESSAGE lv_msg TYPE 'I'.
        CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
          lo_msg1->get_text( RECEIVING result = lv_msg1 ).
          lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
          MESSAGE lv_msg1 TYPE 'I'.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~update_entity.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 15/05/2020
* Transport No.          : ES1K901774
* Program Description    : Method to Update entity data
***********************************************************************
************************* CHANGE HISTORY *****************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

***********************************************************************
*         DATA Declaration
***********************************************************************
    DATA: lo_data_provider        TYPE REF TO /odsmfe/if_get_entityset_main,
          lo_tech_request_context TYPE REF TO data,
          lo_response_context     TYPE REF TO data,
          lo_msg                  TYPE REF TO /iwbep/cx_mgw_busi_exception,
          lo_msg1                 TYPE REF TO /iwbep/cx_mgw_tech_exception.
    DATA: lv_msg  TYPE string,
          lv_msg1 TYPE string.
    CONSTANTS: lc_put TYPE string VALUE 'PUT'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

********************************************************************************************
*  Get the Class Instance for the Data Provider Class
*  Interface Below Serves as FACADE for all the classes being called from here
*  We will perform two common actions of setting all variables in the handler class
*  and we will also call the method to get the data/create/modify the entityset
*  currently there are two branches only which represents a BAPI and a select query
********************************************************************************************
*

    lo_data_provider ?=  /odsmfe/cl_model_factory=>gosb_obj->/odsmfe/if_model_factory~gmib_get_instance_model(

      im_entity_set_name       = iv_entity_set_name )."++ ES1K901991

    IF lo_data_provider IS BOUND.

** Set the global data for later use.
      lo_data_provider->gmib_set_global_variable(
      EXPORTING
        im_context                =  mo_context
        im_request_details        =  mr_request_details
        im_service_document_name  =  mr_service_document_name
        im_service_version        =  mr_service_version
        im_service_namespace      =  mr_service_namespace
        im_injection              =  mo_injection
        im_http_method            =  lc_put ).

* Call the method to get the data.

      lo_data_provider->gmib_get_entityset_data(
      EXPORTING
        im_data_provider              =   io_data_provider
        im_data_ext_class             =   me
        im_entity_name                =   iv_entity_name
        im_entity_set_name            =   iv_entity_set_name
        im_source_name                =   iv_source_name
        im_key_tab                    =   it_key_tab
        im_navigation_path            =   it_navigation_path
        im_tech_request_context_ent_u = io_tech_request_context
      IMPORTING
        ex_entity                     =   er_entity ).
    ELSE.
      TRY.
* SAP Gateway Application Interface.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~update_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              io_data_provider        = io_data_provider
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = er_entity.

        CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
          lo_msg->get_text( RECEIVING result = lv_msg ).
          lo_msg->get_longtext( RECEIVING result = lv_msg ).
          MESSAGE lv_msg TYPE 'I'.
        CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
          lo_msg1->get_text( RECEIVING result = lv_msg1 ).
          lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
          MESSAGE lv_msg1 TYPE 'I'.

      ENDTRY.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
