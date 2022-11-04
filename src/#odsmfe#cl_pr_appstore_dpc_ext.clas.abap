class /ODSMFE/CL_PR_APPSTORE_DPC_EXT definition
  public
  inheriting from /ODSMFE/CL_PR_APPSTORE_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_PR_APPSTORE_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :05/03/2021
* Transport No.          :ES1K902573
* Program Description    :Method to Execute a READ request
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

    DATA: lo_data_provider TYPE REF TO /odsmfe/if_get_entityset, "Data Provider for ODATA Services
          lv_bus_exp       TYPE string,
          lo_bus_exp       TYPE REF TO /iwbep/cx_mgw_busi_exception,
          lv_tec_exp       TYPE string,
          lo_tec_exp       TYPE REF TO /iwbep/cx_mgw_tech_exception.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*
***********************************************************************************************
* Get the Class Instance for the Data Provider Class
* Interface Below Serves as FACADE for all the classes being called from here
* We will perform two common actions of setting all variables in the handler class
* and we will also call the method to get the data/create/modify the entityset
* currently there are two branches only which represents a BAPI and a select query
***********************************************************************************************

    lo_data_provider ?=  /odsmfe/cl_model_factory=>gosb_obj->/odsmfe/if_model_factory~gmib_get_instance_model(
    EXPORTING
      im_entity_name     = iv_entity_name
      im_entity_set_name = iv_entity_set_name ).

    TRY.

        IF lo_data_provider IS BOUND.

          "/Set the global data for later use.
          lo_data_provider->gmib_set_global_variable(
          EXPORTING
            im_context                =  mo_context
            im_request_details        =  mr_request_details
            im_service_document_name  =  mr_service_document_name
            im_service_version        =  mr_service_version
            im_service_namespace      =  mr_service_namespace
            im_injection              =  mo_injection ).


          "/Call the method to get the data.
          lo_data_provider->gmib_get_entityset_data(
          EXPORTING
             im_data_ext_class           =   me
             im_entity_name              =   iv_entity_name
             im_entity_set_name          =   iv_entity_set_name
             im_source_name              =   iv_source_name
             im_filter_select_options    =   it_filter_select_options
             im_paging                   =   is_paging
             im_key_tab                  =   it_key_tab
             im_navigation_path          =   it_navigation_path
             im_order                    =   it_order
             im_filter_string            =   iv_filter_string
             im_search_string            =   iv_search_string
             im_tech_request_context     =   io_tech_request_context
           IMPORTING
             ex_entityset                =   er_entityset
             ex_response_context         =   es_response_context ).

        ELSE.


          "/SAP Gateway Application Interface.

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

        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception INTO lo_bus_exp. " Business Exception
        CALL METHOD lo_bus_exp->if_message~get_text
          RECEIVING
            result = lv_bus_exp.

      CATCH /iwbep/cx_mgw_tech_exception INTO lo_tec_exp. " Technical Exception
        CALL METHOD lo_tec_exp->if_message~get_text
          RECEIVING
            result = lv_tec_exp.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
