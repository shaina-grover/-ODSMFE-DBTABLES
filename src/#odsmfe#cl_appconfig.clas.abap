class /ODSMFE/CL_APPCONFIG definition
  public
  inheriting from /ODSMFE/CL_GET_ENTITYSET_SUP
  create public .

public section.

  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_EXECUTE_QUERY
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_GET_ADDITIONAL_WHERE
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_GET_WHERE_CLAUSE
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_SORT_DATA
    redefinition .
protected section.
private section.

  data GITII_ENTITYSET type /ODSMFE/CL_PR_APPSTORE_MPC=>TT_APPLICATIONCONFIG .
ENDCLASS.



CLASS /ODSMFE/CL_APPCONFIG IMPLEMENTATION.


  method /ODSMFE/IF_GET_ENTITYSET~GMIB_EXECUTE_QUERY.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :23/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Execute the select query
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

    DATA: lv_select        TYPE /odsmfe/if_get_entityset~gtyt_select,
          lv_where_clause  TYPE string,
          lv_from          TYPE string,
          lst_filter       TYPE /iwbep/s_mgw_select_option,
          lst_filter_range TYPE /iwbep/s_cod_select_option,
          lit_appconfig    TYPE /odsmfe/cl_pr_appstore_mpc=>tt_applicationconfig.

    FIELD-SYMBOLS: <lfsst_select> TYPE /odsmfe/if_get_entityset~gtyt_select.

    DATA: lv_active  TYPE string.

    DATA: lo_super  TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    TRY.
        "/Instantiate super class
        CREATE OBJECT lo_super
          EXPORTING
            im_entity_name = im_entity_name.

        "/Get Service Config Data
        CALL METHOD lo_super->/odsmfe/if_get_entityset_main~gmib_get_entityset_serv_config
          EXPORTING
            im_entity_set_name = im_entity_set_name
          IMPORTING
            ex_active          = lv_active.
        IF lv_active IS INITIAL.
          RETURN.
        ENDIF. " IF lv_active IS INITIAL.

        "/ Fetch The Model Instance
        me->/odsmfe/if_get_entityset~gmib_get_model_instance( ).

        "/ Get The Wehre Clause
        lv_where_clause = /odsmfe/if_get_entityset~gmib_get_where_clause( im_tech_request_context ).

        "/Get The Fields For Selection From The Config Table
        me->/odsmfe/if_get_entityset~gmib_get_selection_fields(
        EXPORTING im_entity     = im_entity_name
        CHANGING  ch_select_fld = lv_select ).

        "/ Build From Clause From The Config Table
        lv_from = /odsmfe/if_get_entityset~gmib_get_from_clause( im_entity_name ).

        TRY .
            SELECT (lv_select)
            INTO CORRESPONDING FIELDS OF TABLE gitii_entityset
            FROM (lv_from)
            WHERE (lv_where_clause).
            IF sy-subrc EQ 0.
              me->/odsmfe/if_get_entityset~gmib_sort_data( ).

              "/ Data Provider for ODATA Services
              GET REFERENCE OF gitii_entityset INTO ch_entityset.
            ENDIF. "/IF sy-subrc EQ 0.
          CATCH cx_sy_dynamic_osql_syntax. " Open SQL Error
            CHECK sy-subrc EQ 0.
        ENDTRY.
    ENDTRY.
  endmethod.


  METHOD /odsmfe/if_get_entityset~gmib_get_additional_where.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :24/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Get the Additional where Clause
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

    DATA: lv_fnam TYPE char30,
          lv_fval	TYPE char30.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

*    lv_fnam = 'ACTIVE'.
*    lv_fval = abap_true.
*    CONCATENATE `'` lv_fval `'` INTO lv_fval.
*
*    "/ Below method can be called here Number of times.
*    "/ Data Provider for ODATA Services.
*    super->/odsmfe/if_get_entityset~gmib_get_additional_where(
*    EXPORTING
*      im_fnam     = lv_fnam
*      im_fval     = lv_fval
*    CHANGING
*      ch_where    = ch_where ).
  ENDMETHOD.


  method /ODSMFE/IF_GET_ENTITYSET~GMIB_GET_WHERE_CLAUSE.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :24/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to get Build the SQL Where Clause
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

    TRY.
        "/ Data Provider for ODATA Services
        re_where_cls = super->/odsmfe/if_get_entityset~gmib_get_where_clause( im_tech_request_context ).

        "/get additional where condition for query
        me->/odsmfe/if_get_entityset~gmib_get_additional_where(
        CHANGING
          ch_where    = re_where_cls ).

      CATCH cx_static_check.


    ENDTRY.
  endmethod.


  method /ODSMFE/IF_GET_ENTITYSET~GMIB_SORT_DATA.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :24/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Sort Entityset data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

    SORT gitii_entityset BY recordnum.
  endmethod.
ENDCLASS.
