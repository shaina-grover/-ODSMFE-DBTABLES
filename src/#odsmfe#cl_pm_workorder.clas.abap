CLASS /odsmfe/cl_pm_workorder DEFINITION
  PUBLIC
  INHERITING FROM /odsmfe/cl_pm_workorder_abs

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /odsmfe/if_create_instance .

    DATA gv_onlinesearch TYPE flag .
    DATA gv_unassigned TYPE flag .
    DATA gt_aufnr_delta1 TYPE /odsmfe/pm_valid_aufnr_tab .
    CLASS-DATA gosb_obj TYPE REF TO /odsmfe/cl_model_factory .
    TYPES:BEGIN OF ty_Selectoptions,
            property TYPE string,
            sign     TYPE bapisign,
            option   TYPE bapioption,
            low      TYPE c LENGTH 100,
            high     TYPE c LENGTH 100,
          END OF ty_selectoptions.
    DATA: select_option TYPE /odsmfe/mgw_select_option_tab . "/IWBEP/T_MGW_SELECT_OPTION.

    METHODS get_cloud_dest
      EXPORTING
        ex_dest TYPE rfcdest.

    METHODS dynamic_join_clause
      RETURNING
        VALUE(re_from) TYPE string .
    METHODS get_workorder_common
      IMPORTING
        !it_filter_select_options TYPE /odsmfe/mgw_select_option_tab OPTIONAL "/IWBEP/T_MGW_SELECT_OPTION
        !iv_mobileuser            TYPE string OPTIONAL
        !it_workorder_header      TYPE /odsmfe/cs_caufv_tab OPTIONAL.
    "raising
    "/IWBEP/CX_MGW_BUSI_EXCEPTION .

    METHODS get_workorder_detail
        REDEFINITION .
    METHODS get_workorder_longtext
        REDEFINITION .
  PROTECTED SECTION.

    CONSTANTS gc_option_eq TYPE bapioption VALUE 'EQ' ##NO_TEXT.
    DATA gs_filter_vals TYPE /odsmfe/wm_filter_vals .
    DATA lt_filter_vals TYPE TABLE OF /odsmfe/wm_filter_vals.
    CONSTANTS gc_sign_i TYPE bapisign VALUE 'I' ##NO_TEXT.
    DATA gv_user TYPE usnam .
    DATA gv_parvw TYPE char2 . "PARVW
    DATA gv_roleid TYPE /odsmfe/roleid .
    CONSTANTS gc_option_bt TYPE bapioption VALUE 'BT' ##NO_TEXT.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF gty_parnr_object,
        objnr TYPE c LENGTH 22, "ihpa-objnr,
        parnr TYPE c LENGTH 12, "ihpa-parnr,
      END OF gty_parnr_object .
    TYPES:
      BEGIN OF gty_valid_wo_parnr,
        aufnr TYPE /odsmfe/st_aufk-aufnr,
        auart TYPE c LENGTH 4, "aufart,
        objnr TYPE  j_objnr,
        parnr TYPE c LENGTH 12,
        priok TYPE c LENGTH 1, "priok,
      END OF gty_valid_wo_parnr .
    TYPES:
      gtt_valid_wo_parnr TYPE STANDARD TABLE OF gty_valid_wo_parnr .
    TYPES:
      BEGIN OF gty_wo_object,
        aufpl TYPE /odsmfe/st_afko_inc-aufpl,
        aufnr TYPE /odsmfe/st_aufk-aufnr,
        auart TYPE /odsmfe/st_aufk-auart,
        objnr TYPE /odsmfe/st_aufk-objnr,
        parnr TYPE c LENGTH 12,
        priok TYPE c LENGTH 1,
      END OF gty_wo_object .
    TYPES:
      gtt_wo_object TYPE STANDARD TABLE OF gty_wo_object .
    TYPES:
      BEGIN OF gty_afvc_key,
        aufpl TYPE /odsmfe/st_afvc-aufpl,
        aplzl TYPE n LENGTH 8, "co_aplzl,
        pernr TYPE n LENGTH 8,
      END OF gty_afvc_key .
    TYPES:
      gtt_afvc_key TYPE STANDARD TABLE OF gty_afvc_key .
    TYPES:
      gtt_mobstatfilt TYPE STANDARD TABLE OF /odsmfe/tb_mstfl .
    TYPES:
      BEGIN OF gty_wo_stat_obj,
        objnr TYPE j_objnr,
        stat  TYPE c LENGTH 5, "j_status,
        inact TYPE c LENGTH 1, "j_inact,
      END OF gty_wo_stat_obj .


    DATA gt_valid_oper TYPE gtt_afvc_key .
    DATA gt_oper_del TYPE /odsmfe/pm_oper_object_tab .

******
* soc by Viswa
    TYPES:
      BEGIN OF gty_data,
        wa(512) TYPE c,
      END OF gty_data.

    TYPES: BEGIN OF gty_options,
             text(72) TYPE c,
           END OF gty_options.
    TYPES: BEGIN OF gty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF gty_fields.

    DATA: gt_options TYPE TABLE OF gty_options,
          gt_fields  TYPE TABLE OF gty_fields,
          gt_data    TYPE TABLE OF gty_data.

* eoc by Viswa

    METHODS add_filter_select_options
      IMPORTING
        !it_filter_select_options TYPE /odsmfe/mgw_select_option_tab."/IWBEP/T_MGW_SELECT_OPTION  .
    METHODS get_assignment_type
      IMPORTING
        !iv_mobileuser TYPE string.
    "raising
    "" /IWBEP/CX_MGW_BUSI_EXCEPTION .
    METHODS get_data_asgn_type_1
      IMPORTING
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab
      CHANGING
        !ct_valid_wo_parnr   TYPE gtt_valid_wo_parnr .
    METHODS get_data_asgn_type_2
      IMPORTING
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab
      EXPORTING
        !et_wo_object        TYPE gtt_wo_object .
    METHODS get_data_asgn_type_3
      IMPORTING
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab
      CHANGING
        !ct_valid_wo_parnr   TYPE gtt_valid_wo_parnr.
    " raising
    "  /IWBEP/CX_MGW_BUSI_EXCEPTION .

    METHODS get_data_asgn_type_4
      IMPORTING
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab
      EXPORTING
        !et_wo_object        TYPE gtt_wo_object .


    METHODS get_data_asgn_type_5
      IMPORTING
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab
      EXPORTING
        !et_wo_object        TYPE gtt_wo_object .

    METHODS get_mobile_filter
      IMPORTING
        !iv_entity_set_name TYPE string OPTIONAL
      EXPORTING
        !es_filter_vals     TYPE /odsmfe/wm_filter_vals .

    METHODS get_mobile_filters_dynamic
      IMPORTING
        !iv_entity_set_name TYPE string OPTIONAL
      EXPORTING
        !es_filter_vals     TYPE /odsmfe/wm_filter_vals .

    METHODS get_mobile_filter_values.
    METHODS get_selection_asgn_type_1
      EXPORTING
        VALUE(et_valid_wo_parnr) TYPE gtt_valid_wo_parnr .
    METHODS get_selection_asgn_type_3
      EXPORTING
        VALUE(et_valid_wo_parnr) TYPE gtt_valid_wo_parnr .
    METHODS get_sel_asgn_type_2
      IMPORTING
        !it_valid_oper TYPE gtt_afvc_key
      EXPORTING
        !et_wo_object  TYPE gtt_wo_object .
    METHODS get_sel_asgn_type_2_dlta
      IMPORTING
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab
      EXPORTING
        !et_valid_oper       TYPE gtt_afvc_key .
    METHODS get_sel_asgn_type_2_nondlta
      EXPORTING
        !et_valid_oper TYPE gtt_afvc_key .
    METHODS get_sel_asgn_type_4
      IMPORTING
        !it_valid_oper TYPE gtt_afvc_key
      EXPORTING
        !et_wo_object  TYPE gtt_wo_object .
    METHODS get_sel_asgn_type_4_dlta
      IMPORTING
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab
      EXPORTING
        !et_valid_oper       TYPE gtt_afvc_key .
    METHODS get_sel_asgn_type_4_nondlta
      EXPORTING
        !et_valid_oper TYPE gtt_afvc_key .

    METHODS get_sel_asgn_type_5
      IMPORTING
        !it_valid_oper TYPE gtt_afvc_key
      EXPORTING
        !et_wo_object  TYPE gtt_wo_object .
    METHODS get_sel_asgn_type_5_dlta
      IMPORTING
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab
      EXPORTING
        !et_valid_oper       TYPE gtt_afvc_key
        !et_wo_object        TYPE gtt_wo_object .
    METHODS get_sel_asgn_type_5_nondlta
      EXPORTING
        !et_valid_oper TYPE gtt_afvc_key
        !et_wo_object  TYPE gtt_wo_object .
    METHODS get_unassigned_wo_hdr
      CHANGING
        !ct_wo_object TYPE gtt_wo_object .
    METHODS get_unassigned_wo_opr
      CHANGING
        !ct_wo_object TYPE gtt_wo_object .
    METHODS get_user_plants
      IMPORTING
        !ip_user           TYPE syuname
        !ip_plant_category TYPE /odsmfe/plant_category DEFAULT 'W'
      RETURNING
        VALUE(et_plants)   TYPE /odsmfe/core_range_tab .
    METHODS get_user_workcenter
      IMPORTING
        !ip_user             TYPE syuname
        !ip_wc_category      TYPE /odsmfe/wc_category OPTIONAL
      RETURNING
        VALUE(et_workcenter) TYPE /odsmfe/core_range_tab .
    METHODS populate_range_data
      IMPORTING
        !iv_parva     TYPE char40 OPTIONAL
      CHANGING
        !ct_range_tab TYPE /odsmfe/core_range_tab .
    METHODS wo_by_login_user
      IMPORTING
        !it_workorder_header       TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_mobileuser             TYPE string OPTIONAL
      EXPORTING
        VALUE(et_created_wo_parnr) TYPE gtt_valid_wo_parnr .
    METHODS wo_non_online_search
      IMPORTING
        !iv_mobileuser       TYPE string
        !it_workorder_header TYPE /odsmfe/cs_caufv_tab OPTIONAL
      EXPORTING
        !et_wo_object        TYPE gtt_wo_object.
    "raising
    " /IWBEP/CX_MGW_BUSI_EXCEPTION .
    METHODS wo_online_search
      EXPORTING
        !et_wo_object TYPE gtt_wo_object .

    METHODS get_mobstat_filter
      EXPORTING
        !et_mobstatfilt TYPE gtt_mobstatfilt .
    METHODS adjust_wo_stat_filters
      CHANGING
        !ct_wo_object TYPE gtt_wo_object .
    METHODS adjust_wo_oper_stat_filters
      CHANGING
        !ct_wo_object TYPE gtt_wo_object .
    METHODS populate_output_data
      IMPORTING
        !it_wo_object TYPE gtt_wo_object .
    METHODS populate_person_responsible .
    METHODS get_status_for_wo .
ENDCLASS.



CLASS /ODSMFE/CL_PM_WORKORDER IMPLEMENTATION.


 METHOD get_cloud_dest.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : VREDDY
* Creation Date          : 05/04/2023
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


  METHOD /odsmfe/if_create_instance~create_object_factory.

***********************************************************
* Data Declaration
***********************************************************
    FIELD-SYMBOLS: <fs_clss_config> TYPE /odsmfe/tb_entcl.
    DATA: lo_obj   TYPE REF TO object,
          lo_exref TYPE REF TO cx_root,
          lv_mesg  TYPE string.
* CONSTANTS:
    CONSTANTS: lc_i TYPE string VALUE 'I'.

***********************************************************
* Main Section
***********************************************************
    IF /odsmfe/if_create_instance~gs_class_config IS INITIAL.
***** START: Select Data From /odsmfe/tb_entcl  *************************
      SELECT SINGLE entityset,
             clsname
        FROM /odsmfe/tb_entcl
        WHERE entityset EQ @im_entity INTO CORRESPONDING FIELDS OF @/odsmfe/if_create_instance~gs_class_config.
***** END: Select Data From /odsmfe/tb_entcl  ***************************
    ENDIF.

    IF /odsmfe/if_create_instance~gs_class_config IS NOT INITIAL.
      ASSIGN /odsmfe/if_create_instance~gs_class_config TO <fs_clss_config>.
      CHECK <fs_clss_config> IS ASSIGNED.
      TRY.
          CREATE OBJECT lo_obj TYPE (<fs_clss_config>-clsname)
           EXPORTING
              im_entity_name     =   im_entity.

        CATCH cx_sy_create_object_error.
          lv_mesg = lo_exref->get_text( ).


*           MESSAGE lv_mesg TYPE lc_i. " need to check further
      ENDTRY.

      CHECK lo_obj IS BOUND.
      re_obj ?= lo_obj.

    ENDIF.

  ENDMETHOD.


  METHOD add_filter_select_options.

    TYPES: BEGIN OF lty_iloa,
             iloan TYPE /odsmfe/st_riwol-iloan,
             tplnr TYPE /odsmfe/st_riwol-tplnr,
           END OF lty_iloa.

    DATA: lit_iloa TYPE TABLE OF lty_iloa,
          lst_iloa TYPE lty_iloa.


    DATA: lst_filter       TYPE /odsmfe/mgw_select_option_str, "/iwbep/s_mgw_select_option,

          lst_filter_range TYPE /odsmfe/cod_select_option, "/iwbep/s_cod_select_option,
          lst_filter_val   TYPE /odsmfe/core_range_str,

          lv_tplnr         TYPE /odsmfe/st_riwol-tplnr,
          lv_equnr         TYPE /odsmfe/st_riwol-equnr,
          lv_aufnr         TYPE /odsmfe/st_aufk-aufnr,
          lv_priority      TYPE c LENGTH 1.


    CONSTANTS: lc_mainworkctr            TYPE string VALUE 'MAINWORKCTR',
               lc_plant                  TYPE string VALUE 'PLANT',
               lc_funclocation           TYPE string VALUE 'FUNCLOCATION',
               lc_equipnum               TYPE string VALUE 'EQUIPNUM',
               lc_onlinesearch           TYPE string VALUE 'ONLINESEARCH',
               lc_unassigned             TYPE string VALUE 'UNASSIGNED',
               lc_workordernum           TYPE string VALUE 'WORKORDERNUM',
               lc_createdon              TYPE string VALUE 'CREATEDON',
               lc_priority               TYPE string VALUE 'PRIORITY',
               lc_changedtforordermaster TYPE string VALUE 'CHANGEDTFORORDERMASTER',

               lc_owner_6                TYPE c LENGTH 1 VALUE '6'.
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
    LOOP AT it_filter_select_options INTO lst_filter.

*      CASE ls_filter_select_options-name.
      READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
      TRANSLATE lst_filter-property TO UPPER CASE.

      CASE lst_filter-property.
        WHEN lc_mainworkctr.    "MAINWORKCTR
          IF lst_filter_range-low IS NOT INITIAL.
            lst_filter_val-sign   = gc_sign_i.
            lst_filter_val-option = gc_option_eq.
            lst_filter_val-low    = lst_filter_range-low.
            APPEND lst_filter_val TO gs_filter_vals-work_cntr.
            CLEAR:lst_filter_val, lst_filter_range.
          ENDIF.
        WHEN lc_plant.
          IF lst_filter_range-low IS NOT INITIAL.
            lst_filter_val-sign   = gc_sign_i.
            lst_filter_val-option = gc_option_eq.
            lst_filter_val-low    = lst_filter_range-low.
            APPEND lst_filter_val TO gs_filter_vals-plant.
            CLEAR:lst_filter_val, lst_filter_range.
          ENDIF.

        WHEN lc_funclocation.
          IF lst_filter_range-low IS NOT INITIAL.
            lv_tplnr = lst_filter_range-low.
*need to check
*            CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
*              EXPORTING
*                input     = lv_tplnr
*              IMPORTING
*                output    = lv_tplnr
*              EXCEPTIONS
*                not_found = 1
*                OTHERS    = 2.

*            SELECT iloan ,tplnr
*              FROM iloa
*              WHERE tplnr = @lv_tplnr AND
*                    owner = @lc_owner_6 INTO TABLE @lit_iloa.
            DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.


   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).


            lt_fields = VALUE #( ( fieldname = 'ILOAN' )
                                 ( fieldname = 'TPLNR' )
                                 ).

            IF lv_tplnr  IS NOT INITIAL.
              lt_options = VALUE #( ( text = |TPLNR| & | | & |EQ| & | | & |'| & |{ lv_tplnr }| & |'| ) ).
              DATA(lv_and) = 'AND'.
            ENDIF.
            APPEND VALUE #( text = |{ lv_and }| & | | & |OWNER| & | | & |EQ| & | | & |'| & |{ lc_owner_6 }| & |'| ) TO lt_options.



            "Call RFC to get work orders
            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'ILOA'
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.

            LOOP AT lt_data INTO DATA(ls_Data).
              lst_filter_val-low  = ls_Data+0(40).
              APPEND lst_filter_val TO gs_filter_vals-tplnr.
            ENDLOOP.

*            LOOP AT lit_iloa INTO lst_iloa WHERE iloan IS NOT INITIAL.
*              lst_filter_val-sign   = gc_sign_i.
*              lst_filter_val-option = gc_option_eq.
*              lst_filter_val-low    = lst_iloa-iloan.
*              APPEND lst_filter_val TO gs_filter_vals-tplnr.
*              CLEAR:lst_filter_val, lst_filter_range.
*            ENDLOOP.
          ENDIF.

        WHEN lc_equipnum.
          IF lst_filter_range-low IS NOT INITIAL.
            lv_equnr = lst_filter_range-low.
*check
*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*              EXPORTING
*                input  = lv_equnr
*              IMPORTING
*                output = lv_equnr.

            lst_filter_val-sign   = gc_sign_i.
            lst_filter_val-option = gc_option_eq.
            lst_filter_val-low    = lv_equnr.
            APPEND lst_filter_val TO gs_filter_vals-equnr.
            CLEAR:lst_filter_val, lst_filter_range.
          ENDIF.

        WHEN lc_workordernum.
          IF lst_filter_range-low IS NOT INITIAL.
            lv_aufnr = lst_filter_range-low.
*need to check
*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*              EXPORTING
*                input  = lv_aufnr
*              IMPORTING
*                output = lv_aufnr.
            lst_filter_val-sign   = gc_sign_i.
            lst_filter_val-option = gc_option_eq.
            lst_filter_val-low    = lv_aufnr.
            APPEND lst_filter_val TO gs_filter_vals-aufnr.
            CLEAR:lst_filter_val, lst_filter_range.
          ENDIF.

        WHEN lc_createdon.
          IF lst_filter_range-low NE 00000000.
            lst_filter_val-sign   = gc_sign_i.
            lst_filter_val-option = gc_option_bt.
            lst_filter_val-low    = lst_filter_range-low.
            APPEND lst_filter_val TO gs_filter_vals-created_on.
            CLEAR:lst_filter_val, lst_filter_range.
          ENDIF.

        WHEN lc_priority.
          IF lst_filter_range-low NE 00000000.
            lst_filter_val-sign   = gc_sign_i.
            lst_filter_val-option = gc_option_eq.
            lv_priority = lst_filter_range-low.
            lst_filter_val-low    = lst_filter_range-low.
            IF gv_onlinesearch IS NOT INITIAL.
              CLEAR gs_filter_vals-priority.
            ENDIF.
            APPEND lst_filter_val TO gs_filter_vals-priority.
            CLEAR:lst_filter_val, lst_filter_range.
          ENDIF.

        WHEN lc_onlinesearch.
          IF lst_filter_range-low IS NOT INITIAL.
            gv_onlinesearch = lst_filter_range-low.
          ENDIF.
          CLEAR: lst_filter_range.
        WHEN lc_unassigned.
          IF lst_filter_range-low IS NOT INITIAL.
            gv_unassigned = lst_filter_range-low.
          ENDIF.
          CLEAR: lst_filter_range.

        WHEN lc_changedtforordermaster.
          IF lst_filter_range-low NE 00000000.
            READ TABLE gs_filter_vals-created_on INTO lst_filter_val INDEX 1.

            IF lst_filter_val-low IS NOT INITIAL.
              lst_filter_val-high = lst_filter_range-low.
              MODIFY  gs_filter_vals-created_on FROM lst_filter_val INDEX 1.
            ENDIF.
          ENDIF.
      ENDCASE.

      IF gv_onlinesearch IS NOT INITIAL AND lv_priority IS INITIAL.
        CLEAR gs_filter_vals-priority.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.


  METHOD adjust_wo_oper_stat_filters.

*soc by Viswa
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
*soc by Viswa

    DATA: lit_wo_stat_obj     TYPE STANDARD TABLE OF gty_wo_stat_obj,
          lit_wo_stat_obj_tmp TYPE STANDARD TABLE OF gty_wo_stat_obj,
          lst_wo_stat_obj     TYPE  gty_wo_stat_obj,
          lit_oper_objtmp     TYPE /odsmfe/pm_oper_object_tab,
          lst_oper_object     TYPE /odsmfe/pm_oper_object_str,
          lst_oper_del        TYPE /odsmfe/pm_oper_object_str,
          lv_index            TYPE int8, " sytabix,
          lst_wo_object       TYPE gty_wo_object,
          lst_valid_oper      TYPE gty_afvc_key. " added by Viswa

    "soc by Viswa
    DATA: lv_aufnr TYPE aufnr,
          lv_aufpl TYPE /odsmfe/pm_longtext_str-objtype.

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

    IF gt_valid_oper IS NOT INITIAL.
*soc by Viswa

*Select operations object numbers using assigned operations
*      SELECT afko~aufnr, afvc~aufpl, afvc~aplzl,afvc~vornr, afvc~objnr "SE
*        FROM afvc INNER JOIN afko ON afvc~aufpl = afko~aufpl
*        INTO CORRESPONDING FIELDS OF TABLE @me->gt_oper_object
*        FOR ALL ENTRIES IN @gt_valid_oper
*        WHERE afvc~aufpl EQ @gt_valid_oper-aufpl
*          AND afvc~aplzl EQ @gt_valid_oper-aplzl.   ""#EC CI_NO_TRANSFORM

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).


*      CALL METHOD lr_rfc->get_cloud_dest
*        IMPORTING
*          ex_dest = DATA(lv_rfc).



      LOOP AT gt_valid_oper INTO lst_valid_oper.
        lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                         ( fieldname = 'AUFPL' )
                         ).

        IF lst_valid_oper-aufpl  IS NOT INITIAL.
          lt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lst_valid_oper-aufpl }| & |'| ) ).
        ENDIF.

        "Call RFC to get work orders
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AFKO'
          TABLES
            options     = lt_options
            fields      = lt_fields
            data        = lt_data.

        LOOP AT lt_data INTO DATA(ls_Data).
          lv_aufnr = ls_data+0(12).
          lv_aufpl = ls_data+12(10).
        ENDLOOP.

        CLEAR: lt_fields,lt_options,lt_data.

        lt_fields = VALUE #( ( fieldname = 'AUFPL' )
                             ( fieldname = 'APLZL' )
                             ( fieldname = 'VORNR' )
                             ( fieldname = 'OBJNR' )
                             ).

        IF lst_valid_oper-aufpl  IS NOT INITIAL.
          lt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lst_valid_oper-aufpl }| & |'| ) ).
          DATA(lv_and) = 'AND'.
        ENDIF.

        IF lst_valid_oper-aplzl  IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |APLZL| & | | & |EQ| & | | & |'| & |{ lst_valid_oper-aplzl }| & |'| ) TO lt_options.
        ENDIF.

        "Call RFC to get work order operations
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AFVC'
          TABLES
            options     = lt_options
            fields      = lt_fields
            data        = lt_data.

        LOOP AT lt_data INTO ls_Data.
          IF lv_aufpl = ls_Data+12(10).
            lst_oper_object-aufnr = lv_aufnr.
            lst_oper_object-aufpl = lv_aufpl.
            lst_oper_object-aplzl = ls_data+22(8).
            lst_oper_object-objnr = ls_data+30(22).
            lst_oper_object-vornr = ls_data+65(4).
          ENDIF.

        ENDLOOP.

        APPEND lst_oper_object TO me->gt_oper_object.
        CLEAR: lv_aufnr, lv_aufpl,lst_oper_object.
        CLEAR: lt_options.
      ENDLOOP.

*eoc by Viswa

      IF sy-subrc EQ 0.
        SORT me->gt_oper_object.
      ENDIF.

    ELSE.
      IF ct_wo_object IS NOT INITIAL.

        IF gv_onlinesearch EQ abap_true AND gv_unassigned EQ abap_true AND me->assignment_type EQ '2'.
*soc by Viswa
*
*          SELECT DISTINCT afko~aufnr,afvc~aufpl,afvc~aplzl,afvc~objnr
*          FROM afko INNER JOIN afvc ON afvc~aufpl = afko~aufpl
*          INTO CORRESPONDING FIELDS OF TABLE @me->gt_oper_object
*          FOR ALL ENTRIES IN @ct_wo_object
*          WHERE afko~aufnr EQ @ct_wo_object-aufnr
*          AND   afvc~pernr IN @me->pernr_merged.


          CALL METHOD lr_rfc->get_cloud_dest
            IMPORTING
              ex_dest = lv_rfc.



          LOOP AT ct_wo_object INTO lst_wo_object.
            lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                                       ( fieldname = 'AUFPL' )
                                       ).

            IF lst_wo_object-aufpl  IS NOT INITIAL.
              lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_wo_object-aufnr }| & |'| ) ).
            ENDIF.
            CLEAR: lst_wo_object.

            "Call RFC to get work orders
            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'AFKO'
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.

            LOOP AT lt_data INTO ls_Data.
              lv_aufnr = ls_data+0(12).
              lv_aufpl = ls_data+12(10).
            ENDLOOP.

            CLEAR: lt_fields,lt_options,lt_data.

            lt_fields = VALUE #( ( fieldname = 'AUFPL' )
                                   ( fieldname = 'APLZL' )
                                   ( fieldname = 'OBJNR' )
                                   ).
            IF lv_aufpl  IS NOT INITIAL.
              lt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lv_aufpl }| & |'| ) ).
              lv_and = 'AND'.
            ENDIF.

            IF me->pernr_merged  IS NOT INITIAL.
              APPEND VALUE #( text = |{ lv_and }| & | | & |PERNR| & | | & |{ me->pernr_merged[ 1 ]-option }| & | | & |'| & |{ me->pernr_merged[ 1 ]-low }| & |'| )  TO lt_options.
            ENDIF.

            "Call RFC to get work order operations
            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'AFVC'
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.

            LOOP AT lt_data INTO ls_Data.
              IF lv_aufpl = ls_Data+12(10).
                lst_oper_object-aufnr = lv_aufnr.
                lst_oper_object-aufpl = lv_aufpl.
                lst_oper_object-aplzl = ls_data+22(8).
                lst_oper_object-objnr = ls_data+30(22).
              ENDIF.

            ENDLOOP.

            APPEND lst_oper_object TO me->gt_oper_object.

            CLEAR: lt_options,lst_oper_object,lv_aufnr, lv_aufpl.
          ENDLOOP.

* eoc by Viswa
        ELSE.

*soc by Viswa
*          SELECT DISTINCT afko~aufnr ,afvc~aufpl, afvc~aplzl, afvc~objnr
*            FROM afko INNER JOIN afvc ON afvc~aufpl = afko~aufpl
*            INTO CORRESPONDING FIELDS OF TABLE @me->gt_oper_object
*            FOR ALL ENTRIES IN @ct_wo_object
*            WHERE afko~aufnr EQ @ct_wo_object-aufnr.

          CALL METHOD lr_rfc->get_cloud_dest
            IMPORTING
              ex_dest = lv_rfc.


          LOOP AT ct_wo_object INTO lst_wo_object.

            lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                                       ( fieldname = 'AUFPL' )
                                       ).

            IF lst_wo_object-aufpl  IS NOT INITIAL.
              lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_wo_object-aufnr }| & |'| ) ).
            ENDIF.
            CLEAR: lst_wo_object.

            "Call RFC to get work orders
            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'AFKO'
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.

            LOOP AT lt_data INTO ls_Data.
              lv_aufnr = ls_data+0(12).
              lv_aufpl = ls_data+12(10).
            ENDLOOP.

            CLEAR: lt_fields,lt_options,lt_data.

            lt_fields = VALUE #( ( fieldname = 'AUFPL' )
                                   ( fieldname = 'APLZL' )
                                   ( fieldname = 'OBJNR' )
                                   ).
            IF lv_aufpl  IS NOT INITIAL.
              lt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lv_aufpl }| & |'| ) ).
            ENDIF.

            "Call RFC to get work order operations
            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'AFVC'
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.

            LOOP AT lt_data INTO ls_Data.
              IF lv_aufpl = ls_Data+12(10).
                lst_oper_object-aufnr = lv_aufnr.
                lst_oper_object-aufpl = lv_aufpl.
                lst_oper_object-aplzl = ls_data+22(8).
                lst_oper_object-objnr = ls_data+30(22).
              ENDIF.

            ENDLOOP.

            APPEND lst_oper_object TO me->gt_oper_object.

            CLEAR: lt_options,lst_oper_object,lv_aufnr, lv_aufpl.
          ENDLOOP.
*eoc by Viswa

          IF sy-subrc EQ 0.
            SORT me->gt_oper_object.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.



    IF me->gt_oper_object IS NOT INITIAL.

      "oper_incl_syst_stat
      IF me->gt_oper_object IS NOT INITIAL.
        IF NOT gs_filter_vals-oper_incl_syst_stat IS INITIAL.
* Get the status info.
*soc by Viswa


*          SELECT objnr FROM jest
*           FOR ALL ENTRIES IN @me->gt_oper_object
*           WHERE objnr EQ @me->gt_oper_object-objnr
*             AND stat  IN @gs_filter_vals-oper_incl_syst_stat
*             AND inact EQ @space INTO TABLE @lit_wo_stat_obj.

          CLEAR : gt_fields,gt_options,gt_data,lv_and.

          LOOP AT gt_oper_object INTO lst_oper_object.

            gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                                    ).

            gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_oper_object-objnr }| & |'| ) ).
            lv_and = 'AND'.

            APPEND gs_filter_vals TO lt_filter_vals.

            IF gs_filter_vals-oper_incl_syst_stat IS NOT INITIAL.
              APPEND VALUE #( text = |{ lv_and }| & | | & |STAT| & | | & |{ gs_filter_vals-oper_incl_syst_stat[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-oper_incl_syst_stat[ 1 ]-low }| & |'| )  TO gt_options.
            ENDIF.

            APPEND VALUE #( text = |{ lv_and }| & | | & |INACT| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'JEST'
              TABLES
                options     = gt_options
                fields      = gt_fields
                data        = gt_data.

            LOOP AT gt_data INTO ls_data.

              lst_wo_stat_obj-objnr = ls_data+0(22).

              APPEND lst_wo_stat_obj TO lit_wo_stat_obj.
            ENDLOOP.

          ENDLOOP.
          CLEAR : gt_fields,gt_options,gt_data,lv_and.
*eoc by Viswa
          IF sy-subrc EQ 0.
            lit_oper_objtmp[] = me->gt_oper_object[].
            CLEAR me->gt_oper_object.
            SORT lit_wo_stat_obj.
            SORT lit_oper_objtmp BY objnr.
            LOOP AT lit_wo_stat_obj INTO lst_wo_stat_obj.
              READ TABLE lit_oper_objtmp INTO lst_oper_object
                WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
              IF sy-subrc = 0.
                APPEND lst_oper_object TO me->gt_oper_object.
              ENDIF.
            ENDLOOP.
            CLEAR: lit_wo_stat_obj, lit_oper_objtmp.
          ENDIF.

        ENDIF.

        "oper_excl_syst_stat
        IF NOT gs_filter_vals-oper_excl_syst_stat IS INITIAL.
*soc by Viswa

*          SELECT DISTINCT objnr FROM jest
*            FOR ALL ENTRIES IN @me->gt_oper_object
*            WHERE objnr EQ @me->gt_oper_object-objnr
*              AND stat  IN @gs_filter_vals-oper_excl_syst_stat
*              AND inact EQ @space INTO TABLE @lit_wo_stat_obj.

          CLEAR : gt_fields,gt_options,gt_data,lv_and.

          LOOP AT gt_oper_object INTO lst_oper_object.

            gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                                    ).

            gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_oper_object-objnr }| & |'| ) ).
            lv_and = 'AND'.

            IF gs_filter_vals-oper_excl_syst_stat IS NOT INITIAL.
              APPEND VALUE #( text = |{ lv_and }| & | | & |STAT| & | | & |{ gs_filter_vals-oper_excl_syst_stat[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-oper_excl_syst_stat[ 1 ]-low }| & |'| )  TO gt_options.
            ENDIF.

            APPEND VALUE #( text = |{ lv_and }| & | | & |INACT| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'JEST'
              TABLES
                options     = gt_options
                fields      = gt_fields
                data        = gt_data.

            LOOP AT gt_data INTO ls_data.

              lst_wo_stat_obj-objnr = ls_data+0(22).

              APPEND lst_wo_stat_obj TO lit_wo_stat_obj.
            ENDLOOP.

          ENDLOOP.
          CLEAR : gt_fields,gt_options,gt_data,lv_and.

*eoc by Viswa
          IF sy-subrc EQ 0.
            SORT lit_wo_stat_obj.
            SORT me->gt_oper_object BY objnr.
            LOOP AT lit_wo_stat_obj INTO lst_wo_stat_obj.
              READ TABLE me->gt_oper_object[] INTO lst_oper_del
                 WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
              CLEAR lv_index.
              lv_index = sy-tabix.
              IF sy-subrc = 0.
                APPEND lst_oper_del TO me->gt_oper_del.
                DELETE me->gt_oper_object INDEX lv_index.
              ENDIF.
            ENDLOOP.
            CLEAR lit_wo_stat_obj.
          ENDIF.
        ENDIF.

        "oper_excl_user_stat
        IF NOT gs_filter_vals-oper_excl_user_stat IS INITIAL.
*soc by Viswa

*          SELECT DISTINCT objnr FROM jest
*            FOR ALL ENTRIES IN @me->gt_oper_object
*            WHERE objnr EQ @me->gt_oper_object-objnr
*              AND stat  IN @gs_filter_vals-oper_excl_syst_stat
*              AND inact EQ @space INTO TABLE @lit_wo_stat_obj.

          CLEAR : gt_fields,gt_options,gt_data,lv_and.

          LOOP AT gt_oper_object INTO lst_oper_object.

            gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                                    ).

            gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_oper_object-objnr }| & |'| ) ).
            lv_and = 'AND'.

            IF gs_filter_vals-oper_excl_syst_stat IS NOT INITIAL.
              APPEND VALUE #( text = |{ lv_and }| & | | & |STAT| & | | & |{ gs_filter_vals-oper_excl_syst_stat[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-oper_excl_syst_stat[ 1 ]-low }| & |'| )  TO gt_options.
            ENDIF.

            APPEND VALUE #( text = |{ lv_and }| & | | & |INACT| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'JEST'
              TABLES
                options     = gt_options
                fields      = gt_fields
                data        = gt_data.

            LOOP AT gt_data INTO ls_data.

              lst_wo_stat_obj-objnr = ls_data+0(22).

              APPEND lst_wo_stat_obj TO lit_wo_stat_obj.
            ENDLOOP.

          ENDLOOP.
          CLEAR : gt_fields,gt_options,gt_data,lv_and.

*eoc by Viswa
          IF sy-subrc EQ 0.
            SORT lit_wo_stat_obj.
            SORT me->gt_oper_object BY objnr.

            LOOP AT lit_wo_stat_obj INTO lst_wo_stat_obj.
              READ TABLE me->gt_oper_object[] INTO lst_oper_del
                 WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
              CLEAR lv_index.
              lv_index = sy-tabix.
              IF sy-subrc = 0.
                APPEND lst_oper_del TO me->gt_oper_del.
                DELETE me->gt_oper_object INDEX lv_index.
              ENDIF.
            ENDLOOP.
            CLEAR lit_wo_stat_obj.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    SORT me->gt_oper_object BY aufnr.
    LOOP AT ct_wo_object INTO lst_wo_object.
      lv_index = sy-tabix.
      READ TABLE me->gt_oper_object INTO lst_oper_object
        WITH KEY aufnr = lst_wo_object-aufnr BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE ct_wo_object INDEX lv_index.
      ENDIF.
    ENDLOOP.

* for Operation level assingment - remove the header which are completed, closed , suspended or TECO
    IF me->assignment_type EQ '2' OR me->assignment_type EQ '4'.
      LOOP AT me->gt_oper_object INTO lst_oper_object.
        lv_index = sy-tabix.
        READ TABLE ct_wo_object INTO lst_wo_object
          WITH KEY aufnr = lst_oper_object-aufnr BINARY SEARCH.
        IF sy-subrc <> 0.
          DELETE me->gt_oper_object INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDIF.



  ENDMETHOD.


  METHOD adjust_wo_stat_filters.

    DATA:lst_mobstatfilt  TYPE /odsmfe/tb_mstfl,
         lit_mobstatfilt  TYPE TABLE OF /odsmfe/tb_mstfl,
         ls_filter_val    TYPE /odsmfe/core_range_str,
         lit_date         TYPE /odsmfe/core_range_tab,
         lit_wo_stat_obj  TYPE TABLE OF gty_wo_stat_obj,
         lit_wo_stat_obj1 TYPE TABLE OF gty_wo_stat_obj,
         lit_wo_stat_obj2 TYPE TABLE OF gty_wo_stat_obj,
         lst_wo_stat_obj  TYPE gty_wo_stat_obj,
         lit_wo_objtmp    TYPE gtt_wo_object,
         lst_wo_object    TYPE gty_wo_object.


    CONSTANTS: lc_wo_incl_syst_stat   TYPE string  VALUE 'WO_INCL_SYST_STAT ',
               lc_wo_excl_user_stat   TYPE string   VALUE 'WO_EXCL_USER_STAT',
               lc_wo_excl_syst_stat   TYPE string VALUE 'WO_EXCL_SYST_STAT',
               lc_oper_incl_syst_stat TYPE string VALUE 'OPER_INCL_SYST_STAT',
               lc_oper_excl_user_stat TYPE string VALUE 'OPER_EXCL_USER_STAT',
               lc_oper_excl_syst_stat TYPE string VALUE 'OPER_EXCL_SYST_STAT'.

    CALL METHOD me->get_mobstat_filter
      IMPORTING
        et_mobstatfilt = lit_mobstatfilt.

* Clearing mobile filter status data if role based status filters are available
    IF lit_mobstatfilt IS NOT INITIAL.
      CLEAR : gs_filter_vals-wo_incl_syst_stat , gs_filter_vals-wo_excl_syst_stat , gs_filter_vals-wo_excl_user_stat ,
                gs_filter_vals-oper_excl_user_stat , gs_filter_vals-oper_incl_syst_stat , gs_filter_vals-oper_excl_syst_stat.
    ENDIF.

    LOOP AT lit_mobstatfilt INTO lst_mobstatfilt.
      CASE lst_mobstatfilt-field.
        WHEN lc_wo_incl_syst_stat .
          ls_filter_val-sign = lst_mobstatfilt-sign.
          ls_filter_val-option = lst_mobstatfilt-options.
          ls_filter_val-low = lst_mobstatfilt-low.
          ls_filter_val-high = lst_mobstatfilt-high.
          APPEND ls_filter_val TO  gs_filter_vals-wo_incl_syst_stat.
          CLEAR :ls_filter_val , ls_filter_val ,lst_mobstatfilt.

        WHEN lc_wo_excl_user_stat .
          ls_filter_val-sign = lst_mobstatfilt-sign.
          ls_filter_val-option = lst_mobstatfilt-options.
          ls_filter_val-low = lst_mobstatfilt-low.
          ls_filter_val-high = lst_mobstatfilt-high.
          APPEND ls_filter_val TO  gs_filter_vals-wo_excl_user_stat.
          CLEAR :  lst_mobstatfilt , ls_filter_val , ls_filter_val .

        WHEN lc_wo_excl_syst_stat .
          ls_filter_val-sign = lst_mobstatfilt-sign.
          ls_filter_val-option = lst_mobstatfilt-options.
          ls_filter_val-low = lst_mobstatfilt-low.
          ls_filter_val-high = lst_mobstatfilt-high.
          APPEND ls_filter_val TO  gs_filter_vals-wo_excl_syst_stat.
          CLEAR :  lst_mobstatfilt , ls_filter_val , ls_filter_val .

        WHEN lc_oper_incl_syst_stat .
          ls_filter_val-sign = lst_mobstatfilt-sign.
          ls_filter_val-option = lst_mobstatfilt-options.
          ls_filter_val-low = lst_mobstatfilt-low.
          ls_filter_val-high = lst_mobstatfilt-high.
          APPEND ls_filter_val TO  gs_filter_vals-oper_incl_syst_stat.
          CLEAR :  lst_mobstatfilt , ls_filter_val , ls_filter_val .

        WHEN lc_oper_excl_syst_stat .
          ls_filter_val-sign = lst_mobstatfilt-sign.
          ls_filter_val-option = lst_mobstatfilt-options.
          ls_filter_val-low = lst_mobstatfilt-low.
          ls_filter_val-high = lst_mobstatfilt-high.
          APPEND ls_filter_val TO  gs_filter_vals-oper_excl_syst_stat.
          CLEAR :ls_filter_val , ls_filter_val ,lst_mobstatfilt. .

        WHEN lc_oper_excl_user_stat .
          ls_filter_val-sign = lst_mobstatfilt-sign.
          ls_filter_val-option = lst_mobstatfilt-options.
          ls_filter_val-low = lst_mobstatfilt-low.
          ls_filter_val-high = lst_mobstatfilt-high.
          APPEND ls_filter_val TO  gs_filter_vals-oper_excl_user_stat.
          CLEAR :ls_filter_val , ls_filter_val ,lst_mobstatfilt. .

      ENDCASE.
    ENDLOOP.

* check WO_INCL_SYST_STAT
    IF NOT gs_filter_vals-wo_incl_syst_stat IS INITIAL.
      IF ct_wo_object IS NOT INITIAL.
* soc by Viswa

*        SELECT DISTINCT a~objnr FROM jest AS a INNER JOIN jcds AS b
*               ON a~objnr = b~objnr
*         FOR ALL ENTRIES IN @ct_wo_object
*         WHERE a~objnr EQ @ct_wo_object-objnr
*           AND a~stat  IN @gs_filter_vals-wo_incl_user_stat
*           AND a~inact EQ @space
*           AND b~udate IN @lit_date  INTO TABLE @lit_wo_stat_obj.

        DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

        LOOP AT ct_wo_object INTO lst_wo_object.

          gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                                  ).

          gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_wo_object-objnr }| & |'| ) ).
          DATA(lv_and) = 'AND'.

          IF gs_filter_vals-wo_incl_user_stat IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |STAT| & | | & |{ gs_filter_vals-wo_incl_user_stat[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-wo_incl_user_stat[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.

          APPEND VALUE #( text = |{ lv_and }| & | | & |INACT| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'JEST'
            TABLES
              options     = gt_options
              fields      = gt_fields
              data        = gt_data.

          LOOP AT gt_data INTO DATA(ls_data).

            lst_wo_stat_obj-objnr = ls_data+0(22).

            APPEND lst_wo_stat_obj TO lit_wo_stat_obj1.
          ENDLOOP.

          CLEAR: gt_data.
        ENDLOOP.
        CLEAR: gt_options,gt_fields,gt_data.


        LOOP AT lit_wo_stat_obj1 INTO lst_wo_stat_obj.
          gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                                    ).

          gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_wo_stat_obj-objnr }| & |'| ) ).
          lv_and = 'AND'.

          IF gs_filter_vals-wo_incl_user_stat IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |STAT| & | | & |{ lit_date[ 1 ]-option }| & | | & |'| & |{ lit_date[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.

          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'JCDS'
            TABLES
              options     = gt_options
              fields      = gt_fields
              data        = gt_data.

          LOOP AT gt_data INTO ls_data.

            IF  lst_wo_stat_obj-objnr = ls_data+0(22).

              APPEND lst_wo_stat_obj TO lit_wo_stat_obj.

            ENDIF.
          ENDLOOP.

          CLEAR: gt_data.
        ENDLOOP.
        CLEAR: gt_options,gt_fields,gt_data.

*eoc by Viswa


      ENDIF.
      lit_wo_objtmp[] = ct_wo_object[].
      CLEAR ct_wo_object.
      SORT lit_wo_objtmp BY objnr.
      LOOP AT lit_wo_stat_obj INTO lst_wo_stat_obj.
        READ TABLE lit_wo_objtmp INTO lst_wo_object
        WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
        IF sy-subrc = 0.
          APPEND lst_wo_object TO ct_wo_object.
        ENDIF.
      ENDLOOP.
      CLEAR: lit_wo_stat_obj, lit_wo_objtmp.
    ENDIF.

* check WO_EXCL_USER_STAT
    IF NOT gs_filter_vals-wo_excl_user_stat IS INITIAL.
      IF ct_wo_object IS NOT INITIAL.
*soc by Viswa

*        SELECT DISTINCT objnr FROM jest
*        FOR ALL ENTRIES IN @ct_wo_object
*        WHERE objnr EQ @ct_wo_object-objnr
*          AND stat  IN @gs_filter_vals-wo_excl_user_stat
*          AND inact EQ @space INTO TABLE @lit_wo_stat_obj.

        LOOP AT ct_wo_object INTO lst_wo_object.

          gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                                  ).

          gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_wo_object-objnr }| & |'| ) ).
          lv_and = 'AND'.

          IF gs_filter_vals-wo_excl_user_stat IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |STAT| & | | & |{ gs_filter_vals-wo_excl_user_stat[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-wo_excl_user_stat[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.

          APPEND VALUE #( text = |{ lv_and }| & | | & |INACT| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'JEST'
            TABLES
              options     = gt_options
              fields      = gt_fields
              data        = gt_data.

          LOOP AT gt_data INTO ls_data.

            lst_wo_stat_obj-objnr = ls_data+0(22).

            APPEND lst_wo_stat_obj TO lit_wo_stat_obj.
          ENDLOOP.

          CLEAR: gt_data.
        ENDLOOP.
        CLEAR: gt_options,gt_fields,gt_data.

*eoc by Viswa
        IF sy-subrc EQ 0.
          SORT ct_wo_object BY objnr.
          LOOP AT lit_wo_stat_obj INTO lst_wo_stat_obj.
            READ TABLE ct_wo_object TRANSPORTING NO FIELDS
            WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
            IF sy-subrc = 0.
              DELETE ct_wo_object INDEX sy-tabix.       ""#EC CI_NOORDER
            ENDIF.
          ENDLOOP.
          CLEAR lit_wo_stat_obj.
        ENDIF.
      ENDIF.
    ENDIF.


* Check WO_EXCL_SYST_STAT
    IF NOT gs_filter_vals-wo_excl_syst_stat IS INITIAL.
      IF ct_wo_object IS NOT INITIAL.
*soc by Viswa
*        SELECT DISTINCT objnr FROM jest
*        FOR ALL ENTRIES IN @ct_wo_object
*        WHERE objnr EQ @ct_wo_object-objnr
*          AND stat  IN @gs_filter_vals-wo_excl_syst_stat
*          AND inact EQ @space INTO TABLE @lit_wo_stat_obj.

        LOOP AT ct_wo_object INTO lst_wo_object.

          gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                                  ).

          gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_wo_object-objnr }| & |'| ) ).
          lv_and = 'AND'.

          IF gs_filter_vals-wo_excl_syst_stat IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |STAT| & | | & |{ gs_filter_vals-wo_excl_syst_stat[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-wo_excl_syst_stat[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.

          APPEND VALUE #( text = |{ lv_and }| & | | & |INACT| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'JEST'
            TABLES
              options     = gt_options
              fields      = gt_fields
              data        = gt_data.

          LOOP AT gt_data INTO ls_data.

            lst_wo_stat_obj-objnr = ls_data+0(22).

            APPEND lst_wo_stat_obj TO lit_wo_stat_obj.
          ENDLOOP.

          CLEAR: gt_data.
        ENDLOOP.
        CLEAR: gt_options,gt_fields,gt_data.
*eoc by Viswa
        SORT ct_wo_object BY objnr.
        LOOP AT lit_wo_stat_obj INTO lst_wo_stat_obj.
          READ TABLE ct_wo_object TRANSPORTING NO FIELDS
          WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
          IF sy-subrc = 0.
            DELETE ct_wo_object INDEX sy-tabix.
          ENDIF.
        ENDLOOP.
        CLEAR lit_wo_stat_obj.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD dynamic_join_clause.

    DATA: lit_joinc    TYPE TABLE OF /odsmfe/tb_joinc,
          lv_join_rank TYPE int4,
          lv_first_on  TYPE abap_bool VALUE abap_true.

    FIELD-SYMBOLS :<lfsst_join> TYPE /odsmfe/tb_joinc.

    SELECT * FROM  /odsmfe/tb_joinc
    WHERE entityset    = 'WoHeaderSet'  INTO TABLE @lit_joinc.

    LOOP AT lit_joinc ASSIGNING <lfsst_join>.
      "/We are not using Outer Join condition here
      IF lv_join_rank NE <lfsst_join>-join_rank.

        IF re_from IS INITIAL AND <lfsst_join>-rt_tab IS INITIAL.
          CONCATENATE re_from ` ` <lfsst_join>-left_tab INTO  re_from.
          CONTINUE.
        ELSEIF re_from IS INITIAL AND <lfsst_join>-rt_tab IS NOT INITIAL.
          CONCATENATE re_from ` ` <lfsst_join>-left_tab ` Inner Join ` <lfsst_join>-rt_tab ` on ` INTO re_from.
        ELSE.
          CONCATENATE re_from ` ` ` Inner Join ` <lfsst_join>-rt_tab  ` on ` INTO re_from.
        ENDIF.

        lv_join_rank = <lfsst_join>-join_rank.

        lv_first_on  = abap_true.

      ENDIF.

      "/We are not using OR condition here
      IF lv_first_on = abap_false.
        CONCATENATE re_from ` and ` INTO re_from.
      ELSE.
        lv_first_on = abap_false.
      ENDIF.

      CONCATENATE re_from <lfsst_join>-left_tab `~` <lfsst_join>-l_fnam ` = ` <lfsst_join>-rt_tab `~`  <lfsst_join>-r_fnam
               INTO re_from.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_assignment_type.

    DATA: lv_role_assigment TYPE /odsmfe/pm_assignment_type_dte.

    FIELD-SYMBOLS: <fs_range_str>    TYPE /odsmfe/core_range_str.

    IF iv_mobileuser IS NOT INITIAL.

      SELECT SINGLE roleid FROM /odsmfe/tb_urole
        WHERE userid = @iv_mobileuser
        AND  startdate LE @sy-datum
        AND enddate    GE @sy-datum INTO @gv_roleid.

      IF sy-subrc = 0 AND gv_roleid IS NOT INITIAL.
        CLEAR: lv_role_assigment.
        SELECT SINGLE assignmenttype FROM /odsmfe/tb_roles
               WHERE roleid EQ @gv_roleid INTO @lv_role_assigment.
        IF lv_role_assigment IS NOT INITIAL.
          " MOVE lv_role_assigment TO me->assignment_type.
          me->assignment_type = lv_role_assigment.

        ENDIF.
      ENDIF.
    ENDIF.

    IF me->assignment_type IS INITIAL.
      SORT gs_filter_vals-assignment BY sign option.
* Read assingment value
      READ TABLE gs_filter_vals-assignment ASSIGNING <fs_range_str>
      WITH KEY sign = 'I' option = 'EQ' BINARY SEARCH.
      IF sy-subrc = 0.
        "MOVE <fs_range_str>-low TO me->assignment_type.
        me->assignment_type = <fs_range_str>-low.
      ELSE.
*need to check
*        RAISE EXCEPTION TYPE         /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            textid  = /iwbep/cx_mgw_busi_exception=>business_error
*            message = text-w01.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_data_asgn_type_1.

    DATA: lv_tabix            TYPE int8, "sytabix,
          ls_valid_wo_parnr   TYPE gty_valid_wo_parnr,
          ls_workorder_header TYPE /odsmfe/cs_caufv_str.

    IF it_workorder_header IS NOT INITIAL AND gs_filter_vals-orderid IS INITIAL.
      LOOP AT ct_valid_wo_parnr INTO ls_valid_wo_parnr.
        lv_tabix =  sy-tabix.
        READ TABLE it_workorder_header INTO ls_workorder_header WITH KEY aufnr = ls_valid_wo_parnr-aufnr.
        IF sy-subrc NE 0.
          DELETE ct_valid_wo_parnr INDEX lv_tabix .
        ENDIF.
      ENDLOOP.

    ELSE.
* Only sorting required which is performed at end of method.
    ENDIF.
    SORT ct_valid_wo_parnr BY aufnr.
  ENDMETHOD.


  METHOD get_data_asgn_type_2.

    DATA: lit_valid_oper TYPE gtt_afvc_key.

    IF it_workorder_header IS NOT INITIAL.

      CALL METHOD me->get_sel_asgn_type_2_dlta
        EXPORTING
          it_workorder_header = it_workorder_header
        IMPORTING
          et_valid_oper       = lit_valid_oper.

    ELSE.

      CALL METHOD me->get_sel_asgn_type_2_nondlta
        IMPORTING
          et_valid_oper = lit_valid_oper.

    ENDIF.

    IF lit_valid_oper IS NOT INITIAL.

      CALL METHOD me->get_sel_asgn_type_2
        EXPORTING
          it_valid_oper = lit_valid_oper
        IMPORTING
          et_wo_object  = et_wo_object.

      gt_valid_oper[] = lit_valid_oper[].

    ENDIF.

  ENDMETHOD.


  METHOD get_data_asgn_type_3.

    DATA: lv_tabix            TYPE int8, " sytabix,
          ls_valid_wo_parnr   TYPE gty_valid_wo_parnr,
          ls_workorder_header TYPE /odsmfe/cs_caufv_str.

    IF it_workorder_header IS NOT INITIAL AND gs_filter_vals-orderid IS INITIAL.
      LOOP AT ct_valid_wo_parnr INTO ls_valid_wo_parnr.
        lv_tabix =  sy-tabix.
        READ TABLE it_workorder_header INTO ls_workorder_header WITH KEY aufnr = ls_valid_wo_parnr-aufnr.
        IF sy-subrc NE 0.
          DELETE ct_valid_wo_parnr INDEX lv_tabix .
        ENDIF.
      ENDLOOP.
    ELSE.
* Only sorting required which is performed at end of method.
    ENDIF.
    SORT ct_valid_wo_parnr BY aufnr.
  ENDMETHOD.


  METHOD get_data_asgn_type_4.

    DATA: lit_valid_oper TYPE gtt_afvc_key.

    IF it_workorder_header IS NOT INITIAL.

      CALL METHOD me->get_sel_asgn_type_4_dlta
        EXPORTING
          it_workorder_header = it_workorder_header
        IMPORTING
          et_valid_oper       = lit_valid_oper.

    ELSE.

      CALL METHOD me->get_sel_asgn_type_4_nondlta
        IMPORTING
          et_valid_oper = lit_valid_oper.

    ENDIF.

    IF lit_valid_oper IS NOT INITIAL.

      CALL METHOD me->get_sel_asgn_type_4
        EXPORTING
          it_valid_oper = lit_valid_oper
        IMPORTING
          et_wo_object  = et_wo_object.

      gt_valid_oper[] = lit_valid_oper[].

    ENDIF.

  ENDMETHOD.


  METHOD get_data_asgn_type_5.

    DATA: lit_valid_oper TYPE gtt_afvc_key.

    IF it_workorder_header IS NOT INITIAL.

      CALL METHOD me->get_sel_asgn_type_5_dlta
        EXPORTING
          it_workorder_header = it_workorder_header
        IMPORTING
          et_valid_oper       = lit_valid_oper
          et_wo_object        = et_wo_object.

    ELSE.

      CALL METHOD me->get_sel_asgn_type_5_nondlta
        IMPORTING
          et_valid_oper = lit_valid_oper
          et_wo_object  = et_wo_object.

    ENDIF.

    IF lit_valid_oper IS NOT INITIAL.
      gt_valid_oper[] = lit_valid_oper[].
    ENDIF.


  ENDMETHOD.


  METHOD get_mobile_filter.

    DATA: lit_tabname    TYPE /odsmfe/core_range_tab,
          lst_tabname    TYPE /odsmfe/core_range_str,
          lo_filter      TYPE REF TO /odsmfe/cl_config_utili,
          lit_mobfilters TYPE /odsmfe/mobfilters_tab,
          lst_mobfilters TYPE /odsmfe/mobfilters_str,
          lst_filter_val TYPE /odsmfe/core_range_str,
          lit_plants     TYPE /odsmfe/core_range_tab.

    CONSTANTS: lc_sign_i              TYPE bapisign VALUE 'I',
               lc_eq                  TYPE bapioption VALUE 'EQ',
               lc_i                   TYPE /odsmfe/plant_category VALUE 'I',
               lc_aufk                TYPE char100 VALUE 'AUFK',
               lc_jest                TYPE char100 VALUE 'JEST',
               lc_pa0001              TYPE char100 VALUE 'PA0001',
               lc_workorder           TYPE char100 VALUE 'WORKORDER',
               lc_adrc                TYPE char100 VALUE 'ADRC',
               lc_stko                TYPE char100 VALUE 'STKO',
               lc_crhd                TYPE char100 VALUE 'CRHD',
               lc_auart               TYPE char40 VALUE 'AUART',
               lc_aufnr               TYPE char40 VALUE 'AUFNR',
               lc_autyp               TYPE char40 VALUE 'AUTYP',
               lc_bukrs               TYPE char40 VALUE 'BUKRS',
               lc_idat1               TYPE char40 VALUE 'IDAT1',
               lc_idat2               TYPE char40 VALUE 'IDAT2',
               lc_idat3               TYPE char40 VALUE 'IDAT3',
               lc_kokrs               TYPE char40 VALUE 'KOKRS',
               lc_persno_exception    TYPE char40 VALUE 'PERSNO_EXCEPTION',
               lc_pm_phase            TYPE char40 VALUE 'PM_PHASE',
               lc_assignment          TYPE char40 VALUE 'ASSIGNMENT',
               lc_werks               TYPE char40 VALUE 'WERKS',
               lc_arbpl               TYPE char40 VALUE 'ARBPL',
               lc_objid               TYPE char40 VALUE 'OBJID',
               lc_wo_excl_syst_stat   TYPE char40 VALUE 'WO_EXCL_SYST_STAT',
               lc_wo_incl_syst_stat   TYPE char40 VALUE 'WO_INCL_SYST_STAT',
               lc_wo_excl_user_stat   TYPE char40 VALUE 'WO_EXCL_USER_STAT',
               lc_pernr               TYPE char40 VALUE 'PERNR',
               lc_oper_excl_syst_stat TYPE char40 VALUE 'OPER_EXCL_SYST_STAT',
               lc_oper_incl_syst_stat TYPE char40 VALUE 'OPER_INCL_SYST_STAT',
               lc_oper_excl_user_stat TYPE char40 VALUE 'OPER_EXCL_USER_STAT',
               lc_ingpr               TYPE char40 VALUE 'INGPR',
               lc_iwerk               TYPE char40 VALUE 'IWERK',
               lc_pending_num         TYPE char40 VALUE 'PENDING_NUM',
               lc_hist_num            TYPE char40 VALUE 'HIST_NUM',
               lc_histdur_num         TYPE char40 VALUE 'HISTDUR_NUM',
               lc_pendingdur_num      TYPE char40 VALUE 'PENDINGDUR_NUM',
               lc_hist_refdt_typ      TYPE char40 VALUE 'HIST_REFDT_TYP',
               lc_woaddressnumber     TYPE char40 VALUE 'WOADDRESSNUMBER',
               lc_stlty               TYPE char40 VALUE 'STLTY',
               lc_stlal               TYPE char40 VALUE 'STLAL'.


************************************************************************
* Main Section
************************************************************************

* To Fetch Mobile Filters
    CLEAR lst_tabname.
    lst_tabname-sign = lc_sign_i.
    lst_tabname-option = lc_eq.
    lst_tabname-low = lc_aufk.
    lst_tabname-high = ''.
    APPEND lst_tabname TO lit_tabname.
    CLEAR lst_tabname.

    lst_tabname-sign = lc_sign_i.
    lst_tabname-option = lc_eq.
    lst_tabname-low = lc_crhd.
    lst_tabname-high = ''.
    APPEND lst_tabname TO lit_tabname.
    CLEAR lst_tabname.

    lst_tabname-sign = lc_sign_i.
    lst_tabname-option = lc_eq.
    lst_tabname-low = lc_jest.
    lst_tabname-high = ''.
    APPEND lst_tabname TO lit_tabname.
    CLEAR lst_tabname.

    lst_tabname-sign = lc_sign_i.
    lst_tabname-option = lc_eq.
    lst_tabname-low = lc_pa0001.
    lst_tabname-high = ''.
    APPEND lst_tabname TO lit_tabname.
    CLEAR lst_tabname.

    CLEAR lst_tabname.
    lst_tabname-sign = lc_sign_i.
    lst_tabname-option = lc_eq.
    lst_tabname-low = lc_workorder.
    lst_tabname-high = ''.
    APPEND lst_tabname TO lit_tabname.

    CLEAR lst_tabname.
    lst_tabname-sign = lc_sign_i.
    lst_tabname-option = lc_eq.
    lst_tabname-low = lc_adrc.
    lst_tabname-high = ''.
    APPEND lst_tabname TO lit_tabname.

    CLEAR lst_tabname.
    lst_tabname-sign = lc_sign_i.
    lst_tabname-option = lc_eq.
    lst_tabname-low = lc_stko.
    lst_tabname-high = ''.
    APPEND lst_tabname TO lit_tabname.



    lo_filter = /odsmfe/cl_config_utili=>get_instance( ).
    lo_filter->get_mobile_filters( EXPORTING lv_tabname = lit_tabname
                                               lv_active  = abap_true
                                               lv_entity_set_name = iv_entity_set_name
                                     IMPORTING et_filters = lit_mobfilters ).


    LOOP AT lit_mobfilters INTO lst_mobfilters.
      CASE lst_mobfilters-field.
        WHEN lc_auart.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-order_type.
          CLEAR lst_filter_val.

        WHEN lc_aufnr.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-orderid.
          CLEAR lst_filter_val.

        WHEN lc_autyp.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-order_catg.
          CLEAR lst_filter_val.

        WHEN lc_bukrs.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-comp_code.
          CLEAR lst_filter_val.

        WHEN lc_idat1.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-date_release.
          CLEAR lst_filter_val.
        WHEN lc_idat2.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-date_completion.
          CLEAR lst_filter_val.
        WHEN lc_idat3.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-date_close.
          CLEAR lst_filter_val.
        WHEN lc_kokrs.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-co_area.
          CLEAR lst_filter_val.
        WHEN lc_persno_exception.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-persno_exception.
          CLEAR lst_filter_val.
        WHEN lc_pm_phase.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-pm_phase.
          CLEAR lst_filter_val.
        WHEN lc_assignment.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-assignment.
          CLEAR lst_filter_val.

        WHEN lc_werks.
          CALL METHOD me->get_user_plants
            EXPORTING
              ip_user           = gv_user
              ip_plant_category = lc_i
            RECEIVING
              et_plants         = lit_plants.
          IF lit_plants IS NOT INITIAL.
            APPEND LINES OF lit_plants TO es_filter_vals-plant.
          ENDIF.
          CLEAR lst_filter_val.
        WHEN lc_arbpl.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-work_cntr.
          CLEAR lst_filter_val.
        WHEN lc_objid.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-work_cntr.
          CLEAR lst_filter_val.
        WHEN lc_wo_excl_syst_stat.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-wo_excl_syst_stat.
          CLEAR lst_filter_val.

        WHEN lc_wo_incl_syst_stat.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-wo_incl_syst_stat.
          CLEAR lst_filter_val.
        WHEN lc_wo_excl_user_stat.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-wo_excl_user_stat.
          CLEAR lst_filter_val.
        WHEN lc_pernr.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-employee_id.
          CLEAR lst_filter_val.
        WHEN lc_oper_excl_syst_stat.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-oper_excl_syst_stat.
          CLEAR lst_filter_val.
        WHEN lc_oper_incl_syst_stat.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-oper_incl_syst_stat.
          CLEAR lst_filter_val.
        WHEN lc_oper_excl_user_stat.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-oper_excl_user_stat.
          CLEAR lst_filter_val.
        WHEN lc_ingpr.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-plangroup.
          CLEAR lst_filter_val.
        WHEN lc_iwerk.

          CALL METHOD me->get_user_plants
            EXPORTING
              ip_user           = gv_user
              ip_plant_category = lc_i
            RECEIVING
              et_plants         = lit_plants.

          IF lit_plants IS NOT INITIAL.
            APPEND LINES OF lit_plants TO es_filter_vals-planplant.
          ENDIF.
          CLEAR lst_filter_val.
        WHEN lc_hist_num.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-hist_num.
          CLEAR lst_filter_val.
        WHEN lc_pending_num.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-pending_num.
          CLEAR lst_filter_val.
        WHEN lc_histdur_num.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-histdur_num.
          CLEAR lst_filter_val.
        WHEN lc_pendingdur_num.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-pendingdur_num.
          CLEAR lst_filter_val.
        WHEN lc_hist_refdt_typ.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-hist_refdt_typ.
          CLEAR lst_filter_val.
        WHEN lc_woaddressnumber.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-woaddressnumber.
          CLEAR lst_filter_val.
        WHEN lc_stlty.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-bomcategory.
          CLEAR lst_filter_val.
        WHEN lc_stlal.
          lst_filter_val-sign = lst_mobfilters-sign.
          lst_filter_val-option = lst_mobfilters-options.
          lst_filter_val-low = lst_mobfilters-low.
          lst_filter_val-high = lst_mobfilters-high.
          APPEND lst_filter_val TO es_filter_vals-alternative.
          CLEAR lst_filter_val.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_mobile_filters_dynamic.
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*----------------------------------------------------------------------*
* Date        : 16/03/2021                                             *
* User ID     : CRAYABHARAM                                            *
* Request No  : ES1K902573                                             *
* Description : Get the dynamic mobile filter values                   *
*----------------------------------------------------------------------*
********************** CHANGE HISTORY **********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
************************************************************************

* ---------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* ---------------------------------------------------------------------*

* --- Internal Tables ---  --------------------------------------------*

    DATA: lst_filter_val TYPE /odsmfe/core_range_str,
          lit_filter_val TYPE /odsmfe/core_range_tab,
          lit_filters    TYPE STANDARD TABLE OF /odsmfe/tb_filtr,
          lst_filters    TYPE /odsmfe/tb_filtr,

          lit_plants     TYPE /odsmfe/core_range_tab,
          lit_workcenter TYPE /odsmfe/core_range_tab.

* DATA : xyz  TYPE TABLE OF  xyz ,
*xyz  TYPE TABLE OF  xyz .

* --- Variables ---  --------------------------------------------------*

    DATA : lv_dyn_filter   TYPE    /odsmfe/value.

* --- Object References ---  ------------------------------------------*

* DATA : xyz  TYPE REF TO  xyz ,
*xyz  TYPE REF TO  xyz .

* --- Data References ---  --------------------------------------------*

    DATA : lo_filter_values TYPE REF TO data.


* --- Field Symbols ---  ----------------------------------------------*

    FIELD-SYMBOLS: <lfsit_filter_val>    TYPE /odsmfe/core_range_tab,
                   <lfsst_filter_values> TYPE any,
                   <lfsst_field>         TYPE  /odsmfe/tb_filtr-field.
* ---------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* ---------------------------------------------------------------------*

* ---------------------------------------------------------------------------*
*            S T A R T   O F   C O N S T A N T S                             *
* ---------------------------------------------------------------------------*
    CONSTANTS: lc_dyn_filter TYPE string VALUE 'MOBILE_FILTER_DYNAMIC_LOGIC',
               lc_x          TYPE char1 VALUE 'X',
               lc_true       TYPE string VALUE 'TRUE',
               lc_werks      TYPE /odsmfe/de_mfe_fieldname VALUE 'WERKS',
               lc_iwerk      TYPE /odsmfe/de_mfe_fieldname VALUE 'IWERK',
               lc_arbpl      TYPE /odsmfe/de_mfe_fieldname VALUE 'ARBPL',
               lc_w          TYPE /odsmfe/plant_category VALUE 'W',
               lc_i          TYPE /odsmfe/plant_category VALUE 'I',
               lc_s          TYPE /odsmfe/plant_category VALUE 'S',
               lc_a          TYPE /odsmfe/wc_category VALUE 'A'.
* ---------------------------------------------------------------------------*
*            E N D   O F   C O N S T A N T S                                 *
* ---------------------------------------------------------------------------*

    IF es_filter_vals  IS SUPPLIED.

* Select all the filters from config table for the respective entityset

      SELECT mandt, entitysetname, tabname ,field, recordno, field_descr, sign ,options ,low, high, active
       FROM /odsmfe/tb_filtr
       WHERE  entitysetname = @iv_entity_set_name
       AND   active = @abap_on   INTO TABLE @lit_filters.

      IF sy-subrc EQ 0.

        "GET REFERENCE OF es_filter_vals INTO lo_filter_values.

        "ASSIGN lo_filter_values->* TO <lfsst_filter_values>.
        ASSIGN es_filter_vals TO <lfsst_filter_values>.

        IF <lfsst_filter_values> IS ASSIGNED.
          LOOP AT lit_filters INTO lst_filters.

            IF lst_filters-low IS NOT INITIAL.

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

            ELSE.

              CASE lst_filters-field.
                WHEN lc_werks.     "Plant

                  CALL METHOD me->get_user_plants
                    EXPORTING
                      ip_user           = gv_user
                      ip_plant_category = lc_w
                    RECEIVING
                      et_plants         = lit_plants.

                  es_filter_vals-plant =  lit_plants .
                WHEN lc_iwerk.

                  CALL METHOD me->get_user_plants
                    EXPORTING
                      ip_user           = gv_user
                      ip_plant_category = lc_i
                    RECEIVING
                      et_plants         = lit_plants.

                  es_filter_vals-planplant =  lit_plants .
              ENDCASE.

              CASE lst_filters-field.
                WHEN lc_arbpl.

                  CALL METHOD me->get_user_workcenter
                    EXPORTING
                      ip_user        = gv_user
                      ip_wc_category = lc_a
                    RECEIVING
                      et_workcenter  = lit_workcenter.

                  es_filter_vals-work_cntr  =  lit_workcenter .
              ENDCASE.
            ENDIF.
          ENDLOOP.
          es_filter_vals  = <lfsst_filter_values>.

        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_mobile_filter_values.
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*----------------------------------------------------------------------*
* Date        : 16/03/2021                                             *
* User ID     : CRAYABHARAM                                            *
* Request No  : ES1K902573                                             *
* Description : Get the mobile data filter values                      *
*----------------------------------------------------------------------*
********************** CHANGE HISTORY **********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
************************************************************************

* ---------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* ---------------------------------------------------------------------*

* --- Internal Tables ---  --------------------------------------------*

* DATA : xyz  TYPE TABLE OF  xyz ,
*xyz  TYPE TABLE OF  xyz .

* --- Variables ---  --------------------------------------------------*

    DATA : lv_dyn_filter   TYPE    /odsmfe/value.
*xyz  TYPE  xyz .

* --- Object References ---  ------------------------------------------*

* DATA : xyz  TYPE REF TO  xyz ,
*xyz  TYPE REF TO  xyz .

* --- Data References ---  --------------------------------------------*

* DATA : xyz  TYPE REF TO  xyz ,
*xyz  TYPE REF TO  xyz .

* --- Field Symbols ---  ----------------------------------------------*

* FIELD-SYMBOLS : <fs_ xyz>  TYPE  xyz ,
*<fs_ xyz>  TYPE  xyz .

* ---------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* ---------------------------------------------------------------------*

* ---------------------------------------------------------------------------*
*            S T A R T   O F   C O N S T A N T S                             *
* ---------------------------------------------------------------------------*
    CONSTANTS: lc_dyn_filter      TYPE string VALUE 'MOBILE_FILTER_DYNAMIC_LOGIC',
               lc_entity_set_name TYPE string VALUE 'WoHeaderSet',
               lc_x               TYPE char1 VALUE 'X',
               lc_true            TYPE string VALUE 'TRUE'.
* ---------------------------------------------------------------------------*
*            E N D   O F   C O N S T A N T S                                 *
* ---------------------------------------------------------------------------*


* Check dynamic mobile filter
    SELECT SINGLE param_value
          FROM /odsmfe/tb_apcon
               WHERE param_name = @lc_dyn_filter
          AND active = @lc_x  INTO @lv_dyn_filter.

    IF lv_dyn_filter EQ lc_true.

      CALL METHOD me->get_mobile_filters_dynamic
        EXPORTING
          iv_entity_set_name = lc_entity_set_name
        IMPORTING
          es_filter_vals     = gs_filter_vals.

    ELSE.

      CALL METHOD me->get_mobile_filter
        EXPORTING
          iv_entity_set_name = lc_entity_set_name
        IMPORTING
          es_filter_vals     = gs_filter_vals.

    ENDIF.

  ENDMETHOD.


  METHOD get_mobstat_filter.

    CONSTANTS: lc_entity_set_name TYPE string VALUE 'WoHeaderSet',
               lc_x               TYPE char1 VALUE 'X'.

    SELECT * FROM /odsmfe/tb_mstfl
     WHERE roleid = @gv_roleid
     AND entityset = @lc_entity_set_name
     AND objecttype = @lc_x
     AND active = @lc_x INTO TABLE @et_mobstatfilt .
  ENDMETHOD.


  METHOD get_selection_asgn_type_1.

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

    DATA: lst_valid_wo_parnr TYPE  gty_valid_wo_parnr.

    DATA: lv_aufnr     TYPE aufnr,
          lv_aufpl     TYPE /odsmfe/pm_longtext_str-objtype,
          lv_objnr(16) TYPE c,
          lv_auart(4)  TYPE c.

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.


*    CONSTANTS: lc_obtyp_ori   TYPE j_obtyp VALUE 'ORI'.

    CONSTANTS: lc_obtyp_ori(3)   TYPE c VALUE 'ORI'.
*soc by Viswa
*    SELECT DISTINCT aufk~aufnr,aufk~auart, aufk~objnr ,ihpa~parnr ,afih~priok
*      FROM aufk INNER JOIN afih ON aufk~aufnr = afih~aufnr
*                INNER JOIN ihpa ON aufk~objnr = ihpa~objnr    AND
*                                   ihpa~parvw = @gv_parvw      AND
*                                   ihpa~obtyp = @lc_obtyp_ori  AND
*                                   ihpa~kzloesch = @space
*             WHERE ihpa~parnr IN @me->pernr_merged
*             AND aufk~auart IN @gs_filter_vals-order_type
*             AND aufk~autyp IN @gs_filter_vals-order_catg
*             AND aufk~bukrs IN @gs_filter_vals-comp_code
*             AND aufk~werks IN @gs_filter_vals-plant
*             AND aufk~kokrs IN @gs_filter_vals-co_area
*             AND aufk~idat1 IN @gs_filter_vals-date_release
*             AND aufk~idat2 IN @gs_filter_vals-date_completion
*             AND aufk~idat3 IN @gs_filter_vals-date_close
*             AND aufk~loekz = @space INTO TABLE @et_valid_wo_parnr.                  ""#EC CI_BUFFJOIN

    call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).



    lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                         ( fieldname = 'AUART' )
                         ( fieldname = 'OBJNR' )
                               ).

    IF gs_filter_vals-order_type  IS NOT INITIAL.
      lt_options = VALUE #( ( text = |AUART| & | | & |{ gs_filter_vals-order_type[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_type[ 1 ]-low }| & |'|  ) ).
      DATA(lv_and) = 'AND'.
    ENDIF.

    IF gs_filter_vals-order_catg IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ gs_filter_vals-order_catg[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_catg[ 1 ]-low }| & |'| )  TO lt_options.
    ENDIF.

    IF gs_filter_vals-comp_code IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |BUKRS| & | | & |{ gs_filter_vals-comp_code[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-comp_code[ 1 ]-low }| & |'| )  TO lt_options.
    ENDIF.
    IF gs_filter_vals-plant IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO lt_options.
    ENDIF.
    IF gs_filter_vals-co_area IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |KOKRS| & | | & |{ gs_filter_vals-co_area[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-co_area[ 1 ]-low }| & |'| )  TO lt_options.
    ENDIF.
    IF gs_filter_vals-date_release IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT1| & | | & |{ gs_filter_vals-date_release[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_release[ 1 ]-low }| & |'| )  TO lt_options.
    ENDIF.
    IF gs_filter_vals-date_completion IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT2| & | | & |{ gs_filter_vals-date_completion[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_completion[ 1 ]-low }| & |'| )  TO lt_options.
    ENDIF.
    IF gs_filter_vals-date_close IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT3| & | | & |{ gs_filter_vals-date_close[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_close[ 1 ]-low }| & |'| )  TO lt_options.
    ENDIF.

    APPEND VALUE #( text = |{ lv_and }| & | | & |LOEKZ| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO lt_options.




    "Call RFC to get work orders
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'AUFK'
      TABLES
        options     = lt_options
        fields      = lt_fields
        data        = lt_data.

    LOOP AT lt_data INTO DATA(ls_Data).
      lst_valid_wo_parnr-aufnr = ls_data+0(12).
      lst_valid_wo_parnr-auart = ls_data+12(4).
      lst_valid_wo_parnr-objnr = ls_data+16(18).
    ENDLOOP.

    CLEAR: lt_fields,lt_options,lt_data,lv_and.

    lt_fields = VALUE #( ( fieldname = 'OBJNR' )
                           ( fieldname = 'PARNR' )
                           ).
    IF lv_objnr  IS NOT INITIAL.
      lt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lv_aufpl }| & |'| ) ).
      lv_and = 'AND'.
    ENDIF.
    IF me->pernr_merged IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |PARNR| & | | & |{ me->pernr_merged[ 1 ]-option }| & | | & |'| & |{ me->pernr_merged[ 1 ]-low }| & |'| )  TO lt_options.
    ENDIF.
    IF gv_parvw IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |PARVW| & | | & |EQ| & | | & |'| & |gv_parvw| & |'| ) TO lt_options.
    ENDIF.

    APPEND VALUE #( text = |{ lv_and }| & | | & |OBTYP| & | | & |EQ| & | | & |'| & |lc_obtyp_ori| & |'| ) TO lt_options.

    APPEND VALUE #( text = |{ lv_and }| & | | & |KZLOESCH| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO lt_options.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'IHPA'
      TABLES
        options     = lt_options
        fields      = lt_fields
        data        = lt_data.

    LOOP AT lt_data INTO ls_Data.

      IF lv_objnr = ls_data+0(18).
        lst_valid_wo_parnr-parnr = ls_data+18(12).

      ENDIF.
    ENDLOOP.

    CLEAR: lt_fields,lt_options,lt_data,lv_and.

    lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                              ( fieldname = 'PRIOK' )
                              ).

    IF lv_aufpl  IS NOT INITIAL.
      lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_valid_wo_parnr-aufnr }| & |'| ) ).
    ENDIF.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'AFIH'
      TABLES
        options     = lt_options
        fields      = lt_fields
        data        = lt_data.

    LOOP AT lt_data INTO ls_Data.

      IF lst_valid_wo_parnr-aufnr =  ls_data+0(12).
        lst_valid_wo_parnr-priok = ls_data+12(1).

      ENDIF.
    ENDLOOP.

    APPEND lst_valid_wo_parnr TO et_valid_wo_parnr.

*eoc by Viswa
  ENDMETHOD.


  METHOD get_selection_asgn_type_3.

***
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

    DATA: lst_valid_wo_parnr TYPE  gty_valid_wo_parnr.

    DATA: lv_aufnr     TYPE aufnr,
          lv_aufpl     TYPE /odsmfe/pm_longtext_str-objtype,
          lv_objnr(16) TYPE c,
          lv_auart(4)  TYPE c.

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.
***

    IF me->gewrk_merged IS NOT INITIAL.
*soc by Viswa

*      SELECT a~aufnr,a~auart ,a~objnr ,b~priok FROM aufk AS a
*                 INNER JOIN afih AS b ON a~aufnr = b~aufnr
*                   WHERE a~auart IN @gs_filter_vals-order_type
*                     AND a~autyp IN @gs_filter_vals-order_catg
*                     AND a~bukrs IN @gs_filter_vals-comp_code
*                     AND a~werks IN @gs_filter_vals-plant
*                     AND a~kokrs IN @gs_filter_vals-co_area
*                     AND a~idat1 IN @gs_filter_vals-date_release
*                     AND a~idat2 IN @gs_filter_vals-date_completion
*                     AND a~idat3 IN @gs_filter_vals-date_close
*                     AND a~loekz = @space
*                     AND b~gewrk IN @me->gewrk_merged INTO CORRESPONDING FIELDS OF TABLE @et_valid_wo_parnr.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

      lt_fields = VALUE #( ( fieldname = 'AUFNR' )
      ( fieldname = 'AUART' )
      ( fieldname = 'OBJNR' )
                                 ).
      IF gs_filter_vals-order_type  IS NOT INITIAL.
        lt_options = VALUE #( ( text = |AUART| & | | & |{ gs_filter_vals-order_type[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_type[ 1 ]-low }| & |'|  ) ).
        DATA(lv_and) = 'AND'.
      ENDIF.

      IF gs_filter_vals-order_catg IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ gs_filter_vals-order_catg[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_catg[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.

      IF gs_filter_vals-comp_code IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |BUKRS| & | | & |{ gs_filter_vals-comp_code[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-comp_code[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.
      IF gs_filter_vals-plant IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.
      IF gs_filter_vals-co_area IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |KOKRS| & | | & |{ gs_filter_vals-co_area[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-co_area[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.
      IF gs_filter_vals-date_release IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT1| & | | & |{ gs_filter_vals-date_release[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_release[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.
      IF gs_filter_vals-date_completion IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT2| & | | & |{ gs_filter_vals-date_completion[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_completion[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.
      IF gs_filter_vals-date_close IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT3| & | | & |{ gs_filter_vals-date_close[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_close[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.

      APPEND VALUE #( text = |{ lv_and }| & | | & |LOEKZ| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO lt_options.


      "Call RFC to get work order
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AUFK'
        TABLES
          options     = lt_options
          fields      = lt_fields
          data        = lt_data.

      LOOP AT lt_data INTO DATA(ls_Data).

        lst_valid_wo_parnr-aufnr = ls_data+0(12).
        lst_valid_wo_parnr-auart = ls_data+12(4).
        lst_valid_wo_parnr-objnr = ls_data+16(22).

      ENDLOOP.


      CLEAR: lt_fields,lt_options,lt_data,lv_and.

      lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                       ( fieldname = 'PRIOK' )
                             ).
      IF lst_valid_wo_parnr-aufnr IS NOT INITIAL.
        lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_valid_wo_parnr-aufnr }| & |'| ) ).
        lv_and = 'AND'.
      ENDIF.
      IF me->gewrk_merged IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ me->gewrk_merged[ 1 ]-option }| & | | & |'| & |{ me->gewrk_merged[ 1 ]-low }| & |'| )  TO lt_options.
      ENDIF.

      "Call RFC to get work order
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFIH'
        TABLES
          options     = lt_options
          fields      = lt_fields
          data        = lt_data.

      LOOP AT lt_data INTO ls_Data.

        IF lst_valid_wo_parnr-aufnr = ls_data+0(12).
          lst_valid_wo_parnr-priok = ls_data+12(1).

        ENDIF.

      ENDLOOP.

      APPEND lst_valid_wo_parnr TO et_valid_wo_parnr.

*eoc by Viswa
    ENDIF.

  ENDMETHOD.


  METHOD get_sel_asgn_type_2.
*soc by Viswa

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

    DATA: lst_wo_object  TYPE gty_wo_object,
          lst_valid_oper TYPE gty_afvc_key.

    DATA: lv_aufnr     TYPE aufnr,
          lv_aufpl     TYPE /odsmfe/pm_longtext_str-objtype,
          lv_objnr(16) TYPE c,
          lv_auart(4)  TYPE c.

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

*    SELECT DISTINCT a~aufpl b~aufnr b~auart b~objnr c~priok
*      FROM afko AS a INNER JOIN aufk AS b ON a~aufnr = b~aufnr
*                     INNER JOIN afih AS c ON a~aufnr = c~aufnr
*      INTO CORRESPONDING FIELDS OF TABLE et_wo_object
*      FOR ALL ENTRIES IN it_valid_oper
*      WHERE a~aufpl = it_valid_oper-aufpl
*        AND b~aufnr IN gs_filter_vals-orderid "Added by ODS
*        AND b~auart IN gs_filter_vals-order_type
*        AND b~autyp IN gs_filter_vals-order_catg
*        AND b~werks IN gs_filter_vals-plant
*        AND c~iphas IN gs_filter_vals-pm_phase
*        AND c~gewrk IN me->gewrk_merged
*        AND b~kokrs IN gs_filter_vals-co_area
*        AND b~bukrs IN gs_filter_vals-comp_code
*        AND b~idat1 IN gs_filter_vals-date_release
*        AND b~idat2 IN gs_filter_vals-date_completion
*        AND b~idat3 IN gs_filter_vals-date_close
*        AND b~loekz EQ space.                    ""#EC CI_NO_TRANSFORM

    IF it_valid_oper IS NOT INITIAL.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).


      LOOP AT it_valid_oper INTO lst_valid_oper.

        lt_fields = VALUE #( ( fieldname = 'AUFNR' )
    ( fieldname = 'AUFPL' )
                               ).

        lt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lst_valid_oper-aufpl }| & |'| ) ).


        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AFKO'
          TABLES
            options     = lt_options
            fields      = lt_fields
            data        = lt_data.

        LOOP AT lt_data INTO DATA(ls_Data).
          lv_aufnr = ls_data+0(12).
          lv_aufpl = ls_data+12(10).
        ENDLOOP.

        CLEAR: lt_fields,lt_options,lt_data.

        lt_fields = VALUE #( ( fieldname = 'AUFNR' )
     ( fieldname = 'AUART' )
     ( fieldname = 'OBJNR' )
                                ).
        IF lv_aufnr  IS NOT INITIAL.
          lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_aufnr }| & |'| ) ).
          DATA(lv_and) = 'AND'.
        ENDIF.

        IF gs_filter_vals-order_type  IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |AUART| & | | & |{ gs_filter_vals-order_type[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_type[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.

        IF gs_filter_vals-order_catg IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ gs_filter_vals-order_catg[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_catg[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.

        IF gs_filter_vals-comp_code IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |BUKRS| & | | & |{ gs_filter_vals-comp_code[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-comp_code[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.
        IF gs_filter_vals-plant IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.
        IF gs_filter_vals-co_area IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |KOKRS| & | | & |{ gs_filter_vals-co_area[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-co_area[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.
        IF gs_filter_vals-date_release IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT1| & | | & |{ gs_filter_vals-date_release[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_release[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.
        IF gs_filter_vals-date_completion IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT2| & | | & |{ gs_filter_vals-date_completion[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_completion[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.
        IF gs_filter_vals-date_close IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT3| & | | & |{ gs_filter_vals-date_close[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_close[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.

        APPEND VALUE #( text = |{ lv_and }| & | | & |LOEKZ| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO lt_options.


        "Call RFC to get work order
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AUFK'
          TABLES
            options     = lt_options
            fields      = lt_fields
            data        = lt_data.

        LOOP AT lt_data INTO ls_Data.
          IF lv_aufnr = ls_data+0(12).
            lst_wo_object-aufnr = ls_data+0(12).
            lst_wo_object-auart = ls_data+12(4).
            lst_wo_object-objnr = ls_data+16(18).
            lst_wo_object-aufpl = lv_aufpl.
          ENDIF.
        ENDLOOP.

        CLEAR: lt_fields,lt_options,lt_data,lv_and.

        lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                       ( fieldname = 'PRIOK' )
                             ).
        IF lv_aufnr IS NOT INITIAL.
          lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_aufnr }| & |'| ) ).
          lv_and = 'AND'.
        ENDIF.
        IF me->gewrk_merged IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ me->gewrk_merged[ 1 ]-option }| & | | & |'| & |{ me->gewrk_merged[ 1 ]-low }| & |'| )  TO lt_options.
        ENDIF.

        "Call RFC to get work order
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AFIH'
          TABLES
            options     = lt_options
            fields      = lt_fields
            data        = lt_data.

        LOOP AT lt_data INTO ls_Data.

          IF lv_aufnr = ls_data+0(12).
            lst_wo_object-priok = ls_data+12(1).

          ENDIF.

        ENDLOOP.

        APPEND lst_wo_object TO et_wo_object.
        CLEAR: lt_fields,lt_options,lt_data,lv_and,lst_wo_object,lv_aufnr,lv_aufpl.
      ENDLOOP.

    ENDIF.

* eoc by Viswa
  ENDMETHOD.


  METHOD get_sel_asgn_type_2_dlta.

    DATA: lit_valid_oper TYPE gtt_afvc_key.
*soc by Viswa

    DATA: lv_aufnr     TYPE aufnr,
          lv_aufpl     TYPE /odsmfe/pm_longtext_str-objtype,
          lv_objnr(16) TYPE c,
          lv_auart(4)  TYPE c.

    DATA: lst_wo_header  TYPE /odsmfe/cs_caufv_str,
          lst_valid_oper TYPE gty_afvc_key.

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

*  SELECT DISTINCT afvc~aufpl afvc~aplzl afvc~pernr FROM afko
*       INNER JOIN afvc ON afvc~aufpl = afko~aufpl
*       INNER JOIN afvv ON afvv~aufpl = afvc~aufpl
*       AND afvv~aplzl = afvc~aplzl
*       INTO TABLE et_valid_oper
*       FOR ALL ENTRIES IN it_workorder_header
*        WHERE afko~aufnr = it_workorder_header-aufnr
*          AND  afvc~pernr IN me->pernr_merged
*          AND   afvc~arbid IN me->arbid_merged
*          AND   afvc~aufpl NE space.                 ""#EC CI_NO_TRANSFORM

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).




    LOOP AT it_workorder_header INTO lst_wo_header.

      gt_fields = VALUE #( ( fieldname = 'AUFNR' )
   ( fieldname = 'AUFPL' )
                              ).

      gt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lst_wo_header-aufpl }| & |'| ) ).


      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFKO'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO DATA(ls_Data).
        lv_aufnr = ls_data+0(12).
        lv_aufpl = ls_data+12(10).
      ENDLOOP.

      CLEAR: gt_options, gt_fields, gt_data.

      Gt_fields = VALUE #( ( fieldname = 'AUFPL' )
                                ( fieldname = 'APLZL' )
                                ( fieldname = 'PERNR' )
                                ).
      IF lv_aufpl  IS NOT INITIAL.
        gt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lv_aufpl }| & |'| ) ).
        DATA(lv_and) = 'AND'.
      ENDIF.

      IF me->pernr_merged  IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |PERNR| & | | & |{ me->pernr_merged[ 1 ]-option }| & | | & |'| & |{ me->pernr_merged[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      IF me->arbid_merged  IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |ARBID| & | | & |{ me->arbid_merged[ 1 ]-option }| & | | & |'| & |{ me->arbid_merged[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.


      "Call RFC to get work order operations
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFVC'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.
        IF lv_aufpl = ls_Data+12(10).
          lst_valid_oper-aufpl = lv_aufpl.
          lst_valid_oper-aplzl = ls_data+10(8).
          lst_valid_oper-pernr = ls_data+18(8).
        ENDIF.

      ENDLOOP.

      APPEND lst_valid_oper TO et_valid_oper.

      CLEAR: gt_options, gt_fields, gt_data,lv_and,lv_aufpl,lv_aufnr.
    ENDLOOP.
* eoc by Viswa
  ENDMETHOD.


  METHOD get_sel_asgn_type_2_nondlta.
*soc by Viswa
*    SELECT DISTINCT aufpl aplzl pernr FROM afvc
*      INTO TABLE et_valid_oper
*      WHERE pernr IN me->pernr_merged
*      AND   arbid IN me->arbid_merged
*      AND   aufpl NE space.                      ""#EC CI_NO_TRANSFORM

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

    DATA:  lst_valid_oper TYPE gty_afvc_key.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

    Gt_fields = VALUE #( ( fieldname = 'AUFPL' )
                        ( fieldname = 'APLZL' )
                        ( fieldname = 'PERNR' )
                        ).

    IF me->pernr_merged  IS NOT INITIAL.
      gt_options = VALUE #( ( text = |PERNR| & | | & |{ me->pernr_merged[ 1 ]-option }| & | | & |'| & |{ me->pernr_merged[ 1 ]-low }| & |'|  ) ).
      DATA(lv_and) = 'AND'.
    ENDIF.

    IF me->arbid_merged  IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |ARBID| & | | & |{ me->arbid_merged[ 1 ]-option }| & | | & |'| & |{ me->arbid_merged[ 1 ]-low }| & |'| )  TO gt_options.
    ENDIF.

    APPEND VALUE #( text = |{ lv_and }| & | | & |AUFPL| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.
    "Call RFC to get work order operations
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'AFVC'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO DATA(ls_Data).
      lst_valid_oper-aufpl = ls_data+0(10).
      lst_valid_oper-aplzl = ls_data+10(8).
      lst_valid_oper-pernr = ls_data+18(8).

    ENDLOOP.

    APPEND lst_valid_oper TO et_valid_oper.

    CLEAR: gt_options, gt_fields, gt_data,lv_and.

*eoc by Viswa
  ENDMETHOD.


  METHOD get_sel_asgn_type_4.

*soc by Viswa

    DATA: lst_wo_object  TYPE gty_wo_object,
          lst_valid_oper TYPE gty_afvc_key.

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

    DATA: lv_aufnr TYPE aufnr,
          lv_aufpl TYPE /odsmfe/pm_longtext_str-objtype.

*    SELECT aufk~aufnr aufk~auart aufk~objnr afih~priok
*             FROM afko  INNER JOIN aufk ON afko~aufnr = aufk~aufnr
*             INNER JOIN afih  ON afko~aufnr = afih~aufnr
*             INTO CORRESPONDING FIELDS OF TABLE et_wo_object                   " WoPriority field fetch - Ysindhu - ES1K901939
*             FOR ALL ENTRIES IN it_valid_oper
*             WHERE afko~aufpl = it_valid_oper-aufpl AND
*                   aufk~aufnr IN gs_filter_vals-orderid  AND
*                   aufk~auart IN gs_filter_vals-order_type AND "Added by ODS
*                   aufk~autyp IN gs_filter_vals-order_catg AND
*                   aufk~werks IN gs_filter_vals-plant AND
*                   afih~iphas IN gs_filter_vals-pm_phase AND
*                   aufk~kokrs IN gs_filter_vals-co_area AND
*                   aufk~bukrs IN gs_filter_vals-comp_code AND
*                   aufk~idat1 IN gs_filter_vals-date_release AND
*                   aufk~idat2 IN gs_filter_vals-date_completion AND
*                   aufk~idat3 IN gs_filter_vals-date_close AND
*                   aufk~loekz EQ space.   ""#EC CI_NO_TRANSFORM ""#EC CI_BUFFJOIN

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).



    LOOP AT it_valid_oper INTO lst_valid_oper.

      gt_fields = VALUE #( ( fieldname = 'AUFNR' )
  ( fieldname = 'AUFPL' )
                             ).

      gt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lst_valid_oper-aufpl }| & |'| ) ).


      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFKO'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO DATA(ls_Data).
        lv_aufnr = ls_data+0(12).
        lv_aufpl = ls_data+12(10).
      ENDLOOP.

      CLEAR: gt_fields,gt_options,gt_data.

      gt_fields = VALUE #( ( fieldname = 'AUFNR' )
                          ( fieldname = 'AUART' )
                          ( fieldname = 'OBJNR' )
                                ).

      IF gs_filter_vals-orderid  IS NOT INITIAL.
        gt_options = VALUE #( ( text = |AUFNR| & | | & |{ gs_filter_vals-orderid[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-orderid[ 1 ]-low }| & |'|  ) ).
        DATA(lv_and) = 'AND'.
      ENDIF.

      IF gs_filter_vals-order_type IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |AUART| & | | & |{ gs_filter_vals-order_type[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_type[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      IF gs_filter_vals-order_catg IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ gs_filter_vals-order_catg[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_catg[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      IF gs_filter_vals-comp_code IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |BUKRS| & | | & |{ gs_filter_vals-comp_code[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-comp_code[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-plant IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-co_area IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |KOKRS| & | | & |{ gs_filter_vals-co_area[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-co_area[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-date_release IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT1| & | | & |{ gs_filter_vals-date_release[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_release[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-date_completion IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT2| & | | & |{ gs_filter_vals-date_completion[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_completion[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-date_close IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT3| & | | & |{ gs_filter_vals-date_close[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_close[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      APPEND VALUE #( text = |{ lv_and }| & | | & |LOEKZ| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.




      "Call RFC to get work orders
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AUFK'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.
        IF lv_aufnr = ls_data+0(12).

          lst_wo_object-aufnr = ls_data+0(12).
          lst_wo_object-auart = ls_data+12(4).
          lst_wo_object-objnr = ls_data+16(18).

        ENDIF.
      ENDLOOP.

      CLEAR: gt_fields,gt_options,gt_data,lv_and.

      gt_fields = VALUE #( ( fieldname = 'PRIOK' )
                             ).

      IF lv_aufnr  IS NOT INITIAL.
        gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_aufnr }| & |'| ) ).
        lv_and = 'AND'.
      ENDIF.

      IF gs_filter_vals-pm_phase IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IPHAS| & | | & |{ gs_filter_vals-pm_phase[ 1 ]-option }| & | | & |'| & | { gs_filter_vals-pm_phase[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFIH'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.

        lst_wo_object-priok = ls_data+0(1).

      ENDLOOP.

      APPEND lst_wo_object TO et_wo_object.

      CLEAR: gt_fields,gt_options,gt_data,lv_and.

    ENDLOOP.


*eoc by Viswa
  ENDMETHOD.


  METHOD get_sel_asgn_type_4_dlta.

    DATA: lit_valid_oper TYPE gtt_afvc_key.

    IF me->arbid_merged IS NOT INITIAL.

*soc by Viswa
      DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

      DATA:  lst_valid_oper TYPE gty_afvc_key.

      DATA: lv_aufnr     TYPE aufnr,
            lv_aufpl     TYPE /odsmfe/pm_longtext_str-objtype,
            lv_objnr(16) TYPE c,
            lv_auart(4)  TYPE c.

      DATA: lst_wo_header TYPE /odsmfe/cs_caufv_str.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

*      SELECT DISTINCT afvc~aufpl afvc~aplzl afvc~pernr FROM afko
*         INNER JOIN afvc ON afvc~aufpl = afko~aufpl
*         INNER JOIN afvv ON afvv~aufpl = afvc~aufpl AND
*                            afvv~aplzl = afvc~aplzl
*         INTO TABLE et_valid_oper
*         FOR ALL ENTRIES IN it_workorder_header
*          WHERE afko~aufnr = it_workorder_header-aufnr AND
*                afvc~arbid IN me->arbid_merged.
      ""#EC CI_NO_TRANSFORM

      LOOP AT it_workorder_header INTO lst_wo_header.

        gt_fields = VALUE #( ( fieldname = 'AUFNR' )
                            ( fieldname = 'AUFPL' )
                            ).


        IF lst_wo_header-aufnr  IS NOT INITIAL.
          gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_wo_header-aufnr }| & |'| ) ).
        ENDIF.

        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AFKO'
          TABLES
            options     = gt_options
            fields      = gt_fields
            data        = gt_data.

        LOOP AT gt_data INTO DATA(ls_Data).

          lv_aufnr = ls_data+0(12).
          lv_aufpl = ls_data+12(10).
        ENDLOOP.

        CLEAR : gt_fields,gt_options,gt_data.

        gt_fields = VALUE #( ( fieldname = 'AUFPL' )
       ( fieldname = 'APLZL' )
       ( fieldname = 'PERNR' )
                                  ).

        IF lv_aufnr  IS NOT INITIAL.
          gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_aufnr }| & |'| ) ).
          DATA(lv_and) = 'AND'.
        ENDIF.

        IF me->arbid_merged IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |ARBID| & | | & |{ me->arbid_merged[ 1 ]-option }| & | | & |'| & | { me->arbid_merged[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.


        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AFKO'
          TABLES
            options     = gt_options
            fields      = gt_fields
            data        = gt_data.

        LOOP AT gt_data INTO ls_Data.
          IF lv_aufpl = ls_data+0(10).
            lst_valid_oper-aufpl = ls_data+0(10).
            lst_valid_oper-aplzl = ls_data+10(8).
            lst_valid_oper-pernr = ls_data+18(8).
          ENDIF.
        ENDLOOP.

        APPEND lst_valid_oper TO  et_valid_oper.

        CLEAR : gt_fields,gt_options,gt_data,lv_and.
      ENDLOOP.

*eoc by Viswa
    ENDIF.
  ENDMETHOD.


  METHOD get_sel_asgn_type_4_nondlta.

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
*    SELECT DISTINCT aufpl aplzl pernr FROM afvc
*      INTO TABLE et_valid_oper
*      WHERE arbid IN me->arbid_merged
*      AND   aufpl NE space.
    ""#EC CI_NOFIRST

    DATA(low) =  me->arbid_merged .
    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.


   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

    lt_fields = VALUE #( ( fieldname = 'AUFPL' )
                   ( fieldname = 'APLZL' )
                   ( fieldname = 'PERNR' ) ).

    lt_options = VALUE #( ( text = |ARBID| & | | & |{ me->arbid_merged[ 1 ]-option }| & | | & |'| & |{ me->arbid_merged[ 1 ]-low }| & |'|  ) ).
    DATA(lv3_and) = 'AND'.

    APPEND VALUE #( text = |{ lv3_and }| & | | & |AUFPL| & | | & |NE| & | | & |'| & |' '| & |'| ) TO lt_options.



    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'AFVC'
      TABLES
        options     = lt_options
        fields      = lt_fields
        data        = lt_data.
    DATA: es_valid TYPE gty_afvc_key.

    LOOP AT lt_data INTO DATA(ls_Data).
      es_valid  = LS_data+0(8).
      APPEND es_valid  TO et_valid_oper.
      CLEAR es_valid .
    ENDLOOP.                  ""#EC CI_NO_TRANSFORM
  ENDMETHOD.


  METHOD get_sel_asgn_type_5.

  ENDMETHOD.


  METHOD get_sel_asgn_type_5_dlta.
*soc by Viswa

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

    DATA:  lst_valid_oper TYPE gty_afvc_key.

    DATA: lv_aufnr TYPE aufnr,
          lv_aufpl TYPE /odsmfe/pm_longtext_str-objtype.

    DATA: lst_wo_header TYPE /odsmfe/cs_caufv_str,
          lst_wo_object TYPE gty_wo_object.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

*    SELECT DISTINCT kbed~aufpl kbed~aplzl FROM afko
*            INNER JOIN afvc ON afvc~aufpl = afko~aufpl
*            INNER JOIN afvv ON afvv~aufpl = afvc~aufpl AND afvv~aplzl = afvc~aplzl
*            INNER JOIN kbed ON kbed~aufpl = afvv~aufpl AND kbed~aplzl = afvv~aplzl
*            INTO CORRESPONDING FIELDS OF TABLE et_valid_oper
*            FOR ALL ENTRIES IN it_workorder_header
*            WHERE afko~aufnr = it_workorder_header-aufnr
*            AND   kbed~pernr IN me->pernr_merged
*            AND   kbed~arbid IN me->arbid_merged
*            AND   kbed~typkz = 1.                      ""#EC CI_NO_TRANSFORM

*    IF sy-subrc EQ 0 AND et_valid_oper IS NOT INITIAL.
*      SELECT DISTINCT b~aufnr b~auart b~objnr c~priok
*      FROM afko AS a INNER JOIN aufk AS b ON a~aufnr = b~aufnr
*      INNER JOIN afih AS c ON a~aufnr = c~aufnr
*      INTO CORRESPONDING FIELDS OF TABLE et_wo_object                   " WoPriority field fetch - Ysindhu - ES1K901939
*      FOR ALL ENTRIES IN et_valid_oper
*      WHERE a~aufpl = et_valid_oper-aufpl
*      AND b~aufnr IN gs_filter_vals-orderid      "Added by ODS
*      AND b~auart IN gs_filter_vals-order_type
*      AND b~autyp IN gs_filter_vals-order_catg
*      AND b~werks IN gs_filter_vals-plant
*      AND c~iphas IN gs_filter_vals-pm_phase
*      AND c~gewrk IN me->gewrk_merged
*      AND b~kokrs IN gs_filter_vals-co_area
*      AND b~bukrs IN gs_filter_vals-comp_code
*      AND b~idat1 IN gs_filter_vals-date_release
*      AND b~idat2 IN gs_filter_vals-date_completion
*      AND b~idat3 IN gs_filter_vals-date_close
*      AND b~loekz EQ space.                    ""#EC CI_NO_TRANSFORM
*      IF sy-subrc EQ 0.
*        SORT et_wo_object BY aufnr.
*      ENDIF.
*    ENDIF.


    LOOP AT it_workorder_header INTO lst_wo_header.

      gt_fields = VALUE #( ( fieldname = 'AUFNR' )
                          ( fieldname = 'AUFPL' )
                          ).


      IF lst_wo_header-aufnr  IS NOT INITIAL.
        gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_wo_header-aufnr }| & |'| ) ).
      ENDIF.

      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFKO'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO DATA(ls_Data).

        lv_aufnr = ls_data+0(12).
        lv_aufpl = ls_data+12(10).
      ENDLOOP.

      CLEAR : gt_fields,gt_options,gt_data.


      gt_fields = VALUE #( ( fieldname = 'AUFPL' )
                          ( fieldname = 'APLZL' )

                         ).


      IF lv_aufpl  IS NOT INITIAL.
        gt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lv_aufpl }| & |'| ) ).
        DATA(lv_and) = 'AND'.
      ENDIF.

      IF me->pernr_merged IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |PERNR| & | | & |{ me->pernr_merged[ 1 ]-option }| & | | & |'| & | { me->pernr_merged[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      IF me->arbid_merged IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |ARBID| & | | & |{ me->arbid_merged[ 1 ]-option }| & | | & |'| & | { me->arbid_merged[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.


      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'KBED'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.

        lst_valid_oper-aufpl = ls_data+0(10).
        lst_valid_oper-aplzl = ls_data+10(8).

      ENDLOOP.

      APPEND lst_valid_oper TO et_valid_oper.

      CLEAR : gt_fields,gt_options,gt_data.
    ENDLOOP.

    LOOP AT et_valid_oper INTO lst_valid_oper.

      gt_fields = VALUE #( ( fieldname = 'AUFNR' )
( fieldname = 'AUFPL' )
                           ).

      gt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lst_valid_oper-aufpl }| & |'| ) ).


      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFKO'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.
        lv_aufnr = ls_data+0(12).
        lv_aufpl = ls_data+12(10).
      ENDLOOP.

      CLEAR: gt_fields,gt_options,gt_data.

      gt_fields = VALUE #( ( fieldname = 'AUFNR' )
                          ( fieldname = 'AUART' )
                          ( fieldname = 'OBJNR' )
                                ).

      IF gs_filter_vals-orderid  IS NOT INITIAL.
        gt_options = VALUE #( ( text = |AUFNR| & | | & |{ gs_filter_vals-orderid[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-orderid[ 1 ]-low }| & |'|  ) ).
        lv_and = 'AND'.
      ENDIF.

      IF gs_filter_vals-order_type IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |AUART| & | | & |{ gs_filter_vals-order_type[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_type[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      IF gs_filter_vals-order_catg IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ gs_filter_vals-order_catg[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_catg[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      IF gs_filter_vals-comp_code IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |BUKRS| & | | & |{ gs_filter_vals-comp_code[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-comp_code[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-plant IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-co_area IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |KOKRS| & | | & |{ gs_filter_vals-co_area[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-co_area[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-date_release IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT1| & | | & |{ gs_filter_vals-date_release[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_release[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-date_completion IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT2| & | | & |{ gs_filter_vals-date_completion[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_completion[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.
      IF gs_filter_vals-date_close IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT3| & | | & |{ gs_filter_vals-date_close[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_close[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      APPEND VALUE #( text = |{ lv_and }| & | | & |LOEKZ| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.




      "Call RFC to get work orders
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AUFK'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.
        IF lv_aufnr = ls_data+0(12).

          lst_wo_object-aufnr = ls_data+0(12).
          lst_wo_object-auart = ls_data+12(4).
          lst_wo_object-objnr = ls_data+16(18).

        ENDIF.
      ENDLOOP.

      CLEAR: gt_fields,gt_options,gt_data,lv_and.

      gt_fields = VALUE #( ( fieldname = 'PRIOK' )
                             ).

      IF lv_aufnr  IS NOT INITIAL.
        gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_aufnr }| & |'| ) ).
        lv_and = 'AND'.
      ENDIF.

      IF gs_filter_vals-pm_phase IS NOT INITIAL.
        APPEND VALUE #( text = |{ lv_and }| & | | & |IPHAS| & | | & |{ gs_filter_vals-pm_phase[ 1 ]-option }| & | | & |'| & | { gs_filter_vals-pm_phase[ 1 ]-low }| & |'| )  TO gt_options.
      ENDIF.

      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFIH'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.

        lst_wo_object-priok = ls_data+0(1).

      ENDLOOP.

      APPEND lst_wo_object TO et_wo_object.


      CLEAR : gt_fields,gt_options,gt_data.
    ENDLOOP.

    IF et_wo_object IS NOT INITIAL.
      SORT et_wo_object BY aufnr.
    ENDIF.
* eoc by Viswa
  ENDMETHOD.


  METHOD get_sel_asgn_type_5_nondlta.
* soc by Viswa

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

    DATA:  lst_valid_oper TYPE gty_afvc_key.

    DATA: lv_aufnr TYPE aufnr,
          lv_aufpl TYPE /odsmfe/pm_longtext_str-objtype.

    DATA: lst_wo_header TYPE /odsmfe/cs_caufv_str,
          lst_wo_object TYPE gty_wo_object.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

* SELECT DISTINCT aufpl ,aplzl FROM kbed          ""#EC CI_NOFIRST.
*           INTO CORRESPONDING FIELDS OF TABLE @et_valid_oper
*           WHERE pernr IN @me->pernr_merged
*           AND typkz =  1.                          ""#EC CI_NO_TRANSFORM
*
*    IF sy-subrc EQ 0 AND et_valid_oper IS NOT INITIAL.
*      SELECT DISTINCT aufk~aufnr aufk~auart aufk~objnr afih~priok
*      FROM afko INNER JOIN aufk ON afko~aufnr = aufk~aufnr
*      INNER JOIN afih ON afko~aufnr = afih~aufnr
*      INTO CORRESPONDING FIELDS OF TABLE et_wo_object                               " WoPriority field fetch - Ysindhu - ES1K901939
*      FOR ALL ENTRIES IN et_valid_oper
*      WHERE afko~aufpl = et_valid_oper-aufpl.  ""#EC CI_NO_TRANSFORM
*      IF sy-subrc EQ 0.
*        SORT et_wo_object BY aufnr.
*      ENDIF.
*    ENDIF.

    gt_fields = VALUE #( ( fieldname = 'AUFPL' )
                         ( fieldname = 'APLZL' )
                               ).

    IF me->pernr_merged  IS NOT INITIAL.
      gt_options = VALUE #( ( text = |PERNR| & | | & |{ me->pernr_merged[ 1 ]-option }| & | | & |'| & |{ me->pernr_merged[ 1 ]-low }| & |'|  ) ).
      DATA(lv_and) = 'AND'.
    ENDIF.

    APPEND VALUE #( text = |{ lv_and }| & | | & |TYPKZ| & | | & |EQ| & | | & |'| & | 1 | & |'| ) TO gt_options.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'KBED'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO DATA(ls_Data).

      lst_valid_oper-aufpl = ls_data+0(10).
      lst_valid_oper-aplzl = ls_data+10(8).

    ENDLOOP.

    APPEND lst_valid_oper TO et_valid_oper.

    CLEAR : gt_fields,gt_options,gt_data.

    LOOP AT et_valid_oper INTO lst_valid_oper.

      gt_fields = VALUE #( ( fieldname = 'AUFNR' )
( fieldname = 'AUFPL' )
                           ).

      gt_options = VALUE #( ( text = |AUFPL| & | | & |EQ| & | | & |'| & |{ lst_valid_oper-aufpl }| & |'| ) ).


      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFKO'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.
        lv_aufnr = ls_data+0(12).
        lv_aufpl = ls_data+12(10).
      ENDLOOP.
      CLEAR: gt_fields,gt_options,gt_data.

      gt_fields = VALUE #( ( fieldname = 'AUFNR' )
                        ( fieldname = 'AUART' )
                        ( fieldname = 'OBJNR' )
                          ).

      gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_aufnr }| & |'| ) ).


      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AUFK'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.
        IF lv_aufnr = ls_data+0(12).

          lst_wo_object-aufnr = ls_data+0(12).
          lst_wo_object-auart = ls_data+12(4).
          lst_wo_object-objnr = ls_data+16(18).

        ENDIF.
      ENDLOOP.

      CLEAR: gt_fields,gt_options,gt_data,lv_and.

      gt_fields = VALUE #( ( fieldname = 'PRIOK' )
                             ).

      IF lv_aufnr  IS NOT INITIAL.
        gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_aufnr }| & |'| ) ).

      ENDIF.

      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFIH'
        TABLES
          options     = gt_options
          fields      = gt_fields
          data        = gt_data.

      LOOP AT gt_data INTO ls_Data.

        lst_wo_object-priok = ls_data+0(1).

      ENDLOOP.

      APPEND lst_wo_object TO et_wo_object.

      CLEAR: gt_fields,gt_options,gt_data.
    ENDLOOP.

*eoc by Viswa
  ENDMETHOD.


  METHOD get_status_for_wo.

    DATA: lst_oper_object     TYPE /odsmfe/pm_oper_object_str,
          lit_wo_stat_obj_tmp TYPE STANDARD TABLE OF gty_wo_stat_obj,
          lit_wo_stat_obj     TYPE STANDARD TABLE OF gty_wo_stat_obj,
          lst_wo_stat_obj     TYPE gty_wo_stat_obj,
          lit_oper_objtmp     TYPE /odsmfe/pm_oper_object_tab,
          lst_oper_del        TYPE /odsmfe/pm_oper_object_str,
          lv_index            TYPE int8. " sytabix.

*soc by Viswa
    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.


   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

*    SELECT objnr ,stat ,inact FROM jest
*      INTO TABLE @lit_wo_stat_obj.

    gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                           ( fieldname = 'STAT' )
                           ( fieldname = 'INACT' )
                           ).

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'JEST'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO DATA(ls_data).
      lst_wo_stat_obj-objnr = ls_data+0(22).
      lst_wo_stat_obj-stat = ls_data+22(5).
      lst_wo_stat_obj-inact = ls_data+27(1).

      APPEND lst_wo_stat_obj TO  lit_wo_stat_obj.
    ENDLOOP.

    CLEAR: gt_fields, gt_options, gt_data.

* eoc by Viswa

    DELETE lit_wo_stat_obj_tmp WHERE inact NE space.

*    LOOP AT me->gt_oper_object INTO lst_oper_object.
*      READ TABLE lit_wo_stat_obj_tmp INTO lst_wo_stat_obj WITH KEY objnr = lst_oper_object-objnr.
*      IF sy-subrc = 0.
*        APPEND lst_wo_stat_obj TO lit_wo_stat_obj.
*      ENDIF.
*      CLEAR: lst_oper_object,lst_wo_stat_obj.
*    ENDLOOP.


    IF lit_wo_stat_obj IS NOT INITIAL.
      "oper_incl_syst_stat
      IF NOT gs_filter_vals-oper_incl_syst_stat IS INITIAL.
        lit_oper_objtmp[] = me->gt_oper_object[].
        CLEAR me->gt_oper_object.
        SORT lit_oper_objtmp BY objnr.
        LOOP AT lit_wo_stat_obj_tmp INTO lst_wo_stat_obj WHERE stat IN gs_filter_vals-oper_incl_syst_stat.
          READ TABLE lit_oper_objtmp INTO lst_oper_object
             WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
          IF sy-subrc = 0.
            APPEND lst_oper_object TO me->gt_oper_object.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF NOT gs_filter_vals-oper_excl_syst_stat IS INITIAL.
        SORT me->gt_oper_object BY objnr.
        LOOP AT lit_wo_stat_obj INTO lst_wo_stat_obj WHERE stat IN gs_filter_vals-oper_excl_syst_stat.
          READ TABLE me->gt_oper_object[] INTO lst_oper_del
            WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
          CLEAR lv_index.
          lv_index = sy-tabix.
          IF sy-subrc = 0.
            APPEND lst_oper_del TO me->gt_oper_del.
            DELETE me->gt_oper_object INDEX lv_index.
          ENDIF.
        ENDLOOP.
      ENDIF.


      IF NOT gs_filter_vals-oper_excl_user_stat IS INITIAL.
        SORT me->gt_oper_object BY objnr.
        LOOP AT lit_wo_stat_obj INTO lst_wo_stat_obj WHERE stat IN gs_filter_vals-oper_excl_user_stat.
          READ TABLE me->gt_oper_object[] INTO lst_oper_del
             WITH KEY objnr = lst_wo_stat_obj-objnr BINARY SEARCH.
          CLEAR lv_index.
          lv_index = sy-tabix.
          IF sy-subrc = 0.
            APPEND lst_oper_del TO me->gt_oper_del.
            DELETE me->gt_oper_object INDEX lv_index.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_unassigned_wo_hdr.

    TYPES: BEGIN OF lty_parnr_object,
             objnr TYPE j_objnr,
             parnr TYPE c LENGTH 12,
           END OF lty_parnr_object.

    DATA: lit_parnr_object TYPE TABLE OF lty_parnr_object,
          lst_parnr_object TYPE lty_parnr_object,
          lit_wo_object    TYPE gtt_wo_object,
          lst_wo_object    TYPE gty_wo_object,
          lv_tabix         TYPE sy-tabix.

*    CONSTANTS: lc_obtyp_ori           TYPE j_obtyp VALUE 'ORI'.
    CONSTANTS: lc_obtyp_ori(3)           TYPE c VALUE 'ORI'.

    IF ct_wo_object IS NOT INITIAL.

      lit_wo_object[] = ct_wo_object[].
      SORT lit_wo_object BY objnr.
      DELETE ADJACENT DUPLICATES FROM lit_wo_object COMPARING objnr.

* Get orders with assignments
*soc by Viswa
      DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

*  SELECT objnr, parnr
*      FROM ihpa
*      INTO TABLE @lit_parnr_object
*      FOR ALL ENTRIES IN @lit_wo_object
*      WHERE objnr EQ @lit_wo_object-objnr
*      AND   parvw EQ @gv_parvw
*      AND   obtyp EQ @lc_obtyp_ori
*      AND   kzloesch EQ @space.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

      LOOP AT lit_wo_object INTO lst_wo_object.

        gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                             ( fieldname = 'PARNR' )
                               ).

        gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_wo_object-objnr }| & |'| ) ).
        DATA(lv_and) = 'AND'.

        IF gv_parvw IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |PARVW| & | | & |EQ| & | | & |'| & | gv_parvw | & |'| ) TO gt_options.
        ENDIF.
        IF lc_obtyp_ori IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |OBTYP| & | | & |EQ| & | | & |'| & | lc_obtyp_ori | & |'| ) TO gt_options.
        ENDIF.

        APPEND VALUE #( text = |{ lv_and }| & | | & |KZLOESCH| & | | & |EQ| & | | & |'| & | space | & |'| ) TO gt_options.

        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AUFK'
          TABLES
            options     = gt_options
            fields      = gt_fields
            data        = gt_data.

        LOOP AT gt_data INTO DATA(ls_Data).

          lst_parnr_object-objnr = ls_data+0(22).
          lst_parnr_object-parnr = ls_data+22(12).

        ENDLOOP.

        APPEND lst_parnr_object TO lit_parnr_object.

        CLEAR: gt_fields,gt_options,gt_data.

      ENDLOOP.

* eoc by Viswa
* If no entries found, no data is available for the seleciton criteria
      IF sy-subrc NE 0.
        CLEAR ct_wo_object[].
      ELSE.
* Remove orders with assignments from response.
        SORT lit_parnr_object BY objnr.
        LOOP AT ct_wo_object INTO lst_wo_object.
          lv_tabix =  sy-tabix.
          READ TABLE lit_parnr_object INTO lst_parnr_object WITH KEY objnr = lst_wo_object-objnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            DELETE ct_wo_object INDEX lv_tabix .
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_unassigned_wo_opr.

    DATA: lst_pernr_range LIKE LINE OF me->pernr_merged,
          lit_valid_oper  TYPE gtt_afvc_key,
          lst_valid_oper  TYPE gty_afvc_key,
          lst_wo_object   TYPE gty_wo_object,
          lv_tabix        TYPE sy-tabix.

    lst_pernr_range-sign = 'I'.
    lst_pernr_range-option = 'EQ'.
    APPEND lst_pernr_range TO me->pernr_merged.
    CLEAR lst_pernr_range.

* soc by Viswa
    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

*    SELECT DISTINCT aufpl aplzl pernr
*     FROM afvc                                         "#EC CI_NOFIELD
*      INTO TABLE lit_valid_oper
*      WHERE pernr IN me->pernr_merged
*      AND   aufpl NE space.
    gt_fields = VALUE #( ( fieldname = 'AUFPL' )
                             ( fieldname = 'APLZL' )
                             ( fieldname = 'PERNR' )
                               ).

    IF me->pernr_merged  IS NOT INITIAL.
      gt_options = VALUE #( ( text = |PERNR| & | | & |{ me->pernr_merged[ 1 ]-option }| & | | & |'| & |{ me->pernr_merged[ 1 ]-low }| & |'|  ) ).
    ENDIF.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'AFVC'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO DATA(ls_Data).

      lst_valid_oper-aufpl = ls_data+0(10).
      lst_valid_oper-aplzl = ls_data+10(8).
      lst_valid_oper-pernr = ls_data+18(8).

    ENDLOOP.

    APPEND lst_valid_oper TO lit_valid_oper.

    CLEAR: gt_fields,gt_options,gt_data.
*eoc by Viswa

    IF lit_valid_oper IS NOT INITIAL.
      SORT lit_valid_oper BY aufpl.
      LOOP AT ct_wo_object INTO lst_wo_object.
        lv_tabix =  sy-tabix.
        READ TABLE lit_valid_oper INTO lst_valid_oper WITH KEY aufpl = lst_wo_object-aufpl BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE ct_wo_object INDEX lv_tabix .
        ENDIF.
      ENDLOOP.
      gt_valid_oper[] = lit_valid_oper[].
    ENDIF.

  ENDMETHOD.


  METHOD get_user_plants.
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*----------------------------------------------------------------------*
* Date        : 16/03/2021                                             *
* User ID     : CRAYABHARAM                                            *
* Request No  : ES1K902573                                             *
* Description : Get user plants                                        *
*----------------------------------------------------------------------*
********************** CHANGE HISTORY **********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
************************************************************************

    TYPES: BEGIN OF ltys_usr05,
             parva(40) TYPE c, " usr05-parva,
           END OF ltys_usr05.

    DATA: lv_parameter_id(20) TYPE c, " memoryid,
          lv_parva(40)        TYPE c, " xuvalue,

          lt_parva            TYPE TABLE OF ltys_usr05,
          lst_parva           TYPE ltys_usr05,
          lst_plant           TYPE /odsmfe/core_range_str.

    CONSTANTS: lc_w       TYPE /odsmfe/plant_category VALUE 'W',
               lc_i       TYPE /odsmfe/plant_category VALUE 'I',
               lc_s       TYPE /odsmfe/plant_category VALUE 'S',
               lc_wrk(20) TYPE c VALUE 'WRK',
               lc_iwk(20) TYPE c VALUE 'IWK',
               lc_swk(20) TYPE c VALUE 'SWK',
               lc_sign_i  TYPE bapisign VALUE 'I',
               lc_eq      TYPE bapioption VALUE 'EQ'.

    CASE ip_plant_category.
      WHEN lc_w.
        lv_parameter_id = lc_wrk.
      WHEN lc_i.
        lv_parameter_id = lc_iwk.
      WHEN lc_s.
        lv_parameter_id = lc_swk.
    ENDCASE.

*  Getting user parameter value

*soc by Viswa

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

    call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

*    SELECT SINGLE parva
*    FROM usr05
*    WHERE  bname = @ip_user AND parid = @lv_parameter_id INTO @lv_parva.

    gt_fields = VALUE #( ( fieldname = 'PARVA' )
                           ).

    IF ip_user  IS NOT INITIAL.
      gt_options = VALUE #( ( text = |BNAME| & | | & |EQ| & | | & |'| & |{ ip_user }| & |'| ) ).
      DATA(lv_and) = 'AND'.
    ENDIF.

    IF lv_parameter_id IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |PARID| & | | & |EQ| & | | & |'| & | { lv_parameter_id } | & |'| ) TO gt_options.
    ENDIF.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'USR05'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO DATA(ls_Data).

      lv_parva = ls_data+0(40).

    ENDLOOP.

    CLEAR: gt_fields,gt_options,gt_data.
*eoc by Viswa

    IF sy-subrc = 0 AND lv_parva IS NOT INITIAL.
      TRANSLATE lv_parva TO UPPER CASE.
      SPLIT lv_parva AT ',' INTO TABLE lt_parva.

      LOOP AT lt_parva INTO lst_parva.
        lst_plant-sign   = lc_sign_i.
        lst_plant-option = lc_eq.
        CONDENSE lst_parva-parva .
        lst_plant-low    = lst_parva-parva.
        APPEND lst_plant TO et_plants .
        CLEAR lst_plant.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_user_workcenter.

    CONSTANTS: lc_a       TYPE /odsmfe/wc_category VALUE 'A',
               lc_v       TYPE /odsmfe/wc_category VALUE 'V',
               lc_agr(20) TYPE c VALUE 'AGR',
               lc_vap(20) TYPE c VALUE 'VAP',
               lc_sign_i  TYPE bapisign VALUE 'I',
               lc_eq      TYPE bapioption VALUE 'EQ'.

    TYPES: BEGIN OF ltys_usr05,
             parva(40) TYPE c, " usr05-parva,
           END OF ltys_usr05.

    DATA :lt_parva            TYPE TABLE OF ltys_usr05,
          lst_parva           TYPE ltys_usr05,
          lst_wcenter         TYPE /odsmfe/core_range_str,

          lv_parva(40)        TYPE c, " xuvalue,
          lv_parameter_id(20) TYPE c. " memoryid.

    CASE ip_wc_category.
      WHEN lc_a.
        lv_parameter_id = lc_agr.
      WHEN lc_v.
        lv_parameter_id = lc_vap.
    ENDCASE.

*  Getting user parameter value
*soc by Viswa

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

*    SELECT SINGLE parva
*    FROM usr05
*    WHERE  bname = @ip_user AND parid = @lv_parameter_id INTO @lv_parva.

    gt_fields = VALUE #( ( fieldname = 'PARVA' )
                           ).

    IF ip_user  IS NOT INITIAL.
      gt_options = VALUE #( ( text = |BNAME| & | | & |EQ| & | | & |'| & |{ ip_user }| & |'| ) ).
      DATA(lv_and) = 'AND'.
    ENDIF.

    IF lv_parameter_id IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |PARID| & | | & |EQ| & | | & |'| & | { lv_parameter_id } | & |'| ) TO gt_options.
    ENDIF.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'USR05'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO DATA(ls_Data).

      lv_parva = ls_data+0(40).

    ENDLOOP.

    CLEAR: gt_fields,gt_options,gt_data.

*eoc by Viswa
    IF sy-subrc = 0 AND lv_parva IS NOT INITIAL.
      TRANSLATE lv_parva TO UPPER CASE.
      SPLIT lv_parva AT ',' INTO TABLE lt_parva.

      LOOP AT lt_parva INTO lst_parva.
        lst_wcenter-sign   = lc_sign_i.
        lst_wcenter-option = lc_eq.
        CONDENSE lst_parva-parva .
        lst_wcenter-low    = lst_parva-parva.
        APPEND lst_wcenter TO et_workcenter .
        CLEAR lst_wcenter.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_workorder_common.
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*----------------------------------------------------------------------*
* Date        : 16/03/2021                                             *
* User ID     : CRAYABHARAM                                            *
* Request No  : ES1K902573                                             *
* Description : This method fetches the details of workorder. This is  *
*               a copy of the mJC method, and has been further         *
*               cleaned up and performance optimised                   *
*----------------------------------------------------------------------*
********************** CHANGE HISTORY **********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
************************************************************************

* ---------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* ---------------------------------------------------------------------*

* --- Internal Tables ---  --------------------------------------------*

    DATA :
      lst_filter           TYPE /odsmfe/mgw_select_option_str, "/iwbep/s_mgw_select_option,
      lit_created_wo_parnr TYPE gtt_valid_wo_parnr,
      lit_wo_object        TYPE gtt_wo_object.

* --- Variables ---  --------------------------------------------------*

    DATA : lv_parvw TYPE c LENGTH 2 ."parvw.

* --- Object References ---  ------------------------------------------*

* --- Data References ---  --------------------------------------------*

* --- Field Symbols ---  ----------------------------------------------*

    FIELD-SYMBOLS: <fs_range_str>    TYPE /odsmfe/core_range_str.
* ---------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* ---------------------------------------------------------------------*

    CONSTANTS: lc_sign_i TYPE bapisign VALUE 'I',
               lc_eq     TYPE bapioption VALUE 'EQ'.

    CALL METHOD me->get_mobile_filter_values.

*Read partner function
    SORT gs_filter_vals-parvw BY sign option.
    READ TABLE gs_filter_vals-parvw ASSIGNING <fs_range_str>
         WITH KEY sign = lc_sign_i
                  option = lc_eq .
    IF sy-subrc = 0.
      "MOVE- <fs_range_str>-low TO gv_parvw.
      gv_parvw = <fs_range_str>-low .
      UNASSIGN <fs_range_str>.
    ENDIF.


    IF it_filter_select_options IS NOT INITIAL.
      CALL METHOD me->add_filter_select_options
        EXPORTING
          it_filter_select_options = it_filter_select_options.
    ENDIF.


* Get Assignment
    TRY.
        CALL METHOD me->get_assignment_type
          EXPORTING
            iv_mobileuser = iv_mobileuser.

*      CATCH /iwbep/cx_mgw_busi_exception .
    ENDTRY.

*******************************************************************************************************
* Multiple outcomes are possible based on the query parameters / select option as detailed below
* 1. Work Orders created by Login user
* 2. Online search
*     a.Unassigned
*     b.Assigned
* 3. Non Online Search Scenario
*******************************************************************************************************

    CALL METHOD me->wo_by_login_user
      EXPORTING
        it_workorder_header = it_workorder_header
        iv_mobileuser       = iv_mobileuser
      IMPORTING
        et_created_wo_parnr = lit_created_wo_parnr.

    IF lit_created_wo_parnr IS NOT INITIAL.
      APPEND LINES OF lit_created_wo_parnr TO lit_wo_object.
      DELETE ADJACENT DUPLICATES FROM lit_wo_object COMPARING ALL FIELDS.
    ENDIF.


    IF gv_onlinesearch IS NOT INITIAL.

      CALL METHOD me->wo_online_search
        IMPORTING
          et_wo_object = lit_wo_object.

    ELSE.
      TRY.
          CALL METHOD me->wo_non_online_search
            EXPORTING
              iv_mobileuser       = iv_mobileuser
              it_workorder_header = it_workorder_header
            IMPORTING
              et_wo_object        = lit_wo_object.
*        CATCH /iwbep/cx_mgw_busi_exception.
      ENDTRY.
    ENDIF.

    IF lit_wo_object IS INITIAL.
      RETURN.
    ENDIF.

* Adjust WO based on mobile status
    CALL METHOD me->adjust_wo_stat_filters
      CHANGING
        ct_wo_object = lit_wo_object.


* Adjust WO based on Operation status and compare mobile status filters

    CALL METHOD me->adjust_wo_oper_stat_filters
      CHANGING
        ct_wo_object = lit_wo_object.


    CALL METHOD me->populate_output_data
      EXPORTING
        it_wo_object = lit_wo_object.



  ENDMETHOD.


  METHOD get_workorder_detail.
  ENDMETHOD.


  METHOD get_workorder_longtext.
  ENDMETHOD.


  METHOD populate_output_data.

    DATA: lst_wo_object   TYPE gty_wo_object,
          lst_aufnr_delta TYPE /odsmfe/pm_valid_aufnr_str.

    IF it_wo_object IS NOT INITIAL.
      LOOP AT it_wo_object INTO lst_wo_object.
        MOVE-CORRESPONDING lst_wo_object TO lst_aufnr_delta.
        APPEND lst_aufnr_delta TO me->gt_aufnr_delta.
        CLEAR: lst_aufnr_delta, lst_wo_object.
      ENDLOOP.

    ENDIF.

    IF me->gt_aufnr_delta IS NOT INITIAL.
* Sort the data & delete Duplicate records
      SORT  me->gt_aufnr_delta[] BY aufnr.
      DELETE ADJACENT DUPLICATES FROM  me->gt_aufnr_delta[] COMPARING aufnr.

* Populate person responsible
      CALL METHOD me->populate_person_responsible.

    ENDIF.
    gt_aufnr_delta1[] = me->gt_aufnr_delta[].

  ENDMETHOD.


  METHOD populate_person_responsible.

    DATA: lit_wo_object    TYPE gtt_wo_object,
          lst_wo_object    TYPE gty_wo_object,
          lit_parnr_object TYPE TABLE OF gty_parnr_object,
          lst_parnr_object TYPE gty_parnr_object,
          lst_aufnr_parnr  TYPE /odsmfe/pm_aufnr_parnr_str.

*    CONSTANTS: lc_obtyp_ori           TYPE j_obtyp VALUE 'ORI'.
    CONSTANTS: lc_obtyp_ori(3)           TYPE c VALUE 'ORI'.

    IF me->gt_aufnr_delta IS NOT INITIAL.
*soc by Viswa
      DATA: lr_rfc          TYPE REF TO /odsmfe/cl_get_ent_super_bapi,
            lst_aufnr_delta TYPE /odsmfe/pm_valid_aufnr_str.


   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).


*      SELECT aufnr auart objnr
*        FROM aufk
*        INTO CORRESPONDING FIELDS OF TABLE lit_wo_object
*        FOR ALL ENTRIES IN me->gt_aufnr_delta
*        WHERE aufnr EQ me->gt_aufnr_delta-aufnr.

      LOOP AT gt_aufnr_delta INTO lst_aufnr_delta.

        gt_fields = VALUE #( ( fieldname = 'AUFNR' )
                                 ( fieldname = 'AUART' )
                                 ( fieldname = 'OBJNR' )
                                   ).

        gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_aufnr_delta-aufnr }| & |'| ) ).

        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AUFK'
          TABLES
            options     = gt_options
            fields      = gt_fields
            data        = gt_data.

        LOOP AT gt_data INTO DATA(ls_Data).
          lst_wo_object-aufnr = ls_data+0(12).
          lst_wo_object-auart = ls_data+12(4).
          lst_wo_object-objnr = ls_data+16(18).
        ENDLOOP.

        APPEND lst_wo_object TO lit_wo_object.

        CLEAR: gt_fields,gt_options,gt_data.
      ENDLOOP.

*eoc by Viswa

      IF sy-subrc = 0.
        SORT lit_wo_object BY objnr.
        DELETE ADJACENT DUPLICATES FROM lit_wo_object COMPARING objnr.
*soc by Viswa

*  SELECT objnr, parnr
*      FROM ihpa
*      INTO TABLE @lit_parnr_object
*      FOR ALL ENTRIES IN @lit_wo_object
*      WHERE objnr EQ @lit_wo_object-objnr
*      AND   parvw EQ @gv_parvw
*      AND   obtyp EQ @lc_obtyp_ori
*      AND   kzloesch EQ @space.


        LOOP AT lit_wo_object INTO lst_wo_object.

          gt_fields = VALUE #( ( fieldname = 'OBJNR' )
                               ( fieldname = 'PARNR' )
                                 ).

          gt_options = VALUE #( ( text = |OBJNR| & | | & |EQ| & | | & |'| & |{ lst_wo_object-objnr }| & |'| ) ).
          DATA(lv_and) = 'AND'.

          IF gv_parvw IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |PARVW| & | | & |EQ| & | | & |'| & | gv_parvw | & |'| ) TO gt_options.
          ENDIF.
          IF lc_obtyp_ori IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |OBTYP| & | | & |EQ| & | | & |'| & | lc_obtyp_ori | & |'| ) TO gt_options.
          ENDIF.

          APPEND VALUE #( text = |{ lv_and }| & | | & |KZLOESCH| & | | & |EQ| & | | & |'| & | space | & |'| ) TO gt_options.

          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'AUFK'
            TABLES
              options     = gt_options
              fields      = gt_fields
              data        = gt_data.

          LOOP AT gt_data INTO ls_Data.

            lst_parnr_object-objnr = ls_data+0(22).
            lst_parnr_object-parnr = ls_data+22(12).

          ENDLOOP.

          APPEND lst_parnr_object TO lit_parnr_object.

          CLEAR: gt_fields,gt_options,gt_data.

        ENDLOOP.

* eoc by Viswa

        IF sy-subrc EQ 0.
          SORT lit_parnr_object BY objnr.
          LOOP AT lit_wo_object INTO lst_wo_object.
            READ TABLE lit_parnr_object INTO lst_parnr_object
            WITH KEY objnr = lst_wo_object-objnr
            BINARY SEARCH.
            IF sy-subrc EQ 0.
              lst_aufnr_parnr-aufnr = lst_wo_object-aufnr.
              lst_aufnr_parnr-parnr = lst_parnr_object-parnr.
              APPEND lst_aufnr_parnr TO me->gt_aufnr_parnr.
              CLEAR lst_aufnr_parnr.
            ENDIF.
          ENDLOOP.
          SORT me->gt_aufnr_parnr BY aufnr.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD populate_range_data.

    CONSTANTS:lc_work_center_objty  TYPE c LENGTH 2 VALUE 'A'."pm_objty

    DATA: lst_range TYPE /odsmfe/core_range_str.
* SOC BY VISWA

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

*    SELECT DISTINCT objid AS low FROM crhd
*         INTO CORRESPONDING FIELDS OF TABLE @ct_range_tab
*         WHERE arbpl = @iv_parva AND
*               objty = @lc_work_center_objty AND
*               begda LE @sy-datum AND
*               endda GE @sy-datum AND
*               werks IN @gs_filter_vals-plant.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

    gt_fields = VALUE #( ( fieldname = 'OBJID' )
                           ).
    IF iv_parva IS NOT INITIAL.
      gt_options = VALUE #( ( text = |ARBPL| & | | & |EQ| & | | & |'| & |{ iv_parva }| & |'| ) ).
      DATA(lv_and) = 'AND'.
    ENDIF.

    APPEND VALUE #( text = |{ lv_and }| & | | & |BEGDA| & | | & |LE| & | | & |'| & | sy-datum | & |'| ) TO gt_options.
    APPEND VALUE #( text = |{ lv_and }| & | | & |ENDDA| & | | & |GE| & | | & |'| & | sy-datum | & |'| ) TO gt_options.

    IF gs_filter_vals-plant IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO gt_options.
    ENDIF.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'CRHD'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO DATA(ls_Data).
      lst_range-sign = 'I'.
      lst_range-option = 'EQ'.
      lst_range-low = ls_data+0(10).
      APPEND lst_range TO ct_range_tab.
    ENDLOOP.



*    IF sy-subrc EQ 0.
*      lst_range-sign = 'I'.
*      lst_range-option = 'EQ'.
*      MODIFY ct_range_tab FROM lst_range
*      TRANSPORTING sign option WHERE sign = space.   ""#EC CI_STDSEQ
*    ENDIF.

    CLEAR: gt_fields,gt_options,gt_data.
*EOC BY VISWA
  ENDMETHOD.


  METHOD wo_by_login_user.


    DATA:lv_createdby_wo      TYPE char1,
         lit_created_wo_parnr TYPE STANDARD TABLE OF gty_valid_wo_parnr.

    DATA: lst_wo_header        TYPE /odsmfe/cs_caufv_str,
          lst_created_wo_parnr TYPE gty_valid_wo_parnr.
    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

    CONSTANTS:lc_createdby_wo TYPE string VALUE 'DOWNLOAD_CREATEDBY_WO',
              lc_x            TYPE char1 VALUE 'X'.

    SELECT SINGLE param_value
      FROM /odsmfe/tb_apcon
      WHERE param_name = @lc_createdby_wo AND
            active EQ @lc_x
            INTO @lv_createdby_wo.

    IF lv_createdby_wo IS NOT INITIAL.
*soc by Viswa

*      IF it_workorder_header IS NOT INITIAL.
*        SELECT DISTINCT aufk~aufnr, aufk~auart, aufk~objnr, afih~priok FROM aufk
*        INNER JOIN afih ON aufk~aufnr = afih~aufnr
*        INTO TABLE @DATAet_created_wo_parnr
*        FOR ALL ENTRIES IN @it_workorder_header
*        WHERE aufk~aufnr = @it_workorder_header-aufnr
*        AND aufk~ernam EQ @iv_mobileuser
*        AND aufk~auart IN @gs_filter_vals-order_type
*        AND aufk~autyp IN @gs_filter_vals-order_catg
*        AND aufk~bukrs IN @gs_filter_vals-comp_code
*        AND aufk~werks IN @gs_filter_vals-plant
*        AND aufk~kokrs IN @gs_filter_vals-co_area
*        AND aufk~idat1 IN @gs_filter_vals-date_release
*        AND aufk~idat2 IN @gs_filter_vals-date_completion
*        AND aufk~idat3 IN @gs_filter_vals-date_close
*        AND aufk~loekz = @space.
*      ELSE.
*        SELECT DISTINCT aufk~aufnr aufk~auart aufk~objnr afih~priok FROM aufk
*         INNER JOIN afih ON aufk~aufnr = afih~aufnr
*         INTO TABLE et_created_wo_parnr
*         WHERE aufk~ernam EQ iv_mobileuser
*         AND aufk~aufnr IN gs_filter_vals-orderid
*         AND aufk~auart IN gs_filter_vals-order_type
*         AND aufk~autyp IN gs_filter_vals-order_catg
*         AND aufk~bukrs IN gs_filter_vals-comp_code
*         AND aufk~werks IN gs_filter_vals-plant
*         AND aufk~kokrs IN gs_filter_vals-co_area
*         AND aufk~idat1 IN gs_filter_vals-date_release
*         AND aufk~idat2 IN gs_filter_vals-date_completion
*         AND aufk~idat3 IN gs_filter_vals-date_close
*         AND aufk~loekz = space.
*      ENDIF.
   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

      IF it_workorder_header IS NOT INITIAL.
        LOOP AT it_workorder_header INTO lst_wo_header.

          gt_fields = VALUE #( ( fieldname = 'AUFNR' )
       ( fieldname = 'AUART' )
       ( fieldname = 'OBJNR' )
                                  ).

          gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_wo_header-aufnr }| & |'| ) ).
          DATA(lv_and) = 'AND'.

          APPEND VALUE #( text = |{ lv_and }| & | | & |ERNAM| & | | & |EQ| & | | & |'| & |iv_mobileuser| & |'| ) TO gt_options.

          IF gs_filter_vals-order_type IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |AUART| & | | & |{ gs_filter_vals-order_type[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_type[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.

          IF gs_filter_vals-order_catg IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ gs_filter_vals-order_catg[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_catg[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.

          IF gs_filter_vals-comp_code IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |BUKRS| & | | & |{ gs_filter_vals-comp_code[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-comp_code[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.
          IF gs_filter_vals-plant IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.
          IF gs_filter_vals-co_area IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |KOKRS| & | | & |{ gs_filter_vals-co_area[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-co_area[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.
          IF gs_filter_vals-date_release IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT1| & | | & |{ gs_filter_vals-date_release[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_release[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.
          IF gs_filter_vals-date_completion IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT2| & | | & |{ gs_filter_vals-date_completion[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_completion[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.
          IF gs_filter_vals-date_close IS NOT INITIAL.
            APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT3| & | | & |{ gs_filter_vals-date_close[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_close[ 1 ]-low }| & |'| )  TO gt_options.
          ENDIF.

          APPEND VALUE #( text = |{ lv_and }| & | | & |LOEKZ| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'AUFK'
            TABLES
              options     = gt_options
              fields      = gt_fields
              data        = gt_data.

          LOOP AT gt_data INTO DATA(ls_Data).

            lst_created_wo_parnr-aufnr = ls_data+0(12).
            lst_created_wo_parnr-auart = ls_data+12(4).
            lst_created_wo_parnr-objnr = ls_data+16(18).
          ENDLOOP.

          CLEAR: gt_fields,gt_options,gt_data,lv_and.

          gt_fields = VALUE #( ( fieldname = 'PRIOK' )
                                 ).

          IF lst_wo_header-aufnr  IS NOT INITIAL.
            gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_wo_header-aufnr }| & |'| ) ).
          ENDIF.

          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'AFIH'
            TABLES
              options     = gt_options
              fields      = gt_fields
              data        = gt_data.

          LOOP AT gt_data INTO ls_Data.

            lst_created_wo_parnr-priok = ls_data+0(1).

          ENDLOOP.


          APPEND lst_created_wo_parnr TO et_created_wo_parnr.

          CLEAR: gt_fields,gt_options,gt_data,lv_and.
        ENDLOOP.
      ELSE.
        gt_fields = VALUE #( ( fieldname = 'AUFNR' )
       ( fieldname = 'AUART' )
       ( fieldname = 'OBJNR' )
                                  ).

        gt_options = VALUE #( ( text = |ERNAM| & | | & |EQ| & | | & |'| & |{ iv_mobileuser }| & |'| ) ).
        lv_and = 'AND'.

        IF gs_filter_vals-orderid IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |AUFNR| & | | & |{ gs_filter_vals-orderid[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-orderid[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.

        IF gs_filter_vals-order_type IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |AUART| & | | & |{ gs_filter_vals-order_type[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_type[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.

        IF gs_filter_vals-order_catg IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |AUTYP| & | | & |{ gs_filter_vals-order_catg[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-order_catg[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.

        IF gs_filter_vals-comp_code IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |BUKRS| & | | & |{ gs_filter_vals-comp_code[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-comp_code[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.
        IF gs_filter_vals-plant IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.
        IF gs_filter_vals-co_area IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |KOKRS| & | | & |{ gs_filter_vals-co_area[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-co_area[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.
        IF gs_filter_vals-date_release IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT1| & | | & |{ gs_filter_vals-date_release[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_release[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.
        IF gs_filter_vals-date_completion IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT2| & | | & |{ gs_filter_vals-date_completion[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_completion[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.
        IF gs_filter_vals-date_close IS NOT INITIAL.
          APPEND VALUE #( text = |{ lv_and }| & | | & |IDAT3| & | | & |{ gs_filter_vals-date_close[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-date_close[ 1 ]-low }| & |'| )  TO gt_options.
        ENDIF.

        APPEND VALUE #( text = |{ lv_and }| & | | & |LOEKZ| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

        "Call RFC to get work orders
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AUFK'
          TABLES
            options     = gt_options
            fields      = gt_fields
            data        = gt_data.

        LOOP AT gt_data INTO ls_Data.

          lst_created_wo_parnr-aufnr = ls_data+0(12).
          lst_created_wo_parnr-auart = ls_data+12(4).
          lst_created_wo_parnr-objnr = ls_data+16(18).

          CLEAR: gt_fields,gt_options,gt_data,lv_and.

          gt_fields = VALUE #( ( fieldname = 'PRIOK' )
                                 ).

          IF lst_created_wo_parnr-aufnr  IS NOT INITIAL.
            gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_created_wo_parnr-aufnr }| & |'| ) ).


            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'AFIH'
              TABLES
                options     = gt_options
                fields      = gt_fields
                data        = gt_data.

            LOOP AT gt_data INTO ls_Data.

              lst_created_wo_parnr-priok = ls_data+0(1).

            ENDLOOP.

          ENDIF.
          APPEND lst_created_wo_parnr TO et_created_wo_parnr.

          CLEAR: gt_fields,gt_options,gt_data,lv_and.


        ENDLOOP.
        CLEAR: gt_fields,gt_options,gt_data,lv_and.

      ENDIF.

*eoc by Viswa
      SORT et_created_wo_parnr BY aufnr.
    ENDIF.

  ENDMETHOD.


  METHOD wo_non_online_search.
    DATA: lv_oper_filter       TYPE flag,
          lv_parid(20)         TYPE  c, "memoryid,
          lv_parva(40)         TYPE c, "xuvalue,
          lst_pernr_range      LIKE LINE OF me->pernr_merged,
          lit_workorder_header TYPE /odsmfe/cs_caufv_tab,
          lit_valid_wo_parnr   TYPE gtt_valid_wo_parnr,
          lit_valid_oper       TYPE gtt_afvc_key.

    CONSTANTS: lc_1           TYPE /odsmfe/pm_assignment_type_dte VALUE '1',
               lc_2           TYPE /odsmfe/pm_assignment_type_dte VALUE '2',
               lc_3           TYPE /odsmfe/pm_assignment_type_dte VALUE '3',
               lc_4           TYPE /odsmfe/pm_assignment_type_dte VALUE '4',
               lc_5           TYPE /odsmfe/pm_assignment_type_dte VALUE '5',
               lc_pernr_dummy TYPE /odsmfe/pm_longtext_str-aplzl VALUE '99999999',
               lc_head(20)    TYPE c VALUE 'VAP',
               lc_op(20)      TYPE c VALUE 'AGR'.

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

    CLEAR lv_oper_filter.

    DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.




    if NOT ( gs_filter_vals-oper_incl_syst_stat IS INITIAL
       AND   gs_filter_vals-oper_excl_syst_stat IS INITIAL
       AND   gs_filter_vals-oper_incl_user_stat IS INITIAL
      AND    gs_filter_vals-oper_excl_user_stat IS INITIAL ).
    lv_oper_filter = abap_true.
  ENDIF.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

  IF me->assignment_type EQ lc_1 OR
     me->assignment_type EQ lc_2 OR
     me->assignment_type EQ lc_5.

    lst_pernr_range-sign = 'I'.
    lst_pernr_range-option = 'EQ'.

* get communication user
*      SELECT pernr AS low FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @me->pernr_merged
*            WHERE usrid = @iv_mobileuser               ""#EC CI_NOFIRST
*            AND begda LE @sy-datum
*            AND endda GE @sy-datum .                   ""#EC CI_NOFIRST

    .

    lt_fields = VALUE #( ( fieldname = 'PERNR' )
                    ).

    lt_options = VALUE #( ( text = |USRID| & | | & |EQ| & | | & |'| & |{ iv_mobileuser }| & |'| ) ).
    DATA(lv3_and) = 'AND'.

    APPEND VALUE #( text = |{ lv3_and }| & | | & |BEGDA| & | | & |EQ| & | | & |'| & |sy-datum| & |'| ) TO lt_options.
    APPEND VALUE #( text = |{ lv3_and }| & | | & |ENDDA| & | | & |EQ| & | | & |'| & |sy-datum| & |'| ) TO lt_options.


    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'PA0105'
      TABLES
        options     = lt_options
        fields      = lt_fields
        data        = lt_data.
    DATA: low TYPE n LENGTH 8.

    LOOP AT lt_data INTO DATA(ls_Data).
      low = LS_data+0(8).
      APPEND low TO me->pernr_merged.
      CLEAR low.
    ENDLOOP.

    IF sy-subrc EQ 0.
      MODIFY me->pernr_merged FROM lst_pernr_range TRANSPORTING sign option WHERE sign = space.  ""#EC CI_STDSEQ
    ELSE.
      lst_pernr_range-low = lc_pernr_dummy.
      APPEND lst_pernr_range TO me->pernr_merged.
    ENDIF.
  ENDIF.

  IF me->assignment_type EQ lc_3 OR
     me->assignment_type EQ lc_4.

    IF me->assignment_type = lc_3.
      lv_parid = lc_head.
    ELSE.
      lv_parid = lc_op.
    ENDIF.
*soc by Viswa

*      SELECT SINGLE parva FROM usr05  WHERE bname = @iv_mobileuser AND parid EQ @lv_parid INTO @lv_parva.

    gt_fields = VALUE #( ( fieldname = 'PARVA' )
                           ).

    IF iv_mobileuser  IS NOT INITIAL.
      gt_options = VALUE #( ( text = |BNAME| & | | & |EQ| & | | & |'| & |{ iv_mobileuser }| & |'| ) ).
      DATA(lv_and) = 'AND'.
    ENDIF.

    IF lv_parid IS NOT INITIAL.
      APPEND VALUE #( text = |{ lv_and }| & | | & |PARID| & | | & |EQ| & | | & |'| & | { lv_parid } | & |'| ) TO gt_options.
    ENDIF.
  call method ME->get_cloud_dest
    IMPORTING
      ex_dest = lv_rfc.
*  DATA(lv_top)     = im_request->get_paging( )->get_page_size( ).
*  DATA(lv_skip)    = im_request->get_paging( )->get_offset( ).

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'USR05'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO ls_Data.

      lv_parva = ls_data+0(40).

    ENDLOOP.

    CLEAR: gt_fields,gt_options,gt_data.


*eoc by Viswa
    IF sy-subrc NE 0 AND lv_parva IS INITIAL.
*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            textid  = /iwbep/cx_mgw_busi_exception=>business_error
*            message = text-001.
    ELSEIF sy-subrc =  0 AND lv_parva IS NOT INITIAL.
      TRANSLATE lv_parva TO UPPER CASE.
      IF lv_parid = lc_head.

        CALL METHOD me->populate_range_data
          EXPORTING
            iv_parva     = lv_parva
          CHANGING
            ct_range_tab = me->gewrk_merged.

      ELSEIF lv_parid = lc_op.
        CALL METHOD me->populate_range_data
          EXPORTING
            iv_parva     = lv_parva
          CHANGING
            ct_range_tab = me->arbid_merged.
      ENDIF.
    ENDIF.

  ENDIF.

*    lit_workorder_header = it_workorder_header.

  CASE me->assignment_type.
    WHEN lc_1.

      CALL METHOD me->get_selection_asgn_type_1
        IMPORTING
          et_valid_wo_parnr = lit_valid_wo_parnr.

      CALL METHOD me->get_data_asgn_type_1
        EXPORTING
          it_workorder_header = it_workorder_header
        CHANGING
          ct_valid_wo_parnr   = lit_valid_wo_parnr.

      et_wo_object[] = lit_valid_wo_parnr[].

    WHEN lc_2.

      CALL METHOD me->get_data_asgn_type_2
        EXPORTING
          it_workorder_header = it_workorder_header
        IMPORTING
          et_wo_object        = et_wo_object.

    WHEN lc_3.

      CALL METHOD me->get_selection_asgn_type_3
        IMPORTING
          et_valid_wo_parnr = lit_valid_wo_parnr.

      CALL METHOD me->get_data_asgn_type_3
        EXPORTING
          it_workorder_header = it_workorder_header
        CHANGING
          ct_valid_wo_parnr   = lit_valid_wo_parnr.

      et_wo_object[] = lit_valid_wo_parnr[].

    WHEN lc_4.
      CALL METHOD me->get_data_asgn_type_4
        EXPORTING
          it_workorder_header = it_workorder_header
        IMPORTING
          et_wo_object        = et_wo_object.
    WHEN lc_5.
      CALL METHOD me->get_data_asgn_type_5
        EXPORTING
          it_workorder_header = it_workorder_header
        IMPORTING
          et_wo_object        = et_wo_object.
    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


METHOD wo_online_search.

  DATA: lit_wo_object TYPE gtt_wo_object,
        lst_wo_object TYPE gty_wo_object.

  CONSTANTS: lc_1 TYPE /odsmfe/pm_assignment_type_dte VALUE '1',
             lc_2 TYPE /odsmfe/pm_assignment_type_dte VALUE '2'.


*soc by Viswa

*    SELECT DISTINCT a~aufpl b~aufnr b~auart b~objnr          ""#EC CI_NOORDER
*       FROM afko AS a INNER JOIN aufk AS b ON a~aufnr = b~aufnr
*       INNER JOIN afih AS c ON a~aufnr = c~aufnr
*       INTO TABLE lit_wo_object
*       WHERE b~aufnr IN gs_filter_vals-orderid
*       AND   b~erdat IN gs_filter_vals-created_on
*       AND   b~werks IN gs_filter_vals-plant
*       AND   b~vaplz IN gs_filter_vals-work_cntr
*       AND   b~loekz = space
*       AND   c~priok IN gs_filter_vals-priority "Added by ODS
*       AND   c~equnr IN gs_filter_vals-equnr
*       AND   c~iloan IN gs_filter_vals-tplnr.
  DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.

   call method ME->get_cloud_dest
    IMPORTING
      ex_dest = data(lv_rfc).

  gt_fields = VALUE #( ( fieldname = 'AUFNR' )
( fieldname = 'AUART' )
( fieldname = 'OBJNR' )
                          ).


  IF gs_filter_vals-orderid  IS NOT INITIAL.
    gt_options = VALUE #( ( text = |AUFNR| & | | & |{ gs_filter_vals-orderid[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-orderid[ 1 ]-low }| & |'|  ) ).
    DATA(lv_and) = 'AND'.
  ENDIF.

  IF gs_filter_vals-created_on IS NOT INITIAL.
    APPEND VALUE #( text = |{ lv_and }| & | | & |ERDAT| & | | & |{ gs_filter_vals-created_on[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-created_on[ 1 ]-low }| & |'| )  TO gt_options.
  ENDIF.

  IF gs_filter_vals-plant IS NOT INITIAL.
    APPEND VALUE #( text = |{ lv_and }| & | | & |WERKS| & | | & |{ gs_filter_vals-plant[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-plant[ 1 ]-low }| & |'| )  TO gt_options.
  ENDIF.
  IF gs_filter_vals-work_cntr IS NOT INITIAL.
    APPEND VALUE #( text = |{ lv_and }| & | | & |VAPLZ| & | | & |{ gs_filter_vals-work_cntr[ 1 ]-option }| & | | & |'| & |{ gs_filter_vals-work_cntr[ 1 ]-low }| & |'| )  TO gt_options.
  ENDIF.


  APPEND VALUE #( text = |{ lv_and }| & | | & |LOEKZ| & | | & |EQ| & | | & |'| & |SPACE| & |'| ) TO gt_options.

  "Call RFC to get work orders
  CALL FUNCTION 'RFC_READ_TABLE'
    DESTINATION lv_rfc
    EXPORTING
      query_table = 'AUFK'
    TABLES
      options     = gt_options
      fields      = gt_fields
      data        = gt_data.

  LOOP AT gt_data INTO DATA(ls_Data).

    lst_wo_object-aufnr = ls_data+0(12).
    lst_wo_object-auart = ls_data+12(4).
    lst_wo_object-objnr = ls_data+16(18).

    CLEAR: gt_fields,gt_options,gt_data,lv_and.


    gt_fields = VALUE #( ( fieldname = 'AUFPL' )
                            ).

    gt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_wo_object-aufnr }| & |'| ) ).

    "Call RFC to get work orders
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'AUFK'
      TABLES
        options     = gt_options
        fields      = gt_fields
        data        = gt_data.

    LOOP AT gt_data INTO ls_Data.

      lst_wo_object-aufpl = ls_data+0(10).

    ENDLOOP.

    APPEND lst_wo_object TO lit_wo_object.

  ENDLOOP.


  CLEAR: gt_fields,gt_options,gt_data,lv_and.

*eoc by Viswa

  IF gv_unassigned IS INITIAL.
* No operation required, sorting happening towards end of method.
  ELSE.
    IF lit_wo_object IS NOT INITIAL.
      CASE me->assignment_type.
        WHEN lc_1.
* Get work orders which do not have a partner assigned as the assignment is at header level.

          CALL METHOD me->get_unassigned_wo_hdr
            CHANGING
              ct_wo_object = lit_wo_object.

        WHEN lc_2.
* Get work orders whose operations do not have a partner assigned as the assignment is at operation level.
          CALL METHOD me->get_unassigned_wo_opr
            CHANGING
              ct_wo_object = lit_wo_object.

        WHEN OTHERS.
      ENDCASE.
    ENDIF.
    SORT lit_wo_object BY aufnr.
  ENDIF.
  et_wo_object[] = lit_wo_object[].
ENDMETHOD.
ENDCLASS.
