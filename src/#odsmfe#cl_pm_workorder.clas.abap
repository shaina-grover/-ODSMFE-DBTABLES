class /ODSMFE/CL_PM_WORKORDER definition
  public
  inheriting from /ODSMFE/CL_PM_WORKORDER_ABS
  final
  create public .

public section.

  interfaces /ODSMFE/IF_CREATE_INSTANCE .

  data GV_ONLINESEARCH type FLAG .
  data GV_UNASSIGNED type FLAG .
  data GT_AUFNR_DELTA1 type /ODSMFE/PM_VALID_AUFNR_TAB .
  class-data GOSB_OBJ type ref to /ODSMFE/CL_MODEL_FACTORY .

  methods DYNAMIC_JOIN_CLAUSE
    returning
      value(RE_FROM) type STRING .
  methods GET_WORKORDER_COMMON
    importing
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional
      !IV_MOBILEUSER type STRING optional
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB optional
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .

  methods GET_WORKORDER_DETAIL
    redefinition .
  methods GET_WORKORDER_LONGTEXT
    redefinition .
protected section.

  constants GC_OPTION_EQ type BAPIOPTION value 'EQ' ##NO_TEXT.
  data GS_FILTER_VALS type /ODSMFE/WM_FILTER_VALS .
  constants GC_SIGN_I type BAPISIGN value 'I' ##NO_TEXT.
  data GV_USER type USNAM .
  data GV_PARVW type PARVW .
  data GV_ROLEID type /ODSMFE/ROLEID .
  constants GC_OPTION_BT type BAPIOPTION value 'BT' ##NO_TEXT.
private section.

  types:
    BEGIN OF gty_parnr_object,
           objnr TYPE ihpa-objnr,
           parnr TYPE ihpa-parnr,
         END OF gty_parnr_object .
  types:
    BEGIN OF gty_valid_wo_parnr,
      aufnr TYPE aufnr,
      auart TYPE aufart,
      objnr TYPE  j_objnr,
      parnr TYPE i_parnr,
      priok TYPE priok,
    END OF gty_valid_wo_parnr .
  types:
    gtt_valid_wo_parnr TYPE STANDARD TABLE OF gty_valid_wo_parnr .
  types:
    BEGIN OF gty_wo_object,
      aufpl TYPE afko-aufpl,
      aufnr TYPE aufk-aufnr,
      auart TYPE aufk-auart,
      objnr TYPE aufk-objnr,
      parnr TYPE i_parnr,
      priok TYPE afih-priok,
    END OF gty_wo_object .
  types:
    gtt_wo_object TYPE STANDARD TABLE OF gty_wo_object .
  types:
    BEGIN OF gty_afvc_key,
      aufpl TYPE co_aufpl,
      aplzl TYPE co_aplzl,
      pernr TYPE co_pernr,
    END OF gty_afvc_key .
  types:
    gtt_afvc_key TYPE STANDARD TABLE OF gty_afvc_key .
  types:
    gtt_mobstatfilt TYPE STANDARD TABLE OF /odsmfe/tb_mstfl .
  types:
    BEGIN OF gty_wo_stat_obj,
      objnr TYPE jest-objnr,
      stat  TYPE j_status,
      inact TYPE j_inact,
    END OF gty_wo_stat_obj .

  data GT_VALID_OPER type GTT_AFVC_KEY .
  data GT_OPER_DEL type /ODSMFE/PM_OPER_OBJECT_TAB .

  methods ADD_FILTER_SELECT_OPTIONS
    importing
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION .
  methods GET_ASSIGNMENT_TYPE
    importing
      !IV_MOBILEUSER type STRING
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_DATA_ASGN_TYPE_1
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB
    changing
      !CT_VALID_WO_PARNR type GTT_VALID_WO_PARNR .
  methods GET_DATA_ASGN_TYPE_2
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB
    exporting
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_DATA_ASGN_TYPE_3
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB
    changing
      !CT_VALID_WO_PARNR type GTT_VALID_WO_PARNR
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_DATA_ASGN_TYPE_4
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB
    exporting
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_DATA_ASGN_TYPE_5
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB
    exporting
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_MOBILE_FILTER
    importing
      !IV_ENTITY_SET_NAME type STRING optional
    exporting
      !ES_FILTER_VALS type /ODSMFE/WM_FILTER_VALS .
  methods GET_MOBILE_FILTERS_DYNAMIC
    importing
      !IV_ENTITY_SET_NAME type STRING optional
    exporting
      !ES_FILTER_VALS type /ODSMFE/WM_FILTER_VALS .
  methods GET_MOBILE_FILTER_VALUES .
  methods GET_SELECTION_ASGN_TYPE_1
    exporting
      value(ET_VALID_WO_PARNR) type GTT_VALID_WO_PARNR .
  methods GET_SELECTION_ASGN_TYPE_3
    exporting
      value(ET_VALID_WO_PARNR) type GTT_VALID_WO_PARNR .
  methods GET_SEL_ASGN_TYPE_2
    importing
      !IT_VALID_OPER type GTT_AFVC_KEY
    exporting
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_SEL_ASGN_TYPE_2_DLTA
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB
    exporting
      !ET_VALID_OPER type GTT_AFVC_KEY .
  methods GET_SEL_ASGN_TYPE_2_NONDLTA
    exporting
      !ET_VALID_OPER type GTT_AFVC_KEY .
  methods GET_SEL_ASGN_TYPE_4
    importing
      !IT_VALID_OPER type GTT_AFVC_KEY
    exporting
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_SEL_ASGN_TYPE_4_DLTA
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB
    exporting
      !ET_VALID_OPER type GTT_AFVC_KEY .
  methods GET_SEL_ASGN_TYPE_4_NONDLTA
    exporting
      !ET_VALID_OPER type GTT_AFVC_KEY .
  methods GET_SEL_ASGN_TYPE_5
    importing
      !IT_VALID_OPER type GTT_AFVC_KEY
    exporting
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_SEL_ASGN_TYPE_5_DLTA
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB
    exporting
      !ET_VALID_OPER type GTT_AFVC_KEY
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_SEL_ASGN_TYPE_5_NONDLTA
    exporting
      !ET_VALID_OPER type GTT_AFVC_KEY
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_UNASSIGNED_WO_HDR
    changing
      !CT_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_UNASSIGNED_WO_OPR
    changing
      !CT_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_USER_PLANTS
    importing
      !IP_USER type SYUNAME
      !IP_PLANT_CATEGORY type /ODSMFE/PLANT_CATEGORY default 'W'
    returning
      value(ET_PLANTS) type /ODSMFE/CORE_RANGE_TAB .
  methods GET_USER_WORKCENTER
    importing
      !IP_USER type SYUNAME
      !IP_WC_CATEGORY type /ODSMFE/WC_CATEGORY optional
    returning
      value(ET_WORKCENTER) type /ODSMFE/CORE_RANGE_TAB .
  methods POPULATE_RANGE_DATA
    importing
      !IV_PARVA type XUVALUE optional
    changing
      !CT_RANGE_TAB type /ODSMFE/CORE_RANGE_TAB .
  methods WO_BY_LOGIN_USER
    importing
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB optional
      !IV_MOBILEUSER type STRING optional
    exporting
      value(ET_CREATED_WO_PARNR) type GTT_VALID_WO_PARNR .
  methods WO_NON_ONLINE_SEARCH
    importing
      !IV_MOBILEUSER type STRING
      !IT_WORKORDER_HEADER type /ODSMFE/CS_CAUFV_TAB optional
    exporting
      !ET_WO_OBJECT type GTT_WO_OBJECT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods WO_ONLINE_SEARCH
    exporting
      !ET_WO_OBJECT type GTT_WO_OBJECT .
  methods GET_MOBSTAT_FILTER
    exporting
      !ET_MOBSTATFILT type GTT_MOBSTATFILT .
  methods ADJUST_WO_STAT_FILTERS
    changing
      !CT_WO_OBJECT type GTT_WO_OBJECT .
  methods ADJUST_WO_OPER_STAT_FILTERS
    changing
      !CT_WO_OBJECT type GTT_WO_OBJECT .
  methods POPULATE_OUTPUT_DATA
    importing
      !IT_WO_OBJECT type GTT_WO_OBJECT .
  methods POPULATE_PERSON_RESPONSIBLE .
  methods GET_STATUS_FOR_WO .
ENDCLASS.



CLASS /ODSMFE/CL_PM_WORKORDER IMPLEMENTATION.


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
      SELECT SINGLE entityset
             clsname
        FROM /odsmfe/tb_entcl
        INTO CORRESPONDING FIELDS OF /odsmfe/if_create_instance~gs_class_config
        WHERE entityset EQ im_entity.
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
          MESSAGE lv_mesg TYPE lc_i.
      ENDTRY.

      CHECK lo_obj IS BOUND.
      re_obj ?= lo_obj.

    ENDIF.

  ENDMETHOD.


  METHOD add_filter_select_options.

    TYPES: BEGIN OF lty_iloa,
             iloan TYPE iloan,
             tplnr TYPE tplnr,
           END OF lty_iloa.

    DATA: lit_iloa TYPE TABLE OF lty_iloa,
          lst_iloa TYPE lty_iloa.


    DATA: lst_filter       TYPE /iwbep/s_mgw_select_option,
          lst_filter_range TYPE /iwbep/s_cod_select_option,
          lst_filter_val   TYPE /odsmfe/core_range_str,

          lv_tplnr         TYPE tplnr,
          lv_equnr         TYPE equnr,
          lv_aufnr         TYPE aufnr,
          lv_priority      TYPE priok.


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

               lc_owner_6                TYPE ilom_owner VALUE '6'.

    LOOP AT it_filter_select_options INTO lst_filter.

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
            CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
              EXPORTING
                input     = lv_tplnr
              IMPORTING
                output    = lv_tplnr
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

            SELECT iloan tplnr
              FROM iloa
              INTO TABLE lit_iloa
              WHERE tplnr = lv_tplnr AND
                    owner = lc_owner_6.

            LOOP AT lit_iloa INTO lst_iloa WHERE iloan IS NOT INITIAL.
              lst_filter_val-sign   = gc_sign_i.
              lst_filter_val-option = gc_option_eq.
              lst_filter_val-low    = lst_iloa-iloan.
              APPEND lst_filter_val TO gs_filter_vals-tplnr.
              CLEAR:lst_filter_val, lst_filter_range.
            ENDLOOP.
          ENDIF.

        WHEN lc_equipnum.
          IF lst_filter_range-low IS NOT INITIAL.
            lv_equnr = lst_filter_range-low.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lv_equnr
              IMPORTING
                output = lv_equnr.
            lst_filter_val-sign   = gc_sign_i.
            lst_filter_val-option = gc_option_eq.
            lst_filter_val-low    = lv_equnr.
            APPEND lst_filter_val TO gs_filter_vals-equnr.
            CLEAR:lst_filter_val, lst_filter_range.
          ENDIF.

        WHEN lc_workordernum.
          IF lst_filter_range-low IS NOT INITIAL.
            lv_aufnr = lst_filter_range-low.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lv_aufnr
              IMPORTING
                output = lv_aufnr.
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
              REFRESH gs_filter_vals-priority.
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
        REFRESH gs_filter_vals-priority.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.


  METHOD adjust_wo_oper_stat_filters.

    DATA: lit_wo_stat_obj     TYPE STANDARD TABLE OF gty_wo_stat_obj,
          lit_wo_stat_obj_tmp TYPE STANDARD TABLE OF gty_wo_stat_obj,
          lst_wo_stat_obj     TYPE  gty_wo_stat_obj,
          lit_oper_objtmp     TYPE /odsmfe/pm_oper_object_tab,
          lst_oper_object     TYPE /odsmfe/pm_oper_object_str,
          lst_oper_del        TYPE /odsmfe/pm_oper_object_str,
          lv_index            TYPE sytabix,
          lst_wo_object       TYPE gty_wo_object.

    IF gt_valid_oper IS NOT INITIAL.
*Select operations object numbers using assigned operations
      SELECT afko~aufnr afvc~aufpl afvc~aplzl afvc~vornr afvc~objnr "SE
        FROM afvc INNER JOIN afko ON afvc~aufpl = afko~aufpl
        INTO CORRESPONDING FIELDS OF TABLE me->gt_oper_object
        FOR ALL ENTRIES IN gt_valid_oper
        WHERE afvc~aufpl EQ gt_valid_oper-aufpl
          AND afvc~aplzl EQ gt_valid_oper-aplzl.   ""#EC CI_NO_TRANSFORM

      IF sy-subrc EQ 0.
        SORT me->gt_oper_object.
      ENDIF.

    ELSE.
      IF ct_wo_object IS NOT INITIAL.

        IF gv_onlinesearch EQ abap_true AND gv_unassigned EQ abap_true AND me->assignment_type EQ '2'.
          SELECT DISTINCT afko~aufnr afvc~aufpl afvc~aplzl afvc~objnr
          FROM afko INNER JOIN afvc ON afvc~aufpl = afko~aufpl
          INTO CORRESPONDING FIELDS OF TABLE me->gt_oper_object
          FOR ALL ENTRIES IN ct_wo_object
          WHERE afko~aufnr EQ ct_wo_object-aufnr
          AND   afvc~pernr IN me->pernr_merged.
        ELSE.

          SELECT DISTINCT afko~aufnr afvc~aufpl afvc~aplzl afvc~objnr
            FROM afko INNER JOIN afvc ON afvc~aufpl = afko~aufpl
            INTO CORRESPONDING FIELDS OF TABLE me->gt_oper_object
            FOR ALL ENTRIES IN ct_wo_object
            WHERE afko~aufnr EQ ct_wo_object-aufnr.
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
          SELECT objnr FROM jest INTO TABLE lit_wo_stat_obj
           FOR ALL ENTRIES IN me->gt_oper_object
           WHERE objnr EQ me->gt_oper_object-objnr
             AND stat  IN gs_filter_vals-oper_incl_syst_stat
             AND inact EQ space.
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
          SELECT DISTINCT objnr FROM jest INTO TABLE lit_wo_stat_obj
            FOR ALL ENTRIES IN me->gt_oper_object
            WHERE objnr EQ me->gt_oper_object-objnr
              AND stat  IN gs_filter_vals-oper_excl_syst_stat
              AND inact EQ space.
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
          SELECT DISTINCT objnr FROM jest INTO TABLE lit_wo_stat_obj
             FOR ALL ENTRIES IN me->gt_oper_object
             WHERE jest~objnr EQ me->gt_oper_object-objnr
               AND jest~stat  IN gs_filter_vals-oper_excl_user_stat
               AND jest~inact EQ space.
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

    DATA:lst_mobstatfilt TYPE /odsmfe/tb_mstfl,
         lit_mobstatfilt TYPE TABLE OF /odsmfe/tb_mstfl,
         ls_filter_val   TYPE /odsmfe/core_range_str,
         lit_date        TYPE /odsmfe/core_range_tab,
         lit_wo_stat_obj TYPE TABLE OF gty_wo_stat_obj,
         lst_wo_stat_obj TYPE gty_wo_stat_obj,
         lit_wo_objtmp   TYPE gtt_wo_object,
         lst_wo_object   TYPE gty_wo_object.


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
      REFRESH : gs_filter_vals-wo_incl_syst_stat , gs_filter_vals-wo_excl_syst_stat , gs_filter_vals-wo_excl_user_stat ,
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
        SELECT DISTINCT a~objnr FROM jest AS a INNER JOIN jcds AS b
               ON a~objnr = b~objnr
              INTO TABLE lit_wo_stat_obj
         FOR ALL ENTRIES IN ct_wo_object
         WHERE a~objnr EQ ct_wo_object-objnr
           AND a~stat  IN gs_filter_vals-wo_incl_user_stat
           AND a~inact EQ space
           AND b~udate IN lit_date.
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
        SELECT DISTINCT objnr FROM jest INTO TABLE lit_wo_stat_obj
        FOR ALL ENTRIES IN ct_wo_object
        WHERE objnr EQ ct_wo_object-objnr
          AND stat  IN gs_filter_vals-wo_excl_user_stat
          AND inact EQ space.
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
        SELECT DISTINCT objnr FROM jest INTO TABLE lit_wo_stat_obj
        FOR ALL ENTRIES IN ct_wo_object
        WHERE objnr EQ ct_wo_object-objnr
          AND stat  IN gs_filter_vals-wo_excl_syst_stat
          AND inact EQ space.
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
    INTO TABLE lit_joinc
    WHERE entityset    = 'WoHeaderSet'.

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

      SELECT SINGLE roleid FROM /odsmfe/tb_urole INTO gv_roleid
        WHERE userid = iv_mobileuser
        AND  startdate LE sy-datum
        AND enddate    GE sy-datum.

      IF sy-subrc = 0 AND gv_roleid IS NOT INITIAL.
        CLEAR: lv_role_assigment.
        SELECT SINGLE assignmenttype FROM /odsmfe/tb_roles
               INTO lv_role_assigment
               WHERE roleid EQ gv_roleid.
        IF lv_role_assigment IS NOT INITIAL.
          MOVE lv_role_assigment TO me->assignment_type.
        ENDIF.
      ENDIF.
    ENDIF.

    IF me->assignment_type IS INITIAL.
      SORT gs_filter_vals-assignment BY sign option.
* Read assingment value
      READ TABLE gs_filter_vals-assignment ASSIGNING <fs_range_str>
      WITH KEY sign = 'I' option = 'EQ' BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE <fs_range_str>-low TO me->assignment_type.
      ELSE.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = text-w01.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_data_asgn_type_1.

    DATA: lv_tabix            TYPE sytabix,
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

    DATA: lv_tabix            TYPE sytabix,
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


  METHOD GET_DATA_ASGN_TYPE_4.

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

    CONSTANTS:  lc_sign_i              TYPE bapisign VALUE 'I',
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

      SELECT mandt entitysetname tabname field recordno field_descr sign options low high active
       FROM /odsmfe/tb_filtr
       INTO TABLE lit_filters
       WHERE  entitysetname = iv_entity_set_name
       AND   active = abap_on.

      IF sy-subrc EQ 0.

        GET REFERENCE OF es_filter_vals INTO lo_filter_values.
        ASSIGN lo_filter_values->* TO <lfsst_filter_values>.

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

              REFRESH lit_filter_val[].

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
          INTO lv_dyn_filter
          WHERE param_name = lc_dyn_filter
          AND active = lc_x.

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

    SELECT * FROM /odsmfe/tb_mstfl INTO TABLE et_mobstatfilt
     WHERE roleid = gv_roleid
     AND entityset = lc_entity_set_name
     AND objecttype = lc_x
     AND active = lc_x .
  ENDMETHOD.


  METHOD get_selection_asgn_type_1.
    CONSTANTS: lc_obtyp_ori   TYPE j_obtyp VALUE 'ORI'.
    SELECT DISTINCT aufk~aufnr aufk~auart aufk~objnr ihpa~parnr afih~priok
      FROM aufk INNER JOIN afih ON aufk~aufnr = afih~aufnr
                INNER JOIN ihpa ON aufk~objnr = ihpa~objnr    AND
                                   ihpa~parvw = gv_parvw      AND
                                   ihpa~obtyp = lc_obtyp_ori  AND
                                   ihpa~kzloesch = space
             INTO TABLE et_valid_wo_parnr
             WHERE ihpa~parnr IN me->pernr_merged
             AND aufk~auart IN gs_filter_vals-order_type
             AND aufk~autyp IN gs_filter_vals-order_catg
             AND aufk~bukrs IN gs_filter_vals-comp_code
             AND aufk~werks IN gs_filter_vals-plant
             AND aufk~kokrs IN gs_filter_vals-co_area
             AND aufk~idat1 IN gs_filter_vals-date_release
             AND aufk~idat2 IN gs_filter_vals-date_completion
             AND aufk~idat3 IN gs_filter_vals-date_close
             AND aufk~loekz = space.                  ""#EC CI_BUFFJOIN
  ENDMETHOD.


  METHOD get_selection_asgn_type_3.

    IF me->gewrk_merged IS NOT INITIAL.

      SELECT a~aufnr a~auart a~objnr b~priok FROM aufk AS a
                 INNER JOIN afih AS b ON a~aufnr = b~aufnr
                 INTO CORRESPONDING FIELDS OF TABLE et_valid_wo_parnr
                   WHERE a~auart IN gs_filter_vals-order_type
                     AND a~autyp IN gs_filter_vals-order_catg
                     AND a~bukrs IN gs_filter_vals-comp_code
                     AND a~werks IN gs_filter_vals-plant
                     AND a~kokrs IN gs_filter_vals-co_area
                     AND a~idat1 IN gs_filter_vals-date_release
                     AND a~idat2 IN gs_filter_vals-date_completion
                     AND a~idat3 IN gs_filter_vals-date_close
                     AND a~loekz = space
                     AND b~gewrk IN me->gewrk_merged.
    ENDIF.

  ENDMETHOD.


  METHOD get_sel_asgn_type_2.
    SELECT DISTINCT a~aufpl b~aufnr b~auart b~objnr c~priok
      FROM afko AS a INNER JOIN aufk AS b ON a~aufnr = b~aufnr
                     INNER JOIN afih AS c ON a~aufnr = c~aufnr
      INTO CORRESPONDING FIELDS OF TABLE et_wo_object
      FOR ALL ENTRIES IN it_valid_oper
      WHERE a~aufpl = it_valid_oper-aufpl
        AND b~aufnr IN gs_filter_vals-orderid "Added by ODS
        AND b~auart IN gs_filter_vals-order_type
        AND b~autyp IN gs_filter_vals-order_catg
        AND b~werks IN gs_filter_vals-plant
        AND c~iphas IN gs_filter_vals-pm_phase
        AND c~gewrk IN me->gewrk_merged
        AND b~kokrs IN gs_filter_vals-co_area
        AND b~bukrs IN gs_filter_vals-comp_code
        AND b~idat1 IN gs_filter_vals-date_release
        AND b~idat2 IN gs_filter_vals-date_completion
        AND b~idat3 IN gs_filter_vals-date_close
        AND b~loekz EQ space.                    ""#EC CI_NO_TRANSFORM
  ENDMETHOD.


  METHOD get_sel_asgn_type_2_dlta.

    DATA: lit_valid_oper TYPE gtt_afvc_key.


    SELECT DISTINCT afvc~aufpl afvc~aplzl afvc~pernr FROM afko
       INNER JOIN afvc ON afvc~aufpl = afko~aufpl
       INNER JOIN afvv ON afvv~aufpl = afvc~aufpl
       AND afvv~aplzl = afvc~aplzl
       INTO TABLE et_valid_oper
       FOR ALL ENTRIES IN it_workorder_header
        WHERE afko~aufnr = it_workorder_header-aufnr
          AND  afvc~pernr IN me->pernr_merged
          AND   afvc~arbid IN me->arbid_merged
          AND   afvc~aufpl NE space.                 ""#EC CI_NO_TRANSFORM
  ENDMETHOD.


  METHOD get_sel_asgn_type_2_nondlta.
    SELECT DISTINCT aufpl aplzl pernr FROM afvc
      INTO TABLE et_valid_oper
      WHERE pernr IN me->pernr_merged
      AND   arbid IN me->arbid_merged
      AND   aufpl NE space.                      ""#EC CI_NO_TRANSFORM
  ENDMETHOD.


  METHOD get_sel_asgn_type_4.
    SELECT aufk~aufnr aufk~auart aufk~objnr afih~priok
             FROM afko  INNER JOIN aufk ON afko~aufnr = aufk~aufnr
             INNER JOIN afih  ON afko~aufnr = afih~aufnr
             INTO CORRESPONDING FIELDS OF TABLE et_wo_object                   " WoPriority field fetch - Ysindhu - ES1K901939
             FOR ALL ENTRIES IN it_valid_oper
             WHERE afko~aufpl = it_valid_oper-aufpl AND
                   aufk~aufnr IN gs_filter_vals-orderid  AND
                   aufk~auart IN gs_filter_vals-order_type AND "Added by ODS
                   aufk~autyp IN gs_filter_vals-order_catg AND
                   aufk~werks IN gs_filter_vals-plant AND
                   afih~iphas IN gs_filter_vals-pm_phase AND
                   aufk~kokrs IN gs_filter_vals-co_area AND
                   aufk~bukrs IN gs_filter_vals-comp_code AND
                   aufk~idat1 IN gs_filter_vals-date_release AND
                   aufk~idat2 IN gs_filter_vals-date_completion AND
                   aufk~idat3 IN gs_filter_vals-date_close AND
                   aufk~loekz EQ space.   ""#EC CI_NO_TRANSFORM ""#EC CI_BUFFJOIN
  ENDMETHOD.


  METHOD get_sel_asgn_type_4_dlta.

    DATA: lit_valid_oper TYPE gtt_afvc_key.

    IF me->arbid_merged IS NOT INITIAL.
      SELECT DISTINCT afvc~aufpl afvc~aplzl afvc~pernr FROM afko
         INNER JOIN afvc ON afvc~aufpl = afko~aufpl
         INNER JOIN afvv ON afvv~aufpl = afvc~aufpl AND
                            afvv~aplzl = afvc~aplzl
         INTO TABLE et_valid_oper
         FOR ALL ENTRIES IN it_workorder_header
          WHERE afko~aufnr = it_workorder_header-aufnr AND
                afvc~arbid IN me->arbid_merged.
      ""#EC CI_NO_TRANSFORM
    ENDIF.
  ENDMETHOD.


  METHOD get_sel_asgn_type_4_nondlta.
    SELECT DISTINCT aufpl aplzl pernr FROM afvc
      INTO TABLE et_valid_oper
      WHERE arbid IN me->arbid_merged
      AND   aufpl NE space.                      ""#EC CI_NO_TRANSFORM
  ENDMETHOD.


  METHOD GET_SEL_ASGN_TYPE_5.

  ENDMETHOD.


  METHOD get_sel_asgn_type_5_dlta.

    SELECT DISTINCT kbed~aufpl kbed~aplzl FROM afko
            INNER JOIN afvc ON afvc~aufpl = afko~aufpl
            INNER JOIN afvv ON afvv~aufpl = afvc~aufpl AND afvv~aplzl = afvc~aplzl
            INNER JOIN kbed ON kbed~aufpl = afvv~aufpl AND kbed~aplzl = afvv~aplzl
            INTO CORRESPONDING FIELDS OF TABLE et_valid_oper
            FOR ALL ENTRIES IN it_workorder_header
            WHERE afko~aufnr = it_workorder_header-aufnr
            AND   kbed~pernr IN me->pernr_merged
            AND   kbed~arbid IN me->arbid_merged
            AND   kbed~typkz = 1.                      ""#EC CI_NO_TRANSFORM

    IF sy-subrc EQ 0 AND et_valid_oper IS NOT INITIAL.
      SELECT DISTINCT b~aufnr b~auart b~objnr c~priok
      FROM afko AS a INNER JOIN aufk AS b ON a~aufnr = b~aufnr
      INNER JOIN afih AS c ON a~aufnr = c~aufnr
      INTO CORRESPONDING FIELDS OF TABLE et_wo_object                   " WoPriority field fetch - Ysindhu - ES1K901939
      FOR ALL ENTRIES IN et_valid_oper
      WHERE a~aufpl = et_valid_oper-aufpl
      AND b~aufnr IN gs_filter_vals-orderid      "Added by ODS
      AND b~auart IN gs_filter_vals-order_type
      AND b~autyp IN gs_filter_vals-order_catg
      AND b~werks IN gs_filter_vals-plant
      AND c~iphas IN gs_filter_vals-pm_phase
      AND c~gewrk IN me->gewrk_merged
      AND b~kokrs IN gs_filter_vals-co_area
      AND b~bukrs IN gs_filter_vals-comp_code
      AND b~idat1 IN gs_filter_vals-date_release
      AND b~idat2 IN gs_filter_vals-date_completion
      AND b~idat3 IN gs_filter_vals-date_close
      AND b~loekz EQ space.                    ""#EC CI_NO_TRANSFORM
      IF sy-subrc EQ 0.
        SORT et_wo_object BY aufnr.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_sel_asgn_type_5_nondlta.
    SELECT DISTINCT aufpl aplzl FROM kbed          ""#EC CI_NOFIRST.
           INTO CORRESPONDING FIELDS OF TABLE et_valid_oper
           WHERE pernr IN me->pernr_merged
           AND typkz =  1.                          ""#EC CI_NO_TRANSFORM

    IF sy-subrc EQ 0 AND et_valid_oper IS NOT INITIAL.
      SELECT DISTINCT aufk~aufnr aufk~auart aufk~objnr afih~priok
      FROM afko INNER JOIN aufk ON afko~aufnr = aufk~aufnr
      INNER JOIN afih ON afko~aufnr = afih~aufnr
      INTO CORRESPONDING FIELDS OF TABLE et_wo_object                               " WoPriority field fetch - Ysindhu - ES1K901939
      FOR ALL ENTRIES IN et_valid_oper
      WHERE afko~aufpl = et_valid_oper-aufpl.  ""#EC CI_NO_TRANSFORM
      IF sy-subrc EQ 0.
        SORT et_wo_object BY aufnr.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_status_for_wo.

    DATA: lst_oper_object     TYPE /odsmfe/pm_oper_object_str,
          lit_wo_stat_obj_tmp TYPE STANDARD TABLE OF gty_wo_stat_obj,
          lit_wo_stat_obj     TYPE STANDARD TABLE OF gty_wo_stat_obj,
          lst_wo_stat_obj     TYPE gty_wo_stat_obj,
          lit_oper_objtmp     TYPE /odsmfe/pm_oper_object_tab,
          lst_oper_del        TYPE /odsmfe/pm_oper_object_str,
          lv_index            TYPE sytabix.

    SELECT objnr stat inact FROM jest
      INTO TABLE lit_wo_stat_obj.

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


  METHOD GET_UNASSIGNED_WO_HDR.

    TYPES: BEGIN OF lty_parnr_object,
             objnr TYPE ihpa-objnr,
             parnr TYPE ihpa-parnr,
           END OF lty_parnr_object.

    DATA: lit_parnr_object TYPE TABLE OF lty_parnr_object,
          lst_parnr_object TYPE lty_parnr_object,
          lit_wo_object    TYPE gtt_wo_object,
          lst_wo_object    TYPE gty_wo_object,
          lv_tabix         TYPE sy-tabix.

    CONSTANTS: lc_obtyp_ori           TYPE j_obtyp VALUE 'ORI'.

    IF ct_wo_object IS NOT INITIAL.

      lit_wo_object[] = ct_wo_object[].
      SORT lit_wo_object BY objnr.
      DELETE ADJACENT DUPLICATES FROM lit_wo_object COMPARING objnr.

* Get orders with assignments
      SELECT objnr parnr
      FROM ihpa
      INTO TABLE lit_parnr_object
      FOR ALL ENTRIES IN lit_wo_object
      WHERE objnr EQ lit_wo_object-objnr
      AND   parvw EQ gv_parvw
      AND   obtyp EQ lc_obtyp_ori
      AND   kzloesch EQ space.

* If no entries found, no data is available for the seleciton criteria
      IF sy-subrc NE 0.
        REFRESH ct_wo_object[].
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

    SELECT DISTINCT aufpl aplzl pernr
      FROM afvc                                         "#EC CI_NOFIELD
      INTO TABLE lit_valid_oper
      WHERE pernr IN me->pernr_merged
      AND   aufpl NE space.

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

    TYPES:   BEGIN OF ltys_usr05,
               parva TYPE usr05-parva,
             END OF ltys_usr05.

    DATA: lv_parameter_id TYPE memoryid,
          lv_parva        TYPE xuvalue,

          lt_parva        TYPE TABLE OF ltys_usr05,
          lst_parva       TYPE ltys_usr05,
          lst_plant       TYPE /odsmfe/core_range_str.

    CONSTANTS: lc_w      TYPE /odsmfe/plant_category VALUE 'W',
               lc_i      TYPE /odsmfe/plant_category VALUE 'I',
               lc_s      TYPE /odsmfe/plant_category VALUE 'S',
               lc_wrk    TYPE memoryid VALUE 'WRK',
               lc_iwk    TYPE memoryid VALUE 'IWK',
               lc_swk    TYPE memoryid VALUE 'SWK',
               lc_sign_i TYPE bapisign VALUE 'I',
               lc_eq     TYPE bapioption VALUE 'EQ'.

    CASE ip_plant_category.
      WHEN lc_w.
        lv_parameter_id = lc_wrk.
      WHEN lc_i.
        lv_parameter_id = lc_iwk.
      WHEN lc_s.
        lv_parameter_id = lc_swk.
    ENDCASE.

*  Getting user parameter value
    SELECT SINGLE parva
    FROM usr05
    INTO lv_parva
    WHERE  bname = ip_user AND parid = lv_parameter_id.

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

    CONSTANTS: lc_a   TYPE /odsmfe/wc_category VALUE 'A',
               lc_v   TYPE /odsmfe/wc_category VALUE 'V',
               lc_agr TYPE memoryid VALUE 'AGR',
               lc_vap TYPE memoryid VALUE 'VAP',
               lc_sign_i TYPE bapisign VALUE 'I',
               lc_eq     TYPE bapioption VALUE 'EQ'.

    TYPES:   BEGIN OF ltys_usr05,
               parva TYPE usr05-parva,
             END OF ltys_usr05.

    DATA :lt_parva        TYPE TABLE OF ltys_usr05,
          lst_parva       TYPE ltys_usr05,
          lst_wcenter     TYPE /odsmfe/core_range_str,

          lv_parva        TYPE xuvalue,
          lv_parameter_id TYPE memoryid.

    CASE ip_wc_category.
      WHEN lc_a.
        lv_parameter_id = lc_agr.
      WHEN lc_v.
        lv_parameter_id = lc_vap.
    ENDCASE.

*  Getting user parameter value
    SELECT SINGLE parva
    FROM usr05
    INTO lv_parva
    WHERE  bname = ip_user AND parid = lv_parameter_id.
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

    DATA : lst_filter           TYPE /iwbep/s_mgw_select_option,
           lit_created_wo_parnr TYPE gtt_valid_wo_parnr,
           lit_wo_object        TYPE gtt_wo_object.

* --- Variables ---  --------------------------------------------------*

    DATA : lv_parvw TYPE parvw.

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
      MOVE <fs_range_str>-low TO gv_parvw.
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

      CATCH /iwbep/cx_mgw_busi_exception .
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
        CATCH /iwbep/cx_mgw_busi_exception.
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


  method GET_WORKORDER_DETAIL.
  endmethod.


  method GET_WORKORDER_LONGTEXT.
  endmethod.


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
      gt_aufnr_delta1[] = me->gt_aufnr_delta[].
    ENDIF.

  ENDMETHOD.


  METHOD populate_person_responsible.

    DATA: lit_wo_object    TYPE gtt_wo_object,
          lst_wo_object    TYPE gty_wo_object,
          lit_parnr_object TYPE TABLE OF gty_parnr_object,
          lst_parnr_object TYPE gty_parnr_object,
          lst_aufnr_parnr  TYPE /odsmfe/pm_aufnr_parnr_str.

    CONSTANTS:lc_obtyp_ori TYPE j_obtyp VALUE 'ORI'.

    IF me->gt_aufnr_delta IS NOT INITIAL.
      SELECT aufnr auart objnr
        FROM aufk
        INTO CORRESPONDING FIELDS OF TABLE lit_wo_object
        FOR ALL ENTRIES IN me->gt_aufnr_delta
        WHERE aufnr EQ me->gt_aufnr_delta-aufnr.

      IF sy-subrc = 0.
        SORT lit_wo_object BY objnr.
        DELETE ADJACENT DUPLICATES FROM lit_wo_object COMPARING objnr.

        SELECT objnr parnr
        FROM ihpa
        INTO TABLE lit_parnr_object
        FOR ALL ENTRIES IN lit_wo_object
        WHERE objnr EQ lit_wo_object-objnr
        AND   parvw EQ gv_parvw "lc_parvw_resp Change by ODS -  ES1K902361
        AND   obtyp EQ lc_obtyp_ori
        AND   kzloesch EQ space.

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

    CONSTANTS:lc_work_center_objty   TYPE pm_objty VALUE 'A'.

    DATA: lst_range TYPE /odsmfe/core_range_str.

    SELECT DISTINCT objid AS low FROM crhd
         INTO CORRESPONDING FIELDS OF TABLE ct_range_tab
         WHERE arbpl = iv_parva AND
               objty = lc_work_center_objty AND
               begda LE sy-datum AND
               endda GE sy-datum AND
               werks IN gs_filter_vals-plant.
    IF sy-subrc EQ 0.
      lst_range-sign = 'I'.
      lst_range-option = 'EQ'.
      MODIFY ct_range_tab FROM lst_range
      TRANSPORTING sign option WHERE sign = space.   ""#EC CI_STDSEQ
    ENDIF.

  ENDMETHOD.


  METHOD wo_by_login_user.


    DATA:lv_createdby_wo      TYPE char1,
         lit_created_wo_parnr TYPE STANDARD TABLE OF gty_valid_wo_parnr.

    CONSTANTS:lc_createdby_wo TYPE string VALUE 'DOWNLOAD_CREATEDBY_WO',
              lc_x            TYPE char1 VALUE 'X'.

    SELECT SINGLE param_value
      FROM /odsmfe/tb_apcon INTO lv_createdby_wo
      WHERE param_name = lc_createdby_wo AND
            active EQ lc_x.

    IF lv_createdby_wo IS NOT INITIAL.
      IF it_workorder_header IS NOT INITIAL.
        SELECT DISTINCT aufk~aufnr aufk~auart aufk~objnr afih~priok FROM aufk
        INNER JOIN afih ON aufk~aufnr = afih~aufnr
        INTO TABLE et_created_wo_parnr
        FOR ALL ENTRIES IN it_workorder_header
        WHERE aufk~aufnr = it_workorder_header-aufnr
        AND aufk~ernam EQ iv_mobileuser
        AND aufk~auart IN gs_filter_vals-order_type
        AND aufk~autyp IN gs_filter_vals-order_catg
        AND aufk~bukrs IN gs_filter_vals-comp_code
        AND aufk~werks IN gs_filter_vals-plant
        AND aufk~kokrs IN gs_filter_vals-co_area
        AND aufk~idat1 IN gs_filter_vals-date_release
        AND aufk~idat2 IN gs_filter_vals-date_completion
        AND aufk~idat3 IN gs_filter_vals-date_close
        AND aufk~loekz = space.
      ELSE.
        SELECT DISTINCT aufk~aufnr aufk~auart aufk~objnr afih~priok FROM aufk
         INNER JOIN afih ON aufk~aufnr = afih~aufnr
         INTO TABLE et_created_wo_parnr
         WHERE aufk~ernam EQ iv_mobileuser
         AND aufk~aufnr IN gs_filter_vals-orderid
         AND aufk~auart IN gs_filter_vals-order_type
         AND aufk~autyp IN gs_filter_vals-order_catg
         AND aufk~bukrs IN gs_filter_vals-comp_code
         AND aufk~werks IN gs_filter_vals-plant
         AND aufk~kokrs IN gs_filter_vals-co_area
         AND aufk~idat1 IN gs_filter_vals-date_release
         AND aufk~idat2 IN gs_filter_vals-date_completion
         AND aufk~idat3 IN gs_filter_vals-date_close
         AND aufk~loekz = space.
      ENDIF.
      SORT et_created_wo_parnr BY aufnr.
    ENDIF.

  ENDMETHOD.


  METHOD wo_non_online_search.
    DATA: lv_oper_filter       TYPE flag,
          lv_parid             TYPE memoryid,
          lv_parva             TYPE xuvalue,
          lst_pernr_range      LIKE LINE OF me->pernr_merged,
          lit_workorder_header TYPE /odsmfe/cs_caufv_tab,
          lit_valid_wo_parnr   TYPE gtt_valid_wo_parnr,
          lit_valid_oper       TYPE gtt_afvc_key.

    CONSTANTS: lc_1           TYPE /odsmfe/pm_assignment_type_dte VALUE '1',
               lc_2           TYPE /odsmfe/pm_assignment_type_dte VALUE '2',
               lc_3           TYPE /odsmfe/pm_assignment_type_dte VALUE '3',
               lc_4           TYPE /odsmfe/pm_assignment_type_dte VALUE '4',
               lc_5           TYPE /odsmfe/pm_assignment_type_dte VALUE '5',
               lc_pernr_dummy TYPE pernr_d VALUE '99999999',
               lc_head        TYPE memoryid VALUE 'VAP',
               lc_op          TYPE memoryid VALUE 'AGR'.

    CLEAR lv_oper_filter.

    IF NOT ( gs_filter_vals-oper_incl_syst_stat IS INITIAL
       AND   gs_filter_vals-oper_excl_syst_stat IS INITIAL
       AND   gs_filter_vals-oper_incl_user_stat IS INITIAL
      AND    gs_filter_vals-oper_excl_user_stat IS INITIAL ).
      lv_oper_filter = abap_true.
    ENDIF.

    IF me->assignment_type EQ lc_1 OR
       me->assignment_type EQ lc_2 OR
       me->assignment_type EQ lc_5.

      lst_pernr_range-sign = 'I'.
      lst_pernr_range-option = 'EQ'.

* get communication user
      SELECT pernr AS low FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE me->pernr_merged
            WHERE usrid = iv_mobileuser               ""#EC CI_NOFIRST
            AND begda LE sy-datum
            AND endda GE sy-datum .                   ""#EC CI_NOFIRST

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

      SELECT SINGLE parva FROM usr05 INTO lv_parva WHERE bname = iv_mobileuser AND parid EQ lv_parid.
      IF sy-subrc NE 0 AND lv_parva IS INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = text-001.
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

    DATA: lit_wo_object TYPE gtt_wo_object.
    CONSTANTS: lc_1 TYPE /odsmfe/pm_assignment_type_dte VALUE '1',
               lc_2 TYPE /odsmfe/pm_assignment_type_dte VALUE '2'.


    SELECT DISTINCT a~aufpl b~aufnr b~auart b~objnr          ""#EC CI_NOORDER
       FROM afko AS a INNER JOIN aufk AS b ON a~aufnr = b~aufnr
       INNER JOIN afih AS c ON a~aufnr = c~aufnr
       INTO TABLE lit_wo_object
       WHERE b~aufnr IN gs_filter_vals-orderid
       AND   b~erdat IN gs_filter_vals-created_on
       AND   b~werks IN gs_filter_vals-plant
       AND   b~vaplz IN gs_filter_vals-work_cntr
       AND   b~loekz = space
       AND   c~priok IN gs_filter_vals-priority "Added by ODS
       AND   c~equnr IN gs_filter_vals-equnr
       AND   c~iloan IN gs_filter_vals-tplnr.

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
