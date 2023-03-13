CLASS /odsmfe/cl_formassingment DEFINITION
  PUBLIC
  INHERITING FROM /odsmfe/cl_get_ent_super_bapi
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    DATA gstib_entity TYPE /odsmfe/cl_pr_formui_mpc=>ts_formassingment .
    DATA gitib_entity TYPE /odsmfe/cl_pr_formui_mpc=>tt_formassingment .

    METHODS /odsmfe/if_get_entityset_bapi~gmib_read_entityset
         REDEFINITION .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORMASSINGMENT IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 11/05/2020
* Transport No.          : ES1K901774
* Program Description    : Displays form FormAssingment data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Change Date            :  29/01/2021
* Transport No.          :  ES1K902499
* Change Description     :  Internal counter field returning zeros as
*                           its Numeric so FE requested to send it as blank
***********************************************************************
* Program Author (SID)   :  SKOTRA
* Change Date            :  10/03/2022
* Transport No.          :  ES1K902969
* Change Description     :  Refactoring SP07
***********************************************************************
********************** CHANGE HISTORY **********************************
* Program Author (SID)   :PPRIYANKA
* Change Date            :03/03/2023
* Transport No.          :ES1K903619
* Change Description     :Logic to fetch userrole via PFCG
************************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

* Constants
    CONSTANTS: lc_e               TYPE string VALUE 'E',
               lc_x               TYPE char1 VALUE 'X',
               lc_entity_set_name TYPE string VALUE 'FormAssingmentSet'.
* Variables
    DATA: lv_mobileuser TYPE string,
          lv_userrole   TYPE /odsmfe/de_roleid,
          lv_deleted    TYPE /odsmfe/de_delted.
    DATA: lit_filter_vals TYPE STANDARD TABLE OF /odsmfe/tb_filtr,
          lst_filter_vals TYPE /odsmfe/tb_filtr.

    FIELD-SYMBOLS:  <lfsst_entity> TYPE /odsmfe/cl_pr_formui_mpc=>ts_formassingment. "++ES1K902499
**********Data and Constant declartion - To fetch userrole via PFCG************
*    Check if table is active or not in table odsmfe/tb_apcon

    CONSTANTS:lc_pfcg_role TYPE string VALUE 'PFCG_ROLE',
              lc_meth      TYPE string VALUE 'ROLE_ASSIGNMENT',
              lc_true      TYPE string VALUE 'TRUE'.

    DATA :  lv_pfcg_role TYPE  /odsmfe/de_mfe_value,
            lv_class     TYPE  /odsmfe/de_mfe_value,
            lo_auth      TYPE REF TO object,
            lv_user      TYPE uname,
            lv_role      TYPE agval.       "agrfield

**********End of Data and Constant declartion - To fetch userrole via PFCG ***********
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

* Get the Logged in User details
    lv_mobileuser = sy-uname.
**********Start of Modificaion- TO fetch userrole via PFCG *************
    SELECT SINGLE param_value
       FROM /odsmfe/tb_apcon
       INTO lv_pfcg_role
       WHERE param_name = lc_pfcg_role
       AND active = lc_x.

    IF sy-subrc = 0.
      IF lv_pfcg_role EQ lc_true.
        lv_user = lv_mobileuser.
        SELECT SINGLE param_value
                FROM /odsmfe/tb_apcon
                INTO lv_class
                WHERE param_name = lc_meth
                AND active = lc_x.
        IF sy-subrc EQ 0.
          CREATE OBJECT lo_auth TYPE (lv_class).
        ENDIF.
        TRY.
            CALL METHOD lo_auth->(lc_meth)    "Get PFCG Role ID
              EXPORTING
                iv_uname = lv_user
              IMPORTING
                ev_field = lv_role.
            IF lv_role IS NOT INITIAL.
              lv_userrole = lv_role.
            ENDIF.
          CATCH /iwbep/cx_mgw_busi_exception.
        ENDTRY.
      ENDIF.
**********End of Modificaion- TO fetch userrole via PFCG - ES1K903522*****************
    ELSE.
*--Start of changes SKOTRA - ES1K902967
*--Get reference for fetching value of user role table
      DATA: ls_usrroletab TYPE /odsmfe/cl_exchmechwo=>gty_roles.
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        ls_usrroletab = lr_exchtab->get_userrole_tab( ).
        CONCATENATE ls_usrroletab-low 'AS a INNER JOIN' ls_usrroletab-high 'AS b ON a~roleid = b~roleid' INTO DATA(lv_userroles) SEPARATED BY space.
*--End of changes SKOTRA - ES1K902967
* check User Roles, Assignment type and Dashboard ID
        SELECT SINGLE a~roleid
              FROM  (lv_userroles) "Changes SKOTRA - ES1K902969
              INTO lv_userrole
              WHERE a~userid = lv_mobileuser.
        IF sy-subrc NE 0.
          CLEAR lv_userrole.
        ENDIF.
      ENDIF. "Changes SKOTRA - ES1K902969
    ENDIF.

    SELECT * FROM /odsmfe/tb_foass
             INTO TABLE gitib_entity
             WHERE formid NE space
               AND roleid EQ lv_userrole
               AND active = abap_true.

* Get the Mobile filter detials
    SELECT entitysetname tabname field recordno
                 field_descr sign options low high active
                 FROM /odsmfe/tb_filtr INTO CORRESPONDING FIELDS OF TABLE lit_filter_vals
                 WHERE entitysetname = lc_entity_set_name
                 AND active = abap_on.
    IF sy-subrc EQ 0.
      SORT lit_filter_vals BY field.
    ENDIF.

* Read Deleted Flag
    READ TABLE lit_filter_vals INTO lst_filter_vals
    WITH KEY field = 'DELETED' active = lc_x BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE lst_filter_vals-low TO lv_deleted.
      CLEAR: lst_filter_vals.
    ENDIF.

    IF lv_deleted IS NOT INITIAL.
      DELETE gitib_entity WHERE deleted IS NOT INITIAL.
    ENDIF.

* SOC by ODS ES1K902499
    LOOP AT gitib_entity ASSIGNING <lfsst_entity>.
      IF <lfsst_entity>-zaehl EQ '00000000'.
        CLEAR:<lfsst_entity>-zaehl.
      ENDIF.
    ENDLOOP.
    UNASSIGN :<lfsst_entity>.
* EOC by ODS ES1K902499

    IF sy-subrc = 0 AND gitib_entity IS NOT INITIAL.
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
