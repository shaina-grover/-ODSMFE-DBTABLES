CLASS /odsmfe/cl_formassingment DEFINITION
  PUBLIC
  INHERITING FROM /odsmfe/cl_get_ent_super_bapi
  CREATE PUBLIC .

  PUBLIC SECTION.

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
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

* Constants
    CONSTANTS: lc_e               TYPE string VALUE 'E',
               lc_x               TYPE c LENGTH 1 VALUE 'X',
               lc_entity_set_name TYPE string VALUE '/ODSMFE/CE_FORMASSIGNMENT'.
* Variables
    DATA: lv_mobileuser TYPE string,
          lv_userrole   TYPE /odsmfe/de_roleid,
          lv_deleted    TYPE /odsmfe/de_deleted.
    DATA: lit_filter_vals TYPE STANDARD TABLE OF /odsmfe/tb_filtr,
          lst_filter_vals TYPE /odsmfe/tb_filtr,
          lit_form_Ass type table of /ODSMFE/TB_FOASS,
          lit_form_assignment TYPE TABLE OF /ODSMFE/CE_FORMASSIGNMENT,
          lst_form_assignment TYPE /odsmfe/ce_formassignment.

  FIELD-SYMBOLS:  <lfsst_entity> TYPE /ODSMFE/TB_FOASS.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

* Get the Logged in User details
    lv_mobileuser = sy-uname.
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
*            INTO @lv_userrole
            WHERE a~userid = @lv_mobileuser
            INTO @lv_userrole.
      IF sy-subrc NE 0.
        CLEAR lv_userrole.
      ENDIF.
    ENDIF. "Changes SKOTRA - ES1K902969

SELECT * FROM /odsmfe/tb_foass
             WHERE formid NE @space
               AND roleid EQ @lv_userrole
               AND active = @abap_true
                 INTO TABLE @lit_form_ass.
* Get the Mobile filter detials
    SELECT entitysetname, tabname, field, recordno,
                 field_descr, sign, options, low, high, active
                  FROM /odsmfe/tb_filtr
                 WHERE entitysetname = @lc_entity_set_name
                 AND active = @abap_on
                 INTO CORRESPONDING FIELDS OF TABLE @lit_filter_vals.
    IF sy-subrc EQ 0.
      SORT lit_filter_vals BY field.
    ENDIF.

* Read Deleted Flag
    READ TABLE lit_filter_vals INTO lst_filter_vals
    WITH KEY field = 'DELETED' active = lc_x BINARY SEARCH.
    IF sy-subrc = 0.
         lv_deleted =  lst_filter_vals-low.
      CLEAR: lst_filter_vals.
    ENDIF.

    IF lv_deleted IS NOT INITIAL.
      DELETE lit_form_ass WHERE deleted IS NOT INITIAL.
    ENDIF.

* SOC by ODS ES1K902499
    LOOP AT lit_form_ass ASSIGNING <lfsst_entity>.
      IF <lfsst_entity>-zaehl EQ '00000000'.
        CLEAR:<lfsst_entity>-zaehl.
      ENDIF.
    ENDLOOP.
    UNASSIGN :<lfsst_entity>.

lit_form_assignment =
     VALUE #( FOR lst_form_ass IN lit_form_ass
     ( FormID = lst_form_ass-formid
       Version = lst_form_ass-version
       OrderType = lst_form_ass-ordertype
        Groups = lst_form_ass-plnnr
       Category = lst_form_ass-category
       JobType = lst_form_ass-jobtype
       Mandatory = lst_form_ass-mandatory
       FlowSequence = lst_form_ass-flowsequence
       MultipleSub = lst_form_ass-multiplesub
       Occur = lst_form_ass-occur
       Theme = lst_form_ass-theme
       StyleSheet = lst_form_ass-stylesheet
       CreatedOn = lst_form_ass-createdon
       CreatedBy = lst_form_ass-createdby
       ModifiedOn = lst_form_ass-modifiedon
       ModifiedBy = lst_form_ass-modifiedby
       Deleted = lst_form_ass-deleted
        ControlKey = lst_form_ass-steus
        TaskListType = lst_form_ass-plnty
        GroupCounter = lst_form_ass-plnal
        InternalCounter = lst_form_ass-zaehl
        EquipCategory =  lst_form_ass-eqtyp
        FuncLocCategory = lst_form_ass-fltyp  ) ).

 MOVE-CORRESPONDING lit_form_assignment TO ex_response_data.

  ENDMETHOD.
ENDCLASS.
