CLASS /odsmfe/cl_approvermasterdata DEFINITION
  PUBLIC
  INHERITING FROM /odsmfe/cl_get_ent_super_bapi
  CREATE PUBLIC .

  PUBLIC SECTION.

    "SOC BY LMETTA
*  type-pools ABAP .
*
*  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_APPROVERMASTERDATA .
*  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_APPROVERMASTERDATA .
    "EOC BY LMETTA

    DATA gvib_user TYPE usnam .

    METHODS /odsmfe/if_get_entityset_bapi~gmib_read_entityset
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /ODSMFE/CL_APPROVERMASTERDATA IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 28/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Fetch the Approver Master Data
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal Table
    DATA: lit_approvermaster TYPE  STANDARD TABLE OF /odsmfe/tb_aprmd,
          lrt_usersystemid   TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_personnelnum   TYPE TABLE OF /odsmfe/st_core_range_str,
          lrs_usersystemid   TYPE /odsmfe/st_core_range_str,
          lrs_personnelnum   TYPE /odsmfe/st_core_range_str.

*          lt_tab             TYPE TABLE OF /ODSMFE/CE_ServiceConfig,
*          lt_approvemaster   type table of /ODSMFE/CE_ServiceConfig.
*          lst_key_tab        TYPE /iwbep/s_mgw_name_value_pair.

* Constants
    CONSTANTS:   "lc_e            TYPE string VALUE 'E',
*               "lc_i            TYPE string VALUE 'I',
*               "lc_eq           TYPE string VALUE 'EQ',
      lc_usersystemid TYPE string VALUE 'USERID',
      lc_personnelnum TYPE string VALUE 'PERSONNELNUM'.

* Field Symbols
    FIELD-SYMBOLS <lfsst_form> TYPE /odsmfe/tb_aprmd.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
*SOC BY LMETTA

*    IF im_key_tab IS NOT INITIAL.
*
*      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
*        CASE lst_key_tab-name.
*          WHEN lc_usersystemid.
*            lrs_usersystemid-sign = lc_i.
*            lrs_usersystemid-option = lc_eq.
*            lrs_usersystemid-low = lst_key_tab-value.
*            APPEND lrs_usersystemid TO lrt_usersystemid.
*            CLEAR lrs_usersystemid.
*
*          WHEN lc_personnelnum.
*            lrs_personnelnum-sign   = lc_i.
*            lrs_personnelnum-option = lc_eq.
*            lrs_personnelnum-low    = lst_key_tab-value.
*            APPEND lrs_personnelnum TO lrt_personnelnum.
*            CLEAR lrs_personnelnum.
*        ENDCASE.
*      ENDLOOP.
*    ENDIF.
*EOC BY LMETTA

**SOC BY LMETTA
    LOOP AT im_filter_select_options INTO DATA(ls_filter_select_options).

      CASE ls_filter_select_options-name.
        WHEN lc_usersystemid.
          lrt_usersystemid = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_usersystemid WHERE low IS INITIAL.
        WHEN lc_personnelnum.
          lrt_personnelnum = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_personnelnum WHERE low IS INITIAL.

      ENDCASE.
    ENDLOOP.
**EOC BY LMETTA


* Fetching Approver Master Data
    SELECT usersystemid, personnelnum, emailid, active, firstname, lastname, contact,
           departmentid, departmentname, role,approverlevel,plant,workcenter
           FROM /odsmfe/tb_aprmd
*           INTO CORRESPONDING FIELDS OF TABLE @lit_approvermaster
           WHERE /odsmfe/tb_aprmd~usersystemid IN @lrt_usersystemid
           AND /odsmfe/tb_aprmd~personnelnum IN @lrt_personnelnum  INTO CORRESPONDING FIELDS OF TABLE @lit_approvermaster.

    IF sy-subrc EQ 0.


* Sorting & Deleting duplicates Forms
      IF sy-subrc = 0  AND lit_approvermaster IS NOT INITIAL.
        SORT lit_approvermaster BY usersystemid personnelnum.
        DELETE ADJACENT DUPLICATES FROM lit_approvermaster COMPARING usersystemid personnelnum.
      ENDIF.
    ENDIF.
    IF lit_approvermaster IS NOT INITIAL.
* Display all data
      LOOP AT lit_approvermaster ASSIGNING <lfsst_form>.
*       MOVE-CORRESPONDING <lfsst_form> TO gstib_entity.
* Get Entity method is requested
*        IF im_key_tab IS NOT INITIAL.
*          GET REFERENCE OF gstib_entity INTO ex_entity.
*        ELSE.
*          APPEND gstib_entity TO gitib_entity.
*          CLEAR gstib_entity.
*        ENDIF.
      ENDLOOP.

* Get EntitySet method is requested
*      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF.
*   ex_response_data[] = lit_approvermaster[].
    MOVE-CORRESPONDING lit_approvermaster TO ex_response_data[].             "BY LMETTA
  ENDMETHOD.
ENDCLASS.
