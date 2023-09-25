class /ODSMFE/CL_DEPTMASTERDATA definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.

 " data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_DEPTMASTERDATA .
  "data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_DEPTMASTERDATA .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_DEPTMASTERDATA IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 31/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Fetch the Department Master Data
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
    DATA: lit_department TYPE  STANDARD TABLE OF /odsmfe/tb_deptm,
          lst_department TYPE  /odsmfe/tb_deptm,
          lrt_deptid TYPE TABLE OF /odsmfe/st_core_range_str,
         lrt_deptname TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_plant      TYPE TABLE OF /odsmfe/st_core_range_str.
*          "lst_key_tab    TYPE /iwbep/s_mgw_name_value_pair,
          "lrs_deptid     TYPE /odsmfe/st_core_range_str,
          "lrs_deptname   TYPE /odsmfe/st_core_range_str,
          "lrs_plant      TYPE /odsmfe/st_core_range_str.

DATA:gitib_entity type table of /ODSMFE/CE_DeptMasterData,
     gstib_entity type /ODSMFE/CE_DeptMasterData.



* Constants
    CONSTANTS:
               lc_deptid   TYPE string VALUE 'DEPARTMENTID',
               lc_deptname TYPE string VALUE 'DEPARTMENTNAME',
               lc_plant    TYPE string VALUE 'PLANT'.

* Field Symbols
    FIELD-SYMBOLS <lfsst_form> TYPE /odsmfe/tb_deptm.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*    IF im_key_tab IS NOT INITIAL.
*
*      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
*        CASE lst_key_tab-name.
*          WHEN lc_deptid.
*            lrs_deptid-sign = lc_i.
*            lrs_deptid-option = lc_eq.
*            lrs_deptid-low = lst_key_tab-value.
*            APPEND lrs_deptid TO lrt_deptid.
*            CLEAR lrs_deptid.
*
*          WHEN lc_deptname.
*            lrs_deptname-sign   = lc_i.
*            lrs_deptname-option = lc_eq.
*            lrs_deptname-low    = lst_key_tab-value.
*            APPEND lrs_deptname TO lrt_deptname.
*            CLEAR lrs_deptname.
*
*          WHEN lc_plant.
*            lrs_plant-sign   = lc_i.
*            lrs_plant-option = lc_eq.
*            lrs_plant-low    = lst_key_tab-value.
*            APPEND lrs_plant TO lrt_plant.
*            CLEAR lrs_plant.
*        ENDCASE.
*       ENDLOOP.
*     ENDIF.

**SOC by Priyanka

         LOOP AT im_filter_select_options INTO DATA(ls_filter_select_options).
          CASE ls_filter_select_options-name.
              WHEN lc_deptid.
                 lrt_deptid = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_deptid WHERE low IS INITIAL.
                 read table lrt_deptid into data(lrs_deptid) INDEX 1.
                 if sy-subrc eq 0.
                lst_department-departmentid = lrs_Deptid-low.
                endif.


              when lc_deptname.
                 lrt_deptname = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_deptname WHERE low IS INITIAL.
                  read table lrt_deptname into data(lrs_deptname) INDEX 1.
                 if sy-subrc eq 0.
                lst_department-departmentname = lrs_deptname-low.
                endif.

               when lc_plant.
                  lrt_plant = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_plant WHERE low IS INITIAL.
                  read table lrt_plant into data(lrs_plant) INDEX 1.
                 if sy-subrc eq 0.
                lst_department-plant = lrs_plant-low.
                endif.

              ENDCASE.
           ENDLOOP.

**EOC By Priyanka

* Fetching Data
    select *
           from /odsmfe/tb_deptm
           where departmentid IN @lrt_deptid[]
           and departmentname IN @lrt_deptname[]
           and plant IN @lrt_plant[]
           into CORRESPONDING FIELDS OF table @lit_department.
*           AND /odsmfe/tb_deptm~active EQ abap_true.



* Sorting & Deleting duplicates Forms
    IF sy-subrc = 0  AND lit_department IS NOT INITIAL.
      SORT lit_department BY departmentid departmentname plant.
      DELETE ADJACENT DUPLICATES FROM lit_department COMPARING departmentid departmentname plant.
    ENDIF.
loop at lit_department into lst_department.

gstib_entity-Active = lst_department-active.
gstib_entity-DepartmentID = lst_department-departmentid.
gstib_entity-DepartmentName = lst_department-departmentname.
gstib_entity-Plant = lst_department-plant.
append gstib_entity to gitib_entity.

endloop.

    " ex_response_data[] = lit_department[].
*    MOVE-CORRESPONDING lit_department to ex_response_data.
      MOVE-CORRESPONDING gitib_entity to ex_response_data.

    "IF lit_department IS NOT INITIAL.
* Display all data
*      LOOP AT lit_department ASSIGNING <lfsst_form>.
*        MOVE-CORRESPONDING <lfsst_form> TO gstib_entity.
** Get Entity method is requested
*        IF im_key_tab IS NOT INITIAL.
*          GET REFERENCE OF gstib_entity INTO ex_entity.
*        ELSE.
*          APPEND gstib_entity TO gitib_entity.
*          CLEAR gstib_entity.
*        ENDIF.
*      ENDLOOP.
*
** Get EntitySet method is requested
*      GET REFERENCE OF gitib_entity INTO ex_entityset.
    "ENDIF.

  ENDMETHOD.
ENDCLASS.
