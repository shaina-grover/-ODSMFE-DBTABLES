CLASS lhc_FormAssignment DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.
        DATA: lst_foass   TYPE /odsmfe/tb_foass,
                   lst_foass1 TYPE /odsmfe/tb_foass.

         CONSTANTS: lc_theme TYPE string VALUE 'theme-grid',
                               lc_active  TYPE abap_bool  VALUE 'X'.

  PRIVATE SECTION.
        METHODS modify FOR behavior IMPORTING
            roots_to_create       FOR CREATE FormAssignment
            roots_to_update      FOR UPDATE FormAssignment
            roots_to_delete       FOR DELETE FormAssignment .

        METHODS read FOR behavior IMPORTING
            lit_formass FOR READ FormAssignment RESULT et_formass.

ENDCLASS.

CLASS lhc_FormAssignment IMPLEMENTATION.
    METHOD modify.

         "/ -----------------------Handle Create Method---------------------------/"
         if roots_to_create IS NOT INITIAL.
            LOOP AT roots_to_create INTO DATA(lst_to_create).
                lst_foass-formid             = lst_to_create-FormID.
                lst_foass-version            = lst_to_create-Version.
                lst_foass-ordertype        = lst_to_create-OrderType.
                lst_foass-steus               = lst_to_create-ControlKey.
                lst_foass-plnty                = lst_to_create-TaskListType.
                lst_foass-plnnr               = lst_to_create-Groups.
                lst_foass-plnal               = lst_to_create-GroupCounter.
                lst_foass-zaehl               = lst_to_create-InternalCounter.
                lst_foass-eqtyp              = lst_to_create-EquipCategory.
                lst_foass-fltyp                = lst_to_create-FuncLocCategory.
                lst_foass-category         = lst_to_create-Category.
                lst_foass-jobtype           = lst_to_create-JobType.
                lst_foass-mandatory      = lst_to_create-Mandatory.
                lst_foass-flowsequence  = lst_to_create-FlowSequence.
                lst_foass-multiplesub     = lst_to_create-MultipleSub.
                lst_foass-occur               = lst_to_create-Occur.
                lst_foass-theme              = lc_theme.
                lst_foass-stylesheet       = lst_to_create-StyleSheet.
                lst_foass-createdon       = lst_to_create-CreatedOn.
                lst_foass-createdby       = lst_to_create-CreatedBy.
                lst_foass-modifiedon     = lst_to_create-ModifiedOn.
                lst_foass-modifiedby      = lst_to_create-ModifiedBy.
                lst_foass-active              = lc_active.
                lst_foass-deleted            = lst_to_create-Deleted.
            ENDLOOP. "/LOOP AT roots_to_create INTO DATA(lst_to_create).

            if lst_foass-formid IS NOT INITIAL.
                MODIFY /odsmfe/tb_foass FROM @lst_foass.
                if sy-subrc NE 0.
                    clear: lst_foass.
                endif.
            endif. "/  if lst_foass-formid IS NOT INITIAL.
            clear: lst_foass.
         endif. "/if roots_to_create IS NOT INITIAL.

         "/ -----------------------Handle Modify Method---------------------------/"
         if roots_to_update IS NOT INITIAL.
            LOOP AT roots_to_update INTO DATA(lst_to_update).
                lst_foass-formid             = lst_to_update-FormID.
                lst_foass-version            = lst_to_update-Version.
                lst_foass-ordertype        = lst_to_update-OrderType.
                lst_foass-steus               = lst_to_update-ControlKey.
                lst_foass-plnty                = lst_to_update-TaskListType.
                lst_foass-plnnr               = lst_to_update-Groups.
                lst_foass-plnal               = lst_to_update-GroupCounter.
                lst_foass-zaehl               = lst_to_update-InternalCounter.
                lst_foass-eqtyp              = lst_to_update-EquipCategory.
                lst_foass-fltyp                = lst_to_update-FuncLocCategory.
                lst_foass-category         = lst_to_update-Category.
                lst_foass-jobtype           = lst_to_update-JobType.
                lst_foass-mandatory      = lst_to_update-Mandatory.
                lst_foass-flowsequence  = lst_to_update-FlowSequence.
                lst_foass-multiplesub     = lst_to_update-MultipleSub.
                lst_foass-occur               = lst_to_update-Occur.
                lst_foass-theme              = lc_theme.
                lst_foass-stylesheet       = lst_to_update-StyleSheet.
                lst_foass-createdon       = lst_to_update-CreatedOn.
                lst_foass-createdby       = lst_to_update-CreatedBy.
                lst_foass-modifiedon     = lst_to_update-ModifiedOn.
                lst_foass-modifiedby      = lst_to_update-ModifiedBy.
                lst_foass-active              = lc_active.
                lst_foass-deleted            = lst_to_update-Deleted.
            ENDLOOP. "/ LOOP AT roots_to_update INTO DATA(lst_to_update).

            SELECT SINGLE *
            FROM /odsmfe/tb_foass
            WHERE formid = @lst_foass-formid
            AND      version = @lst_foass-version
            AND      ordertype = @lst_foass-ordertype
            AND      steus        = @lst_foass-steus
            AND      plnty         = @lst_foass-plnty
            AND      plnnr         = @lst_foass-plnnr
            AND      plnal         = @lst_foass-plnal
            AND      oprnum    = @lst_foass-oprnum
            AND      zaehl        = @lst_foass-zaehl
            AND      eqtyp        = @lst_foass-eqtyp
            AND      fltyp          = @lst_foass-fltyp
            INTO @lst_foass1.

            if sy-subrc eq 0 AND lst_foass1 IS NOT INITIAL.
                MODIFY /odsmfe/tb_foass FROM @lst_foass.
            endif. "/if sy-subrc eq 0 AND lst_foass1 IS NOT INITIAL.

         endif. "/ if roots_to_update IS NOT INITIAL.

    ENDMETHOD.

METHOD read.
ENDMETHOD.

ENDCLASS.
