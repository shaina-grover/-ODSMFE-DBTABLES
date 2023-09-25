    CLASS lhc_FormApprover DEFINITION INHERITING FROM cl_abap_behavior_handler.

 PUBLIC SECTION.
      "/Tables and Structures
      DATA:       lit_fapvr     TYPE TABLE OF /odsmfe/tb_fapvr,
                  lst_fapvr    TYPE /odsmfe/tb_fapvr,
                  lst_fmapr    type /odsmfe/tb_fapvr.
  PRIVATE SECTION.

        METHODS modify FOR behavior IMPORTING
        roots_to_create     FOR create formapprover
        roots_to_update    FOR update formapprover
        roots_to_delete     FOR delete  formapprover .


        METHODS read FOR READ
        IMPORTING lit_formapprover FOR READ FormApprover RESULT ex_formapprover.

ENDCLASS.

CLASS lhc_FormApprover IMPLEMENTATION.

method modify.

           "/ ---------------Handle Create Method-----------------------/"
        if roots_to_create IS NOT INITIAL.
            LOOP AT roots_to_create INTO DATA(lst_fmapvr).
                lst_fapvr-formid                      = lst_fapvr-FormID.
                lst_fapvr-version                      = lst_fapvr-Version.
                lst_fapvr-workordernum          =  lst_fapvr-workordernum.
                lst_fapvr-oprnum                     =  lst_fapvr-OprNum.
                lst_fapvr-approverid                =  lst_fapvr-ApproverID.
                 lst_fapvr-notification               =   lst_fapvr-Notification.
                lst_fapvr-notificationitem        =   lst_fapvr-NotificationItem.
                lst_fapvr-notificationtask        =  lst_fapvr-NotificationTask.
                lst_fapvr-equipment                =  lst_fapvr-Equipment.
               lst_fapvr-functionallocation    =  lst_fapvr-FunctionalLocation.
               lst_fapvr-formname                =  lst_fapvr-FormName.
              lst_fapvr-formstatus               =  lst_fapvr-FormStatus.
               lst_fapvr-assigneddate           =  lst_fapvr-AssignedDate.
               lst_fapvr-assignedtime           =  lst_fapvr-AssignedTime.
                 lst_fapvr-assignedby              = lst_fapvr-AssignedBy.
                APPEND  lst_fapvr TO  lit_fapvr.
                clear:  lst_fapvr.
            ENDLOOP.

            if lit_fapvr[] IS NOT INITIAL.
                MODIFY /odsmfe/tb_fapvr FROM TABLE @lit_fapvr.
            endif. "/if lit_fapvr[] IS NOT INITIAL.

        endif. "/if roots_to_create IS NOT INITIAL.

         "/ --------------Handle Update Method-----------------------/"
        if roots_to_update IS NOT INITIAL.
            LOOP AT roots_to_update INTO DATA(lst_to_update).
                lst_fapvr-formid                       = lst_to_update-FormID.
                lst_fapvr-version                      = lst_to_update-Version.
                lst_fapvr-workordernum          = lst_to_update-WorkOrderNum.
                lst_fapvr-oprnum                     = lst_to_update-OprNum.
                lst_fapvr-approverid                = lst_to_update-ApproverID.
                lst_fapvr-notification               = lst_to_update-Notification.
                lst_fapvr-notificationitem        = lst_to_update-NotificationItem.
                lst_fapvr-notificationtask        = lst_to_update-NotificationTask.
                lst_fapvr-equipment                = lst_to_update-Equipment.
                lst_fapvr-functionallocation    = lst_to_update-FunctionalLocation.
                lst_fapvr-formname                = lst_to_update-FormName.
                lst_fapvr-formstatus               = lst_to_update-FormStatus.
                lst_fapvr-assigneddate           = lst_to_update-AssignedDate.
                lst_fapvr-assignedtime           = lst_to_update-AssignedTime.
                lst_fapvr-assignedby              = lst_to_update-AssignedBy.
            ENDLOOP. "/LOOP AT roots_to_update INTO DATA(lst_to_update)

            if lst_fapvr IS NOT INITIAL.
                SELECT SINGLE *
                    FROM /odsmfe/tb_fapvr
                    WHERE formid                 = @lst_to_update-FormID
                    AND     version                 = @lst_to_update-Version
                    AND     workordernum      = @lst_to_update-WorkOrderNum
                    AND     oprnum                = @lst_to_update-OprNum
                    AND     approverid           = @lst_to_update-ApproverID
                    AND     notification           = @lst_to_update-Notification
                    AND     notificationitem    = @lst_to_update-NotificationItem
                    AND     notificationtask    = @lst_to_update-NotificationTask
                    AND     equipment            = @lst_to_update-Equipment
                    AND     functionallocation = @lst_to_update-FunctionalLocation
                INTO @lst_fmapr.

            endif. "/ if lst_fapvr IS NOT INITIAL.

            if sy-subrc EQ 0 AND lst_to_update-formid = lst_fmapr-formid.
                MODIFY /odsmfe/tb_fapvr FROM @lst_fapvr.
                if sy-subrc NE 0.
                    clear: lst_fapvr.
                endif. "/if sy-subrc NE 0.

            endif. "/ if sy-subrc EQ 0 AND lst_fapvr-formid = lst_fmapr-formid.
            clear: lst_fapvr, lst_fmapr.

        endif. "/if roots_to_update IS NOT INITIAL.

         "/ -------------------Handle Delete Method----------------------------/"
       if roots_to_delete IS NOT INITIAL.
            LOOP AT roots_to_delete INTO DATA(lst_to_delete).
                lst_fapvr-formid                       = lst_to_delete-FormID.
                lst_fapvr-version                      =  lst_to_delete-Version.
                lst_fapvr-workordernum          = lst_to_delete-WorkOrderNum.
                lst_fapvr-workordernum = |{ lst_fapvr-workordernum ALPHA = IN }|.
                lst_fapvr-oprnum                     = lst_to_delete-OprNum.
                lst_fapvr-approverid                = lst_to_delete-ApproverID.
                lst_fapvr-notification               = lst_to_delete-Notification.
                lst_fapvr-notification     = |{ lst_fapvr-notification ALPHA = IN }|.
                lst_fapvr-notificationitem        = lst_to_delete-NotificationItem.
                lst_fapvr-notificationtask        = lst_to_delete-NotificationTask.
                lst_fapvr-equipment                = lst_to_delete-Equipment.
                lst_fapvr-functionallocation    = lst_to_delete-FunctionalLocation.
            ENDLOOP. "/LOOP AT roots_to_delete INTO DATA(lst_to_delete).

            if lst_fapvr IS NOT INITIAL.
                 SELECT SINGLE *
                    FROM /odsmfe/tb_fapvr
                    WHERE formid                 = @lst_fapvr-formid
                    AND     version                 = @lst_fapvr-version
                    AND     workordernum      = @lst_fapvr-workordernum
                    AND     oprnum                = @lst_fapvr-oprnum
                    AND     approverid           = @lst_fapvr-approverid
                    AND     notification           = @lst_fapvr-notification
                    AND     notificationitem    = @lst_fapvr-notificationitem
                    AND     notificationtask    = @lst_fapvr-notificationtask
                    AND     equipment            = @lst_fapvr-equipment
                    AND     functionallocation = @lst_fapvr-functionallocation
                INTO @lst_fmapr.

                if sy-subrc EQ 0 AND lst_fmapr IS NOT INITIAL.
                    DELETE /odsmfe/tb_fapvr FROM @lst_fmapr.
                endif. "/if sy-subrc EQ 0 AND lst_fmapr IS NOT INITIAL.

            endif. "/if lst_fapvr IS NOT INITIAL.

       endif. "/if roots_to_delete IS NOT INITIAL.

    ENDMETHOD.

    METHOD read.
    ENDMETHOD.


ENDCLASS.
