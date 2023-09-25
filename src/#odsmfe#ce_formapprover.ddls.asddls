@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'FormApprover'
define root custom entity /ODSMFE/CE_FormApprover 
 {
  key FormID : /odsmfe/de_formid;
  key Version : /odsmfe/de_version;
  key WorkOrderNum : aufnr;
   @EndUserText.label: 'OprNum'
  key OprNum : abap.char(4);
  key ApproverID : /odsmfe/de_approverid;
  @EndUserText.label: 'Notification'
  key Notification : abap.char(12);
  @EndUserText.label: 'NotificationItem'
  key NotificationItem : abap.char(4);
  @EndUserText.label: 'NotificationTask'
  key NotificationTask: abap.char(4);
  @EndUserText.label: 'Equipment'
  key Equipment : abap.char(18);
  @EndUserText.label: 'FunctionalLocation'
  key FunctionalLocation : abap.char(30);
  FormStatus : /odsmfe/de_formstatus;
  AssignedDate : /odsmfe/de_assigneddate;
  AssignedTime : abap.tims;
  AssignedBy : /odsmfe/de_assignedby;
  FormName : /odsmfe/de_formname;
}
