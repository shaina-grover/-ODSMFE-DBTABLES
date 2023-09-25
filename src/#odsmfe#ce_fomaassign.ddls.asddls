@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom entity for FormManualAssignment'
define root custom entity /ODSMFE/CE_FOMAASSIGN
{

  key FormID             : /odsmfe/de_formid;

  key Version            : /odsmfe/de_version;

  key FormAssignmentType : /odsmfe/de_objcat;

  key WorkOrderNum       : aufnr;

      @EndUserText.label : 'Opearation Number'
  key OprNum             : abap.char(4);

      @EndUserText.label : 'Notification'
  key Notification       : abap.char(12);

      @EndUserText.label : 'Notification Item'
  key NotificationItem   : abap.numc(4);

      @EndUserText.label : 'Task'
  key NotificationTask   : abap.numc(4);

      @EndUserText.label : 'Equipment'
  key Equipment          : abap.char(18);

      @EndUserText.label : 'Functional loc.'
  key FunctionalLocation : abap.char(40);

//      @EndUserText.label : 'EnteredBy'
//      EnteredBy          : abap.char(12);

      Mandatory          : /odsmfe/de_mandatory;

      MultipleSub        : /odsmfe/de_multiplesub;

      Occur              : /odsmfe/de_occur;

      FormCategory       : /odsmfe/de_formcategory;

      PostNotification   : /odsmfe/de_postnotification;

      Theme              : /odsmfe/de_theme;

      Stylesheet         : /odsmfe/de_style;

      @EndUserText.label : 'Date'
      AssignedDate       : abap.dats(0);

      @EndUserText.label : 'Field of type TIMS'
      //        AssignedTime            :/odsmfe/de_assignedtime;
      AssignedTime       : abap.tims;

      AssignedBy         : /odsmfe/de_assignedby;

      JobType            : /odsmfe/de_jobtype;

      FlowSequence       : /odsmfe/de_flowseqn;

      FormName           : /odsmfe/de_formname;

      Active             : /odsmfe/de_active;

      Deleted            : /odsmfe/de_roleid;



}
