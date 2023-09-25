@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'ODSMFE: Properties for myFmRspApproval'
define root custom entity /ODSMFE/CE_MYFORMSRESPAPPROVAL
{
  key FormID            : /odsmfe/de_formid;
  key Version           : /odsmfe/de_version;
  key FormInstanceID    : /odsmfe/de_instanceid;
  key ApproverID        : /odsmfe/de_approverid;
  key FormSubmittedBy   : /odsmfe/de_formsubmittedby;
  key Counter           : /odsmfe/de_counter;
      FormContentStatus : /odsmfe/de_formcontentstatus;
      Remarks           : abap.char(250);
      CreatedDate       : abap.dats;
      CreatedTime       : abap.tims;
      FormName          : /odsmfe/de_formname;
      IterationRequired : abap.char(1);

}
