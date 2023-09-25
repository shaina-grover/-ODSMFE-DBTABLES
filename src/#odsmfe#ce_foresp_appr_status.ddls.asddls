@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom entity FormResp Approval Status'
define root custom entity /ODSMFE/CE_FORESP_APPR_STATUS 
 {
    key   FormID            : /odsmfe/de_formid;
    key   Version           : /odsmfe/de_version;
    
    @EndUserText.label: 'InstanceId'
    key FormInstanceID      : /odsmfe/de_instanceid;
    
    @EndUserText.label: 'Approver ID'
    key ApproverID          : /odsmfe/de_approverid;
    
    @EndUserText.label: 'Form Submitted BY'
    key FormSubmittedBy         : /odsmfe/de_formsubmittedby;
    
    @EndUserText.label: 'Counter'
    key Counter                 : /odsmfe/de_counter;
    
        @EndUserText.label: 'Form Content Status'
        FormContentStatus       : /odsmfe/de_formcontentstatus;
        
        @EndUserText.label: 'Remarks'
        Remarks                 : abap.char( 250 );
        
        @EndUserText.label: 'CreatedOn'
        CreatedDate             : abap.dats;
        
        @EndUserText.label: 'Created Time'
  //      CreatedTime             : /odsmfe/de_createdtime;
           CreatedTime             : abap.tims;
         
        @EndUserText.label: 'FormName'
        FormName                : /odsmfe/de_formname;
        
        @EndUserText.label: 'IterationRequired'
        IterationRequired       : abap.char( 1 );
        
        
}
