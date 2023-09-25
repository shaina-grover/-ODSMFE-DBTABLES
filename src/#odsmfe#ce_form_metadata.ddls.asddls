@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom entity for Form Meta Data'
define root custom entity /ODSMFE/CE_FORM_METADATA 

  {
    key   FormID            : /odsmfe/de_formid;
    key   Version           : /odsmfe/de_version;
    
    @EndUserText.label: 'InstanceId'
    key   InstanceId        : abap.char(50);
    
      @EndUserText.label: 'Submitted Forms'
      Submitted        : abap.char(10);
      
      @EndUserText.label: 'Multiple submissions'
      MultipleSub      : abap.char(1);
      
      @EndUserText.label: 'Order'
      WoNum            : abap.char(12);
      
      @EndUserText.label: 'Order Type'
      OrderType        : abap.char(4);
      
      @EndUserText.label: 'ResponseData'
      ResponseData     : abap.string( 0 );
      
      @EndUserText.label: 'FormHtml'
      FormHtml         : abap.string( 0 );
      
      @EndUserText.label: 'FormModel'
      FormModel        : abap.string( 0 );
      
      @EndUserText.label: 'Occurances'
      Occurances       : /odsmfe/de_occur;
      
      @EndUserText.label: 'CreatedOn'
      CreatedOn        : /odsmfe/de_createdon;
      
      @EndUserText.label: 'CreatedBy'
      CreatedBy        : /odsmfe/de_createdby;
      
      @EndUserText.label: 'IsDraft'
      IsDraft          : /odsmfe/de_isdraft;  
  
}
