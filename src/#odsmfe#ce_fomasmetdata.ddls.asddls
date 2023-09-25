@ObjectModel.query.implementedBy:'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom entity for Form Master Meta Data'
define custom entity /ODSMFE/CE_FOMASMETDATA 
 {
  key FormID            : /odsmfe/de_formid;
  key  Version           : /odsmfe/de_version;
  
    @EndUserText.label: 'File Name'
    FormName          : abap.char(50);
    
    @EndUserText.label: 'CreatedBy'
    CreatedBy         : abap.char(12);
    
    @EndUserText.label: 'FormCategory'
    FormCategory      : abap.char(30);
    
    @EndUserText.label: 'Active'
    Active            : abap.char(1);
    
    @EndUserText.label: 'Description'
    Description       : abap.char(250);
    
    @EndUserText.label: 'FunctionalArea'
    FunctionalArea    : abap.char(20);
    
    @EndUserText.label: 'FunctionalArea'
    SubArea           : abap.char(20);
    
     
  
}
