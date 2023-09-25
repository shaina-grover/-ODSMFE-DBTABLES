@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom entity for FormMaster'


define custom entity /ODSMFE/CE_FORMMASTER 

{
        key FormID   :  /odsmfe/de_formid;
 
        key Version    : /odsmfe/de_version;
 
        @EndUserText.label: 'Code group'
        key CodeGroup  : abap.char(8);
 
        FormName       : /odsmfe/de_formname;
 
        Description    : /odsmfe/de_formdesc;
 
        FormData       : /odsmfe/de_formdata;
 
        FormHTML       : /odsmfe/de_formhtml;
 
        FormModel      : /odsmfe/de_formmodel;
 
        Active         : /odsmfe/de_active;
        
        Theme          : abap.char(123);
        
        Stylesheet     : abap.char(123); 
        
        CreatedOn        : /odsmfe/de_createdon;

        CreatedBy        : /odsmfe/de_createdby;

        ModifiedOn       : /odsmfe/de_modified_on;

        ModifiedBy       : /odsmfe/de_modifiedby;
        
        Category         : /odsmfe/de_formcategory;
        
        @EndUserText.label: 'FunctionalArea'
        FunctionalArea   : abap.char(20);
        
        @EndUserText.label: 'SubArea'
        SubArea          : abap.char(20);
         
}
