@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'ApplicationConfig'
define custom entity /ODSMFE/CE_ApplicationConfig
  {
  key Recordnum : /odsmfe/de_mfe_record;
  key ParamKey : /odsmfe/de_mfe_key;
      Value : /odsmfe/de_mfe_value;
      Category : /odsmfe/de_mfe_category;
      Active : /odsmfe/de_mfe_active;  
}
