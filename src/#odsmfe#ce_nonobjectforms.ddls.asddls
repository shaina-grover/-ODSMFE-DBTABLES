@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'ODSMFE: EntitySet Properties for NonObjectForms'
define custom entity /ODSMFE/CE_NONOBJECTFORMS
{
  key FormID : /odsmfe/de_formid;
  key Version : /odsmfe/de_version;
  key CodeGroup : abap.char( 8 );
  FormName : /odsmfe/de_formname;
Description : /odsmfe/de_formdesc;
FormData : abap.string;
FormHTML : abap.string;
FormModel : abap.string;
Active : /odsmfe/de_active;
Theme : /odsmfe/de_theme;
Stylesheet : /odsmfe/de_style;
CreatedOn : /odsmfe/de_createdon;
CreatedBy : /odsmfe/de_createdby;
ModifiedOn : /odsmfe/de_modified_on;
ModifiedBy : /odsmfe/de_modifiedby;
Category : /odsmfe/de_formcategory;
FunctionalArea : abap.char( 20 );
SubArea : abap.char( 20 );
}

