@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'ODSMFE: CE Properties for myFormsResponseCapture'
define root custom entity /ODSMFE/CE_MYFORMSRESPONSE
{
  key InstanceID            :  /odsmfe/de_instanceid;
         FormID                  :  /odsmfe/de_formid;
         Version                  :  /odsmfe/de_version;
         WoNum                 : aufnr;              
         OperationNum      : abap.char(4);
         TaskListType         : abap.char(1);
         Groups                  : abap.char(8);
         GroupCounter       : abap.char(2);
         InternalCounter    : abap.numc( 8 );
         Equipment            : abap.char(18);
         FunctionLocation  : abap.char(30);
         ResponseData      : abap.string;
         CreatedOn            :  /odsmfe/de_createdon;
         CreatedBy            : /odsmfe/de_createdby;
         ModifiedOn          : /odsmfe/de_modified_on;
         ModifiedBy           : /odsmfe/de_modifiedby;
         NonObjType         : /odsmfe/de_nonobjtype;
         IsDraft                  : /odsmfe/de_isdraft;
         Counter                : /odsmfe/de_counter;
         Remarks               : /odsmfe/de_remark;
         Deleted                : /odsmfe/de_deleted;
         OrderType            : abap.char(4);
         CreatedByName   : abap.char(80);
  
}
