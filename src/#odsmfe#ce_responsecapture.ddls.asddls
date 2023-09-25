@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom entity for RESPONSE CAPTURE'

define root custom entity /ODSMFE/CE_RESPONSECAPTURE 
{
key InstanceID : /odsmfe/de_instanceid;

FormID  : /odsmfe/de_formid;

@EndUserText.label: 'Version'
Version : abap.char(3);
@EndUserText.label: 'WorkOrderNum'
WoNum :abap.char(12);

@EndUserText.label: 'OperationNum'
OperationNum : abap.char(4);

@EndUserText.label: 'TaskListType'
TaskListType : abap.char(1);

@EndUserText.label: 'Group1'
FormGroup : abap.char(8);

@EndUserText.label: 'GroupCounter'
GroupCounter : abap.char(2);

@EndUserText.label: 'InternalCounter'
InternalCounter : abap.numc(8);

@EndUserText.label: 'Equipment'
Equipment : abap.char(18);

@EndUserText.label: 'FunctionLocation'
FunctionLocation : abap.char(30);

@EndUserText.label: 'ResponseData'
ResponseData : abap.rawstring;

CreatedOn : timestamp;

CreatedBy : /odsmfe/de_createdby;

ModifiedOn : timestamp;

ModifiedBy : /odsmfe/de_modifiedby;

IsDraft : /odsmfe/de_isdraft;

@EndUserText.label: 'NonObjType'
NonObjType : abap.char(1);

@EndUserText.label: 'OrderType'
OrderType : abap.char(4);


Counter : /odsmfe/de_counter;


Remarks : /odsmfe/de_remark;

FormMetaData : association[1..*] to /ODSMFE/CE_FORM_METADATA on /ODSMFE/CE_RESPONSECAPTURE.InstanceID = FormMetaData.InstanceId;

}
