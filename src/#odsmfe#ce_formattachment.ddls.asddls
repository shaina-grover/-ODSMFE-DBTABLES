@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'custom entity for FormAttachment'
define root custom entity /ODSMFE/CE_FormAttachment

{

  key InstanceId    : /odsmfe/de_instanceid;

  key FormId        : /odsmfe/de_formid;

  key Version       : /odsmfe/de_version;

      @EndUserText.label : 'Document Counter'
  key AttachCounter     : abap.numc(8);


      @EndUserText.label : 'relative sto loc of phy doc'
      FileName     : abap.char(255);
      
      FileSize        : abap.numc(12);

      @EndUserText.label : 'HTML content type'
      MimeType      : abap.char(128);

      Description   : /odsmfe/de_description;

      ObjectNum        : aufnr;
      
      OrderType      : abap.char(4);

      @EndUserText.label : 'operation/activity number'
      OperationNum         : abap.char(4);

      @EndUserText.label : 'equipment number'
      Equipment     : abap.char(18);

      @EndUserText.label : 'functional location'
      FunctionalLoc : abap.char(40);

      @EndUserText.label : 'string of variable length'
      ImageData     : abap.string(0);

      QuestionId    : /odsmfe/de_question;

      CreatedOn    : /odsmfe/de_createdon;

      CreatedBy    : /odsmfe/de_createdby;

      ModifiedOn   : /odsmfe/de_modifiedon;

      ModifiedBy   : /odsmfe/de_modifiedby;
      
      IntDataPoints : abap.char(1);

}
