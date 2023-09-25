@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom entity for RESPONSE CAPTURE'
define custom entity /ODSMFE/CE_WORKORDER 
 {
  key WorkOrderNumber : aufnr;
  
  @EndUserText.label: 'OrderType'
  OrderType : abap.char(4);
  
  @EndUserText.label: 'Description'
  Description  : abap.char(40);
}
