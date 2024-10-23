extend view entity C_SALESORDERMANAGE with
{
  @EndUserText.label: 'Extension Field'
  @UI.dataFieldDefault: [{hidden: false}]
  @UI.identification: [{hidden: false}]
  @UI.lineItem: [{hidden: false}]
  SALESORDER.ZZ_TEST_SDH as ZZ_TEST_SDH
  
}
