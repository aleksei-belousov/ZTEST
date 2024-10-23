@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_TEST004000'
@ObjectModel.semanticKey: [ 'ID' ]
define root view entity ZC_TEST004000
  provider contract transactional_query
  as projection on ZR_TEST004000
{
  key ID,
  Data,
  LocalLastChanged
  
}
