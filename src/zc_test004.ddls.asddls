@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_TEST004'
@ObjectModel.semanticKey: [ 'ID' ]
define root view entity ZC_TEST004
  provider contract transactional_query
  as projection on ZR_TEST004
{
  key ID,
  Data,
  LocalLastChanged
  
}
