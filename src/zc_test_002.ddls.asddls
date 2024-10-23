@EndUserText.label: 'ZC_TEST_002'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_TEST_002 provider contract transactional_query as projection on ZI_TEST_002
{
    key ID,
    Data,
    Country
    
}
