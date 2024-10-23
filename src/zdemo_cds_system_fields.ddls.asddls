@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'demo cds system fields'
define view entity zdemo_cds_system_fields
with parameters 
@Environment.systemField : #SYSTEM_LANGUAGE p_langu : sylangu,
@Environment.systemField : #CLIENT p_mandt : abap.clnt,
@Environment.systemField : #SYSTEM_DATE p_datum : abap.dats,
@Environment.systemField : #SYSTEM_TIME p_uzeit : abap.tims, 
@Environment.systemField : #USER p_uname : syuname
as select from ztest001
{
    key id,
    data,
    $parameters.p_langu as langu,
    $parameters.p_mandt as mandant,    
    $parameters.p_datum as datum,    
    $parameters.p_uzeit as uzeit,    
    $parameters.p_uname as uname    
}
