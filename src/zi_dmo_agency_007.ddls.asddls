@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_DMO_AGENCY_007'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_DMO_AGENCY_007 as select from zagency
{
    key agency_id as AgencyId,
    name as Name,
    street as Street,
    postal_code as PostalCode,
    city as City,
    country_code as CountryCode,
    phone_number as PhoneNumber,
    email_address as EmailAddress,
    web_address as WebAddress,
    attachment as Attachment,
    mime_type as MimeType,
    filename as Filename,
    local_created_by as LocalCreatedBy,
    local_created_at as LocalCreatedAt,
    local_last_changed_by as LocalLastChangedBy,
    local_last_changed_at as LocalLastChangedAt,
    last_changed_at as LastChangedAt,
    dummy_field as DummyField,
    '19891109' as numc8,
    cast( '19891109' as abap.char( 8 ) ) as char8,
    cast( '19891109' as abap.int4 ) as int4,
    cast( '19891109' as abap.dec(10,2) ) as dec_10_2,
    cast( '19891109' as abap.fltp ) as fltp,
    cast( '19891109' as abap.decfloat16 ) as decfloat16,
    cast( '19891109' as abap.decfloat34 ) as decfloat34,
    cast( '19891109' as abap.dats ) as dats,
    cast( '19891109' as abp_creation_user ) as abp_creation_user1,
    cast( cast( '19891109' as abap.char(12) ) as abp_creation_user preserving type ) as abp_creation_user2
    
}
