@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_AGENCY_007'
@AbapCatalog.extensibility.dataSources: [ 'Agency' ]
//@AbapCatalog.viewEnhancementCategory: [#PROJECTION_LIST]
//@AbapCatalog.extensibility.extensible: true
//@AbapCatalog.extensibility.elementSuffix: 'ZMY'
define root view entity ZI_AGENCY_007 as select from zagency as Agency
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
    last_changed_at as LastChangedAt
}
