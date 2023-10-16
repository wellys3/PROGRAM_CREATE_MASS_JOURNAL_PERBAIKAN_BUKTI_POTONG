@AbapCatalog.sqlViewName: 'ZFIVT00122'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'Program ZFI05E0029 - Table MBEW'
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view ZFICD_ZFI05E0029_MBEW
  as select distinct from mbew as a
{
  key a.mandt,
  key a.matnr,
//  key a.bwkey,
      a.bklas
//}where a.bklas != '';
}where a.bklas like 'IT%';
