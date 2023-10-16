@AbapCatalog.sqlViewName: 'ZFIVT00120'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'Program ZFI05E0029 - Table EKBE'
define view ZFICD_ZFI05E0029_EKBE
  as select from ekbe as a
{
  key a.mandt,
  key a.ebeln,
  key a.ebelp,
  key a.zekkn,
  key a.vgabe,
  key a.gjahr,
  key a.belnr,
  key a.buzei,

      //lpad( a.bpartner_gen, 10, '0' ) as bpartner_gen_b

  key cast(
            (
                  cast(
                        (
                          lpad ( a.buzei, 6 , '0' )
                        ) as abap.char(6)
                      )
            ) as abap.numc(6)
      )     as buzei_b,

  key cast( (concat(a.belnr, a.gjahr) ) as abap.char(20)
          ) as awkey
}
