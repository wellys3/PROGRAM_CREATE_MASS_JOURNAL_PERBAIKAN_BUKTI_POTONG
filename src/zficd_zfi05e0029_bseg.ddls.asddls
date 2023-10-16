@AbapCatalog.sqlViewName: 'ZFIVT00119'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'Program ZFI05E0029 - Table BSEG'
/*+[hideWarning] { "IDS" : [ "CALCULATED_FIELD_CHECK" ]  } */
define view ZFICD_ZFI05E0029_BSEG
  as select from    bseg                      as a
    left outer join bkpf                      as b on  a.mandt = b.mandt
                                                   and a.bukrs = b.bukrs
                                                   and a.belnr = b.belnr
                                                   and a.gjahr = b.gjahr

  //ZFIVT00121 - ZMMDT00042
    left outer join ZFICD_ZFI05E0029_ZMMDT042 as c on  a.mandt = c.mandt
                                                   and a.lifnr = c.bpartner_b

  //  //ZFIVT00122 - MBEW
  //    left outer join ZFICD_ZFI05E0029_MBEW     as d on  a.mandt = d.mandt
  //                                                   and a.matnr = d.matnr
  //  //                                                   and a.bwkey = d.bwkey

    left outer join skat                      as e on  e.spras = 'i'
                                                   and e.ktopl = a.bukrs
                                                   and e.saknr = a.hkont

    left outer join makt                      as f on  f.spras = 'E'
                                                   and f.matnr = a.matnr

    left outer join lfa1                      as g on g.lifnr = a.lifnr

//  //ZFIVT00123 - ZFICD_ZFI05E0029_BSET
//    left outer join ZFICD_ZFI05E0029_BSET     as h on  a.mandt = h.mandt
//                                                   and a.bukrs = h.bukrs
//                                                   and a.belnr = h.belnr
//                                                   and a.gjahr = h.gjahr
//                                                   and a.mwskz = h.mwskz
{
  key a.mandt,
  key a.bukrs,
  key a.gjahr,
  key a.belnr,
  key a.buzei,

      a.h_budat,
      a.buzid,
      a.bschl,
      a.koart,
      a.shkzg,
      a.mwskz,
      a.qsskz,

      a.dmbtr,

      a.wrbtr,

      case
        when a.shkzg = 'H' then a.wrbtr * -1
        else a.wrbtr
      end              as wrbtr_b,

      a.hwbas,
      
//      a.fwbas,
      a.ktosl,
      
      a.qsshb,
      a.zuonr,
      a.sgtxt,
      a.kostl,

      a.hkont          as racct,
      e.txt50          as racct_desc,

      a.lifnr,
      g.name1          as lifnr_name,

      a.qbshb,

      a.matnr,
      f.maktx          as matnr_desc,
      
      a.ebeln,
      a.ebelp,
      
      a.bwkey,

      //      a.h_blart,

      b.blart,
      b.bktxt,
      b.waers,
      b.kursf,
      b.glvor,
      b.awkey,

      cast( ( right(b.awkey,4)
            ) as abap.numc(4)
          )            as awkey_gjahr,

      left(b.awkey,10) as awkey_belnr,

      c.fityp

      //      d.bklas

//      h.fwbas,
//      h.fwste
}
