*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_F02
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA_REVERSE_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_process_data_reverse_po .

  DATA: ld_no TYPE zfist00194-zno.

*--------------------------------------------------------------------*

  CLEAR ld_no.
  LOOP AT git_bseg INTO DATA(gwa_bseg).

    ADD 1 TO ld_no.

    CLEAR git_lineitem.
    git_lineitem-zno = ld_no.
    git_lineitem-matnr = gwa_bseg-matnr.
    git_lineitem-maktx = gwa_bseg-matnr_desc.

    git_lineitem-racct = gwa_bseg-racct.
    git_lineitem-racct_desc = gwa_bseg-racct_desc.
    git_lineitem-lifnr = gwa_bseg-lifnr.
    git_lineitem-lifnr_name = gwa_bseg-lifnr_name.

*    git_lineitem-amount = gwa_bseg-wrbtr_b.
    CASE gwa_bseg-shkzg.
      WHEN 'H'.  "Credit --> To Be +
        git_lineitem-amount = abs( gwa_bseg-wrbtr_b ).
      WHEN 'S'.  "Debit --> To Be -
        git_lineitem-amount = abs( gwa_bseg-wrbtr_b ) * -1.
    ENDCASE.

    git_lineitem-mwskz = gwa_bseg-mwskz.
    git_lineitem-witht = gwa_bseg-witht.
    git_lineitem-withcd = gwa_bseg-withcd.

    READ TABLE git_withitem INTO DATA(lwa_withitem)
      WITH KEY witht = gwa_bseg-witht
               wt_withcd = gwa_bseg-withcd.
    IF sy-subrc EQ 0.
      git_lineitem-qsshh = ( lwa_withitem-wt_qsshh * -1 ).
      git_lineitem-qsshb = ( lwa_withitem-wt_qsshb ).
      git_lineitem-qbshb = ( lwa_withitem-wt_qbshb ).
    ENDIF.

    git_lineitem-kostl = gwa_bseg-kostl.
    git_lineitem-sgtxt = gc_sgtxt_reverse.
    git_lineitem-kind = gc_fl_posting_reverse.

    git_lineitem-koart = gwa_bseg-koart.
    git_lineitem-zuonr = gwa_bseg-zuonr.

    git_lineitem-ktosl = gwa_bseg-ktosl.

    APPEND git_lineitem.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA_REVERSE_NON_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_process_data_reverse_non_po .

  DATA: ld_no            TYPE zfist00194-zno.

*--------------------------------------------------------------------*

  CLEAR ld_no.
  LOOP AT git_bseg INTO DATA(gwa_bseg).

    ADD 1 TO ld_no.

    CLEAR git_lineitem.
    git_lineitem-zno = ld_no.
    git_lineitem-matnr = gwa_bseg-matnr.
    git_lineitem-maktx = gwa_bseg-matnr_desc.

    git_lineitem-racct = gwa_bseg-racct.
    git_lineitem-racct_desc = gwa_bseg-racct_desc.
    git_lineitem-lifnr = gwa_bseg-lifnr.
    git_lineitem-lifnr_name = gwa_bseg-lifnr_name.

*    git_lineitem-amount = gwa_bseg-wrbtr_b.
    CASE gwa_bseg-shkzg.
      WHEN 'H'.  "Credit --> To Be +
        git_lineitem-amount = abs( gwa_bseg-wrbtr_b ).
      WHEN 'S'.  "Debit --> To Be -
        git_lineitem-amount = abs( gwa_bseg-wrbtr_b ) * -1.
    ENDCASE.

    git_lineitem-mwskz = gwa_bseg-mwskz.
    git_lineitem-witht = gwa_bseg-witht.
    git_lineitem-withcd = gwa_bseg-withcd.

    READ TABLE git_withitem INTO DATA(lwa_withitem)
      WITH KEY witht = gwa_bseg-witht
               wt_withcd = gwa_bseg-withcd.
    IF sy-subrc EQ 0.
      git_lineitem-qsshh = ( lwa_withitem-wt_qsshh * -1 ).
      git_lineitem-qsshb = ( lwa_withitem-wt_qsshb ).
      git_lineitem-qbshb = ( lwa_withitem-wt_qbshb ).
    ENDIF.

    git_lineitem-kostl = gwa_bseg-kostl.
    git_lineitem-sgtxt = gc_sgtxt_reverse.
    git_lineitem-kind = gc_fl_posting_reverse.

    git_lineitem-koart = gwa_bseg-koart.
    git_lineitem-zuonr = gwa_bseg-zuonr.

    git_lineitem-ktosl = gwa_bseg-ktosl.

    APPEND git_lineitem.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA_PPH26
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_process_data_pph26 USING p_gwa_header TYPE zfist00193.

  DATA: ld_no                         TYPE zfist00194-zno,
        ld_amount                     TYPE zfist00194-amount,
        ld_sum_amount                 TYPE zfist00194-amount,
        ld_qsshh                      TYPE qsshh,
        ld_wht_amount                 TYPE wt_wt,
        ld_temp_qsshh                 TYPE zfist00194-qsshh,
        ld_kostl                      TYPE bseg-kostl,
        ld_zuonr                      TYPE bseg-zuonr,

        lwa_witht_withcd              TYPE gty_witht_withcd,
        lwa_lineitem                  LIKE LINE OF git_lineitem,
        lit_lineitem_posting_jurnal_1 LIKE  git_lineitem,
        lit_lineitem_posting_jurnal_2 LIKE git_lineitem.

  RANGES: lra_filter_pph26 FOR acdoca-racct.

*--------------------------------------------------------------------*

  CLEAR lra_filter_pph26[].
  f_fill_range: lra_filter_pph26 'I' 'BT' '0000105000' '0000105999'.

  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Reclass Jurnal 1

  CLEAR: ld_no, ld_kostl, ld_zuonr, ld_amount, ld_sum_amount.
  LOOP AT git_bseg INTO DATA(gwa_bseg).

    ADD 1 TO ld_no.

    CLEAR lwa_lineitem.
    lwa_lineitem-zno = ld_no.
*    lwa_lineitem-matnr = gwa_bseg-matnr.
*    lwa_lineitem-maktx = gwa_bseg-matnr_desc.

    "*--------------------------------------------------------------------*

    READ TABLE git_zfidt00322 INTO DATA(lwa_zfidt00322)
      WITH KEY racct = gwa_bseg-racct.
    IF sy-subrc EQ 0.
      lwa_lineitem-matnr = lwa_zfidt00322-matnr.

      SELECT SINGLE maktx FROM makt INTO lwa_lineitem-maktx
        WHERE matnr EQ lwa_zfidt00322-matnr.

      lwa_lineitem-racct = lwa_zfidt00322-racct_to.
      lwa_lineitem-kostl = lwa_zfidt00322-kostl.

      "---

      ld_kostl = lwa_zfidt00322-kostl.
      ld_zuonr = gwa_bseg-zuonr.

      "---

      SELECT SINGLE txt50 FROM skat INTO lwa_lineitem-racct_desc
        WHERE spras EQ 'i' AND
              ktopl EQ p_gwa_header-bukrs AND
              saknr EQ lwa_zfidt00322-racct_to.

    ELSE.

      IF gwa_bseg-racct IN lra_filter_pph26.

        lwa_lineitem-racct = gd_racct_adj_pph26.

        SELECT SINGLE txt50 FROM skat INTO lwa_lineitem-racct_desc
          WHERE spras EQ 'i' AND
                ktopl EQ p_gwa_header-bukrs AND
                saknr EQ gd_racct_adj_pph26.

      ENDIF.

    ENDIF.

    "*--------------------------------------------------------------------*

    lwa_lineitem-lifnr = gwa_bseg-lifnr.
    lwa_lineitem-lifnr_name = gwa_bseg-lifnr_name.

*    lwa_lineitem-amount = gwa_bseg-wrbtr_b.
    CASE gwa_bseg-shkzg.
      WHEN 'H'.  "Credit --> To Be +
        lwa_lineitem-amount = abs( gwa_bseg-wrbtr_b ).
      WHEN 'S'.  "Debit --> To Be -
        lwa_lineitem-amount = abs( gwa_bseg-wrbtr_b ) * -1.
    ENDCASE.

    lwa_lineitem-mwskz = gwa_bseg-mwskz.
    lwa_lineitem-witht = gwa_bseg-witht.
    lwa_lineitem-withcd = gwa_bseg-withcd.

    READ TABLE git_withitem INTO DATA(lwa_withitem)
      WITH KEY witht = gwa_bseg-witht
               wt_withcd = gwa_bseg-withcd.
    IF sy-subrc EQ 0.
      lwa_lineitem-qsshh = ( lwa_withitem-wt_qsshh * -1 ).
      lwa_lineitem-qsshb = ( lwa_withitem-wt_qsshb ).
      lwa_lineitem-qbshb = ( lwa_withitem-wt_qbshb ).
    ENDIF.

    lwa_lineitem-sgtxt = gc_sgtxt_reverse.
    lwa_lineitem-kind = gc_fl_posting_reverse_26.

    lwa_lineitem-koart = gwa_bseg-koart.
    lwa_lineitem-zuonr = gwa_bseg-zuonr.

    lwa_lineitem-ktosl = gwa_bseg-ktosl.

    APPEND lwa_lineitem TO git_lineitem.

  ENDLOOP.

  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Posting Jurnal 2

  "*--------------------------------------------------------------------*
  "Account Vendor (A1)

  READ TABLE git_lineitem WITH KEY racct = gd_racct_adj_pph26.
  IF sy-subrc EQ 0.

    ADD 1 TO ld_no.
    CLEAR lwa_lineitem.
    lwa_lineitem-zno = ld_no.

    READ TABLE git_zfidt00322 INTO lwa_zfidt00322 INDEX 1.
    IF sy-subrc EQ 0.
      lwa_lineitem-lifnr = lwa_zfidt00322-lifnr.

      SELECT SINGLE name1 FROM lfa1 INTO lwa_lineitem-lifnr_name
        WHERE lifnr EQ lwa_zfidt00322-lifnr.

      SELECT SINGLE akont FROM lfb1 INTO @lwa_lineitem-racct
        WHERE bukrs EQ @p_gwa_header-bukrs AND
              lifnr EQ @lwa_lineitem-lifnr.

      SELECT SINGLE txt50 FROM skat INTO lwa_lineitem-racct_desc
        WHERE spras EQ 'i' AND
              ktopl EQ p_gwa_header-bukrs AND
              saknr EQ lwa_lineitem-racct.

    ENDIF.

    CLEAR ld_amount.
    ld_amount = abs( git_lineitem-amount ).
    lwa_lineitem-amount = ld_amount * -1.

    lwa_lineitem-koart = 'K'.
    lwa_lineitem-zuonr = git_lineitem-zuonr.
    lwa_lineitem-kind = gc_fl_posting_1.
    lwa_lineitem-sgtxt = gc_sgtxt_posting.

    lwa_lineitem-ktosl = git_lineitem-ktosl.

    APPEND lwa_lineitem TO git_lineitem.
    ADD ld_amount TO ld_sum_amount.

  ENDIF.

  "*--------------------------------------------------------------------*
  "Account GL (A3)

  CLEAR ld_temp_qsshh.
  CLEAR lwa_witht_withcd.
  READ TABLE git_zfidt00322 INTO lwa_zfidt00322 INDEX 1.
  IF sy-subrc EQ 0.

    ADD 1 TO ld_no.
    CLEAR lwa_lineitem.
    lwa_lineitem-zno = ld_no.
    lwa_lineitem-racct = gc_racct_utang_pph26.

    SELECT SINGLE txt50 FROM skat INTO lwa_lineitem-racct_desc
      WHERE spras EQ 'i' AND
            ktopl EQ p_gwa_header-bukrs AND
            saknr EQ gc_racct_utang_pph26.

    lwa_lineitem-koart = 'S'.
    lwa_lineitem-zuonr = ''.

*    PERFORM f_get_witht_withcd_whtbase    USING lwa_zfidt00322-bpartner
*                                                lwa_zfidt00322-matnr
*                                       CHANGING lwa_lineitem-witht
*                                                lwa_lineitem-withcd
*                                                lwa_lineitem-qsshh.

    PERFORM f_get_witht_withcd_whtbase    USING lwa_zfidt00322-lifnr
                                                lwa_zfidt00322-matnr
                                       CHANGING lwa_witht_withcd-witht
                                                lwa_witht_withcd-withcd.
*                                                lwa_lineitem-qsshh.

*    lwa_witht_withcd-witht = lwa_lineitem-witht.
*    lwa_witht_withcd-withcd = lwa_lineitem-withcd.

    ld_temp_qsshh = ld_sum_amount.

    CLEAR ld_qsshh.
    CLEAR ld_wht_amount.
    CALL FUNCTION 'ZFIFM_CALCULATE_WHT'
      EXPORTING
        im_bukrs     = p_gwa_header-bukrs
        im_lifnr     = lwa_zfidt00322-lifnr
*       IM_BUPLA     =
*       IM_WT_CALY   =
*       im_witht     = lwa_lineitem-witht
*       im_wt_withcd = lwa_lineitem-withcd
        im_witht     = lwa_witht_withcd-witht
        im_wt_withcd = lwa_witht_withcd-withcd
        im_dmbtr     = ld_sum_amount
      IMPORTING
        ex_wt_wt     = ld_wht_amount
        ex_qsshh     = ld_qsshh.

    CLEAR ld_amount.
*    ld_amount = abs( ld_qsshh ).
    ld_amount = abs( ld_wht_amount ).
    lwa_lineitem-amount = ld_amount * -1.

    lwa_lineitem-kind = gc_fl_posting_1.
    lwa_lineitem-sgtxt = gc_sgtxt_posting.

    lwa_lineitem-ktosl = 'WIT'.

    APPEND lwa_lineitem TO git_lineitem.
    ADD ld_amount TO ld_sum_amount.

  ENDIF.

  "*--------------------------------------------------------------------*
  "Account GL (A2)

  READ TABLE git_zfidt00322 INTO lwa_zfidt00322 INDEX 1.
*      WITH KEY racct = git_lineitem-racct.
  IF sy-subrc EQ 0.

    ADD 1 TO ld_no.
    CLEAR lwa_lineitem.
    lwa_lineitem-koart = 'S'.
    lwa_lineitem-zno = ld_no.

    lwa_lineitem-matnr = lwa_zfidt00322-matnr.
    SELECT SINGLE maktx FROM makt INTO lwa_lineitem-maktx
      WHERE matnr EQ lwa_zfidt00322-matnr.

    lwa_lineitem-racct = lwa_zfidt00322-racct_to.
    SELECT SINGLE txt50 FROM skat INTO lwa_lineitem-racct_desc
      WHERE spras EQ 'i' AND
            ktopl EQ p_gwa_header-bukrs AND
            saknr EQ lwa_zfidt00322-racct_to.

    lwa_lineitem-amount = ld_sum_amount.

    lwa_lineitem-witht = lwa_witht_withcd-witht.
    lwa_lineitem-withcd = lwa_witht_withcd-withcd.
    lwa_lineitem-kostl = lwa_zfidt00322-kostl.
    lwa_lineitem-zuonr = ld_zuonr.

    lwa_lineitem-qsshh = ld_temp_qsshh.

    lwa_lineitem-kind = gc_fl_posting_1.
    lwa_lineitem-sgtxt = gc_sgtxt_posting.
    APPEND lwa_lineitem TO git_lineitem.

  ENDIF.


  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Posting Jurnal 3

  CLEAR: ld_sum_amount.

  "*--------------------------------------------------------------------*
  "Account Vendor (A1-2)

  LOOP AT git_bseg INTO gwa_bseg.

    "*--------------------------------------------------------------------*

    READ TABLE git_zfidt00322 INTO lwa_zfidt00322
      WITH KEY racct = gwa_bseg-racct.
    IF sy-subrc EQ 0.

      "*--------------------------------------------------------------------*
      "Account Vendor (A1-2)

      ADD 1 TO ld_no.

      CLEAR lwa_lineitem.
      lwa_lineitem-zno = ld_no.
      lwa_lineitem-lifnr = lwa_zfidt00322-lifnr.

      SELECT SINGLE name1 FROM lfa1 INTO lwa_lineitem-lifnr_name
        WHERE lifnr EQ lwa_zfidt00322-lifnr.

      SELECT SINGLE akont FROM lfb1 INTO @lwa_lineitem-racct
        WHERE bukrs EQ @p_gwa_header-bukrs AND
              lifnr EQ @lwa_zfidt00322-lifnr.

      SELECT SINGLE txt50 FROM skat INTO lwa_lineitem-racct_desc
        WHERE spras EQ 'i' AND
              ktopl EQ p_gwa_header-bukrs AND
              saknr EQ lwa_lineitem-racct.

      CLEAR ld_amount.
      ld_amount = abs( gwa_bseg-wrbtr_b ).
      lwa_lineitem-amount = ld_amount.

      lwa_lineitem-koart = 'K'.
      lwa_lineitem-kind = gc_fl_posting_2.
      lwa_lineitem-sgtxt = gc_sgtxt_posting.
      lwa_lineitem-zuonr = gwa_bseg-zuonr.

      APPEND lwa_lineitem TO git_lineitem.

    ELSE.

      "*--------------------------------------------------------------------*
      "Account GL (A1-1)

      ADD 1 TO ld_no.

      CLEAR lwa_lineitem.
      lwa_lineitem-zno = ld_no.

      lwa_lineitem-racct = gd_racct_adj_pph26.

      SELECT SINGLE txt50 FROM skat INTO lwa_lineitem-racct_desc
        WHERE spras EQ 'i' AND
              ktopl EQ p_gwa_header-bukrs AND
              saknr EQ gd_racct_adj_pph26.

      CLEAR ld_amount.
      ld_amount = abs( gwa_bseg-wrbtr_b ).
      lwa_lineitem-amount = ld_amount * -1.

      lwa_lineitem-koart = 'S'.
      lwa_lineitem-kind = gc_fl_posting_2.
      lwa_lineitem-sgtxt = gc_sgtxt_posting.
      lwa_lineitem-zuonr = gwa_bseg-zuonr.

      SELECT SINGLE * FROM csks INTO @DATA(lwa_csks)
        WHERE kokrs EQ @gwa_header-bukrs AND
              kostl EQ @ld_kostl AND
              ( datab <= @sy-datum AND datbi >= @sy-datum ).
      IF sy-subrc EQ 0.
        lwa_lineitem-prctr = lwa_csks-prctr.
      ENDIF.

      APPEND lwa_lineitem TO git_lineitem.

    ENDIF.

    "*--------------------------------------------------------------------*

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_PRE_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pre_posting    USING p_gwa_header TYPE zfist00193
                            p_flag
                   CHANGING p_subrc
                            p_git_message TYPE gtt_message.

  CLEAR gd_answer.
  gd_message = 'Are you sure to post?'.
  PERFORM f_confirm    USING 'Please confirm?'
                             gd_message
                             'Yes'
                             'No'
                    CHANGING gd_answer.

  CHECK gd_answer EQ '1'.

  PERFORM f_posting    USING '' "X = Check | <BLANK> = No Check
                             p_flag
                             p_gwa_header
                    CHANGING p_subrc
                             p_git_message.

  PERFORM f_set_message_numbering CHANGING git_message.
*      PERFORM f_display_message USING git_message.
  PERFORM f_display_message_b USING git_message.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_posting    USING p_fl_check  "X = Check | <BLANK> = No Check
                        p_flag
                        p_gwa_header TYPE zfist00193
               CHANGING p_subrc
                        p_git_message TYPE gtt_message.

  "Declaration BAPI_ACC_DOCUMENT_POST
  DATA: lwa_docheader         TYPE bapiache09,
        lit_accountgl         TYPE TABLE OF bapiacgl09,
        lit_accountreceivable TYPE TABLE OF bapiacar09,
        lit_accountpayable    TYPE TABLE OF bapiacap09,
        lit_accounttax        TYPE TABLE OF  bapiactx09,
        lit_currencyamount    TYPE TABLE OF bapiaccr09,
        lit_extension2        TYPE TABLE OF bapiparex,
        lit_accountwt         TYPE TABLE OF bapiacwt09,
        lit_return            TYPE bapiret2_tty,
        ld_obj_type           TYPE bapiache09-obj_type,
        ld_obj_key            TYPE bapiache09-obj_key,
        ld_obj_sys            TYPE bapiache09-obj_sys.

  DATA: lit_message        TYPE TABLE OF gty_message.

*--------------------------------------------------------------------*
  "Set unique for FLAG and
  "Sort gc_fl_posting_reverse, gc_fl_posting, gc_fl_posting_1, gc_fl_posting_2

  DATA(lit_lineitem_c) = git_lineitem[].
  SORT lit_lineitem_c ASCENDING BY kind.
  DELETE ADJACENT DUPLICATES FROM lit_lineitem_c COMPARING kind.

  SELECT a~*
    FROM @lit_lineitem_c AS a
      WHERE kind EQ @gc_fl_posting_reverse
        APPENDING TABLE @DATA(lit_lineitem)
  ##itab_key_in_select
  ##itab_db_select.

  SELECT a~*
    FROM @lit_lineitem_c AS a
      WHERE kind EQ @gc_fl_posting_reverse_26
        APPENDING TABLE @lit_lineitem
  ##itab_key_in_select
  ##itab_db_select.

  SELECT a~*
    FROM @lit_lineitem_c AS a
      WHERE kind EQ @gc_fl_posting
        APPENDING TABLE @lit_lineitem
  ##itab_key_in_select
  ##itab_db_select.

  SELECT a~*
    FROM @lit_lineitem_c AS a
      WHERE kind EQ @gc_fl_posting_1
        APPENDING TABLE @lit_lineitem
  ##itab_key_in_select
  ##itab_db_select.

  SELECT a~*
    FROM @lit_lineitem_c AS a
      WHERE kind EQ @gc_fl_posting_2
        APPENDING TABLE @lit_lineitem
  ##itab_key_in_select
  ##itab_db_select.

*--------------------------------------------------------------------*

*  CLEAR ld_all_bapi_success.
  LOOP AT lit_lineitem ASSIGNING FIELD-SYMBOL(<lfs_lineitem>).

    CLEAR: lwa_docheader,
           lit_accountgl[],
           lit_accountreceivable[],
           lit_accountpayable[],
           lit_accounttax[],
           lit_currencyamount[],
           lit_accountwt[],
           lit_extension2[],
           lit_return[].

    "*--------------------------------------------------------------------*

    "Collect data for posting
*    PERFORM f_collect_data_posting     TABLES git_lineitem
*                                       USING <lfs_lineitem>-kind
*                                             p_gwa_header
*                                    CHANGING lwa_docheader
*                                             lit_accountgl[]
*                                             lit_accountreceivable[]
*                                             lit_accountpayable[]
*                                             lit_accounttax
*                                             lit_currencyamount[]
*                                             lit_extension2[]
*                                             lit_accountwt[].

    PERFORM f_collect_data_posting_b    TABLES git_lineitem
                                         USING <lfs_lineitem>-kind
                                               p_flag
                                               p_gwa_header
                                      CHANGING lwa_docheader
                                               lit_accountgl[]
                                               lit_accountreceivable[]
                                               lit_accountpayable[]
                                               lit_accounttax
                                               lit_currencyamount[]
                                               lit_extension2[]
                                               lit_accountwt[].

    "*--------------------------------------------------------------------*
    "Posting

    CLEAR lit_return[].
    PERFORM f_call_post    USING p_fl_check  "X = Check | <BLANK> = No Check
                                 <lfs_lineitem>-kind
                                 p_gwa_header
                                 lwa_docheader
                                 lit_accountgl[]
                                 lit_accountpayable[]
                                 lit_accounttax[]
                                 lit_accountreceivable[]
                                 lit_currencyamount[]
                                 lit_extension2[]
                                 lit_accountwt[]
                        CHANGING p_subrc
                                 ld_obj_type
                                 ld_obj_key
                                 ld_obj_sys
                                 lit_return[]

                                 <lfs_lineitem>

                                 p_git_message.

  ENDLOOP.

  "*--------------------------------------------------------------------*

  CHECK p_fl_check EQ ''.  "X = Check | <BLANK> = No Check

  "*--------------------------------------------------------------------*
  "Commit or RollBack

  CLEAR lit_return[].
  PERFORM f_bapi_commit_or_rollback   TABLES lit_lineitem
                                       USING <lfs_lineitem>-kind
                                             p_gwa_header
                                    CHANGING lit_return[].

  IF lit_return[] IS NOT INITIAL.
    CLEAR lit_message[].
    lit_message[] = CORRESPONDING #( lit_return[] ).
    APPEND LINES OF lit_message[] TO p_git_message[].
  ENDIF.

  "*--------------------------------------------------------------------*
  "Insert Log ZFIDT00326 & ZFIDT00327

  CLEAR lit_return[].
  PERFORM f_insert_zfidt00326 TABLES lit_lineitem
                                     git_lineitem[]
                               USING p_gwa_header
                            CHANGING lit_return[].

  IF lit_return[] IS NOT INITIAL.
    CLEAR lit_message[].
    lit_message[] = CORRESPONDING #( lit_return[] ).
    APPEND LINES OF lit_message[] TO p_git_message[].
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_COLLECT_DATA_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GWA_HEADER
*&      --> GIT_LINEITEM[]
*&      <-- LIT_ACCOUNTGL[]
*&      <-- LIT_ACCOUNTRECEIVABLE[]
*&      <-- LIT_ACCOUNTPAYABLE[]
*&      <-- LIT_CURRENCYAMOUNT[]
*&      <-- LIT_EXTENSION2[]
*&---------------------------------------------------------------------*
FORM f_collect_data_posting    TABLES p_git_lineitem STRUCTURE zfist00194
                                USING p_kind
                                      p_gwa_header TYPE zfist00193
                             CHANGING p_lwa_docheader TYPE bapiache09
                                      p_lit_accountgl TYPE bapiacgl09_tab
                                      p_lit_accountreceivable TYPE bapiacar09_tab
                                      p_lit_accountpayable TYPE bapiacap09_tab
                                      p_lit_accounttax TYPE bapiactx09_tab
                                      p_lit_currencyamount TYPE bapiaccr09_tab
                                      p_lit_extension2 TYPE bapiparex_tab
                                      p_lit_accountwt TYPE bapiacwt09_tab.

  TYPES: BEGIN OF lty_lineitem_mwskz.
      INCLUDE TYPE zfist00194.
  TYPES: is_used TYPE boolean.
  TYPES: END OF lty_lineitem_mwskz.

  RANGES: lra_mwskz FOR bseg-mwskz.

  "Declaration BAPI_ACC_DOCUMENT_POST
  DATA: lwa_accountgl      LIKE LINE OF p_lit_accountgl,
        lwa_accountpayable LIKE LINE OF p_lit_accountpayable,
        lwa_accounttax     LIKE LINE OF p_lit_accounttax,
        lwa_currencyamount LIKE LINE OF p_lit_currencyamount,
        lwa_accountwt      LIKE LINE OF p_lit_accountwt,
        lwa_extension2     TYPE bapiparex.

  DATA: "lit_lineitem       TYPE TABLE OF zfist00194,
    lit_lineitem_mwskz TYPE TABLE OF lty_lineitem_mwskz,
    ld_numbering       TYPE posnr_acc,
    ld_amount          TYPE zfist00194-amount,
    ld_sum_amount      TYPE zfist00194-amount.

*--------------------------------------------------------------------*

  CLEAR lra_mwskz[].
  f_fill_range: lra_mwskz 'I' 'EQ' '' ''.
  f_fill_range: lra_mwskz 'I' 'EQ' '**' ''.

*  DATA(lit_lineitem_mwskz) = p_git_lineitem[].
*  DELETE lit_lineitem_mwskz WHERE mwskz IN lra_mwskz.

  "---

  lit_lineitem_mwskz[] = CORRESPONDING #( p_git_lineitem[] ).
  DELETE lit_lineitem_mwskz WHERE mwskz IN lra_mwskz.
  SORT lit_lineitem_mwskz ASCENDING BY mwskz.
  DELETE ADJACENT DUPLICATES FROM lit_lineitem_mwskz COMPARING mwskz.

  "*--------------------------------------------------------------------*

  CLEAR ld_numbering.

  "Document Header
  p_lwa_docheader-bus_act = 'RFBU'.
  p_lwa_docheader-username = sy-uname.
  p_lwa_docheader-header_txt = gc_bktxt.
  p_lwa_docheader-comp_code = p_gwa_header-bukrs.
  p_lwa_docheader-doc_date = p_gwa_header-bldat.
  p_lwa_docheader-pstng_date = p_gwa_header-budat.
*  p_lwa_docheader-trans_date = p_gwa_header-bldat.
*  p_lwa_docheader-fisc_year = p_gwa_header-bldat(4).
*  p_lwa_docheader-fis_period = p_gwa_header-bldat+4(2).
  p_lwa_docheader-doc_type = p_gwa_header-blart.
  p_lwa_docheader-ref_doc_no = p_gwa_header-bktxt.

  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Line Item

  LOOP AT p_git_lineitem WHERE kind EQ p_kind.

*    CASE p_kind.
*      WHEN gc_fl_posting_reverse OR
*           gc_fl_posting_reverse_26 OR
*           gc_fl_posting OR
*           gc_fl_posting_1 OR
*           gc_fl_posting_2.

    "*--------------------------------------------------------------------*
    "*--------------------------------------------------------------------*
    "*--------------------------------------------------------------------*

    CLEAR: ld_amount, ld_sum_amount.
    CASE p_git_lineitem-koart.
      WHEN 'S'.

        CHECK p_git_lineitem-ktosl NE 'WIT'.

        CASE p_kind.
          WHEN gc_fl_posting_1.

            "*--------------------------------------------------------------------*

            "Line Item GL

            ADD 1 TO ld_numbering.

            CLEAR lwa_accountgl.
            lwa_accountgl-itemno_acc = ld_numbering.
            lwa_accountgl-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
            lwa_accountgl-item_text = p_git_lineitem-sgtxt.
            lwa_accountgl-costcenter =  |{ p_git_lineitem-kostl ALPHA = IN }|.
            lwa_accountgl-material = p_git_lineitem-matnr.
            lwa_accountgl-alloc_nmbr = p_git_lineitem-zuonr.
            lwa_accountgl-tax_code = 'V0'.
            APPEND lwa_accountgl TO p_lit_accountgl.

            "*--------------------------------------------------------------------*
            "Line Currency GL Amount

            CLEAR lwa_currencyamount.
            lwa_currencyamount-itemno_acc = ld_numbering.
            lwa_currencyamount-currency = p_gwa_header-waers.

            IF p_gwa_header-waers NE 'IDR'.
              lwa_currencyamount-exch_rate = p_gwa_header-kursf.
            ENDIF.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              p_git_lineitem-amount
                                     CHANGING lwa_currencyamount-amt_doccur.
            APPEND lwa_currencyamount TO p_lit_currencyamount.

            "*--------------------------------------------------------------------*

          WHEN OTHERS.

            "*--------------------------------------------------------------------*

            "Line Item GL

            ADD 1 TO ld_numbering.

            CLEAR lwa_accountgl.
            lwa_accountgl-itemno_acc = ld_numbering.
            lwa_accountgl-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
            lwa_accountgl-item_text = p_git_lineitem-sgtxt.
            lwa_accountgl-costcenter =  |{ p_git_lineitem-kostl ALPHA = IN }|.
            lwa_accountgl-material = p_git_lineitem-matnr.
            lwa_accountgl-alloc_nmbr = p_git_lineitem-zuonr.
            lwa_accountgl-tax_code = p_git_lineitem-mwskz.
            APPEND lwa_accountgl TO p_lit_accountgl.

            "*--------------------------------------------------------------------*
            "Line Currency GL Amount

            CLEAR lwa_currencyamount.
            lwa_currencyamount-itemno_acc = ld_numbering.
            lwa_currencyamount-currency = p_gwa_header-waers.

            IF p_gwa_header-waers NE 'IDR'.
              lwa_currencyamount-exch_rate = p_gwa_header-kursf.
            ENDIF.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              p_git_lineitem-amount
                                     CHANGING lwa_currencyamount-amt_doccur.
            APPEND lwa_currencyamount TO p_lit_currencyamount.

            "*--------------------------------------------------------------------*

        ENDCASE.

        "*--------------------------------------------------------------------*
        "Line AccountWT GL

*            IF p_git_lineitem-qsshh NE 0.
*
*              CLEAR lwa_accountwt.
*              lwa_accountwt-itemno_acc = ld_numbering.
*              lwa_accountwt-wt_type = p_git_lineitem-witht.
*              lwa_accountwt-wt_code = p_git_lineitem-withcd.
*              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
*                                                p_gwa_header-waers
*                                                p_git_lineitem-qsshh
*                                       CHANGING lwa_accountwt-bas_amt_lc.
*              APPEND lwa_accountwt TO p_lit_accountwt.
*
*            ENDIF.

        "*--------------------------------------------------------------------*
        "*--------------------------------------------------------------------*
        "*--------------------------------------------------------------------*

      WHEN 'K'.

        "*--------------------------------------------------------------------*
        "Line Item AP

        ADD 1 TO ld_numbering.

        CLEAR lwa_accountpayable.
        lwa_accountpayable-itemno_acc = ld_numbering.
        lwa_accountpayable-vendor_no = |{ p_git_lineitem-lifnr ALPHA = IN }|.
        lwa_accountpayable-item_text = p_git_lineitem-sgtxt.
        lwa_accountpayable-alloc_nmbr = p_git_lineitem-zuonr.
        lwa_accountpayable-tax_code = p_git_lineitem-mwskz.
        lwa_accountpayable-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
        APPEND lwa_accountpayable TO p_lit_accountpayable.

        "*--------------------------------------------------------------------*
        "Line Currency AP Amount

        CLEAR lwa_currencyamount.
        lwa_currencyamount-itemno_acc = ld_numbering.
        lwa_currencyamount-currency = p_gwa_header-waers.

        IF p_gwa_header-waers NE 'IDR'.
          lwa_currencyamount-exch_rate = p_gwa_header-kursf.
        ENDIF.

        PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                          p_gwa_header-waers
                                          p_git_lineitem-amount
                                 CHANGING lwa_currencyamount-amt_doccur.
        APPEND lwa_currencyamount TO p_lit_currencyamount.

        CLEAR ld_amount.
        ld_amount = abs( p_git_lineitem-amount ).
        ADD ld_amount TO ld_sum_amount.

        "*--------------------------------------------------------------------*
        "Line Account WT AP

        CASE p_kind.
          WHEN gc_fl_posting_reverse.

            IF p_git_lineitem-qsshh NE 0.

              CLEAR lwa_accountwt.
              lwa_accountwt-itemno_acc = ld_numbering.
              lwa_accountwt-wt_type = p_git_lineitem-witht.
              lwa_accountwt-wt_code = p_git_lineitem-withcd.

              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                p_gwa_header-waers
                                                p_git_lineitem-qsshb
                                       CHANGING lwa_accountwt-bas_amt_tc.

              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                p_gwa_header-waers
                                                p_git_lineitem-qbshb
                                       CHANGING lwa_accountwt-man_amt_tc.

              APPEND lwa_accountwt TO p_lit_accountwt.

            ENDIF.

          WHEN gc_fl_posting.

            IF p_git_lineitem-qsshh NE 0.

              CLEAR lwa_accountwt.
              lwa_accountwt-itemno_acc = ld_numbering.
              lwa_accountwt-wt_type = p_git_lineitem-witht.
              lwa_accountwt-wt_code = p_git_lineitem-withcd.

              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                p_gwa_header-waers
                                                p_git_lineitem-amount
                                       CHANGING lwa_accountwt-bas_amt_tc.

              APPEND lwa_accountwt TO p_lit_accountwt.

            ENDIF.

          WHEN gc_fl_posting_1.

            READ TABLE p_git_lineitem INDEX 4.
            IF sy-subrc EQ 0.

              CLEAR lwa_accountwt.
              lwa_accountwt-itemno_acc = ld_numbering.
              lwa_accountwt-wt_type = p_git_lineitem-witht.
              lwa_accountwt-wt_code = p_git_lineitem-withcd.

              CLEAR p_git_lineitem-amount.
              ld_amount = p_git_lineitem-amount.
              ADD ld_amount TO ld_sum_amount.

              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                p_gwa_header-waers
                                                ld_sum_amount
                                       CHANGING lwa_accountwt-bas_amt_tc.

              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                p_gwa_header-waers
                                                p_git_lineitem-amount
                                       CHANGING lwa_accountwt-man_amt_tc.

              APPEND lwa_accountwt TO p_lit_accountwt.

            ENDIF.

        ENDCASE.

    ENDCASE.

*    ENDCASE.

  ENDLOOP.

  "*--------------------------------------------------------------------*
  "Line Account Tax

  CASE p_kind.
    WHEN gc_fl_posting_reverse OR
         gc_fl_posting.

      IF lit_lineitem_mwskz[] IS NOT INITIAL.

        LOOP AT lit_lineitem_mwskz ASSIGNING FIELD-SYMBOL(<lfs_lineitem_mwskz>).

          "*---

          ADD 1 TO ld_numbering.

          CLEAR lwa_accounttax.
          lwa_accounttax-itemno_acc = ld_numbering.
          lwa_accounttax-tax_code = <lfs_lineitem_mwskz>-mwskz.

          SELECT SINGLE * FROM a003
            INTO @DATA(lwa_a003)
              WHERE aland EQ 'ID' AND
                    mwskz EQ @<lfs_lineitem_mwskz>-mwskz.
          IF sy-subrc EQ 0.

            lwa_accounttax-cond_key = lwa_a003-kschl.

            SELECT SINGLE * FROM t683s
              INTO @DATA(lwa_t683s)
                WHERE kalsm EQ 'TAXID' AND
                      kappl EQ @lwa_a003-kappl AND
                      kschl EQ @lwa_a003-kschl.
            IF sy-subrc EQ 0.
              lwa_accounttax-acct_key = lwa_t683s-kvsl1.
            ENDIF.

          ENDIF.

          lwa_accounttax-acct_key = p_git_lineitem-kvsl1.
          APPEND lwa_accounttax TO p_lit_accounttax.

          "*---

          CLEAR lwa_currencyamount.
          lwa_currencyamount-itemno_acc = ld_numbering.
          lwa_currencyamount-currency = p_gwa_header-waers.

          IF p_gwa_header-waers NE 'IDR'.
            lwa_currencyamount-exch_rate = p_gwa_header-kursf.
          ENDIF.

          SELECT SINGLE * FROM zficd_zfi05e0029_bset INTO @DATA(lwa_bset)
            WHERE bukrs EQ @p_gwa_header-bukrs AND
                  gjahr EQ @p_gwa_header-gjahr AND
                  belnr EQ @p_gwa_header-belnr AND
                  mwskz EQ @<lfs_lineitem_mwskz>-mwskz.
          IF sy-subrc EQ 0.
            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              lwa_bset-fwbas
                                     CHANGING lwa_currencyamount-amt_doccur.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              lwa_bset-fwste
                                     CHANGING lwa_currencyamount-amt_base.
          ENDIF.

          APPEND lwa_currencyamount TO p_lit_currencyamount.

          "*---

          <lfs_lineitem_mwskz>-is_used = 'X'.

        ENDLOOP.

      ENDIF.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_COLLECT_DATA_POSTING_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GWA_HEADER
*&      --> GIT_LINEITEM[]
*&      <-- LIT_ACCOUNTGL[]
*&      <-- LIT_ACCOUNTRECEIVABLE[]
*&      <-- LIT_ACCOUNTPAYABLE[]
*&      <-- LIT_CURRENCYAMOUNT[]
*&      <-- LIT_EXTENSION2[]
*&---------------------------------------------------------------------*
FORM f_collect_data_posting_b    TABLES p_git_lineitem STRUCTURE zfist00194
                                USING p_kind
                                      p_flag  "RE | NOT_RE | PPH26
                                      p_gwa_header TYPE zfist00193
                             CHANGING p_lwa_docheader TYPE bapiache09
                                      p_lit_accountgl TYPE bapiacgl09_tab
                                      p_lit_accountreceivable TYPE bapiacar09_tab
                                      p_lit_accountpayable TYPE bapiacap09_tab
                                      p_lit_accounttax TYPE bapiactx09_tab
                                      p_lit_currencyamount TYPE bapiaccr09_tab
                                      p_lit_extension2 TYPE bapiparex_tab
                                      p_lit_accountwt TYPE bapiacwt09_tab.

  TYPES: BEGIN OF lty_lineitem_mwskz.
      INCLUDE TYPE zfist00194.
  TYPES: is_used TYPE boolean.
  TYPES: END OF lty_lineitem_mwskz.

  RANGES: lra_mwskz FOR bseg-mwskz.

  "Declaration BAPI_ACC_DOCUMENT_POST
  DATA: lwa_accountgl      LIKE LINE OF p_lit_accountgl,
        lwa_accountpayable LIKE LINE OF p_lit_accountpayable,
        lwa_accounttax     LIKE LINE OF p_lit_accounttax,
        lwa_currencyamount LIKE LINE OF p_lit_currencyamount,
        lwa_accountwt      LIKE LINE OF p_lit_accountwt,
        lwa_extension2     TYPE bapiparex.

  DATA: lit_lineitem                TYPE TABLE OF zfist00194,
        lit_lineitem_mwskz          TYPE TABLE OF lty_lineitem_mwskz,
        ld_numbering                TYPE posnr_acc,
        ld_amount                   TYPE zfist00194-amount,
        ld_sum_amount               TYPE zfist00194-amount,
        ld_sum_amount_acctax_doccur TYPE zfist00194-amount,
        ld_sum_amount_acctax_base   TYPE zfist00194-amount.

*--------------------------------------------------------------------*

  CLEAR lra_mwskz[].
  f_fill_range: lra_mwskz 'I' 'EQ' '' ''.
  f_fill_range: lra_mwskz 'I' 'EQ' '**' ''.

  "---

  lit_lineitem_mwskz[] = CORRESPONDING #( p_git_lineitem[] ).
  DELETE lit_lineitem_mwskz WHERE mwskz IN lra_mwskz.
  SORT lit_lineitem_mwskz ASCENDING BY mwskz.
  DELETE ADJACENT DUPLICATES FROM lit_lineitem_mwskz COMPARING mwskz.

  "*--------------------------------------------------------------------*

  CLEAR lit_lineitem[].
  lit_lineitem[] = p_git_lineitem[].

  "*--------------------------------------------------------------------*

  CLEAR ld_numbering.

  "Document Header
  p_lwa_docheader-bus_act = 'RFBU'.
  p_lwa_docheader-username = sy-uname.
  p_lwa_docheader-header_txt = gc_bktxt.
  p_lwa_docheader-comp_code = p_gwa_header-bukrs.
  p_lwa_docheader-doc_date = p_gwa_header-bldat.
  p_lwa_docheader-pstng_date = p_gwa_header-budat.
  p_lwa_docheader-doc_type = p_gwa_header-blart.
  p_lwa_docheader-ref_doc_no = p_gwa_header-bktxt.

  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Line Item

  LOOP AT p_git_lineitem WHERE kind EQ p_kind.

    CLEAR: ld_amount, ld_sum_amount, ld_sum_amount_acctax_doccur, ld_sum_amount_acctax_base.
    CASE p_kind.
      WHEN gc_fl_posting_reverse OR
           gc_fl_posting_reverse_26.

        CASE p_flag.
          WHEN 'RE' OR 'NOT_RE'.

            "*--------------------------------------------------------------------*
            "*------------------REVERSE RE & REVERSE NOT RE-----------------------*
            "*--------------------------------------------------------------------*

            CASE p_git_lineitem-koart.
              WHEN 'S'.

                CHECK p_git_lineitem-ktosl NE 'WIT'.

                "*--------------------------------------------------------------------*
                "Line Item GL

                ADD 1 TO ld_numbering.

                CLEAR lwa_accountgl.
                lwa_accountgl-itemno_acc = ld_numbering.
                lwa_accountgl-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
                lwa_accountgl-item_text = p_git_lineitem-sgtxt.
                lwa_accountgl-costcenter =  |{ p_git_lineitem-kostl ALPHA = IN }|.
                lwa_accountgl-material = p_git_lineitem-matnr.
                lwa_accountgl-alloc_nmbr = p_git_lineitem-zuonr.
                lwa_accountgl-tax_code = p_git_lineitem-mwskz.
                APPEND lwa_accountgl TO p_lit_accountgl.

                "*--------------------------------------------------------------------*
                "Line Currency GL Amount

                CLEAR lwa_currencyamount.
                lwa_currencyamount-itemno_acc = ld_numbering.
                lwa_currencyamount-currency = p_gwa_header-waers.

                IF p_gwa_header-waers NE 'IDR'.
                  lwa_currencyamount-exch_rate = p_gwa_header-kursf.
                ENDIF.

                PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                  p_gwa_header-waers
                                                  p_git_lineitem-amount
                                         CHANGING lwa_currencyamount-amt_doccur.
                APPEND lwa_currencyamount TO p_lit_currencyamount.

                "*--------------------------------------------------------------------*

              WHEN 'K'.

                "*--------------------------------------------------------------------*
                "Line Item AP

                ADD 1 TO ld_numbering.

                CLEAR lwa_accountpayable.
                lwa_accountpayable-itemno_acc = ld_numbering.
                lwa_accountpayable-vendor_no = |{ p_git_lineitem-lifnr ALPHA = IN }|.
                lwa_accountpayable-item_text = p_git_lineitem-sgtxt.
                lwa_accountpayable-alloc_nmbr = p_git_lineitem-zuonr.
                lwa_accountpayable-tax_code = p_git_lineitem-mwskz.
                lwa_accountpayable-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
                APPEND lwa_accountpayable TO p_lit_accountpayable.

                "*--------------------------------------------------------------------*
                "Line Currency AP Amount

                CLEAR lwa_currencyamount.
                lwa_currencyamount-itemno_acc = ld_numbering.
                lwa_currencyamount-currency = p_gwa_header-waers.

                IF p_gwa_header-waers NE 'IDR'.
                  lwa_currencyamount-exch_rate = p_gwa_header-kursf.
                ENDIF.

                CLEAR ld_amount.
                SELECT SUM( amount ) AS amount
                  FROM @lit_lineitem AS a
                  WHERE kind EQ @p_kind AND
                        ktosl EQ 'WIT'
                    INTO @ld_amount
                  ##db_feature_mode[itabs_in_from_clause]
                  ##itab_key_in_select
                  ##ITAB_DB_SELECT.

                CLEAR ld_sum_amount.
                ADD ld_amount TO ld_sum_amount.
                ADD p_git_lineitem-amount TO ld_sum_amount.

                PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                  p_gwa_header-waers
                                                  ld_sum_amount
                                         CHANGING lwa_currencyamount-amt_doccur.
                APPEND lwa_currencyamount TO p_lit_currencyamount.

                "*--------------------------------------------------------------------*
                "Line Account WT AP

*****                CLEAR lwa_accountwt.
*****                lwa_accountwt-itemno_acc = ld_numbering.
*****
******                lwa_accountwt-wt_type = p_git_lineitem-witht.
******                lwa_accountwt-wt_code = p_git_lineitem-withcd.
*****
******                PERFORM f_convert_amount    USING 'TO_EXTERNAL'
******                                                  p_gwa_header-waers
******                                                  p_git_lineitem-qsshb
******                                         CHANGING lwa_accountwt-bas_amt_tc.
******
******                PERFORM f_convert_amount    USING 'TO_EXTERNAL'
******                                                  p_gwa_header-waers
******                                                  p_git_lineitem-qbshb
******                                         CHANGING lwa_accountwt-man_amt_tc.
*****
*****                SELECT SINGLE *
*****                  FROM @lit_lineitem AS a
*****                  WHERE kind EQ @p_kind AND
*****                        witht NE '' AND
*****                        withcd NE ''
*****                    INTO @DATA(lwa_lineitem)
*****                  ##db_feature_mode[itabs_in_from_clause]
*****                  ##itab_key_in_select
*****                  ##ITAB_DB_SELECT.
*****                IF sy-subrc EQ 0.
*****
*****                  lwa_accountwt-wt_type = lwa_lineitem-witht.
*****                  lwa_accountwt-wt_code = lwa_lineitem-withcd.
*****
*****                  PERFORM f_convert_amount    USING 'TO_EXTERNAL'
*****                                                    p_gwa_header-waers
*****                                                    lwa_lineitem-qsshb
*****                                           CHANGING lwa_accountwt-bas_amt_tc.
*****                  lwa_accountwt-bas_amt_tc = abs( lwa_accountwt-bas_amt_tc ).
*****
*****                  PERFORM f_convert_amount    USING 'TO_EXTERNAL'
*****                                                    p_gwa_header-waers
*****                                                    lwa_lineitem-qbshb
*****                                           CHANGING lwa_accountwt-man_amt_tc.
*****                  lwa_accountwt-man_amt_tc = abs( lwa_accountwt-man_amt_tc ).
*****
*****                ENDIF.
*****
*****                APPEND lwa_accountwt TO p_lit_accountwt.

                IF git_withitem[] IS NOT INITIAL.

                  LOOP AT git_withitem INTO DATA(lwa_withitem).

                    CLEAR lwa_accountwt.
                    lwa_accountwt-itemno_acc = ld_numbering.

                    lwa_accountwt-wt_type = lwa_withitem-witht.
                    lwa_accountwt-wt_code = lwa_withitem-wt_withcd.

                    PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                      p_gwa_header-waers
                                                      lwa_withitem-wt_qsshb
                                             CHANGING lwa_accountwt-bas_amt_tc.
                    lwa_accountwt-bas_amt_tc = abs( lwa_accountwt-bas_amt_tc ).

                    PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                      p_gwa_header-waers
                                                      lwa_withitem-wt_qbshb
                                             CHANGING lwa_accountwt-man_amt_tc.
                    lwa_accountwt-man_amt_tc = abs( lwa_accountwt-man_amt_tc ).


                    APPEND lwa_accountwt TO p_lit_accountwt.

                  ENDLOOP.

                ENDIF.

            ENDCASE.

            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*

          WHEN 'PPH26'.

            "*--------------------------------------------------------------------*
            "*----------------------------REVERSE PPH26---------------------------*
            "*--------------------------------------------------------------------*

            CASE p_git_lineitem-koart.
              WHEN 'S'.

                "*--------------------------------------------------------------------*
                "Line Item GL

                ADD 1 TO ld_numbering.

                CLEAR lwa_accountgl.
                lwa_accountgl-itemno_acc = ld_numbering.
                lwa_accountgl-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
                lwa_accountgl-item_text = p_git_lineitem-sgtxt.
                lwa_accountgl-costcenter =  |{ p_git_lineitem-kostl ALPHA = IN }|.
                lwa_accountgl-material = p_git_lineitem-matnr.
                lwa_accountgl-alloc_nmbr = p_git_lineitem-zuonr.
                lwa_accountgl-tax_code = p_git_lineitem-mwskz.
                APPEND lwa_accountgl TO p_lit_accountgl.

                "*--------------------------------------------------------------------*
                "Line Currency GL Amount

                CLEAR lwa_currencyamount.
                lwa_currencyamount-itemno_acc = ld_numbering.
                lwa_currencyamount-currency = p_gwa_header-waers.

                IF p_gwa_header-waers NE 'IDR'.
                  lwa_currencyamount-exch_rate = p_gwa_header-kursf.
                ENDIF.

                PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                  p_gwa_header-waers
                                                  p_git_lineitem-amount
                                         CHANGING lwa_currencyamount-amt_doccur.
                APPEND lwa_currencyamount TO p_lit_currencyamount.

                "*--------------------------------------------------------------------*

              WHEN 'K'.

                "*--------------------------------------------------------------------*
                "Line Item AP

                ADD 1 TO ld_numbering.

                CLEAR lwa_accountpayable.
                lwa_accountpayable-itemno_acc = ld_numbering.
                lwa_accountpayable-vendor_no = |{ p_git_lineitem-lifnr ALPHA = IN }|.
                lwa_accountpayable-item_text = p_git_lineitem-sgtxt.
                lwa_accountpayable-alloc_nmbr = p_git_lineitem-zuonr.
                lwa_accountpayable-tax_code = p_git_lineitem-mwskz.
                lwa_accountpayable-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
                APPEND lwa_accountpayable TO p_lit_accountpayable.

                "*--------------------------------------------------------------------*
                "Line Currency AP Amount

                CLEAR lwa_currencyamount.
                lwa_currencyamount-itemno_acc = ld_numbering.
                lwa_currencyamount-currency = p_gwa_header-waers.

                IF p_gwa_header-waers NE 'IDR'.
                  lwa_currencyamount-exch_rate = p_gwa_header-kursf.
                ENDIF.

                PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                  p_gwa_header-waers
                                                  p_git_lineitem-amount
                                         CHANGING lwa_currencyamount-amt_doccur.
                APPEND lwa_currencyamount TO p_lit_currencyamount.

                CLEAR ld_amount.
                ld_amount = abs( p_git_lineitem-amount ).
                ADD ld_amount TO ld_sum_amount.

            ENDCASE.

            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*

        ENDCASE.

      WHEN gc_fl_posting.

        "*--------------------------------------------------------------------*
        "*-------------------POSTING RE & POSTING NOT RE----------------------*
        "*--------------------------------------------------------------------*

        CASE p_git_lineitem-koart.
          WHEN 'S'.

            "*--------------------------------------------------------------------*
            "Line Item GL

            ADD 1 TO ld_numbering.

            CLEAR lwa_accountgl.
            lwa_accountgl-itemno_acc = ld_numbering.
            lwa_accountgl-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
            lwa_accountgl-item_text = p_git_lineitem-sgtxt.
            lwa_accountgl-costcenter =  |{ p_git_lineitem-kostl ALPHA = IN }|.
            lwa_accountgl-material = p_git_lineitem-matnr.
            lwa_accountgl-alloc_nmbr = p_git_lineitem-zuonr.
            lwa_accountgl-tax_code = p_git_lineitem-mwskz.
            APPEND lwa_accountgl TO p_lit_accountgl.

            "*--------------------------------------------------------------------*
            "Line Currency GL Amount

            CLEAR lwa_currencyamount.
            lwa_currencyamount-itemno_acc = ld_numbering.
            lwa_currencyamount-currency = p_gwa_header-waers.

            IF p_gwa_header-waers NE 'IDR'.
              lwa_currencyamount-exch_rate = p_gwa_header-kursf.
            ENDIF.

            IF ld_sum_amount EQ 0.
              CLEAR ld_amount.
              ld_amount = ( p_git_lineitem-amount * p_git_lineitem-kbetr ).

              ld_sum_amount_acctax_doccur = ld_amount.
              ld_sum_amount_acctax_base = p_git_lineitem-amount.

              CLEAR ld_sum_amount.
              ld_sum_amount = p_git_lineitem-amount + ld_amount.
            ENDIF.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              ld_sum_amount
                                     CHANGING lwa_currencyamount-amt_doccur.
            APPEND lwa_currencyamount TO p_lit_currencyamount.

            "*--------------------------------------------------------------------*

          WHEN 'K'.

            "*--------------------------------------------------------------------*
            "Line Item AP

            ADD 1 TO ld_numbering.

            CLEAR lwa_accountpayable.
            lwa_accountpayable-itemno_acc = ld_numbering.
            lwa_accountpayable-vendor_no = |{ p_git_lineitem-lifnr ALPHA = IN }|.
            lwa_accountpayable-item_text = p_git_lineitem-sgtxt.
            lwa_accountpayable-alloc_nmbr = p_git_lineitem-zuonr.
            lwa_accountpayable-tax_code = p_git_lineitem-mwskz.
            lwa_accountpayable-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
            APPEND lwa_accountpayable TO p_lit_accountpayable.

            "*--------------------------------------------------------------------*
            "Line Currency AP Amount

            CLEAR lwa_currencyamount.
            lwa_currencyamount-itemno_acc = ld_numbering.
            lwa_currencyamount-currency = p_gwa_header-waers.

            IF p_gwa_header-waers NE 'IDR'.
              lwa_currencyamount-exch_rate = p_gwa_header-kursf.
            ENDIF.

            IF ld_sum_amount EQ 0.
              CLEAR ld_amount.
              ld_amount = ( p_git_lineitem-amount * p_git_lineitem-kbetr ).

              ld_sum_amount_acctax_doccur = ld_amount.
              ld_sum_amount_acctax_base = p_git_lineitem-amount.

              CLEAR ld_sum_amount.
              ld_sum_amount = p_git_lineitem-amount + ld_amount.


            ENDIF.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              ld_sum_amount
                                     CHANGING lwa_currencyamount-amt_doccur.
            APPEND lwa_currencyamount TO p_lit_currencyamount.

            CLEAR ld_amount.
            ld_amount = abs( p_git_lineitem-amount ).
            ADD ld_amount TO ld_sum_amount.

            "*--------------------------------------------------------------------*
            "Line Account WT AP

            CLEAR lwa_accountwt.
            lwa_accountwt-itemno_acc = ld_numbering.
            lwa_accountwt-wt_type = p_git_lineitem-witht.
            lwa_accountwt-wt_code = p_git_lineitem-withcd.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              p_git_lineitem-amount
                                     CHANGING lwa_accountwt-bas_amt_tc.

            APPEND lwa_accountwt TO p_lit_accountwt.

            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*

        ENDCASE.

      WHEN gc_fl_posting_1.

        "*--------------------------------------------------------------------*
        "*-------------------------POSTING_1 PPH26----------------------------*
        "*--------------------------------------------------------------------*

        CASE p_git_lineitem-koart.
          WHEN 'S'.

            CHECK p_git_lineitem-ktosl NE 'WIT'.

            "*--------------------------------------------------------------------*
            "Line Item GL

            ADD 1 TO ld_numbering.

            CLEAR lwa_accountgl.
            lwa_accountgl-itemno_acc = ld_numbering.
            lwa_accountgl-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
            lwa_accountgl-item_text = p_git_lineitem-sgtxt.
            lwa_accountgl-costcenter =  |{ p_git_lineitem-kostl ALPHA = IN }|.
            lwa_accountgl-material = p_git_lineitem-matnr.
            lwa_accountgl-alloc_nmbr = p_git_lineitem-zuonr.
*            lwa_accountgl-tax_code = 'V0'.
            APPEND lwa_accountgl TO p_lit_accountgl.

            "*--------------------------------------------------------------------*
            "Line Currency GL Amount

            CLEAR lwa_currencyamount.
            lwa_currencyamount-itemno_acc = ld_numbering.
            lwa_currencyamount-currency = p_gwa_header-waers.

            IF p_gwa_header-waers NE 'IDR'.
              lwa_currencyamount-exch_rate = p_gwa_header-kursf.
            ENDIF.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              p_git_lineitem-amount
                                     CHANGING lwa_currencyamount-amt_doccur.
            APPEND lwa_currencyamount TO p_lit_currencyamount.

            "*--------------------------------------------------------------------*

          WHEN 'K'.

            "*--------------------------------------------------------------------*
            "Line Item AP

            ADD 1 TO ld_numbering.

            CLEAR lwa_accountpayable.
            lwa_accountpayable-itemno_acc = ld_numbering.
            lwa_accountpayable-vendor_no = |{ p_git_lineitem-lifnr ALPHA = IN }|.
            lwa_accountpayable-item_text = p_git_lineitem-sgtxt.
            lwa_accountpayable-alloc_nmbr = p_git_lineitem-zuonr.
            lwa_accountpayable-tax_code = p_git_lineitem-mwskz.
            lwa_accountpayable-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
            APPEND lwa_accountpayable TO p_lit_accountpayable.

            "*--------------------------------------------------------------------*
            "Line Currency AP Amount

            CLEAR lwa_currencyamount.
            lwa_currencyamount-itemno_acc = ld_numbering.
            lwa_currencyamount-currency = p_gwa_header-waers.

            IF p_gwa_header-waers NE 'IDR'.
              lwa_currencyamount-exch_rate = p_gwa_header-kursf.
            ENDIF.

            CLEAR ld_amount.
            SELECT SUM( amount ) AS amount
              FROM @lit_lineitem AS a
              WHERE kind EQ @p_kind AND
                    ktosl EQ 'WIT'
                INTO @ld_amount
              ##db_feature_mode[itabs_in_from_clause]
              ##itab_key_in_select
              ##ITAB_DB_SELECT.

            CLEAR ld_sum_amount.
            ADD ld_amount TO ld_sum_amount.
            ADD p_git_lineitem-amount TO ld_sum_amount.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
*                                              p_git_lineitem-amount
                                              ld_sum_amount
                                     CHANGING lwa_currencyamount-amt_doccur.
            APPEND lwa_currencyamount TO p_lit_currencyamount.

*            CLEAR ld_amount.
*            ld_amount = abs( p_git_lineitem-amount ).
*            ADD ld_amount TO ld_sum_amount.

            "*--------------------------------------------------------------------*
            "Line Account WT AP

*****            READ TABLE p_git_lineitem INDEX 4.
*****            IF sy-subrc EQ 0.
*****
*****              CLEAR lwa_accountwt.
*****              lwa_accountwt-itemno_acc = ld_numbering.
*****              lwa_accountwt-wt_type = p_git_lineitem-witht.
*****              lwa_accountwt-wt_code = p_git_lineitem-withcd.
*****
*****              CLEAR p_git_lineitem-amount.
*****              ld_amount = p_git_lineitem-amount.
*****              ADD ld_amount TO ld_sum_amount.
*****
*****              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
*****                                                p_gwa_header-waers
*****                                                ld_sum_amount
*****                                       CHANGING lwa_accountwt-bas_amt_tc.
*****
*****              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
*****                                                p_gwa_header-waers
*****                                                p_git_lineitem-amount
*****                                       CHANGING lwa_accountwt-man_amt_tc.
*****
*****              APPEND lwa_accountwt TO p_lit_accountwt.
*****
*****            ENDIF.

            SELECT SINGLE *
              FROM @lit_lineitem AS a
              WHERE kind EQ @p_kind AND
                    witht NE '' AND
                    withcd NE ''
                INTO @DATA(lwa_lineitem)
              ##db_feature_mode[itabs_in_from_clause]
              ##itab_key_in_select
              ##ITAB_DB_SELECT.
            IF sy-subrc EQ 0.

              CLEAR lwa_accountwt.
              lwa_accountwt-itemno_acc = ld_numbering.

              lwa_accountwt-wt_type = lwa_lineitem-witht.
              lwa_accountwt-wt_code = lwa_lineitem-withcd.

              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                p_gwa_header-waers
                                                ld_sum_amount
                                       CHANGING lwa_accountwt-bas_amt_tc.
              lwa_accountwt-bas_amt_tc = abs( lwa_accountwt-bas_amt_tc ).

              PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                                p_gwa_header-waers
                                                ld_amount
                                       CHANGING lwa_accountwt-man_amt_tc.
              lwa_accountwt-man_amt_tc = abs( lwa_accountwt-man_amt_tc ).
              APPEND lwa_accountwt TO p_lit_accountwt.

            ENDIF.

            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*

        ENDCASE.

      WHEN gc_fl_posting_2.

        "*--------------------------------------------------------------------*
        "*-------------------------POSTING_2 PPH26----------------------------*
        "*--------------------------------------------------------------------*

        CASE p_git_lineitem-koart.
          WHEN 'S'.

            "*--------------------------------------------------------------------*
            "Line Item GL

            ADD 1 TO ld_numbering.

            CLEAR lwa_accountgl.
            lwa_accountgl-itemno_acc = ld_numbering.
            lwa_accountgl-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
            lwa_accountgl-item_text = p_git_lineitem-sgtxt.
            lwa_accountgl-costcenter =  |{ p_git_lineitem-kostl ALPHA = IN }|.
            lwa_accountgl-material = p_git_lineitem-matnr.
            lwa_accountgl-alloc_nmbr = p_git_lineitem-zuonr.
            lwa_accountgl-tax_code = p_git_lineitem-mwskz.
            lwa_accountgl-profit_ctr = p_git_lineitem-prctr.
            APPEND lwa_accountgl TO p_lit_accountgl.

            "*--------------------------------------------------------------------*
            "Line Currency GL Amount

            CLEAR lwa_currencyamount.
            lwa_currencyamount-itemno_acc = ld_numbering.
            lwa_currencyamount-currency = p_gwa_header-waers.

            IF p_gwa_header-waers NE 'IDR'.
              lwa_currencyamount-exch_rate = p_gwa_header-kursf.
            ENDIF.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              p_git_lineitem-amount
                                     CHANGING lwa_currencyamount-amt_doccur.
            APPEND lwa_currencyamount TO p_lit_currencyamount.

            "*--------------------------------------------------------------------*

          WHEN 'K'.

            "*--------------------------------------------------------------------*
            "Line Item AP

            ADD 1 TO ld_numbering.

            CLEAR lwa_accountpayable.
            lwa_accountpayable-itemno_acc = ld_numbering.
            lwa_accountpayable-vendor_no = |{ p_git_lineitem-lifnr ALPHA = IN }|.
            lwa_accountpayable-item_text = p_git_lineitem-sgtxt.
            lwa_accountpayable-alloc_nmbr = p_git_lineitem-zuonr.
            lwa_accountpayable-tax_code = p_git_lineitem-mwskz.
            lwa_accountpayable-gl_account = |{ p_git_lineitem-racct ALPHA = IN }|.
            APPEND lwa_accountpayable TO p_lit_accountpayable.

            "*--------------------------------------------------------------------*
            "Line Currency AP Amount

            CLEAR lwa_currencyamount.
            lwa_currencyamount-itemno_acc = ld_numbering.
            lwa_currencyamount-currency = p_gwa_header-waers.

            IF p_gwa_header-waers NE 'IDR'.
              lwa_currencyamount-exch_rate = p_gwa_header-kursf.
            ENDIF.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              p_git_lineitem-amount
                                     CHANGING lwa_currencyamount-amt_doccur.
            APPEND lwa_currencyamount TO p_lit_currencyamount.

            CLEAR ld_amount.
            ld_amount = abs( p_git_lineitem-amount ).
            ADD ld_amount TO ld_sum_amount.

            "*--------------------------------------------------------------------*
            "Line Account WT AP

            CLEAR lwa_accountwt.
            lwa_accountwt-itemno_acc = ld_numbering.
            lwa_accountwt-wt_type = p_git_lineitem-witht.
            lwa_accountwt-wt_code = p_git_lineitem-withcd.

            APPEND lwa_accountwt TO p_lit_accountwt.

            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*
            "*--------------------------------------------------------------------*

        ENDCASE.

    ENDCASE.

  ENDLOOP.

  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Line Account Tax

  CASE p_kind.
    WHEN gc_fl_posting_reverse.

      IF lit_lineitem_mwskz[] IS NOT INITIAL.

        LOOP AT lit_lineitem_mwskz ASSIGNING FIELD-SYMBOL(<lfs_lineitem_mwskz>).

          "*---

          ADD 1 TO ld_numbering.

          CLEAR lwa_accounttax.
          lwa_accounttax-itemno_acc = ld_numbering.
          lwa_accounttax-tax_code = <lfs_lineitem_mwskz>-mwskz.

          SELECT SINGLE * FROM a003
            INTO @DATA(lwa_a003)
              WHERE aland EQ 'ID' AND
                    mwskz EQ @<lfs_lineitem_mwskz>-mwskz.
          IF sy-subrc EQ 0.

            lwa_accounttax-cond_key = lwa_a003-kschl.

            SELECT SINGLE * FROM t683s
              INTO @DATA(lwa_t683s)
                WHERE kalsm EQ 'TAXID' AND
                      kappl EQ @lwa_a003-kappl AND
                      kschl EQ @lwa_a003-kschl.
            IF sy-subrc EQ 0.
              lwa_accounttax-acct_key = lwa_t683s-kvsl1.
            ENDIF.

          ENDIF.

          APPEND lwa_accounttax TO p_lit_accounttax.

          "*---

          CLEAR lwa_currencyamount.
          lwa_currencyamount-itemno_acc = ld_numbering.
          lwa_currencyamount-currency = p_gwa_header-waers.

          IF p_gwa_header-waers NE 'IDR'.
            lwa_currencyamount-exch_rate = p_gwa_header-kursf.
          ENDIF.

          SELECT SINGLE * FROM zficd_zfi05e0029_bset INTO @DATA(lwa_bset)
            WHERE bukrs EQ @p_gwa_header-bukrs AND
                  gjahr EQ @p_gwa_header-gjahr AND
                  belnr EQ @p_gwa_header-belnr AND
                  mwskz EQ @<lfs_lineitem_mwskz>-mwskz.
          IF sy-subrc EQ 0.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              lwa_bset-fwste
                                     CHANGING lwa_currencyamount-amt_doccur.
            lwa_currencyamount-amt_doccur = lwa_currencyamount-amt_doccur * -1.

            PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                              p_gwa_header-waers
                                              lwa_bset-fwbas
                                     CHANGING lwa_currencyamount-amt_base.
            lwa_currencyamount-amt_base = lwa_currencyamount-amt_base * -1.

          ENDIF.

          APPEND lwa_currencyamount TO p_lit_currencyamount.

          "*---

          <lfs_lineitem_mwskz>-is_used = 'X'.

        ENDLOOP.

      ENDIF.

    WHEN gc_fl_posting.

      IF lit_lineitem_mwskz[] IS NOT INITIAL.

        LOOP AT lit_lineitem_mwskz ASSIGNING <lfs_lineitem_mwskz>.

          "*---

          ADD 1 TO ld_numbering.

          CLEAR lwa_accounttax.
          lwa_accounttax-itemno_acc = ld_numbering.
          lwa_accounttax-tax_code = <lfs_lineitem_mwskz>-mwskz.

          SELECT SINGLE * FROM a003
            INTO @lwa_a003
              WHERE aland EQ 'ID' AND
                    mwskz EQ @<lfs_lineitem_mwskz>-mwskz.
          IF sy-subrc EQ 0.

            lwa_accounttax-cond_key = lwa_a003-kschl.

            SELECT SINGLE * FROM t683s
              INTO @lwa_t683s
                WHERE kalsm EQ 'TAXID' AND
                      kappl EQ @lwa_a003-kappl AND
                      kschl EQ @lwa_a003-kschl.
            IF sy-subrc EQ 0.
              lwa_accounttax-acct_key = lwa_t683s-kvsl1.
            ENDIF.

          ENDIF.

          APPEND lwa_accounttax TO p_lit_accounttax.

          "*---

          CLEAR lwa_currencyamount.
          lwa_currencyamount-itemno_acc = ld_numbering.
          lwa_currencyamount-currency = p_gwa_header-waers.

          IF p_gwa_header-waers NE 'IDR'.
            lwa_currencyamount-exch_rate = p_gwa_header-kursf.
          ENDIF.

          PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                            p_gwa_header-waers
                                            ld_sum_amount_acctax_doccur
                                   CHANGING lwa_currencyamount-amt_doccur.

          PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                            p_gwa_header-waers
                                            ld_sum_amount_acctax_base
                                   CHANGING lwa_currencyamount-amt_base.

          APPEND lwa_currencyamount TO p_lit_currencyamount.

          "*---

          <lfs_lineitem_mwskz>-is_used = 'X'.

        ENDLOOP.

      ENDIF.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CALL_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PU_PROC
*&      --> LIT_BLNTAB[]
*&      --> LIT_FTCLEAR[]
*&      --> LIT_FTPOST[]
*&      --> LIT_FTTAX[]
*&      <-- LWA_RETURN
*&---------------------------------------------------------------------*
FORM f_call_post  USING    p_fl_check "X = Check | <BLANK> = No Check
                           p_flag
                           p_gwa_header TYPE zfist00193
                           p_lwa_docheader TYPE bapiache09
                           p_lit_accountgl TYPE bapiacgl09_tab
                           p_lit_accountpayable TYPE bapiacap09_tab
                           p_lit_accounttax TYPE bapiactx09_tab
                           p_lit_accountreceivable TYPE bapiacar09_tab
                           p_lit_currencyamount TYPE bapiaccr09_tab
                           p_lit_extension2 TYPE  bapiparex_tab
                           p_lit_accountwt TYPE bapiacwt09_tab
                  CHANGING p_subrc
                           p_type
                           p_key
                           p_sys
                           p_lit_return TYPE bapiret2_tty
                           p_lwa_lineitem TYPE zfist00194

                           p_git_message TYPE gtt_message.

  DATA: ld_check_success   TYPE boolean,
        ld_posting_success TYPE boolean,

        lit_return         TYPE TABLE OF bapiret2,
        lit_message        TYPE TABLE OF gty_message.

*--------------------------------------------------------------------*

  CLEAR: p_type,
         p_key,
         p_sys,
         p_lit_return[].

  "*--------------------------------------------------------------------*

  CLEAR lit_return[].
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader    = p_lwa_docheader
    TABLES
      accountgl         = p_lit_accountgl
      accountreceivable = p_lit_accountreceivable
      accountpayable    = p_lit_accountpayable
      accounttax        = p_lit_accounttax
      currencyamount    = p_lit_currencyamount
      return            = lit_return
      extension2        = p_lit_extension2
      accountwt         = p_lit_accountwt.

  APPEND LINES OF lit_return[] TO p_lit_return[].

  CLEAR lit_message[].
  lit_message[] = CORRESPONDING #( lit_return[] ).
  APPEND LINES OF lit_message[] TO p_git_message[].

  CLEAR ld_check_success.
  READ TABLE p_lit_return INTO DATA(lwa_return) WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    p_subrc = 1.
  ELSE.

    IF p_subrc EQ 1.
      "Do nothing
    ELSE.
      p_subrc = 0.
      ld_check_success = 'X'.
    ENDIF.

  ENDIF.

  "*--------------------------------------------------------------------*

  CHECK p_fl_check EQ ''.  "X = Check | <BLANK> = No Check

  "*--------------------------------------------------------------------*

  IF ld_check_success EQ 'X'.

    CLEAR lit_return[].
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = p_lwa_docheader
      IMPORTING
        obj_type          = p_type
        obj_key           = p_key
        obj_sys           = p_sys
      TABLES
        accountgl         = p_lit_accountgl
        accountreceivable = p_lit_accountreceivable
        accountpayable    = p_lit_accountpayable
        accounttax        = p_lit_accounttax
        currencyamount    = p_lit_currencyamount
        return            = lit_return
        extension2        = p_lit_extension2
        accountwt         = p_lit_accountwt.

    APPEND LINES OF lit_return[] TO p_lit_return[].

    CLEAR lit_message[].
    lit_message[] = CORRESPONDING #( lit_return[] ).
    APPEND LINES OF lit_message[] TO p_git_message[].

    CLEAR ld_posting_success.
    READ TABLE lit_return INTO lwa_return WITH KEY type = 'E'.
    IF sy-subrc NE 0.

      READ TABLE lit_return INTO lwa_return WITH KEY type = 'S'.
      IF sy-subrc EQ 0.
        ld_posting_success = 'X'.
      ENDIF.

    ENDIF.

    "*--------------------------------------------------------------------*

    CLEAR lwa_return.
    IF ld_posting_success EQ 'X'.

      p_lwa_lineitem-gjahr_posted = p_lwa_docheader-pstng_date(4).
      p_lwa_lineitem-belnr_posted = p_key+0(10).
      p_lwa_lineitem-flag_bapi = 'X'.  "X=Success | <BLANK>=Failed

    ENDIF.

  ENDIF.

  "*--------------------------------------------------------------------*

  "Insert log
  PERFORM f_insert_log USING p_flag  "REVERSE | POSTING
                             p_gwa_header
                             p_lit_return.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_BAPI_COMMIT_OR_CALLBACK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_bapi_commit_or_rollback    TABLES p_lit_lineitem STRUCTURE zfist00194
                                   USING p_flag
                                         p_gwa_header TYPE zfist00193
                                CHANGING p_lit_return TYPE bapiret2_tty.

  DATA: lwa_return         TYPE bapiret2.

*--------------------------------------------------------------------*

  CLEAR lwa_return.
  READ TABLE p_lit_lineitem WITH KEY flag_bapi = ''.
  IF sy-subrc EQ 0.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = lwa_return.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      IMPORTING
        return = lwa_return.

    gd_mode = 'D1'.

  ENDIF.

  IF lwa_return IS NOT INITIAL.
    APPEND lwa_return TO p_lit_return.

    "Insert log
    PERFORM f_insert_log USING p_flag
                               p_gwa_header
                               p_lit_return.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_INSERT_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_BAPIRET2
*&---------------------------------------------------------------------*
FORM f_insert_log  USING    p_kind_of_log
                            p_gwa_header TYPE zfist00193
                            p_lit_return TYPE gtt_return.

  DATA: "ld_gjahr       TYPE bseg-gjahr,
        "ld_belnr       TYPE bseg-belnr,
        lwa_zfidt00325 TYPE zfidt00325.

*--------------------------------------------------------------------*

  IF p_lit_return[] IS NOT INITIAL.

*    CLEAR: gd_subrc, ld_gjahr, ld_belnr.
*    PERFORM f_check_doc_h_text USING p_gwa_header-bktxt
*                               CHANGING gd_subrc
*                                        ld_gjahr
*                                        ld_belnr
*                                        p_gwa_header.

    "*--------------------------------------------------------------------*

    SELECT SINGLE MAX( seqno ) FROM zfidt00325 INTO @DATA(ld_seqno)
      WHERE bukrs EQ @p_gwa_header-bukrs AND
*            gjahr EQ @ld_gjahr AND
            gjahr EQ @p_gwa_header-gjahr AND
*            belnr EQ @ld_belnr AND
            belnr EQ @p_gwa_header-belnr AND
            bktxt EQ @p_gwa_header-bktxt AND
            kind EQ @p_kind_of_log.
    IF sy-subrc EQ 0.
      ADD 1 TO ld_seqno.
    ELSE.
      ld_seqno = 1.
    ENDIF.

    "*--------------------------------------------------------------------*

    LOOP AT p_lit_return INTO DATA(lwa_return).

      CLEAR lwa_zfidt00325.
      lwa_zfidt00325-bukrs = p_gwa_header-bukrs.
      lwa_zfidt00325-gjahr = p_gwa_header-gjahr.
      lwa_zfidt00325-belnr = p_gwa_header-belnr.
      lwa_zfidt00325-bktxt = p_gwa_header-bktxt.
      lwa_zfidt00325-kind = p_kind_of_log.
      lwa_zfidt00325-seqno = ld_seqno.

      CALL FUNCTION 'ZFIFM_GET_TIME_2'
        IMPORTING
          ex_date = lwa_zfidt00325-erdat
          ex_time = lwa_zfidt00325-erzeit.

      lwa_zfidt00325-msgid = lwa_return-id.
      lwa_zfidt00325-msgno = lwa_return-number.
      lwa_zfidt00325-type = lwa_return-type.
      lwa_zfidt00325-message = lwa_return-message.
      lwa_zfidt00325-ernam = sy-uname.
      MODIFY zfidt00325 FROM lwa_zfidt00325.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

      ADD 1 TO ld_seqno.

    ENDLOOP.

  ENDIF.

*--------------------------------------------------------------------*

  CLEAR ld_seqno.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_INSERT_ZFIDT00326
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM f_insert_zfidt00326    TABLES p_lit_lineitem STRUCTURE zfist00194
                                   p_git_lineitem STRUCTURE zfist00194
                             USING p_gwa_header TYPE zfist00193
                                    "ld_all_bapi_success
                          CHANGING p_lit_return TYPE bapiret2_tty.

  DATA: lwa_zfidt00326 TYPE zfidt00326,
        lit_zfidt00327 TYPE TABLE OF zfidt00327,
        lwa_zfidt00327 TYPE zfidt00327.

*--------------------------------------------------------------------*

  CLEAR lit_zfidt00327[].

  READ TABLE p_lit_lineitem WITH KEY flag_bapi = ''.  "X=Success | <BLANK>=Failed
  IF sy-subrc EQ 0.
    "Do nothing
  ELSE.

    LOOP AT p_lit_lineitem.

      CLEAR lwa_zfidt00326.
      lwa_zfidt00326-bukrs = p_gwa_header-bukrs.
      lwa_zfidt00326-gjahr = p_gwa_header-gjahr.
      lwa_zfidt00326-belnr = p_gwa_header-belnr.
      lwa_zfidt00326-bktxt = p_gwa_header-bktxt.
      lwa_zfidt00326-kind = p_lit_lineitem-kind.
      lwa_zfidt00326-blart = p_gwa_header-blart.
      lwa_zfidt00326-bldat = p_gwa_header-bldat.
      lwa_zfidt00326-budat = p_gwa_header-budat.
      lwa_zfidt00326-kursf = p_gwa_header-kursf.
      lwa_zfidt00326-waers = p_gwa_header-waers.
      lwa_zfidt00326-total_cr = p_gwa_header-total_cr.
      lwa_zfidt00326-total_db = p_gwa_header-total_db.
      lwa_zfidt00326-gjahr_posted = p_lit_lineitem-gjahr_posted.
      lwa_zfidt00326-belnr_posted = p_lit_lineitem-belnr_posted.
      CALL FUNCTION 'ZFIFM_GET_TIME_2'
        IMPORTING
          ex_date = lwa_zfidt00326-erdat
          ex_time = lwa_zfidt00326-erzeit.
      lwa_zfidt00326-ernam = sy-uname.

      MODIFY zfidt00326 FROM lwa_zfidt00326.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.

        "ZFIMSG 138: Insertion posted doc. & & & & to ZFIDT00326 successfully
        CLEAR gd_message.
        MESSAGE s138(zfimsg) WITH p_lit_lineitem-kind
                                  p_gwa_header-bukrs
                                  p_lit_lineitem-gjahr_posted
                                  p_lit_lineitem-belnr_posted
                                  INTO gd_message.

        "ZFIMSG 138: Insertion posted doc. & & & & to ZFIDT00326 successfully
        APPEND VALUE #( type = 'S'
                        id = 'ZFIMSG'
                        number = '138'
                        message = gd_message
                        message_v1 = p_lit_lineitem-kind
                        message_v2 = p_gwa_header-bukrs
                        message_v3 = p_lit_lineitem-gjahr_posted
                        message_v4 = p_lit_lineitem-belnr_posted
                      )
          TO p_lit_return.

      ELSE.

        "ZFIMSG 139: Insertion posted doc. & & & & to ZFIDT00326 failed
        CLEAR gd_message.
        MESSAGE s139(zfimsg) WITH p_lit_lineitem-kind
                                  p_gwa_header-bukrs
                                  p_lit_lineitem-gjahr_posted
                                  p_lit_lineitem-belnr_posted
                                  INTO gd_message.

        "ZFIMSG 139: Insertion posted doc. & & & & to ZFIDT00326 failed
        APPEND VALUE #( type = 'E'
                        id = 'ZFIMSG'
                        number = '139'
                        message = gd_message
                        message_v1 = p_lit_lineitem-kind
                        message_v2 = p_gwa_header-bukrs
                        message_v3 = p_lit_lineitem-gjahr_posted
                        message_v4 = p_lit_lineitem-belnr_posted
                      )
          TO p_lit_return.

      ENDIF.

      "*--------------------------------------------------------------------*

*      LOOP AT p_git_lineitem INTO DATA(lwa_lineitem)
      LOOP AT p_git_lineitem ASSIGNING FIELD-SYMBOL(<lfs_lineitem>)
        WHERE kind EQ p_lit_lineitem-kind.

        CLEAR lwa_zfidt00327.

        lwa_zfidt00327-bukrs = p_gwa_header-bukrs.
        lwa_zfidt00327-belnr = p_gwa_header-belnr.
        lwa_zfidt00327-gjahr = p_gwa_header-gjahr.

        lwa_zfidt00327-zno = <lfs_lineitem>-zno.
        lwa_zfidt00327-matnr = <lfs_lineitem>-matnr.
        lwa_zfidt00327-maktx = <lfs_lineitem>-maktx.
        lwa_zfidt00327-racct = <lfs_lineitem>-racct.
        lwa_zfidt00327-racct_desc = <lfs_lineitem>-racct_desc.
        lwa_zfidt00327-lifnr = <lfs_lineitem>-lifnr.
        lwa_zfidt00327-lifnr_name = <lfs_lineitem>-lifnr_name.
        lwa_zfidt00327-waers = p_gwa_header-waers.
        lwa_zfidt00327-amount = <lfs_lineitem>-amount.
        lwa_zfidt00327-mwskz = <lfs_lineitem>-mwskz.
        lwa_zfidt00327-witht = <lfs_lineitem>-witht.
        lwa_zfidt00327-withcd = <lfs_lineitem>-withcd.
        lwa_zfidt00327-qsshh = <lfs_lineitem>-qsshh.
        lwa_zfidt00327-kostl = <lfs_lineitem>-kostl.
        lwa_zfidt00327-sgtxt = <lfs_lineitem>-sgtxt.
        lwa_zfidt00327-kind = <lfs_lineitem>-kind.
        lwa_zfidt00327-koart = <lfs_lineitem>-koart.
        lwa_zfidt00327-zuonr = <lfs_lineitem>-zuonr.

        APPEND lwa_zfidt00327 TO lit_zfidt00327.

        "*--------------------------------------------------------------------*
        "Insert GJAHR_POSTED & BELNR_POSTED to Table Control Screen

        <lfs_lineitem>-gjahr_posted = p_lit_lineitem-gjahr_posted.
        <lfs_lineitem>-belnr_posted = p_lit_lineitem-belnr_posted.

      ENDLOOP.


    ENDLOOP.

    "*--------------------------------------------------------------------*

    IF lit_zfidt00327[] IS NOT INITIAL.
      MODIFY zfidt00327 FROM TABLE lit_zfidt00327.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.

        "ZFIMSG 142: Insertion posted doc. & & & & to ZFIDT00327 successfully
        CLEAR gd_message.
        MESSAGE s142(zfimsg) WITH p_gwa_header-bukrs
                                  p_gwa_header-gjahr
                                  p_gwa_header-belnr
                                  INTO gd_message.

        "ZFIMSG 142: Insertion posted doc. & & & & to ZFIDT00327 successfully
        APPEND VALUE #( type = 'S'
                        id = 'ZFIMSG'
                        number = '142'
                        message = gd_message
                        message_v1 = p_gwa_header-bukrs
                        message_v2 = p_gwa_header-gjahr
                        message_v3 = p_gwa_header-belnr
                        message_v4 = ''
                      )
          TO p_lit_return.

      ELSE.

        "ZFIMSG 143: Insertion posted doc. & & & & to ZFIDT00327 failed
        CLEAR gd_message.
        MESSAGE s139(zfimsg) WITH p_gwa_header-bukrs
                                  p_gwa_header-gjahr
                                  p_gwa_header-belnr
                                  INTO gd_message.

        "ZFIMSG 143: Insertion posted doc. & & & & to ZFIDT00327 failed
        APPEND VALUE #( type = 'E'
                        id = 'ZFIMSG'
                        number = '143'
                        message = gd_message
                        message_v1 = p_gwa_header-bukrs
                        message_v2 = p_gwa_header-gjahr
                        message_v3 = p_gwa_header-belnr
                        message_v4 = ''
                      )
          TO p_lit_return.

      ENDIF.
    ENDIF.

  ENDIF.

  "*--------------------------------------------------------------------*

  "Insert log
  PERFORM f_insert_log USING gc_fl_log  "LOG
                             p_gwa_header
                             p_lit_return.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_check    USING p_flag
                      p_gwa_header TYPE zfist00193
             CHANGING p_subrc
                      p_git_message TYPE gtt_message.

  DATA: ld_total_cr TYPE zfist00193-total_cr.

*--------------------------------------------------------------------*
  "Check validation

  PERFORM f_validation_b    USING p_gwa_header
                         CHANGING p_subrc
                                  p_git_message.

  "*--------------------------------------------------------------------*
  "Check line item exist or not

  IF git_lineitem[] IS INITIAL.

    "ZFIMSG 135: Line item must be filled
    APPEND VALUE #( type = 'E'
                    id = 'ZFIMSG'
                    number = 135
                    message_v1 = ''
                    message_v2 = ''
                    message_v3 = ''
                    message_v4 = ''
                  )
      TO p_git_message.

    p_subrc = 1.
  ENDIF.

  "*--------------------------------------------------------------------*
  "Check balance

  CHECK p_subrc EQ 0.

  CLEAR ld_total_cr.
  ld_total_cr = abs( gwa_header-total_cr ).
  IF gwa_header-total_db NE ld_total_cr.

    "ZFIMSG 136: Transaction is not balance
    APPEND VALUE #( type = 'E'
                    id = 'ZFIMSG'
                    number = 136
                    message_v1 = ''
                    message_v2 = ''
                    message_v3 = ''
                    message_v4 = ''
                  )
      TO p_git_message.

    p_subrc = 1.
  ENDIF.

  "*--------------------------------------------------------------------*
  "Check BAPI_ACC_DOCUMENT_CHECK

  CHECK p_subrc EQ 0.

  PERFORM f_posting    USING 'X' "X = Check | <BLANK> = No Check
                             p_flag
                             p_gwa_header
                    CHANGING p_subrc
                             p_git_message.

  "*--------------------------------------------------------------------*
  "Give message 'Ready to post'

  CHECK p_subrc EQ 0.

*  "ZFIMSG 137: Ready to post
*  APPEND VALUE #( type = 'S'
*                  id = 'ZFIMSG'
*                  number = 137
*                  message_v1 = ''
*                  message_v2 = ''
*                  message_v3 = ''
*                  message_v4 = ''
*                )
*    TO p_git_message.

  "ZFIMSG 134: Check: &
  APPEND VALUE #( type = 'S'
                  id = 'ZFIMSG'
                  number = 134
                  message_v1 = 'Ok. Ready to post'
                  message_v2 = ''
                  message_v3 = ''
                  message_v4 = ''
                )
    TO p_git_message.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_SET_MESSAGE_NUMBERING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_message_numbering CHANGING p_git_message TYPE gtt_message.

  "*--------------------------------------------------------------------*
  "Set message numbering

  LOOP AT p_git_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
    <lfs_message>-no = sy-tabix.
    <lfs_message>-lineno = sy-tabix.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_WITHT_N_WITHCD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_ZFIDT00322_BPARNTER
*&      --> LWA_ZFIDT00322_MATNR
*&      <-- LWA_LINEITEM_WITHT
*&      <-- LWA_LINEITEM_WITHCD
*&---------------------------------------------------------------------*
*FORM f_get_witht_withcd_whtbase  USING    p_bpartner
*                                          p_matnr
*                                 CHANGING p_witht
*                                          p_withcd
*                                          p_qsshh.

FORM f_get_witht_withcd_whtbase  USING    p_lifnr
                                          p_matnr
                                 CHANGING p_witht
                                          p_withcd.
*                                          p_qsshh.

  "Get MBEW (ZFIVT00122)
  "Relasi 1 MATNR punya 1 BKLAS walaupun BWKEY nya beda-beda
  SELECT SINGLE * FROM zficd_zfi05e0029_mbew INTO @DATA(lwa_mbew)
    WHERE matnr EQ @p_matnr.

  CHECK lwa_mbew-bklas IS NOT INITIAL.

  "Get ZMMDT00042 (ZFIVT00121)
  SELECT SINGLE * FROM zficd_zfi05e0029_zmmdt042 INTO @DATA(lwa_zmmdt00042)
    WHERE bpartner_b EQ @p_lifnr.

  CHECK lwa_zmmdt00042-fityp IS NOT INITIAL.

  "Get ZFIDT00005
  SELECT * FROM zfidt00005 INTO TABLE @DATA(lit_whtmap)
    WHERE zvtc = @lwa_zmmdt00042-fityp.
  IF lit_whtmap[] IS NOT INITIAL.
    SORT lit_whtmap BY zvtc bklas ztdf.
  ENDIF.

  CHECK lit_whtmap[] IS NOT INITIAL.

  "Get ZFIDT00002
  SELECT zvtc, ztrf FROM zfidt00002 INTO TABLE @DATA(lit_zfidt00002)
    WHERE zvtc EQ @lwa_zmmdt00042-fityp.
  IF lit_zfidt00002[] IS NOT INITIAL.
    SORT lit_zfidt00002 BY zvtc.
    DELETE ADJACENT DUPLICATES FROM lit_zfidt00002 COMPARING zvtc.
  ENDIF.

  CHECK lit_zfidt00002[] IS NOT INITIAL.

  "Get ZFIDT00013
  SELECT bklas, ztrf FROM zfidt00013 INTO TABLE @DATA(lit_zfidt00013)
    WHERE bklas EQ @lwa_mbew-bklas.
  IF lit_zfidt00013[] IS NOT INITIAL.
    SORT lit_zfidt00013 BY bklas.
    DELETE ADJACENT DUPLICATES FROM lit_zfidt00013 COMPARING bklas.
  ENDIF.

  CHECK lit_zfidt00013[] IS NOT INITIAL.

  PERFORM f_get_wht_type_n_code_b    USING lwa_zmmdt00042-fityp
                                           lwa_mbew-bklas
                                           lit_zfidt00002
                                           lit_zfidt00013
                                           lit_whtmap
                                  CHANGING p_witht
                                           p_withcd.

*  CHECK p_witht IS NOT INITIAL AND
*        p_withcd IS NOT INITIAL.
*
*  " Get detail of WHT data
*  SELECT SINGLE
*         bukrs,
*         belnr,
*         gjahr,
*         witht,
*         wt_withcd,
*         wt_qsshh,
*         wt_qbshh,
*         koart,
*         wt_acco
*    FROM with_item
*      INTO @DATA(lwa_withitem)
*          WHERE bukrs EQ @gwa_header-bukrs AND
*                belnr EQ @gwa_header-belnr AND
*                gjahr EQ @gwa_header-gjahr AND
*                witht EQ @p_witht AND
*                wt_withcd EQ @p_withcd.
*  IF sy-subrc EQ 0.
*    p_qsshh = lwa_withitem-wt_qsshh.
*  ENDIF.

ENDFORM.
