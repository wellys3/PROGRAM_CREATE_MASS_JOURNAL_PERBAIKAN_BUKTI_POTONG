*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_data CHANGING p_flag  "RE | NOT_RE | PPH26
                         p_gwa_header TYPE zfist00193.

*--------------------------------------------------------------------*

  CLEAR gd_subrc.
  PERFORM f_check_doc_h_text    USING p_gwa_header-bktxt
                             CHANGING gd_subrc
                                      p_gwa_header.

  CASE gd_subrc.
    WHEN 0.

      CASE p_gwa_header-blart.
        WHEN 'RE'.

          p_flag = 'RE'. "RE | NOT_RE | PPH26

          "Logic Get Data Reversal Invoice PO
          CLEAR gd_subrc.
          PERFORM f_get_data_reverse_po USING "ld_gjahr
                                              "ld_belnr
                                              p_gwa_header
                                     CHANGING gd_subrc.

          IF gd_subrc EQ 0.

            "Logic Process Data Reversal Invoice PO
            PERFORM f_process_data_reverse_po.

          ELSE.

            "ZFIMSG 133: Data with Doc. Header Text & not found
            MESSAGE s133(zfimsg) WITH p_gwa_header-bktxt DISPLAY LIKE 'E'.

            CLEAR gd_cursor_name.
            gd_cursor_name = 'GWA_HEADER-BKTXT'.

          ENDIF.

        WHEN OTHERS.

          "Define PPH26 or NOT_PPH26
          CLEAR gd_subrc.
          PERFORM f_define    USING p_gwa_header
                           CHANGING gd_subrc
                                    p_flag.  "RE | NOT_RE | PPH26

          IF gd_subrc EQ 0.

            CASE gd_flag.
              WHEN 'NOT_RE'.

                "Logic Get Data Reversal Invoice Non-PO
                PERFORM f_get_data_inv_non_po USING gd_flag
                                                    p_gwa_header.

                "Logic Process Data Reversal Invoice PO
                PERFORM f_process_data_reverse_non_po.

              WHEN 'PPH26'.

                "Logic Get Data Journal PPH26
                PERFORM f_get_data_inv_pph26 USING gd_flag
                                                    p_gwa_header.

                "Logic Process Data Journal PPH26
                PERFORM f_process_data_pph26 USING p_gwa_header.

            ENDCASE.

          ELSE.

            "ZFIMSG 133: Data with Doc. Header Text & not found
            MESSAGE s133(zfimsg) WITH p_gwa_header-bktxt DISPLAY LIKE 'E'.

            CLEAR gd_cursor_name.
            gd_cursor_name = 'GWA_HEADER-BKTXT'.

          ENDIF.

      ENDCASE.

    WHEN 1.

      "ZFIMSG 133: Data with Doc. Header Text & not found
      MESSAGE s133(zfimsg) WITH p_gwa_header-bktxt DISPLAY LIKE 'E'.

      CLEAR gd_cursor_name.
      gd_cursor_name = 'GWA_HEADER-BKTXT'.

    WHEN 2.

      "ZFIMSG 132: Document has been reversed
      MESSAGE s132(zfimsg) DISPLAY LIKE 'E'.

      CLEAR gd_cursor_name.
      gd_cursor_name = 'GWA_HEADER-BKTXT'.

    WHEN 3.

      "ZFIMSG 141: Document has been processed
      MESSAGE s141(zfimsg) DISPLAY LIKE 'E'.

      CLEAR gd_cursor_name.
      gd_cursor_name = 'GWA_HEADER-BKTXT'.

  ENDCASE.

  "*--------------------------------------------------------------------*

  gd_bktxt_old = p_gwa_header-bktxt.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_GLMAP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_STAGING
*&      <-- GT_GLMAP
*&---------------------------------------------------------------------*
FORM f_get_glmap  USING pt_bseg          TYPE gtt_bseg_2
               CHANGING pct_glmap        TYPE gtt_glmap.

  DATA: lt_bseg       TYPE gtt_bseg.

  lt_bseg        = pt_bseg.
  SORT lt_bseg BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING matnr.

  CHECK lt_bseg IS NOT INITIAL.

  SELECT *
    FROM zmmdt00010
    INTO TABLE pct_glmap
    FOR ALL ENTRIES IN lt_bseg
    WHERE matnr = lt_bseg-matnr AND
          glacc <> space AND
          rcacc <> space.
  IF sy-subrc = 0.
    SORT pct_glmap BY matnr zbsrt.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_WHTMAP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_LFA1
*&      <-- GT_WHTMAP
*&---------------------------------------------------------------------*
FORM f_get_whtmap  USING pt_bseg          TYPE gtt_bseg
                CHANGING pct_whtmap       TYPE gtt_whtmap.

  DATA: lt_bseg       TYPE gtt_bseg.

  lt_bseg     = pt_bseg.
  SORT lt_bseg BY fityp.
  DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING fityp.

  CHECK lt_bseg IS NOT INITIAL.

  SELECT *
    FROM zfidt00005
    INTO TABLE pct_whtmap
    FOR ALL ENTRIES IN lt_bseg
    WHERE zvtc = lt_bseg-fityp.
  IF sy-subrc = 0.
    SORT pct_whtmap BY zvtc bklas ztdf.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_FIDT00002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_get_fidt00002  USING    pt_bseg        TYPE gtt_bseg
                      CHANGING pct_zfidt00002  TYPE gtt_zfidt00002.

  CHECK pt_bseg[] IS NOT INITIAL.

  SELECT zvtc ztrf
    INTO TABLE pct_zfidt00002
    FROM zfidt00002
    FOR ALL ENTRIES IN pt_bseg
    WHERE zvtc EQ pt_bseg-fityp.

  SORT pct_zfidt00002 BY zvtc.
  DELETE ADJACENT DUPLICATES FROM pct_zfidt00002
    COMPARING zvtc.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_FIDT00013
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_get_fidt00013  USING    pt_bseg        TYPE gtt_bseg
                      CHANGING pct_zfidt00013  TYPE gtt_zfidt00013.

  CHECK pt_bseg[] IS NOT INITIAL.

*  SELECT bklas ztrf FROM zfidt00013
*    INTO TABLE pct_zfidt00013
*    FOR ALL ENTRIES IN pt_bseg
*    WHERE bklas EQ pt_bseg-bklas.
*
*  SORT pct_zfidt00013 BY bklas.
*  DELETE ADJACENT DUPLICATES FROM pct_zfidt00013 COMPARING bklas.

  DATA(lit_bseg) = pt_bseg[].
  DELETE lit_bseg WHERE matnr IS INITIAL.
  SORT lit_bseg ASCENDING BY matnr.
  DELETE ADJACENT DUPLICATES FROM lit_bseg COMPARING matnr.

  IF lit_bseg[] IS NOT INITIAL.
    SELECT * FROM zficd_zfi05e0029_mbew
      INTO TABLE @DATA(lit_zfivt00122)
      FOR ALL ENTRIES IN @lit_bseg
        WHERE matnr EQ @lit_bseg-matnr.
    IF lit_zfivt00122[] IS NOT INITIAL.
      SELECT bklas ztrf FROM zfidt00013
        INTO TABLE pct_zfidt00013
        FOR ALL ENTRIES IN lit_zfivt00122
          WHERE bklas EQ lit_zfivt00122-bklas.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_WHT_TYPE_N_CODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_ZFIDT00002
*&      --> GIT_ZFIDT00013
*&      <-- GWA_BSEG_B
*&---------------------------------------------------------------------*
FORM f_get_wht_type_n_code  USING    pt_fidt00002 TYPE gtt_zfidt00002
                                     pt_fidt00013 TYPE gtt_zfidt00013
                                     pt_whtmap       TYPE gtt_whtmap
                                     p_fityp
                                     p_bklas
                            CHANGING p_gwa_bseg TYPE gty_bseg
                                     p_lra_witht TYPE zfitt00036
                                     p_lra_withcd TYPE zfitt00037.

  DATA: lwa_fidt00002 TYPE gty_zfidt00002,
        lwa_fidt00013 TYPE gty_zfidt00013,
        ld_ztdf       TYPE zfidt00005-ztdf.

*--------------------------------------------------------------------*

  CLEAR ld_ztdf.
*  CHECK p_gwa_bseg-lifnr IS NOT INITIAL.
*  CHECK p_gwa_bseg-matnr IS NOT INITIAL.
*  CHECK p_gwa_bseg-bklas IS NOT INITIAL.
  CHECK p_bklas IS NOT INITIAL.

  "*--------------------------------------------------------------------*

  READ TABLE pt_fidt00002 INTO lwa_fidt00002
*    WITH KEY zvtc = p_gwa_bseg-fityp
    WITH KEY zvtc = p_fityp
    BINARY SEARCH.
  IF sy-subrc NE 0.
    CLEAR lwa_fidt00002.
  ENDIF.

  READ TABLE pt_fidt00013 INTO lwa_fidt00013
*    WITH KEY bklas = p_gwa_bseg-bklas
    WITH KEY bklas = p_bklas
    BINARY SEARCH.
  IF sy-subrc NE 0.
    CLEAR lwa_fidt00013.
  ENDIF.

  IF lwa_fidt00002-ztrf EQ space AND
     lwa_fidt00013-ztrf EQ space.
    ld_ztdf = abap_true.
  ELSE.
    ld_ztdf = abap_false.
  ENDIF.

  READ TABLE pt_whtmap INTO DATA(lwa_whtmap)
*    WITH KEY zvtc  = p_gwa_bseg-fityp
    WITH KEY zvtc  = p_fityp
*             bklas = p_gwa_bseg-bklas
             bklas = p_bklas
             ztdf  = ld_ztdf
    BINARY SEARCH.
  IF sy-subrc = 0.

    p_gwa_bseg-witht     = lwa_whtmap-witht.
    p_gwa_bseg-withcd     = lwa_whtmap-zwht_code.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_whtmap-witht ) TO p_lra_witht.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_whtmap-zwht_code ) TO p_lra_withcd.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_WHT_TYPE_N_CODE_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_ZFIDT00002
*&      --> GIT_ZFIDT00013
*&      <-- GWA_BSEG_B
*&---------------------------------------------------------------------*
FORM f_get_wht_type_n_code_b  USING    p_fityp
                                       p_bklas
                                       pt_fidt00002 TYPE gtt_zfidt00002
                                       pt_fidt00013 TYPE gtt_zfidt00013
                                       pt_whtmap       TYPE gtt_whtmap
                              CHANGING p_witht
                                       p_withcd.

  DATA: lwa_fidt00002 TYPE gty_zfidt00002,
        lwa_fidt00013 TYPE gty_zfidt00013,
        ld_ztdf       TYPE zfidt00005-ztdf.

*--------------------------------------------------------------------*

  CLEAR ld_ztdf.

  READ TABLE pt_fidt00002 INTO lwa_fidt00002
    WITH KEY zvtc = p_fityp
    BINARY SEARCH.
  IF sy-subrc NE 0.
    CLEAR lwa_fidt00002.
  ENDIF.

  READ TABLE pt_fidt00013 INTO lwa_fidt00013
    WITH KEY bklas = p_bklas
    BINARY SEARCH.
  IF sy-subrc NE 0.
    CLEAR lwa_fidt00013.
  ENDIF.

  IF lwa_fidt00002-ztrf EQ space AND
     lwa_fidt00013-ztrf EQ space.
    ld_ztdf = abap_true.
  ELSE.
    ld_ztdf = abap_false.
  ENDIF.

  READ TABLE pt_whtmap INTO DATA(lwa_whtmap)
    WITH KEY zvtc  = p_fityp
             bklas = p_bklas
             ztdf  = ld_ztdf
    BINARY SEARCH.
  IF sy-subrc = 0.

    p_witht  = lwa_whtmap-witht.
    p_withcd = lwa_whtmap-zwht_code.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA_REVERSE_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LD_GJAHR
*&      --> LD_BELNR
*&---------------------------------------------------------------------*
FORM f_get_data_reverse_po USING "p_gjahr
                                 "p_belnr
                                 p_gwa_header TYPE zfist00193
                        CHANGING p_subrc.

  DATA: ld_bklas   TYPE mbew-bklas,

        lit_bseg   TYPE TABLE OF gty_bseg,
        lra_witht  TYPE TABLE OF zfist00196,
        lra_withcd TYPE TABLE OF zfist00197.

*--------------------------------------------------------------------*

  "Select BSEG - Get Invoice Document of Invoice Receipts Header & Detail
  SELECT * FROM zficd_zfi05e0029_bseg INTO CORRESPONDING FIELDS OF TABLE @git_bseg
    WHERE bukrs EQ @p_gwa_header-bukrs AND
*          gjahr EQ @p_gjahr AND
          gjahr EQ @p_gwa_header-gjahr AND
*          belnr EQ @p_belnr AND
          belnr EQ @p_gwa_header-belnr AND
          buzid NE 'W'. "GR/IR ITEM

  IF git_bseg[] IS NOT INITIAL.

    "*--------------------------------------------------------------------*

    "Select RSEG - Get Purchase Order number and PO Line Item
    SELECT belnr,
           gjahr,
           ebeln,
           ebelp,
           buzei
      FROM rseg INTO TABLE @git_rseg
        FOR ALL ENTRIES IN @git_bseg
          WHERE belnr EQ @git_bseg-awkey_belnr AND
                gjahr EQ @git_bseg-awkey_gjahr.

    IF git_rseg[] IS NOT INITIAL.

      "Select EKBE - Get Material Document of Goods Receipt – from History per Purchasing Document
      SELECT *
        FROM zficd_zfi05e0029_ekbe INTO TABLE @git_ekbe
        FOR ALL ENTRIES IN @git_rseg
        WHERE ebeln EQ @git_rseg-ebeln AND
              ebelp EQ @git_rseg-ebelp AND
              gjahr EQ @git_rseg-gjahr AND
              buzei_b EQ @git_rseg-buzei AND
              vgabe EQ '1'. "Goods Receipt

      IF git_ekbe[] IS NOT INITIAL.

        "Select BSEG - Get Accounting Document for Goods Receipt Header & Detail
        SELECT * FROM zficd_zfi05e0029_bseg INTO TABLE @git_bseg_2
          FOR ALL ENTRIES IN @git_ekbe
          WHERE bukrs EQ @gwa_header-bukrs AND
                awkey EQ @git_ekbe-awkey AND
                buzid NE 'W'. "GR/IR ITEM

        IF git_bseg_2[] IS NOT INITIAL.

*          "Get mapping GL Account based on material in Accounting Document
*          PERFORM f_get_glmap USING git_bseg_2[]
*                           CHANGING git_glmap.

          CLEAR lit_bseg.
          lit_bseg[] = CORRESPONDING #( git_bseg_2 ).

          "Remove LIFNR where KOART != K
          LOOP AT lit_bseg ASSIGNING FIELD-SYMBOL(<lfs_bseg>)
            WHERE koart NE 'K'.
            <lfs_bseg>-lifnr = ''.
            <lfs_bseg>-lifnr_name = ''.

            "---

            "Get MWSKZ based on BUKRS, EBELN & EBELP
            SELECT SINGLE * FROM bseg INTO @DATA(lwa_bseg)
              WHERE bukrs EQ @<lfs_bseg>-bukrs AND
                    gjahr EQ @p_gwa_header-gjahr AND
                    belnr EQ @p_gwa_header-belnr AND
                    ebeln EQ @<lfs_bseg>-ebeln AND
                    ebelp EQ @<lfs_bseg>-ebelp.
            IF sy-subrc EQ 0.
              <lfs_bseg>-mwskz = lwa_bseg-mwskz.
            ENDIF.

          ENDLOOP.

          APPEND LINES OF lit_bseg[] TO git_bseg[].

        ENDIF.

      ENDIF.

    ENDIF.

    "*--------------------------------------------------------------------*

    "Get ZFIDT00005 Mapping WHT
    PERFORM f_get_whtmap USING git_bseg[]
                      CHANGING git_whtmap.

    "*--------------------------------------------------------------------*
    "Get WITHT & WITHCD

    PERFORM f_get_fidt00002 USING git_bseg[]
                         CHANGING git_zfidt00002.

    PERFORM f_get_fidt00013 USING git_bseg[]
                         CHANGING git_zfidt00013.

    "*--------------------------------------------------------------------*
    "Fill WITHT & WITHCD

    CLEAR: lra_witht[], lra_withcd[].
    LOOP AT git_bseg ASSIGNING <lfs_bseg>.

      CLEAR ld_bklas.
      SELECT SINGLE bklas FROM zficd_zfi05e0029_mbew INTO @ld_bklas
        WHERE matnr EQ @<lfs_bseg>-matnr.

      PERFORM f_get_wht_type_n_code USING git_zfidt00002
                                          git_zfidt00013
                                          git_whtmap
                                          <lfs_bseg>-fityp
                                          ld_bklas
                                     CHANGING <lfs_bseg>
                                              lra_witht[]
                                              lra_withcd[].

    ENDLOOP.

    "*--------------------------------------------------------------------*

    SORT lra_witht ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM lra_witht COMPARING low.

    SORT lra_withcd ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM lra_withcd COMPARING low.

    "*--------------------------------------------------------------------*

    "Get detail of WHT data
    SELECT bukrs,
           belnr,
           gjahr,
           witht,
           wt_withcd,
           wt_qsshh,
           wt_qsshb,
           wt_qbshh,
           wt_qbshb,
           koart,
           wt_acco
      FROM with_item
        INTO TABLE @git_withitem
          FOR ALL ENTRIES IN @git_bseg
            WHERE bukrs EQ @git_bseg-bukrs AND
                  belnr EQ @git_bseg-belnr AND
                  gjahr EQ @git_bseg-gjahr. "AND
*                  witht IN @lra_witht AND
*                  wt_withcd IN @lra_withcd.

    SORT git_bseg ASCENDING BY bukrs belnr gjahr buzei.

  ELSE.
    p_subrc = 1.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA_INV_NON_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LD_GJAHR
*&      --> LD_BELNR
*&---------------------------------------------------------------------*
FORM f_get_data_inv_non_po  USING p_flag_pph26
                                  p_gwa_header TYPE zfist00193.
*                         CHANGING p_subrc.

  DATA: ld_bklas   TYPE mbew-bklas,

        lit_bseg   TYPE TABLE OF gty_bseg,
        lra_witht  TYPE TABLE OF zfist00196,
        lra_withcd TYPE TABLE OF zfist00197.

*--------------------------------------------------------------------*
  "Manipulate data
  "Line LIFNR yang memiliki FITYP, FITYP nya dicopy ke line MATNR.

  LOOP AT git_bseg INTO DATA(lwa_bseg) WHERE lifnr IS NOT INITIAL AND fityp IS NOT INITIAL.
    DATA(ld_fityp) = lwa_bseg-fityp.
    EXIT.
  ENDLOOP.

  IF ld_fityp IS NOT INITIAL.
    LOOP AT git_bseg ASSIGNING FIELD-SYMBOL(<lfs_bseg>) WHERE matnr IS NOT INITIAL.
      <lfs_bseg>-fityp = ld_fityp.
    ENDLOOP.
  ENDIF.

  "*--------------------------------------------------------------------*

  "Get ZFIDT00005 Mapping WHT
  PERFORM f_get_whtmap USING git_bseg[]
                    CHANGING git_whtmap.

  "*--------------------------------------------------------------------*
  "Get WITHT & WITHCD

  PERFORM f_get_fidt00002 USING git_bseg[]
                       CHANGING git_zfidt00002.

  PERFORM f_get_fidt00013 USING git_bseg[]
                       CHANGING git_zfidt00013.

  "*--------------------------------------------------------------------*
  "Fill WITHT & WITHCD

  CLEAR: lra_witht[], lra_withcd[].
*  LOOP AT git_bseg ASSIGNING FIELD-SYMBOL(<lfs_bseg>).
  LOOP AT git_bseg ASSIGNING <lfs_bseg>.

    CLEAR ld_bklas.
    SELECT SINGLE bklas FROM zficd_zfi05e0029_mbew INTO @ld_bklas
      WHERE matnr EQ @<lfs_bseg>-matnr.

    PERFORM f_get_wht_type_n_code USING git_zfidt00002
                                        git_zfidt00013
                                        git_whtmap
                                        <lfs_bseg>-fityp
                                        ld_bklas
                                   CHANGING <lfs_bseg>
                                            lra_witht[]
                                            lra_withcd[].

  ENDLOOP.

  "*--------------------------------------------------------------------*

  SORT lra_witht ASCENDING BY low.
  DELETE ADJACENT DUPLICATES FROM lra_witht COMPARING low.

  SORT lra_withcd ASCENDING BY low.
  DELETE ADJACENT DUPLICATES FROM lra_withcd COMPARING low.

  "*--------------------------------------------------------------------*

  "Get detail of WHT data
  SELECT bukrs,
         belnr,
         gjahr,
         witht,
         wt_withcd,
         wt_qsshh,
         wt_qsshb,
         wt_qbshh,
         wt_qbshb,
         koart,
         wt_acco
    FROM with_item
      INTO TABLE @git_withitem
        FOR ALL ENTRIES IN @git_bseg
          WHERE bukrs EQ @git_bseg-bukrs AND
                belnr EQ @git_bseg-belnr AND
                gjahr EQ @git_bseg-gjahr. "AND
*                witht IN @lra_witht AND
*                wt_withcd IN @lra_withcd.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA_INV_PPH26
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LD_GJAHR
*&      --> LD_BELNR
*&---------------------------------------------------------------------*
FORM f_get_data_inv_pph26  USING p_flag_pph26
                                  p_gwa_header TYPE zfist00193.
*                         CHANGING p_subrc.

  DATA: ld_bklas   TYPE mbew-bklas,

        lit_bseg   TYPE TABLE OF gty_bseg,
        lra_witht  TYPE TABLE OF zfist00196,
        lra_withcd TYPE TABLE OF zfist00197.

*--------------------------------------------------------------------*
  "Get ZFIDT00005 Mapping WHT

  PERFORM f_get_whtmap USING git_bseg[]
                    CHANGING git_whtmap.

  "*--------------------------------------------------------------------*
  "Get WITHT & WITHCD

  PERFORM f_get_fidt00002 USING git_bseg[]
                       CHANGING git_zfidt00002.

  PERFORM f_get_fidt00013 USING git_bseg[]
                       CHANGING git_zfidt00013.

  "*--------------------------------------------------------------------*
  "Fill WITHT & WITHCD

  CLEAR: lra_witht[], lra_withcd[].
  LOOP AT git_bseg ASSIGNING FIELD-SYMBOL(<lfs_bseg>).

    CLEAR ld_bklas.
    SELECT SINGLE bklas FROM zficd_zfi05e0029_mbew INTO @ld_bklas
      WHERE matnr EQ @<lfs_bseg>-matnr.

    PERFORM f_get_wht_type_n_code USING git_zfidt00002
                                        git_zfidt00013
                                        git_whtmap
                                        <lfs_bseg>-fityp
                                        ld_bklas
                                   CHANGING <lfs_bseg>
                                            lra_witht[]
                                            lra_withcd[].

  ENDLOOP.

  "*--------------------------------------------------------------------*

  " Get detail of WHT data
  SELECT bukrs,
         belnr,
         gjahr,
         witht,
         wt_withcd,
         wt_qsshh,
         wt_qsshb,
         wt_qbshh,
         wt_qbshb,
         koart,
         wt_acco
    FROM with_item
      INTO TABLE @git_withitem
        FOR ALL ENTRIES IN @git_bseg
          WHERE bukrs EQ @git_bseg-bukrs AND
                belnr EQ @git_bseg-belnr AND
                gjahr EQ @git_bseg-gjahr.  "AND
*                witht IN @lra_witht AND
*                wt_withcd IN @lra_withcd.

*--------------------------------------------------------------------*
  "Get STVARV Adjustment PPH26

  CALL METHOD zcl_tvarv=>get_parameter_value
    EXPORTING
      variable_name = gc_stvarv_name_adj_pph26
    RECEIVING
      param         = DATA(ld_tvarvc_low).

  gd_racct_adj_pph26 = |{ ld_tvarvc_low ALPHA = IN }|.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CHECK_DOC_H_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GWA_HEADER_BKTXT
*&      <-- LD_GJAHR
*&      <-- LD_BELNR
*&      <-- GWA_HEADER
*&---------------------------------------------------------------------*
FORM f_check_doc_h_text  USING    p_bktxt
                         CHANGING p_subrc
                                  p_gwa_header TYPE zfist00193.

  DATA(ld_len) = strlen( p_bktxt ).
*  IF ld_len EQ 14.
  IF ld_len >= 5 .

    p_gwa_header-gjahr = gwa_header-bktxt(4).
*    p_gwa_header-belnr = gwa_header-bktxt+4(10).

    DATA(ld_len_belnr) = ld_len - 4.
    p_gwa_header-belnr = gwa_header-bktxt+4(ld_len_belnr).
    p_gwa_header-belnr = |{ p_gwa_header-belnr ALPHA = IN }|.

    "*--------------------------------------------------------------------*

    SELECT SINGLE bukrs,
                  belnr,
                  gjahr,
                  blart,
                  bktxt,
                  stblg,
                  waers,
                  kursf,
                  bldat
      FROM bkpf
        INTO @DATA(lwa_bkpf)
          WHERE bukrs EQ @gwa_header-bukrs AND
                gjahr EQ @p_gwa_header-gjahr AND
                belnr EQ @p_gwa_header-belnr.
    IF sy-subrc EQ 0.

      IF lwa_bkpf-stblg EQ ''.

*        SELECT SINGLE * FROM zfidt00326 INTO @DATA(lwa_zfidt00326)
*          WHERE bukrs EQ @p_gwa_header-bukrs AND
*                gjahr EQ @p_gwa_header-gjahr AND
*                belnr EQ @p_gwa_header-belnr.
*        IF sy-subrc NE 0.

        CASE lwa_bkpf-blart.
          WHEN 'RE'.

            p_gwa_header-blart = lwa_bkpf-blart.
            p_gwa_header-waers = lwa_bkpf-waers.
            p_gwa_header-kursf = lwa_bkpf-kursf.
            p_gwa_header-bldat = lwa_bkpf-bldat.

          WHEN OTHERS.

            p_gwa_header-blart = lwa_bkpf-blart.
            p_gwa_header-waers = lwa_bkpf-waers.
            p_gwa_header-kursf = lwa_bkpf-kursf.
            p_gwa_header-bldat = lwa_bkpf-bldat.

        ENDCASE.

*        ELSE.
*
*          p_subrc = 3. "Document has been processed
*
*        ENDIF.

      ELSE.

        p_subrc = 2. "Document has been reversed

      ENDIF.

    ELSE.

      p_subrc = 1. "not found

      p_gwa_header-blart = ''.
      p_gwa_header-waers = ''.

    ENDIF.

  ELSE.

    p_subrc = 1. "not found

    p_gwa_header-blart = ''.
    p_gwa_header-waers = ''.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CLEAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_clear USING p_kind.

  CASE p_kind.
    WHEN 'CLEAR_ALL_W/0_HEADER'.

      CLEAR: git_bseg[], git_bseg[], git_rseg[], git_ekbe[], git_bseg_2[],
             git_whtmap[], git_zfidt00002[], git_zfidt00013[],
             git_zfidt00322[],git_lineitem[].
      CLEAR: git_message[].
      CLEAR: gd_racct_adj_pph26.
      CLEAR: gwa_zfidt00322.

    WHEN 'CLEAR_ALL'.

      CLEAR gd_bktxt_old.
      CLEAR gwa_header.
      CLEAR: git_bseg[], git_bseg[], git_rseg[], git_ekbe[], git_bseg_2[],
             git_whtmap[], git_zfidt00002[], git_zfidt00013[],
             git_zfidt00322[],git_lineitem[].
      CLEAR: git_message[].
      CLEAR: gd_racct_adj_pph26.
      CLEAR: gwa_zfidt00322.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_DEFINE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GWA_HEADER
*&      <-- GD_FLAG_PPH26
*&---------------------------------------------------------------------*
FORM f_define  USING    p_gwa_header TYPE zfist00193
               CHANGING p_subrc
                        p_flag.  "RE | NOT_RE | PPH26

  DATA: ld_found TYPE boolean.

*--------------------------------------------------------------------*

  "*--------------------------------------------------------------------*
  "Get Table Maintain Crt. Mass Jour. Perbaikan BukPot (ZFI05E0029)

*  SELECT * FROM zfidt00322 INTO TABLE git_zfidt00322.

  "*--------------------------------------------------------------------*

  CLEAR ld_found.

  "Select BSEG - Get Invoice Document of Invoice Receipts Header & Detail
  SELECT * FROM zficd_zfi05e0029_bseg INTO CORRESPONDING FIELDS OF TABLE @git_bseg
    WHERE bukrs EQ @p_gwa_header-bukrs AND
          gjahr EQ @p_gwa_header-gjahr AND
          belnr EQ @p_gwa_header-belnr.

  IF git_bseg[] IS NOT INITIAL.

    LOOP AT git_bseg INTO DATA(lwa_bseg).

*      READ TABLE git_zfidt00322 INTO DATA(lwa_zfidt00322)
*        WITH KEY racct = lwa_bseg-racct.
*      IF sy-subrc EQ 0.
*        ld_found = 'X'.
*        EXIT.
*      ENDIF.

      SELECT SINGLE * FROM zfidt00322 INTO @gwa_zfidt00322
        WHERE racct EQ @lwa_bseg-racct.
      IF sy-subrc EQ 0.
        APPEND gwa_zfidt00322 TO git_zfidt00322.
        ld_found = 'X'.
        EXIT.
      ENDIF.


*      IF lwa_bseg-racct IN lra_filter_pph26.
*        ld_found = 'X'.
*        EXIT.
*      ENDIF.

    ENDLOOP.

    IF ld_found EQ 'X'.
      p_flag = 'PPH26'.  "RE | NOT_RE | PPH26
    ELSE.
      p_flag = 'NOT_RE'.  "RE | NOT_RE | PPH26
    ENDIF.

    SORT git_bseg ASCENDING BY bukrs belnr gjahr buzei.

  ELSE.

    p_subrc = 1.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_FILL_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_fill_input USING p_gwa_header TYPE zfist00193
                        p_flag.

  DATA: lwa_witht_withcd TYPE gty_witht_withcd,
        ld_racct         TYPE acdoca-racct.

*--------------------------------------------------------------------*

  CASE p_gwa_header-blart.
    WHEN 'RE'.

      "*--------------------------------------------------------------------*
      "Fill RACCT from MATNR

      LOOP AT git_lineitem WHERE kind EQ gc_fl_posting AND
                                 matnr NE space.

        CLEAR lwa_witht_withcd.
*        LOOP AT git_lineitem INTO DATA(lwa_lineitem) WHERE kind EQ gc_fl_posting AND
*                                                           matnr NE space.
*          lwa_witht_withcd-matnr = lwa_lineitem-matnr.
*          EXIT.
*        ENDLOOP.
        lwa_witht_withcd-matnr = git_lineitem-matnr.

        LOOP AT git_lineitem INTO DATA(lwa_lineitem) WHERE kind EQ gc_fl_posting AND
                                                     lifnr NE space.
          lwa_witht_withcd-lifnr = lwa_lineitem-lifnr.
          EXIT.
        ENDLOOP.

        PERFORM f_get_witht_withcd_whtbase    USING lwa_witht_withcd-lifnr
                                                    lwa_witht_withcd-matnr
                                           CHANGING lwa_witht_withcd-witht
                                                    lwa_witht_withcd-withcd.

        git_lineitem-witht = lwa_witht_withcd-witht.
        git_lineitem-withcd = lwa_witht_withcd-withcd.

        SELECT SINGLE glacc FROM zmmdt00010 INTO git_lineitem-racct
          WHERE matnr EQ git_lineitem-matnr AND
                knttp EQ 'K'. "AND
*                zacty EQ 'K'.

        CLEAR ld_racct.
        ld_racct = |{ git_lineitem-racct ALPHA = IN }|.

        SELECT SINGLE txt50 FROM skat INTO git_lineitem-racct_desc
          WHERE spras EQ 'i' AND
                ktopl EQ p_gwa_header-bukrs AND
                saknr EQ ld_racct.
        IF sy-subrc NE 0.
          SELECT SINGLE txt50 FROM skat INTO git_lineitem-racct_desc
            WHERE spras EQ 'E' AND
                  ktopl EQ p_gwa_header-bukrs AND
                  saknr EQ ld_racct.
        ENDIF.

        git_lineitem-koart = 'S'.

        MODIFY git_lineitem.

      ENDLOOP.

      "*--------------------------------------------------------------------*
      "Fill RACCT from LIFNR

      LOOP AT git_lineitem WHERE kind EQ gc_fl_posting AND
                                 lifnr NE space.

        SELECT SINGLE akont FROM lfb1 INTO @git_lineitem-racct
          WHERE bukrs EQ @p_gwa_header-bukrs AND
                lifnr EQ @git_lineitem-lifnr.

        SELECT SINGLE txt50 FROM skat INTO git_lineitem-racct_desc
          WHERE spras EQ 'i' AND
                ktopl EQ p_gwa_header-bukrs AND
                saknr EQ git_lineitem-racct.
        IF sy-subrc NE 0.
          SELECT SINGLE txt50 FROM skat INTO git_lineitem-racct_desc
            WHERE spras EQ 'E' AND
                  ktopl EQ p_gwa_header-bukrs AND
                  saknr EQ ld_racct.
        ENDIF.

        git_lineitem-koart = 'K'.

        MODIFY git_lineitem.

      ENDLOOP.

      "*--------------------------------------------------------------------*
      "Fill KSCHL, KVSL1 & Calculate KBETR

      LOOP AT git_lineitem WHERE kind EQ gc_fl_posting AND
                                 mwskz IS NOT INITIAL.

        SELECT SINGLE * FROM a003
          INTO @DATA(lwa_a003)
            WHERE aland EQ 'ID' AND
                  mwskz EQ @git_lineitem-mwskz.
        IF sy-subrc EQ 0.

          git_lineitem-kschl = lwa_a003-kschl.

          SELECT SINGLE * FROM t683s
            INTO @DATA(lwa_t683s)
              WHERE kalsm EQ 'TAXID' AND
                    kappl EQ @lwa_a003-kappl AND
                    kschl EQ @lwa_a003-kschl.
          IF sy-subrc EQ 0.
            git_lineitem-kvsl1 = lwa_t683s-kvsl1.
          ENDIF.

          SELECT SINGLE * FROM konp
            INTO @DATA(lwa_konp)
              WHERE knumh EQ @lwa_a003-knumh AND
                    kappl EQ @lwa_a003-kappl AND
                    kschl EQ @lwa_a003-kschl.
          IF sy-subrc EQ 0.
*            git_lineitem-kbetr = lwa_konp-kbetr / 10.
            git_lineitem-kbetr = lwa_konp-kbetr.
          ENDIF.

        ENDIF.

        MODIFY git_lineitem.

      ENDLOOP.

      "*--------------------------------------------------------------------*
      "*--------------------------------------------------------------------*
      "*--------------------------------------------------------------------*

    WHEN OTHERS.

      CASE p_flag.
        WHEN 'NOT_RE'.

          "*--------------------------------------------------------------------*
          "Fill RACCT from MATNR

          LOOP AT git_lineitem WHERE kind EQ gc_fl_posting AND
                                     matnr NE space.

            CLEAR lwa_witht_withcd.
*            LOOP AT git_lineitem INTO lwa_lineitem WHERE kind EQ gc_fl_posting AND
*                                                         matnr NE space.
*              lwa_witht_withcd-matnr = lwa_lineitem-matnr.
*              EXIT.
*            ENDLOOP.
            lwa_witht_withcd-matnr = git_lineitem-matnr.

            LOOP AT git_lineitem INTO lwa_lineitem WHERE kind EQ gc_fl_posting AND
                                                         lifnr NE space.
              lwa_witht_withcd-lifnr = lwa_lineitem-lifnr.
              EXIT.
            ENDLOOP.

            PERFORM f_get_witht_withcd_whtbase    USING lwa_witht_withcd-lifnr
                                                        lwa_witht_withcd-matnr
                                               CHANGING lwa_witht_withcd-witht
                                                        lwa_witht_withcd-withcd.

            git_lineitem-witht = lwa_witht_withcd-witht.
            git_lineitem-withcd = lwa_witht_withcd-withcd.

            SELECT SINGLE glacc FROM zmmdt00010 INTO git_lineitem-racct
              WHERE matnr EQ git_lineitem-matnr AND
                    knttp EQ 'K'. "AND
*                zacty EQ 'K'.

            CLEAR ld_racct.
            ld_racct = |{ git_lineitem-racct ALPHA = IN }|.

            SELECT SINGLE txt50 FROM skat INTO git_lineitem-racct_desc
              WHERE spras EQ 'i' AND
                    ktopl EQ p_gwa_header-bukrs AND
                    saknr EQ ld_racct.
            IF sy-subrc NE 0.
              SELECT SINGLE txt50 FROM skat INTO git_lineitem-racct_desc
                WHERE spras EQ 'E' AND
                      ktopl EQ p_gwa_header-bukrs AND
                      saknr EQ ld_racct.
            ENDIF.

            git_lineitem-koart = 'S'.

            MODIFY git_lineitem.

          ENDLOOP.

          "*--------------------------------------------------------------------*
          "Fill RACCT from LIFNR

          LOOP AT git_lineitem WHERE kind EQ gc_fl_posting AND
                                     lifnr NE space.

            SELECT SINGLE akont FROM lfb1 INTO @git_lineitem-racct
              WHERE bukrs EQ @p_gwa_header-bukrs AND
                    lifnr EQ @git_lineitem-lifnr.

            SELECT SINGLE txt50 FROM skat INTO git_lineitem-racct_desc
              WHERE spras EQ 'i' AND
                    ktopl EQ p_gwa_header-bukrs AND
                    saknr EQ git_lineitem-racct.
            IF sy-subrc NE 0.
              SELECT SINGLE txt50 FROM skat INTO git_lineitem-racct_desc
                WHERE spras EQ 'E' AND
                      ktopl EQ p_gwa_header-bukrs AND
                      saknr EQ ld_racct.
            ENDIF.

            git_lineitem-koart = 'K'.

            MODIFY git_lineitem.

          ENDLOOP.

          "*--------------------------------------------------------------------*
          "Fill KSCHL, KVSL1 & Calculate KBETR

          LOOP AT git_lineitem WHERE kind EQ gc_fl_posting AND
                                     mwskz IS NOT INITIAL.

            SELECT SINGLE * FROM a003
              INTO @lwa_a003
                WHERE aland EQ 'ID' AND
                      mwskz EQ @git_lineitem-mwskz.
            IF sy-subrc EQ 0.

              git_lineitem-kschl = lwa_a003-kschl.

              SELECT SINGLE * FROM t683s
                INTO @lwa_t683s
                  WHERE kalsm EQ 'TAXID' AND
                        kappl EQ @lwa_a003-kappl AND
                        kschl EQ @lwa_a003-kschl.
              IF sy-subrc EQ 0.
                git_lineitem-kvsl1 = lwa_t683s-kvsl1.
              ENDIF.

              SELECT SINGLE * FROM konp
                INTO @lwa_konp
                  WHERE knumh EQ @lwa_a003-knumh AND
                        kappl EQ @lwa_a003-kappl AND
                        kschl EQ @lwa_a003-kschl.
              IF sy-subrc EQ 0.
*                git_lineitem-kbetr = lwa_konp-kbetr / 10.
                git_lineitem-kbetr = lwa_konp-kbetr.
              ENDIF.

            ENDIF.

            MODIFY git_lineitem.

          ENDLOOP.

          "*--------------------------------------------------------------------*
          "*--------------------------------------------------------------------*
          "*--------------------------------------------------------------------*

        WHEN 'PPH26'.

*****          LOOP AT git_lineitem WHERE mwskz IS NOT INITIAL.
*****
*****            SELECT SINGLE * FROM a003
*****              INTO @lwa_a003
*****                WHERE aland EQ 'ID' AND
*****                      mwskz EQ @git_lineitem-mwskz.
*****            IF sy-subrc EQ 0.
*****
*****              git_lineitem-kschl = lwa_a003-kschl.
*****
*****              SELECT SINGLE * FROM t683s
*****                INTO @lwa_t683s
*****                  WHERE kalsm EQ 'TAXID' AND
*****                        kappl EQ @lwa_a003-kappl AND
*****                        kschl EQ @lwa_a003-kschl.
*****              IF sy-subrc EQ 0.
*****                git_lineitem-kvsl1 = lwa_t683s-kvsl1.
*****              ENDIF.
*****
*****              SELECT SINGLE * FROM konp
*****                INTO @lwa_konp
*****                  WHERE knumh EQ @lwa_a003-knumh AND
*****                        kappl EQ @lwa_a003-kappl AND
*****                        kschl EQ @lwa_a003-kschl.
*****              IF sy-subrc EQ 0.
*****                git_lineitem-kbetr = lwa_konp-kbetr / 10.
*****              ENDIF.
*****
*****            ENDIF.
*****
*****            MODIFY git_lineitem.
*****
*****          ENDLOOP.

      ENDCASE.

  ENDCASE.

ENDFORM.
