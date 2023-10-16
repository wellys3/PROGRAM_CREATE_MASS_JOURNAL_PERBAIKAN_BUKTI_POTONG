*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_9100_I
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  PERFORM f_user_command_9100.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_user_command_9100 .

  CASE gd_ok_code.

    WHEN '&NEW'.

      gd_mode = 'C'.
      PERFORM f_clear USING 'CLEAR_ALL'.

    WHEN '&ENTER'.   "Enter

      CLEAR: gd_subrc, git_message[].
      PERFORM f_validation_b    USING gwa_header
                             CHANGING gd_subrc
                                      git_message.

      CHECK gd_subrc EQ 0.

      "*--------------------------------------------------------------------*

      PERFORM f_fill_input USING gwa_header
                                 gd_flag.

      "*--------------------------------------------------------------------*

      IF gd_bktxt_old NE gwa_header-bktxt.

        PERFORM f_clear USING 'CLEAR_ALL_W/0_HEADER'.

        CLEAR gd_flag.
        PERFORM f_get_data CHANGING gd_flag  "RE | NOT_RE | PPH26
                                    gwa_header.

      ENDIF.

      "*--------------------------------------------------------------------*

    WHEN '&CHECK'.

      CLEAR: gd_subrc, git_message[].
      PERFORM f_check    USING gd_flag
                               gwa_header
                      CHANGING gd_subrc
                               git_message.

      IF gd_subrc EQ 0.
        "ZFIMSG 134: Check: &
        MESSAGE s134(zfimsg) WITH 'Ok. Ready to post'.
      ELSE.

        "ZFIMSG 134: Check: &
        MESSAGE s134(zfimsg) WITH 'Not ok' DISPLAY LIKE 'E'.

        PERFORM f_set_message_numbering CHANGING git_message.
*        PERFORM f_display_message USING git_message.
        PERFORM f_display_message_b USING git_message.

      ENDIF.

    WHEN '&POST'. "Post

      CLEAR: gd_subrc, git_message[].
      PERFORM f_check    USING gd_flag
                               gwa_header
                      CHANGING gd_subrc
                               git_message.

      "*--------------------------------------------------------------------*

      READ TABLE git_message INTO DATA(lwa_message) WITH KEY type = 'E'.
      IF sy-subrc NE 0.

        CLEAR: gd_subrc, git_message[].
        PERFORM f_pre_posting     USING gwa_header
                                        gd_flag
                               CHANGING gd_subrc
                                       git_message.

      ELSE.
        "ZFIMSG 134: Check: &
        MESSAGE s134(zfimsg) WITH 'Not ok, please check message' DISPLAY LIKE 'E'.

        PERFORM f_set_message_numbering CHANGING git_message.
*        PERFORM f_display_message USING git_message.
        PERFORM f_display_message_b USING git_message.

      ENDIF.

    WHEN '&OTHER'.

      CALL SCREEN 9200 STARTING AT 10 5 .

    WHEN '&MESSAGE'.

      IF git_message[] IS NOT INITIAL.

        PERFORM f_set_message_numbering CHANGING git_message.
*      PERFORM f_display_message USING git_message.
        PERFORM f_display_message_b USING git_message.

      ELSE.
        "ZFIMSG 140: There is no message(s) to display
        MESSAGE w140(zfimsg).
      ENDIF.

  ENDCASE.

  PERFORM f_calc_sum.

ENDFORM.


**&---------------------------------------------------------------------*
**& Form F_VALIDATION
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_validation    USING p_kind
*                  CHANGING p_subrc
*                           p_git_message TYPE gtt_message.
*
*  CLEAR: gd_cursor_name, gd_cursor_line.
*
**  IF gd_ok_code = '&ENTER'.
*
*  IF gwa_header-bukrs IS INITIAL.
*    gd_cursor_name = 'GWA_HEADER-BUKRS'.
**    MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    MESSAGE 'Fill out Company Code required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    p_subrc = 1.
*
*    APPEND VALUE #( type = 'E'
*                    message = 'Fill out Company Code required entry fields'
*                  )
*      TO p_git_message.
*
*    IF p_kind EQ 'ENTER'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
******    IF gwa_header-blart IS INITIAL.
*******      SET CURSOR FIELD 'GWA_HEADER-BLART'.
******      gd_cursor_name = 'GWA_HEADER-BLART'.
******      MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
******      p_subrc = 1.
******      EXIT.
******    ENDIF.
*
*  IF gwa_header-bldat IS INITIAL.
**    gd_cursor_name = 'GWA_HEADER-BLDAT'.
**    MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    IF p_subrc NE 1 .
*      gd_cursor_name = 'GWA_HEADER-BLDAT'.
*      MESSAGE 'Fill out Document Date required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*
*    p_subrc = 1.
*
*    APPEND VALUE #( type = 'E'
*                    message = 'Fill out Document Date required entry fields'
*                  )
*      TO p_git_message.
*
*    IF p_kind EQ 'ENTER'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  IF gwa_header-budat IS INITIAL.
**    gd_cursor_name = 'GWA_HEADER-BUDAT'.
**    MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    IF p_subrc NE 1 .
*      gd_cursor_name = 'GWA_HEADER-BUDAT'.
*      MESSAGE 'Fill out Posting Date required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*    p_subrc = 1.
*
*    APPEND VALUE #( type = 'E'
*                    message = 'Fill out Posting Date required entry fields'
*                  )
*      TO p_git_message.
*
*    IF p_kind EQ 'ENTER'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  IF gwa_header-bktxt IS INITIAL.
**    gd_cursor_name = 'GWA_HEADER-BKTXT'.
**    MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    IF p_subrc NE 1.
*      gd_cursor_name = 'GWA_HEADER-BKTXT'.
*      MESSAGE 'Fill out Doc. Header Text required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*    p_subrc = 1.
*
*    APPEND VALUE #( type = 'E'
*                    message = 'Fill out Doc. Header Text required entry fields'
*                  )
*      TO p_git_message.
*
*    IF p_kind EQ 'ENTER'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
******    IF gwa_header-waers IS INITIAL.
******      gd_cursor_name = 'GWA_HEADER-WAERS'.
******      MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
******      p_subrc = 1.
******      EXIT.
******    ENDIF.
*
*  READ TABLE git_lineitem WITH KEY kind = gc_fl_posting.
*  IF sy-subrc EQ 0.
*
*    LOOP AT git_lineitem WHERE flag EQ gc_fl_posting.
*
*      gd_cursor_line = sy-tabix.
*
*      IF git_lineitem-matnr IS NOT INITIAL AND
*         git_lineitem-lifnr IS NOT INITIAL.
*
*        CLEAR gd_message.
*        DATA(ld_cursor_line) = CONV string( gd_cursor_line ).
*
*        gd_message = |'Material & Vendor cannot be filled together' 'in line' { ld_cursor_line }|.
*
*        IF p_subrc NE 1.
*          gd_cursor_name = 'GIT_LINEITEM-MATNR'.
*          MESSAGE gd_message TYPE 'S' DISPLAY LIKE 'E'.
*        ELSE.
*          CLEAR gd_cursor_line.
*        ENDIF.
*
*        p_subrc = 1.
*
*        APPEND VALUE #( type = 'E'
*                        message = gd_message
*                      )
*          TO p_git_message.
*
*        IF p_kind EQ 'ENTER'.
*          EXIT.
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDIF.
*
*ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_validation_b    USING p_gwa_header TYPE zfist00193
                    CHANGING p_subrc
                             p_git_message TYPE gtt_message.

*--------------------------------------------------------------------*

  CLEAR: gd_cursor_name, gd_cursor_line.

  "*--------------------------------------------------------------------*

  IF p_gwa_header-bukrs IS INITIAL.
    IF gd_cursor_name IS INITIAL.
      gd_cursor_name = 'GWA_HEADER-BUKRS'.

      "ZFIMSG 130: Fill out & required entry fields
      MESSAGE s130(zfimsg) DISPLAY LIKE 'E' WITH 'Company Code'.
    ENDIF.

    APPEND VALUE #( type = 'E'
                    id = 'ZFIMSG'
                    number = 130
                    message_v1 = 'Company Code'
                    message_v2 = ''
                    message_v3 = ''
                    message_v4 = ''
                  )
      TO p_git_message.
    p_subrc = 1.
  ENDIF.

  "*--------------------------------------------------------------------*

  IF p_gwa_header-bktxt IS INITIAL.
    IF gd_cursor_name IS INITIAL.
      gd_cursor_name = 'GWA_HEADER-BKTXT'.

      "ZFIMSG 130: Fill out & required entry fields
      MESSAGE s130(zfimsg) DISPLAY LIKE 'E' WITH 'Doc. Header Text'.
    ENDIF.
    APPEND VALUE #( type = 'E'
                    id = 'ZFIMSG'
                    number = 130
                    message_v1 = 'Doc. Header Text'
                    message_v2 = ''
                    message_v3 = ''
                    message_v4 = ''
                  )
      TO p_git_message.
    p_subrc = 1.
  ENDIF.

  "*--------------------------------------------------------------------*

*Confirmed dari Christian Bunaidi, 21/9/2023, 11:00 WIB,
*Reason: Document Date tidak perlu diisi oleh user, tapi langsung dari BKPF-BLDAT
*****  IF p_gwa_header-bldat IS INITIAL.
*****    IF gd_cursor_name IS INITIAL.
*****      gd_cursor_name = 'GWA_HEADER-BLDAT'.
*****
*****      "ZFIMSG 130: Fill out & required entry fields
*****      MESSAGE s130(zfimsg) DISPLAY LIKE 'E' WITH 'Document Date'.
*****    ENDIF.
*****
*****    APPEND VALUE #( type = 'E'
*****                    id = 'ZFIMSG'
*****                    number = 130
*****                    message_v1 = 'Document Date'
*****                    message_v2 = ''
*****                    message_v3 = ''
*****                    message_v4 = ''
*****                  )
*****      TO p_git_message.
*****    p_subrc = 1.
*****  ENDIF.

  "*--------------------------------------------------------------------*

  IF p_gwa_header-budat IS INITIAL.
    IF gd_cursor_name IS INITIAL.
      gd_cursor_name = 'GWA_HEADER-BUDAT'.

      "ZFIMSG 130: Fill out & required entry fields
      MESSAGE s130(zfimsg) DISPLAY LIKE 'E' WITH 'Posting Date'.
    ENDIF.
    APPEND VALUE #( type = 'E'
                    id = 'ZFIMSG'
                    number = 130
                    message_v1 = 'Posting Date'
                    message_v2 = ''
                    message_v3 = ''
                    message_v4 = ''
                  )
      TO p_git_message.
    p_subrc = 1.
  ENDIF.

  "*--------------------------------------------------------------------*

  READ TABLE git_lineitem WITH KEY kind = gc_fl_posting.
  IF sy-subrc EQ 0.

    LOOP AT git_lineitem WHERE kind EQ gc_fl_posting.

      gd_tabix = sy-tabix.

      IF git_lineitem-matnr IS NOT INITIAL AND
         git_lineitem-lifnr IS NOT INITIAL.

        CLEAR gd_message.
        gd_message = gd_tabix. CONDENSE gd_message.

        IF gd_cursor_name IS INITIAL.
          gd_cursor_name = 'GIT_LINEITEM-MATNR'.
          gd_cursor_line = gd_tabix.

          "ZFIMSG 131: Material && Vendor cannot be filled together in line &
          MESSAGE s131(zfimsg) DISPLAY LIKE 'E' WITH gd_message.
        ENDIF.

        APPEND VALUE #( type = 'E'
                        id = 'ZFIMSG'
                        number = 131
                        message_v1 = gd_message
                        message_v2 = ''
                        message_v3 = ''
                        message_v4 = ''
                      )
          TO p_git_message.

        p_subrc = 1.
        EXIT.

      ENDIF.

    ENDLOOP.

  ENDIF.

  "*--------------------------------------------------------------------*

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  M_CHECK_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_matnr INPUT.

  IF git_lineitem-matnr IS NOT INITIAL AND
     git_lineitem-kind NE gc_fl_posting_reverse.

    PERFORM f_check_matnr USING     git_lineitem-matnr.
    PERFORM f_get_maktx   USING     git_lineitem-matnr
                          CHANGING  git_lineitem-maktx.

  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Form F_CHECK_MATNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GWA_INVOICE_ITEM_MATNR
*&---------------------------------------------------------------------*
FORM f_check_matnr  USING     pu_matnr.

  DATA:
    ld_matnr     TYPE mara-matnr,
    lwa_material TYPE zmmdt00010,
    lwa_blart    TYPE zfidt00111.

  CHECK pu_matnr IS NOT INITIAL.

  SELECT SINGLE
    matnr
    FROM mara
    INTO ld_matnr
    WHERE matnr = pu_matnr.
  IF sy-subrc <> 0.

    "U9 557: Material does not exist
    MESSAGE s557(u9) DISPLAY LIKE 'E'.

    gd_cursor_name = 'GIT_LINEITEM-MATNR'.
    gd_cursor_line = tc_lineitem-current_line.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_MAKTX INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_get_maktx  USING    pu_matnr
                  CHANGING pc_maktx.

  DATA:
    ld_matnr TYPE makt-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = pu_matnr
    IMPORTING
      output       = ld_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  IF ld_matnr IS INITIAL.
    CLEAR pc_maktx.

  ELSE.
    SELECT SINGLE maktx
      INTO pc_maktx
      FROM makt
      WHERE matnr EQ ld_matnr
        AND spras EQ sy-langu.
    IF sy-subrc NE 0.
      CLEAR pc_maktx.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  M_CHECK_LIFNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_lifnr INPUT.

  IF git_lineitem-lifnr IS NOT INITIAL AND
     git_lineitem-kind NE gc_fl_posting_reverse.

    PERFORM f_check_lifnr    USING git_lineitem-lifnr
                          CHANGING git_lineitem-lifnr_name.

*    PERFORM f_get_vendor_name   USING git_lineitem-lifnr
*
*  PERFORM f_check_otv USING gwa_invoice-lifnr
*                   CHANGING gd_is_otv.

  ENDIF.

*  IF gd_is_otv = abap_true.
*    CLEAR gwa_otv.
*    gwa_otv-lifnr       = gwa_invoice-lifnr.
*    CALL SCREEN 9005 STARTING AT 10 10.
*  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Form F_CHECK_LIFNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GWA_INVOICE_LIFNR
*&---------------------------------------------------------------------*
FORM f_check_lifnr    USING pu_lifnr
                   CHANGING pc_lifnr_name.

  DATA: ld_lifnr TYPE lfa1-lifnr.

*--------------------------------------------------------------------*

  CLEAR ld_lifnr.
  ld_lifnr = |{ pu_lifnr ALPHA = IN }|.

  SELECT SINGLE * FROM lfa1 INTO @DATA(lwa_lfa1)
    WHERE lifnr EQ @ld_lifnr.
  IF sy-subrc EQ 0.

    pc_lifnr_name = lwa_lfa1-name1.

  ELSE.

    "ZFIMGS 145: & not valid
    MESSAGE s145(zfimsg) WITH 'Vendor' DISPLAY LIKE 'E'.

    gd_cursor_name = 'GIT_LINEITEM-LIFNR'.
    gd_cursor_line = tc_lineitem-current_line.

  ENDIF.

*  DATA: lwa_but100    TYPE but100.

*  CHECK pu_lifnr IS NOT INITIAL.

*{(ADD)/Equine/SAPABAP/EG-END/EG-MAN/16082019/Cause:change validation lifnr
*  SELECT rltyp
*    FROM but100
*    INTO TABLE @DATA(lt_reltyp)
*    WHERE partner EQ @pu_lifnr.

*  CHECK sy-subrc IS INITIAL.

*  READ TABLE lt_reltyp TRANSPORTING NO FIELDS WITH KEY rltyp = gc_rltyp.
*  IF sy-subrc IS INITIAL.
*    MESSAGE e036(/iam/config) WITH pu_lifnr.
*  ENDIF.
*
*  "Vendor type Karyawan is not allowed
*  READ TABLE lt_reltyp TRANSPORTING NO FIELDS WITH KEY rltyp = gc_rlty_karyawan.
*  IF sy-subrc IS INITIAL.
*    MESSAGE e084(zfimsg).
*  ENDIF.
*
**{(ADD)/Equine/EG-ABAP/EG-ADS/EG-MAN/03092019/Cause:one time vendor validation
*  "One Time Vendor is not allowed
*  READ TABLE lt_reltyp TRANSPORTING NO FIELDS WITH KEY rltyp = gc_rlty_otv.
*  IF sy-subrc IS INITIAL.
*    MESSAGE e095(zfimsg).
*  ENDIF.
**}(END OF ADD)/Equine/EG-ABAP/EG-ADS/EG-MAN/03092019
*
*  "Withdrawal: Vendor type must be "Cabang"
*  IF p_cbyr EQ gc_zlsch-withdrawal.
*    READ TABLE lt_reltyp TRANSPORTING NO FIELDS WITH KEY rltyp = gc_rlty_cabang.
*    IF sy-subrc IS NOT INITIAL.
*      READ TABLE lt_reltyp TRANSPORTING NO FIELDS WITH KEY rltyp = gc_rlty_cabang2.
*      IF sy-subrc IS NOT INITIAL.
*        MESSAGE e084(zfimsg).
*      ENDIF.
*    ENDIF.
*  ENDIF.

ENDFORM.


**&---------------------------------------------------------------------*
**& Form F_GET_VENDOR_NAME
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
*FORM f_get_vendor_name  USING pu_lifnr
*                     CHANGING pc_lifnr_name.
*
*select single * from lfa1 into data(l
*  where lifnr eq @pu_lifnr
*
*ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  M_CHECK_RACCT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_racct INPUT.

  IF git_lineitem-racct IS NOT INITIAL AND
     git_lineitem-kind NE gc_fl_posting_reverse.

    PERFORM f_check_racct    USING gwa_header-bukrs
                                   git_lineitem-racct
                          CHANGING git_lineitem-racct_desc.

  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Form F_CHECK_RACCT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM f_check_racct    USING pu_bukrs
                            pu_racct
                   CHANGING pu_racct_desc.

  DATA: ld_racct TYPE lfa1-lifnr.

*--------------------------------------------------------------------*

  CLEAR ld_racct.
  ld_racct = |{ pu_racct ALPHA = IN }|.

  SELECT SINGLE * FROM skat INTO @DATA(lwa_skat)
    WHERE spras EQ 'i' AND
          ktopl EQ @pu_bukrs AND
          saknr EQ @ld_racct.
  IF sy-subrc EQ 0.

    pu_racct_desc = lwa_skat-txt50.

  ELSE.

    CLEAR lwa_skat.
    SELECT SINGLE * FROM skat INTO @lwa_skat
      WHERE spras EQ 'E' AND
            ktopl EQ @pu_bukrs AND
            saknr EQ @ld_racct.
    IF sy-subrc EQ 0.

      pu_racct_desc = lwa_skat-txt50.

    ELSE.

      "ZFIMGS 145: & not valid
      MESSAGE s145(zfimsg) WITH 'G/L Account' DISPLAY LIKE 'E'.

      gd_cursor_name = 'GIT_LINEITEM-RACCT'.
      gd_cursor_line = tc_lineitem-current_line.

    ENDIF.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  M_F4_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_f4_matnr INPUT.
  PERFORM f_f4_matnr CHANGING git_lineitem-matnr.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Form F_F4_MATNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_f4_matnr  CHANGING pc_matnr.

  TYPES: BEGIN OF lty_f4_matnr,
           matnr TYPE zmmdt00010-matnr,
           maktx TYPE zmmdt00010-maktx,
         END OF lty_f4_matnr,
         ltt_f4_matnr TYPE TABLE OF lty_f4_matnr.

  DATA: lt_zmm10       TYPE TABLE OF zmmdt00010,
        lt_return_tab  TYPE ptrv_shelptab,
        lt_f4_matnr    TYPE ltt_f4_matnr,

        lwa_zmm10      LIKE LINE OF lt_zmm10,
        lwa_return_tab LIKE LINE OF lt_return_tab,
        lwa_f4_matnr   LIKE LINE OF lt_f4_matnr.

  DATA: lt_map  TYPE TABLE OF dselc,
        lwa_map TYPE dselc.

  SELECT matnr matkl bklas zbsrt knttp zacty
    maktx
    INTO CORRESPONDING FIELDS OF TABLE lt_zmm10
    FROM zmmdt00010
*    WHERE matnr IN gt_matkl
    WHERE matnr EQ pc_matnr.
*      AND zbsrt EQ gd_zbsrt.
  IF sy-subrc EQ 0.

    LOOP AT lt_zmm10 INTO lwa_zmm10.
      CLEAR lwa_f4_matnr.
      lwa_f4_matnr-matnr = lwa_zmm10-matnr.
      lwa_f4_matnr-maktx = lwa_zmm10-maktx.

      APPEND lwa_f4_matnr TO lt_f4_matnr.
    ENDLOOP.

*  ” Set return fields Order
    CLEAR lwa_map.
    lwa_map-fldname = 'F0001'.        " Set that field 1 of SH table fills VBAP-VBELN
    lwa_map-dyfldname = 'GIT_LINEITEM-MATNR'.
    APPEND lwa_map TO lt_map.

*  ” Item
    CLEAR lwa_map.
    lwa_map-fldname = 'F0002'.        " Set that field 2 of SH table fills VBAP-VBELN
    lwa_map-dyfldname = 'GIT_LINEITEM-MAKTX'.
    APPEND lwa_map TO lt_map.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'MATNR'
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'GIT_LINEITEM-MATNR'
      TABLES
        value_tab       = lt_f4_matnr
        dynpfld_mapping = lt_map
        return_tab      = lt_return_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
    ENDIF.

    READ TABLE lt_return_tab INTO lwa_return_tab INDEX 1.
    IF sy-subrc = 0.
      pc_matnr      = lwa_return_tab-fieldval.
    ENDIF.

  ELSE.
    MESSAGE s003(aq) DISPLAY LIKE 'W' WITH
      'Material suggestion is not available'.
*      'Material suggestion is not available for'
*      'Doc Type'.
*      p_blart.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CALC_SUM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_calc_sum .

  DATA: lit_lineitem TYPE TABLE OF zfist00194.

*--------------------------------------------------------------------*
  "Sum Debit

  CLEAR lit_lineitem[].
  lit_lineitem[] = git_lineitem[].

  SELECT SUM( amount ) AS amount
    FROM @lit_lineitem AS a
    WHERE amount > 0
*    GROUP BY cfcode, rhcur
      INTO @gwa_header-total_db
    ##db_feature_mode[itabs_in_from_clause]
    ##itab_key_in_select
    ##ITAB_DB_SELECT.

  "*--------------------------------------------------------------------*
  "Sum Credit

  CLEAR lit_lineitem[].
  lit_lineitem[] = git_lineitem[].

  SELECT SUM( amount ) AS amount
    FROM @lit_lineitem AS a
    WHERE amount < 0
*    GROUP BY cfcode, rhcur
      INTO @gwa_header-total_cr
    ##db_feature_mode[itabs_in_from_clause]
    ##itab_key_in_select
    ##ITAB_DB_SELECT.

  "*--------------------------------------------------------------------*
  "Traffic Light Compare Debet & Credit

  IF abs( gwa_header-total_cr ) EQ 0 AND gwa_header-total_db EQ 0.
    gwa_header-icon = icon_red_light.
  ELSEIF abs( gwa_header-total_cr ) EQ gwa_header-total_db.
    gwa_header-icon = icon_green_light.
  ELSE.
    gwa_header-icon = icon_red_light.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_9100 INPUT.

  CASE gd_ok_code.

    WHEN '&F03' OR  "Back
         '&F15' OR  "Exit
         '&F12'.    "Cancel

      CLEAR gd_message.
      CASE gd_ok_code.
        WHEN '&F03'.
          gd_message = 'back?'.
        WHEN '&F15'.
          gd_message = 'exit?'.
        WHEN '&F12'.
          gd_message = 'cancel?'.
      ENDCASE.

      CONCATENATE 'Are you sure to' gd_message
        INTO gd_message SEPARATED BY space.

      CLEAR gd_answer.
      PERFORM f_confirm    USING 'Please confirm?'
                                 gd_message
                                 'Yes'
                                 'No'
                        CHANGING gd_answer.

      CHECK gd_answer EQ '1'.

      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
