*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_9200_I
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_9200 INPUT.

  CASE gd_ok_code.

    WHEN "'&F03' OR  "Back
         "'&F15' OR  "Exit
         '&F12'.    "Cancel

*      CLEAR gd_message.
*      CASE gd_ok_code.
*        WHEN '&F03'.
*          gd_message = 'back?'.
*        WHEN '&F15'.
*          gd_message = 'exit?'.
*        WHEN '&F12'.
*          gd_message = 'cancel?'.
*      ENDCASE.
*
*      CONCATENATE 'Are you sure to' gd_message
*        INTO gd_message SEPARATED BY space.
*
*      CLEAR gd_answer.
*      PERFORM f_confirm    USING 'Please confirm?'
*                                 gd_message
*                                 'Yes'
*                                 'No'
*                        CHANGING gd_answer.
*
*      CHECK gd_answer EQ '1'.

      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.

  CASE gd_ok_code.
    WHEN '&EXEC'.

      PERFORM f_fill_another_document   TABLES git_lineitem[]
                                         USING gwa_another
                                      CHANGING gwa_header.

      LEAVE TO SCREEN 0 .

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_ANOTHER_BUKRS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_another_bukrs INPUT.

  CLEAR: git_dynpfld_mapping, git_dynpfld_mapping[].
  git_dynpfld_mapping-fldname = 'F0002'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-BUKRS'.
  APPEND git_dynpfld_mapping.

  git_dynpfld_mapping-fldname = 'F0003'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-GJAHR'.
  APPEND git_dynpfld_mapping.

  git_dynpfld_mapping-fldname = 'F0004'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-BELNR'.
  APPEND git_dynpfld_mapping.

  CLEAR git_zfidt00326[].
  PERFORM f_get_value_request_another CHANGING git_zfidt00326[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BUKRS'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'GWA_ANOTHER-BUKRS'
      value_org       = 'S'
    TABLES
      value_tab       = git_zfidt00326
      return_tab      = git_return_tab
      dynpfld_mapping = git_dynpfld_mapping.

  TRY.
      gwa_another-bukrs = git_return_tab[ retfield = 'GWA_ANOTHER-BUKRS' ]-fieldval.
      gwa_another-gjahr = git_return_tab[ retfield = 'GWA_ANOTHER-GJAHR' ]-fieldval.
      gwa_another-belnr = git_return_tab[ retfield = 'GWA_ANOTHER-BELNR' ]-fieldval.

    CATCH cx_root.
  ENDTRY.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_ANOTHER_GJAHR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_another_gjahr INPUT.

  CLEAR: git_dynpfld_mapping, git_dynpfld_mapping[].
  git_dynpfld_mapping-fldname = 'F0002'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-BUKRS'.
  APPEND git_dynpfld_mapping.

  git_dynpfld_mapping-fldname = 'F0003'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-GJAHR'.
  APPEND git_dynpfld_mapping.

  git_dynpfld_mapping-fldname = 'F0004'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-BELNR'.
  APPEND git_dynpfld_mapping.

  CLEAR git_zfidt00326[].
  PERFORM f_get_value_request_another CHANGING git_zfidt00326[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'GJAHR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'GWA_ANOTHER-GJAHR'
      value_org       = 'S'
    TABLES
      value_tab       = git_zfidt00326
      return_tab      = git_return_tab
      dynpfld_mapping = git_dynpfld_mapping.

  TRY.
      gwa_another-bukrs = git_return_tab[ retfield = 'GWA_ANOTHER-BUKRS' ]-fieldval.
      gwa_another-gjahr = git_return_tab[ retfield = 'GWA_ANOTHER-GJAHR' ]-fieldval.
      gwa_another-belnr = git_return_tab[ retfield = 'GWA_ANOTHER-BELNR' ]-fieldval.

    CATCH cx_root.
  ENDTRY.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_ANOTHER_BELNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_another_belnr INPUT.

  CLEAR: git_dynpfld_mapping, git_dynpfld_mapping[].
  git_dynpfld_mapping-fldname = 'F0002'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-BUKRS'.
  APPEND git_dynpfld_mapping.

  git_dynpfld_mapping-fldname = 'F0003'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-GJAHR'.
  APPEND git_dynpfld_mapping.

  git_dynpfld_mapping-fldname = 'F0004'.
  git_dynpfld_mapping-dyfldname = 'GWA_ANOTHER-BELNR'.
  APPEND git_dynpfld_mapping.

  CLEAR git_zfidt00326[].
  PERFORM f_get_value_request_another CHANGING git_zfidt00326[].

  CLEAR: git_return_tab.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BELNR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'GWA_ANOTHER-BELNR'
      value_org       = 'S'
    TABLES
      value_tab       = git_zfidt00326
      return_tab      = git_return_tab
      dynpfld_mapping = git_dynpfld_mapping.

  TRY.
      gwa_another-bukrs = git_return_tab[ retfield = 'GWA_ANOTHER-BUKRS' ]-fieldval.
      gwa_another-gjahr = git_return_tab[ retfield = 'GWA_ANOTHER-GJAHR' ]-fieldval.
      gwa_another-belnr = git_return_tab[ retfield = 'GWA_ANOTHER-BELNR' ]-fieldval.

    CATCH cx_root.
  ENDTRY.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Form F_GET_VALUE_REQUEST_ANOTHER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- p_lit_zfidt00326[]
*&---------------------------------------------------------------------*
FORM f_get_value_request_another  CHANGING p_lit_zfidt00326 TYPE gtt_zfidt00326.

  DATA: lwa_another       TYPE zfist00198,
        lit_dynpro_values TYPE TABLE OF dynpread,
        lwa_field_value   LIKE LINE OF lit_dynpro_values.

  RANGES: lra_bukrs FOR zfist00198-bukrs,
          lra_gjahr FOR zfist00198-gjahr,
          lra_belnr FOR zfist00198-belnr.

*--------------------------------------------------------------------*

*  CLEAR: lwa_field_value, lit_dynpro_values.
*  lwa_field_value-fieldname = 'GWA_ANOTHER-BUKRS'.
*  APPEND lwa_field_value TO lit_dynpro_values.
*  lwa_field_value-fieldname = 'GWA_ANOTHER-GJAHR'.
*  APPEND lwa_field_value TO lit_dynpro_values.
*  lwa_field_value-fieldname = 'GWA_ANOTHER-BELNR'.
*  APPEND lwa_field_value TO lit_dynpro_values.
*
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname             = sy-repid
*      dynumb             = sy-dynnr
*      translate_to_upper = 'X'
*    TABLES
*      dynpfields         = lit_dynpro_values.
*
*  CLEAR: lwa_field_value, lra_bukrs[].
*  lwa_field_value = lit_dynpro_values[ fieldname = 'GWA_ANOTHER-BUKRS' ].
*  IF lwa_field_value-fieldvalue IS NOT INITIAL.
*    f_fill_range: lra_bukrs 'I' 'EQ' lwa_field_value-fieldvalue ''.
*  ENDIF.
*
*  CLEAR: lwa_field_value, lra_gjahr[].
*  lwa_field_value = lit_dynpro_values[ fieldname = 'GWA_ANOTHER-GJAHR' ].
*  IF lwa_field_value-fieldvalue IS NOT INITIAL.
*    f_fill_range: lra_gjahr 'I' 'EQ' lwa_field_value-fieldvalue ''.
*  ENDIF.
*
*  CLEAR: lwa_field_value, lra_belnr[].
*  lwa_field_value = lit_dynpro_values[ fieldname = 'GWA_ANOTHER-BELNR' ].
*  IF lwa_field_value-fieldvalue IS NOT INITIAL.
*    f_fill_range: lra_belnr 'I' 'EQ' lwa_field_value-fieldvalue ''.
*  ENDIF.

*  SELECT *
*    FROM zfidt00326
*    INTO TABLE @p_lit_zfidt00326
*      WHERE bukrs IN @lra_bukrs AND
*            gjahr IN @lra_gjahr AND
*            belnr IN @lra_belnr.

  SELECT *
    FROM zfidt00326
    INTO TABLE @p_lit_zfidt00326.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_FILL_ANOTHER_DOCUMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GWA_ANOTHER
*&---------------------------------------------------------------------*
FORM f_fill_another_document    TABLES p_git_lineitem STRUCTURE zfist00194
                                 USING p_gwa_another TYPE zfist00198
                              CHANGING gwa_header TYPE zfist00193.

  "Get Header ZFIDT00326
  SELECT *
    FROM zfidt00326
      INTO TABLE @DATA(lit_zfidt00326)
      WHERE bukrs = @p_gwa_another-bukrs AND
            gjahr = @p_gwa_another-gjahr AND
            belnr = @p_gwa_another-belnr.
  IF lit_zfidt00326[] IS NOT INITIAL.

    DATA(lit_zfidt00326_h) = lit_zfidt00326[].
    SORT lit_zfidt00326_h ASCENDING BY bukrs gjahr belnr.
    DELETE ADJACENT DUPLICATES FROM lit_zfidt00326_h COMPARING bukrs gjahr belnr.

    TRY.
        gwa_header = CORRESPONDING #( lit_zfidt00326_h[ 1 ] ).

      CATCH cx_root.
    ENDTRY.

    "*--------------------------------------------------------------------*

    "Get Detail ZFIDT00327
    SELECT *
      FROM zfidt00327
        INTO CORRESPONDING FIELDS OF TABLE p_git_lineitem
        WHERE bukrs  = p_gwa_another-bukrs AND
              gjahr = p_gwa_another-gjahr AND
              belnr = p_gwa_another-belnr.
    IF p_git_lineitem[] IS NOT INITIAL.

      LOOP AT p_git_lineitem ASSIGNING FIELD-SYMBOL(<lfs_lineitem>).

        TRY.
            <lfs_lineitem>-gjahr_posted = lit_zfidt00326[ kind = <lfs_lineitem>-kind ]-gjahr_posted.
            <lfs_lineitem>-belnr_posted = lit_zfidt00326[ kind = <lfs_lineitem>-kind ]-belnr_posted.

          CATCH cx_root.
        ENDTRY.

      ENDLOOP.

    ENDIF.

    gd_mode = 'D2'.

  ELSE.

    "ZFIMSG 144: No data found
    MESSAGE s144(zfimsg) DISPLAY LIKE 'W'.

    gd_mode = 'C'.

  ENDIF.

ENDFORM.
