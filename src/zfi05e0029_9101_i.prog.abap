*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_9101_I
*&---------------------------------------------------------------------*


*&SPWIZARD: INPUT MODULE FOR TC 'TC_LINEITEM'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc_lineitem_modify INPUT.
  MODIFY git_lineitem
    FROM git_lineitem
    INDEX tc_lineitem-current_line.
ENDMODULE.


*&SPWIZARD: INPUT MODUL FOR TC 'TC_LINEITEM'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_lineitem_mark INPUT.
  DATA: g_tc_lineitem_wa2 LIKE LINE OF git_lineitem.
  IF tc_lineitem-line_sel_mode = 1
  AND git_lineitem-sel = 'X'.
    LOOP AT git_lineitem INTO g_tc_lineitem_wa2
      WHERE sel = 'X'.
      g_tc_lineitem_wa2-sel = ''.
      MODIFY git_lineitem
        FROM g_tc_lineitem_wa2
        TRANSPORTING sel.
    ENDLOOP.
  ENDIF.
  MODIFY git_lineitem
    FROM git_lineitem
    INDEX tc_lineitem-current_line
    TRANSPORTING sel.
ENDMODULE.


*&SPWIZARD: INPUT MODULE FOR TC 'TC_LINEITEM'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_lineitem_user_command INPUT.
  gd_ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_LINEITEM'
                              'GIT_LINEITEM'
                              'SEL'
                     CHANGING gd_ok_code.
  sy-ucomm = gd_ok_code.
ENDMODULE.
