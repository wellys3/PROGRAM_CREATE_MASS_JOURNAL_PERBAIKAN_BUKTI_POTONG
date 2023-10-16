*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_9100_O
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Module STATUS_9100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9100 OUTPUT.

  CLEAR git_exclude[] .

  "*--------------------------------------------------------------------*

  IF gd_mode IS INITIAL.
    gd_mode = 'C'.
  ENDIF.

  "*--------------------------------------------------------------------*

  CASE gd_mode.
    WHEN 'C'.
      SET PF-STATUS 'STATUS_9100'.
      SET TITLEBAR 'TITLE_MAIN' WITH 'Create'.

    WHEN 'D1'.

      "Exclude
      APPEND '&POST' TO git_exclude.
      APPEND '&CHECK' TO git_exclude.

      SET PF-STATUS 'STATUS_9100' EXCLUDING git_exclude.
      SET TITLEBAR 'TITLE_MAIN' WITH 'Display'.

    WHEN 'D2'.

      "Exclude
      APPEND '&POST' TO git_exclude.
      APPEND '&CHECK' TO git_exclude.
      APPEND '&MESSAGE' TO git_exclude.

      SET PF-STATUS 'STATUS_9100' EXCLUDING git_exclude.
      SET TITLEBAR 'TITLE_MAIN' WITH 'Display'.

  ENDCASE.

  PERFORM f_calc_sum.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_screen_9100 OUTPUT.

  CASE gd_mode.
    WHEN 'C'.

      LOOP AT SCREEN.

        CASE screen-group1.
          WHEN 'G1'.
            screen-required = '2'.
            MODIFY SCREEN.
        ENDCASE.

      ENDLOOP.

    WHEN 'D1' OR 'D2'.

      LOOP AT SCREEN.

        CASE screen-group1.
          WHEN 'G1'.
            screen-input = '0'.
            MODIFY SCREEN.
        ENDCASE.

      ENDLOOP.

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Module SET_CURSOR OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_cursor OUTPUT.

  IF gd_cursor_name IS NOT INITIAL AND gd_cursor_line IS INITIAL.

    SET CURSOR FIELD gd_cursor_name.

  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Module SET_CURSOR OUTPUT_B
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_cursor_b OUTPUT.

  IF gd_cursor_name IS NOT INITIAL AND gd_cursor_line IS NOT INITIAL.
    SET CURSOR FIELD gd_cursor_name LINE gd_cursor_line.
  ENDIF.

ENDMODULE.
