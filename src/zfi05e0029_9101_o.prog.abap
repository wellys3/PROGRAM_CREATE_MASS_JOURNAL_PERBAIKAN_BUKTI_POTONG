*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_9101_O
*&---------------------------------------------------------------------*


*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_LINEITEM'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_lineitem_change_tc_attr OUTPUT.
  DESCRIBE TABLE git_lineitem LINES tc_lineitem-lines.

*--------------------------------------------------------------------*

  IF git_lineitem[] IS INITIAL.

    tc_lineitem-line_sel_mode = 0. "0 no selection | 1 single selection | 2 multiple selection

  ELSE.

    tc_lineitem-line_sel_mode = 2. "0 no selection | 1 single selection | 2 multiple selection

  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Module TC_LINEITEM_CHANGE_COL_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE tc_lineitem_change_col_attr OUTPUT.

  IF git_lineitem[] IS INITIAL.

    LOOP AT tc_lineitem-cols INTO gwa_cols.
      gwa_cols-screen-input = 0.

      CASE gd_mode.
        WHEN 'C'.

          IF gwa_cols-screen-name EQ 'GIT_LINEITEM-GJAHR_POSTED' OR
             gwa_cols-screen-name EQ 'GIT_LINEITEM-BELNR_POSTED'.
            gwa_cols-invisible = '1'.
          ENDIF.

      ENDCASE.

      MODIFY tc_lineitem-cols FROM gwa_cols.
    ENDLOOP.

  ELSE.

    LOOP AT tc_lineitem-cols INTO gwa_cols.
      gwa_cols-screen-input = 0.

      CASE gd_mode.
        WHEN 'D1' OR 'D2'.

          IF gwa_cols-screen-name EQ 'GIT_LINEITEM-GJAHR_POSTED' OR
             gwa_cols-screen-name EQ 'GIT_LINEITEM-BELNR_POSTED'.
            gwa_cols-invisible = '0'.
          ENDIF.

      ENDCASE.

      MODIFY tc_lineitem-cols FROM gwa_cols.
    ENDLOOP.

  ENDIF.

ENDMODULE.


*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_LINEITEM'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_lineitem_get_lines OUTPUT.
  gd_tc_lineitem_lines = sy-loopc.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN_9101 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_screen_9101 OUTPUT.

  CASE gd_mode.
    WHEN 'C'.

      CASE gd_flag.
        WHEN 'RE' OR 'NOT_RE'.

          LOOP AT SCREEN.

            CASE screen-group1.
*          WHEN 'MOD' OR 'PAG' OR 'MAR'.
              WHEN 'MOD'.
                screen-input = '1'.
                MODIFY SCREEN.
            ENDCASE.

          ENDLOOP.

        WHEN 'PPH26'.

          CASE gwa_zfidt00322-sindikasi.
            WHEN 'X'.

              LOOP AT SCREEN.

                CASE screen-group1.
*          WHEN 'MOD' OR 'PAG' OR 'MAR'.
                  WHEN 'MOD'.
                    screen-input = '0'.
                    MODIFY SCREEN.
                ENDCASE.

              ENDLOOP.

            WHEN ''.

              LOOP AT SCREEN.

                CASE screen-group1.
*          WHEN 'MOD' OR 'PAG' OR 'MAR'.
                  WHEN 'MOD'.
                    screen-input = '1'.
                    MODIFY SCREEN.
                ENDCASE.

              ENDLOOP.

          ENDCASE.

      ENDCASE.

    WHEN 'D1' OR 'D2'.

      LOOP AT SCREEN.

        CASE screen-group1.
*          WHEN 'MOD' OR 'PAG' OR 'MAR'.
          WHEN 'MOD'.
            screen-input = '0'.
            MODIFY SCREEN.
        ENDCASE.

      ENDLOOP.

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Module TC_LINEITEM_CHANGE_FIELD_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE tc_lineitem_change_field_attr OUTPUT.

  IF git_lineitem-kind EQ gc_fl_posting. "OR
*     git_lineitem-kind EQ gc_fl_posting_1 OR
*     git_lineitem-kind EQ gc_fl_posting_2 OR
*     git_lineitem-kind EQ gc_fl_posting_reverse OR
*     git_lineitem-kind EQ gc_fl_posting_reverse_26.

*if git_lineitem[] is not initial.

    LOOP AT SCREEN.

      CASE gd_mode.
        WHEN 'C'.

          IF git_lineitem-matnr IS INITIAL AND
             git_lineitem-lifnr IS INITIAL.

            IF screen-name = 'GIT_LINEITEM-MATNR' OR
               screen-name = 'GIT_LINEITEM-LIFNR'.

              CLEAR: git_lineitem-matnr,
                     git_lineitem-maktx,
                     git_lineitem-racct,
                     git_lineitem-racct_desc,
                     git_lineitem-racct_desc,
                     git_lineitem-lifnr,
                     git_lineitem-lifnr_name,
                     git_lineitem-amount,
                     git_lineitem-mwskz,
                     git_lineitem-witht,
                     git_lineitem-withcd,
                     git_lineitem-qsshh,
                     git_lineitem-qsshb,
                     git_lineitem-qbshb,
                     git_lineitem-prctr,
                     git_lineitem-kostl,
                     git_lineitem-sgtxt,
                     git_lineitem-kind,
                     git_lineitem-koart,
                     git_lineitem-gjahr_posted,
                     git_lineitem-belnr_posted,
                     git_lineitem-flag_bapi,
                     git_lineitem-kschl,
                     git_lineitem-kvsl1,
                     git_lineitem-kbetr,
                     git_lineitem-ktosl.

              screen-input = '1'.
              screen-required = '2'.
              MODIFY SCREEN.

            ENDIF.

          ELSEIF git_lineitem-matnr IS NOT INITIAL AND
                 git_lineitem-lifnr IS INITIAL.

            IF screen-name = 'GIT_LINEITEM-MATNR'.
              screen-input = '1'.
              screen-required = '2'.
              MODIFY SCREEN.
            ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
                   screen-name = 'GIT_LINEITEM-MWSKZ' OR
                   screen-name = 'GIT_LINEITEM-KOSTL' OR
                   screen-name = 'GIT_LINEITEM-SGTXT'.
              screen-input = '1'.
              MODIFY SCREEN.
            ELSEIF screen-name = 'GIT_LINEITEM-LIFNR'.
              screen-input = '0'.
            ENDIF.

          ELSEIF git_lineitem-matnr IS INITIAL AND
                 git_lineitem-lifnr IS NOT INITIAL.

            IF screen-name = 'GIT_LINEITEM-LIFNR'.
              screen-input = '1'.
              screen-required = '2'.
              MODIFY SCREEN.
            ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
                   screen-name = 'GIT_LINEITEM-MWSKZ' OR
                   screen-name = 'GIT_LINEITEM-KOSTL' OR
                   screen-name = 'GIT_LINEITEM-SGTXT'.
              screen-input = '1'.
              MODIFY SCREEN.
            ELSEIF screen-name = 'GIT_LINEITEM-MATNR'.
              screen-input = '0'.
            ENDIF.

          ELSEIF git_lineitem-matnr IS NOT INITIAL AND
                 git_lineitem-lifnr IS NOT INITIAL.

            IF screen-name = 'GIT_LINEITEM-MATNR' OR
               screen-name = 'GIT_LINEITEM-LIFNR'.

              screen-input = '1'.
              screen-required = '2'.
              MODIFY SCREEN.

            ENDIF.

          ENDIF.

          "*--------------------------------------------------------------------*

*****          CASE gd_flag.
*****            WHEN 'RE' OR 'NOT_RE'.
*****
*****            WHEN 'PPH26'.
*****
*****              CASE gwa_zfidt00322-sindikasi.
*****                WHEN 'X'.
*****
*****                WHEN ''.
*****
*****                  IF git_lineitem-kind EQ gc_fl_posting_1.
*****
*****                    IF git_lineitem-matnr IS INITIAL AND
*****                       git_lineitem-lifnr IS INITIAL.
*****
*****                      IF screen-name = 'GIT_LINEITEM-MATNR' OR
*****                         screen-name = 'GIT_LINEITEM-LIFNR'.
*****
*****                        CLEAR: git_lineitem-matnr,
*****                               git_lineitem-maktx,
*****                               git_lineitem-racct,
*****                               git_lineitem-racct_desc,
*****                               git_lineitem-racct_desc,
*****                               git_lineitem-lifnr,
*****                               git_lineitem-lifnr_name,
*****                               git_lineitem-amount,
*****                               git_lineitem-mwskz,
*****                               git_lineitem-witht,
*****                               git_lineitem-withcd,
*****                               git_lineitem-qsshh,
*****                               git_lineitem-qsshb,
*****                               git_lineitem-qbshb,
*****                               git_lineitem-prctr,
*****                               git_lineitem-kostl,
*****                               git_lineitem-sgtxt,
*****                               git_lineitem-kind,
*****                               git_lineitem-koart,
*****                               git_lineitem-gjahr_posted,
*****                               git_lineitem-belnr_posted,
*****                               git_lineitem-flag_bapi,
*****                               git_lineitem-kschl,
*****                               git_lineitem-kvsl1,
*****                               git_lineitem-kbetr,
*****                               git_lineitem-ktosl.
*****
*****                        screen-input = '1'.
*****                        screen-required = '2'.
*****                        MODIFY SCREEN.
*****
*****                      ENDIF.
*****
*****                    ELSEIF git_lineitem-matnr IS NOT INITIAL AND
*****                           git_lineitem-lifnr IS INITIAL.
*****
*****                      IF screen-name = 'GIT_LINEITEM-MATNR'.
*****                        screen-input = '1'.
*****                        screen-required = '2'.
*****                        MODIFY SCREEN.
*****                      ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
*****                             screen-name = 'GIT_LINEITEM-MWSKZ' OR
*****                             screen-name = 'GIT_LINEITEM-KOSTL' OR
*****                             screen-name = 'GIT_LINEITEM-SGTXT'.
*****                        screen-input = '1'.
*****                        MODIFY SCREEN.
*****                      ELSEIF screen-name = 'GIT_LINEITEM-LIFNR'.
*****                        screen-input = '0'.
*****                      ENDIF.
*****
*****                    ELSEIF git_lineitem-matnr IS INITIAL AND
*****                           git_lineitem-lifnr IS NOT INITIAL.
*****
*****                      IF screen-name = 'GIT_LINEITEM-LIFNR'.
*****                        screen-input = '1'.
*****                        screen-required = '2'.
*****                        MODIFY SCREEN.
*****                      ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
*****                             screen-name = 'GIT_LINEITEM-MWSKZ' OR
*****                             screen-name = 'GIT_LINEITEM-KOSTL' OR
*****                             screen-name = 'GIT_LINEITEM-SGTXT'.
*****                        screen-input = '1'.
*****                        MODIFY SCREEN.
*****                      ELSEIF screen-name = 'GIT_LINEITEM-MATNR'.
*****                        screen-input = '0'.
*****                      ENDIF.
*****
*****                    ELSEIF git_lineitem-matnr IS NOT INITIAL AND
*****                           git_lineitem-lifnr IS NOT INITIAL.
*****
*****                      IF screen-name = 'GIT_LINEITEM-MATNR' OR
*****                         screen-name = 'GIT_LINEITEM-LIFNR'.
*****
*****                        screen-input = '1'.
*****                        screen-required = '2'.
*****                        MODIFY SCREEN.
*****
*****                      ENDIF.
*****
*****                    ENDIF.
*****
*****                  ENDIF.
*****
*****              ENDCASE.
*****
*****          ENDCASE.

        WHEN 'D1' OR 'D2'.

          IF git_lineitem-matnr IS INITIAL AND
             git_lineitem-lifnr IS INITIAL.

            IF screen-name = 'GIT_LINEITEM-MATNR' OR
               screen-name = 'GIT_LINEITEM-LIFNR'.

              CLEAR: git_lineitem-matnr,
                     git_lineitem-maktx,
                     git_lineitem-racct,
                     git_lineitem-racct_desc,
                     git_lineitem-racct_desc,
                     git_lineitem-lifnr,
                     git_lineitem-lifnr_name,
                     git_lineitem-amount,
                     git_lineitem-mwskz,
                     git_lineitem-witht,
                     git_lineitem-withcd,
                     git_lineitem-qsshh,
                     git_lineitem-qsshb,
                     git_lineitem-qbshb,
                     git_lineitem-prctr,
                     git_lineitem-kostl,
                     git_lineitem-sgtxt,
                     git_lineitem-kind,
                     git_lineitem-koart,
                     git_lineitem-gjahr_posted,
                     git_lineitem-belnr_posted,
                     git_lineitem-flag_bapi,
                     git_lineitem-kschl,
                     git_lineitem-kvsl1,
                     git_lineitem-kbetr,
                     git_lineitem-ktosl.

              screen-input = '1'.
              screen-required = '2'.
              MODIFY SCREEN.

            ENDIF.

          ELSEIF git_lineitem-matnr IS NOT INITIAL AND
                 git_lineitem-lifnr IS INITIAL.

            IF screen-name = 'GIT_LINEITEM-MATNR'.
              screen-input = '0'.
              screen-required = '0'.
              MODIFY SCREEN.
            ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
                   screen-name = 'GIT_LINEITEM-MWSKZ' OR
                   screen-name = 'GIT_LINEITEM-KOSTL' OR
                   screen-name = 'GIT_LINEITEM-SGTXT'.
              screen-input = '0'.
              MODIFY SCREEN.
            ELSEIF screen-name = 'GIT_LINEITEM-LIFNR'.
              screen-input = '0'.
            ENDIF.

          ELSEIF git_lineitem-matnr IS INITIAL AND
                 git_lineitem-lifnr IS NOT INITIAL.

            IF screen-name = 'GIT_LINEITEM-LIFNR'.
              screen-input = '0'.
              screen-required = '0'.
              MODIFY SCREEN.
            ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
                   screen-name = 'GIT_LINEITEM-MWSKZ' OR
                   screen-name = 'GIT_LINEITEM-KOSTL' OR
                   screen-name = 'GIT_LINEITEM-SGTXT'.
              screen-input = '0'.
              MODIFY SCREEN.
            ELSEIF screen-name = 'GIT_LINEITEM-MATNR'.
              screen-input = 0.
            ENDIF.

          ELSEIF git_lineitem-matnr IS NOT INITIAL AND
                 git_lineitem-lifnr IS NOT INITIAL.

            IF screen-name = 'GIT_LINEITEM-MATNR' OR
               screen-name = 'GIT_LINEITEM-LIFNR'.

              screen-input = '0'.
              screen-required = '0'.
              MODIFY SCREEN.

            ENDIF.

          ENDIF.

      ENDCASE.

    ENDLOOP.

  ELSEIF git_lineitem-kind EQ gc_fl_posting_1.

    LOOP AT SCREEN.

      CASE gd_flag.
        WHEN 'RE' OR 'NOT_RE'.

        WHEN 'PPH26'.

          CASE gwa_zfidt00322-sindikasi.
            WHEN 'X'.

            WHEN ''.

*              IF git_lineitem-kind EQ gc_fl_posting_1.

              IF git_lineitem-matnr IS INITIAL AND
                 git_lineitem-lifnr IS INITIAL AND
                 git_lineitem-kind IS INITIAL.

                IF screen-name = 'GIT_LINEITEM-MATNR' OR
                   screen-name = 'GIT_LINEITEM-LIFNR'.

                  CLEAR: git_lineitem-matnr,
                         git_lineitem-maktx,
                         git_lineitem-racct,
                         git_lineitem-racct_desc,
                         git_lineitem-racct_desc,
                         git_lineitem-lifnr,
                         git_lineitem-lifnr_name,
                         git_lineitem-amount,
                         git_lineitem-mwskz,
                         git_lineitem-witht,
                         git_lineitem-withcd,
                         git_lineitem-qsshh,
                         git_lineitem-qsshb,
                         git_lineitem-qbshb,
                         git_lineitem-prctr,
                         git_lineitem-kostl,
                         git_lineitem-sgtxt,
                         git_lineitem-kind,
                         git_lineitem-koart,
                         git_lineitem-gjahr_posted,
                         git_lineitem-belnr_posted,
                         git_lineitem-flag_bapi,
                         git_lineitem-kschl,
                         git_lineitem-kvsl1,
                         git_lineitem-kbetr,
                         git_lineitem-ktosl.

                  screen-input = '1'.
                  screen-required = '2'.
                  MODIFY SCREEN.

                ENDIF.

              ELSEIF git_lineitem-matnr IS INITIAL AND
                 git_lineitem-lifnr IS INITIAL AND
                 git_lineitem-kind IS NOT INITIAL.

                IF screen-name = 'GIT_LINEITEM-MATNR' OR
                   screen-name = 'GIT_LINEITEM-LIFNR'.

*                  CLEAR: git_lineitem-matnr,
*                         git_lineitem-maktx,
*                         git_lineitem-racct,
*                         git_lineitem-racct_desc,
*                         git_lineitem-racct_desc,
*                         git_lineitem-lifnr,
*                         git_lineitem-lifnr_name,
*                         git_lineitem-amount,
*                         git_lineitem-mwskz,
*                         git_lineitem-witht,
*                         git_lineitem-withcd,
*                         git_lineitem-qsshh,
*                         git_lineitem-qsshb,
*                         git_lineitem-qbshb,
*                         git_lineitem-prctr,
*                         git_lineitem-kostl,
*                         git_lineitem-sgtxt,
*                         git_lineitem-kind,
*                         git_lineitem-koart,
*                         git_lineitem-gjahr_posted,
*                         git_lineitem-belnr_posted,
*                         git_lineitem-flag_bapi,
*                         git_lineitem-kschl,
*                         git_lineitem-kvsl1,
*                         git_lineitem-kbetr,
*                         git_lineitem-ktosl.

*                  screen-input = '1'.
                  screen-input = '0'.
*                  screen-required = '2'.
                  MODIFY SCREEN.

                ENDIF.

              ELSEIF git_lineitem-matnr IS NOT INITIAL AND
                     git_lineitem-lifnr IS INITIAL AND
                     git_lineitem-kind IS INITIAL.

                IF screen-name = 'GIT_LINEITEM-MATNR'.
                  screen-input = '1'.
                  screen-required = '2'.
                  MODIFY SCREEN.
                ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
                       screen-name = 'GIT_LINEITEM-MWSKZ' OR
                       screen-name = 'GIT_LINEITEM-KOSTL' OR
                       screen-name = 'GIT_LINEITEM-SGTXT'.
                  screen-input = '1'.
                  MODIFY SCREEN.
                ELSEIF screen-name = 'GIT_LINEITEM-LIFNR'.
                  screen-input = '0'.
                ENDIF.

              ELSEIF git_lineitem-matnr IS NOT INITIAL AND
                     git_lineitem-lifnr IS INITIAL AND
                     git_lineitem-kind IS NOT INITIAL.

                IF screen-name = 'GIT_LINEITEM-MATNR'.
                  screen-input = '1'.
*                  screen-required = '2'.
                  MODIFY SCREEN.
                ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
                       screen-name = 'GIT_LINEITEM-MWSKZ' OR
                       screen-name = 'GIT_LINEITEM-KOSTL' OR
                       screen-name = 'GIT_LINEITEM-SGTXT'.
                  screen-input = '1'.
                  MODIFY SCREEN.
                ELSEIF screen-name = 'GIT_LINEITEM-LIFNR'.
                  screen-input = '0'.
                ENDIF.

              ELSEIF git_lineitem-matnr IS INITIAL AND
                     git_lineitem-lifnr IS NOT INITIAL AND
                     git_lineitem-kind IS INITIAL.

                IF screen-name = 'GIT_LINEITEM-LIFNR'.
                  screen-input = '1'.
                  screen-required = '2'.
                  MODIFY SCREEN.
                ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
                       screen-name = 'GIT_LINEITEM-MWSKZ' OR
                       screen-name = 'GIT_LINEITEM-KOSTL' OR
                       screen-name = 'GIT_LINEITEM-SGTXT'.
                  screen-input = '1'.
                  MODIFY SCREEN.
                ELSEIF screen-name = 'GIT_LINEITEM-MATNR'.
                  screen-input = '0'.
                ENDIF.

              ELSEIF git_lineitem-matnr IS INITIAL AND
                     git_lineitem-lifnr IS NOT INITIAL AND
                     git_lineitem-kind IS NOT INITIAL.

                IF screen-name = 'GIT_LINEITEM-LIFNR'.
                  screen-input = '1'.
*                  screen-required = '2'.
                  MODIFY SCREEN.
                ELSEIF screen-name = 'GIT_LINEITEM-AMOUNT' OR
                       screen-name = 'GIT_LINEITEM-MWSKZ' OR
                       screen-name = 'GIT_LINEITEM-KOSTL' OR
                       screen-name = 'GIT_LINEITEM-SGTXT'.
                  screen-input = '1'.
                  MODIFY SCREEN.
                ELSEIF screen-name = 'GIT_LINEITEM-MATNR'.
                  screen-input = '0'.
                ENDIF.

              ELSEIF git_lineitem-matnr IS NOT INITIAL AND
                     git_lineitem-lifnr IS NOT INITIAL AND
                     git_lineitem-kind IS INITIAL.

                IF screen-name = 'GIT_LINEITEM-MATNR' OR
                   screen-name = 'GIT_LINEITEM-LIFNR'.

                  screen-input = '1'.
                  screen-required = '2'.
                  MODIFY SCREEN.

                ENDIF.

              ELSEIF git_lineitem-matnr IS NOT INITIAL AND
                     git_lineitem-lifnr IS NOT INITIAL AND
                     git_lineitem-kind IS NOT INITIAL.

                IF screen-name = 'GIT_LINEITEM-MATNR' OR
                   screen-name = 'GIT_LINEITEM-LIFNR'.

                  screen-input = '1'.
*                  screen-required = '2'.
                  MODIFY SCREEN.

                ENDIF.

              ENDIF.

*              ENDIF.

          ENDCASE.

      ENDCASE.

    ENDLOOP.

  ENDIF.

ENDMODULE.
