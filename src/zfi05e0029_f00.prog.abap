*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_F00
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LD_ANSWER  text
*&---------------------------------------------------------------------*
FORM f_confirm USING p_word1
                     p_word2
                     p_button1
                     p_button2
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = p_word1
      text_question         = p_word2
      text_button_1         = p_button1
      text_button_2         = p_button2
      display_cancel_button = 'X'
    IMPORTING
      answer                = p_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    "Do nothing
  ENDIF.

ENDFORM.


FORM f_convert_amount  USING p_kind
                             p_currency
                             p_amount
                    CHANGING p_output.
  DATA: ld_io  TYPE bapicurr-bapicurr.
  CASE p_kind.
    WHEN 'TO_INTERNAL'.
      ld_io = p_amount.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
        EXPORTING
          currency             = p_currency
          amount_external      = ld_io
          max_number_of_digits = 23
        IMPORTING
          amount_internal      = p_output.
    WHEN 'TO_EXTERNAL'.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_currency
          amount_internal = p_amount
        IMPORTING
          amount_external = ld_io.
      IF sy-subrc EQ 0.
        p_output = ld_io.
      ENDIF.
  ENDCASE.
ENDFORM.
