*&---------------------------------------------------------------------*
*& Include          ZFI02R0033_HOME_F00
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_initialization .

  PERFORM set_text .

ENDFORM.


*&---------------------------------------------------------------------*
*& Form SET_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_text .

  text904 = 'Selection Area'.

  text801 = 'Table Maintain Create Mass Journal Perbaikan BukPot (ZFIDT00322)'.
  text802 = 'Table Trans. Log Prog. Create Mass Journal Perbaikan BukPot (ZFIDT00325)'.
  text803 = 'Table Trans. Result Post. Mass Journal Perbaikan BukPot - Header (ZFIDT00326)'.
  text804 = 'Table Trans. Result Post. Mass Journal Perbaikan BukPot - Detail (ZFIDT00327)'.
  text805 = 'Program Create Mass Journal Perbaikan Bukti Potong'.

ENDFORM.
