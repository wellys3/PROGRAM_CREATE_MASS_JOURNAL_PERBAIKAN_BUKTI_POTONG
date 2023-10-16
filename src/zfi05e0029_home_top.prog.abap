*&---------------------------------------------------------------------*
*& Include          ZFI02R0033_HOME_TOP
*&---------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Variable                                                    *
*--------------------------------------------------------------------*
*---Variable Program - Single Value
DATA: gd_rb         TYPE char20,
      gd_line_excel TYPE i,
      gd_tabix      TYPE i,
      gd_subrc      TYPE sy-subrc,
      gd_message    TYPE text255,
      gd_answer(1). "Variable for Popup Answer.
*--------------------------------------------------------------------*
* End - Global Variable                                              *
*--------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Selection Screen                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a04 WITH FRAME TITLE text904.
SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS rb1 RADIOBUTTON GROUP rb1 USER-COMMAND rad1 DEFAULT 'X' modif id p2.
PARAMETERS rb1 RADIOBUTTON GROUP rb1 DEFAULT 'X' MODIF ID p2.
SELECTION-SCREEN COMMENT 4(79) text801.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 4(79) text802  MODIF ID p2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb3 RADIOBUTTON GROUP rb1  MODIF ID p2.
SELECTION-SCREEN COMMENT 4(79) text803.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb4 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 4(79) text804.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb5 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 4(79) text805.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a04.
*----------------------------------------------------------------------*
* End - Selection Screen                                               *
*----------------------------------------------------------------------*
