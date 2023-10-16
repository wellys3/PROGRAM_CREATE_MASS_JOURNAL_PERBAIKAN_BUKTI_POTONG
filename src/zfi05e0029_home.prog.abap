*&---------------------------------------------------------------------*
*& Program ZFI05E0029_HOME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Description : Create Mass Journal Perbaikan Bukti Potong - Home
*&
*& Module      : Financial Accounting
*& Functional  : - Yeremia Khristian Suherman (yeremia.suherman@equine.co.id)
*&               - Melia Suryani (melia.suryani@equine.co.id)
*& FSD Loc.    : - SO2_MIME_REPOSITORY --> SAP --> PUBLIC --> ZFSD
*& FSD         : - 0022.10. ADMF-EQG.P2207.0047-FSD-FI-CR27C-Create Mass Journal Perbaikan Bukti Potong v1.2_20230822
*& Developer   : Welly Sugiarto (welly.sugiarto@equine.co.id)
*& Date        : September 21st, 2023
*& Copyright   : © 2023 PT Equine Global
*&               © 2023 PT Adira Dinamika Multi Finance
*&
*& Transport Request History (Any changes of TR will be updated here future):
*& *  A4DK909100 SAPABAP CR27C - Create Mass Jour. Perbaikan BukPot WSU MSU #1
*& *  Changelog: * Initial Release
*&---------------------------------------------------------------------*


PROGRAM zfi05e0029_home.


*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*
INCLUDE zfi05e0029_home_top.   ""Types, Data, Constant Declaration & Selection-Screen
INCLUDE zfi05e0029_home_f00.   "Other Function for whole this program
INCLUDE zfi05e0029_home_f01.   "Get Data
*----------------------------------------------------------------------*
* End - Includes                                                       *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_initialization.
*----------------------------------------------------------------------*
* End - Initialization                                                 *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR gd_subrc.
  PERFORM f_check_auth CHANGING gd_subrc.

  CHECK gd_subrc EQ 0.

  PERFORM f_execute.

END-OF-SELECTION.
*----------------------------------------------------------------------*
* End - Start-of-Selection                                             *
