*&---------------------------------------------------------------------*
*& Report ZFI05E0029
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Description : Create Mass Journal Perbaikan Bukti Potong
*&
*& Module      : Financial Accounting
*& Functional  : - Yeremia Khristian Suherman (yeremia.suherman@equine.co.id)
*&               - Melia Suryani (melia.suryani@equine.co.id)
*& FSD Loc.    : - SO2_MIME_REPOSITORY --> SAP --> PUBLIC --> ZFSD
*& FSD         : - 0022.10. ADMF-EQG.P2207.0047-FSD-FI-CR27C-Create Mass Journal Perbaikan Bukti Potong v1.2_20230822
*& Developer   : Welly Sugiarto (welly.sugiarto@equine.co.id)
*& Date        : August 23rd, 2023
*& Copyright   : © 2023 PT Equine Global
*&               © 2023 PT Adira Dinamika Multi Finance
*&
*& Transport Request History (Any changes of TR will be updated here future):
*& *  A4DK909100 SAPABAP CR27C - Create Mass Jour. Perbaikan BukPot WSU MSU #1
*& *  Changelog: * Initial Release
*&---------------------------------------------------------------------*


PROGRAM zfi05e0029.


*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*
INCLUDE: zfi05e0029_top, "Types, Data, Constant Declaration & Selection-Screen.

         zfi05e0029_f00, "Other Function for whole this program
         zfi05e0029_f01, "Get Data
         zfi05e0029_f02, "Process Data
         ZFI05E0029_f03, "Display Data

         zfi05e0029_9100_o, "9100 PBO
         zfi05e0029_9100_i, "9100 PBI

         zfi05e0029_9101_o, "9101 PBO
         zfi05e0029_9101_i, "9101 PBI
         zfi05e0029_9101_s, "9101 SubRoutine

         zfi05e0029_9200_o, "9200 PBO
         zfi05e0029_9200_i. "9200 PBI

START-OF-SELECTION.

  CALL SCREEN '9100'.

*----------------------------------------------------------------------*
* End - Includes                                                       *
*----------------------------------------------------------------------*
