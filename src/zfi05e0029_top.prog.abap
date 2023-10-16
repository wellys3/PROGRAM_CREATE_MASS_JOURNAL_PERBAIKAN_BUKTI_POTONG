*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_TOP
*&---------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Type-Pools                                                         *
*--------------------------------------------------------------------*
*TYPE-POOLS: icon, truxs, col, fiehc.
*--------------------------------------------------------------------*
* End - Type-Pools                                                   *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Nodes                                                              *
*--------------------------------------------------------------------*
*NODES: peras.
*--------------------------------------------------------------------*
* End - Nodes                                                        *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Infotype
*--------------------------------------------------------------------*
*INFOTYPES: 0000, 0001, 2006 MODE N.
*INFOTYPES: 0000, 0001, 2006.
*--------------------------------------------------------------------*
* End Infotype
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Tables                                                             *
*--------------------------------------------------------------------*
TABLES: zfist00194.
*--------------------------------------------------------------------*
* End - Tables                                                       *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Constants                                                   *
*--------------------------------------------------------------------*
CONSTANTS: gc_sgtxt_reverse         TYPE bseg-sgtxt VALUE 'RECLASS MANUAL ADJUSTMENT PAJAK',
           gc_sgtxt_posting         TYPE bseg-sgtxt VALUE 'POSTING MANUAL ADJUSTMENT PAJAK',
           gc_bktxt                 TYPE bkpf-bktxt VALUE 'KOREKSI MANUAL',

           gc_fl_posting_reverse    TYPE zfist00194-kind VALUE 'REVERSE',
           gc_fl_posting_reverse_26 TYPE zfist00194-kind VALUE 'REVERSE_26', "REVERSE_PPH26
           gc_fl_posting            TYPE zfist00194-kind VALUE 'POSTING',
           gc_fl_posting_1          TYPE zfist00194-kind VALUE 'POSTING_1',
           gc_fl_posting_2          TYPE zfist00194-kind VALUE 'POSTING_2',
           gc_fl_log                TYPE zfist00194-kind VALUE 'LOG',

           gc_title_message         TYPE lvc_title VALUE 'Message Information',
           gc_stvarv_name_adj_pph26 TYPE tvarvc-name VALUE 'ZFI05E0029_RACCT_ADJ_PPH26',
           gc_racct_utang_pph26     TYPE tvarvc-name VALUE '0000241005'.
*--------------------------------------------------------------------*
* End - Global Constants                                             *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Types                                                       *
*--------------------------------------------------------------------*
*Custom

TYPES: BEGIN OF gty_rseg,
         belnr TYPE rseg-belnr,
         gjahr TYPE rseg-gjahr,
         ebeln TYPE rseg-ebeln,
         ebelp TYPE rseg-ebelp,
         buzei TYPE rseg-buzei,
       END OF gty_rseg,

       BEGIN OF gty_zfidt00002,
         zvtc TYPE zfidt00002-zvtc,
         ztrf TYPE zfidt00002-ztrf,
       END OF gty_zfidt00002,

       BEGIN OF gty_zfidt00013,
         bklas TYPE zfidt00013-bklas,
         ztrf  TYPE zfidt00013-ztrf,
       END OF gty_zfidt00013,

       BEGIN OF gty_withitem,
         bukrs     TYPE with_item-bukrs,
         belnr     TYPE with_item-belnr,
         gjahr     TYPE with_item-gjahr,
         witht     TYPE with_item-witht,
         wt_withcd TYPE with_item-wt_withcd,
         wt_qsshh  TYPE with_item-wt_qsshh,
         wt_qsshb  TYPE with_item-wt_qsshb,
         wt_qbshh  TYPE with_item-wt_qbshh,
         wt_qbshb  TYPE with_item-wt_qbshb,
         koart     TYPE with_item-koart,
         wt_acco   TYPE with_item-wt_acco,
       END OF gty_withitem.

TYPES: BEGIN OF gty_bseg.
    INCLUDE TYPE zficd_zfi05e0029_bseg.
*         include TYPE ZFIVT00119.
TYPES: witht  TYPE zfist00194-witht,
       withcd TYPE zfist00194-withcd.
TYPES: END OF gty_bseg.

*TYPES: BEGIN OF gty_message,
*         no      TYPE zfist00194-no,
*         type    TYPE bapiret2-type,
*         id      TYPE bapiret2-id,
*         number  TYPE bapiret2-number,
*         message TYPE bapiret2-message.
*TYPES: END OF gty_message.

TYPES: BEGIN OF gty_message,
         no         TYPE zfist00194-zno,

         type       TYPE bapiret2-type,
         id         TYPE bapiret2-id,
         number     TYPE bapiret2-number,
         message    TYPE bapiret2-message,

         message_v1 TYPE bapiret2-message_v1,
         message_v2 TYPE bapiret2-message_v2,
         message_v3 TYPE bapiret2-message_v3,
         message_v4 TYPE bapiret2-message_v4,

         lineno     LIKE mesg-zeile.
TYPES: END OF gty_message.

TYPES: BEGIN OF gty_witht_withcd,
         lifnr  TYPE zfidt00322-lifnr,
         matnr  TYPE zfidt00322-matnr,
         witht  TYPE with_item-witht,
         withcd TYPE with_item-wt_withcd,
       END OF gty_witht_withcd.

TYPES: gtt_whtmap     TYPE TABLE OF zfidt00005,
       gtt_zfidt00002 TYPE TABLE OF gty_zfidt00002,
       gtt_zfidt00013 TYPE TABLE OF gty_zfidt00013,
       gtt_bseg       TYPE TABLE OF gty_bseg,
       gtt_bseg_2     TYPE TABLE OF zficd_zfi05e0029_bseg,
       gtt_glmap      TYPE TABLE OF zmmdt00010,
       gtt_return     TYPE TABLE OF bapiret2,
       gtt_zfidt00322 TYPE TABLE OF zfidt00322,
       gtt_message    TYPE TABLE OF gty_message,
       gtt_zfidt00326 TYPE TABLE OF zfidt00326.

*--------------------------------------------------------------------*
*Standard
*TYPES: BEGIN OF gty_named_dref,
*         name TYPE string,
*         dref TYPE REF TO data,
*       END OF gty_named_dref.
*--------------------------------------------------------------------*
* End - Global Types                                                 *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Variable                                                    *
*--------------------------------------------------------------------*
*Custom

**---Variable Program - Table & Work Area
DATA: gwa_header     TYPE zfist00193,
      gwa_another    TYPE zfist00198,
      git_lineitem   TYPE TABLE OF zfist00194 WITH HEADER LINE,
*      git_glmap      TYPE TABLE OF zmmdt00010,
      git_whtmap     TYPE TABLE OF zfidt00005,
      git_zfidt00002 TYPE gtt_zfidt00002,
      git_zfidt00013 TYPE gtt_zfidt00013,
      git_bseg       TYPE TABLE OF gty_bseg,
      git_bseg_2     TYPE TABLE OF zficd_zfi05e0029_bseg,
      git_withitem   TYPE TABLE OF gty_withitem,
      git_rseg       TYPE TABLE OF gty_rseg,
      git_ekbe       TYPE TABLE OF zficd_zfi05e0029_ekbe,
      git_zfidt00322 TYPE TABLE OF zfidt00322,
      gwa_zfidt00322 TYPE zfidt00322,
      git_message    TYPE TABLE OF gty_message,
      git_zfidt00326 TYPE TABLE OF zfidt00326.

DATA: gd_racct_adj_pph26 TYPE acdoca-racct,
      gd_flag      TYPE char10.  "RE | NOT_RE | PPH26

**---Screen
*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_LINEITEM' ITSELF
CONTROLS: tc_lineitem TYPE TABLEVIEW USING SCREEN 9101.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_LINEITEM'
DATA: gd_tc_lineitem_lines LIKE sy-loopc,
*      gd_mode              TYPE char2 VALUE 'C', "C = Create | D1 = Display with Message | D2 = Display without Message
      gd_mode              TYPE char2, "C = Create | D1 = Display with Message | D2 = Display without Message
      gd_ok_code           TYPE sy-ucomm,
      gd_cursor_name(20)   TYPE c,
      gd_checked           TYPE boolean,
      gd_bktxt_old         TYPE bkpf-bktxt,
      gd_cursor_line       TYPE sy-tabix,

      gwa_cols             TYPE cxtab_column,          "column of table control
*      gwa_lines             TYPE cxtab_,          "column of table control
      git_exclude           TYPE TABLE OF sy-ucomm.
**---End Screen

*--------------------------------------------------------------------*
*Standard

*---Variable Program - Single Value
DATA: gd_rb         TYPE char20,
      gd_line_excel TYPE i,
      gd_tabix      TYPE i,
      gd_subrc      TYPE sy-subrc,
      gd_message    TYPE text255,
      gd_times      TYPE i,
      gd_answer(1). "Variable for Popup Answer.

DATA: git_return_tab      TYPE TABLE OF ddshretval WITH HEADER LINE,
*DATA: git_return_tab      TYPE TABLE OF ddshretval ,
      git_dynpfld_mapping TYPE TABLE OF dselc WITH HEADER LINE.

**---For AMDP Class
*DATA: gd_where          TYPE sxmsbody,
*      gd_where1         TYPE sxmsbody,
*      gd_where2         TYPE sxmsbody,
*      gd_where3         TYPE sxmsbody,
*      gd_where4         TYPE sxmsbody,
*      gd_where5         TYPE sxmsbody,
*      git_named_seltabs TYPE TABLE OF gty_named_dref,
*      gwa_named_seltabs TYPE gty_named_dref.
*
**---For Refresh ALV
*DATA: gwa_stable     TYPE lvc_s_stbl,
*      gd_refreshmode TYPE salv_de_constant.
*
**---For Debugger
*DATA: git_terminal          TYPE TABLE OF tvarvc WITH HEADER LINE,
*      gd_opcode_usr_attr(1) TYPE x VALUE 5,
*      gd_terminal           TYPE usr41-terminal,
*      gd_zdebug             TYPE text255,
*      gd_flag               TYPE text255.
*
**---For Status Progress
*DATA: gd_percent TYPE i,
*      gd_lines   TYPE i.
*
**---Variable Get Execution Time
*DATA: gd_start TYPE p DECIMALS 3,
*      gd_stop  TYPE p DECIMALS 3,
*      gd_run   TYPE p DECIMALS 3.
*--------------------------------------------------------------------*
* End - Global Variable                                              *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Range                                                       *
*--------------------------------------------------------------------*
*Custom

*--------------------------------------------------------------------*

*Standard

*RANGES: gra_racct_adj_pph26 FOR acdoca-racct.
*        gra_kode_penghasilan FOR zfidt00314-zkode_penghasilan,
*        gra_uname FOR sy-uname.
*--------------------------------------------------------------------*
* End - Global Variable                                              *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Define                                                             *
*--------------------------------------------------------------------*
DEFINE f_fill_range.
  &1-sign = &2.
  &1-option = &3.
  &1-low = &4.
  &1-high = &5.
  APPEND &1.
END-OF-DEFINITION.

"Example: f_fill_range: lra_lptyp 'I' 'EQ' lwa_lptyp-lptyp ''.
*--------------------------------------------------------------------*
* End - Define                                                       *
*--------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Selection Screen                                                     *
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK a01 WITH FRAME TITLE text900.
*SELECT-OPTIONS: s_bukrs FOR zfivt00100-bukrs MEMORY ID zfi05r0021_bukrs NO-EXTENSION NO INTERVALS DEFAULT gc_rbukrs,
*                s_gjahr FOR zfivt00100-gjahr MEMORY ID zfi05r0021_gjahr NO-EXTENSION NO INTERVALS,
*                s_monat FOR zfivt00100-monat MEMORY ID zfi05r0021_monat NO-EXTENSION NO INTERVALS,
*                s_nobkpt FOR zfivt00100-zno_bukpot_internal MEMORY ID zfi05r0021_nobkpt MODIF ID p4.
*SELECTION-SCREEN END OF BLOCK a01.
*
**--------------------------------------------------------------------*
*
*SELECTION-SCREEN BEGIN OF BLOCK a05 WITH FRAME TITLE text600.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT (31) text601.
*PARAMETERS: c_sw_tab AS CHECKBOX USER-COMMAND u2. "MODIF ID p1. "Checkbox Switch Save
*SELECTION-SCREEN COMMENT 36(30) text602.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK a05.
*
**--------------------------------------------------------------------*
*
*SELECTION-SCREEN BEGIN OF BLOCK a04 WITH FRAME TITLE text700.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS rb1 RADIOBUTTON GROUP rb1 DEFAULT 'X' MODIF ID p2.
*SELECTION-SCREEN COMMENT 4(30) text701 MODIF ID p3.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS rb2 RADIOBUTTON GROUP rb1 MODIF ID p2.
*SELECTION-SCREEN COMMENT 4(30) text702 MODIF ID p3.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS rb3 RADIOBUTTON GROUP rb1  MODIF ID p2.
*SELECTION-SCREEN COMMENT 4(30) text703 MODIF ID p3.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK a04.
*----------------------------------------------------------------------*
* End - Selection Screen                                               *
*----------------------------------------------------------------------*
