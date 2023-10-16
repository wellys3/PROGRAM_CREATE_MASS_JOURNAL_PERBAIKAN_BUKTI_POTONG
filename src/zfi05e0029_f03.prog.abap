*&---------------------------------------------------------------------*
*& Include          ZFI05E0029_F03
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_message USING p_git_message TYPE gtt_message.

  "FALV creation with only table passed
*  DATA lcl_falv TYPE REF TO gcl_falv.
  DATA(lcl_falv) = zcl_falv=>create( EXPORTING i_popup = abap_true
                                     CHANGING ct_table = p_git_message ).

  "Creation of falv with local redefinition
*  DATA(lcl_falv) = zcl_falv=>create( CHANGING ct_table = p_git_message ).

  "Add title variable
  lcl_falv->title_v1 = gc_title_message.

*  "Set checkbox
*  lcl_falv->set_mark_field( 'SELECT' ).
*  lcl_falv->column( 'SELECT' )->set_edit( abap_true ). "Set checkbox editable
*  lcl_falv->column( 'SELECT' )->set_col_opt( iv_value = 'X' ). "Set column optimization

*  "Set hotspot
*  lcl_falv->column( 'BELNR' )->set_hotspot( 'X' ). "Set hotspot
*  lcl_falv->column( 'MBLNR' )->set_hotspot( 'X' ). "Set hotspot
*  lcl_falv->column( 'MATNR' )->set_hotspot( 'X' ). "Set hotspot
*  lcl_falv->column( 'BELNR_PPH' )->set_hotspot( 'X' ). "Set hotspot
*  lcl_falv->column( 'BELNR_RECLASS' )->set_hotspot( 'X' ). "Set hotspot

  "Set layout
  lcl_falv->layout->set_zebra( iv_value = 'X' ). "Set zebra
  lcl_falv->layout->set_col_opt( iv_value = 'X' ).
  lcl_falv->layout->set_cwidth_opt( iv_value = 'X' ).
  lcl_falv->layout->set_totals_bef( 'X' ). "Set sum on Top
  lcl_falv->layout->set_sel_mode( 'A' ).

  "Set Gui status to fully dynamic (no standard buttons of ALV Grid)
  "lcl_falv->gui_status->fully_dynamic = abap_true.

  "Modify field
  PERFORM f_modify_field USING lcl_falv.

*  "Add button
*  PERFORM f_add_button USING lcl_falv.

*  "Change grid to edit mode
*  lcl_falv->set_editable( iv_modify = abap_true ).

*  "Set size top of page
*  lcl_falv->top_of_page_height = 75.

  "Display full screen grid
*  lcl_falv->show_top_of_page( )->display( ).
*  lcl_falv->display( ).
  lcl_falv->display(
                     iv_start_row = 10
                     iv_start_column = 25
                     iv_end_row = 20
                     iv_end_column = 175
   ).

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_MODIFY_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> FALV
*&---------------------------------------------------------------------*
*FORM f_modify_field USING p_me TYPE REF TO gcl_falv.
FORM f_modify_field USING p_me TYPE REF TO zcl_falv.

*Reference
*  p_me->column( 'XXX' )->set_no_out( 'X' ). "Hide or not column
*  p_me->column( 'XXX' )->set_key( '' ). "Remove or not key column form dictionary
*  p_me->column( 'XXX' )->set_fix_column( iv_value = '' ). "Remove or not fix column
*  p_me->column( 'XXX' )->set_col_pos( '01' ). "Set column position with specific sort number
*  p_me->column( 'XXX' )->set_cfieldname( 'YYY' ). "Set this column to refer with currency field
*  p_me->column( 'XXX' )->set_qfieldname( 'YYY' ). "Set this column to refer with quantity field
*  p_me->column( 'XXX' )->set_reptext( 'Your description' ). "Set heading column label
*  p_me->column( 'XXX' )->set_scrtext_s( 'Your description' ). "Set short column label
*  p_me->column( 'XXX' )->set_scrtext_m( 'Your description' ). "Set medium column label
*  p_me->column( 'XXX' )->set_scrtext_l( 'Your description' ). "Set long column label

  p_me->column( 'LINENO' )->set_no_out( 'X' ).
  p_me->column( 'MESSAGE_V1' )->set_no_out( 'X' ).
  p_me->column( 'MESSAGE_V2' )->set_no_out( 'X' ).
  p_me->column( 'MESSAGE_V3' )->set_no_out( 'X' ).
  p_me->column( 'MESSAGE_V4' )->set_no_out( 'X' ).

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_DISPLAY_MESSAGE_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_MESSAGE
*&---------------------------------------------------------------------*
FORM f_display_message_b  USING    p_git_message TYPE gtt_message.

* types for messages
  TYPES: BEGIN OF lty_message_tab,
           msgid  TYPE sy-msgid,
           msgty  TYPE sy-msgty,
           msgno  TYPE sy-msgno,
           msgv1  TYPE sy-msgv1,
           msgv2  TYPE sy-msgv2,
           msgv3  TYPE sy-msgv3,
           msgv4  TYPE sy-msgv4,
           lineno TYPE mesg-zeile,
         END OF lty_message_tab.
  TYPES: ltt_message_tab TYPE lty_message_tab OCCURS 20.

  DATA: lit_message_tab TYPE ltt_message_tab.

*--------------------------------------------------------------------*

  lit_message_tab = CORRESPONDING #( p_git_message
                                       MAPPING msgid = id
                                               msgty = type
                                               msgno = number
                                               msgv1 = message_v1
                                               msgv2 = message_v2
                                               msgv3 = message_v3
                                               msgv4 = message_v4
                                               lineno = lineno
                                    ).

*--------------------------------------------------------------------*

  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
    TABLES
      i_message_tab = lit_message_tab.

ENDFORM.
