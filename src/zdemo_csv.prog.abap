*&---------------------------------------------------------------------*
*& Report ZDEMO_CSV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_csv.

**********************************************************************
* Definiciones Globales
**********************************************************************
TYPE-POOLS: vrm.

DATA: gt_format_date TYPE vrm_values,
      gt_sep_date    TYPE vrm_values,
      gt_sflight     TYPE TABLE OF sflight,
      gt_manual      TYPE ztt_csv_field,
      gs_manual      TYPE zst_csv_field.

DATA: go_split     TYPE REF TO cl_gui_splitter_container,
      go_container TYPE REF TO cl_gui_container,
      go_alv       TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gt_sort      TYPE lvc_t_sort,
      gs_layout    TYPE lvc_s_layo,
      gs_variant   TYPE disvariant.


DEFINE add_manual.
  CLEAR: gs_manual.
  gs_manual-name = &1.
  gs_manual-rollname = &2.
  APPEND gs_manual TO gt_manual.
END-OF-DEFINITION.


**********************************************************************
* Pantalla de Selección
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-t00.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_down RADIOBUTTON GROUP g0 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (15) FOR FIELD p_down.
PARAMETERS: p_up   RADIOBUTTON GROUP g0.
SELECTION-SCREEN COMMENT (15) FOR FIELD p_up.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_file TYPE string LOWER CASE.

SELECTION-SCREEN SKIP.

PARAMETERS: p_format AS CHECKBOX.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECTION-SCREEN COMMENT /01(72) gv_ifld1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_std   RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_std.
PARAMETERS: p_custom RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_custom.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
SELECTION-SCREEN COMMENT /01(72) gv_ifld2.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) FOR FIELD p_fordat.
PARAMETERS: p_fordat TYPE c AS LISTBOX VISIBLE LENGTH 12.
SELECTION-SCREEN COMMENT 40(20) FOR FIELD p_sepdat.
PARAMETERS: p_sepdat TYPE c AS LISTBOX VISIBLE LENGTH 10.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
SELECTION-SCREEN COMMENT /01(72) gv_ifld3.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) FOR FIELD p_sapval.
PARAMETERS: p_sapval TYPE text255 VISIBLE LENGTH 15.
SELECTION-SCREEN COMMENT 40(20) FOR FIELD p_csvval.
PARAMETERS: p_csvval TYPE text255 VISIBLE LENGTH 15.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-t04.
SELECTION-SCREEN COMMENT /01(72) gv_ifld4.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) FOR FIELD p_len.
PARAMETERS: p_len   TYPE numc2.
SELECTION-SCREEN COMMENT 40(20) FOR FIELD p_dec.
PARAMETERS: p_dec   TYPE numc2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) FOR FIELD p_sep_m.
PARAMETERS: p_sep_m TYPE char1.
SELECTION-SCREEN COMMENT 40(20) FOR FIELD p_sep_d.
PARAMETERS: p_sep_d TYPE char1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_sign  AS CHECKBOX.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_sign.
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_alpha AS CHECKBOX.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_alpha.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN END OF BLOCK b0.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_local.

INITIALIZATION.
  PERFORM init.


**********************************************************************
* Lógica programa
**********************************************************************
START-OF-SELECTION.

  IF p_file IS NOT INITIAL.
    IF p_up IS NOT INITIAL.
      PERFORM upload_csv.
    ELSEIF p_down IS NOT INITIAL.
      PERFORM download_csv.
    ENDIF.
  ENDIF.


END-OF-SELECTION.

  IF p_file IS NOT INITIAL.
    IF p_up IS NOT INITIAL.
      " Visualizar datos del fichero cargados en pantalla ALV
      CALL SCREEN 9000.
    ELSEIF p_down IS NOT INITIAL.
      " Mensaje con la cantidad de líneas descargadas
      PERFORM msg_csv.
    ENDIF.
  ELSE.
    MESSAGE s398(00) WITH TEXT-e00 DISPLAY LIKE 'E'.
  ENDIF.


**********************************************************************
* Rutinas generales
**********************************************************************

*&---------------------------------------------------------------------*
*&      Form  F4_LOCAL
*&---------------------------------------------------------------------*
FORM f4_local .

  IF p_up IS NOT INITIAL.
    p_file = zcl_util_csv=>f4_file_local_open( ).
  ELSEIF p_down IS NOT INITIAL.
    p_file = zcl_util_csv=>f4_file_local_save( ).
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .

  DATA: ls_list TYPE vrm_value.

  ls_list-key = '1'.
  ls_list-text = 'DDMMYY'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '2'.
  ls_list-text = 'MMDDYY'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '3'.
  ls_list-text = 'YYMMDD'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '4'.
  ls_list-text = 'DDMMYYYY'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '5'.
  ls_list-text = 'MMDDYYYY'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '6'.
  ls_list-text = 'YYYYMMDD'.
  APPEND ls_list TO gt_format_date.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_FORDAT'
      values          = gt_format_date
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.


  ls_list-key = '/'.
  ls_list-text = 'Barra'.
  APPEND ls_list TO gt_sep_date.
  ls_list-key = '.'.
  ls_list-text = 'Punto'.
  APPEND ls_list TO gt_sep_date.
  ls_list-key = '-'.
  ls_list-text = 'Guión'.
  APPEND ls_list TO gt_sep_date.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_SEPDAT'
      values          = gt_sep_date
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  " Textos de información
  gv_ifld1 = TEXT-i01.
  gv_ifld2 = TEXT-i02.
  gv_ifld3 = TEXT-i03.
  gv_ifld4 = TEXT-i04.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_CSV
*&---------------------------------------------------------------------*
FORM upload_csv .

  DATA: li_csv_upload TYPE REF TO zif_util_csv_upload,
        lv_mask	      TYPE char8,
        lv_sep        TYPE char1.

  DATA: lt_data    TYPE REF TO data,
        ls_sflight TYPE sflight.
  FIELD-SYMBOLS: <ft_data> TYPE ANY TABLE,
                 <fs_data> TYPE any.

  CLEAR: gt_sflight[].

  IF p_format IS INITIAL.

    " Upload Local con Utilidad CSV
    lt_data = zcl_util_csv=>upload( iv_filepath = p_file
                                    iv_typename = 'SFLIGHT' ).
  ELSE.

    IF p_std IS NOT INITIAL.

      " Instancia de Upload para establecer formatos a medida
      " con tipo de datos del Diccionario.
      li_csv_upload = zcl_util_csv=>custom_upload( iv_filepath = p_file
                                                   iv_typename = 'SFLIGHT' ).

    ELSEIF p_custom IS NOT INITIAL.

      " Se añade solo una parte de los campos de la tabla SFLIGHT
      " indicando nombre de campo y tipo.
      CLEAR: gt_manual[].
      add_manual 'MANDT'           'S_MANDT'.
      add_manual 'CARRID'          'S_CARR_ID'.
      add_manual 'CONNID'          'S_CONN_ID'.
      add_manual 'FLDATE'          'S_DATE'.
      add_manual 'PRICE'           'S_PRICE'.
      add_manual 'CURRENCY'        'S_CURRCODE'.
      add_manual 'PLANETYPE'       'S_PLANETYE'.
      add_manual 'SEATSMAX'        'S_SEATSMAX'.
      add_manual 'SEATSOCC'        'S_SEATSOCC'.
      add_manual 'PAYMENTSUM'      'S_SUM'.

      " Instancia de Upload para establecer formatos a medida
      " con tabla manual de campos.
      li_csv_upload = zcl_util_csv=>custom_upload( iv_filepath = p_file
                                                   it_manual   = gt_manual ).

    ENDIF.

    " Formatear fecha
    CASE p_fordat.
      WHEN 1.
        lv_mask = zcl_util_csv=>cs_mask-ddmmyy.
      WHEN 2.
        lv_mask = zcl_util_csv=>cs_mask-mmddyy.
      WHEN 3.
        lv_mask = zcl_util_csv=>cs_mask-yymmdd.
      WHEN 4.
        lv_mask = zcl_util_csv=>cs_mask-ddmmyyyy.
      WHEN 5.
        lv_mask = zcl_util_csv=>cs_mask-mmddyyyy.
      WHEN 6.
        lv_mask = zcl_util_csv=>cs_mask-yyyymmdd.
    ENDCASE.
    " Separador fecha
    lv_sep = p_sepdat.

    IF lv_mask IS NOT INITIAL.

      li_csv_upload->set_format_date( iv_fieldname = 'FLDATE'
                                      iv_mask      = lv_mask
                                      iv_sep       = lv_sep ).
    ENDIF.

    " Formatear número
    IF p_len IS NOT INITIAL.
      li_csv_upload->set_format_number( iv_fieldname     = 'PAYMENTSUM'
                                        iv_longitud      = p_len
                                        iv_decimales     = p_dec
                                        iv_sep_decimales = p_sep_d
                                        iv_sep_miles     = p_sep_m
                                        iv_sign          = p_sign
                                        iv_alpha         = p_alpha ).
    ENDIF.

    " Substituir valor por Mapeo
    IF p_sapval IS NOT INITIAL AND p_csvval IS NOT INITIAL.
      li_csv_upload->set_format_value_map( iv_fieldname = 'CURRENCY'
                                           iv_sap_value = p_sapval
                                           iv_csv_value = p_csvval ).
    ENDIF.

    " Cargar y Transformar CSV
    lt_data = li_csv_upload->convert_from_csv( ).

  ENDIF.

  " Pasar datos genéricos a tabla tipificada
  ASSIGN lt_data->* TO <ft_data>.
  IF <ft_data> IS ASSIGNED.
    LOOP AT <ft_data> ASSIGNING <fs_data>.
      CLEAR: ls_sflight.
      MOVE-CORRESPONDING <fs_data> TO ls_sflight.
      APPEND ls_sflight TO gt_sflight.
    ENDLOOP.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_CSV
*&---------------------------------------------------------------------*
FORM download_csv .

  DATA: lr_table_sflight TYPE REF TO data,
        li_csv_download  TYPE REF TO zif_util_csv_download,
        lv_mask	         TYPE char8,
        lv_sep 	         TYPE char1.

  CLEAR: gt_sflight[].
  SELECT * FROM sflight INTO TABLE gt_sflight WHERE carrid EQ 'AA'.
  GET REFERENCE OF gt_sflight INTO lr_table_sflight.

  IF p_format IS INITIAL.

    " Download Local con Utilidad CSV
    zcl_util_csv=>download( iv_filepath   = p_file
                            iv_typename   = 'SFLIGHT'
                            ir_table_data = lr_table_sflight ).

  ELSE.


    IF p_std IS NOT INITIAL.

      " Instancia de Download para establecer formatos a medida
      " con tipo de datos del Diccionario.
      li_csv_download = zcl_util_csv=>custom_download( iv_filepath = p_file
                                                       iv_typename = 'SFLIGHT' ).

    ELSEIF p_custom IS NOT INITIAL.

      " Se añade solo una parte de los campos de la tabla SFLIGHT
      " indicando nombre de campo y tipo.
      CLEAR: gt_manual[].
      add_manual 'MANDT'           'S_MANDT'.
      add_manual 'CARRID'          'S_CARR_ID'.
      add_manual 'CONNID'          'S_CONN_ID'.
      add_manual 'FLDATE'          'S_DATE'.
      add_manual 'PRICE'           'S_PRICE'.
      add_manual 'CURRENCY'        'S_CURRCODE'.
      add_manual 'PLANETYPE'       'S_PLANETYE'.
      add_manual 'SEATSMAX'        'S_SEATSMAX'.
      add_manual 'SEATSOCC'        'S_SEATSOCC'.
      add_manual 'PAYMENTSUM'      'S_SUM'.

      " Instancia de Download para establecer formatos a medida
      " con tabla manual de campos.
      li_csv_download = zcl_util_csv=>custom_download( iv_filepath = p_file
                                                       it_manual   = gt_manual ).

    ENDIF.

    " Formatear fecha
    CASE p_fordat.
      WHEN 1.
        lv_mask = zcl_util_csv=>cs_mask-ddmmyy.
      WHEN 2.
        lv_mask = zcl_util_csv=>cs_mask-mmddyy.
      WHEN 3.
        lv_mask = zcl_util_csv=>cs_mask-yymmdd.
      WHEN 4.
        lv_mask = zcl_util_csv=>cs_mask-ddmmyyyy.
      WHEN 5.
        lv_mask = zcl_util_csv=>cs_mask-mmddyyyy.
      WHEN 6.
        lv_mask = zcl_util_csv=>cs_mask-yyyymmdd.
    ENDCASE.
    " Separador fecha
    lv_sep = p_sepdat.

    IF lv_mask IS NOT INITIAL.
      li_csv_download->set_format_date( iv_fieldname = 'FLDATE'
                                        iv_mask      = lv_mask
                                        iv_sep       = lv_sep ).
    ENDIF.

    " Formatear número
    IF p_len IS NOT INITIAL.
      li_csv_download->set_format_number( iv_fieldname     = 'PAYMENTSUM'
                                          iv_longitud      = p_len
                                          iv_decimales     = p_dec
                                          iv_sep_decimales = p_sep_d
                                          iv_sep_miles     = p_sep_m
                                          iv_sign          = p_sign
                                          iv_alpha         = p_alpha ).
    ENDIF.

    " Substituir valor por Mapeo
    IF p_sapval IS NOT INITIAL AND p_csvval IS NOT INITIAL.
      li_csv_download->set_format_value_map( iv_fieldname = 'CURRENCY'
                                             iv_sap_value = p_sapval
                                             iv_csv_value = p_csvval ).
    ENDIF.

    " Convertir a CSV
    li_csv_download->convert_to_csv( ir_table_data = lr_table_sflight ).

    " Descargar CSV
    li_csv_download->download( ).

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  MSG_CSV
*&---------------------------------------------------------------------*
FORM msg_csv .

  DATA: lv_text TYPE text10,
        lv_num  TYPE int4.

  lv_num = lines( gt_sflight ).
  lv_text = lv_num.
  CONDENSE lv_text NO-GAPS.
  MESSAGE s398(00) WITH 'Nº registros descargados:' lv_text.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'BASICO'.
  SET TITLEBAR 'CARGA_CSV'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  ALV_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE alv_9000 OUTPUT.
  PERFORM alv_9000.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN '&F03'.
      SET SCREEN 0. LEAVE SCREEN.
      PERFORM clear_9000.
    WHEN '&F15' OR '&F12'.
      PERFORM clear_9000.
      SET SCREEN 0. LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  CLEAR_9000
*&---------------------------------------------------------------------*
FORM clear_9000.

  IF go_alv IS NOT INITIAL.
    go_alv->free( ).
  ENDIF.
  IF go_container IS NOT INITIAL.
    go_container->free( ).
  ENDIF.
  IF go_split IS NOT INITIAL.
    go_split->free( ).
  ENDIF.

  CLEAR: go_split, go_container, go_alv.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ALV_9000
*&---------------------------------------------------------------------*
FORM alv_9000 .

  DATA: ls_stable TYPE lvc_s_stbl.

  IF go_alv IS INITIAL.
    " Splitter de 1x2 para capturar pantalla completa
    CREATE OBJECT go_split
      EXPORTING
        metric            = '0001'
        parent            = cl_gui_container=>default_screen
        rows              = 1
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    go_container = go_split->get_container( row = 1 column = 1 ).

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_container.

    gs_variant-report  = sy-repid.
    gs_layout-sel_mode = 'A'.

    PERFORM catalogo USING 'SFLIGHT'.
    CALL METHOD go_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_sflight[]
        it_fieldcatalog = gt_fieldcat.

  ELSE.
    " Refrescar ALV y mantener posición barras desplazamiento
    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    go_alv->refresh_table_display(
      EXPORTING
        is_stable      = ls_stable
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CATALOGO
*&---------------------------------------------------------------------*
FORM catalogo USING uv_tabname TYPE tabname.

  CLEAR: gt_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = uv_tabname
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.
