class ZCL_UTIL_CSV definition
  public
  final
  create private .

public section.
  type-pools ABAP .

  interfaces ZIF_UTIL_CSV_UPLOAD .
  interfaces ZIF_UTIL_CSV_DOWNLOAD .

  constants:
    BEGIN OF cs_mask,
      ddmmyyyy TYPE char8 VALUE 'DDMMYYYY',
      ddmmyy   TYPE char8 VALUE 'DDMMYY',
      mmddyyyy TYPE char8 VALUE 'MMDDYYYY',
      mmddyy   TYPE char8 VALUE 'MMDDYY',
      yyyymmdd TYPE char8 VALUE 'YYYYMMDD',
      yymmdd   TYPE char8 VALUE 'YYMMDD',
    END OF cs_mask .

  class-methods F4_FOLDER_LOCAL
    importing
      !IV_DYNFLD_UPDATE type CHAR8 optional
      !IV_WINDOW_TITLE type STRING default SPACE
      !IV_INITIAL_FOLDER type STRING default 'C:\'
      !IV_DYNAME type PROGNAME default SY-CPROG
      !IV_DYNUMB type SYCHAR04 default SY-DYNNR
    returning
      value(RV_FOLDER) type STRING .
  class-methods F4_FOLDER_SERVER
    importing
      !IV_DYNFLD_UPDATE type CHAR8 optional
      !IV_WINDOW_TITLE type STRING default SPACE
      !IV_DYNAME type PROGNAME default SY-CPROG
      !IV_DYNUMB type SYCHAR04 default SY-DYNNR
    returning
      value(RV_FOLDER) type STRING .
  class-methods F4_FILE_LOCAL_OPEN
    importing
      !IV_WINDOW_TITLE type STRING default SPACE
      !IV_INITIAL_FOLDER type STRING default 'C:\'
      !IV_DEFAULT_EXTENSION type STRING optional
    returning
      value(RV_FILEPATH) type STRING .
  class-methods F4_FILE_LOCAL_SAVE
    importing
      !IV_WINDOW_TITLE type STRING default SPACE
      !IV_INITIAL_FOLDER type STRING default 'C:\'
      !IV_DEFAULT_EXTENSION type STRING optional
    returning
      value(RV_FILEPATH) type STRING .
  class-methods F4_FILE_SERVER_FULLSCREEN
    returning
      value(RV_FILEPATH) type STRING .
  class-methods F4_FILE_SERVER_POPUP
    returning
      value(RV_FILEPATH) type STRING .
  class-methods UPLOAD
    importing
      !IV_FILEPATH type STRING
      !IV_TYPENAME type TYPENAME optional
      !IT_MANUAL type ZTT_CSV_FIELD optional
      !IV_LOCAL type FLAG default ABAP_TRUE
      !IV_SERVER type FLAG default ABAP_FALSE
    returning
      value(RR_TABLE_DATA) type ref to DATA .
  class-methods CUSTOM_UPLOAD
    importing
      !IV_FILEPATH type STRING
      !IV_TYPENAME type TYPENAME optional
      !IT_MANUAL type ZTT_CSV_FIELD optional
      !IV_LOCAL type FLAG default ABAP_TRUE
      !IV_SERVER type FLAG default ABAP_FALSE
    returning
      value(RI_CSV_UPLOAD) type ref to ZIF_UTIL_CSV_UPLOAD .
  class-methods DOWNLOAD
    importing
      !IV_FILEPATH type STRING
      !IV_TYPENAME type TYPENAME optional
      !IT_MANUAL type ZTT_CSV_FIELD optional
      !IV_LOCAL type FLAG default ABAP_TRUE
      !IV_SERVER type FLAG default ABAP_FALSE
      !IV_CODEPAGE type ABAP_ENCODING default SPACE
      !IR_TABLE_DATA type ref to DATA
    returning
      value(RV_LINES_DOWNLOADED) type INT4 .
  class-methods CUSTOM_DOWNLOAD
    importing
      !IV_FILEPATH type STRING
      !IV_TYPENAME type TYPENAME optional
      !IT_MANUAL type ZTT_CSV_FIELD optional
      !IV_LOCAL type FLAG default ABAP_TRUE
      !IV_SERVER type FLAG default ABAP_FALSE
    returning
      value(RI_CSV_DOWNLOAD) type ref to ZIF_UTIL_CSV_DOWNLOAD .
protected section.
private section.

  types:
    ty_line TYPE c LENGTH 10000 .
  types:
    BEGIN OF ty_format_num,
      longitud      TYPE numc2,
      decimales     TYPE numc2,
      sep_decimales TYPE char1,
      sep_miles     TYPE char1,
      alpha         TYPE flag,
      sign          TYPE flag,
    END OF ty_format_num .
  types:
    tty_format_num TYPE TABLE OF ty_format_num .
  types:
    BEGIN OF ty_format_map,
      fieldname TYPE fieldname,
      sap_value TYPE text255,
      csv_value TYPE text255,
    END OF ty_format_map .
  types:
    tty_format_map TYPE TABLE OF ty_format_map .
  types:
    BEGIN OF ty_format_dat,
      mask TYPE char8,
      sep  TYPE char1,
    END OF ty_format_dat .
  types:
    tty_format_dat TYPE TABLE OF ty_format_dat .
  types:
    BEGIN OF ty_field,
      pos        TYPE int4,
      name       TYPE fieldname,
      rollname   TYPE rollname,
      leng       TYPE ddleng,
      outputlen  TYPE outputlen,
      decimals   TYPE decimals,
      datatype   TYPE dynptype,
      has_format TYPE char1,
      format_num TYPE ty_format_num,
      format_dat TYPE ty_format_dat,
    END OF ty_field .
  types:
    tty_field TYPE TABLE OF ty_field .

  constants:
    BEGIN OF cs_format,
      number TYPE char1 VALUE 'N',
      map    TYPE char1 VALUE 'M',
      date   TYPE char1 VALUE 'D',
    END OF cs_format .
  constants CV_SEP type CHAR1 value ';'. "#EC NOTEXT
  data MV_SOURCE type FLAG .
  data MV_DDIC_TYPE type TYPENAME .
  data MV_FILE_SEP type CHAR1 .
  data MV_FILEPATH type STRING .
  data MV_FILE_LENGTH type INT4 .
  data MV_FILENAME type STRING .
  data MT_PLAIN_DATA type STRINGTAB .
  data MO_CONV type ref to CL_RSDA_CSV_CONVERTER .
  constants CV_BARRA type CHAR1 value '/'. "#EC NOTEXT
  constants CV_CONTRA type CHAR1 value '\'. "#EC NOTEXT
  data MT_FIELD type TTY_FIELD .
  data MT_FORMAT_NUM type TTY_FORMAT_NUM .
  data MT_FORMAT_MAP type TTY_FORMAT_MAP .
  data MT_FORMAT_DAT type TTY_FORMAT_DAT .

  methods SET_FORMAT_VALUE_MAP
    importing
      !IV_FIELDNAME type FIELDNAME
      !IV_SAP_VALUE type TEXT255
      !IV_CSV_VALUE type TEXT255 .
  methods SET_FORMAT_NUMBER
    importing
      !IV_FIELDNAME type FIELDNAME
      !IV_LONGITUD type NUMC2
      !IV_DECIMALES type NUMC2
      !IV_SEP_DECIMALES type CHAR1 default '.'
      !IV_SEP_MILES type CHAR1 default ''
      !IV_SIGN type FLAG default ABAP_FALSE
      !IV_ALPHA type FLAG default ABAP_FALSE .
  methods SET_FORMAT_DATE
    importing
      !IV_FIELDNAME type FIELDNAME
      !IV_MASK type CHAR8
      !IV_SEP type CHAR1 default '' .
  methods FORMAT_TO_LINE
    changing
      !CR_LINE type ref to DATA .
  methods FORMAT_TO_NUMBER
    importing
      !IS_FIELD type TY_FIELD
    changing
      !CR_LINE type ref to DATA .
  methods FORMAT_TO_DATE
    importing
      !IS_FIELD type TY_FIELD
    changing
      !CR_LINE type ref to DATA .
  methods FORMAT_TO_MAP
    importing
      !IS_FIELD type TY_FIELD
    changing
      !CR_LINE type ref to DATA .
  methods FORMAT_FROM_LINE
    changing
      !CV_LINE type TY_LINE .
  methods FORMAT_FROM_NUMBER
    importing
      !IS_FIELD type TY_FIELD
    changing
      !CV_FIELD type TEXT255 .
  methods FORMAT_FROM_DATE
    importing
      !IS_FIELD type TY_FIELD
    changing
      !CV_FIELD type TEXT255 .
  methods FORMAT_FROM_MAP
    importing
      !IS_FIELD type TY_FIELD
    changing
      !CV_FIELD type TEXT255 .
  methods CREATE_STRUCTURE
    importing
      !IV_FORMAT type FLAG default ABAP_TRUE
    returning
      value(RR_STRUC) type ref to DATA .
  methods CREATE_TABLE
    importing
      !IV_FORMAT type FLAG default ABAP_TRUE
    returning
      value(RR_TABLE) type ref to DATA .
  methods UPLOAD_SERVER .
  methods UPLOAD_LOCAL
    importing
      !IV_CODEPAGE type ABAP_ENCODING default SPACE .
  methods DOWNLOAD_SERVER
    returning
      value(RV_LINES) type INT4 .
  methods DOWNLOAD_LOCAL
    importing
      !IV_CODEPAGE type ABAP_ENCODING default SPACE
    returning
      value(RV_LINES) type INT4 .
  class-methods CREATE
    importing
      !IV_FILEPATH type STRING
    returning
      value(RO_UTIL_CSV) type ref to ZCL_UTIL_CSV .
  methods SET_DDIC_FORMAT
    importing
      !IV_TYPENAME type TYPENAME
    returning
      value(RV_NUM_FIELDS) type INT4 .
  methods SET_MANUAL_FORMAT
    importing
      !IT_MANUAL type ZTT_CSV_FIELD
    returning
      value(RV_NUM_FIELDS) type INT4 .
  methods TO_CSV
    importing
      !IR_TABLE_DATA type ref to DATA
    returning
      value(RV_LINES) type INT4 .
  methods FROM_CSV
    returning
      value(RR_TABLE_DATA) type ref to DATA .
ENDCLASS.



CLASS ZCL_UTIL_CSV IMPLEMENTATION.


METHOD create.

    DATA: lt_split TYPE stringtab,
          lv_last  TYPE int4.

    IF iv_filepath IS INITIAL.
      " Ruta fichero obligatoria
      RETURN.
    ENDIF.

    " Crear objeto a retornar
    CREATE OBJECT ro_util_csv.
    ro_util_csv->mv_filepath = iv_filepath.

    " Determinar tipo de separador en ruta fichero para separar
    " el nombre de fichero de la ruta completa.
    FIND cv_barra IN iv_filepath IN CHARACTER MODE.
    IF sy-subrc EQ 0.
      SPLIT iv_filepath AT cv_barra INTO TABLE lt_split.
      ro_util_csv->mv_file_sep = cv_barra.
    ELSE.

      FIND cv_contra IN iv_filepath IN CHARACTER MODE.
      IF sy-subrc EQ 0.
        SPLIT iv_filepath AT cv_contra INTO TABLE lt_split.
        ro_util_csv->mv_file_sep = cv_contra.
      ENDIF.

    ENDIF.

    " Capturar nombre fichero
    lv_last = lines( lt_split ).
    IF lv_last GT 0.
      READ TABLE lt_split INTO ro_util_csv->mv_filename INDEX lv_last.
    ENDIF.

  ENDMETHOD.


METHOD create_structure.

    DATA: lo_struc_type TYPE REF TO cl_abap_structdescr,
          lt_comp       TYPE cl_abap_structdescr=>component_table,
          ls_comp       LIKE LINE OF lt_comp,
          ls_field      TYPE ty_field.


    " Crear un Tipo de datos con los componentes obtenidos de la estructura
    " de formato configurada en el método SET_DDIC_FORMAT.
    SORT mt_field BY pos.
    LOOP AT mt_field INTO ls_field.

      CLEAR: ls_comp.
      ls_comp-name = ls_field-name.
      IF iv_format EQ abap_false.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_name( ls_field-rollname ).
      ELSEIF iv_format EQ abap_true AND ls_field-has_format IS INITIAL.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_name( ls_field-rollname ).
      ELSEIF iv_format EQ abap_true AND ls_field-has_format IS NOT INITIAL.
        " Si tiene formato se crea como una variable de Texto
        ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'TEXT255' ).
      ENDIF.
      APPEND ls_comp TO lt_comp.

    ENDLOOP.

    lo_struc_type = cl_abap_structdescr=>create( lt_comp ).
    CREATE DATA rr_struc TYPE HANDLE lo_struc_type.

  ENDMETHOD.


METHOD create_table.

    DATA: lo_struc_type TYPE REF TO cl_abap_structdescr,
          lo_table_type TYPE REF TO cl_abap_tabledescr,
          lt_comp       TYPE cl_abap_structdescr=>component_table,
          ls_comp       LIKE LINE OF lt_comp,
          ls_field      TYPE ty_field.


    " Crear un Tipo de datos con los componentes obtenidos de la estructura
    " de formato configurada en el método SET_DDIC_FORMAT.
    SORT mt_field BY pos.
    LOOP AT mt_field INTO ls_field.

      CLEAR: ls_comp.
      ls_comp-name = ls_field-name.
      IF iv_format EQ abap_false.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_name( ls_field-rollname ).
      ELSEIF iv_format EQ abap_true AND ls_field-has_format IS INITIAL.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_name( ls_field-rollname ).
      ELSEIF iv_format EQ abap_true AND ls_field-has_format IS NOT INITIAL.
        " Si tiene formato se crea como una variable de Texto
        ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'TEXT255' ).
      ENDIF.
      APPEND ls_comp TO lt_comp.

    ENDLOOP.

    lo_struc_type = cl_abap_structdescr=>create( lt_comp ).
    lo_table_type = cl_abap_tabledescr=>create( p_line_type = lo_struc_type ).

    CREATE DATA rr_table TYPE HANDLE lo_table_type.

  ENDMETHOD.


METHOD CUSTOM_DOWNLOAD.

    DATA: lo_util_csv TYPE REF TO zcl_util_csv.

    IF it_manual[] IS INITIAL AND iv_typename IS INITIAL.
      " No se ha indicado ningún formato de datos
      RETURN.
    ENDIF.

    " Crear objeto utilidad CSV
    lo_util_csv = zcl_util_csv=>create( iv_filepath = iv_filepath ).

    " Establecer formato datos
    IF iv_typename IS NOT INITIAL.
      lo_util_csv->set_ddic_format( iv_typename = iv_typename ).
    ENDIF.
    IF it_manual[] IS NOT INITIAL.
      lo_util_csv->set_manual_format( it_manual = it_manual ).
    ENDIF.

    IF iv_local IS NOT INITIAL.
      lo_util_csv->mv_source = abap_true.
    ELSEIF iv_server IS NOT INITIAL.
      lo_util_csv->mv_source = abap_false.
    ENDIF.

    " Retornar objeto como interfaz de descarga
    ri_csv_download ?= lo_util_csv.

  ENDMETHOD.


METHOD CUSTOM_UPLOAD.

    DATA: lo_util_csv TYPE REF TO zcl_util_csv.

    IF it_manual[] IS INITIAL AND iv_typename IS INITIAL.
      " No se ha indicado ningún formato de datos
      RETURN.
    ENDIF.

    " Crear objeto utilidad CSV
    lo_util_csv = zcl_util_csv=>create( iv_filepath = iv_filepath ).

    " Cargar datos aplanados
    IF iv_local IS NOT INITIAL.
      lo_util_csv->upload_local( ).
      lo_util_csv->mv_source = abap_true.
    ELSEIF iv_server IS NOT INITIAL.
      lo_util_csv->upload_server( ).
      lo_util_csv->mv_source = abap_false.
    ENDIF.

    " Establecer formato datos
    IF iv_typename IS NOT INITIAL.
      lo_util_csv->set_ddic_format( iv_typename = iv_typename ).
    ENDIF.
    IF it_manual[] IS NOT INITIAL.
      lo_util_csv->set_manual_format( it_manual = it_manual ).
    ENDIF.

    " Retornar objeto como interfaz
    ri_csv_upload ?= lo_util_csv.

  ENDMETHOD.


METHOD download.

    DATA: lo_csv TYPE REF TO zcl_util_csv.

    CLEAR: rv_lines_downloaded.

    IF it_manual[] IS INITIAL AND iv_typename IS INITIAL.
      " No se ha indicado ningún formato de datos
      RETURN.
    ENDIF.

    " Crear objeto utilidad CSV
    lo_csv = zcl_util_csv=>create( iv_filepath = iv_filepath ).

    " Establecer formato datos
    IF iv_typename IS NOT INITIAL.
      lo_csv->set_ddic_format( iv_typename = iv_typename ).
    ENDIF.
    IF it_manual[] IS NOT INITIAL.
      lo_csv->set_manual_format( it_manual = it_manual ).
    ENDIF.

    " Pasar tabla con formato configurado a datos aplanados CSV
    lo_csv->to_csv( ir_table_data = ir_table_data ).

    " Descargar datos aplanados
    IF iv_local IS NOT INITIAL.
      rv_lines_downloaded = lo_csv->download_local( iv_codepage = iv_codepage ).
      lo_csv->mv_source = abap_true.
    ELSEIF iv_server IS NOT INITIAL.
      rv_lines_downloaded = lo_csv->download_server( ).
      lo_csv->mv_source = abap_false.
    ENDIF.

  ENDMETHOD.


METHOD download_local.

    CLEAR: rv_lines.

    IF mt_plain_data IS INITIAL.
      " No hay datos a descargar
    ELSE.
      cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename                  = mv_filepath
          codepage                  = iv_codepage
        CHANGING
          data_tab                  = mt_plain_data
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          not_supported_by_gui      = 22
          error_no_gui              = 23
          OTHERS                    = 24 ).
      IF sy-subrc EQ 0.
        rv_lines = lines( mt_plain_data ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD download_server.

    DATA: lv_line TYPE string.

    CLEAR: rv_lines.

    " Abrir fichero del servidor en modo texto y codificación UTF-8
    OPEN DATASET mv_filepath
      FOR OUTPUT
      IN TEXT MODE
      ENCODING UTF-8
      IGNORING CONVERSION ERRORS.
    IF sy-subrc EQ 0.
      " Pasar datos al fichero
      LOOP AT mt_plain_data INTO lv_line.
        TRANSFER lv_line TO mv_filepath.
      ENDLOOP.
      " Cerrar fichero
      CLOSE DATASET mv_filepath.
      IF sy-subrc EQ 0.
        rv_lines = lines( mt_plain_data ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD f4_file_local_open.

    DATA: lt_file TYPE filetable,
          ls_file TYPE file_table,
          lv_rc   TYPE i.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = iv_window_title
        initial_directory       = iv_initial_folder
        default_extension       = iv_default_extension
      CHANGING
        file_table              = lt_file
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc EQ 0.
      LOOP AT lt_file INTO ls_file.
        MOVE ls_file-filename TO rv_filepath.
        RETURN.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


METHOD f4_file_local_save.

    DATA: lv_filename TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = iv_window_title
        initial_directory         = iv_initial_folder
          default_extension       = iv_default_extension
      CHANGING
        filename                  = lv_filename      " File Name to Save
        path                      = lv_path          " Path to File
        fullpath                  = lv_fullpath      " Path + File Name
      EXCEPTIONS
        cntl_error                = 1                " Control error
        error_no_gui              = 2                " No GUI available
        not_supported_by_gui      = 3                " GUI does not support this
        invalid_default_file_name = 4                " Invalid default file name
        OTHERS                    = 5 ).

    IF sy-subrc EQ 0.
      rv_filepath = lv_fullpath.
    ENDIF.

  ENDMETHOD.


METHOD f4_file_server_fullscreen.

    DATA: lv_path_name TYPE c LENGTH 500.

    SUBMIT rs_get_f4_dir_from_applserv AND RETURN.
    IMPORT path_name = lv_path_name FROM MEMORY ID 'PATH_NAME_SDL'.

    IF lv_path_name IS NOT INITIAL.
      rv_filepath = lv_path_name.
    ENDIF.

  ENDMETHOD.


METHOD f4_file_server_popup.

    DATA: lv_search_dir TYPE dxfields-longpath VALUE '/sapglobal/users',
          lv_filepath   TYPE dxlpath.

    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = 'A'
        i_server        = ' '
        i_path          = lv_search_dir
      IMPORTING
        o_path          = lv_filepath
      EXCEPTIONS
        rfc_error       = 1
        OTHERS          = 2.
    IF sy-subrc EQ 0.
      rv_filepath = lv_filepath.
    ENDIF.

  ENDMETHOD.


METHOD f4_folder_local.

    DATA: lt_dynpfields TYPE TABLE OF dynpread,
          ls_dynpfields LIKE LINE OF lt_dynpfields.


    cl_gui_frontend_services=>directory_browse(
      EXPORTING
      window_title         = iv_window_title
        initial_folder       = iv_initial_folder
      CHANGING
        selected_folder      = rv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).

    IF sy-subrc = 0 AND iv_dynfld_update IS NOT INITIAL.

      CLEAR: lt_dynpfields, ls_dynpfields.
      ls_dynpfields-fieldname  = iv_dynfld_update.
      ls_dynpfields-fieldvalue = rv_folder.
      APPEND ls_dynpfields TO lt_dynpfields.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = iv_dyname
          dynumb               = iv_dynumb
        TABLES
          dynpfields           = lt_dynpfields
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          undefind_error       = 7
          OTHERS               = 8.

    ENDIF.

  ENDMETHOD.


METHOD f4_folder_server.

    DATA: lt_dynpfields    TYPE TABLE OF dynpread,
          lt_server_folder TYPE TABLE OF user_dir,
          lt_return        TYPE STANDARD TABLE OF ddshretval,
          ls_dynpfields    LIKE LINE OF lt_dynpfields,
          ls_return        LIKE LINE OF lt_return.


    " Directorios SAP de Usuario
    SELECT * FROM user_dir INTO TABLE lt_server_folder.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        ddic_structure  = 'USER_DIR'
        retfield        = 'DIRNAME'
        value_org       = 'S'
      TABLES
        value_tab       = lt_server_folder
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.


    READ TABLE lt_return INTO ls_return INDEX 1.
    IF sy-subrc = 0.
      " Retornar ruta directorio seleccionado
      rv_folder = ls_return-fieldval.

      " Actualizar Screen si se indica un campo
      IF iv_dynfld_update IS NOT INITIAL.

        CLEAR: lt_dynpfields, ls_dynpfields.
        ls_dynpfields-fieldname  = iv_dynfld_update.
        ls_dynpfields-fieldvalue = rv_folder.
        APPEND ls_dynpfields TO lt_dynpfields.

        CALL FUNCTION 'DYNP_VALUES_UPDATE'
          EXPORTING
            dyname               = iv_dyname
            dynumb               = iv_dynumb
          TABLES
            dynpfields           = lt_dynpfields
          EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.

      ENDIF.

    ENDIF.

  ENDMETHOD.


METHOD format_from_date.

    " Si hay separador configurado se debe quitar
    IF is_field-format_dat-sep IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF is_field-format_dat-sep IN cv_field WITH space.
    ENDIF.

    " Quitar separadores típicos si los hay
    REPLACE ALL OCCURRENCES OF '.' IN cv_field WITH space.
    REPLACE ALL OCCURRENCES OF '/' IN cv_field WITH space.
    REPLACE ALL OCCURRENCES OF '-' IN cv_field WITH space.
    CONDENSE cv_field NO-GAPS.

    " Asegurar que solo hay números
    IF cv_field CO '1234567890 '.

      " En base a la longitud de entrada se pueden aplicar unos
      " formatos u otros. Si no tiene la longitud adecuada se
      " inicializa la fecha SAP.
      IF strlen( cv_field ) EQ 6.

        " En base a la mascara de edición fijada
        CASE is_field-format_dat-mask.

          WHEN cs_mask-ddmmyy.
            cv_field = |{ cv_field+4(2) }{ cv_field+2(2) }{ cv_field+0(2) }|.

          WHEN cs_mask-mmddyy.
            cv_field = |{ cv_field+4(2) }{ cv_field+0(2) }{ cv_field+2(2) }|.

          WHEN cs_mask-yymmdd.
            " Solo le faltan los dos dígitos iniciales para tener el formato SAP

        ENDCASE.
        " Añadir dígitos iniciales del año
        cv_field = |{ sy-datum+0(2) }{ cv_field }|.

      ELSEIF strlen( cv_field ) EQ 8.

        " En base a la mascara de edición fijada
        CASE is_field-format_dat-mask.
          WHEN cs_mask-ddmmyyyy.
            cv_field = |{ cv_field+4(4) }{ cv_field+2(2) }{ cv_field+0(2) }|.

          WHEN cs_mask-mmddyyyy.
            cv_field = |{ cv_field+4(4) }{ cv_field+0(2) }{ cv_field+2(2) }|.

          WHEN cs_mask-yyyymmdd.
            " Ya es el formato SAP

        ENDCASE.

      ELSE.
        cv_field = '00000000'.
      ENDIF.

    ENDIF.

  ENDMETHOD.


METHOD format_from_line.

    DATA: lt_line          TYPE TABLE OF text255,
          ls_field         TYPE ty_field,
          lv_format_active TYPE flag VALUE abap_false.

    FIELD-SYMBOLS: <fv_field> TYPE text255.

    " Separar datos CSV en una tabla a partir del separador
    SPLIT cv_line AT cv_sep INTO TABLE lt_line.

    LOOP AT mt_field INTO ls_field WHERE has_format IS NOT INITIAL.
      " Si hay algún campo con formato fijar marca
      lv_format_active = abap_true.

      " Capturar el campo a formatear a partir de la posicíón en la configuración
      READ TABLE lt_line ASSIGNING <fv_field> INDEX ls_field-pos.
      IF sy-subrc EQ 0.

        CASE ls_field-has_format.
          WHEN cs_format-number.

            format_from_number( EXPORTING is_field = ls_field
                                CHANGING  cv_field = <fv_field> ).

          WHEN cs_format-map.

            format_from_map( EXPORTING is_field = ls_field
                             CHANGING  cv_field = <fv_field> ).

          WHEN cs_format-date.

            format_from_date( EXPORTING is_field = ls_field
                              CHANGING  cv_field = <fv_field> ).

        ENDCASE.

      ENDIF.

    ENDLOOP.

    " Actualizar línea de salida si existe algún campo que tenga formato
    IF lv_format_active EQ abap_true.
      CLEAR: cv_line.
      LOOP AT lt_line ASSIGNING <fv_field>.
        IF sy-tabix EQ 1.
          cv_line = |{ <fv_field> }|.
        ELSE.
          cv_line = |{ cv_line }{ cv_sep }{ <fv_field> }|.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


METHOD format_from_map.

    DATA: ls_map TYPE ty_format_map.

    READ TABLE mt_format_map INTO ls_map
      WITH KEY fieldname = is_field-name
               csv_value = cv_field.
    IF sy-subrc EQ 0.
      cv_field = ls_map-sap_value.
    ELSE.
      " Si no hay mapeo se vacía el campo de salida
      CLEAR: cv_field.
    ENDIF.

  ENDMETHOD.


METHOD format_from_number.

  DATA: lv_int      TYPE text255,
        lv_dec      TYPE text255,
        lv_pattern  TYPE string,
        lv_negativo TYPE flag.

  " Patrón de número válido incluyendo los separadores configurados
  lv_pattern = ' 0987654321+-.,'                 &&
               is_field-format_num-sep_decimales &&
               is_field-format_num-sep_miles.
  IF cv_field CN lv_pattern.
    RETURN.
  ENDIF.

  " Si la configuración indica que los datos de entrada contiene signo
  " positivo, buscar el simbolo del signo y quitarlo, además de fijar
  " una variable que indica dicho signo encontrado.
  IF is_field-format_num-sign EQ abap_true.
    FIND '+' IN cv_field.
    IF sy-subrc EQ 0.
      " Marcar como positivo
      lv_negativo = abap_false.
      " Quitar símbolo positivo
      REPLACE '+' IN cv_field WITH space.
    ENDIF.
  ENDIF.
  " Buscar negativo y quitar
  FIND '-' IN cv_field.
  IF sy-subrc EQ 0.
    " Marcar como negativo
    lv_negativo = abap_true.
    " Quitar símbolo negativo
    REPLACE '-' IN cv_field WITH space.
  ENDIF.
  CONDENSE cv_field NO-GAPS.

  " Para importar un número solamente interesa fijar el separador
  " SAP '.' para Decimales y quitar cualquier otro separador.
  " Si el campo no tiene separador de decimales configurado para el
  " import significa que se debe tratar como un entero.
  IF is_field-format_num-sep_decimales IS NOT INITIAL.

    " Separar parte entera y decimal
    SPLIT cv_field
      AT   is_field-format_num-sep_decimales
      INTO lv_int lv_dec.

    " Quitar ceros a la izquierda
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_int
      IMPORTING
        output = lv_int.

    " Si hay separador de miles se debe quitar
    IF is_field-format_num-sep_miles IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF is_field-format_num-sep_miles
        IN lv_int WITH space.
    ENDIF.
    " Unir parte entera y decimal mediante el separador de decimales SAP
    cv_field = |{ lv_int }.{ lv_dec }|.
  ELSE.

    " Si hay separador de miles se debe quitar
    IF is_field-format_num-sep_miles IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF is_field-format_num-sep_miles
        IN cv_field WITH space.
    ENDIF.

    " Quitar cualquier separador que pueda tener
    REPLACE ALL OCCURRENCES OF '.' IN cv_field WITH space.
    REPLACE ALL OCCURRENCES OF ',' IN cv_field WITH space.

    " Quitar ceros a la izquierda
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cv_field
      IMPORTING
        output = cv_field.
  ENDIF.

  CONDENSE cv_field NO-GAPS.

  " Añadir signo negativo si se ha detectado
  IF lv_negativo EQ abap_true.
    cv_field = |-{ cv_field }|.
  ENDIF.

ENDMETHOD.


METHOD format_to_date.

    DATA: lv_date TYPE datum.

    FIELD-SYMBOLS: <fs_line>  TYPE any,
                   <fv_field> TYPE any.


    ASSIGN cr_line->* TO <fs_line>.
    IF <fs_line> IS ASSIGNED.
      " Capturar campo a formatear de la línea de datos
      ASSIGN COMPONENT is_field-name OF STRUCTURE <fs_line> TO <fv_field>.
      IF <fv_field> IS ASSIGNED.

        CONDENSE <fv_field> NO-GAPS.
        " Verificar que la fecha contiene solo caracteres válidos
        IF <fv_field> CN ' 0123456789'.
          RETURN.
        ENDIF.
        " Pasar a variable tipo fecha
        lv_date = <fv_field>.

        " En base a la mascara de edición fijada
        CASE is_field-format_dat-mask.
          WHEN cs_mask-ddmmyyyy.
            IF is_field-format_dat-sep IS NOT INITIAL.
              <fv_field> = lv_date+6(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+4(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+0(4).
            ELSE.
              <fv_field> = lv_date+6(2) &&
                           lv_date+4(2) &&
                           lv_date+0(4).
            ENDIF.

          WHEN cs_mask-ddmmyy.
            IF is_field-format_dat-sep IS NOT INITIAL.
              <fv_field> = lv_date+6(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+4(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+2(2).
            ELSE.
              <fv_field> = lv_date+6(2) &&
                           lv_date+4(2) &&
                           lv_date+2(2).
            ENDIF.

          WHEN cs_mask-mmddyyyy.
            IF is_field-format_dat-sep IS NOT INITIAL.
              <fv_field> = lv_date+4(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+6(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+0(4).
            ELSE.
              <fv_field> = lv_date+4(2) &&
                           lv_date+6(2) &&
                           lv_date+0(4).
            ENDIF.

          WHEN cs_mask-mmddyy.
            IF is_field-format_dat-sep IS NOT INITIAL.
              <fv_field> = lv_date+4(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+6(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+2(2).
            ELSE.
              <fv_field> = lv_date+4(2) &&
                           lv_date+6(2) &&
                           lv_date+2(2).
            ENDIF.

          WHEN cs_mask-yyyymmdd.
            IF is_field-format_dat-sep IS NOT INITIAL.
              <fv_field> = lv_date+0(4)            &&
                           is_field-format_dat-sep &&
                           lv_date+4(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+6(2).
            ELSE.
              <fv_field> = lv_date.
            ENDIF.

          WHEN cs_mask-yymmdd.
            IF is_field-format_dat-sep IS NOT INITIAL.
              <fv_field> = lv_date+2(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+4(2)            &&
                           is_field-format_dat-sep &&
                           lv_date+6(2).
            ELSE.
              <fv_field> = lv_date+2(2) &&
                           lv_date+4(2) &&
                           lv_date+6(2).
            ENDIF.

        ENDCASE.

      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD format_to_line.

    DATA: ls_field TYPE ty_field.

    LOOP AT mt_field INTO ls_field WHERE has_format IS NOT INITIAL.

      CASE ls_field-has_format.
        WHEN cs_format-number.

          format_to_number( EXPORTING is_field = ls_field
                            CHANGING  cr_line  = cr_line ).

        WHEN cs_format-map.

          format_to_map( EXPORTING is_field = ls_field
                         CHANGING  cr_line  = cr_line ).

        WHEN cs_format-date.

          format_to_date( EXPORTING is_field = ls_field
                          CHANGING  cr_line  = cr_line ).

      ENDCASE.

    ENDLOOP.


  ENDMETHOD.


METHOD format_to_map.

    DATA: ls_map TYPE ty_format_map.

    FIELD-SYMBOLS: <fs_line>  TYPE any,
                   <fv_field> TYPE any.


    ASSIGN cr_line->* TO <fs_line>.
    IF <fs_line> IS ASSIGNED.
      " Capturar campo a formatear de la línea de datos
      ASSIGN COMPONENT is_field-name OF STRUCTURE <fs_line> TO <fv_field>.
      IF <fv_field> IS ASSIGNED.

        READ TABLE mt_format_map INTO ls_map
          WITH KEY fieldname = is_field-name
                   sap_value = <fv_field>.
        IF sy-subrc EQ 0.
          <fv_field> = ls_map-csv_value.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD format_to_number.

  DATA: lo_int       TYPE REF TO cl_abap_elemdescr,
        lo_dec       TYPE REF TO cl_abap_elemdescr,
        lr_int       TYPE REF TO data,
        lr_dec       TYPE REF TO data,
        lv_int       TYPE int4,
        lv_dec       TYPE int4,
        lv_tint      TYPE string,
        lv_tint_a    TYPE string,
        lv_tdec      TYPE string,
        lv_num       TYPE string,
        lv_miles     TYPE i,
        lv_resto     TYPE i,
        lv_cont      TYPE i,
        lv_num_miles TYPE string,
        lv_negativo  TYPE flag.

  FIELD-SYMBOLS: <fs_line>  TYPE any,
                 <fv_field> TYPE any,
                 <fv_int>   TYPE any,
                 <fv_dec>   TYPE any.


  ASSIGN cr_line->* TO <fs_line>.
  IF <fs_line> IS ASSIGNED.
    " Capturar campo a formatear de la línea de datos
    ASSIGN COMPONENT is_field-name OF STRUCTURE <fs_line> TO <fv_field>.
    IF <fv_field> IS ASSIGNED.

      lv_num = <fv_field>.
      CONDENSE lv_num NO-GAPS.

      " Debe tener valor
      IF lv_num IS INITIAL.
        RETURN.
      ENDIF.
      " Verificar que es un número válido
      IF lv_num CN ' 0987654321+-.,'.
        RETURN.
      ENDIF.

      " Asegurar que el separador de decimales está en formato SAP '.'
      " para poder aplicar el formato de salida correctamente.
      IF lv_num CS ',' AND lv_num CS '.'.
        " Detectados dos separadores, se debe eliminar el de miles.
        REPLACE ALL OCCURRENCES OF ',' IN lv_num WITH space.
        CONDENSE lv_num NO-GAPS.
      ELSEIF lv_num NS ',' AND lv_num CS '.'.
        " Solo hay separador decimales '.' -> Ya tiene el formato SAP
      ELSEIF lv_num CS ',' AND lv_num NS '.'.
        " Se considera que el número recibido es un decimal con separador ','
        " y se debe convertir a separador decimal en formato SAP '.'
        REPLACE ALL OCCURRENCES OF ',' IN lv_num WITH '.'.
      ELSEIF lv_num NS ',' AND lv_num NS '.'.
        " No hay separadores -> Se trata de un número entero
      ENDIF.

      " Longitud debe ser mayor a 0
      IF is_field-format_num-longitud LE 0.
        RETURN.
      ENDIF.
      " Longitud debe ser mayor a los decimales
      IF is_field-format_num-longitud LT is_field-format_num-decimales.
        RETURN.
      ENDIF.

      " Buscar si es un número negativo
      FIND '-' IN lv_num IN CHARACTER MODE.
      IF sy-subrc EQ 0.
        lv_negativo = abap_true.
        " Quitar el signo para evitar problemas durante el formateo
        REPLACE '-' IN lv_num WITH space.
      ENDIF.

      " La parte entera es la longitud total menos la cantidad de decimales
      lv_int = is_field-format_num-longitud - is_field-format_num-decimales.
      lv_dec = is_field-format_num-decimales.

      " Crear variables tipo NUMC con las lóngitudes de la parte entera
      " y de la parte decimal.
      IF lv_int GT 0.
        lo_int = cl_abap_elemdescr=>get_n( p_length = lv_int ).
        CREATE DATA lr_int TYPE HANDLE lo_int.
        ASSIGN lr_int->* TO <fv_int>.
        " Parte entera
        IF <fv_int> IS ASSIGNED.
          <fv_int> = trunc( lv_num ).
          lv_tint = <fv_int>.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = lv_tint
            IMPORTING
              output = lv_tint_a.
          CONDENSE lv_tint_a NO-GAPS.
          " Quitar ceros si está marcada la variable
          IF is_field-format_num-alpha EQ abap_true.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = lv_tint
              IMPORTING
                output = lv_tint.
          ENDIF.
          CONDENSE lv_tint NO-GAPS.
        ENDIF.
      ENDIF.

      " Parte decimal
      IF lv_dec GT 0.
        lo_dec = cl_abap_elemdescr=>get_n( p_length = lv_dec ).
        CREATE DATA lr_dec TYPE HANDLE lo_dec.
        ASSIGN lr_dec->* TO <fv_dec>.

        IF <fv_dec> IS ASSIGNED.
          lv_tdec = frac( lv_num ).
          CONDENSE lv_tdec NO-GAPS.  " Quitar espacios
          SHIFT lv_tdec BY 2 PLACES. " Quitar '0.'
          CLEAR: lv_cont.
          lv_cont = strlen( lv_tdec ).
          " Bucle para sobrescribir la variable tipo N de derecha
          " a izquierda carácter a carácter. Con ello se rellenan
          " ceros a la derecha en la variable final.
          IF lv_cont GT 0 AND lv_dec GT 0.
            IF lv_cont GT lv_dec.
              lv_cont = lv_dec.
            ENDIF.
            " Quitar uno para calcular offset de inicio de copia
            lv_cont = lv_cont - 1.
            WHILE lv_cont GE 0.
              <fv_dec>+lv_cont(1) = lv_tdec+lv_cont(1).
              lv_cont = lv_cont - 1.
            ENDWHILE.
          ENDIF.

        ENDIF.
      ENDIF.


      " Dígitos para parte Entera y Decimal
      " **********************************************************************
      IF <fv_int> IS ASSIGNED AND <fv_dec> IS ASSIGNED.

        IF is_field-format_num-sep_decimales IS NOT INITIAL AND
           is_field-format_num-sep_miles     IS NOT INITIAL AND
           is_field-format_num-sep_miles     NE space.
          " CON Separador de Decimales y CON Separador de Miles
          " ***************************************************
          CLEAR: lv_miles, lv_resto, lv_cont, lv_num_miles.
          IF is_field-format_num-alpha EQ abap_true.
            lv_cont = strlen( lv_tint_a ).
          ELSE.
            lv_cont = lv_int.
          ENDIF.

          IF lv_cont GE 3.
            lv_miles = lv_cont DIV 3.
            lv_resto = lv_cont MOD 3.
            IF lv_resto EQ 0.
              lv_miles = lv_miles - 1.
            ENDIF.
            DO lv_miles TIMES.
              lv_cont = lv_cont - 3.
              lv_num_miles = |{ is_field-format_num-sep_miles }{ lv_tint+lv_cont(3) }{ lv_num_miles }|.
            ENDDO.
            " Offset del último tramo
            IF is_field-format_num-alpha EQ abap_true.
              lv_cont = strlen( lv_tint_a ).
              lv_cont = lv_cont - ( lv_miles * 3 ).
            ELSE.
              lv_cont = lv_int - ( lv_miles * 3 ).
            ENDIF.
          ENDIF.
          lv_num_miles = |{ lv_tint+0(lv_cont) }{ lv_num_miles }|.
          CONDENSE lv_num_miles NO-GAPS.

          <fv_field> = |{ lv_num_miles }{ is_field-format_num-sep_decimales }{ <fv_dec> }|.

        ELSEIF is_field-format_num-sep_decimales IS NOT INITIAL AND
             ( is_field-format_num-sep_miles     IS INITIAL OR
               is_field-format_num-sep_miles     EQ space ).
          " CON Separador de Decimales y SIN Separador de Miles
          " ***************************************************
          <fv_field> = |{ lv_tint }{ is_field-format_num-sep_decimales }{ <fv_dec> }|.

        ELSEIF is_field-format_num-sep_decimales IS INITIAL AND
               is_field-format_num-sep_miles     IS NOT INITIAL AND
               is_field-format_num-sep_miles     NE space.
          " SIN Separador de Decimales y CON Separador de Miles
          " ***************************************************
          CLEAR: lv_miles, lv_resto, lv_cont, lv_num_miles.
          IF is_field-format_num-alpha EQ abap_true.
            lv_cont = strlen( lv_tint_a ).
          ELSE.
            lv_cont = lv_int.
          ENDIF.

          IF lv_cont GE 3.
            lv_miles = lv_cont DIV 3.
            lv_resto = lv_cont MOD 3.
            IF lv_resto EQ 0.
              lv_miles = lv_miles - 1.
            ENDIF.
            DO lv_miles TIMES.
              lv_cont = lv_cont - 3.
              lv_num_miles = |{ is_field-format_num-sep_miles }{ lv_tint+lv_cont(3) }{ lv_num_miles }|.
            ENDDO.
            " Offset del último tramo
            IF is_field-format_num-alpha EQ abap_true.
              lv_cont = strlen( lv_tint_a ).
              lv_cont = lv_cont - ( lv_miles * 3 ).
            ELSE.
              lv_cont = lv_int - ( lv_miles * 3 ).
            ENDIF.
          ENDIF.
          lv_num_miles = |{ lv_tint+0(lv_cont) }{ lv_num_miles }|.
          CONDENSE lv_num_miles NO-GAPS.

          <fv_field> = |{ lv_num_miles }{ <fv_dec> }|.

        ELSEIF is_field-format_num-sep_decimales IS INITIAL AND
             ( is_field-format_num-sep_miles     IS INITIAL OR
               is_field-format_num-sep_miles     EQ space ).
          " SIN Separador de Decimales y SIN Separador de Miles
          " ***************************************************
          <fv_field> = |{ lv_tint }{ <fv_dec> }|.

        ENDIF.

      ELSEIF <fv_int> IS ASSIGNED AND <fv_dec> IS NOT ASSIGNED.
        " Solamente dígitos para parte Entera
        " **********************************************************************
        IF is_field-format_num-sep_miles   IS NOT INITIAL AND
           is_field-format_num-sep_miles   NE space.

          CLEAR: lv_miles, lv_resto, lv_cont, lv_num_miles.
          IF is_field-format_num-alpha EQ abap_true.
            lv_cont = strlen( lv_tint_a ).
          ELSE.
            lv_cont = lv_int.
          ENDIF.

          IF lv_cont GE 3.
            lv_miles = lv_cont DIV 3.
            lv_resto = lv_cont MOD 3.
            IF lv_resto EQ 0.
              lv_miles = lv_miles - 1.
            ENDIF.
            DO lv_miles TIMES.
              lv_cont = lv_cont - 3.
              lv_num_miles = |{ is_field-format_num-sep_miles }{ lv_tint+lv_cont(3) }{ lv_num_miles }|.
            ENDDO.
            " Offset del último tramo
            IF is_field-format_num-alpha EQ abap_true.
              lv_cont = strlen( lv_tint_a ).
              lv_cont = lv_cont - ( lv_miles * 3 ).
            ELSE.
              lv_cont = lv_int - ( lv_miles * 3 ).
            ENDIF.
          ENDIF.
          lv_num_miles = |{ lv_tint+0(lv_cont) }{ lv_num_miles }|.
          CONDENSE lv_num_miles NO-GAPS.

          <fv_field> = lv_num_miles.

        ELSE.
          IF is_field-format_num-alpha EQ abap_true.
            <fv_field> = lv_tint_a.
          ELSE.
            <fv_field> = <fv_int>.
          ENDIF.
        ENDIF.

      ELSEIF <fv_int> IS NOT ASSIGNED AND <fv_dec> IS ASSIGNED.
        " Solamente dígitos para parte Decimal
        " **********************************************************************
        IF is_field-format_num-sep_decimales IS NOT INITIAL.
          <fv_field> = |0{ is_field-format_num-sep_decimales }{ <fv_dec> }|.
        ELSE.
          " Separador SAP por defecto
          <fv_field> = |0.{ <fv_dec> }|.
        ENDIF.

      ENDIF.

      " Añadir signo negativo cuando sea necesario
      IF lv_negativo EQ abap_true.
        <fv_field> = |-{ <fv_field> }|.
      ELSE.
        " En caso de número positivo, en base a la configuración fijar el signo +
        IF is_field-format_num-sign IS NOT INITIAL.
          <fv_field> = |+{ <fv_field> }|.
        ENDIF.
      ENDIF.

    ENDIF. " Campo de entrada asignado?
  ENDIF. " Línea de datos de entrada asignada?

ENDMETHOD.


METHOD from_csv.

    DATA: lo_csv_converter TYPE REF TO cl_rsda_csv_converter,
          lr_struc_data    TYPE REF TO data.
    " No usar STRING ya que el Conversor necesita una variable con longitud
    " definida. Se pre-fijan 10.000 carácteres por línea, a priori suficiente.
    DATA: lv_line TYPE ty_line.

    FIELD-SYMBOLS: <ft_table> TYPE ANY TABLE,
                   <fs_line>  TYPE any.


    " Crear un Tipo de datos con los componentes obtenidos de la estructura
    " de formato configurada en el método SET_DDIC_FORMAT.
    lr_struc_data = create_structure( iv_format = abap_false ).
    rr_table_data = create_table( iv_format = abap_false ).
    ASSIGN lr_struc_data->* TO <fs_line>.
    ASSIGN rr_table_data->* TO <ft_table>.
    IF <ft_table> IS ASSIGNED.

      lo_csv_converter = cl_rsda_csv_converter=>create( i_separator = cv_sep ).

      " Pasar datos de entrada a la estructura de formato
      LOOP AT mt_plain_data INTO lv_line.

        CLEAR: <fs_line>.
        " Formatear campos configurados
        format_from_line( CHANGING cv_line = lv_line ).

        " Convertir CSV a estructura DDIC
        lo_csv_converter->csv_to_structure(
            EXPORTING i_data   = lv_line
            IMPORTING e_s_data = <fs_line> ).

        IF <fs_line> IS NOT INITIAL.
          INSERT <fs_line> INTO TABLE <ft_table>.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


METHOD set_ddic_format.


    DATA: lt_dfies TYPE TABLE OF dfies,
          ls_dfies TYPE dfies,
          ls_field TYPE ty_field.

    " Recuperar campos del tipo de datos de entrada
    " => Se espera una Estructura del diccionario
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = iv_typename
      TABLES
        dfies_tab = lt_dfies
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    " Contar campos actuales
    IF mt_field[] IS NOT INITIAL.
      SORT mt_field BY pos DESCENDING.
      READ TABLE mt_field INTO ls_field INDEX 1.
      rv_num_fields = ls_field-pos.
    ENDIF.
    SORT mt_field BY pos ASCENDING.

    LOOP AT lt_dfies INTO ls_dfies.

      READ TABLE mt_field TRANSPORTING NO FIELDS
        WITH KEY name = ls_dfies-fieldname.
      IF sy-subrc NE 0.

        rv_num_fields = rv_num_fields + 1.

        CLEAR: ls_field.
        ls_field-pos       = rv_num_fields.
        ls_field-name      = ls_dfies-fieldname.
        ls_field-rollname  = ls_dfies-rollname.
        ls_field-leng      = ls_dfies-leng.
        ls_field-outputlen = ls_dfies-outputlen.
        ls_field-decimals  = ls_dfies-decimals.
        ls_field-datatype  = ls_dfies-datatype.

        APPEND ls_field TO mt_field.

      ENDIF.

    ENDLOOP.

    IF rv_num_fields GT 0.
      mv_ddic_type = iv_typename.
    ENDIF.

  ENDMETHOD.


METHOD set_format_date.

    FIELD-SYMBOLS: <fs_field> TYPE ty_field.

    " Capturar campo configurado
    READ TABLE mt_field ASSIGNING <fs_field> WITH KEY name = iv_fieldname.
    IF sy-subrc EQ 0.
      <fs_field>-has_format      = cs_format-date.
      <fs_field>-format_dat-mask = iv_mask.
      <fs_field>-format_dat-sep  = iv_sep.
    ENDIF.

  ENDMETHOD.


METHOD set_format_number.

    FIELD-SYMBOLS: <fs_field> TYPE ty_field.

    " Capturar campo configurado
    READ TABLE mt_field ASSIGNING <fs_field> WITH KEY name = iv_fieldname.
    IF sy-subrc EQ 0.
      <fs_field>-has_format               = cs_format-number.
      <fs_field>-format_num-longitud      = iv_longitud.
      <fs_field>-format_num-decimales     = iv_decimales.
      <fs_field>-format_num-sep_decimales = iv_sep_decimales.
      <fs_field>-format_num-sep_miles     = iv_sep_miles.
      <fs_field>-format_num-sign          = iv_sign.
      <fs_field>-format_num-alpha         = iv_alpha.
    ENDIF.

  ENDMETHOD.


METHOD set_format_value_map.

    DATA: ls_map TYPE ty_format_map.
    FIELD-SYMBOLS: <fs_field> TYPE ty_field.

    " Capturar campo configurado
    READ TABLE mt_field ASSIGNING <fs_field> WITH KEY name = iv_fieldname.
    IF sy-subrc EQ 0.
      <fs_field>-has_format = cs_format-map.
      " Los campos de mapeo se guardan en una tabla separada
      ls_map-fieldname = iv_fieldname.
      ls_map-sap_value = iv_sap_value.
      ls_map-csv_value = iv_csv_value.
      APPEND ls_map TO mt_format_map.
    ENDIF.

  ENDMETHOD.


METHOD set_manual_format.

    DATA: lt_dd04l  TYPE TABLE OF dd04l,
          ls_dd04l  TYPE dd04l,
          ls_manual TYPE zst_csv_field,
          ls_field  TYPE ty_field.

    CLEAR: rv_num_fields.

    IF it_manual[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_dd04l FROM dd04l
        FOR ALL ENTRIES IN it_manual
        WHERE rollname EQ it_manual-rollname.
    ENDIF.

    " Contar campos actuales
    IF mt_field[] IS NOT INITIAL.
      SORT mt_field BY pos DESCENDING.
      READ TABLE mt_field INTO ls_field INDEX 1.
      rv_num_fields = ls_field-pos.
    ENDIF.
    SORT mt_field BY pos ASCENDING.

    LOOP AT it_manual INTO ls_manual.

      " Comprobar que el Tipo de Datos existe en el DDIC de SAP
      READ TABLE lt_dd04l INTO ls_dd04l
        WITH KEY rollname = ls_manual-rollname.
      IF sy-subrc EQ 0.

        " Añadir solo si no existe un campo con el mismo nombre
        READ TABLE mt_field TRANSPORTING NO FIELDS
          WITH KEY name = ls_manual-name.
        IF sy-subrc NE 0.

          rv_num_fields = rv_num_fields + 1.

          CLEAR: ls_field.
          ls_field-pos       = rv_num_fields.
          ls_field-name      = ls_manual-name.
          ls_field-rollname  = ls_manual-rollname.
          ls_field-leng      = ls_dd04l-leng.
          ls_field-outputlen = ls_dd04l-outputlen.
          ls_field-decimals  = ls_dd04l-decimals.
          ls_field-datatype  = ls_dd04l-datatype.

          APPEND ls_field TO mt_field.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


METHOD to_csv.

    DATA: lo_csv_converter TYPE REF TO cl_rsda_csv_converter,
          lr_struc_data    TYPE REF TO data.
    " No usar STRING ya que el Conversor necesita una variable con longitud
    " definida. Se pre-fijan 10.000 carácteres por línea, a priori suficiente.
    DATA: lv_line          TYPE c LENGTH 10000.

    FIELD-SYMBOLS: <ft_table_in>  TYPE ANY TABLE,
                   <fs_line_in>   TYPE any,
                   <fs_line_comp> TYPE any.

    CLEAR: rv_lines.

    " Crear un Tipo de datos con los componentes obtenidos de la estructura
    " de formato configurada en el método SET_DDIC_FORMAT.
    lr_struc_data = create_structure( ).
    ASSIGN lr_struc_data->* TO <fs_line_comp>.

    " Assignar tabla de datos de entrada
    ASSIGN ir_table_data->* TO <ft_table_in>.
    IF <ft_table_in> IS ASSIGNED.

      lo_csv_converter = cl_rsda_csv_converter=>create( i_separator = cv_sep ).

      " Pasar datos de entrada a la estructura de formato
      LOOP AT <ft_table_in> ASSIGNING <fs_line_in>.

        CLEAR: <fs_line_comp>, lv_line.

        " Solo pasar datos de los campos que concuerdan con
        " la estructura DDIC de formato.
        MOVE-CORRESPONDING <fs_line_in> TO <fs_line_comp>.

        " Formatear datos con la configuración de formato fijada
        format_to_line( CHANGING cr_line = lr_struc_data ).

        " Convertir línea a CSV
        lo_csv_converter->structure_to_csv(
          EXPORTING i_s_data = <fs_line_comp>
          IMPORTING e_data   = lv_line ).

        IF lv_line IS NOT INITIAL.
          APPEND lv_line TO mt_plain_data.
          rv_lines = rv_lines + 1.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


METHOD upload.

    DATA: lo_csv TYPE REF TO zcl_util_csv.

    IF it_manual[] IS INITIAL AND iv_typename IS INITIAL.
      " No se ha indicado ningún formato de datos
      RETURN.
    ENDIF.

    " Crear objeto utilidad CSV
    lo_csv = zcl_util_csv=>create( iv_filepath = iv_filepath ).

    " Cargar datos aplanados
    IF iv_local IS NOT INITIAL.
      lo_csv->upload_local( ).
      lo_csv->mv_source = abap_true.
    ELSEIF iv_server IS NOT INITIAL.
      lo_csv->upload_server( ).
      lo_csv->mv_source = abap_false.
    ENDIF.

    " Establecer formato datos
    IF iv_typename IS NOT INITIAL.
      lo_csv->set_ddic_format( iv_typename = iv_typename ).
    ENDIF.
    IF it_manual[] IS NOT INITIAL.
      lo_csv->set_manual_format( it_manual = it_manual ).
    ENDIF.

    " Pasar datos aplanados CSV a tabla con formato configurado
    lo_csv->from_csv( RECEIVING rr_table_data = rr_table_data ).

  ENDMETHOD.


METHOD upload_local.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = mv_filepath
        filetype                = 'ASC'
        has_field_separator     = 'X'
        codepage                = iv_codepage
      IMPORTING
        filelength              = mv_file_length
      CHANGING
        data_tab                = mt_plain_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.

  ENDMETHOD.


METHOD upload_server.

    DATA: lv_line  TYPE string,
          lv_subrc TYPE sysubrc.


    OPEN DATASET mv_filepath FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    lv_subrc = sy-subrc.
    IF lv_subrc EQ 0.

      DO.
        CLEAR: lv_line.
        READ DATASET mv_filepath INTO lv_line.
        IF sy-subrc <> 0.
          EXIT.
        ELSE.
          APPEND lv_line TO mt_plain_data.
        ENDIF.

      ENDDO.
      CLOSE DATASET mv_filepath.
    ENDIF.

  ENDMETHOD.


METHOD ZIF_UTIL_CSV_DOWNLOAD~CONVERT_TO_CSV.

    " Pasar tabla con formato configurado a datos aplanados CSV
    to_csv( ir_table_data = ir_table_data ).

  ENDMETHOD.


METHOD zif_util_csv_download~download.

    CLEAR: rv_lines_downloaded.

    " Descargar datos aplanados
    IF mv_source IS NOT INITIAL. " 'X' => Local
      rv_lines_downloaded = download_local( iv_codepage = iv_codepage ).
    ELSE.                        " ''  => Servidor
      rv_lines_downloaded = download_server( ).
    ENDIF.

  ENDMETHOD.


METHOD zif_util_csv_download~get_csv.

    rt_csv[] = mt_plain_data[].

  ENDMETHOD.


METHOD zif_util_csv_download~set_format_date.

    set_format_date( iv_fieldname = iv_fieldname
                     iv_mask      = iv_mask
                     iv_sep       = iv_sep ).

  ENDMETHOD.


METHOD zif_util_csv_download~set_format_number.

    set_format_number( iv_fieldname     = iv_fieldname
                       iv_longitud      = iv_longitud
                       iv_decimales     = iv_decimales
                       iv_sep_decimales = iv_sep_decimales
                       iv_sep_miles     = iv_sep_miles
                       iv_sign          = iv_sign
                       iv_alpha         = iv_alpha ).

  ENDMETHOD.


METHOD zif_util_csv_download~set_format_value_map.

    set_format_value_map( iv_fieldname = iv_fieldname
                          iv_sap_value = iv_sap_value
                          iv_csv_value = iv_csv_value ).

  ENDMETHOD.


METHOD ZIF_UTIL_CSV_UPLOAD~CONVERT_FROM_CSV.

    " Pasar datos aplanados CSV a tabla con formato configurado
    rr_table_data = me->from_csv( ).

  ENDMETHOD.


METHOD zif_util_csv_upload~set_format_date.

    set_format_date( iv_fieldname = iv_fieldname
                     iv_mask      = iv_mask
                     iv_sep       = iv_sep ).

  ENDMETHOD.


METHOD zif_util_csv_upload~set_format_number.

    set_format_number( iv_fieldname     = iv_fieldname
                       iv_longitud      = iv_longitud
                       iv_decimales     = iv_decimales
                       iv_sep_decimales = iv_sep_decimales
                       iv_sep_miles     = iv_sep_miles
                       iv_sign          = iv_sign
                       iv_alpha         = iv_alpha ).

  ENDMETHOD.


METHOD zif_util_csv_upload~set_format_value_map.

    set_format_value_map( iv_fieldname = iv_fieldname
                          iv_sap_value = iv_sap_value
                          iv_csv_value = iv_csv_value ).

  ENDMETHOD.
ENDCLASS.
