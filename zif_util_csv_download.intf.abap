interface ZIF_UTIL_CSV_DOWNLOAD
  public .


  methods SET_FORMAT_VALUE_MAP
    importing
      !IV_FIELDNAME type FIELDNAME
      !IV_SAP_VALUE type TEXT255
      !IV_CSV_VALUE type TEXT255 .
  type-pools ABAP .
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
  methods CONVERT_TO_CSV
    importing
      !IR_TABLE_DATA type ref to DATA .
  methods DOWNLOAD
    importing
      !IV_CODEPAGE type ABAP_ENCODING default SPACE
    returning
      value(RV_LINES_DOWNLOADED) type INT4 .
  methods GET_CSV
    returning
      value(RT_CSV) type STRINGTAB .
endinterface.
