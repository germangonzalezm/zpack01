class ZCL_CI_006 definition
  public
  inheriting from CL_CI_TEST_SCAN
  create public .

*"* public components of class ZCL_CI_006
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .

  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods RUN
    redefinition .
protected section.
*"* protected components of class ZCL_CI_006
*"* do not include other source files here!!!

  types T_SEARCH_STRING type SYCHAR72 .

  data COMMENT_MODE type FLAG .
  data LITERAL_MODE type FLAG .
  data MSGKIND      type SYCHAR01.
  data SEARCH_STRINGS type SCI_SRCHSTR .
private section.

*"* private components of class ZCL_CI_006
*"* do not include other source files here!!!
  constants C_MY_NAME type SEOCLSNAME value 'ZCL_CI_006' ##NO_TEXT.
  constants MCODE_0001 type SCI_ERRC value '0001' ##NO_TEXT ##NEEDED.
ENDCLASS.



CLASS ZCL_CI_006 IMPLEMENTATION.


method CONSTRUCTOR .
  SUPER->CONSTRUCTOR( ).
  DESCRIPTION        = '6.  Inexistencia de sentencias SELECT * en programa'(000).
  VERSION            = '001'.
  CATEGORY           = 'ZCL_CI_CATEGORY_CYT'.
  POSITION           = '999'.
  MSGKIND            = 'N'.
*  HAS_ATTRIBUTES     = C_TRUE.
*  ATTRIBUTES_OK      = C_FALSE.
*  REMOTE_ENABLED     = ABAP_TRUE.
*  REMOTE_RFC_ENABLED = ABAP_TRUE.
  USES_CHECKSUM      = ABAP_TRUE.
endmethod.                    "CONSTRUCTOR


method GET_ATTRIBUTES.
  export LITERAL_MODE   = LITERAL_MODE
         COMMENT_MODE   = COMMENT_MODE
         SEARCH_STRINGS = SEARCH_STRINGS
         MSGKIND        = MSGKIND
         to data buffer P_ATTRIBUTES.
endmethod.                    "GET_ATTRIBUTES


method GET_MESSAGE_TEXT .
  data:
    L_CODE type SCI_ERRC.

  if P_TEST <> MYNAME or P_CODE = C_CODE_NOT_REMOTE_ENABLED.
    SUPER->GET_MESSAGE_TEXT(
                exporting P_TEST = P_TEST P_CODE = P_CODE
                importing P_TEXT = P_TEXT ).
    return.
  endif.
  L_CODE = P_CODE.
  shift L_CODE left deleting leading SPACE.
  P_TEXT = 'Suchstring Nummer &N gefunden in: &1'(101).
  replace first occurrence of '&N' in P_TEXT with L_CODE.

endmethod.                    "GET_MESSAGE_TEXT


method IF_CI_TEST~QUERY_ATTRIBUTES .
  data:
    L_ATTRIBUTES     type SCI_ATTTAB,
    L_ATTRIBUTE      like line of L_ATTRIBUTES,
    L_COMMENT_MODE   like COMMENT_MODE,
    L_LITERAL_MODE   like LITERAL_MODE,
    L_MSGKIND        like MSGKIND,
    L_SEARCH_STRINGS like SEARCH_STRINGS,
    L_SEARCH_STRING  like line of SEARCH_STRINGS,
    L_STRLEN         type I,
    L_MESSAGE(72)    type C.

  define FILL_ATT.
    get reference of &1 into L_ATTRIBUTE-REF.
    L_ATTRIBUTE-TEXT = &2.
    L_ATTRIBUTE-KIND = &3.
    append L_ATTRIBUTE to L_ATTRIBUTES.
  end-of-definition.

  L_SEARCH_STRINGS = SEARCH_STRINGS.
  L_MSGKIND        = MSGKIND.
  L_COMMENT_MODE   = COMMENT_MODE.
  L_LITERAL_MODE   = LITERAL_MODE.

  L_STRLEN = 0.

*-- fill attribute table
*FILL_ATT L_CHAR    'Char'(211)       ' '.
*FILL_ATT L_NUM     'Numerisch'(212)  ' '.
*FILL_ATT L_STRUCT  'Struktur'(213)   ' '.

  FILL_ATT L_COMMENT_MODE   'Kommentare'(202)       'C'.
  FILL_ATT L_LITERAL_MODE   'Literale'(203)         'C'.
  FILL_ATT L_MSGKIND        'Art der Meldung'(205)  ' '.
  FILL_ATT L_SEARCH_STRINGS 'Suchstring(s)'(204)    ' '.

*-- only search with 2 letters minimum
  do.
    L_STRLEN = 999999.
    if CL_CI_QUERY_ATTRIBUTES=>GENERIC(
                          P_NAME       = 'ZCL_CI_006'
                          P_TITLE      = 'Meine Selektionen'(005)
                          P_ATTRIBUTES = L_ATTRIBUTES
                          P_MESSAGE    = L_MESSAGE
                          P_DISPLAY    = P_DISPLAY ) = 'X'.
*-- = 'X' --> 'Exit' Button pressed on PopUp
      return.
    endif.
    if L_SEARCH_STRINGS is initial.
      L_MESSAGE = 'Bitte mindestens einen Suchstring angeben'(902).
      L_STRLEN = 0.
    else.
      loop at L_SEARCH_STRINGS into L_SEARCH_STRING.
        translate L_SEARCH_STRING using '* + '.
        condense L_SEARCH_STRING no-gaps.
        if STRLEN( L_SEARCH_STRING ) < L_STRLEN.
          L_STRLEN = STRLEN( L_SEARCH_STRING ).
        endif.
      endloop.

      if L_STRLEN < 2.
        L_MESSAGE = 'Bitte mindestens 2 Buchstaben eingeben'(901).
      else.
        case L_MSGKIND.
          when 'E' or 'W' or 'N'.
            exit.
          when others.
            L_MESSAGE = 'Meldungsart nur E, W oder N'(903).
        endcase.
      endif.
    endif.
  enddo.

  SEARCH_STRINGS = L_SEARCH_STRINGS.
  MSGKIND        = L_MSGKIND.
  LITERAL_MODE   = L_LITERAL_MODE.
  COMMENT_MODE   = L_COMMENT_MODE.
  ATTRIBUTES_OK  = C_TRUE.

endmethod.                    "IF_CI_TEST~QUERY_ATTRIBUTES


method PUT_ATTRIBUTES .

  import literal_mode   = literal_mode
         comment_mode   = comment_mode
         search_strings = search_strings
         msgkind        = msgkind
         from data buffer p_attributes.

endmethod.                    "PUT_ATTRIBUTES


method run .

  data:
    l_include       type sobj_name,
    l_row           type token_row,
    l_column        type token_col,
    l_tokennr       like statement_wa-from,
    l_code          type sci_errc,
    l_search_string like line of search_strings,
    l_position      type i,
    l_checksum      type sci_crc64.
  data: lv_result type c value is initial.

  append initial line to search_strings.
  search_strings[ 1 ] = 'SELECT'.

  if search_strings is initial.
    return.
  endif.

  if ref_scan is initial.
    check get( ) = 'X'.
  endif.

  check ref_scan->subrc = 0.

*-- loop at all tokens
  loop at ref_scan->statements into statement_wa.
    check statement_wa-from <= statement_wa-to.
    l_position = sy-tabix.
    if statement_wa-type = 'S' or
       statement_wa-type = 'P'.
      check comment_mode = 'X'.
    endif.

    loop at ref_scan->tokens into token_wa
           from statement_wa-from to statement_wa-to.
      l_tokennr = sy-tabix.
      if token_wa-type = 'S'.
        check literal_mode = 'X'.
      endif.

      loop at search_strings into l_search_string.
*-- does ABAP-string contain search-string ?
        if token_wa-str cp l_search_string.
*          lv_result = 'X'.
*          exit.
          if statement_wa-from + 1 < statement_wa-to .
            if ref_scan->tokens[ statement_wa-from + 1 ]-str eq '*' .


              unpack sy-tabix to l_code(4).
              l_include = get_include( ).
              l_row     = get_line_abs( l_tokennr ).
              l_column  = get_column_abs( l_tokennr ).
              clear l_checksum.
              cl_ci_provide_checksum=>gen_chksum_from_chars( exporting p_param = l_search_string changing  p_crc_value = l_checksum exceptions parameter_error = 0 ).
              get_stmt_checksum( exporting p_position = l_position changing p_checksum = l_checksum exceptions error = 1 ).
              inform( p_sub_obj_type = c_type_include
                      p_sub_obj_name = l_include
                      p_position     = l_position
                      p_line         = l_row
                      p_column       = l_column
                      p_kind         = 'E'
*                      p_kind         = msgkind
                      p_test         = c_my_name
                      p_code           = l_code
                      p_suppress     = '"#EC CI_NOFIND '
                      p_param_1      = token_wa-str
                      p_checksum_1   = l_checksum-i1 ).
              exit.
            endif.
          endif.

        endif.     "l_strpos > l_pos
      endloop.
    endloop.
  endloop.
*  if lv_result is initial.
*    unpack sy-tabix to l_code(4).
*    l_include = get_include( ).
*    l_row     = get_line_abs( l_tokennr ).
*    l_column  = get_column_abs( l_tokennr ).
*    clear l_checksum.
*    cl_ci_provide_checksum=>gen_chksum_from_chars( exporting p_param = l_search_string changing  p_crc_value = l_checksum exceptions parameter_error = 0 ).
*    get_stmt_checksum( exporting p_position = l_position changing p_checksum = l_checksum exceptions error = 1 ).
*    inform( p_sub_obj_type = c_type_include
*            p_sub_obj_name = l_include
*            p_position     = l_position
*            p_line         = l_row
*            p_column       = l_column
**            p_kind         = msgkind
*            p_kind         = 'E'
*            p_test         = c_my_name
*            p_code         = l_code
*            p_suppress     = '"#EC CI_NOFIND '
*            p_param_1      = token_wa-str
*            p_checksum_1   = l_checksum-i1 ).
*    exit.
*
*  endif.
endmethod.                    "RUN
ENDCLASS.
