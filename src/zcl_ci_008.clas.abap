class ZCL_CI_008 definition
  public
  inheriting from CL_CI_TEST_SCAN
  create public .

*"* public components of class ZZCL_CI_008
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .

  methods GET_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods RUN
    redefinition .
  protected section.
  private section.

    constants C_CODE_DIFF type SCI_ERRC value 'PP_ERR'.

    data CASE   type T_CASE.
    data INDENT type SYCHAR01.

ENDCLASS.



CLASS ZCL_CI_008 IMPLEMENTATION.


  method CONSTRUCTOR .

    SUPER->CONSTRUCTOR( ).

    DESCRIPTION    = '8.  Check Pretty Print State'(001).
    CATEGORY       = 'ZCL_CI_CATEGORY_CYT'.
    HAS_ATTRIBUTES = ABAP_TRUE.
    ATTRIBUTES_OK  = ABAP_TRUE.
    USES_CHECKSUM  = ABAP_TRUE.

    SCID_FILL_MESSAGE C_CODE_DIFF C_WARNING  'Inkorrekter Pretty-Print-Zustand'(002)   '' ''.

    CASE-NONE = ABAP_TRUE.

  endmethod.                    "CONSTRUCTOR


  method GET_ATTRIBUTES.
    export CASE = CASE INDENT = INDENT to data buffer P_ATTRIBUTES.
  endmethod.


  method IF_CI_TEST~QUERY_ATTRIBUTES.
    data:
      L_ATTRIBUTES type SCI_ATTTAB,
      L_ATTRIBUTE  like line of L_ATTRIBUTES,
      L_INDENT     like INDENT,
      L_CASE       like CASE.

    define FILL_ATT.
      get reference of &1 into L_ATTRIBUTE-REF.
      L_ATTRIBUTE-TEXT = &2.
      L_ATTRIBUTE-KIND = &3.
      append L_ATTRIBUTE to L_ATTRIBUTES.
    end-of-definition.

    L_INDENT = INDENT.
    L_CASE   = CASE.

    FILL_ATT L_INDENT          'Einrücken'(101)                     'C'.
    FILL_ATT SY-INDEX          'Groß/Kleinkonvertierung'(102)       'G'.
    FILL_ATT L_CASE-NONE       'Keine'(103)                         'R'.
    FILL_ATT L_CASE-LOWER      'Kleinschreibung'(104)               'R'.
    FILL_ATT L_CASE-UPPER      'Großschreibung'(105)                'R'.
    FILL_ATT L_CASE-KEY_UPPER  'Schlüsselwort groß '(106)           'R'.
    FILL_ATT L_CASE-KEY_LOWER  'Schlüsselwort klein'(107)           'R'.

    if CL_CI_QUERY_ATTRIBUTES=>GENERIC(
                    P_NAME       = MYNAME
                    P_TITLE      = 'Einstellungen der Prüfung'(100)
                    P_ATTRIBUTES = L_ATTRIBUTES
                    P_DISPLAY    = P_DISPLAY ) <> ABAP_TRUE.
      INDENT = L_INDENT.
      CASE   = L_CASE.
    endif.
  endmethod.


  method PUT_ATTRIBUTES.
    import CASE = CASE INDENT = INDENT from data buffer P_ATTRIBUTES.
  endmethod.


  method run .
    data:
      l_includes   type standard table of program,
      l_source_in  type swbse_max_line_tab,
      l_source_out type swbse_max_line_tab,
      l_checksum   type sci_crc64.

    check get( ) = 'X'.

    data(l_settings) = new lcl_settings( p_case = case p_indent = indent ).

    case = '    X'.
    indent = 'X'.

    loop at ref_scan->levels assigning field-symbol(<l_level>) where type = 'P'.
      check <l_level>-name+30(2) <> 'CP'.
      check <l_level>-name+30(2) <> 'CU'.
      check <l_level>-name+30(2) <> 'CO'.
      check <l_level>-name+30(2) <> 'CI'.
      check <l_level>-name+30(2) <> 'IP'.
      check <l_level>-name+30(2) <> 'IU'.
      check <l_level>-name np 'L*UXX'.
      insert <l_level>-name into table l_includes.
    endloop.

    loop at l_includes assigning field-symbol(<l_include>).
      read report <l_include> into l_source_in.

      call function 'PRETTY_PRINTER'
        exporting
          inctoo   = abap_false
          settings = l_settings
        tables
          otext    = l_source_in
          ntext    = l_source_out.

      if l_source_in <> l_source_out.
        loop at l_source_in assigning field-symbol(<l_source_in>).
          data(l_tabix) = sy-tabix.
          read table l_source_out index l_tabix assigning field-symbol(<l_source_out>).
          if <l_source_in> <> <l_source_out>.
            clear l_checksum.
            concatenate indent case into data(l_params).
            cl_ci_provide_checksum=>gen_chksum_from_string( exporting p_param          = l_params
                                                           changing  p_crc_value      = l_checksum
                                                           exceptions parameter_error = 1 ). assert sy-subrc = 0.
            cl_ci_provide_checksum=>gen_chksum_from_chars( exporting p_param          = <l_include>
                                                           changing  p_crc_value      = l_checksum
                                                           exceptions parameter_error = 1 ). assert sy-subrc = 0.
            inform( p_sub_obj_type = 'PROG'
                    p_sub_obj_name = <l_include>
                    p_line         = l_tabix
                    p_column       = 0
                    p_kind         = c_warning
                    p_test         = myname
                    p_code         = c_code_diff
                    p_checksum_1   = l_checksum-i1 ).
            exit.
          endif.
        endloop.
      endif.

    endloop.

  endmethod.
ENDCLASS.
