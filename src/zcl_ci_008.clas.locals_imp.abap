*"* local class implementation for public class
*"* CL_CI_TEST_INCLUDE
*"* use this source file for the implementation part of
*"* local helper classes


class LCL_SETTINGS definition final.
  public section.
    interfaces IF_PRETTY_PRINTER_SETTINGS.
    methods CONSTRUCTOR importing P_CASE type T_CASE P_INDENT type SYCHAR01.
  private section.
    data:
      CASE   type T_CASE,
      INDENT type SYCHAR01.
endclass.


class LCL_SETTINGS implementation.

  method CONSTRUCTOR.
    INDENT = P_INDENT.
    CASE   = P_CASE.
  endmethod.


  method IF_PRETTY_PRINTER_SETTINGS~GET_CASE_MODE.

    case ABAP_TRUE.
      when CASE-NONE.
        CASE_MODE = IF_PRETTY_PRINTER_SETTINGS=>CO_CASE_MODE_EMPTY.
      when CASE-LOWER.
        CASE_MODE = IF_PRETTY_PRINTER_SETTINGS=>CO_CASE_MODE_LOWER.
      when CASE-UPPER.
        CASE_MODE = IF_PRETTY_PRINTER_SETTINGS=>CO_CASE_MODE_UPPER.
      when CASE-KEY_UPPER.
        CASE_MODE = IF_PRETTY_PRINTER_SETTINGS=>CO_CASE_MODE_HIKEY.
      when CASE-KEY_LOWER.
        CASE_MODE = IF_PRETTY_PRINTER_SETTINGS=>CO_CASE_MODE_LOKEY.
    endcase.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method CL_PRETTY_PRINTER_WB_SETTINGS->IF_PRETTY_PRINTER_SETTINGS~GET_INDENT_MODE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] INDENT_MODE                    TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IF_PRETTY_PRINTER_SETTINGS~GET_INDENT_MODE.

    INDENT_MODE = IF_PRETTY_PRINTER_SETTINGS=>CO_INDENT.
    if INDENT = ABAP_TRUE.
      INDENT_MODE = IF_PRETTY_PRINTER_SETTINGS=>CO_INDENT_NO.
    else.
      INDENT_MODE = IF_PRETTY_PRINTER_SETTINGS=>CO_INDENT.
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method CL_PRETTY_PRINTER_WB_SETTINGS->IF_PRETTY_PRINTER_SETTINGS~GET_LINE_LENGTH
* +-------------------------------------------------------------------------------------------------+
* | [<-()] LENGTH                         TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IF_PRETTY_PRINTER_SETTINGS~GET_LINE_LENGTH.
    LENGTH = 255.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method CL_PRETTY_PRINTER_WB_SETTINGS->IF_PRETTY_PRINTER_SETTINGS~GET_SOURCE_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] SOURCE_TYPE                    TYPE        TROBJTYPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IF_PRETTY_PRINTER_SETTINGS~GET_SOURCE_TYPE.
    SOURCE_TYPE = IF_PRETTY_PRINTER_SETTINGS=>CO_SOURCE_TYPE_ABAP.
  endmethod.

endclass.
