*"* local class implementation for public class
*"* CL_CI_TEST_SELECT
*"* use this source file for the implementation part of
class LCX_EXCEPTION definition final inheriting from CX_NO_CHECK.
  public section.
    data:
      INCLUDE type SOBJ_NAME,
      LINE    type TOKEN_ROW,
      COLUMN  type TOKEN_COL.
    methods CONSTRUCTOR importing
                          P_INCLUDE type SOBJ_NAME
                          P_LINE    type TOKEN_ROW
                          P_COLUMN  type TOKEN_COL.
endclass.                    "cx_exception DEFINITION

class  LCL_TEST definition deferred.

class ZCL_CI_002 definition local friends LCL_TEST.

class LCX_EXCEPTION implementation.
  method CONSTRUCTOR.
    SUPER->CONSTRUCTOR( ).
    INCLUDE  = P_INCLUDE.
    LINE     = P_LINE.
    COLUMN   = P_COLUMN.
  endmethod.
endclass.
