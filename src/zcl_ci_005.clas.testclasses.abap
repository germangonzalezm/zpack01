constants: c_Dev_System type sysysid value 'uIA'.
constants: c_Ref_System type sysysid value 'Y7A'.

class th_Analyser definition for testing.

  public section.
    types:
      begin of ty_Unsafe_Query,
        statement_Ndx        type sytabix,
        table_Var_Name       type string,
        has_External_Content type abap_Bool,
      end of ty_Unsafe_Query,
      ty_Unsafe_Queries type standard table of ty_Unsafe_Query
                             with key statement_Ndx.

    class-methods:
      locate_Unsafe_Queries
        importing   source_Lines         type string_Table
                    safe_Procedure_Names type string_Hashed_Table optional
        returning   value(result)        type ty_Unsafe_Queries
        raising     cx_Dynamic_Check,

      locate_Unsafe_Queries1
        importing program              type program
                  p_Sysid              type sysysid optional
                  safe_Procedure_Names type string_Hashed_Table optional
        returning value(result)        type ty_Unsafe_Queries
        raising   cx_Dynamic_Check.

  private section.
    class-methods:
      scan_Source
        importing   source_Lines  type string_Table
        returning   value(result) type ref to scanned_Source
        raising     cx_Dynamic_Check,
                    scan_Source1
        importing   srcid         type scr_Source_Id
                    program       type program
        returning   value(result) type ref to scanned_Source
        raising     cx_Dynamic_Check.

endclass.


class td_Listener_Spy definition for testing.

  public section.

    interfaces listener.
    data:
      unsafe_Queries type th_Analyser=>ty_Unsafe_Queries read-only.

endclass.



class th_Analyser implementation.

  method locate_Unsafe_Queries.
    data:
      scanned_Source type ref to scanned_Source,
      listener_Spy   type ref to td_Listener_Spy,
      analysis       type ref to for_All_Entries_Analysis.

    create object listener_Spy.
    create object analysis
      exporting
        listener             = listener_Spy
        safe_Procedure_Names = safe_Procedure_Names.

    scanned_Source = scan_Source( source_Lines ).
    analysis->examine( scanned_Source ).
    result = listener_Spy->unsafe_Queries.
  endmethod.


  method locate_Unsafe_Queries1.
    data:
      scanned_Source type ref to scanned_Source,
      listener_Spy   type ref to td_Listener_Spy,
      analysis       type ref to for_All_Entries_Analysis,
      l_Srcid        type scr_Source_Id.

    create object listener_Spy.
    create object analysis
      exporting
        listener             = listener_Spy
        safe_Procedure_Names = safe_Procedure_Names.
    if p_Sysid is not initial.
      l_Srcid = cl_Abap_Source_Id=>get( p_Sysid = p_Sysid p_Rfc_Mode = abap_True ).
    endif.

    scanned_Source = scan_Source1( srcid = l_Srcid program = program ).
    analysis->examine( scanned_Source ).
    result = listener_Spy->unsafe_Queries.
  endmethod.


  method scan_Source.
    data:
      tokens     type stokesx_Tab,
      statements type sstmnt_Tab,
      levels     type slevel_Tab.
    scan abap-source source_Lines
      levels into !levels
      tokens into !tokens
      statements into !statements
      with analysis.
    if ( 0 ne sy-subrc or statements is initial ).
      cl_Abap_Unit_Assert=>abort( 'cannot tokenize source' ).
    endif.

    create object result
      exporting
        tokens     = tokens
        statements = statements
        levels     = levels.
  endmethod.


  method scan_Source1.

    if srcid is initial.
      select single * from trdir into @data(trdir_Record_Reference) where name = @program.
      data(reference_Include) = cl_Ci_Source_Include=>create( p_Trdir = trdir_Record_Reference ).
    endif.
    cl_Ci_Scan=>create(
      exporting p_Include      = reference_Include
                p_Noaunit      = ' '
                p_With_Pragmas = 'X'
                p_Srcid        = srcid
                p_Name         = program
      importing p_Ref          = data(reference_Scan) ).

    result =
      new scanned_Source(
        tokens     = reference_Scan->tokens
        statements = reference_Scan->statements
        levels     = reference_Scan->levels ).
  endmethod.

endclass.


class td_Listener_Spy implementation.

  method listener~handle_Unsafe_Query.
    data:
      unsafe_Query like line of me->unsafe_Queries.
    unsafe_Query-statement_Ndx = query_Ndx.
    unsafe_Query-table_Var_Name = table_Var_Name.
    unsafe_Query-has_External_Content = has_External_Content.
    insert unsafe_Query into table me->unsafe_Queries.
  endmethod.

endclass.

define mac_Append_To_Source_Lines.
  append &2 to me->source_Lines ##no_Text.
  if ( &1 <> lines( me->source_Lines ) ).
    cl_Abap_Unit_Assert=>fail( msg = &2 && &1 level = if_Aunit_Constants=>tolerable ).
  endif.
end-of-definition.

define mac_Append_Expectation.
  me->expected_Query-statement_Ndx = &1.
  me->expected_Query-table_Var_Name = &2.
  me->expected_Query-has_External_Content = &3.
  insert me->expected_Query into table me->expected_Queries.
end-of-definition.



class tc_Unsafe_Queries_Basic definition
  for testing risk level harmless.

  private section.
    data:
      source_Lines     type string_Table,
      expected_Query   type th_Analyser=>ty_Unsafe_Query,
      expected_Queries type th_Analyser=>ty_Unsafe_Queries,
      actual_Queries   type th_Analyser=>ty_Unsafe_Queries.

    methods:
      blame_Condition_Some_Case     for testing,
      blame_No_Subrc_Fetch_Into     for testing,
      blame_No_Subrc_Fetch_Append   for testing,
      blame_No_Subrc_Query          for testing,
      blame_After_Clear_Lines_Var   for testing,
      blame_After_Clear_Field_Symb  for testing,
      blame_After_Delete_Table      for testing,
      blame_After_Refresh_Table     for testing,
      blame_After_Call              for testing,
      blame_Clear_After_Full_Exit   for testing.

    constants:
      c_External type abap_Bool value abap_True,
      c_Local    type abap_Bool value abap_False.

endclass.


class tc_Unsafe_Queries_Basic implementation.

 method blame_Condition_Some_Case.
     mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'case sy-uname.                                    ',
      4 '  when `GANDALF`.                                 ',
      5 '    append initial line to tadir_Entries.         ',
      6 '  when `SAURON`.                                  ',
      7 '  " --- append initial line to tadir_Entries.     ',
      8 '  when others.                                    ',
      9 '    append initial line to tadir_Entries.         ',
     10 'endcase                                           ',
     11 'select * from tadir into table tadir_Entries      ',
     12 '  for all entries in tadir_Entries                ',
     13 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

  endmethod.


  method blame_No_Subrc_Fetch_Into.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: tadir_Cursor  type cursor.                  ',
      4 'open cursor with hold tadir_Cursor for            ',
      5 '  select * from tadir package size 10.            ',
      6 'fetch next cursor tadir_Cursor                    ',
      7 '  into table tadir_Entries.                       ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      6 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_1' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_1_E' ).

      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_No_Subrc_Fetch_Append.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: tadir_Cursor  type cursor.                  ',
      4 'open cursor with hold tadir_Cursor for            ',
      5 '  select * from tadir package size 10.            ',
      6 'fetch next cursor tadir_Cursor                    ',
      7 '  appending table tadir_Entries.                  ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      6 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_2' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_2_E' ).

      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_No_Subrc_Query.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'select * from tadir into table tadir_Entries.     ',
      4 'select * from tadir into table tadir_Entries      ',
      5 '  for all entries in tadir_Entries                ',
      6 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      4 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_3' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_3_E' ).

      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_After_Clear_Lines_Var.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 '!lines = lines( tadir_Entries[] ).                ',
      5 'clear lines.                                      ',
      6 'if ( !lines is not initial ).                     ',
      7 '  select * from tadir into table tadir_Entries    ',
      8 '    for all entries in tadir_Entries              ',
      9 '    where pgmid = tadir_Entries-pgmid.            ',
     10 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      7 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_4' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_4_E' ).

      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_After_Clear_Field_Symb.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'field-symbols: <entries> type standard table.     ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 'insert initial line into table tadir_Enries[].    ',
      5 'assign tadir_Entries to <entries>.                ',
      6 'clear <entries>.                                  ',
      7 'select * from tadir into table tadir_Entries      ',
      8 '  for all entries in tadir_Entries                ',
      9 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      7 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_5' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_5_E' ).

      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_After_Call.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'insert initial line into table tadir_Entries.     ',
      4 'cl_Tralla=>read( changing x = tadir_Entries       ',
      5 '                 exceptions others = 1 ).         ',
      6 'if ( 0 ne sy-subrc ).                             ',
      7 '  exit.                                           ',
      8 'endif.                                            ',
      9 'select * from tadir into table tadir_Entries      ',
     10 '  for all entries in tadir_Entries                ',
     11 '  where pgmid = tadir_Entries-pgmid.              '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      8 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_6' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_6_E' ).

      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Clear_After_Full_Exit.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( 1 eq 1 ).                                    ',
      4 '  if ( tadir_Entries is initial ).                ',
      5 '    leave program.                                ',
      6 '  else.                                           ',
      7 '    clear tadir_Entries.                          ',
      8 '    select * from tadir into table tadir_Entries  ',
      9 '      for all entries in tadir_Entries            ',
     10 '      where pgmid = tadir_Entries-pgmid.          ',
     11 '  endif.                                          ',
     12 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      8 'TADIR_ENTRIES' c_Local.

    " ##TODO March 2013
    " the implementation carries over the final flag from the sibling block
    " but the current block is NOT final only the parent block shall be
    " final which is computed by the state of the last sibling => rework
    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries
      level = if_Aunit_Constants=>tolerable ).

    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_7' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_7_E' ).

      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_After_Delete_Table.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 'insert initial line into table tadir_Entries.     ',
      5 '!lines = lines( tadir_Entries[] ).                ',
      6 'delete tadir_Entries[] where obj_Name is initial. ',
      7 'if ( !lines is not initial ).                     ',
      8 '  select * from tadir into table tadir_Entries    ',
      9 '    for all entries in tadir_Entries              ',
     10 '    where pgmid = tadir_Entries-pgmid.            ',
     11 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      8 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
  endmethod.


  method blame_After_Refresh_Table.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 '!lines = lines( tadir_Entries[] ).                ',
      5 'refresh tadir_Entries[].                          ',
      6 'if ( !lines is not initial ).                     ',
      7 '  select * from tadir into table tadir_Entries    ',
      8 '    for all entries in tadir_Entries              ',
      9 '    where pgmid = tadir_Entries-pgmid.            ',
     10 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      7 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_8' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_8_E' ).

      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.

endclass.


class tc_Loop_And_Queries_Heuristic definition
  for testing risk level harmless.

  private section.
    data:
      source_Lines     type string_Table,
      expected_Query   type th_Analyser=>ty_Unsafe_Query,
      expected_Queries type th_Analyser=>ty_Unsafe_Queries,
      actual_Queries   type th_Analyser=>ty_Unsafe_Queries.

    methods:
      pass_Do_Append          for testing,
      pass_Loop_Append        for testing,
      pass_Loop_If_Append     for testing,
      pass_Loop_Case_Append   for testing,
      blame_Loop_Append_Clear for testing.

endclass.


class tc_Loop_And_Queries_Heuristic implementation.

  method pass_Do_Append.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'do 5 times.                                       ',
      4 '  insert initial line into table tadir_Entries.   ',
      5 'enddo.                                            ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_9' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_9_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Loop_Append.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: source_Entries type standard table of tadir.',
      4 'source_Entries = cl_Repository=>get_Entries( ).   ',
      5 'loop at source_Entries transporting no fields.    ',
      6 '  insert initial line into table tadir_Entries.   ',
      7 'endloop.                                          ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_10' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_10_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Loop_If_Append.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: source_Entries type standard table of tadir.',
      4 'source_Entries = cl_Repository=>get_Entries( ).   ',
      5 'loop at source_Entries transporting no fields.    ',
      6 '  if ( sy-tabix eq 1 ).                           ',
      7 '    insert initial line into table tadir_Entries. ',
      8 '  else.                                           ',
      9 '    insert initial line into table tadir_Entries. ',
     10 '  endif.                                          ',
     11 'endloop.                                          ',
     12 'select * from tadir into table tadir_Entries      ',
     13 '  for all entries in tadir_Entries                ',
     14 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    check sy-sysid eq c_Dev_System.
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_11' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_11_E' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.


  method pass_Loop_Case_Append.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: source_Entries type standard table of tadir.',
      4 'source_Entries = cl_Repository=>get_Entries( ).   ',
      5 'loop at source_Entries transporting no fields.    ',
      6 '  case sy-tabix.                                  ',
      7 '    when others.                                  ',
      8 '      append initial line to tadir_Entries.       ',
      9 '  endcase.                                        ',
     10 'endloop.                                          ',
     11 'select * from tadir into table tadir_Entries      ',
     12 '  for all entries in tadir_Entries                ',
     13 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    check sy-sysid eq c_Dev_System.
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_12' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_12_E' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.


  method blame_Loop_Append_Clear.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: source_Entries type standard table of tadir.',
      4 'source_Entries = cl_Repository=>get_Entries( ).   ',
      5 'loop at source_Entries transporting no fields.    ',
      6 '  insert initial line into table tadir_Entries.   ',
      7 '  clear tadir_Entries.                            ',
      8 'endloop.                                          ',
      9 'select * from tadir into table tadir_Entries      ',
     10 '  for all entries in tadir_Entries                ',
     11 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      9 'TADIR_ENTRIES' abap_False.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    check sy-sysid eq c_Dev_System.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_13' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_13_E' ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
  endmethod.


endclass.


class tc_Safe_Queries_Basic definition
  for testing risk level harmless.

  private section.
    data:
      source_Lines     type string_Table,
      expected_Query   type th_Analyser=>ty_Unsafe_Query,
      expected_Queries type th_Analyser=>ty_Unsafe_Queries,
      actual_Queries   type th_Analyser=>ty_Unsafe_Queries.

    methods:
      pass_Condition_All_If         for testing,
      pass_Condition_All_If_Not     for testing,
      pass_Condition_All_When       for testing,
      pass_Content_Append           for testing,
      pass_Content_Collect          for testing,
      pass_Content_Insert           for testing,
      pass_Content_Value            for testing,
      pass_Content_Fetch            for testing,
      pass_Content_Query            for testing,
      pass_Content_Query_Delayed    for testing,
      pass_Content_Query_Host_Var   for testing,
      pass_Content_Query_Host_Var2  for testing,
      pass_Content_Split            for testing,
      pass_Delete_Adjacent          for testing,
      pass_Is_Initial               for testing,
      pass_Is_Initial_After_Not     for testing,
      pass_Is_Initial_Leave         for testing,
      pass_Is_Initial_Raising       for testing,
      pass_Is_Initial_Return        for testing,
      pass_Is_Initial_Style_Change  for testing,
      pass_Move_Lines_Var_If        for testing,
      pass_Read_Subrc_If            for testing,
      pass_Read_Subrc_Move_If       for testing,
      pass_Safe_Procedure           for testing,
      pass_Size_Probe_Describe_If   for testing,
      pass_Size_Probe_Descr_Tfill   for testing,
      pass_Size_Probe_Lines_If      for testing,
      pass_Size_Probe_Lines_Case    for testing,
      pass_Size_Probe_Lines_Check   for testing.


endclass.


class tc_Safe_Queries_Basic implementation.

  method pass_Condition_All_If.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( 1 = 2 ).                                     ',
      4 '  append initial line to tadir_Entries.           ',
      5 'else.                                             ',
      6 '  append initial line to tadir_Entries.           ',
      7 'endif.                                            ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

  endmethod.


  method pass_Condition_All_If_Not.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( not 1 = 2 ).                                 ',
      4 '  append initial line to tadir_Entries.           ',
      5 'else.                                             ',
      6 '  append initial line to tadir_Entries.           ',
      7 'endif.                                            ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

  endmethod.


  method pass_Condition_All_When.
     mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'case sy-uname.                                    ',
      4 '  when `GANDALF`.                                 ',
      5 '    append initial line to tadir_Entries.         ',
      6 '  when `SAURON`.                                  ',
      7 '    append initial line to tadir_Entries.         ',
      8 '  when others.                                    ',
      9 '    append initial line to tadir_Entries.         ',
     10 'endcase                                           ',
     11 'select * from tadir into table tadir_Entries      ',
     12 '  for all entries in tadir_Entries                ',
     13 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

  endmethod.


  method pass_Content_Append.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'append initial line to tadir_Entries.             ',
      4 'select * from tadir into table tadir_Entries      ',
      5 '  for all entries in tadir_Entries                ',
      6 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    check sy-sysid eq c_Dev_System.
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_14' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_14_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Content_Collect.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir type tadir.                           ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 'collect tadir into table tadir_Entries.           ',
      5 'select * from tadir into table tadir_Entries      ',
      6 '  for all entries in tadir_Entries                ',
      7 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).


    check sy-sysid eq c_Dev_System.
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_15' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_15_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Content_Insert.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'insert initial line into table tadir_Entries.     ',
      4 'select * from tadir into table tadir_Entries      ',
      5 '  for all entries in tadir_Entries                ',
      6 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_16' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_16_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Content_Value.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'tadir_Entries = VALUE #( (                        ',
      4 '  pgmid = ''R3TR''                                ',
      5 '  object = ''CLAS'' ) ).                          ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_17' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

*    " remote test (UIA=>Y7A): not possible in 7.0 system
*    if sy-sysid = c_Dev_System .
*      me->actual_Queries =
*        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_17_E' ).
*
*      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
*    endif.
  endmethod.


  method pass_Content_Fetch.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: tadir_Cursor  type cursor.                  ',
      4 'open cursor with hold tadir_Cursor for            ',
      5 '  select * from tadir package size 10.            ',
      6 'fetch next cursor tadir_Cursor                    ',
      7 '  into table tadir_Entries.                       ',
      8 'if ( 0 ne sy-dbcnt ).                             ',
      9 '  select * from tadir into table tadir_Entries    ',
     10 '    for all entries in tadir_Entries              ',
     11 '    where pgmid = tadir_Entries-pgmid.            ',
     12 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_18' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_18_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Content_Query.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'select * from tadir into table tadir_Entries.     ',
      4 'if ( 0 ne sy-dbcnt ).                             ',
      5 '  select * from tadir into table tadir_Entries    ',
      6 '    for all entries in tadir_Entries              ',
      7 '    where pgmid = tadir_Entries-pgmid.            ',
      8 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_19' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_19_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Content_Query_Delayed.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( sy-datum < `2011` ),                         ',
      4 '  select * from tadir into table tadir_Entries.   ',
      5 'else.                                             ',
      6 '  select distinct pgmid from tadir                ',
      7 '    into corresponding fields of table tadir_Entries.',
      8 'endif.                                            ',
      9 'if ( 0 ne sy-dbcnt ).                             ',
     10 '  select * from tadir into table tadir_Entries    ',
     11 '    for all entries in tadir_Entries              ',
     12 '    where pgmid = tadir_Entries-pgmid.            ',
     13 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_20' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_20_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Content_Query_Host_Var.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'select * from tadir into table @tadir_Entries.    ',
      4 'if ( 0 ne sy-dbcnt ).                             ',
      5 '  select * from tadir into table tadir_Entries    ',
      6 '    for all entries in tadir_Entries              ',
      7 '    where pgmid = tadir_Entries-pgmid.            ',
      8 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.


  method pass_Content_Query_Host_Var2.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'form read using c_Objects c_Findings.             ',
      3 'check c_Objects is not initial.                   ',
      4 'select * from satc_Wl_Object                      ',
      5 '  into table @data(c_Findings)                    ',
      6 '  for all entries in @c_Objects                   ',
      7 '    where worklist_Id = @c_Objects-worklist_Id.   ',
      8 'endform.                                          '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.


  method pass_Content_Split.
    mac_Append_To_Source_Lines:
      1 'report bla.                                          ',
      2 'data: tadir_Entries type standard table of tadir.    ',
      3 'split `WWW.SAP.COM` at `.` into table tadir_Entries. ',
      4 'select * from tadir into table tadir_Entries         ',
      5 '  for all entries in tadir_Entries                   ',
      6 '  where pgmid = tadir_Entries-pgmid.                 '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_21' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_21_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Delete_Adjacent.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'insert initial line into table tadir_Entries.     ',
      4 'delete adjacent duplicates from tadir_Entries[].  ',
      5 'if ( tadir_Entries is not initial ).              ',
      6 '  select * from tadir into table tadir_Entries    ',
      7 '    for all entries in tadir_Entries              ',
      8 '    where pgmid = tadir_Entries-pgmid.            ',
      9 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.

  method pass_Is_Initial.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( tadir_Entries is not initial ).              ',
      4 '  select * from tadir into table tadir_Entries    ',
      5 '    for all entries in tadir_Entries              ',
      6 '    where pgmid = tadir_Entries-pgmid.            ',
      7 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_22' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_22_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Is_Initial_After_Not.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( not tadir_Entries is initial ).              ',
      4 '  select * from tadir into table tadir_Entries    ',
      5 '    for all entries in tadir_Entries              ',
      6 '    where pgmid = tadir_Entries-pgmid.            ',
      7 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.

  method pass_Is_Initial_Style_Change.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( tadir_Entries[] is not initial ).            ',
      4 '  select * from tadir into table tadir_Entries    ',
      5 '    for all entries in tadir_Entries              ',
      6 '    where pgmid = tadir_Entries-pgmid.            ',
      7 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_23' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_23_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Is_Initial_Leave.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( tadir_Entries[] is initial ).                ',
      4 '  leave to screen 0.                              ',
      5 'endif.                                            ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_24' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_24_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Is_Initial_Raising.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( tadir_Entries[] is initial ).                ',
      4 '  message ''bla'' type ''E'' raising  not_Found.  ',
      5 'endif.                                            ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_25' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_25_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Is_Initial_Return.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( tadir_Entries[] is initial ).                ',
      4 '  return.                                         ',
      5 'endif.                                            ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_26' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_26_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Read_Subrc_If.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'read table tadir_Entries                          ',
      4 '  transporting no fields                          ',
      5 '  index 1.                                        ',
      6 'if ( sy-subrc is initial ).                       ',
      7 '  select * from tadir into table tadir_Entries    ',
      8 '    for all entries in tadir_Entries              ',
      9 '    where pgmid = tadir_Entries-pgmid.            ',
     10 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_27' ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_27_E' ).

      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Read_Subrc_Move_If.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: return_Code type sysubrc.                   ',
      4 'read table tadir_Entries                          ',
      5 '  transporting no fields                          ',
      6 '  index 1.                                        ',
      7 'move sy-subrc to return_Code.                     ',
      8 'if ( return_Code is initial ).                    ',
      9 '  select * from tadir into table tadir_Entries    ',
     10 '    for all entries in tadir_Entries              ',
     11 '    where pgmid = tadir_Entries-pgmid.            ',
     12 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_28' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_28_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Safe_Procedure.
    data:
      safe_Procedure_Names type string_Hashed_Table.

    "cl_Abap_Unit_Assert=>abort( ). " implementation is wrong, test not statisfory

    insert `IF_SAFE~METHOD` into table safe_Procedure_Names.

    mac_Append_To_Source_Lines:
      1 'method if_Other~method.                             ',
      2 'endmethod.                                          ',
      3 'method if_Safe~Method.                              ',
      4 '  data: tadir_Entries type standard table of tadir. ',
      5 '  select * from tadir into table tadir_Entries      ',
      6 '    for all entries in tadir_Entries                ',
      7 '    where pgmid = tadir_Entries-pgmid.              ',
      8 'endmethod.                                          '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries(
        source_Lines =  me->source_Lines
        safe_Procedure_Names = safe_Procedure_Names ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_X1'
                                           safe_Procedure_Names = safe_Procedure_Names ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_X1_E'
                                             safe_Procedure_Names = safe_Procedure_Names ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Size_Probe_Describe_If.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 'describe table tadir_Entries lines !lines.        ',
      5 'if ( !lines is not initial ).                     ',
      6 '  select * from tadir into table tadir_Entries    ',
      7 '    for all entries in tadir_Entries              ',
      8 '    where pgmid = tadir_Entries-pgmid.            ',
      9 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_29' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_29_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Size_Probe_Descr_Tfill.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 'describe table tadir_Entries.                     ',
      5 'if ( sy-tfill is not initial ).                   ',
      6 '  select * from tadir into table tadir_Entries    ',
      7 '    for all entries in tadir_Entries              ',
      8 '    where pgmid = tadir_Entries-pgmid.            ',
      9 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_30' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_30_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Size_Probe_Lines_Case.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 '!lines = lines( tadir_Entries[] ).                ',
      5 'case lines.                                       ',
      6 '  when 1.                                         ',
      7 '    select * from tadir into table tadir_Entries  ',
      8 '      for all entries in tadir_Entries            ',
      9 '      where pgmid = tadir_Entries-pgmid.          ',
     10 'endcase.                                          '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_31' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_31_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Size_Probe_Lines_Check.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 'describe table tadir_Entries lines !lines.        ',
      5 'check !lines is not initial.                      ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_32' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_32_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Size_Probe_Lines_If.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 '!lines = lines( tadir_Entries[] ).                ',
      5 'if ( !lines is not initial ).                     ',
      6 '  select * from tadir into table tadir_Entries    ',
      7 '    for all entries in tadir_Entries              ',
      8 '    where pgmid = tadir_Entries-pgmid.            ',
      9 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_33' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_33_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Move_Lines_Var_If.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: other type i.                               ',
      4 'data: tadir_Entries type standard table of tadir. ',
      5 '!lines = lines( tadir_Entries[] ).                ',
      6 'other = -2 + lines.                               ',
      7 'if ( 0 < other ).                                 ',
      8 '  select * from tadir into table tadir_Entries    ',
      9 '    for all entries in tadir_Entries              ',
     10 '    where pgmid = tadir_Entries-pgmid.            ',
     11 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_34' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_34_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.



endclass.


class tc_Safe_Queries_Complex definition
  for testing risk level harmless.

  private section.
    data:
      source_Lines     type string_Table,
      expected_Query   type th_Analyser=>ty_Unsafe_Query,
      expected_Queries type th_Analyser=>ty_Unsafe_Queries,
      actual_Queries   type th_Analyser=>ty_Unsafe_Queries.

    methods:
      pass_Assign_Probe              for testing,
      pass_Assign_Probe_Src_Before   for testing,
      pass_Assign_Probe_Target       for testing,
      pass_Clear_After_Reassign      for testing,
      pass_Clear_After_Unassign      for testing,
      pass_Chk_And_Insert_Is_Safe    for testing,
      pass_If_Elseif_Else            for testing,
      pass_Create_Loop_Assign        for testing,
      pass_Safe_Transfer             for testing,
      pass_Safe_Transfer_Move        for testing,
      pass_Syst_Fld_Indrct           for testing,
      pass_Syst_Fld_Indrct_Move      for testing,
      pass_Clear_Outside_Scope       for testing,
      pass_Fetch_Append_And_Subrc    for testing,
      pass_Fetch_Append_Prior_Cntnt  for testing.

endclass.

class tc_Safe_Queries_Complex implementation.

  method pass_Assign_Probe.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'field-symbols: <table>  type standard table.      ',
      4 'assign tadir_Entries to <entries>.                ',
      5 'if ( <entries> is not initial ).                  ',
      6 '  select * from tadir into table tadir_Entries    ',
      7 '    for all entries in tadir_Entries              ',
      8 '    where pgmid = tadir_Entries-pgmid.            ',
      9 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_35' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_35_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Assign_Probe_Src_Before.
    mac_Append_To_Source_Lines:
      1  'report bla.                                                        ',
      2  'form read_Untyped                                                  ',
      3  '   using i_Entries type data                                       ',
      4  '   changing e_Entries type data.                                   ',
      5  '  fields-symbols: <tadir_Entries> type standard table of tadir.    ',
      6  '  if ( i_Entries is initial ).                                     ',
      7  '    return.                                                        ',
      8  '  endif.                                                           ',
      9  '  assign i_Entries to <tadir_Entries>.                             ',
      10  '  select * from tadir                                             ',
      11 '    into corresponding fields of table <e_Entries>                 ',
      12 '    for all entries in <tadir_Entries>                             ',
      13 '    where pgmid = <tadir_Entries>-pgmid.                           ',
      14 'endform.                                                           '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.


  method pass_Assign_Probe_Target.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'field-symbols: <table>  type standard table.      ',
      4 'assign tadir_Entries to <entries>.                ',
      5 'if ( <entries> is not initial ).                  ',
      6 '  select * from tadir into table tadir_Entries    ',
      7 '    for all entries in <entries>                  ',
      8 '    where pgmid = tadir_Entries-pgmid.            ',
      9 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_36' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_36_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.

  method pass_Clear_After_Reassign.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'field-symbols: <entries>  type data.              ',
      4 'assign tadir_Entries to <entries>.                ',
      5 'insert initial line into table <entries>.         ',
      6 'assign sy-subrc to <entries>.                     ',
      7 'clear <entries>.                                  ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_37' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_37_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Syst_Fld_Indrct.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: cnt_Lines type sydbcnt.                     ',
      4 'select * from tadir into table tadir_Entries.     ',
      5 'cnt_Lines = sy-dbcnt.                             ',
      6 'if ( 0 ne cnt_Lines ).                            ',
      7 '  select * from tadir into table tadir_Entries    ',
      8 '    for all entries in tadir_Entries              ',
      9 '    where pgmid = tadir_Entries-pgmid.            ',
     10 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_39' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_39_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Syst_Fld_Indrct_Move.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: cnt_Lines type sydbcnt.                     ',
      4 'select * from tadir into table tadir_Entries.     ',
      5 'move sy-dbcnt to cnt_Lines.                       ',
      6 'if ( 0 ne cnt_Lines ).                            ',
      7 '  select * from tadir into table tadir_Entries    ',
      8 '    for all entries in tadir_Entries              ',
      9 '    where pgmid = tadir_Entries-pgmid.            ',
     10 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_40' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_40_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Clear_After_Unassign.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'field-symbols: <entries>  type standard table.    ',
      4 'assign tadir_Entries to <entries>.                ',
      5 'insert initial line into table <entries>.         ',
      6 'unassign <entries>.                               ',
      7 'clear <entries>.                                  ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_41' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_41_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Chk_And_Insert_Is_Safe.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( tadir_Entries is initial ).                  ',
      4 '  insert initital line into table tadir_Entries.  ',
      5 'endif.                                            ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_42' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_42_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_If_Elseif_Else.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( 1 = 2 ).                                     ',
      4 '  " nop                                           ',
      5 'elseif ( tadir_Entries is initial ).              ',
      6 '  message `BAD DATA` type `E`.                    ',
      7 'elseif ( 1 = 3 ).                                 ',
      8 '  " nop                                           ',
      9 'else.                                             ',
     10 '  select * from tadir into table tadir_Entries    ',
     11 '    for all entries in tadir_Entries              ',
     12 '    where pgmid = tadir_Entries-pgmid.            ',
     13 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_43' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_43_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Create_Loop_Assign.
    mac_Append_To_Source_Lines:
      1 'method loop_At_Input.                                   ',
      2 '  data: tadir_Entries  type standard table of tadir.    ',
      3 '  data: dref type ref to data.                          ',
      4 '  field-symbols: <tadir_Entry> type tadir.              ',
      5 '  field-symbols: <entries> type standard table of tadir.',
      6 '  if ( i_Entries is not initial ).                      ',
      7 '    create data dref like i_Entries.                    ',
      8 '    loop at i_Entries assigning <entry>.                ',
      9 '      assign dref->* to <entries>.                      ',
     10 '      insert <entry> into table <entries>.              ',
     11 '    endloop.                                            ',
     12 '    select * from tadir into table tadir_Entries        ',
     13 '      for all entries in <entries>[]                    ',
     14 '      where pgmid = <entries>-pgmid.                    ',
     15 'endmethod.                                              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_X2' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_X2_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Safe_Transfer.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: with_Entries   type standard table of tadir.',
      4 'insert initial line into table with_Entries.      ',
      5 'tadir_Entries = with_Entries[].                   ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_44' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_44_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Safe_Transfer_Move.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: with_Entries   type standard table of tadir.',
      4 'insert initial line into table with_Entries.      ',
      5 'move with_Entries[] to tadir_Entries.             ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_45' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_45_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Clear_Outside_Scope.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'insert initial line into table tadir_Entries[].   ',
      4 'if ( ''20110101'' > sy-datum ).                   ',
      5 '  clear tadir_Entries.                            ',
      6 'else.                                             ',
      7 '  select * from tadir into table tadir_Entries[]  ',
      8 '    for all entries in tadir_Entries              ',
      9 '    where pgmid = tadir_Entries-pgmid.            ',
     10 'endif.                                            '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_46' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_46_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Fetch_Append_And_Subrc.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: tadir_Cursor  type cursor.                  ',
      4 'open cursor with hold tadir_Cursor for            ',
      5 '  select * from tadir package size 10.            ',
      6 'fetch next cursor tadir_Cursor                    ',
      7 '  appending table tadir_Entries.                  ',
      8 'if ( 0 eq sy-subrc ).                             ',
      9 '  select * from tadir into table tadir_Entries    ',
     10 '    for all entries in tadir_Entries              ',
     11 '  where pgmid = tadir_Entries-pgmid.              ',
     12 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_47' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_47_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Fetch_Append_Prior_Cntnt.
    " internal table filled prio to call / no need to ensure
    " that append successful by sy-subrc
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: tadir_Cursor  type cursor.                  ',
      4 'append initial line into table tadir_Entries      ',
      5 'open cursor with hold tadir_Cursor for            ',
      6 '  select * from tadir package size 10.            ',
      7 'fetch next cursor tadir_Cursor                    ',
      8 '  appending table tadir_Entries.                  ',
      9 'select * from tadir into table tadir_Entries      ',
     10 '  for all entries in tadir_Entries                ',
     11 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_48' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_48_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


endclass.


class tc_Unsafe_Queries_Complex definition
  for testing risk level harmless.

  private section.
    data:
      source_Lines     type string_Table,
      expected_Query   type th_Analyser=>ty_Unsafe_Query,
      expected_Queries type th_Analyser=>ty_Unsafe_Queries,
      actual_Queries   type th_Analyser=>ty_Unsafe_Queries.

    methods:
      blame_Check_In_Loop          for testing,
      blame_Check_Other_And_Insert for testing,
      blame_Respecting_Module      for testing,
      blame_Respecting_Scope       for testing,
      blame_Syst_Fld_Indrct_Clear  for testing,
      blame_Unsafe_Transfer        for testing,
      blame_Unsafe_Transfer_Move   for testing.

    constants:
      c_External type abap_Bool value abap_True,
      c_Local    type abap_Bool value abap_False.

endclass.

class tc_Unsafe_Queries_Complex implementation.

  method blame_Check_In_Loop.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: tadir_Entry    type tadir.                  ',
      4 'select * from tadir into table tadir_Entries.     ',
      5 'loop at tadir_Entries into tadir_Entry.           ',
      6 '  if ( tadir_Entries is initial ).                ',
      7 '  endif.                                          ',
      8 'endloop.                                          ',
      9 'select * from tadir into table tadir_Entries      ',
     10 '  for all entries in tadir_Entries                ',
     11 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      9 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_49' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_49_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Check_Other_And_Insert.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( sy-uzeit ist initial ).                      ',
      4 '  if ( tadir_Entries is initial ).                ',
      5 '    insert initital line into table tadir_Entries.',
      6 '  endif.                                          ',
      7 'endif.                                            ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      8 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
  endmethod.


  method blame_Respecting_Module.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: lines type i.                               ',
      3 'data: tadir_Entries type standard table of tadir. ',
      4 'form get_Lines.                                   ',
      5 '  lines = lines( tadir_Entries[] ).               ',
      6 'endform.                                          ',
      7 'form query.                                       ',
      8 '  if ( !lines is not initial ).                   ',
      9 '    select * from tadir into table tadir_Entries  ',
     10 '      for all entries in tadir_Entries            ',
     11 '      where pgmid = tadir_Entries-pgmid.          ',
     12 '  endif.                                          ',
     13 'endform.                                          '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      9 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_50' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_50_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Respecting_Scope.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'if ( ''20110101'' > sy-datum ).                   ',
      4 '  insert initial line into table tadir_Entries[]. ',
      5 'elseif ( ''20110101'' < sy-datum ).               ',
      6 '  select * from tadir into table tadir_Entries[]  ',
      7 '    for all entries in tadir_Entries              ',
      8 '    where pgmid = tadir_Entries-pgmid.            ',
      9 'else.                                             ',
     10 '  select * from tadir into table tadir_Entries[]  ',
     11 '    for all entries in tadir_Entries              ',
     12 '    where pgmid = tadir_Entries-pgmid.            ',
     13 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:      6 'TADIR_ENTRIES' c_Local.
    mac_Append_Expectation:      8 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_51' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_51_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.

  method blame_Unsafe_Transfer.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: no_Entries     type standard table of tadir.',
      4 'insert initial line into table tadir_Entries.     ',
      5 'tadir_Entries = no_Entries[].                     ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:      6 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_52' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_52_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Unsafe_Transfer_Move.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'data: no_Entries     type standard table of tadir.',
      4 'insert initial line into table tadir_Entries.     ',
      5 'move no_Entries to tadir_Entries.                 ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      6 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_53' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_53_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Syst_Fld_Indrct_Clear.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'data: cnt_Lines type sydbcnt.                     ',
      4 'select * from tadir into table tadir_Entries.     ',
      5 'cnt_Lines = sy-dbcnt.                             ',
      6 'clear cnt_Lines.                                  ',
      7 'if ( 0 ne cnt_Lines ).                            ',
      8 '  select * from tadir into table tadir_Entries    ',
      9 '    for all entries in tadir_Entries              ',
     10 '    where pgmid = tadir_Entries-pgmid.            ',
     11 'endif.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:  8 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_54' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_54_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


endclass.

class tc_Unsafe_Queries_Scope definition
  for testing risk level harmless.

  private section.
    data:
      source_Lines     type string_Table,
      expected_Query   type th_Analyser=>ty_Unsafe_Query,
      expected_Queries type th_Analyser=>ty_Unsafe_Queries,
      actual_Queries   type th_Analyser=>ty_Unsafe_Queries.

    methods:
      blame_Scope_Local             for testing,
      blame_Scope_Signature         for testing,
      blame_Scope_Call_Mthd         for testing,
      blame_Scope_Call_Mthd_Fnct    for testing,
      blame_Scope_Call_Mthd_Stmnt   for testing,
      blame_Scope_Perform           for testing,
      blame_Scope_Assign_Argument   for testing,
      blame_Scope_Assign_Global     for testing.

    constants:
      c_External type abap_Bool value abap_True,
      c_Local    type abap_Bool value abap_False.

endclass.

class tc_Unsafe_Queries_Scope implementation.

  method blame_Scope_Local.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'select * from tadir into table tadir_Entries      ',
      4 '  for all entries in tadir_Entries                ',
      5 '  where pgmid = tadir_Entries-pgmid.              '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:      3 'TADIR_ENTRIES' c_Local.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    check sy-sysid eq c_Dev_System.
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_55' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_55_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Scope_Signature.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'form get using tadir_Entries type data.           ',
      3 '  select * from tadir into table tadir_Entries    ',
      4 '    for all entries in tadir_Entries              ',
      5 '    where pgmid = tadir_Entries-pgmid.            ',
      6 'endform.                                          '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:      3 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    check sy-sysid eq c_Dev_System.

    " rework test by reference
    clear me->expected_Queries.
    mac_Append_Expectation:
      4 'TADIR_ENTRIES' c_External.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_56' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_56_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Scope_Call_Mthd.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'insert initial line into table tadir_Entries.     ',
      4 'cl_Tralla=>read( importing x = tadir_Entries ).   ',
      5 'select * from tadir into table tadir_Entries      ',
      6 '  for all entries in tadir_Entries                ',
      7 '  where pgmid = tadir_Entries-pgmid.              '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation: 5 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    check sy-sysid eq c_Dev_System.
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_57' ).
    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_57_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Scope_Call_Mthd_Fnct.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'insert initial line into table tadir_Entries.     ',
      4 'tadir_Entries = cl_Tralla=>read( ).               ',
      5 'select * from tadir into table tadir_Entries      ',
      6 '  for all entries in tadir_Entries                ',
      7 '  where pgmid = tadir_Entries-pgmid.              '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      5 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    check sy-sysid eq c_Dev_System.
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_58' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_58_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Scope_Call_Mthd_Stmnt.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'insert initial line into table tadir_Entries.     ',
      4 'call method cl_Tralla=>read                       ',
      5 '  importing x = tadir_Entries .                   ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:     5 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    check sy-sysid eq c_Dev_System.
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_59' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_59_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Scope_Perform.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries type standard table of tadir. ',
      3 'insert initial line into table tadir_Entries.     ',
      4 'perform sub_Handle_Tralla                         ',
      5 '  using tadir_Entries.                            ',
      6 'select * from tadir into table tadir_Entries      ',
      7 '  for all entries in tadir_Entries                ',
      8 '  where pgmid = tadir_Entries-pgmid.              '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:      5 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).

    check sy-sysid eq c_Dev_System.
    " rework test by reference
    clear me->expected_Queries.
    mac_Append_Expectation:      8 'TADIR_ENTRIES' c_External.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_60' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_60_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Scope_Assign_Global.
    mac_Append_To_Source_Lines:
      1 'report bla.                                         ',
      2 '  data: global_Entries type standard table of tadir.',
      3 'form read_Tadir.                                    ',
      4 '  data: tadir_Entries type standard table of tadir. ',
      5 '  tadir_Entries = global_Entries.                   ',
      6 '  select * from tadir into table tadir_Entries      ',
      7 '    for all entries in tadir_Entries                ',
      8 '    where pgmid = tadir_Entries-pgmid.              ',
      9 'endform.                                            '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:      6 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_61' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_61_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


  method blame_Scope_Assign_Argument.
    mac_Append_To_Source_Lines:
      1 'report bla.                                           ',
      2 'class lcl_Repository definition.                      ',
      3 '  public section.                                     ',
      4 '   methods read_By_Entries                            ',
      5 '     importing i_Entries type standard table.         ',
      6 'endclass.                                             ',
      7 '                                                      ',
      8 'class lcl_Repository implementation.                  ',
      9 '  method read_By_Entries.                             ',
     10 '    data: tadir_Entries type standard table of tadir. ',
     11 '    tadir_Entries = i_Entries.                        ',
     12 '  select * from tadir into table tadir_Entries        ',
     13 '    for all entries in tadir_Entries                  ',
     14 '    where pgmid = tadir_Entries-pgmid.                ',
     15 '  endmethod.                                          ',
     16 'endclass.                                             '.
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:   10 'TADIR_ENTRIES' c_External.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_62' ).
    cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_62_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
    act = me->actual_Queries
    exp = me->expected_Queries ).
    endif.
  endmethod.


endclass.

class th_Block_Is_Identical definition for testing.

  public section.
    methods constructor importing block type ref to block.
    interfaces if_Constraint.

  private section.
    data:
      block  type ref to block,
      reason type string.

endclass.


class th_Block_Is_Identical implementation.

  method constructor.
    me->block = block.
  endmethod.

  method if_Constraint~is_Valid.
    data:
      act_Block              type ref to block,
      type_Descriptor        type ref to cl_Abap_Classdescr,
      name_Of_Expected_Value type string,
      name_Of_Actual_Value   type string.
    field-symbols:
      <attribute>      type abap_Attrdescr,
      <actual_Value>   type data,
      <expected_Value> type data.


    if ( data_Object is not bound ).
      result = abap_False.
      me->reason = 'No valid reference' ##no_Text.
      return.
    endif.
    try.
        act_Block ?= data_Object.
      catch cx_Sy_Move_Cast_Error.
        result = abap_False.
        me->reason = 'Type mismatch' ##no_Text.
        return.
    endtry.
    type_Descriptor ?= cl_Abap_Typedescr=>describe_By_Object_Ref( me->block ).

    loop at type_Descriptor->attributes assigning <attribute>
      where visibility = cl_Abap_Objectdescr=>public.
      name_Of_Expected_Value  = |ME->BLOCK->{ <attribute>-name }|.
      name_Of_Actual_Value =    |ACT_BLOCK->{ <attribute>-name }|.
      assign (name_Of_Expected_Value) to <expected_Value>.
      assign (name_Of_Actual_Value)   to <actual_Value>.
      if ( <expected_Value> ne <actual_Value> ).
        me->reason = |Attribute { <attribute>-name } differs| ##no_Text.
        result = abap_False.
        return.
      endif.
    endloop.

    clear me->reason.
    result = abap_True.

  endmethod.

  method if_Constraint~get_Description.
    if ( me->reason is not initial ).
      insert me->reason into table result[].
    else.
      insert `Block is identical` into table result[] ##no_Text.
    endif.
  endmethod.

endclass.



class tc_Block definition for testing risk level harmless.

  private section.
    methods setup.
    methods is_Clone_Identical for testing.

    data:
      block type ref to block.

endclass.



class tc_Block implementation.

  method setup.
    data: table_Var_Names type string_Hashed_Table.

    insert `RECORDS` into table table_Var_Names.

    create object me->block.
    me->block->set_Table_Variable_Names( table_Var_Names ).
    me->block->add_Variable_Name( `IS_INITAL` ).
    me->block->set_Has_Been_Checked( abap_True ).
    me->block->set_Is_Ensured_By_Exit( abap_True ).
    me->block->set_Do_Check_Sys_Field_Next( ).

  endmethod.

  method is_Clone_Identical.
    data:
      cloned_Block type ref to block,
      is_Identical type ref to if_Constraint.

    create object is_Identical type th_Block_Is_Identical
      exporting
        block = me->block.
    cloned_Block = me->block->clone( ).

    cl_Abap_Unit_Assert=>assert_That( act = cloned_Block exp = is_Identical ).

  endmethod.

endclass.


class tc_Exit_Queries_Complex definition
  for testing risk level harmless.

  private section.
    data:
      source_Lines     type string_Table,
      expected_Query   type th_Analyser=>ty_Unsafe_Query,
      expected_Queries type th_Analyser=>ty_Unsafe_Queries,
      actual_Queries   type th_Analyser=>ty_Unsafe_Queries.

    methods:
      pass_Exit                      for testing,
      pass_Exit_Later_Reset          for testing,
      pass_Exit_Sgntr_If             for testing,
      pass_Exit_Sgntr_If_Thn         for testing,
      pass_Full_Exit                 for testing,
      pass_Full_Exit_Later_Reset     for testing,
      pass_Full_Exit_Loop            for testing,
      blame_Exit_Loop                for testing.


    constants:
      c_External type abap_Bool value abap_True,
      c_Local    type abap_Bool value abap_False.

endclass.


class tc_Exit_Queries_Complex implementation.

  method pass_Exit.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( 1 eq 1 ).                                    ',
      4 '  if ( tadir_Entries is initial ).                ',
      5 '    exit.                                         ',
      6 '  endif.                                          ',
      7 'endif.                                            ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_65' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_65_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Exit_Later_Reset.
    " exit is carried over up to the level where the
    " variable has been queried even if its fully
    " safe
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( tadir_Entries is initial ).                  ',
      4 '  if ( 1 eq 2 ).                                  ',
      5 '    exit.                                         ',
      6 '  endif.                                          ',
      7 'endif.                                            ',
      8 'clear tadir_Entries.                              ',
      9 'endif.                                            ',
     10 'select * from tadir into table tadir_Entries      ',
     11 '  for all entries in tadir_Entries                ',
     12 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_66' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_66_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Exit_Sgntr_If.
    mac_Append_To_Source_Lines:
      1 'method read_By_I_Tadir_Keys.                        ',
      2 '  data: tadir_Entries  type standard table of tadir.',
      3 '  if (  i_Tadir_Keys is initial ).                  ',
      4 '    return.                                         ',
      5 '  endif.                                            ',
      6 '  tadir_Entries = i_Tadir_Keys.                     ',
      7 '  select * from tadir into table tadir_Entries      ',
      8 '    for all entries in tadir_Entries                ',
      9 '    where pgmid = tadir_Entries-pgmid.              ',
     10 'endmethod.                                          '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_X3' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_X3_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Exit_Sgntr_If_Thn.
    mac_Append_To_Source_Lines:
      1 'method read_By_I_Tadir_Keys.                        ',
      2 '  data: tadir_Entries  type standard table of tadir.',
      3 '  if (  i_Tadir_Keys is initial ).                  ',
      4 '    return.                                         ',
      5 '  else.                                             ',
      6 '    tadir_Entries = i_Tadir_Keys.                   ',
      7 '  endif.                                            ',
      8 '  select * from tadir into table tadir_Entries      ',
      9 '    for all entries in tadir_Entries                ',
     10 '    where pgmid = tadir_Entries-pgmid.              ',
     11 'endmethod.                                          '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_X4' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_X4_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Full_Exit.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( 1 eq 1 ).                                    ',
      4 '  if ( tadir_Entries is initial ).                ',
      5 '    exit.                                         ',
      6 '  endif.                                          ',
      7 'endif.                                            ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_69' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_69_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Full_Exit_Later_Reset.
    " retrun by raising is carried over up to the level where the
    " variable has been queried even if its fully safe
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( tadir_Entries is initial ).                  ',
      4 '  if ( 1 eq 2 ).                                  ',
      5 '    raise xpt_Wrong.                              ',
      6 '  endif.                                          ',
      7 'endif.                                            ',
      8 '  clear tadir_Entries.                            ',
      9 'endif.                                            ',
     10 'select * from tadir into table tadir_Entries      ',
     11 '  for all entries in tadir_Entries                ',
     12 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_70' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_70_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method pass_Full_Exit_Loop.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( tadir_Entries is initial ).                  ',
      4 '  do 5 times.                                     ',
      5 '    return.                                       ',
      6 '  enddo.                                          ',
      7 'endif.                                            ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_71' ).
    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).

    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_71_E' ).
      cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
    endif.
  endmethod.


  method blame_Exit_Loop.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir.',
      3 'if ( tadir_Entries is initial ).                  ',
      4 '  do 5 times.                                     ',
      5 '    exit.                                         ',
      6 '  enddo.                                          ',
      7 'endif.                                            ',
      8 'select * from tadir into table tadir_Entries      ',
      9 '  for all entries in tadir_Entries                ',
     10 '  where pgmid = tadir_Entries-pgmid.              '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    mac_Append_Expectation:
      8 'TADIR_ENTRIES' abap_False.

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
    " rework test by reference
    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries1( program = 'RS_CI_TEST_FAE_72' ).
    cl_Abap_Unit_Assert=>assert_Equals(
   act = me->actual_Queries
   exp = me->expected_Queries ).
    " remote test (UIA=>Y7A)
    if sy-sysid = c_Dev_System .
      me->actual_Queries =
        th_Analyser=>locate_Unsafe_Queries1( p_Sysid = c_Ref_System program = 'RS_CI_TEST_FAE_72_E' ).
      cl_Abap_Unit_Assert=>assert_Equals(
        act = me->actual_Queries
        exp = me->expected_Queries ).
    endif.
  endmethod.

endclass.


class tc_Header_Line definition
  for testing risk level harmless.

  private section.
    data:
      source_Lines     type string_Table,
      expected_Query   type th_Analyser=>ty_Unsafe_Query,
      expected_Queries type th_Analyser=>ty_Unsafe_Queries,
      actual_Queries   type th_Analyser=>ty_Unsafe_Queries.

    methods:
      blame_Check_Header            for testing,
      blame_Check_Header_Select_Opt for testing,
      pass_Check_Itab               for testing,
      pass_Check_Lines              for testing.

endclass.


class tc_Header_Line implementation.


  method blame_Check_Header.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir with header line.',
      3 'if ( tadir_Entries is not initial ).            ',
      4 '  select * from tadir into table tadir_Entries  ',
      5 '    for all entries in tadir_Entries            ',
      6 '    where pgmid = tadir_Entries-pgmid.          ',
      7 'endif.                                          '.

    mac_Append_Expectation:
      4 'TADIR_ENTRIES' abap_False.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
  endmethod.


  method blame_Check_Header_Select_Opt.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'tables: tadir.                                    ',
      3 'select-options: s_Names  for tadir-obj_Name.      ',
      4 'if ( s_Names is not initial ).                    ',
      5 '  select obj_Name as low from tadir               ',
      6 '    into corresponding fields of table s_Entries  ',
      7 '    for all entries in s_Names                    ',
      8 '    where obj_Name = s_Names-low.                 ',
      9 'endif.                                            '.

    mac_Append_Expectation:
      5 'S_NAMES' abap_False.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act = me->actual_Queries
      exp = me->expected_Queries ).
  endmethod.


  method pass_Check_Itab.
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir with header line.',
      3 'if ( tadir_Entries[] is not initial ).            ',
      4 '  select * from tadir into table tadir_Entries  ',
      5 '    for all entries in tadir_Entries            ',
      6 '    where pgmid = tadir_Entries-pgmid.          ',
      7 'endif.                                          '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.


  method pass_Check_Lines.
    " lines is smart enough to use itab and not header line
    mac_Append_To_Source_Lines:
      1 'report bla.                                       ',
      2 'data: tadir_Entries  type standard table of tadir with header line.',
      3 'if ( lines ( tadir_Entries ) is not initial ).  ',
      4 '  select * from tadir into table tadir_Entries  ',
      5 '    for all entries in tadir_Entries            ',
      6 '    where pgmid = tadir_Entries-pgmid.          ',
      7 'endif.                                          '.

    me->actual_Queries =
      th_Analyser=>locate_Unsafe_Queries( me->source_Lines ).

    cl_Abap_Unit_Assert=>assert_Initial( me->actual_Queries ).
  endmethod.

endclass.
