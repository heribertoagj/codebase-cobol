      *---------------------------------------------------------------*
      *         BVVEW415                                              *
      *         BVVE - SLC/SGC - CRIPTOGRAFIA                         *
      *         COPY BOOK PARA MANIPULAR DATA                         *
      *         TABELA DE MESES (P/ MONTAR HEADER)                    *
      *---------------------------------------------------------------*

       01  WRK-DT-EDIT.

           02 WRK-DIA-N                PIC 9(02)      VALUE ZEROS.
           02 FILLER                   PIC X(01)      VALUE '/'.
           02 WRK-MES-N                PIC 9(02)      VALUE ZEROS.
           02 FILLER                   PIC X(01)      VALUE '/'.
           02 WRK-ANO-N                PIC 9(04)      VALUE ZEROS.

       01  WRK-DT-EDIT-STRING.

           02 WRK-DIA-S                PIC 9(02)       VALUE ZEROS.
           02 FILLER                   PIC X(01)       VALUE '/'.
           02 WRK-MES-S                PIC X(03)       VALUE SPACES.
           02 FILLER                   PIC X(01)       VALUE '/'.
           02 WRK-ANO-S                PIC 9(04)       VALUE ZEROS.

       01  TABELA-DEF-MESES.
           02 FILLER                   PIC X(03) VALUE 'JAN'.
           02 FILLER                   PIC X(03) VALUE 'FEV'.
           02 FILLER                   PIC X(03) VALUE 'MAR'.
           02 FILLER                   PIC X(03) VALUE 'ABR'.
           02 FILLER                   PIC X(03) VALUE 'MAI'.
           02 FILLER                   PIC X(03) VALUE 'JUN'.
           02 FILLER                   PIC X(03) VALUE 'JUL'.
           02 FILLER                   PIC X(03) VALUE 'AGO'.
           02 FILLER                   PIC X(03) VALUE 'SET'.
           02 FILLER                   PIC X(03) VALUE 'OUT'.
           02 FILLER                   PIC X(03) VALUE 'NOV'.
           02 FILLER                   PIC X(03) VALUE 'DEZ'.

       01  TAB-X REDEFINES TABELA-DEF-MESES.
           02 TAB-MESES                PIC X(03) OCCURS 12 TIMES.
