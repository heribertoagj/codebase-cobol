      /--------------------------- I#BVVE01 ---------------------------*
      *                       TABELA DE MESES                          *
      *----------------------------------------------------------------*
      *   - TABELA DE MESES COM OS 6 ULTIMOS MESES POR EXTENSO         *
      *----------------------------------------------------------------*

       01  TAB-MESES.
           05  TAB-MES.
               10  FILLER                   PIC  X(026)  VALUE   '01DEZ
      -            'NOV OUT SET AGO JUL'.
               10  FILLER                   PIC  X(026)  VALUE   '02JAN
      -            'DEZ NOV OUT SET AGO'.
               10  FILLER                   PIC  X(026)  VALUE   '03FEV
      -            'JAN DEZ NOV OUT SET'.
               10  FILLER                   PIC  X(026)  VALUE   '04MAR
      -            'FEV JAN DEZ NOV OUT'.
               10  FILLER                   PIC  X(026)  VALUE   '05ABR
      -            'MAR FEV JAN DEZ NOV'.
               10  FILLER                   PIC  X(026)  VALUE   '06MAI
      -            'ABR MAR FEV JAN DEZ'.
               10  FILLER                   PIC  X(026)  VALUE   '07JUN
      -            'MAI ABR MAR FEV JAN'.
               10  FILLER                   PIC  X(026)  VALUE   '08JUL
      -            'JUN MAI ABR MAR FEV'.
               10  FILLER                   PIC  X(026)  VALUE   '09AGO
      -            'JUL JUN MAI ABR MAR'.
               10  FILLER                   PIC  X(026)  VALUE   '10SET
      -            'AGO JUL JUN MAI ABR'.
               10  FILLER                   PIC  X(026)  VALUE   '11OUT
      -            'SET AGO JUL JUN MAI'.
               10  FILLER                   PIC  X(026)  VALUE   '12NOV
      -            'OUT SET AGO JUL JUN'.
           05  TAB-MES-R      REDEFINES     TAB-MES.
               07  FILLER                   OCCURS  12   TIMES.
                   10  TAB-MES-ATUAL        PIC  9(02).
                   10  TAB-MES1-EXTENSO     PIC  X(03).
                   10  FILLER               PIC  X(01).
                   10  TAB-MES2-EXTENSO     PIC  X(03).
                   10  FILLER               PIC  X(01).
                   10  TAB-MES3-EXTENSO     PIC  X(03).
                   10  FILLER               PIC  X(01).
                   10  TAB-MES4-EXTENSO     PIC  X(03).
                   10  FILLER               PIC  X(01).
                   10  TAB-MES5-EXTENSO     PIC  X(03).
                   10  FILLER               PIC  X(01).
                   10  TAB-MES6-EXTENSO     PIC  X(03).
                   10  FILLER               PIC  X(01).

