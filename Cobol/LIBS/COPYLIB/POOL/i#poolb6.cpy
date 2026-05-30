      *---------------------------------------------------------------*
      *     -INC I#POOLB6  -  AREA DE COMUNICACAO COM A POOL7160      *
      *     (MODULO PARA TRATAMENTO DE ERROS EM APLICACOES CICS)      *
      *---------------------------------------------------------------*
       01 ERRO-AREA.
          05 ERR-VERSAO          PIC X(06).
          05 ERR-MENSAGEM.
             07 ERR-COD-MSG      PIC 9(03).
             07 FILLER           PIC X(01).
             07 ERR-TEXTO-MSG    PIC X(79).
          05 ERR-RETURN-CODE     PIC 9(02).
          05 ERR-TIPO-ACESSO     PIC X(03).
          05 ERR-PGM             PIC X(08).
          05 ERR-DADOS-DB2.
             07 ERR-DBD-TAB      PIC X(18).
             07 ERR-FUN-COMANDO  PIC X(10).
             07 ERR-STA-CODE     PIC X(04).
             07 ERR-SQL-CODE REDEFINES
                   ERR-STA-CODE  PIC S9(09) COMP-4.
             07 ERR-LOCAL        PIC X(04).
             07 ERR-SEGM         PIC X(08).
             07 FILLER           PIC X(31).

          05 ERR-TEXTO REDEFINES ERR-DADOS-DB2
                                 PIC X(75).
          05 ERR-COD-USER        PIC X(07).
          05 ERR-COD-DEPTO       PIC X(06).
          05 ERR-MODULO          PIC X(08)  VALUE SPACES.
          05 FILLER              PIC X(24).
          05 ERR-SQLCA           PIC X(136).
          05 ERR-DFHEIBLK        PIC X(85).
          05 ERR-TERMINAL        PIC X(08)  VALUE SPACES.
          05 ERR-CICS            PIC X(08)  VALUE SPACES.
          05 FILLER              PIC X(61).