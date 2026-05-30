      *================================================================*
      * I#BVVEAS - AREA DE COMUNICACAO COM MODULO BVVE7061             *
      *            CONSULTA SALDO DE CREDITO E DEBITO NA BASE BACCV041 *
      *----------------------------------------------------------------*
      * LRECL: 220                                                     *
      *================================================================*
       01  BVVEAS-COMMAREA-7061.
           03 BVVEAS-E-ENTRADA.
              05 BVVEAS-E-AGENCIA          PIC  9(005)     VALUE ZEROS.
              05 BVVEAS-E-CONTA            PIC  9(007)     VALUE ZEROS.
              05 BVVEAS-E-DESC-BAND        PIC  X(004)     VALUE SPACES.
      *          "VISA" / "MAST" / "ELO " / "DINE"
              05 FILLER                    PIC  X(029)     VALUE SPACES.
           03 BVVEAS-S-SAIDA.
              05 BVVEAS-S-COD-RETORNO      PIC  9(002)     VALUE ZEROS.
      *          00 = RETORNO OK
      *          04 = ERRO DE CONSISTENCIA DE ENTRADA
      *          90 = ERRO GENERICO
      *          99 = ERRO DB2
              05 BVVEAS-S-TOT-CREDITO      PIC S9(013)V99  VALUE ZEROS.
              05 BVVEAS-S-TOT-DEBITO       PIC S9(013)V99  VALUE ZEROS.
              05 BVVEAS-S-ERRO-LOCAL       PIC  9(002)     VALUE ZEROS.
              05 BVVEAS-S-ERRO-EIBRESP     PIC  9(004)     VALUE ZEROS.
              05 BVVEAS-S-ERRO-TEXTO       PIC  X(079)     VALUE SPACES.
              05 BVVEAS-S-ERRO-DB2.
                 07 BVVEAS-S-EDB2-TABELA   PIC  X(018)     VALUE SPACES.
                 07 BVVEAS-S-EDB2-COMANDO  PIC  X(010)     VALUE SPACES.
                 07 BVVEAS-S-EDB2-SQLCODE  PIC S9(009)     VALUE 0.
              05 FILLER                    PIC  X(021)     VALUE SPACES.
