      *----------------------------------------------------------------*
      * I#BVVEAT - AREA DE COMUNICACAO COM MODULO BVVE2452             *
      *            CONSULTA SALDO DA AGENDA FINANCEIRA DO CLIENTE      *
      * LRECL: 200                                                     *
      *----------------------------------------------------------------*
       01  BVVEAT-COMMAREA-2452.
           03 BVVEAT-E-ENTRADA.
              05 BVVEAT-E-AGENCIA          PIC  9(004)     VALUE ZEROS.
              05 BVVEAT-E-CONTA            PIC  9(007)     VALUE ZEROS.
              05 BVVEAT-E-PRODUTO          PIC  9(005)     VALUE ZEROS.
              05 FILLER                    PIC  X(033)     VALUE SPACES.
           03 BVVEAT-S-SAIDA.
              05 BVVEAT-S-SALDO            PIC S9(015)V99  VALUE ZEROS.
              05 BVVEAT-S-COD-RETORNO      PIC  9(002)     VALUE ZEROS.
      *          00 = RETORNO OK
      *          99 = RETORNO COM ERRO
              05 BVVEAT-S-ERRO-TEXTO       PIC  X(100)     VALUE SPACES.
              05 FILLER                    PIC  X(032)     VALUE SPACES.
