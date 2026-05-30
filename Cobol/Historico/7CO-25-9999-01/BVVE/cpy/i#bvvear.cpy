      *================================================================*
      * I#BVVEAR - AREA DE COMUNICACAO COM MODULO BVVE7060             *
      *            LISTAR OU CONSULTAR BANDEIRAS BVVE X CIP            *
      *----------------------------------------------------------------*
      * LRECL: 690                                                     *
      *================================================================*
       01  BVVEAR-COMMAREA-7060.
           03 BVVEAR-E-ENTRADA.
              05 BVVEAR-E-TP-PESQ          PIC  X(001)     VALUE SPACES.
      *          C = CONSULTAR / L = LISTAR
              05 BVVEAR-E-ORIG-BAND        PIC  X(001)     VALUE ZEROS.
      *          B = BVVE / C = CIP
              05 BVVEAR-E-COD-BAND         PIC  9(003)     VALUE ZEROS.
              05 BVVEAR-E-COD-BAND-X   REDEFINES BVVEAR-E-COD-BAND
                                           PIC  X(003).
              05 FILLER                    PIC  X(025)     VALUE SPACES.
           03 BVVEAR-S-SAIDA.
              05 BVVEAR-S-COD-RETORNO      PIC  9(002)     VALUE ZEROS.
      *          00 = RETORNO OK
      *          95 = ERRO GENERICO
              05 BVVEAR-S-LISTA-BAND.
                 07 BVVEAR-S-LISTA     OCCURS 12.
                    10 BVVEAR-S-BAND-BVVE  PIC  9(003)     VALUE ZEROS.
                    10 BVVEAR-S-BAND-CIP   PIC  X(003)     VALUE SPACES.
                    10 BVVEAR-S-BAND-DESC  PIC  X(040)     VALUE SPACES.
                    10 BVVEAR-S-BAND-REDUZ REDEFINES BVVEAR-S-BAND-DESC
                                           PIC  X(004).
              05 BVVEAR-S-ERRO-LOCAL       PIC  9(002)     VALUE ZEROS.
              05 BVVEAR-S-ERRO-EIBRESP     PIC  9(004)     VALUE ZEROS.
              05 BVVEAR-S-ERRO-TEXTO       PIC  X(079)     VALUE SPACES.
              05 FILLER                    PIC  X(021)     VALUE SPACES.
