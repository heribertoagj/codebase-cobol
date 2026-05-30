      *================================================================*
      * I#BVVEAQ - AREA DE COMUNICACAO COM MODULO BVVE2451             *
      *            INTERFACE COM O GRAN PARA SOLICITACOES DE TRAVAS E  *
      *            DESTRAVAS DE DOMICILIO BANCARIO ONLINE.             *
      *----------------------------------------------------------------*
      * LRECL: 606                                                     *
      *================================================================*
       01  BVVEAQ-COMMAREA-2451.
           03 BVVEAQ-E-ENTRADA.
              05 BVVEAQ-E-TP-MANUT         PIC  X(001)     VALUE SPACES.
      *          T = TRAVA / D = DESTRAVA
              05 BVVEAQ-E-INCLUSAO.
                 07 BVVEAQ-E-I-CCUSTO      PIC  X(004)     VALUE SPACES.
                 07 BVVEAQ-E-I-CNPJ-CPF    PIC  9(009)     VALUE ZEROS.
                 07 BVVEAQ-E-I-FILIAL      PIC  9(004)     VALUE ZEROS.
                 07 BVVEAQ-E-I-CTRL        PIC  9(002)     VALUE ZEROS.
                 07 BVVEAQ-E-I-TP-PSSOA    PIC  X(001)     VALUE SPACES.
      *             J = JURIDICA / F = FISICA
                 07 BVVEAQ-E-I-PRODUTO     PIC  9(005)     VALUE ZEROS.
                 07 BVVEAQ-E-I-BAND        PIC  X(003)     VALUE SPACES.
      *             003 = MASTER / 004 = VISA
                 07 BVVEAQ-E-I-RAIZ        PIC  X(001)     VALUE SPACES.
      *             S = SIM / N = NAO
                 07 BVVEAQ-E-I-AG-CLI      PIC  9(004)     VALUE ZEROS.
                 07 BVVEAQ-E-I-CTA-CLI     PIC  9(013)     VALUE ZEROS.
                 07 BVVEAQ-E-I-DTFIM-VALID PIC  9(008)     VALUE ZEROS.
                 07 BVVEAQ-E-I-USUARIO     PIC  X(009)     VALUE SPACES.
                 07 FILLER                 PIC  X(017)     VALUE SPACES.
              05 BVVEAQ-E-BAIXA        REDEFINES BVVEAQ-E-INCLUSAO.
                 07 BVVEAQ-E-B-CCUSTO      PIC  X(004).
                 07 BVVEAQ-E-B-CNPJ-CPF    PIC  9(009).
                 07 BVVEAQ-E-B-FILIAL      PIC  9(004).
                 07 BVVEAQ-E-B-CTRL        PIC  9(002).
                 07 BVVEAQ-E-B-TP-PSSOA    PIC  X(001).
      *             J = JURIDICA / F = FISICA
                 07 BVVEAQ-E-B-PRODUTO     PIC  9(005).
                 07 BVVEAQ-E-B-BAND        PIC  X(003).
      *             003 = MASTER / 004 = VISA
                 07 BVVEAQ-E-B-USUARIO     PIC  X(009).
                 07 FILLER                 PIC  X(043).
           03 BVVEAQ-S-SAIDA.
              05 BVVEAQ-S-COD-RETORNO      PIC  9(002)     VALUE ZEROS.
      *          00 = RETORNO OK
      *          99 = RETORNO COM ERRO
              05 BVVEAQ-S-NRO-CTRL-IF      PIC  X(023)     VALUE SPACES.
              05 BVVEAQ-S-ERRO.
                 07 BVVEAQ-S-ERRO-TEXTO    PIC  X(100) OCCURS 5
                                                           VALUE SPACES.
