      *================================================================*
      *  I#BVVEM2 - SOLICITACOES DE DESTRAVA/EXCLUSAO DE DOMICILIO     *
      *             BANCARIO PARA ENVIO AO MASSIVO BVVE                *
      *----------------------------------------------------------------*
      *  LRECL: 080                                                    *
      *================================================================*
       01  BVVEM2-ENVIO-DESTRAVA.
           05 BVVEM2-TP-REG            PIC  X(001)         VALUE SPACES.
      *       1 = HEADER / 2 = DETALHE / 3 = TRAILLER
           05 BVVEM2-HEADER.
              10 BVVEM2-HD-DATA        PIC  X(010)         VALUE SPACES.
      *          FORMATO: DD.MM.AAAA
              10 BVVEM2-HD-HORA        PIC  X(008)         VALUE SPACES.
              10 BVVEM2-HD-ROTINA      PIC  X(004)         VALUE SPACES.
              10 BVVEM2-HD-ORIGEM      PIC  X(001)         VALUE SPACES.
      *          V = VAREJO / G = GRAN
              10 FILLER                PIC  X(056)         VALUE SPACES.
           05 BVVEM2-DETALHE       REDEFINES BVVEM2-HEADER.
              10 BVVEM2-DET-CNPJ.
                 15 BVVEM2-DET-CNPJN   PIC  9(015).
              10 FILLER            REDEFINES BVVEM2-DET-CNPJ.
                 15 BVVEM2-DET-CPF.
                    20 BVVEM2-DET-CPFN PIC  9(011).
                 15 FILLER             PIC  X(004).
              10 BVVEM2-DET-TP-PSSOA   PIC  X(001).
      *          F = FISICA / J = JURIDICA
              10 BVVEM2-DET-CPROP-ESQM PIC  X(003).
      *          003 = MASTER / 004 = VISA
              10 FILLER                PIC  X(060).
           05 BVVEM2-TRAILLER      REDEFINES BVVEM2-HEADER.
              10  BVVEM2-TR-QTD-REG    PIC  9(009).
              10  FILLER               PIC  X(070).
