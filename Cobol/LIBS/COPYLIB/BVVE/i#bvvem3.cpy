      *================================================================*
      *  I#BVVEM3 - SOLICITACOES DE ALTERACAO/INCLUSAO DE TRAVA DE     *
      *             DOMICILIO BANCARIO PARA ENVIO AO MASSIVO BVVE      *
      *----------------------------------------------------------------*
      *  LRECL: 080                                                    *
      *================================================================*
       01  BVVEM3-ENVIO-ALTERACAO.
           03 BVVEM3-TP-REG            PIC  X(001)         VALUE SPACES.
      *       1 = HEADER / 2 = DETALHE / 3 = TRAILLER
           03 BVVEM3-HEADER.
              05 BVVEM3-HD-DATA        PIC  X(010)         VALUE SPACES.
      *          FORMATO: DD.MM.AAAA
              05 BVVEM3-HD-HORA        PIC  X(008)         VALUE SPACES.
              05 BVVEM3-HD-ROTINA      PIC  X(004)         VALUE SPACES.
              05 BVVEM3-HD-ORIGEM      PIC  X(001)         VALUE SPACES.
      *          V = VAREJO / G = GRAN
              05 FILLER                PIC  X(056)         VALUE SPACES.
           03 BVVEM3-DETALHE       REDEFINES  BVVEM3-HEADER.
              05 BVVEM3-DET-CNPJ.
                 07 BVVEM3-DET-CNPJN   PIC  9(015).
              05 FILLER            REDEFINES  BVVEM3-DET-CNPJ.
                 07 BVVEM3-DET-CPF.
                    09 BVVEM3-DET-CPFN PIC  9(011).
                 07 FILLER             PIC  X(004).
              05 BVVEM3-DET-TP-PSSOA   PIC  X(001).
      *          F = FISICA / J = JURIDICA
              05 BVVEM3-DET-AGENCIA.
                 07 BVVEM3-DET-AGEN    PIC  9(005).
              05 BVVEM3-DET-CONTA.
                 07 BVVEM3-DET-CONTAN  PIC  9(007).
              05 BVVEM3-DET-CPROP-ESQM PIC  X(003).
      *          003 = MASTER / 004 = VISA
              05 BVVEM3-DET-PRAZO.
                 07 BVVEM3-DET-PRAZON  PIC  9(004).
              05 FILLER                PIC  X(044).
           03 BVVEM3-TRAILLER      REDEFINES  BVVEM3-HEADER.
              05 BVVEM3-TR-QTD-REG     PIC  9(009).
              05 FILLER                PIC  X(070).
