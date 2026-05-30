      *================================================================*
      *  I#BVVEAU - ENVIO E RECEPCAO DE SOLICITACOES DE ALTERACAO DE   *
      *             TRAVA ( GRAN X BVVE MASSIVO )                      *
      *----------------------------------------------------------------*
      *  LRECL: 600                                                    *
      *================================================================*
       01  BVVEAU-ALTERACAO-TRV.
           03 BVVEAU-TP-REG            PIC  X(001)         VALUE SPACES.
           03 BVVEAU-HEADER.
              05 BVVEAU-HD-DATA        PIC  X(010)         VALUE SPACES.
              05 BVVEAU-HD-HORA        PIC  X(008)         VALUE SPACES.
              05 BVVEAU-HD-ROTINA      PIC  X(004)         VALUE SPACES.
              05 BVVEAU-HD-ORIGEM      PIC  X(001)         VALUE SPACES.
              05 BVVEAU-HD-FILLER      PIC  X(576)         VALUE SPACES.
           03 BVVEAU-DETALHE       REDEFINES   BVVEAU-HEADER.
              05 BVVEAU-DET-CNPJ.
                 07 BVVEAU-DET-CNPJN   PIC  9(015).
              05 FILLER    REDEFINES  BVVEAU-DET-CNPJ.
                 07 BVVEAU-DET-CPF.
                    09 BVVEAU-DET-CPFN PIC  9(011).
                 07 BVVEAU-DET-ZEROS   PIC  9(004).
              05 BVVEAU-DET-TP-PSSOA   PIC  X(001).
              05 BVVEAU-DET-AGENCIA.
                 07 BVVEAU-DET-AGE-N   PIC  9(005).
              05 BVVEAU-DET-CONTA.
                 07 BVVEAU-DET-CTA-N   PIC  9(007).
              05 BVVEAU-DET-PRODUTO.
                 07 BVVEAU-DET-PRODN   PIC  9(005).
              05 BVVEAU-DET-CPROP-ESQM PIC  X(003).
      *          003 = MASTER / 004 = VISA                             *
              05 BVVEAU-DET-PRAZO.
                 07 BVVEAU-DET-PRAZON  PIC  9(004).
              05 BVVEAU-DET-COD-RET    PIC  9(002).
              05 BVVEAU-DET-ERRO.
                 07 BVVEAU-DET-ERROTXT PIC  X(100) OCCURS 5 TIMES.
              05 BVVEAU-DET-FILLER     PIC  X(057).
           03 BVVEAU-TRAILLER      REDEFINES  BVVEAU-HEADER.
              05 BVVEAU-TR-QTDE-REG    PIC  9(009).
              05 BVVEAU-TR-FILLER      PIC  X(590).
