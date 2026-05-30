      *================================================================*
      *  I#BVVEAV - ENVIO E RECEPCAO DE SOLICITACOES DE DESTRAVA       *
      *             ( GRAN X BVVE MASSIVO )                            *
      *----------------------------------------------------------------*
      *  LRECL: 600                                                    *
      *================================================================*
       01  BVVEAV-DESTRAVA.
           03 BVVEAV-TP-REG            PIC  X(001)         VALUE SPACES.
           03 BVVEAV-HEADER.
              05 BVVEAV-HD-DATA        PIC  X(010)         VALUE SPACES.
              05 BVVEAV-HD-HORA        PIC  X(008)         VALUE SPACES.
              05 BVVEAV-HD-ROTINA      PIC  X(004)         VALUE SPACES.
              05 BVVEAV-HD-ORIGEM      PIC  X(001)         VALUE SPACES.
              05 BVVEAV-HD-FILLER      PIC  X(576)         VALUE SPACES.
           03 BVVEAV-DETALHE       REDEFINES   BVVEAV-HEADER.
              05 BVVEAV-DET-CNPJ.
                 07 BVVEAV-DET-CNPJN   PIC  9(015).
              05 FILLER    REDEFINES  BVVEAV-DET-CNPJ.
                 07 BVVEAV-DET-CPF.
                    09 BVVEAV-DET-CPFN PIC  9(011).
                 07 BVVEAV-DET-ZEROS   PIC  9(004).
              05 BVVEAV-DET-TP-PSSOA   PIC  X(001).
              05 BVVEAV-DET-PRODUTO.
                 07 BVVEAV-DET-PRODN   PIC  9(005).
              05 BVVEAV-DET-CPROP-ESQM PIC  X(003).
      *          003 = MASTER / 004 = VISA                             *
              05 BVVEAV-DET-COD-RET    PIC  9(002).
              05 BVVEAV-DET-ERRO.
                 07 BVVEAV-DET-ERROTXT PIC  X(100) OCCURS 5 TIMES.
              05 BVVEAV-DET-FILLER     PIC  X(073).
           03 BVVEAV-TRAILLER      REDEFINES  BVVEAV-HEADER.
              05 BVVEAV-TR-QTDE-REG    PIC  9(009).
              05 BVVEAV-TR-FILLER      PIC  X(590).
