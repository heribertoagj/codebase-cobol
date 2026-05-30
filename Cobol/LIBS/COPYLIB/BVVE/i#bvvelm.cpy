      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVELM                                            *
      * DESCRICAO: BOOK DE LIMITE CONTA GARANTIDA                      *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 046  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * LIM-CNPJ                     | CNPJ                            *
      * LIM-CONTRATO                 | NUMERO CONTARTO                 *
      * LIM-SUB-PRODUTO              | SUB PRODUTO                     *
      * LIM-SITUACAO-CONTRATO        | SITUACAO CONTRATO:              *
      * LIM-LIMITE-CONTRATO          | VALOR LIMITE CONTRATO           *
      * LIM-LIMITE-DISP              | VALOR LIMITE DISPONIVEL         *
      * LIM-DT-VENCIMENTO            | DATA VENCIMENTO                 *
      * LIM-DT-BASE-ARQ              | DATA BASE ARQUIVO               *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  LIM-CONTA-GARANT.
           05 LIM-CNPJ                           PIC 9(009) COMP-3.
           05 LIM-CNPJ-FILIAL                    PIC 9(005) COMP-3.
           05 LIM-CNPJ-CONTROLE                  PIC 9(002) COMP-3.
           05 LIM-CONTRATO                       PIC 9(009) COMP-3.
           05 LIM-SUB-PRODUTO                    PIC 9(003) COMP-3.
           05 LIM-LIMITE-CONTRATO                PIC 9(015) V99 COMP-3.
           05 LIM-LIMITE-DISP                    PIC 9(015) V99 COMP-3.
           05 LIM-DT-VENCIMENTO                  PIC X(010) COMP-3.
           05 LIM-DT-BASE-ARQ                    PIC X(010) COMP-3.
           05 LIM-SITUACAO-CONTRATO              PIC 9(002) COMP-3.
