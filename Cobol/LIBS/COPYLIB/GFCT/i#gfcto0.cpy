      *****************************************************************
      * NOME DA INC - I#GFCTO0                                        *
      * DESCRICAO   - EXCECAO ADESAO COMPULSORIA - CONSULTA           *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTO0-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTO0-SAIDA.
           03 GFCTO0-LL                            PIC S9(004) COMP.
           03 GFCTO0-ZZ                            PIC S9(004) COMP.
           03 GFCTO0-TRANSACAO                     PIC  X(009).
           03 GFCTO0-FUNCAO                        PIC  X(008).
           03 GFCTO0-DADOS-RST.
              05 GFCTO0-QTDE-OCOR                  PIC  9(003).
              05 GFCTO0-QTDE-TOT-REG               PIC  9(006).
              05 GFCTO0-FUNC-BDSCO                 PIC  X(007).
              05 GFCTO0-FILTRO.
                 07 GFCTO0-FILTRO-PACOTE           PIC  9(005).
                 07 GFCTO0-FILTRO-AGPTO            PIC  9(003).
                 07 GFCTO0-FILTRO-AGENCIA          PIC  9(005).
                 07 GFCTO0-FILTRO-CONTA            PIC  9(013).
                 07 GFCTO0-FILTRO-SEQ              PIC  9(009).
              05 GFCTO0-PONTEIRO.
                 07 GFCTO0-PONT-PACOTE             PIC  9(005).
                 07 GFCTO0-PONT-AGPTO              PIC  9(003).
                 07 GFCTO0-PONT-AGENCIA            PIC  9(005).
                 07 GFCTO0-PONT-CONTA              PIC  9(013).
                 07 GFCTO0-PONT-SEQ                PIC  9(009).
                 07 GFCTO0-PONT-DINI               PIC  X(010).
                 07 GFCTO0-PONT-HINI               PIC  X(026).
              05 GFCTO0-ERRO                       PIC  9(001).
              05 GFCTO0-COD-MSG-ERRO               PIC  9(007).
              05 GFCTO0-COD-SQL-ERRO               PIC  9(003).
              05 GFCTO0-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTO0-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(144).
           03 GFCTO0-OCORR                         OCCURS 08 TIMES.
              05 GFCTO0-AGENCIA                    PIC  9(005).
              05 GFCTO0-CONTA                      PIC  9(013).
              05 GFCTO0-DESC-NOME                  PIC  X(040).
              05 GFCTO0-DATA-INICIO                PIC  X(010).
              05 GFCTO0-DATA-FIM                   PIC  X(010).
              05 GFCTO0-CANCELADO                  PIC  X(001).
