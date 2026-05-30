      *****************************************************************
      * NOME DA INC - GFCTWANQ                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - DEFER DETALHE    *
      *               COMUNICACAO WEB X IMS        - SAIDA (0875)     *
      *               DETALHE - FLEXIBILIZAÇ O                        *
      * TAMANHO     - 1010                                            *
      * GFCTNQ-LL   - 1010                                            *
      * DATA        - 11.06.2010                                      *
      * RESPONSAVEL - CIBELE  - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNQ-SAIDA.
           03  GFCTNQ-LL                            PIC S9(004) COMP.
           03  GFCTNQ-ZZ                            PIC S9(004) COMP.
           03  GFCTNQ-TRANSACAO                     PIC  X(009).
           03  GFCTNQ-FUNCAO                        PIC  X(008).
           03  GFCTNQ-QTDE-OCOR                     PIC  9(003).
           03  GFCTNQ-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTNQ-FUNC-BDSCO                    PIC  X(007).
           03  GFCTNQ-ERRO                          PIC  9(001).
           03  GFCTNQ-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNQ-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNQ-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTNQ-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(014).
           03  GFCTNQ-DADOS-VIG-HIST.
               05  GFCTNQ-AGENCIA                   PIC  9(005).
               05  GFCTNQ-NOME-AGENC                PIC  X(040).
               05  GFCTNQ-CONTA                     PIC  9(013).
               05  GFCTNQ-TIMESTAMP                 PIC  X(026).
               05  GFCTNQ-NOME-CLIENTE              PIC  X(040).
               05  GFCTNQ-COD-SEGMENTO              PIC  9(003).
               05  GFCTNQ-NOME-SEGMENTO             PIC  X(040).
               05  GFCTNQ-COD-CONJ-SERV             PIC  9(003).
               05  GFCTNQ-NOME-CONJ-SERV            PIC  X(040).
               05  GFCTNQ-LOTE                      PIC  9(011).
               05  GFCTNQ-DINIC-VIG-SOLIC           PIC  X(010).
               05  GFCTNQ-DFIM-VIG-SOLIC            PIC  X(010).
               05  GFCTNQ-DINIC-VIG-CALC            PIC  X(010).
               05  GFCTNQ-DFIM-VIG-CALC             PIC  X(010).
               05  GFCTNQ-CTIPO-OPER-MOTVO          PIC  9(003).
               05  GFCTNQ-NOME-OPER-MOTVO           PIC  X(030).
               05  GFCTNQ-COD-FUNC-DEF              PIC  9(009).
               05  GFCTNQ-NOME-FUNC-DEF             PIC  X(040).
               05  GFCTNQ-TIMESTAMP-DEF             PIC  X(026).
               05  GFCTNQ-CMOTVO-JUSTF              PIC  9(002).
               05  GFCTNQ-NOME-MOTVO                PIC  X(030).
               05  GFCTNQ-SITUACAO-FLEX             PIC  9(002).
               05  GFCTNQ-RJUSTF                    PIC  X(200).
               05  GFCTNQ-OPERACAO                  PIC  X(001).
      * D = USUARIO PODE DEFERIR
      * I = USUARIO PODE INDEFERIR OU TRANSFERIR
      * E = ENVIO A PROXIMO DEPENDENCIA
               05  GFCTNQ-CFUNC-ANALISE             PIC  9(009).
               05  GFCTNQ-NOME-FUNC-ANALISE         PIC  X(040).
               05  GFCTNQ-DATA-ANALISE              PIC  X(010).
               05  GFCTNQ-HORA-ANALISE              PIC  X(005).
               05  GFCTNQ-RJUSTF-PARECER            PIC  X(200).
