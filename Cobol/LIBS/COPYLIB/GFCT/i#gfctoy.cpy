      *****************************************************************
      * NOME DA INC - I#GFCTOY                                        *
      * DESCRICAO   - TEXT DE NOME DA AGENCIA COM AGRUPAMENTO         *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTOY-ENTRADA.
           03  GFCTOY-LL                            PIC S9(004) COMP.
           03  GFCTOY-ZZ                            PIC S9(004) COMP.
           03  GFCTOY-TRANSACAO                     PIC  X(009).
           03  GFCTOY-FUNCAO                        PIC  X(008).
           03  GFCTOY-DADOS-RST.
               05  GFCTOY-FILTRO.
                   07  GFCTOY-FILTRO-AGRUP          PIC  9(003).
                   07  GFCTOY-FILTRO-AGENCIA        PIC  9(005).
               05  GFCTOY-QTDE-OCOR                 PIC  9(007).
               05  GFCTOY-QTDE-TOT-REG              PIC  9(009).
               05  GFCTOY-FUNC-BDSCO                PIC  X(007).
               05  GFCTOY-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(047).
