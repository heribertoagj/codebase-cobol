      *****************************************************************
      * NOME DA INC - I#GFCTOZ                                        *
      * DESCRICAO   - TEXT DE NOME DA AGENCIA COM AGRUPAMENTO         *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 200                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTOZ-SAIDA.
           03  GFCTOZ-LL                            PIC S9(004) COMP.
           03  GFCTOZ-ZZ                            PIC S9(004) COMP.
           03  GFCTOZ-TRANSACAO                     PIC  X(009).
           03  GFCTOZ-FUNCAO                        PIC  X(008).
           03  GFCTOZ-FILTRO.
               05  GFCTOZ-FILTRO-AGRUP              PIC  9(003).
               05  GFCTOZ-FILTRO-AGENCIA            PIC  9(005).
           03  GFCTOZ-QTDE-OCOR                     PIC  9(007).
           03  GFCTOZ-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTOZ-FUNC-BDSCO                    PIC  X(007).
           03  GFCTOZ-ERRO                          PIC  9(001).
           03  GFCTOZ-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOZ-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOZ-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTOZ-FIM                           PIC  X(001).
           03  GFCTOZ-DADOS.
               05  GFCTOZ-COD-AGENCIA               PIC  9(005).
               05  GFCTOZ-DESC-AGENCIA              PIC  X(050).
               05  FILLER                           PIC  X(002).
