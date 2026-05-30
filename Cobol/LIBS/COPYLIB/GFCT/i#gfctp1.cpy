      *****************************************************************
      * NOME DA INC - I#GFCTP1                                        *
      * DESCRICAO   - TEXT DE NOME DO POSTO BANCARIO COM AGRUPAMENTO  *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 200                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTP1-SAIDA.
           03  GFCTP1-LL                            PIC S9(004) COMP.
           03  GFCTP1-ZZ                            PIC S9(004) COMP.
           03  GFCTP1-TRANSACAO                     PIC  X(009).
           03  GFCTP1-FUNCAO                        PIC  X(008).
           03  GFCTP1-FILTRO.
               05  GFCTP1-FILTRO-AGRUP              PIC  9(003).
               05  GFCTP1-FILTRO-AGENCIA            PIC  9(005).
               05  GFCTP1-FILTRO-POSTO              PIC  9(005).
           03  GFCTP1-QTDE-OCOR                     PIC  9(007).
           03  GFCTP1-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTP1-FUNC-BDSCO                    PIC  X(007).
           03  GFCTP1-ERRO                          PIC  9(001).
           03  GFCTP1-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTP1-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTP1-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTP1-FIM                           PIC  X(001).
           03  GFCTP1-DADOS.
               05  GFCTP1-COD-POSTO                 PIC  9(005).
               05  GFCTP1-DESC-POSTO                PIC  X(020).
               05  FILLER                           PIC  X(027).
