      *****************************************************************
      * NOME DA INC - I#GFCTP3                                        *
      * DESCRICAO   - TEXT DE NOME DO CORRESPONDENTE BANCARIO COM     *
      *               AGRUPAMENTO - COMUNICACAO WEB X IMS - SAIDA     *
      * TAMANHO     - 200                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTP3-SAIDA.
           03  GFCTP3-LL                            PIC S9(004) COMP.
           03  GFCTP3-ZZ                            PIC S9(004) COMP.
           03  GFCTP3-TRANSACAO                     PIC  X(009).
           03  GFCTP3-FUNCAO                        PIC  X(008).
           03  GFCTP3-FILTRO.
               05  GFCTP3-FILTRO-AGRUP              PIC  9(003).
               05  GFCTP3-FILTRO-AGENCIA            PIC  9(005).
               05  GFCTP3-FILTRO-CORRESP            PIC  9(005).
           03  GFCTP3-QTDE-OCOR                     PIC  9(007).
           03  GFCTP3-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTP3-FUNC-BDSCO                    PIC  X(007).
           03  GFCTP3-ERRO                          PIC  9(001).
           03  GFCTP3-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTP3-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTP3-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTP3-FIM                           PIC  X(001).
           03  GFCTP3-DADOS.
               05  GFCTP3-COD-CORRESP               PIC  9(005).
               05  GFCTP3-DESC-CORRESP              PIC  X(020).
               05  FILLER                           PIC  X(027).
