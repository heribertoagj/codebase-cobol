      *****************************************************************
      * NOME DA INC - I#GFCTP5                                        *
      * DESCRICAO   - TEXT DE NOME DO MUNICIPIO COM AGRUPAMENTO       *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 200                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
       01  GFCTP5-SAIDA.
           03  GFCTP5-LL                            PIC S9(004) COMP.
           03  GFCTP5-ZZ                            PIC S9(004) COMP.
           03  GFCTP5-TRANSACAO                     PIC  X(009).
           03  GFCTP5-FUNCAO                        PIC  X(008).
           03  GFCTP5-FILTRO.
               05  GFCTP5-FILTRO-AGRUP              PIC  9(003).
               05  GFCTP5-FILTRO-CMUN               PIC  9(007).
           03  GFCTP5-QTDE-OCOR                     PIC  9(007).
           03  GFCTP5-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTP5-FUNC-BDSCO                    PIC  X(007).
           03  GFCTP5-ERRO                          PIC  9(001).
           03  GFCTP5-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTP5-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTP5-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTP5-FIM                           PIC  X(001).
           03  GFCTP5-DADOS.
               05 GFCTP5-COD-MUNICIPIO              PIC  9(007).
               05 GFCTP5-DESC-MUNICIPIO             PIC  X(030).
               05 FILLER                            PIC  X(018).
