***************************** Top of Data ******************************
      *****************************************************************
      * NOME DA INC - I#GFCTKK                                        *
      * DESCRICAO   - TEXT DE MUNICIPIO                               *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 09.02.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
       01  GFCTKK-SAIDA.
           03  GFCTKK-LL                            PIC S9(004) COMP.
           03  GFCTKK-ZZ                            PIC S9(004) COMP.
           03  GFCTKK-TRANSACAO                     PIC  X(009).
           03  GFCTKK-FUNCAO                        PIC  X(008).
           03  GFCTKK-QTDE-OCOR                     PIC  9(003).
           03  GFCTKK-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTKK-FUNC-BDSCO                    PIC  X(007).
           03  GFCTKK-CMUN-IBGE                     PIC  9(007).
           03  GFCTKK-ERRO                          PIC  9(001).
      *        1 - ERRO TRATAVEL
      *        9 - ERRO DB2
           03  GFCTKK-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTKK-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTKK-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTKK-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(838).
           03  GFCTKK-DADOS.
               05 GFCTKK-COD-MUNICIPIO              PIC  9(007).
               05 GFCTKK-DESC-MUNICIPIO             PIC  X(030).
