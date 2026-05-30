      *****************************************************************
      * NOME DA INC - I#GFCT5Q                                        *
      * DESCRICAO   - DADOS BASICOS - GRUPO DE SERVICOS    INCLUSAO   *
      * COMUNICACAO WEB X IMS - SA DA                     ALTERACAO   *
      * TAMANHO     - 1010                                 EXCLUSAO   *
      * GFCT5Q-LL   - 1010                                CONSULTA    *
      * DATA        - 11.02.2005                                      *
      * RESPONSAVEL - MARCELO  - PROCWORK                             *
      *****************************************************************
       01  GFCT5Q-SAIDA.
           03  GFCT5Q-LL                            PIC S9(004) COMP.
           03  GFCT5Q-ZZ                            PIC S9(004) COMP.
           03  GFCT5Q-TRANSACAO                     PIC  X(009).
           03  GFCT5Q-FUNCAO                        PIC  X(008).
           03  GFCT5Q-QTDE-OCOR                     PIC  9(003).
           03  GFCT5Q-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT5Q-FUNC-BDSCO                    PIC  X(007).
           03  GFCT5Q-TERMINAL                      PIC  X(008).
           03  GFCT5Q-ERRO                          PIC  9(001).
           03  GFCT5Q-COD-MSG-ERRO                  PIC  9(007).
           03  GFCT5Q-COD-SQL-ERRO                  PIC  9(003).
           03  GFCT5Q-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCT5Q-P-CGRP-SERVC                  PIC  9(005).
           03  GFCT5Q-P-CTPO-LCTO-CTBIL             PIC  9(002).
           03  GFCT5Q-P-CSEQ-EVNTO-CTBIL            PIC  9(002).
           03  GFCT5Q-DINI-PERIODO                  PIC  X(010).
           03  GFCT5Q-DFIM-PERIODO                  PIC  X(010).
           03  GFCT5Q-CONS-LISTA                    PIC  X(001).
           03  GFCT5Q-CONTINUA                      PIC  X(001).
******* S - CONTINUA A INCLUIR NO MESMO GRUPO N - NOVO GRUPO.
           03  GFCT5Q-ACABOU-DE-INCLUIR             PIC  X(001).
******* 'S'-NAO QUER MAIS INCLUIR REGISTROS  'N'-CONTINUA INCLUINDO

           03  GFCT5Q-FIM                           PIC  X(001).
310108     03  FILLER                               PIC  X(615).
           03  GFCT5Q-DADOS.
               05  GFCT5Q-DADOS-FIXOS.
                   07  GFCT5Q-COD-GRUPO             PIC  9(005).
                   07  GFCT5Q-DESC-GRUPO            PIC  X(030).
                   07  GFCT5Q-DINI-VGC              PIC  X(010).
                   07  GFCT5Q-DFIM-VGC              PIC  X(010).
130208             07  GFCT5Q-SO-TARIFA             PIC  X(001).
310108             07  GFCT5Q-COD-TAR-COBR          PIC  9(005).
310108             07  GFCT5Q-DESCR-TAR-COBR        PIC  X(040).
130208             07  GFCT5Q-COD-TAR-NOVA          PIC  9(005).
130208             07  GFCT5Q-DESCR-TAR-NOVA        PIC  X(040).
                   07  GFCT5Q-ERRO-OCOR             PIC  9(001).
    ********** ZERO=OK   1=ERRO
                   07  GFCT5Q-DESC-MSG-ERRO-OCOR    PIC  X(079).
