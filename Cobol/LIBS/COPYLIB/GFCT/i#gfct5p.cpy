      *****************************************************************
      * NOME DA INC - I#GFCT5P                                        *
      * DESCRICAO   - DADOS BASICOS - GRUPO DE SERVICOS    INCLUSAO   *
      * COMUNICACAO WEB X IMS - ENTRADA                    ALTERACAO  *
      * TAMANHO     - 300                                  EXCLUSAO   *
      * GFCT5P-LL   - 300                                             *
      * DATA        - 11.02.2005                                      *
      * RESPONSAVEL - MARCELO  - PROCWORK                             *
      *****************************************************************
       01  GFCT5P-ENTRADA.
           03  GFCT5P-LL                            PIC S9(004) COMP.
           03  GFCT5P-ZZ                            PIC S9(004) COMP.
           03  GFCT5P-TRANSACAO                     PIC  X(009).
           03  GFCT5P-FUNCAO                        PIC  X(008).
           03  GFCT5P-DADOS-RST.
               05  GFCT5P-FILTRO                    PIC  9(005).
               05  GFCT5P-QTDE-OCOR                 PIC  9(003).
               05  GFCT5P-QTDE-TOT-REG              PIC  9(006).
               05  GFCT5P-FUNC-BDSCO                PIC  X(007).
               05  GFCT5P-TERMINAL                  PIC  X(008).
               05  GFCT5P-P-CGRP-SERVC              PIC  9(005).
               05  GFCT5P-P-CTPO-LCTO-CTBIL         PIC  9(002).
               05  GFCT5P-P-CSEQ-EVNTO-CTBIL        PIC  9(002).
               05  GFCT5P-DINI-PERIODO              PIC  X(010).
               05  GFCT5P-DFIM-PERIODO              PIC  X(010).
               05  GFCT5P-CONS-LISTA                PIC  X(001).
******* C = CONSULTA LCTO     L = LISTA TODOS LCTOS. S - SO FIXO
               05  GFCT5P-CONTINUA                  PIC  X(001).
******* S - CONTINUA A INCLUIR NO MESMO GRUPO N - NOVO GRUPO.
               05  GFCT5P-ACABOU-DE-INCLUIR         PIC  X(001).
******* 'S'-NAO QUER MAIS INCLUIR REGISTROS  'N'-CONTINUA INCLUINDO
               05  GFCT5P-FIM                       PIC  X(001).
310108         05  FILLER                           PIC  X(071).
           03  GFCT5P-DADOS.
               05  GFCT5P-DADOS-FIXOS.
                   07  GFCT5P-COD-GRUPO             PIC  9(005).
                   07  GFCT5P-DESC-GRUPO            PIC  X(030).
                   07  GFCT5P-DINI-VGC              PIC  X(010).
                   07  GFCT5P-DFIM-VGC              PIC  X(010).
130208             07  GFCT5P-SO-TARIFA             PIC  X(001).
310108             07  GFCT5P-COD-TAR-COBR          PIC  9(005).
310108             07  GFCT5P-DESCR-TAR-COBR        PIC  X(040).
310108             07  GFCT5P-COD-TAR-NOVA          PIC  9(005).
310108             07  GFCT5P-DESCR-TAR-MOVA        PIC  X(040).
