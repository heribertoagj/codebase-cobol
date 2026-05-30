      *----------------------------------------------------------------*
      *    I#BVVE25                        LRECL: 300                  *
      *                                                                *
      *    BVVE - AFILIACAO ELETRONICA DE ESTABELECIMENTOS             *
      *                                                                *
      *    AREA COMUNICACAO                                            *
      *----------------------------------------------------------------*

       01  BVVE25-AREA.
           03  BVVE25-LL                  PIC S9(004)  COMP.
           03  BVVE25-ZZ                  PIC S9(004)  COMP.
           03  BVVE25-TRANCODE            PIC  X(015).
           03  BVVE25-SENHAS              PIC  X(037).
           03  BVVE25-NOVA-OPCAO          PIC  9(002).
           03  BVVE25-MENSAGEM            PIC  X(080).
           03  BVVE25-NUMERO              PIC  9(009).
           03  BVVE25-FILIAL              PIC  9(005).
           03  BVVE25-CONTROLE            PIC  9(002).
           03  BVVE25-NOME                PIC  X(032).
           03  BVVE25-RAMO                PIC  9(005).
           03  BVVE25-DESCR-RAMO          PIC  X(040).
           03  BVVE25-DATA-DE             PIC  X(010).
           03  BVVE25-DATA-ATE            PIC  X(010).
           03  BVVE25-BANCO               PIC  9(003).
           03  BVVE25-AGENCIA             PIC  9(005).
           03  BVVE25-SITUACAO            PIC  9(001).
JEM        03  BVVE25-CCTRL-MATRIZ        PIC  9(002).
JEM        03  BVVE25-DIRETO              PIC  9(001).
           03  BVVE25-FILLER              PIC  X(037).
