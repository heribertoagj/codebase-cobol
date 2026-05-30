      *****************************************************************
      * NOME DA INC - I#GFCTNJ                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - LISTA CONSULTA HIST    *
      *               COMUNICACAO WEB X IMS  - ENTRADA GFCT0883       *
      * TAMANHO     - 200                                             *
      * GFCTNJ-LL   - 200                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *---------------------------------------------------------------*
      *    ALTERACAO EM DEZEMBRO/2011                                 *
      *    - AUMENTAR O TAMANHO DO CAMPO GFCTNJ-QTDE-OCOR EM 1 BYTE   *
      *      VISTO QUE NAO ESTA COMPORTANDO MAIS DE 999 OCORRENCIAS,  *
      *      FORAM ADAPTADOS TAMBEM OS TAMANHOS NA WEB.               *
      *---------------------------------------------------------------*
      *
       01  GFCTNJ-ENTRADA.
           03  GFCTNJ-LL                             PIC S9(004) COMP.
           03  GFCTNJ-ZZ                             PIC S9(004) COMP.
           03  GFCTNJ-TRANSACAO                      PIC  X(009).
           03  GFCTNJ-FUNCAO                         PIC  X(008).
           03  GFCTNJ-DADOS-RST.
BI1211         05  GFCTNJ-QTDE-OCOR                  PIC  9(007).
BI1211         05  GFCTNJ-QTDE-TOT-REG               PIC  9(009).
               05  GFCTNJ-FUNC-BDSCO                 PIC  X(007).
               05  GFCTNJ-FILTRO.
                   10  GFCTNJ-FLT-TIPO-HIST          PIC  X(001).
      *                T = TODOS
      *                P = PEDIDOS
      *                D = DEFERIDOS NAO VIGENTES
                   10  GFCTNJ-FLT-CAGPTO-CTA         PIC  9(003).
                   10  GFCTNJ-FLT-CCGC-CPF           PIC  9(009).
                   10  GFCTNJ-FLT-CFLIAL-CGC         PIC  9(005).
                   10  GFCTNJ-FLT-CCTRL-CPF-CGC      PIC  9(002).
                   10  GFCTNJ-FLT-CDEPDC             PIC  9(005).
                   10  GFCTNJ-FLT-CPOSTO-SERVC       PIC  9(005).
                   10  GFCTNJ-FLT-CSGMTO-TARIF       PIC  9(003).
                   10  GFCTNJ-FLT-CMUN-IBGE          PIC  9(007).
                   10  GFCTNJ-FLT-CSGL-UF            PIC  X(002).
                   10  GFCTNJ-FLT-CCJTO-SERVC        PIC  9(003).
                   10  GFCTNJ-FLT-GRUPO-CTBIL        PIC  9(003).
                   10  GFCTNJ-FLT-SUB-GRUPO-CTBIL    PIC  9(003).
                   10  GFCTNJ-FLT-DESC-CTBIL         PIC  X(040).
           03  GFCTNJ-FIM                            PIC  X(001).
           03  FILLER                                PIC  X(064).

