      *****************************************************************
      * NOME DA INC - I#GFCTNH                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - LISTA CONSULTA VIGENTE *
      *               COMUNICACAO WEB X IMS - ENTRADA - GFCT0422      *
      * TAMANHO     - 200                                             *
      * GFCTKF-LL   - 200                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *---------------------------------------------------------------*
      *    ALTERACAO EM DEZEMBRO/2011                                 *
      *    - AUMENTAR O TAMANHO DO CAMPO GFCTNH-QTDE-OCOR EM 1 BYTE   *
      *      VISTO QUE NAO ESTA COMPORTANDO MAIS DE 999 OCORRENCIAS,  *
      *      FORAM ADAPTADOS TAMBEM OS TAMANHOS NA WEB.               *
      *---------------------------------------------------------------*
      *
       01  GFCTNH-ENTRADA.
           03  GFCTNH-LL                            PIC S9(004) COMP.
           03  GFCTNH-ZZ                            PIC S9(004) COMP.
           03  GFCTNH-TRANSACAO                     PIC  X(009).
           03  GFCTNH-FUNCAO                        PIC  X(008).
           03  GFCTNH-DADOS-RST.
BI1211         05  GFCTNH-QTDE-OCOR                 PIC  9(007).
BI1211         05  GFCTNH-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNH-FUNC-BDSCO                PIC  X(007).
               05  GFCTNH-FILTRO.
                   15  GFCTNH-FLT-CAGPTO-CTA        PIC  9(003).
ST2506*            15  GFCTNH-FLT-CCGC-CPF          PIC  9(009).
ST2506*            15  GFCTNH-FLT-CFLIAL-CGC        PIC  9(005).
ST2506             15  GFCTNH-FLT-CCGC-CPF          PIC  X(009).
ST2506             15  GFCTNH-FLT-CFLIAL-CGC        PIC  X(004).
                   15  GFCTNH-FLT-CCTRL-CPF-CGC     PIC  9(002).
                   15  GFCTNH-FLT-CDEPDC            PIC  9(005).
                   15  GFCTNH-FLT-CPOSTO-SERVC      PIC  9(005).
                   15  GFCTNH-FLT-CSGMTO-TARIF      PIC  9(003).
                   15  GFCTNH-FLT-CMUN-IBGE         PIC  9(007).
                   15  GFCTNH-FLT-CSGL-UF           PIC  X(002).
                   15  GFCTNH-FLT-CCJTO-SERVC       PIC  9(003).
                   15  GFCTNH-FLT-GRUPO-CTBIL        PIC  9(003).
                   15  GFCTNH-FLT-SUB-GRUPO-CTBIL    PIC  9(003).
                   15  GFCTNH-FLT-DESC-CTBIL         PIC  X(040).
               05  GFCTNH-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(065).
