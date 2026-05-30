      *****************************************************************
      * NOME DA INC - I#GFCTNI                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - LISTA CONSULTA VIGENTE *
      *               COMUNICACAO WEB X IMS - SAIDA - GFCT0422        *
      * TAMANHO     - 1010                                            *
      * GFCTKG-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *---------------------------------------------------------------*
      *    ALTERACAO EM DEZEMBRO/2011                                 *
      *    - AUMENTAR O TAMANHO DO CAMPO GFCTNI-QTDE-OCOR EM 1 BYTE   *
      *      VISTO QUE NAO ESTA COMPORTANDO MAIS DE 999 OCORRENCIAS,  *
      *      FORAM ADAPTADOS TAMBEM OS TAMANHOS NA WEB.               *
      *---------------------------------------------------------------*
      *
       01  GFCTNI-SAIDA.
           03  GFCTNI-LL                            PIC S9(004) COMP.
           03  GFCTNI-ZZ                            PIC S9(004) COMP.
           03  GFCTNI-TRANSACAO                     PIC  X(009).
           03  GFCTNI-FUNCAO                        PIC  X(008).
           03  GFCTNI-DADOS-RST.
BI1211         05  GFCTNI-QTDE-OCOR                 PIC  9(007).
BI1211         05  GFCTNI-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNI-FUNC-BDSCO                PIC  X(007).
               05  GFCTNI-FILTRO.
                   15  GFCTNI-FLT-CAGPTO-CTA        PIC  9(003).
ST2506*            15  GFCTNI-FLT-CCGC-CPF          PIC  9(009).
ST2506*            15  GFCTNI-FLT-CFLIAL-CGC        PIC  9(005).
ST2506             15  GFCTNI-FLT-CCGC-CPF          PIC  X(009).
ST2506             15  GFCTNI-FLT-CFLIAL-CGC        PIC  X(004).
                   15  GFCTNI-FLT-CCTRL-CPF-CGC     PIC  9(002).
                   15  GFCTNI-FLT-CDEPDC            PIC  9(005).
                   15  GFCTNI-FLT-CPOSTO-SERVC      PIC  9(005).
                   15  GFCTNI-FLT-CSGMTO-TARIF      PIC  9(003).
                   15  GFCTNI-FLT-CMUN-IBGE         PIC  9(007).
                   15  GFCTNI-FLT-CSGL-UF           PIC  X(002).
                   15  GFCTNI-FLT-CCJTO-SERVC       PIC  9(003).
                   15  GFCTNI-FLT-GRUPO-CTBIL       PIC  9(003).
                   15  GFCTNI-FLT-SUB-GRUPO-CTBIL   PIC  9(003).
           03  GFCTNI-ERRO                          PIC  9(001).
           03  GFCTNI-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNI-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNI-DESC-MSG-ERRO                 PIC  X(080).
           03  GFCTNI-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(112).
           03  GFCTNI-DADOS OCCURS 4.
               05  GFCTNI-TIMESTAMP                 PIC  X(026).
ST2506*        05  GFCTNI-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTNI-FILIAL                    PIC  9(005).
ST2506         05  GFCTNI-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTNI-FILIAL                    PIC  X(004).
               05  GFCTNI-CONTROLE                  PIC  9(002).
               05  GFCTNI-AGENCIA                   PIC  9(005).
               05  GFCTNI-POSTO                     PIC  9(005).
               05  GFCTNI-SEGMENTO                  PIC  9(003).
               05  GFCTNI-MUNICIPIO                 PIC  9(007).
               05  GFCTNI-UF                        PIC  X(002).
               05  GFCTNI-DESCR-AGPTO               PIC  X(028).
               05  GFCTNI-COD-TARIFA                PIC  9(005).
               05  GFCTNI-DESCR-TARIFA              PIC  X(015).
               05  GFCTNI-DATA-INIC                 PIC  X(010).
               05  GFCTNI-DATA-FIM                  PIC  X(010).
               05  GFCTNI-GRUPO-CTBIL
                                       PIC  9(003).
               05  GFCTNI-SUB-GRUPO-CTBIL
                                       PIC  9(003).
               05  GFCTNI-DESC-CTBIL   PIC  X(040).
