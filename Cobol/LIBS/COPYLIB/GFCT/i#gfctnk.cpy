      *****************************************************************
      * NOME DA INC - I#GFCTNK                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - LISTA   CONSULTA HIST  *
      *               COMUNICACAO WEB X IMS  - SAIDA - GFCT0883       *
      * TAMANHO     - 1010                                            *
      * GFCTNK-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *---------------------------------------------------------------*
      *    ALTERACAO EM DEZEMBRO/2011                                 *
      *    - AUMENTAR O TAMANHO DO CAMPO GFCTNK-QTDE-OCOR EM 1 BYTE   *
      *      VISTO QUE NAO ESTA COMPORTANDO MAIS DE 999 OCORRENCIAS,  *
      *      FORAM ADAPTADOS TAMBEM OS TAMANHOS NA WEB.               *
      *---------------------------------------------------------------*
      *
       01  GFCTNK-SAIDA.
           03  GFCTNK-LL                            PIC S9(004) COMP.
           03  GFCTNK-ZZ                            PIC S9(004) COMP.
           03  GFCTNK-TRANSACAO                     PIC  X(009).
           03  GFCTNK-FUNCAO                        PIC  X(008).
           03  GFCTNK-DADOS-RST.
BI1211         05  GFCTNK-QTDE-OCOR                 PIC  9(007).
BI1211         05  GFCTNK-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNK-FUNC-BDSCO                PIC  X(007).
               05  GFCTNK-FILTRO.
                   10  GFCTNK-FLT-TIPO-HIST         PIC  X(001).
      *                T = TODOS
      *                P = PEDIDOS
      *                D = DEFERIDOS NAO VIGENTES
                   10  GFCTNK-FLT-CAGPTO-CTA        PIC  9(003).
                   10  GFCTNK-FLT-CCGC-CPF          PIC  9(009).
                   10  GFCTNK-FLT-CFLIAL-CGC        PIC  9(005).
                   10  GFCTNK-FLT-CCTRL-CPF-CGC     PIC  9(002).
                   10  GFCTNK-FLT-CDEPDC            PIC  9(005).
                   10  GFCTNK-FLT-CPOSTO-SERVC      PIC  9(005).
                   10  GFCTNK-FLT-CSGMTO-TARIF      PIC  9(003).
                   10  GFCTNK-FLT-CMUN-IBGE         PIC  9(007).
                   10  GFCTNK-FLT-CSGL-UF           PIC  9(002).
                   10  GFCTNK-FLT-CCJTO-SERVC       PIC  9(003).
                   10  GFCTNK-FLT-GRUPO-CTBIL       PIC  9(003).
                   10  GFCTNK-FLT-SUB-GRUPO-CTBIL   PIC  9(003).
           03  GFCTNK-ERRO                          PIC  9(001).
           03  GFCTNK-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNK-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNK-DESC-MSG-ERRO                 PIC  X(080).
           03  GFCTNK-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(103).
           03  GFCTNK-DADOS  OCCURS 4.
ST2506*        05  GFCTNK-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTNK-FILIAL                    PIC  9(005).
ST2506         05  GFCTNK-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTNK-FILIAL                    PIC  X(004).
               05  GFCTNK-CONTROLE                  PIC  9(002).
               05  GFCTNK-AGENCIA                   PIC  9(005).
               05  GFCTNK-POSTO                     PIC  9(005).
               05  GFCTNK-SEGMENTO                  PIC  9(003).
               05  GFCTNK-MUNICIPIO                 PIC  9(007).
               05  GFCTNK-UF                        PIC  X(002).
               05  GFCTNK-DESCR-AGPTO               PIC  X(030).
               05  GFCTNK-COD-TARIFA                PIC  9(005).
               05  GFCTNK-DESCR-TARIFA              PIC  X(015).
               05  GFCTNK-TIMESTAMP                 PIC  X(026).
               05  GFCTNK-SEQUENCIA                 PIC  9(002).
               05  GFCTNK-SITUACAO                  PIC  X(020).
               05  GFCTNK-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNK-SUB-GRUPO-CTBIL           PIC  9(003).
               05  GFCTNK-DESC-CTBIL                PIC  X(038).
