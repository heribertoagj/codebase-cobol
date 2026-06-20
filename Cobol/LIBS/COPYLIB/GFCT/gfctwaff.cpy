      *****************************************************************
      * NOME DA INC - GFCTWAFF                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERVICOS INCLUSAO *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * WAFG-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER  - PROCWORK                              *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                  *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  WAFF-ENTRADA.
           03 WAFF-LL                            PIC S9(004) COMP.
           03 WAFF-ZZ                            PIC S9(004) COMP.
           03 WAFF-TRANSACAO                     PIC  X(009).
           03 WAFF-FUNCAO                        PIC  X(008).
           03 WAFF-DADOS-RST.
              05 WAFF-FUNC-BDSCO                 PIC  X(007).
              05 WAFF-TERMINAL                   PIC  X(008).
              05 WAFF-FIM                        PIC  X(001).
              05 WAFF-FILLER                     PIC  X(646).
           03 WAFF-FIXO.
              05 WAFF-CAGPTO                     PIC  9(003).
              05 WAFF-CTARIF                     PIC  9(005).
              05 WAFF-HINCL                      PIC  X(026).
              05 WAFF-CAGEN                      PIC  9(005).
              05 WAFF-CPAB                       PIC  9(005).
              05 WAFF-CCTA                       PIC  9(013).
              05 WAFF-CCPF.
ST2506*          10 WAFF-CCPF-NUM                PIC  9(009).
ST2506*          10 WAFF-CCPF-FIL                PIC  9(005).
ST2506*          10 WAFF-CCPF-CTR                PIC  9(002).
ST2506           10 WAFF-CCPF-NUM                PIC  X(009).
ST2506           10 WAFF-CCPF-FIL                PIC  X(004).
ST2506           10 WAFF-CCPF-CTR                PIC  9(002).
              05 WAFF-JUSTIF                     PIC  X(254).
