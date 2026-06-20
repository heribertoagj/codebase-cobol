      *****************************************************************
      * NOME DA INC - I#GFCTNR                                        *
      * DESCRICAO   - FLEXIBILIZACAO AGRUPAMENTO   - DEFERIMENTO      *
      *               COMUNICACAO WEB X IMS        - ENTRADA (0889)   *
      * TAMANHO     - 500                                             *
      * GFCTNR-LL   - 500                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTNR-ENTRADA.
           03  GFCTNR-LL                            PIC S9(004) COMP.
           03  GFCTNR-ZZ                            PIC S9(004) COMP.
           03  GFCTNR-TRANSACAO                     PIC  X(009).
           03  GFCTNR-FUNCAO                        PIC  X(008).
           03  GFCTNR-DADOS-RST.
               05  GFCTNR-QTDE-OCOR                 PIC  9(003).
               05  GFCTNR-QTDE-TOT-REG              PIC  9(006).
               05  GFCTNR-FUNC-BDSCO                PIC  X(007).
               05  GFCTNR-AGRUPMTO                  PIC  9(003).
ST2507*        05  GFCTNR-CGC-CPF                   PIC  9(009).
ST2507*        05  GFCTNR-FILIAL                    PIC  9(005).
ST2507         05  GFCTNR-CGC-CPF                   PIC  X(009).
ST2507         05  GFCTNR-FILIAL                    PIC  X(004).
               05  GFCTNR-CONTROLE                  PIC  X(002).
               05  GFCTNR-AGENCIA                   PIC  9(005).
               05  GFCTNR-POSTO                     PIC  9(005).
               05  GFCTNR-SEGMENTO                  PIC  9(003).
               05  GFCTNR-MUNICIPIO                 PIC  9(007).
               05  GFCTNR-UF                        PIC  X(002).
               05  GFCTNR-TARIFA                    PIC  9(005).
               05  GFCTNR-TIMESTAMP                 PIC  X(026).
               05  GFCTNR-SEQUENCIA                 PIC  9(002).
               05  GFCTNR-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTNR-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTNR-RJUSTF-DEFER              PIC  X(255).
               05  GFCTNR-OPERACAO                  PIC  X(001).
               05  GFCTNR-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNR-SUB-GRUPO-CTBIL           PIC  9(003).
      * D = DEFERIDO
      * I = INDEFERIDO
               05  GFCTNR-FIM                       PIC  X(001).
ST2507*        05  FILLER                           PIC  X(106).
ST2507         05  FILLER                           PIC  X(107).
