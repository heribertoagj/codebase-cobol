      *****************************************************************
      * NOME DA INC - I#GFCTNP                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO  - DETALHE DEFERIMENTO   *
      *               COMUNICACAO WEB X IMS   - ENTRADA - GFCT0888    *
      * TAMANHO     - 300                                             *
      * GFCTNP-LL   - 300                                             *
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
       01  GFCTNP-ENTRADA.
           03  GFCTNP-LL                            PIC S9(004) COMP.
           03  GFCTNP-ZZ                            PIC S9(004) COMP.
           03  GFCTNP-TRANSACAO                     PIC  X(009).
           03  GFCTNP-FUNCAO                        PIC  X(008).
           03  GFCTNP-DADOS-RST.
               05  GFCTNP-QTDE-OCOR                 PIC  9(003).
               05  GFCTNP-QTDE-TOT-REG              PIC  9(006).
               05  GFCTNP-FUNC-BDSCO                PIC  X(007).
               05  GFCTNP-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(122).
           03  GFCTNP-DADOS.
               05  GFCTNP-AGRUPMTO                  PIC  9(003).
ST2507*        05  GFCTNP-CGC-CPF                   PIC  9(009).
ST2507*        05  GFCTNP-FILIAL                    PIC  9(005).
ST2507         05  GFCTNP-CGC-CPF                   PIC  X(009).
ST2507         05  GFCTNP-FILIAL                    PIC  X(004).
               05  GFCTNP-CONTROLE                  PIC  X(002).
               05  GFCTNP-AGENCIA                   PIC  9(005).
               05  GFCTNP-POSTO                     PIC  9(005).
               05  GFCTNP-SEGMENTO                  PIC  9(003).
               05  GFCTNP-MUNICIPIO                 PIC  9(007).
               05  GFCTNP-UF                        PIC  X(002).
               05  GFCTNP-DESCR-AGPTO               PIC  X(030).
               05  GFCTNP-TARIFA                    PIC  9(005).
               05  GFCTNP-DESCR-TARIFA              PIC  X(030).
               05  GFCTNP-TIMESTAMP                 PIC  X(026).
               05  GFCTNP-SEQUENCIA                 PIC  9(002).
               05  GFCTNP-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNP-SUB-GRUPO-CTBIL           PIC  9(003).
ST2507         05  FILLER                           PIC  X(001).
