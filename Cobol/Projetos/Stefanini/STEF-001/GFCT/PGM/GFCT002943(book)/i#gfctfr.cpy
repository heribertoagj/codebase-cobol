      *****************************************************************
      * NOME DA INC - I#GFCTFR                                        *
      * DESCRICAO   - SOLICITACAO ESTORNO AGRUPAMENTO                 *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTFR-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTFR-ENTRADA.
           03 GFCTFR-LL                            PIC S9(004) COMP.
           03 GFCTFR-ZZ                            PIC S9(004) COMP.
           03 GFCTFR-TRANSACAO                     PIC  X(009).
           03 GFCTFR-FUNCAO                        PIC  X(008).
           03 GFCTFR-DADOS-RST.
              05 GFCTFR-FUNC-BDSCO                 PIC  X(007).
              05 GFCTFR-TERMINAL                   PIC  X(008).
              05 GFCTFR-FIM                        PIC  X(001).
              05 GFCTFR-FILLER                     PIC  X(313).
           03 GFCTFR-FIXO.
              05 GFCTFR-CAGPTO                     PIC  9(003).
              05 GFCTFR-RAGPTO                     PIC  X(030).
              05 GFCTFR-CSERVC-TARIF               PIC  9(005).
              05 GFCTFR-RSERVC-TARIF               PIC  X(100).
              05 GFCTFR-ESTORNO-TOTAL              PIC  X(001).
ST2507*       05 GFCTFR-CGC-CPF                    PIC  9(009).
ST2507*       05 GFCTFR-FILIAL                     PIC  9(005).
ST2507        05 GFCTFR-CGC-CPF                    PIC  X(009).
ST2507        05 GFCTFR-FILIAL                     PIC  X(004).
              05 GFCTFR-CONTROLE                   PIC  X(002).
              05 GFCTFR-CAGENCIA                   PIC  9(005).
              05 GFCTFR-RAGENCIA                   PIC  X(030).
              05 GFCTFR-CCONTA                     PIC  9(013).
              05 GFCTFR-RCONTA                     PIC  X(050).
              05 GFCTFR-CPAB                       PIC  9(005).
              05 GFCTFR-RPAB                       PIC  X(030).
              05 GFCTFR-RAZAO                      PIC  9(005).
              05 GFCTFR-LCTO                       PIC  9(003).
              05 GFCTFR-TP-PESSOA                  PIC  X(001).
              05 GFCTFR-CSEGMENTO                  PIC  9(003).
              05 GFCTFR-RSEGMENTO                  PIC  X(030).
              05 GFCTFR-DATA-DEBITO                PIC  X(010).
              05 GFCTFR-COPERACAO                  PIC  9(003).
              05 GFCTFR-ROPERACAO                  PIC  X(030).
              05 GFCTFR-CMOTIVO                    PIC  9(002).
              05 GFCTFR-RMOTIVO                    PIC  X(030).
              05 GFCTFR-COMENTARIO                 PIC  X(255).
